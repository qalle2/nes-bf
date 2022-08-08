; Qalle's Brainfuck (NES, ASM6)

; --- Constants ---------------------------------------------------------------

; Notes:
; - Only the first sprite slot is actually used, and the other slots only need
;   their Y positions to be set to $ff, so the OAM page accommodates many other
;   variables at addresses not divisible by 4 ($05-$07, $09-$0b, $0d-$0f, ...).
; - "PPU buffer" = what to write to PPU memory on next VBlank.
; - Bottom half of stack ($0100-$017f) is used for other purposes by ml_prep1.
; - nmi_done: did the NMI routine just run? Used for once-per-frame stuff;
;   set by NMI; read & cleared at the start of main loop.

; RAM
sprite_data     equ $00    ; OAM page ($100 bytes, see above)
program_mode    equ $05    ; see constants below
bf_ram_ptr      equ $06    ; pointer to bf_ram (2 bytes)
nmi_done        equ $09    ; see above ($00 = no, $80 = yes)
ppu_ctrl_copy   equ $0a    ; copy of ppu_ctrl
frame_counter   equ $0b    ; for blinking cursors
pad_status      equ $0d    ; joypad status
prev_pad_status equ $0e    ; joypad status on previous frame
ppu_buf_adrhi   equ $0f    ; PPU buffer - high byte of address (0 = nothing)
ppu_buf_adrlo   equ $11    ; PPU buffer - low  byte of address
ppu_buf_value   equ $12    ; PPU buffer - value
ppu_buf_count   equ $13    ; PPU buffer - repeat count
program_len     equ $15    ; length of BF program
bf_pc           equ $16    ; program counter of BF program (preincremented)
output_len      equ $17    ; number of characters printed by BF program
keyb_x          equ $19    ; cursor X position on virtual keyboard (0-15)
keyb_y          equ $1a    ; cursor Y position on virtual keyboard (0-5)
sp_copy         equ $1b    ; copy of stack pointer
bf_program      equ $0200  ; BF program ($100 bytes)
brackets        equ $0300  ; target addresses of "[" and "]" ($100 bytes)
bf_ram          equ $0400  ; RAM of BF program ($400 bytes; must be at $0400)

; memory-mapped registers
ppu_ctrl        equ $2000
ppu_mask        equ $2001
ppu_status      equ $2002
oam_addr        equ $2003
ppu_scroll      equ $2005
ppu_addr        equ $2006
ppu_data        equ $2007
dmc_freq        equ $4010
oam_dma         equ $4014
snd_chn         equ $4015
joypad1         equ $4016
joypad2         equ $4017

; values for program_mode (must be 0, 1, ... because they're used as indexes
; to jump table)
mode_edit       equ 0    ; editing BF program (must be 0)
mode_prep1      equ 1    ; preparing to run BF program, part 1/2
mode_prep2      equ 2    ; preparing to run BF program, part 2/2
mode_run        equ 3    ; BF program running
mode_input      equ 4    ; BF program waiting for input
mode_ended      equ 5    ; BF program finished

; Possible transitions between program modes:
;
;   (start)
;      |
;      v
;    edit <-----------+-----+-------\
;      ^              |     |       |
;      |            ended   |       |
;      |                ^   /       |
;      v                 \ /        |
;    prep1 --> prep2 --> run <--> input

; --- Macros ------------------------------------------------------------------

macro dwbe _word
                db >(_word), <(_word)   ; big-endian word
endm

; --- iNES header -------------------------------------------------------------

                ; see https://wiki.nesdev.org/w/index.php/INES
                base $0000
                db "NES", $1a            ; file id
                db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
                db %00000001, %00000000  ; NROM mapper, vertical NT mirroring
                pad $0010, $00           ; unused

; --- Initialization ----------------------------------------------------------

                base $c000              ; last 16 KiB of CPU address space

reset           ; initialize the NES
                ; see https://wiki.nesdev.org/w/index.php/Init_code
                sei                     ; ignore IRQs
                cld                     ; disable decimal mode
                ldx #%01000000
                stx joypad2             ; disable APU frame IRQ
                ldx #$ff
                txs                     ; initialize stack pointer
                inx
                stx ppu_ctrl            ; disable NMI
                stx ppu_mask            ; disable rendering
                stx dmc_freq            ; disable DMC IRQs
                stx snd_chn             ; disable sound channels

                jsr wait_vbl_start      ; wait until next VBlank starts
                jsr init_ram            ; initialize main RAM

                jsr wait_vbl_start      ; wait until next VBlank starts
                jsr init_palette        ; initialize palette (while in VBlank)
                jsr init_vram           ; initialize VRAM

                jsr wait_vbl_start      ; wait until next VBlank starts
                lda #%10000000          ; enable NMI, show NT0
                sta ppu_ctrl_copy
                jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask

                jmp main_loop           ; start main program

wait_vbl_start  bit ppu_status          ; wait until next VBlank starts
-               bit ppu_status
                bpl -
                rts

init_ram        ; initialize main RAM

                ; clear sprites, variables and BF code
                lda #$00
                tax
-               sta sprite_data,x
                sta bf_program,x
                inx
                bne -

                ; hide sprites
                lda #$ff
                ldx #0
-               sta sprite_data,x
                inx
                inx
                inx
                inx
                bne -

                lda #1                  ; 1st column is always empty
                sta program_len
                jmp upd_io_cursor       ; ends with RTS

init_palette    ; copy initial palette (while still in VBlank)

                ldy #$3f
                lda #$00
                jsr set_ppu_addr        ; Y*$100+A -> PPU address

                ldy #8                  ; copy same colors to all subpalettes
--              ldx #0
-               lda initial_palette,x
                sta ppu_data
                inx
                cpx #4
                bne -
                dey
                bne --
                rts

initial_palette ; copied to all subpalettes
                db $0f                  ; background   (black)
                db $30                  ; foreground 1 (white)
                db $28                  ; foreground 2 (yellow)
                db $00                  ; unused

init_vram       ; initialize VRAM

                ; clear name & attribute tables
                ldy #$20
                lda #$00
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ldx #8
                tay
-               jsr fill_vram           ; write A Y times
                dex
                bne -

                ; copy strings to NT0 (edit mode) and NT1 (run mode)
                ldx #$ff
--              inx                     ; new string
                ldy strings,x           ; VRAM address high
                beq +                   ; 0 = end of all strings
                inx
                lda strings,x           ; VRAM address low
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
-               inx
                lda strings,x           ; byte (0 = end of string)
                beq --                  ; next string
                sta ppu_data
                jmp -

+               ; draw horizontal bars in NT0 & NT1
                ldx #(4-1)
-               ldy horz_bars_hi,x
                lda horz_bars_lo,x
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ldy #30
                lda #$81                ; horizontal bar
                jsr fill_vram           ; write A Y times
                dex
                bpl -

                ; draw virtual keyboard in NT1
                ldy #>($2400+19*32+24)
                lda #<($2400+19*32+24)
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ldx #32                 ; X = character code
-               txa                     ; print 16 spaces before each line
                and #%00001111
                bne +
                ldy #16
                lda #$20
                jsr fill_vram           ; write A Y times
+               stx ppu_data
                inx
                bpl -

                rts

fill_vram       ; write A to VRAM Y times
                sta ppu_data
                dey
                bne fill_vram
                rts

strings         ; each string: PPU address high/low, characters, terminator (0)
                ; address high 0 terminates string data

                dwbe $2000+3*32+7
                db   "Qalle's Brainfuck"
                db   0
                dwbe $2000+6*32+12
                db   "Program:"
                db   0
                dwbe $2000+18*32+3
                db   $82, "=+ ", $83, "=-  "
                db   $84, "=< ", $85, "=>  "
                db   $87, "=[ ", $86, "=]"
                db   0
                dwbe $2000+20*32+5
                db   $8d, $8a, $8b, $8a, $89, $8e, "+", $87, "=,  "
                db   $8d, $8a, $8b, $8a, $89, $8e, "+", $86, "=."
                db   0
                dwbe $2000+22*32+8
                db   $8d, $8e, $88, $8c, $8e, "=backspace"
                db   0
                dwbe $2000+24*32+8
                db   $8d, $8a, $8b, $8a, $89, $8e, "+"
                db   $8d, $8e, $88, $8c, $8e, "=run"
                db   0

                dwbe $2400+3*32+7
                db   "Qalle's Brainfuck"
                db   0
                dwbe $2400+6*32+12
                db   "Output:"
                db   0
                dwbe $2400+18*32+9
                db   "Input (", $82, $83, $84, $85, $86, "):"
                db   0
                dwbe $2400+27*32+13
                db   $87, "=exit"
                db   0

                db   0                  ; end of all strings

                if $ - strings > 256
                    error "out of string space"
                endif

                ; VRAM addresses of horizontal bars
                ; (above/below BF code area in both name tables)
horz_bars_hi    dh $20e1, $2201, $24e1, $2601  ; high bytes
horz_bars_lo    dl $20e1, $2201, $24e1, $2601  ; low  bytes

; --- Main loop - common ------------------------------------------------------

main_loop       ; to avoid missing the flag being set by NMI routine,
                ; clear and read it using a single instruction
                asl nmi_done
                bcs +

                ; not 1st round after VBlank; only run mode-specific stuff if
                ; in run mode
                lda program_mode
                cmp #mode_run
                bne main_loop
                jsr ml_run
                jmp main_loop

+               ; 1st round after VBlank; run once-per-frame stuff

                lda pad_status          ; store previous joypad status
                sta prev_pad_status
                jsr read_joypad         ; read joypad

                jsr blink_cursors       ; make cursors blink
                inc frame_counter       ; advance frame counter

                jsr jump_engine         ; run mode-specific sub
                jmp main_loop

read_joypad     ; read 1st joypad or Famicom expansion port controller
                ; see https://www.nesdev.org/wiki/Controller_reading_code
                ; bits: A, B, select, start, up, down, left, right

                lda #1
                sta joypad1
                sta pad_status
                lsr a
                sta joypad1

-               lda joypad1
                and #%00000011
                cmp #1
                rol pad_status
                bcc -
                rts

blink_cursors   ; make cursors blink (set tile according to frame counter)
                lda frame_counter
                and #(1<<3)
                beq +
                lda #$80                ; solid block
+               sta sprite_data+0+1
                rts

jump_engine     ; jump to one sub depending on program mode
                ; note: RTS in the subs below will act like RTS in this sub
                ; see https://www.nesdev.org/wiki/Jump_table
                ; and https://www.nesdev.org/wiki/RTS_Trick

                ; push target address minus one, high byte first
                ldx program_mode
                lda jump_table_hi,x
                pha
                lda jump_table_lo,x
                pha
                ; pull address, low byte first; jump to address plus one
                rts

                ; jump table (high/low bytes)
jump_table_hi   dh ml_edit-1, ml_prep1-1, ml_prep2-1
                dh ml_run-1,  ml_input-1, ml_ended-1
jump_table_lo   dl ml_edit-1, ml_prep1-1, ml_prep2-1
                dl ml_run-1,  ml_input-1, ml_ended-1

; --- Main loop - editing BF program ------------------------------------------

ml_edit         lda #%10000000          ; show NT0
                sta ppu_ctrl_copy

                lda pad_status          ; react to buttons
                cmp prev_pad_status     ; skip if joypad status not changed
                beq +

                cmp #%00110000          ; select & start:
                beq try_to_run          ; try to run BF program

                cmp #%00010000          ; start: try to delete last character
                beq backspace

                ; try to enter an instruction if corresponding button pressed
                ldx #(8-1)
-               cmp edit_buttons,x
                beq enter_instr
                dex
                bpl -
+               rts

try_to_run      ; try to run BF program
                lda #mode_prep1
                sta program_mode
                rts

backspace       ; try to delete last instruction

                ; exit if no characters (1st column is always empty)
                ldy program_len
                dey
                bne +
                rts

+               ; if moving to previous line, delete empty characters too
                tya
                and #%00011111
                bne +
                dey
                dey

+               lda #$00                ; delete last instruction
                sta bf_program,y
                sty program_len

                lda #$00                ; tell NMI routine to redraw tile
                sta ppu_buf_value
                lda #1
                sta ppu_buf_count
                sty ppu_buf_adrlo
                lda #$21
                sta ppu_buf_adrhi       ; set this last to avoid race condition

                lda program_len         ; update input cursor sprite
                jmp upd_io_cursor       ; ends with RTS

enter_instr     ; try to enter a BF instruction (X)

                ; exit if no space (last 2 tiles taken by cursor, empty column)
                ldy program_len
                cpy #$fe
                bne +
                rts

+               ; tell NMI routine to redraw tile
                lda bf_instrs,x
                sta ppu_buf_value
                lda #1
                sta ppu_buf_count
                sty ppu_buf_adrlo
                lda #$21
                sta ppu_buf_adrhi       ; set this last to avoid race condition

                ; store instruction
                lda bf_instrs,x
                sta bf_program,y
                iny
                tya                     ; 1st & last columns are always empty
                and #%00011111
                cmp #%00011111
                bne +
                iny
                iny
+               sty program_len

                tya                     ; update input cursor sprite
                jmp upd_io_cursor       ; ends with RTS

                ; BF instructions and corresponding buttons in edit mode
bf_instrs       db '-', '+'
                db '<', '>'
                db '[', ']'
                db ',', '.'
edit_buttons    db %00000100, %00001000  ; down, up
                db %00000010, %00000001  ; left, right
                db %01000000, %10000000  ; B, A
                db %01100000, %10100000  ; select+B, select+A

; --- Main loop - prepare to run, part 1/2 ------------------------------------

ml_prep1        jsr find_brackets       ; find brackets (carry = error)
                bcs +

                ; brackets OK

                lda #$80                ; tell NMI routine to clear top half
                sta ppu_buf_count       ; of BF output area
                lda #$00
                sta ppu_buf_value
                sta ppu_buf_adrlo
                lda #$25
                sta ppu_buf_adrhi       ; set this last to avoid race condition

                lda #mode_prep2         ; proceed to next mode
                sta program_mode
                rts

+               ; error in brackets
                lda #mode_edit          ; return to editor
                sta program_mode
                rts

find_brackets   ; For each bracket ("[", "]") in BF program, store index of
                ; corresponding bracket in another array.
                ; out: brackets (array), carry (0 = no error, 1 = error)
                ; trashes: bottom half of stack ($0100-$017f)
                ; Note: an interrupt which uses stack must never fire during
                ; this sub.

                tsx                     ; store original stack pointer
                stx sp_copy
                ldx #$7f                ; use $0100-$017f for currently open
                txs                     ; brackets

                ldy #$ff                ; BF program index (preincremented)

                ; process BF program
-               iny
                cpy program_len
                beq +                   ; end of BF program
                lda bf_program,y
                cmp #'['
                beq ++
                cmp #']'
                beq +++
                jmp -

+               ; end of BF program
                tsx                     ; exit if missing "]" (SP != $7f)
                inx
                bpl ++++
                ldx sp_copy             ; restore original stack pointer
                txs
                clc                     ; return success
                rts

++              ; "["
                tsx                     ; exit if too many "["s open (SP = $ff)
                bmi ++++
                tya                     ; push current index
                pha
                jmp -                   ; next instruction

+++             ; "]"
                pla                     ; pull corresponding index
                tsx                     ; exit if missing "[" (SP = $80)
                bmi ++++
                sta brackets,y          ; store corresponding index here
                tax                     ; store this index at corresponding one
                tya
                sta brackets,x
                jmp -                   ; next instruction

++++            ldx sp_copy             ; restore original stack pointer
                txs
                sec                     ; return error
                rts

; --- Main loop - prepare to run, part 2/2 ------------------------------------

ml_prep2        lda #$ff
                sta sprite_data+0+0     ; hide edit cursor
                sta bf_pc               ; reset BF PC

                lda #$00
                sta bf_ram_ptr+0        ; reset low byte of BF RAM pointer
                sta keyb_x              ; reset keyboard cursor
                sta keyb_y

                lda #1
                sta output_len          ; 1st column is always empty
                jsr upd_io_cursor       ; update output cursor

                ; clear BF RAM and leave pointer at its beginning
                lda #>(bf_ram+$400)
                sta bf_ram_ptr+1
                lda #$00
                tay
                ldx #4
--              dec bf_ram_ptr+1
-               sta (bf_ram_ptr),y
                iny
                bne -
                dex
                bne --

                ; tell NMI routine to clear bottom half of BF output area
                lda #$00
                sta ppu_buf_value
                lda #$80
                sta ppu_buf_count
                sta ppu_buf_adrlo
                lda #$25
                sta ppu_buf_adrhi       ; set this last to avoid race condition

                lda #mode_run           ; proceed to next mode
                sta program_mode
                rts

; --- Main loop - BF program running ------------------------------------------

; the only part of the main loop that's run as frequently as possible instead
; of only once per frame

                align $100, $ff         ; for speed

ml_run          lda #%10000001          ; show NT1
                sta ppu_ctrl_copy

                lda pad_status          ; stop program if B pressed
                cmp #%01000000
                bne +
                jmp to_edit_mode        ; ends with RTS

+               ; exit if NMI routine hasn't flushed PPU buffer yet
                lda ppu_buf_adrhi
                bne ++

                ; increment BF PC; end program if end reached
                inc bf_pc
                ldx bf_pc
                cpx program_len
                bne +
                jmp end_program         ; ends with RTS

+               ; process instruction
                lda bf_program,x
                cmp #$2d                ; "-"
                beq dec_value
                cmp #$2b                ; "+"
                beq inc_value
                cmp #$3c                ; "<"
                beq dec_pointer
                cmp #$3e                ; ">"
                beq inc_pointer
                cmp #'['
                beq start_loop
                cmp #']'
                beq end_loop
                cmp #$2c                ; ","
                beq input
                cmp #'.'
                beq output
++              rts

dec_value       ; decrement value at RAM pointer
                lda #$ff
                jmp +

inc_value       ; increment value at RAM pointer
                lda #1
+               ldy #0
                clc
                adc (bf_ram_ptr),y
                sta (bf_ram_ptr),y
                rts

dec_pointer     ; decrement RAM pointer

                dec bf_ram_ptr+0
                lda bf_ram_ptr+0
                cmp #$ff
                bne ++

                dec bf_ram_ptr+1
                jmp +

inc_pointer     ; increment RAM pointer

                inc bf_ram_ptr+0
                bne ++
                inc bf_ram_ptr+1

+               lda bf_ram_ptr+1        ; clamp to $04-$07
                and #%00000011
                ora #%00000100
                sta bf_ram_ptr+1
++              rts

start_loop      ; jump to corresponding "]" if byte at RAM pointer is zero
                ldy #0
                lda (bf_ram_ptr),y
                bne ++
                jmp +

end_loop        ; jump to corresponding "[" if byte at RAM pointer is nonzero
                ldy #0
                lda (bf_ram_ptr),y
                beq ++
+               ldx bf_pc
                lda brackets,x
                sta bf_pc
++              rts

input           ; read input from virtual keyboard to byte at RAM pointer
                jsr upd_keyb_cursor
                lda #mode_input
                sta program_mode
                rts

output          ; output byte at RAM pointer to output area

                ldy #0
                lda (bf_ram_ptr),y      ; if newline ($0a)...
                cmp #$0a
                bne +
                lda output_len          ; move output cursor to next line
                clc
                adc #32
                and #%11100000
                beq end_program         ; if output area full, end program
                sta output_len          ; (code ends with RTS)
                inc output_len          ; 1st column is always empty
                jmp ++

+               sta ppu_buf_value       ; otherwise output RAM value via NMI
                lda #1
                sta ppu_buf_count
                lda output_len
                sta ppu_buf_adrlo
                inc output_len

                lda output_len          ; 1st & last columns are always empty
                and #%00011111
                cmp #%00011111
                bne +
                inc output_len
                inc output_len

+               lda #$25
                sta ppu_buf_adrhi       ; set this last to avoid race condition
                lda output_len          ; if output area full, end program
                cmp #1
                beq end_program         ; ends with RTS

++              lda output_len          ; update output cursor sprite
                jmp upd_io_cursor       ; ends with RTS

end_program     ; end program

                lda #(27*8-1)           ; move cursor next to "B=exit" text
                sta sprite_data+0+0
                lda #(19*8)
                sta sprite_data+0+3

                lda #mode_ended
                sta program_mode
                rts

; --- Main loop - BF program waiting for input --------------------------------

ml_input        ; ignore buttons if anything was pressed on last frame
                lda prev_pad_status
                bne +

                lda pad_status          ; react to buttons
                bmi keyb_input          ; A
                asl a
                bmi to_edit_mode        ; B
                asl a
                asl a
                asl a
                bmi keyb_up             ; up
                asl a
                bmi keyb_down           ; down
                asl a
                bmi keyb_left           ; left
                bne keyb_right          ; right
+               rts

keyb_input      ; store character at cursor to BF RAM

                lda keyb_y              ; get character code
                asl a
                asl a
                asl a
                asl a
                clc
                adc #$20
                ora keyb_x

                cmp #$7f                ; change newline symbol to newline
                bne +
                lda #$0a

+               ldy #0                  ; store character code
                sta (bf_ram_ptr),y
                lda #mode_run           ; return to run mode
                sta program_mode
                rts

keyb_up         ldx keyb_y
                dex
                bpl +
                ldx #(6-1)
                jmp +

keyb_down       ldx keyb_y
                inx
                cpx #6
                bne +
                ldx #0
+               stx keyb_y
                jmp upd_keyb_cursor     ; ends with RTS

keyb_left       ldx keyb_x
                dex
                jmp +

keyb_right      ldx keyb_x
                inx
+               txa
                and #%00001111
                sta keyb_x
                jmp upd_keyb_cursor     ; ends with RTS

upd_keyb_cursor ; update keyboard cursor sprite

                lda keyb_y              ; Y position
                asl a
                asl a
                asl a
                clc
                adc #(20*8-1)
                sta sprite_data+0+0

                lda keyb_x              ; X position
                asl a
                asl a
                asl a
                clc
                adc #(8*8)
                sta sprite_data+0+3
                rts

; --- Main loop - BF program ended --------------------------------------------

ml_ended        lda pad_status          ; if B pressed, switch to edit mode
                and #%01000000
                bne to_edit_mode        ; ends with RTS
                rts

to_edit_mode    ; switch to edit mode

                lda program_len
                jsr upd_io_cursor

                lda #mode_edit
                sta program_mode
                rts

upd_io_cursor   ; update input/output cursor sprite
                ; in: A = input/output length

                pha                     ; Y position
                and #%11100000
                lsr a
                lsr a
                clc
                adc #(8*8-1)
                sta sprite_data+0+0

                pla                     ; X position
                asl a
                asl a
                asl a
                sta sprite_data+0+3
                rts

; --- Interrupt routines ------------------------------------------------------

                align $100, $ff         ; for speed

nmi             pha                     ; push A, X, Y
                txa
                pha
                tya
                pha

                bit ppu_status          ; clear ppu_scroll/ppu_addr latch

                lda #$00                ; do OAM DMA
                sta oam_addr
                lda #>sprite_data
                sta oam_dma

                jsr flush_ppu_buf       ; flush PPU buffer
                jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask

                ; set flag to let once-per-frame stuff run
                ; (other negative values won't do)
                lda #%10000000
                sta nmi_done

                pla                     ; pull Y, X, A
                tay
                pla
                tax
                pla

irq             rti                     ; IRQ unused

flush_ppu_buf   ; flush PPU buffer if address != $00xx

                ldy ppu_buf_adrhi       ; exit if empty
                beq ++

                lda ppu_buf_adrlo
                jsr set_ppu_addr        ; Y*$100+A -> PPU address

                ; count>>1 -> Y, count&1 -> carry, value -> A
                lda ppu_buf_count
                lsr a
                tay
                lda ppu_buf_value

                ; write value to PPU ppu_buf_count times, twice per round
                bcc ppu_write_loop
                iny                     ; odd count
                jmp +
ppu_write_loop  sta ppu_data
+               sta ppu_data
                dey
                bne ppu_write_loop

                lda #$00
                sta ppu_buf_adrhi
++              rts

set_ppu_addr    sty ppu_addr            ; Y*$100+A -> PPU address
                sta ppu_addr
                rts

set_ppu_regs    lda #$00                ; reset PPU scroll
                sta ppu_scroll
                sta ppu_scroll
                lda ppu_ctrl_copy       ; set ppu_ctrl from copy
                sta ppu_ctrl
                lda #%00011110          ; show background & sprites
                sta ppu_mask
                rts

; --- Interrupt vectors -------------------------------------------------------

                pad $fffa, $ff
                dw nmi, reset, irq      ; IRQ unused

; --- CHR ROM -----------------------------------------------------------------

                base $0000
                incbin "chr.bin"
                pad $2000, $ff
