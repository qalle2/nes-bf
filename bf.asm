; Qalle's Brainfuck (NES, ASM6)

; --- Constants ---------------------------------------------------------------

; Notes:
; - Only the first sprite slot is actually used, and the other slots only need
;   their Y positions to be set to $ff, so the OAM page accommodates many other
;   variables at addresses not divisible by 4 ($05-$07, $09-$0b, $0d-$0f, ...).
; - "VRAM buffer" = what to write to PPU on next VBlank.
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
vram_buf_adrhi  equ $0f    ; VRAM buffer - high byte of address (0 = nothing)
vram_buf_adrlo  equ $11    ; VRAM buffer - low  byte of address
vram_buf_value  equ $12    ; VRAM buffer - value
vram_buf_count  equ $13    ; VRAM buffer - repeat count
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

; joypad button bitmasks
pad_a           equ 1<<7  ; A
pad_b           equ 1<<6  ; B
pad_sl          equ 1<<5  ; select
pad_st          equ 1<<4  ; start
pad_u           equ 1<<3  ; up
pad_d           equ 1<<2  ; down
pad_l           equ 1<<1  ; left
pad_r           equ 1<<0  ; right

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
                jsr init_ppu_mem        ; initialize PPU memory

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

                lda #$00                ; clear sprites, variables and BF code
                tax
-               sta sprite_data,x
                sta bf_program,x
                inx
                bne -

                lda #$ff                ; hide sprites
                ldx #0
-               sta sprite_data,x
                inx
                inx
                inx
                inx
                bne -

                inc program_len         ; 1st column is always empty

                rts

init_ppu_mem    ; initialize PPU memory

                ; copy palette (while still in VBlank)
                ldy #$3f
                lda #$00
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ldy #8                  ; copy same colors to all subpalettes
--              ldx #0
-               lda palette,x
                sta ppu_data
                inx
                cpx #4
                bne -
                dey
                bne --

                ; clear NT/AT 0/1
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
--              inx
                ldy strings,x           ; VRAM address high
                beq ++                  ; 0 = end of all strings
                inx
                lda strings,x           ; VRAM address low
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
-               inx
                lda strings,x           ; byte (0 = end of string)
                beq +
                sta ppu_data
                bne -                   ; unconditional
+               beq --                  ; next string (unconditional)

++              ; draw horizontal bars in NT0 & NT1
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

fill_vram       ; write A to VRAM Y times; used by init_ppu_mem
                sta ppu_data
                dey
                bne fill_vram
                rts

palette         ; copied to all subpalettes
                ; note: 2nd color of 1st sprite subpal blinks, used for cursors
                db $0f  ; background (black)
                db $30  ; foreground (white)
                db $28  ; highlight  (yellow)
                db $25  ; unused     (pink)

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

                lda program_mode        ; not 1st round after VBlank;
                cmp #mode_run           ; only run mode-specific stuff
                bne main_loop           ; in run mode
                jsr ml_run
                jmp main_loop

+               jsr once_per_frame      ; 1st round after VBlank;
                jmp main_loop           ; run once-per-frame stuff

once_per_frame  ; stuff that's done only once per frame

                lda pad_status          ; store previous joypad status
                sta prev_pad_status

                ; read 1st joypad or Famicom expansion port controller
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

                ; make cursors blink
                lda frame_counter       ; set tile according to frame counter
                and #(1<<3)
                beq +
                lda #$80                ; solid block
+               sta sprite_data+0+1

                inc frame_counter       ; advance frame counter

                ; jump to one sub depending on program mode
                ; note: RTS in the subs below will act like RTS in this sub
                ; see https://www.nesdev.org/wiki/Jump_table
                ; and https://www.nesdev.org/wiki/RTS_Trick

                ldx program_mode        ; push target address minus one,
                lda jump_table_hi,x     ; high byte first
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
                beq char_entry_end

                cmp #(pad_sl|pad_st)
                beq try_to_run

                cmp #pad_st
                beq backspace

                ldx #(8-1)
-               lda edit_buttons,x      ; enter instruction if
                cmp pad_status          ; corresponding button pressed
                beq enter_instr
                dex
                bpl -
                bmi char_entry_end      ; unconditional

try_to_run      lda #mode_prep1
                sta program_mode
                rts

backspace       ldy program_len         ; if program_len >= 2
                dey                     ; (1st column is always empty)...
                beq char_entry_end

                tya                     ; 1st & last columns are always empty
                and #%00011111
                bne +
                dey
                dey

+               lda #$00                ; delete last instruction
                sta bf_program,y
                sty program_len

                lda #$00                ; tell NMI routine to redraw tile
                sta vram_buf_value
                lda #1
                sta vram_buf_count
                sty vram_buf_adrlo
                lda #$21
                sta vram_buf_adrhi      ; set this last to avoid race condition

                bne char_entry_end      ; unconditional

enter_instr     ldy program_len         ; if program_len < $fe (last column is
                cpy #$fe                ; always empty, cursor takes up
                beq char_entry_end      ; last tile in input area)...

                lda bf_instrs,x         ; tell NMI routine to redraw tile
                sta vram_buf_value
                lda #1
                sta vram_buf_count
                sty vram_buf_adrlo
                lda #$21
                sta vram_buf_adrhi      ; set this last to avoid race condition

                lda bf_instrs,x         ; store instruction
                sta bf_program,y
                iny
                tya                     ; 1st & last columns are always empty
                and #%00011111
                cmp #%00011111
                bne +
                iny
                iny
+               sty program_len
                ; fall through

char_entry_end  lda program_len         ; update input cursor sprite
                jmp upd_io_cursor       ; ends with RTS

                ; BF instructions and corresponding buttons in edit mode
edit_buttons    db pad_d, pad_u, pad_l, pad_r, pad_b, pad_a
                db pad_sl|pad_b, pad_sl|pad_a
bf_instrs       db '-', '+', '<', '>', '[', ']', ',', '.'

; --- Main loop - prepare to run, part 1/2 ------------------------------------

ml_prep1        ; For each bracket in BF program, store index of corresponding
                ; bracket in another array.
                ; - in: bf_program (array), program_len
                ; - out: brackets (array)
                ; - changes: program_mode (according to whether brackets were
                ;   valid)
                ; - trashes: sp_copy, bottom half of stack ($0100-$017f)
                ; Note: an interrupt which uses stack must never fire during
                ; this sub.

                tsx                     ; store original stack pointer
                stx sp_copy
                ldx #$7f                ; use bottom half of stack for
                txs                     ; currently open brackets
                ldy #$ff                ; Y = current program index (preinc'd)

-               iny                     ; next instruction
                cpy program_len
                bne +

                tsx                     ; end of BF program
                inx
                bpl bracket_error       ; missing "]" (SP != $7f)
                lda #mode_prep2         ; brackets valid; proceed
                sta program_mode
                bpl brackets_ok         ; unconditional

+               lda bf_program,y
                cmp #'['
                bne +
                tsx
                bmi bracket_error       ; max number of "["s already open
                tya                     ; (SP = $ff)
                pha                     ; push current index
                jmp -

+               cmp #']'
                bne -
                pla                     ; pull corresponding index
                tsx
                bmi bracket_error       ; missing "[" (SP = $80)
                sta brackets,y          ; store corresponding index here
                tax                     ; store this index at corresponding one
                tya
                sta brackets,x
                jmp -

bracket_error   lda #mode_edit          ; error in brackets; return to editor
                sta program_mode
                bpl +                   ; unconditional

brackets_ok     ; tell NMI routine to clear top half of BF output area
                lda #$80
                sta vram_buf_count
                asl a
                sta vram_buf_value
                sta vram_buf_adrlo
                lda #$25
                sta vram_buf_adrhi      ; set this last to avoid race condition

+               ldx sp_copy             ; restore original SP
                txs
                rts

; --- Main loop - prepare to run, part 2/2 ------------------------------------

ml_prep2        ldx #$ff
                stx sprite_data+0+0     ; hide edit cursor
                stx bf_pc               ; reset BF PC
                inx
                stx bf_ram_ptr+0        ; reset low byte of BF RAM ptr
                stx keyb_x              ; reset keyboard cursor
                stx keyb_y
                inx
                stx output_len          ; 1st column is always empty

                ; clear BF RAM and leave pointer to start of it
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
                lda #$80
                sta vram_buf_count
                sta vram_buf_adrlo
                asl a
                sta vram_buf_value
                lda #$25
                sta vram_buf_adrhi      ; set this last to avoid race condition

                lda #mode_run           ; switch to run mode
                sta program_mode
                rts

; --- Main loop - BF program running ------------------------------------------

; the only part of the main loop that's run as frequently as possible instead
; of only once per frame

                align $100, $ff         ; for speed

ml_run          lda #%10000001          ; show NT1
                sta ppu_ctrl_copy

                lda pad_status          ; stop program if B pressed
                cmp #pad_b
                bne +
                jmp to_edit_mode        ; ends with RTS

+               ; exit if NMI routine hasn't flushed VRAM buffer yet
                lda vram_buf_adrhi
                beq +
                rts

+               ldx bf_pc               ; incremented PC -> X
                inx                     ; end program if end reached
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
                beq dec_ptr
                cmp #$3e                ; ">"
                beq inc_ptr
                cmp #'['
                beq start_loop
                cmp #']'
                beq end_loop
                cmp #$2c                ; ","
                beq input
                cmp #'.'
                beq output
                bne instr_done          ; blank; unconditional

dec_value       lda #$ff                ; decrement byte at RAM pointer
                bne +                   ; unconditional

inc_value       lda #1                  ; increment byte at RAM pointer
+               ldy #0
                clc
                adc (bf_ram_ptr),y
                sta (bf_ram_ptr),y
                jmp instr_done

dec_ptr         dec bf_ram_ptr+0        ; decrement RAM pointer
                lda bf_ram_ptr+0
                cmp #$ff
                bne instr_done
                dec bf_ram_ptr+1
                bpl +                   ; unconditional

inc_ptr         inc bf_ram_ptr+0        ; increment RAM pointer
                bne instr_done
                inc bf_ram_ptr+1
+               lda bf_ram_ptr+1        ; high byte must be %000001xx
                and #%00000011
                ora #%00000100
                sta bf_ram_ptr+1
                bpl instr_done          ; unconditional

start_loop      ; jump to corresponding "]" if byte at RAM pointer is zero
                ldy #0
                lda (bf_ram_ptr),y
                bne instr_done
                beq +                   ; unconditional

end_loop        ; jump to corresponding "[" if byte at RAM pointer is nonzero
                ldy #0
                lda (bf_ram_ptr),y
                beq instr_done
+               lda brackets,x
                tax
                jmp instr_done          ; unconditional

input           ; read input from virtual keyboard to byte at RAM pointer
                lda #mode_input
                sta program_mode
                bpl instr_done          ; unconditional

output          ; output byte at RAM pointer to output area

                ldy #0
                lda (bf_ram_ptr),y      ; if newline ($0a)...
                cmp #$0a
                bne +
                lda output_len          ; move output cursor to next line
                adc #(32-1)             ; carry is always set
                and #%11100000
                beq end_program         ; if output area full, end program
                sta output_len          ; (code ends with RTS)
                inc output_len          ; 1st column is always empty
                bne instr_done          ; unconditional

+               sta vram_buf_value      ; otherwise output RAM value via NMI
                lda #1
                sta vram_buf_count
                lda output_len
                sta vram_buf_adrlo
                inc output_len

                lda output_len          ; 1st & last columns are always empty
                and #%00011111
                cmp #%00011111
                bne +
                inc output_len
                inc output_len

+               lda #$25
                sta vram_buf_adrhi      ; set this last to avoid race condition
                lda output_len          ; if output area full, end program
                cmp #1
                beq end_program         ; ends with RTS
                ; fall through

instr_done      stx bf_pc               ; store new PC
                lda output_len          ; update output cursor sprite
                jmp upd_io_cursor       ; ends with RTS

end_program     lda #mode_ended         ; end program
                sta program_mode
                lda #(27*8-1)           ; move cursor next to "B=exit" text
                sta sprite_data+0+0
                lda #(19*8)
                sta sprite_data+0+3
                rts

; --- Main loop - BF program waiting for input --------------------------------

ml_input        ; ignore buttons if anything was pressed on last frame
                lda prev_pad_status
                bne upd_keyb_cursor     ; update cursor sprite (ends with RTS)

                lda pad_status          ; react to buttons
                bmi keyb_input          ; A
                asl a
                bmi to_edit_mode        ; B (ends with RTS)
                asl a
                asl a
                asl a
                bmi keyb_up             ; up
                asl a
                bmi keyb_down           ; down
                asl a
                bmi keyb_left           ; left
                bne keyb_right          ; right
                beq upd_keyb_cursor     ; unconditional; ends with RTS

keyb_right      ldx keyb_x
                inx
                bpl +                   ; unconditional

keyb_left       ldx keyb_x
                dex
+               txa
                and #%00001111
                sta keyb_x
                bpl upd_keyb_cursor     ; unconditional; ends with RTS

keyb_down       ldx keyb_y
                inx
                cpx #6
                bne +
                ldx #0
                beq +                   ; unconditional

keyb_up         dec keyb_y
                bpl ++
                ldx #(6-1)
+               stx keyb_y
++              jmp upd_keyb_cursor     ; ends with RTS

keyb_input      ; store character at cursor to BF RAM

                lda keyb_y              ; get character code
                asl a
                asl a
                asl a
                asl a
                adc #$20                ; carry is always clear
                ora keyb_x

                cmp #$7f                ; change newline symbol to newline
                bne +
                lda #$0a

+               ldy #0                  ; store character code
                sta (bf_ram_ptr),y
                lda #mode_run           ; return to run mode
                sta program_mode
                rts

upd_keyb_cursor ; update keyboard cursor sprite

                lda keyb_y              ; Y position
                asl a
                asl a
                asl a
                adc #(20*8-1)           ; carry is always clear
                sta sprite_data+0+0

                lda keyb_x              ; X position
                asl a
                asl a
                asl a
                adc #(8*8)              ; carry is always clear
                sta sprite_data+0+3
                rts

; --- Main loop - BF program ended --------------------------------------------

ml_ended        lda pad_status          ; if B pressed, switch to edit mode
                and #pad_b
                bne to_edit_mode        ; ends with RTS
                rts

; --- Main loop - Subs used in many modes -------------------------------------

to_edit_mode    lda #mode_edit          ; switch to edit mode
                sta program_mode
                rts

upd_io_cursor   ; update input/output cursor sprite
                ; in: A = input/output length

                pha                     ; Y position
                and #%11100000
                lsr a
                lsr a
                adc #(8*8-1)            ; carry is always clear
                sta sprite_data+0+0

                pla                     ; X position
                asl a
                asl a
                asl a
                sta sprite_data+0+3
                rts

; --- Subs involving PPU registers (used in init & NMI routine) ---------------

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

                ; flush VRAM buffer if address != $00xx
                ldy vram_buf_adrhi
                beq +
                lda vram_buf_adrlo
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                lda vram_buf_value
                ldy vram_buf_count
                jsr fill_vram           ; write A Y times
                lda #$00
                sta vram_buf_adrhi

+               jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask

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

; --- Interrupt vectors -------------------------------------------------------

                pad $fffa, $ff
                dw nmi, reset, irq      ; IRQ unused

; --- CHR ROM -----------------------------------------------------------------

                base $0000
                incbin "chr.bin"
                pad $2000, $ff
