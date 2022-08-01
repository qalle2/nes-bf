; Qalle's Brainfuck (NES, ASM6)

; TODO:
; - delete these unused variables: pt_data_ptr, pt_common, pt_common_pos
; - flowchart of modes

; --- Constants ---------------------------------------------------------------

; notes:
; - only the first sprite slot is actually used, and the other slots only need
;   their Y positions to be set to $ff, so the OAM page accommodates many other
;   variables at addresses
;   not divisible by 4 ($05-$07, $09-$0b, $0d-$0f, ...)
; - "VRAM buffer" = what to write to PPU on next VBlank
; - bottom half of stack ($0100-$017f) is used for other purposes
; - nmi_done: did the NMI routine just run? used for once-per-frame stuff;
;   set by NMI, read and cleared at the start of main loop

; RAM
sprite_data     equ $00    ; OAM page ($100 bytes, see above)
pt_data_ptr     equ $05    ; pointer to pt_data (2 bytes)
program_mode    equ $07    ; see constants below
bf_ram_ptr      equ $09    ; pointer to bf_ram (2 bytes)
nmi_done        equ $0b    ; see above ($00 = no, $80 = yes)
ppu_ctrl_copy   equ $0d    ; copy of ppu_ctrl
frame_counter   equ $0e    ; for blinking cursors
pad_status      equ $0f    ; joypad status
prev_pad_status equ $11    ; joypad status on previous frame
vram_buf_adrhi  equ $12    ; VRAM buffer - high byte of address (0 = nothing)
vram_buf_adrlo  equ $13    ; VRAM buffer - low  byte of address
vram_buf_value  equ $15    ; VRAM buffer - value
program_len     equ $16    ; length of Brainfuck program
bf_pc           equ $17    ; program counter of BF program (preincremented)
output_len      equ $19    ; number of characters printed by BF program
keyb_x          equ $1a    ; cursor X position on virtual keyboard (0-15)
keyb_y          equ $1b    ; cursor Y position on virtual keyboard (0-5)
stack_ptr_copy  equ $1d    ; copy of stack pointer
pt_common       equ $1e    ; pattern table data - most common byte of tile
pt_common_pos   equ $1f    ; pattern table data - bits denote positions in tile
bf_program      equ $0200  ; Brainfuck program ($100 bytes)
brackets        equ $0300  ; target addresses of "[" and "]" ($100 bytes)
bf_ram          equ $0400  ; RAM of BF program ($400 bytes; must be at $xx00)

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

; colors
color_bg        equ $0f  ; background (black)
color_fg        equ $30  ; foreground (white)
color_hilite    equ $28  ; highlight  (yellow)
color_unused    equ $25  ; unused     (pink)

; tiles
tile_block      equ $80  ; solid block
tile_hbar       equ $81  ; horizontal bar
tile_uarr       equ $82  ; highlight color - up    arrow
tile_darr       equ $83  ; highlight color - down  arrow
tile_larr       equ $84  ; highlight color - left  arrow
tile_rarr       equ $85  ; highlight color - right arrow
tile_aupper     equ $86  ; highlight color - "A"
tile_bupper     equ $87  ; highlight color - "B"
tile_a          equ $88  ; highlight color - "a"
tile_c          equ $89  ; highlight color - "c"
tile_e          equ $8a  ; highlight color - "e"
tile_l          equ $8b  ; highlight color - "l"
tile_r          equ $8c  ; highlight color - "r"
tile_s          equ $8d  ; highlight color - "s"
tile_t          equ $8e  ; highlight color - "t"

; values for program_mode (must be 0, 1, ... because they're used as indexes
; to jump table)
mode_edit       equ 0    ; editing BF program (must be 0)
mode_prep_run1  equ 1    ; preparing to run BF program, part 1/2
mode_prep_run2  equ 2    ; preparing to run BF program, part 2/2
mode_run        equ 3    ; BF program running
mode_input      equ 4    ; BF program waiting for input
mode_ended      equ 5    ; BF program finished

; misc
blink_rate      equ 3    ; cursor blink rate (0 = fastest, 7 = slowest)

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
                ;
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

                lda #$00                ; clear sprite/variable page, BF code
                tax
-               sta sprite_data,x
                sta bf_program,x
                inx
                bne -

                lda #$ff                ; hide all sprites (set Y positions
-               sta sprite_data,x       ; to $ff; X is still 0)
                inx
                inx
                inx
                inx
                bne -

                ; init nonzero variable (1st column is always empty)
                inc program_len

                jsr wait_vbl_start      ; wait until next VBlank starts

                ; set up palette (while still in VBlank)
                ldy #$3f
                jsr set_ppu_addr_pg     ; 0 -> A; Y*$100+A -> PPU address
                ;
                ldy #8                  ; copy same colors to all subpalettes
--              ldx #0
-               lda palette,x
                sta ppu_data
                inx
                cpx #4
                bne -
                dey
                bne --

                ldy #$20                ; fill NT & AT 0 & 1 with $00
                jsr set_ppu_addr_pg     ; 0 -> A; Y*$100+A -> PPU address
                ldx #8
                tay
-               jsr fill_vram           ; write A Y times
                dex
                bne -

                ldx #$ff                ; copy strings to NT0 (edit mode)
--              inx                     ; and NT1 (run mode)
                ldy strings,x           ; VRAM address high
                beq strings_end         ; 0 = end of all strings
                inx
                lda strings,x           ; VRAM address low
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
-               inx
                lda strings,x           ; byte (0 = end of string)
                beq +
                sta ppu_data
                bne -                   ; unconditional
+               beq --                  ; next string (unconditional)

strings_end     ldx #(4-1)              ; draw horizontal bars in NT0 & NT1
-               ldy horz_bars_hi,x
                lda horz_bars_lo,x
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ldy #30
                lda #tile_hbar
                jsr fill_vram           ; write A Y times
                dex
                bpl -

                ldy #$26                ; draw virtual keyboard in NT1
                lda #$78
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ;
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

                jsr wait_vbl_start      ; wait until next VBlank starts

                lda #%10000000          ; enable NMI, show name table 0
                sta ppu_ctrl_copy
                jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask

                jmp main_loop

wait_vbl_start  bit ppu_status          ; wait until next VBlank starts
-               bit ppu_status
                bpl -
                rts

fill_vram       sta ppu_data            ; write A to VRAM Y times
                dey
                bne fill_vram
                rts

palette         ; copied to all subpalettes
                ; note: 2nd color of 1st sprite subpal blinks, used for cursors
                db color_bg, color_fg, color_hilite, color_unused

macro nt_addr _nt, _y, _x
                ; output name table address ($2000-$27bf), high byte first
                dh $2000+(_nt*$400)+(_y*$20)+(_x)
                dl $2000+(_nt*$400)+(_y*$20)+(_x)
endm

strings         ; each string: PPU address high/low, characters, terminator (0)
                ; address high 0 terminates string data
                ;
                nt_addr 0, 3, 7
                db "Qalle's Brainfuck", 0
                nt_addr 0, 6, 12
                db "Program:", 0
                nt_addr 0, 18, 3
                db tile_uarr, "=+ ", tile_darr, "=-  "
                db tile_larr, "=< ", tile_rarr, "=>  "
                db tile_bupper, "=[ ", tile_aupper, "=]", 0
                nt_addr 0, 20, 5
                db tile_s, tile_e, tile_l, tile_e, tile_c, tile_t, "+"
                db tile_bupper, "=, "
                db tile_s, tile_e, tile_l, tile_e, tile_c, tile_t, "+"
                db tile_aupper, "=.", 0
                nt_addr 0, 22, 2
                db tile_s, tile_t, tile_a, tile_r, tile_t, "=BkSp "
                db tile_s, tile_e, tile_l, tile_e, tile_c, tile_t, "+"
                db tile_s, tile_t, tile_a, tile_r, tile_t, "=run", 0
                ;
                nt_addr 1, 3, 7
                db "Qalle's Brainfuck", 0
                nt_addr 1, 6, 12
                db "Output:", 0
                nt_addr 1, 18, 9
                db "Input (", tile_uarr, tile_darr, tile_larr, tile_rarr
                db tile_aupper, "):", 0
                nt_addr 1, 27, 13
                db tile_bupper, "=exit", 0
                ;
                db 0  ; end of all strings

                if $ - strings > 256
                    error "out of string space"
                endif

                ; VRAM addresses of horizontal bars
                ; (above/below Brainfuck code area in both name tables)
horz_bars_hi    dh $20e1, $2201, $24e1, $2601  ; high bytes
horz_bars_lo    dl $20e1, $2201, $24e1, $2601  ; low  bytes

; --- Main loop - common ------------------------------------------------------

main_loop       ; to avoid missing the flag being set by NMI routine,
                ; clear and read it using a single instruction
                asl nmi_done
                bcs +
                ;
                lda program_mode        ; not first round after VBlank;
                cmp #mode_run           ; only run mode-specific stuff
                bne main_loop           ; in run mode
                jsr ml_run
                jmp main_loop
                ;
+               jsr once_per_frame      ; first round after VBlank;
                jmp main_loop           ; run once-per-frame stuff

once_per_frame  ; stuff that's done only once per frame

                lda pad_status          ; store previous joypad status
                sta prev_pad_status

                ; read first joypad or Famicom expansion port controller
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

                lda frame_counter       ; set cursor tile according to
                and #(1<<blink_rate)    ; frame counter
                beq +
                lda #tile_block
+               sta sprite_data+0+1

                inc frame_counter       ; advance frame counter

                ; jump to one sub depending on program mode
                ; note: RTS in the subs below will act like RTS in this sub
                ; see https://www.nesdev.org/wiki/Jump_table
                ; and https://www.nesdev.org/wiki/RTS_Trick
                ;
                ldx program_mode        ; push target address minus one,
                lda jump_table_hi,x     ; high byte first
                pha
                lda jump_table_lo,x
                pha
                ; pull address, low byte first; jump to address plus one
                rts

jump_table_hi   dh ml_edit     -1       ; jump table - high bytes
                dh ml_prep_run1-1
                dh ml_prep_run2-1
                dh ml_run      -1
                dh ml_input    -1
                dh ml_ended    -1
                ;
jump_table_lo   dl ml_edit     -1       ; jump table - low bytes
                dl ml_prep_run1-1
                dl ml_prep_run2-1
                dl ml_run      -1
                dl ml_input    -1
                dl ml_ended    -1

; --- Main loop - editing Brainfuck program -----------------------------------

ml_edit         lda pad_status          ; react to buttons
                cmp prev_pad_status     ; skip if joypad status not changed
                beq char_entry_end
                ;
                cmp #(pad_sl|pad_st)
                beq run_program
                ;
                cmp #pad_st
                beq del_last_char       ; backspace
                ;
                ldx #(bf_instrs_end-bf_instrs-1)
-               lda edit_buttons,x      ; enter instruction if
                cmp pad_status          ; corresponding button pressed
                beq enter_instr
                dex
                bpl -

char_entry_end  lda program_len         ; update input cursor sprite
                jmp upd_io_cursor       ; ends with RTS

run_program     inc program_mode        ; switch to mode_prepare_run1
                rts

del_last_char   ldy program_len         ; if program_len >= 2
                dey                     ; (1st column is always empty)...
                beq char_entry_end
                ;
                tya                     ; 1st & last columns are always empty
                and #%00011111
                bne +
                dey
                dey
                ;
+               lda #$00                ; delete last instruction,
                sta bf_program,y        ; tell NMI routine to redraw
                sta vram_buf_value
                sty program_len
                sty vram_buf_adrlo
                lda #$21
                sta vram_buf_adrhi      ; set this last to avoid race condition
                ;
                bne char_entry_end      ; unconditional

enter_instr     ldy program_len         ; if program_len < $fe (last column is
                cpy #$fe                ; always empty, cursor takes up
                beq char_entry_end      ; last tile in input area)...
                ;
                lda bf_instrs,x         ; add instruction specified by X,
                sta bf_program,y        ; tell NMI routine to draw
                sta vram_buf_value
                sty vram_buf_adrlo
                iny
                ;
                tya                     ; 1st & last columns are always empty
                and #%00011111
                cmp #%00011111
                bne +
                iny
                iny
                ;
+               sty program_len
                lda #$21
                sta vram_buf_adrhi      ; set this last to avoid race condition
                ;
                bne char_entry_end      ; unconditional

                ; Brainfuck instructions and corresponding buttons in edit mode
edit_buttons    db pad_u, pad_d, pad_l, pad_r, pad_b, pad_a
                db pad_sl|pad_b, pad_sl|pad_a
bf_instrs       db "+", "-", "<", ">", "[", "]", ",", "."
bf_instrs_end

; --- Main loop - prepare to run, part 1/2 ------------------------------------

ml_prep_run1    ; for each bracket in Brainfuck program, store index of
                ; corresponding bracket in another array
                ; - in: bf_program (array), program_len
                ; - out: brackets (array)
                ; - changes: program_mode (according to whether brackets were
                ;   valid)
                ; - trashes: stack_ptr_copy, bottom half of stack ($0100-$017f)
                ; note: an interrupt which uses stack must never fire during
                ; this sub
                ;
                tsx                     ; store original stack pointer
                stx stack_ptr_copy
                ldx #$7f                ; use bottom half of stack for
                txs                     ; currently open brackets
                ldy #$ff                ; Y = current program index
                ;                       ; (preincremented)
-               iny                     ; next instruction
                cpy program_len
                bne +
                ;
                tsx                     ; end of Brainfuck program
                inx
                bpl bracket_error       ; missing "]" (SP != $7f)
                inc program_mode        ; brackets valid; proceed
                bpl bracket_exit        ; unconditional
                ;
+               lda bf_program,y
                cmp #$5b                ; "["
                bne +
                tsx
                bmi bracket_error       ; max number of "["s already open
                tya                     ; (SP = $ff)
                pha                     ; push current index
                jmp -
                ;
+               cmp #$5d                ; "]"
                bne -
                pla                     ; pull corresponding index
                tsx
                bmi bracket_error       ; missing "[" (SP = $80)
                sta brackets,y          ; store corresponding index here
                tax                     ; store this index at corresponding one
                tya
                sta brackets,x
                jmp -
                ;
bracket_error   dec program_mode        ; error in brackets; return to editor
bracket_exit    ldx stack_ptr_copy      ; restore original SP
                txs
                rts

; --- Main loop - prepare to run, part 2/2 ------------------------------------

ml_prep_run2    ldy #$ff                ; hide edit cursor, reset BF PC
                sty sprite_data+0+0
                sty bf_pc

                iny                     ; reset low byte of BF RAM ptr,
                sty bf_ram_ptr+0        ; output length, keyboard cursor
                sty output_len
                inc output_len          ; 1st column is always empty
                sty keyb_x
                sty keyb_y

                lda #>(bf_ram+$400)     ; set pointer to after end of BF RAM
                sta bf_ram_ptr+1

--              tya                     ; clear BF RAM, leave pointer at start
                dec bf_ram_ptr+1
-               sta (bf_ram_ptr),y
                iny
                bne -
                lda bf_ram_ptr+1
                cmp #>bf_ram
                bne --

                lda #%10000001          ; show run mode NT
                sta ppu_ctrl_copy

                inc program_mode        ; switch to run mode

rts1            rts

; --- Main loop - Brainfuck program running -----------------------------------

; the only part of the main loop that's run as frequently as possible instead
; of only once per frame

ml_run          lda pad_status          ; stop program if B pressed
                cmp #pad_b
                bne +
                jmp to_edit_mode        ; ends with RTS

+               lda vram_buf_adrhi      ; wait until NMI routine has flushed
                bne rts1                ; VRAM buffer

                tay                     ; always 0; indirect addressing only

                ldx bf_pc               ; incremented PC -> X
                inx                     ; end program if necessary
                cpx program_len
                beq to_ended_mode       ; ends with RTS

                lda bf_program,x        ; instruction -> A

                ; process current instruction (may also be 0 if we're on
                ; first/last column)
                ;
                cmp #$2d                ; "-"
                beq dec_value
                cmp #$2b                ; "+"
                beq inc_value
                cmp #$3c                ; "<"
                beq dec_ptr
                cmp #$3e                ; ">"
                beq inc_ptr
                cmp #$5b                ; "["
                beq start_loop
                cmp #$5d                ; "]"
                beq end_loop
                cmp #$2e                ; "."
                beq output
                cmp #$2c                ; ","
                bne instr_done
                inc program_mode        ; input value to RAM
                ;
instr_done      stx bf_pc               ; store new program counter
                lda output_len          ; update output cursor sprite
                jmp upd_io_cursor       ; ends with RTS

to_ended_mode   lda #mode_ended         ; end program
                sta program_mode
                lda #(27*8-1)           ; move cursor next to "B=exit" text
                sta sprite_data+0+0
                lda #(19*8)
                sta sprite_data+0+3
                rts

dec_value       lda #$ff                ; decrement RAM value
                bne +                   ; unconditional
inc_value       lda #1                  ; increment RAM value
+               clc
                adc (bf_ram_ptr),y
                sta (bf_ram_ptr),y
                jmp instr_done

dec_ptr         lda bf_ram_ptr+0        ; decrement RAM pointer
                bne ++
                lda bf_ram_ptr+1
                cmp #>bf_ram
                bne +
                lda #>(bf_ram+$400)
                sta bf_ram_ptr+1
+               dec bf_ram_ptr+1
++              dec bf_ram_ptr+0
                jmp instr_done

inc_ptr         inc bf_ram_ptr+0        ; increment RAM pointer
                bne instr_done
                inc bf_ram_ptr+1
                lda bf_ram_ptr+1
                cmp #>(bf_ram+$400)
                bne instr_done
                lda #>bf_ram
                sta bf_ram_ptr+1
                bpl instr_done          ; unconditional

start_loop      lda (bf_ram_ptr),y      ; jump to corresponding "]" if RAM = 0
                bne instr_done
                beq +                   ; unconditional
end_loop        lda (bf_ram_ptr),y      ; jump to corresponding "[" if RAM != 0
                beq instr_done
+               lda brackets,x
                tax
                jmp instr_done

output          lda (bf_ram_ptr),y      ; if newline ($0a)...
                cmp #$0a
                bne +
                ;
                lda output_len          ; move output cursor to next line
                adc #(32-1)             ; carry is always set
                and #%11100000
                beq to_ended_mode       ; if output area full, end program
                sta output_len          ; (code ends with RTS)
                inc output_len          ; 1st column is always empty
                bne instr_done          ; unconditional
                ;
+               sta vram_buf_value      ; otherwise output RAM value via NMI
                lda output_len
                sta vram_buf_adrlo
                inc output_len
                ;
                lda output_len          ; first & last columns are always empty
                and #%00011111
                cmp #%00011111
                bne +
                inc output_len
                inc output_len
                ;
+               lda #$25
                sta vram_buf_adrhi      ; set this last to avoid race condition
                lda output_len          ; if output area full, end program
                cmp #$01
                bne +
                jmp to_ended_mode       ; ends with RTS
+               jmp instr_done

; --- Main loop - Brainfuck program waiting for input -------------------------

ml_input        ; ignore buttons if anything was pressed on last frame
                lda prev_pad_status
                ; update cursor sprite coordinates (ends with RTS)
                bne upd_keyb_cursor

                lda pad_status          ; react to buttons
                ;
                bmi keyb_input          ; A
                asl a
                bmi to_edit_mode        ; B; to edit mode (ends with RTS)
                asl a
                asl a
                asl a
                bmi keyb_up             ; up
                asl a
                bmi keyb_down           ; down
                asl a
                bmi keyb_left           ; left
                bne keyb_right          ; right
                ;
                beq upd_keyb_cursor     ; unconditional, ends with RTS

keyb_right      ldx keyb_x
                inx
                bpl +                   ; unconditional
keyb_left       ldx keyb_x
                dex
+               txa
                and #%00001111
                sta keyb_x
                bpl upd_keyb_cursor     ; unconditional, ends with RTS

keyb_down       ldx keyb_y
                inx
                cpx #6
                bne +
                ldx #0
                beq +                   ; unconditional
keyb_up         ldx keyb_y
                dex
                bpl +
                ldx #(6-1)
+               stx keyb_y
                jmp upd_keyb_cursor     ; ends with RTS

keyb_input      lda keyb_y              ; store character at cursor to BF RAM
                asl a
                asl a
                asl a
                asl a
                adc #$20                ; carry is always clear
                ora keyb_x
                ;
                cmp #$7f                ; return symbol ($7f) as newline ($0a)
                bne +
                lda #$0a
                ;
+               ldy #0
                sta (bf_ram_ptr),y
                ;
                dec program_mode        ; switch to mode_run
                ;
                rts

upd_keyb_cursor lda keyb_y              ; update keyboard cursor sprite
                asl a
                asl a
                asl a
                adc #(20*8-1)           ; carry is always clear
                sta sprite_data+0+0
                ;
                lda keyb_x
                asl a
                asl a
                asl a
                adc #(8*8)              ; carry is always clear
                sta sprite_data+0+3
                ;
rts2            rts

; --- Main loop - BF program ended / subs used in more than one mode ----------

ml_ended        lda pad_status          ; if B pressed, switch to edit mode
                and #pad_b              ; (ends with RTS)
                beq rts2
                ;
to_edit_mode    lda #mode_edit          ; switch to edit mode
                sta program_mode        ; (from run/input/ended mode)
                lda #%10000000          ; show edit mode name table
                sta ppu_ctrl_copy
                rts

upd_io_cursor   pha                     ; update input/output cursor sprite
                and #%11100000          ; in: A = input/output length
                lsr a
                lsr a
                adc #(8*8-1)
                sta sprite_data+0+0
                ;
                pla
                asl a
                asl a
                asl a
                sta sprite_data+0+3
                ;
                rts

; --- Interrupt routines ------------------------------------------------------

nmi             pha                     ; push A, X, Y
                txa
                pha
                tya
                pha

                bit ppu_status          ; clear ppu_scroll/ppu_addr latch
                ;
                lda #$00                ; do OAM DMA
                sta oam_addr
                lda #>sprite_data
                sta oam_dma

                ldy vram_buf_adrhi      ; flush VRAM buffer if address != $00xx
                beq buf_flush_done
                ;
                lda vram_buf_adrlo
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                lda vram_buf_value
                sta ppu_data
                ;
                lda #$00
                sta vram_buf_adrhi

buf_flush_done  lda program_mode        ; if in one of "prepare to run" modes,
                ldy #$25                ; fill top/bottom half of output area
                ;                       ; (write byte $00 $80 times to VRAM
                cmp #mode_prep_run1     ; $2500/$2580)
                bne +
                lda #$00
                beq ++                  ; unconditional
                ;
+               cmp #mode_prep_run2
                bne clear_done
                lda #$80
                ;
++              jsr set_ppu_addr        ; Y*$100+A -> PPU address
                asl a                   ; 0 -> A
                ldx #$40
-               sta ppu_data
                sta ppu_data
                dex
                bne -

clear_done      jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask

                ; set flag to let once-per-frame stuff run
                ; (other negative values won't do)
                lda #%10000000
                sta nmi_done

                pla                     ; pull Y, X, A
                tay
                pla
                tax
                pla

irq             rti

; --- Subs involving PPU registers (used in init & NMI routine) ---------------

set_ppu_addr_pg lda #$00                ; 0 -> A; Y*$100+A -> PPU address
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
                dw nmi, reset, irq      ; note: IRQ unused

; --- CHR ROM -----------------------------------------------------------------

                base $0000
                incbin "chr.bin"
                pad $2000, $ff
