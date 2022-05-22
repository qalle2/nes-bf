; Qalle's Brainfuck (NES, ASM6)

; --- Constants -----------------------------------------------------------------------------------

; notes:
; - only the first sprite slot is actually used, and the other slots only need their Y positions
;   to be set to $ff, so the OAM page accommodates many other variables at addresses
;   not divisible by 4 ($05-$07, $09-$0b, $0d-$0f, ...)
; - "VRAM buffer" = what to write to PPU on next VBlank
; - bottom half of stack ($0100-$017f) is used for other purposes
; - nmi_done: did the NMI routine just run? used for once-per-frame stuff; set by NMI,
;   read and cleared at the start of main loop

; RAM
sprite_data     equ $00    ; OAM page ($100 bytes, see above)
pt_data_ptr     equ $05    ; pointer to pt_data (2 bytes)
program_mode    equ $07    ; see constants below
bf_ram_ptr      equ $09    ; pointer to bf_ram (2 bytes; separate from pt_data_ptr for clarity)
nmi_done        equ $0b    ; see above ($00 = no, $80 = yes)
ppu_ctrl_copy   equ $0d    ; copy of ppu_ctrl
frame_counter   equ $0e    ; for blinking cursors
pad_status      equ $0f    ; joypad status
prev_pad_status equ $11    ; joypad status on previous frame
vram_buf_adrhi  equ $12    ; VRAM buffer - high byte of address ($00 = buffer is empty)
vram_buf_adrlo  equ $13    ; VRAM buffer - low  byte of address
vram_buf_value  equ $15    ; VRAM buffer - value
program_len     equ $16    ; length of Brainfuck program (0-$fe)
bf_pc           equ $17    ; program counter of Brainfuck program (preincremented)
output_len      equ $19    ; number of characters printed by the Brainfuck program (0-$fe)
keyb_x          equ $1a    ; cursor X position on virtual keyboard (0-15)
keyb_y          equ $1b    ; cursor Y position on virtual keyboard (0-5)
stack_ptr_copy  equ $1d    ; copy of stack pointer
pt_common       equ $1e    ; pattern table data - most common byte of tile
pt_common_pos   equ $1f    ; pattern table data - bits denote positions of pt_common in tile
bf_program      equ $0200  ; Brainfuck program ($100 bytes)
brackets        equ $0300  ; target addresses of "[" and "]" ($100 bytes)
bf_ram          equ $0400  ; RAM of Brainfuck program ($400 bytes; must be at $xx00)

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
pad_sel         equ 1<<5  ; select
pad_start       equ 1<<4  ; start
pad_u           equ 1<<3  ; up
pad_d           equ 1<<2  ; down
pad_l           equ 1<<1  ; left
pad_r           equ 1<<0  ; right

; colors
color_bg        equ $0f  ; background (black)
color_fg        equ $30  ; foreground (white)
color_unused    equ $25  ; unused (pink)

; tiles
tile_block      equ $80  ; solid block
tile_hbar       equ $81  ; horizontal bar
tile_uarr       equ $82  ; up arrow
tile_darr       equ $83  ; down arrow
tile_larr       equ $84  ; left arrow
tile_rarr       equ $85  ; right arrow

; values for program_mode (must be 0, 1, ... because they're used as indexes to jump table)
mode_edit       equ 0    ; editing BF program (must be 0)
mode_prep_run1  equ 1    ; preparing to run BF program, part 1/2
mode_prep_run2  equ 2    ; preparing to run BF program, part 2/2
mode_run        equ 3    ; BF program running
mode_input      equ 4    ; BF program waiting for input
mode_ended      equ 5    ; BF program finished

; misc
blink_rate      equ 3    ; cursor blink rate (0 = fastest, 7 = slowest)

; --- iNES header ---------------------------------------------------------------------------------

                ; see https://wiki.nesdev.org/w/index.php/INES
                base $0000
                db "NES", $1a            ; file id
                db 1, 0                  ; 16 KiB PRG ROM, 0 KiB CHR ROM (uses CHR RAM)
                db %00000001, %00000000  ; NROM mapper, vertical name table mirroring
                pad $0010, $00           ; unused

; --- Initialization ------------------------------------------------------------------------------

                base $c000              ; start of PRG ROM
                pad $f800, $ff          ; last 2 KiB of CPU address space

reset           ; initialize the NES; see https://wiki.nesdev.org/w/index.php/Init_code
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

                lda #$00                ; clear sprite/variables page and Brainfuck code
                tax
-               sta sprite_data,x
                sta bf_program,x
                inx
                bne -

                lda #$ff                ; hide all sprites (set Y positions to $ff;
-               sta sprite_data,x       ; X is still 0)
                inx
                inx
                inx
                inx
                bne -

                jsr wait_vbl_start      ; wait until next VBlank starts

                ldy #$3f                ; set up palette (while still in VBlank; 8*4 bytes)
                jsr set_ppu_addr_pg     ; 0 -> A; Y*$100+A -> PPU address
                ;
                ldy #8
--              ldx #(4-1)
-               lda palette,x
                sta ppu_data
                dex
                bpl -
                dey
                bne --

                ; fill pattern table 0 (PPU $0000-$0fff) with $00 (Y is still 0)
                jsr set_ppu_addr_pg     ; 0 -> A; Y*$100+A -> PPU address
                ldx #16
-               jsr fill_vram           ; write A Y times
                dex
                bne -

                lda #<pt_data           ; extract data from array to pattern table 0
                sta pt_data_ptr+0       ; (see "pt-data-compress.py" for format)
                lda #>pt_data
                sta pt_data_ptr+1
                ;
                ldy #$02                ; start from tile $20
                jsr set_ppu_addr_pg     ; 0 -> A; Y*$100+A -> PPU address
                ;
--              ldy #0                  ; write 1 tile/round; Y = source index
                lda (pt_data_ptr),y     ; which byte positions of tile use the most common byte
                beq pt_data_end         ; 0 = terminator
                sta pt_common_pos
                ;
                iny
                lda (pt_data_ptr),y     ; what is the most common byte of tile
                sta pt_common
                ;
                ldx #8                  ; write 1st bitplane of tile
-               asl pt_common_pos
                bcc +
                lda pt_common           ; use the most common byte
                jmp ++
+               iny                     ; copy byte from source
                lda (pt_data_ptr),y
++              sta ppu_data
                dex
                bne -
                ;
                iny                     ; advance source index and add it to pointer
                tya
                clc
                adc pt_data_ptr+0
                sta pt_data_ptr+0
                bcc +
                inc pt_data_ptr+1
                ;
+               ldy #8                  ; fill 2nd bitplane of tile with zero
                lda #$00
                jsr fill_vram
                ;
                beq --                  ; next tile (unconditional)

pt_data_end     ldy #$20                ; fill name & attribute table 0 & 1 ($2000-$27ff) with $00
                jsr set_ppu_addr_pg     ; 0 -> A; Y*$100+A -> PPU address
                ldx #8
                tay
-               jsr fill_vram           ; write A Y times
                dex
                bne -

                ldx #$ff                ; copy strings to NT0 (edit mode) and NT1 (run mode)
--              inx
                ldy strings,x           ; VRAM address high (0 = end of all strings)
                beq strings_end
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
                ldy #32
                lda #tile_hbar
                jsr fill_vram           ; write A Y times
                dex
                bpl -

                ldy #$26                ; draw virtual keyboard in NT1
                lda #$78
                jsr set_ppu_addr        ; Y*$100+A -> PPU address
                ;
                ldx #32                 ; X = character code
-               txa                     ; print 16 spaces before start of each line
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

palette         ; copied backwards to all subpalettes
                ; note: 2nd color of 1st sprite subpalette blinks and is used for cursors
                db color_unused, color_unused, color_fg, color_bg

                ; pattern table data
                ; each line encodes 8 bytes (1st bitplane of 1 tile) in 2-9 bytes
                ; (2nd bitplane is always zero)
                ; generated from "pt-data.txt" using "pt-data-compress.py" (see it for the format)
                ;
pt_data         hex ff00                ; tile $20
                hex fa100000            ; tile $21
                hex 3f002828            ; tile $22
                hex d6287c7c00          ; tile $23
                hex 82103c5038147800    ; tile $24
                hex 83004408102044      ; tile $25
                hex 44443828102a3a00    ; tile $26
                hex 3f001010            ; tile $27
                hex 38200810100800      ; tile $28
                hex 38082010102000      ; tile $29
                hex 83004428fe2844      ; tile $2a
                hex ee10fe00            ; tile $2b
                hex f100081020          ; tile $2c
                hex ef00fc              ; tile $2d
                hex f9001818            ; tile $2e
                hex 800204081020408000  ; tile $2f
                hex 6c827c927c00        ; tile $30
                hex bc10303800          ; tile $31
                hex 907c82028080fe00    ; tile $32
                hex 6c02fcfcfc00        ; tile $33
                hex 8608182848fe00      ; tile $34
                hex 6080fefc0202fc00    ; tile $35
                hex 60807efc82827c00    ; tile $36
                hex 80fe04081020408000  ; tile $37
                hex 6c827c7c7c00        ; tile $38
                hex 60827c7e0202fc00    ; tile $39
                hex bb001010            ; tile $3a
                hex b10010102040        ; tile $3b
                hex 8208102040201000    ; tile $3c
                hex db00fefe            ; tile $3d
                hex 8240201008102000    ; tile $3e
                hex 0d107c82020c00      ; tile $3f
                hex 30ba7c82b4807e00    ; tile $40
                hex 6e827cfe00          ; tile $41
                hex 6c42fc7cfc00        ; tile $42
                hex 7c807e7e00          ; tile $43
                hex 3882f88484f800      ; tile $44
                hex 6c80fefefe00        ; tile $45
                hex 6e80fefe00          ; tile $46
                hex 827e80809e828200    ; tile $47
                hex ee82fe00            ; tile $48
                hex 7c10383800          ; tile $49
                hex f804443800          ; tile $4a
                hex 8244485060504800    ; tile $4b
                hex fc80fe00            ; tile $4c
                hex 8e82c6aa9200        ; tile $4d
                hex 8282c2a2928a8600    ; tile $4e
                hex 7c827c7c00          ; tile $4f
                hex 0e80fc8282fc00      ; tile $50
                hex 60827c928a867e00    ; tile $51
                hex 6282fcfc888400      ; tile $52
                hex 60807e7c0202fc00    ; tile $53
                hex 7e10fe00            ; tile $54
                hex fc827c00            ; tile $55
                hex f08244281000        ; tile $56
                hex e28292aac600        ; tile $57
                hex 8282442810284400    ; tile $58
                hex 1e1082442800        ; tile $59
                hex 82fe040810204000    ; tile $5a
                hex 7c20383800          ; tile $5b
                hex 808040201008040200  ; tile $5c
                hex 7c08383800          ; tile $5d
                hex 1f00102844          ; tile $5e
                hex fd00fe              ; tile $5f
                hex 1f00100804          ; tile $60
                hex c10078043c4c34      ; tile $61
                hex 1c444040787800      ; tile $62
                hex c1003c4040403c      ; tile $63
                hex 1c4404043c3c00      ; tile $64
                hex c100384478403c      ; tile $65
                hex 2e2018247800        ; tile $66
                hex c000344c443c0478    ; tile $67
                hex 0e444040586400      ; tile $68
                hex 5e10000000          ; tile $69
                hex 5c0800004830        ; tile $6a
                hex c040485060504800    ; tile $6b
                hex 7e103000            ; tile $6c
                hex c100b6da929292      ; tile $6d
                hex c1005864444444      ; tile $6e
                hex c1003844444438      ; tile $6f
                hex c000586444784040    ; tile $70
                hex c000344c443c0404    ; tile $71
                hex c1005c60404040      ; tile $72
                hex c1003c40380478      ; tile $73
                hex 58200078281000      ; tile $74
                hex c1004444444c34      ; tile $75
                hex c1004444282810      ; tile $76
                hex 3c5400002800        ; tile $77
                hex c1004428102844      ; tile $78
                hex 384400003c0478      ; tile $79
                hex c1007c0810207c      ; tile $7a
                hex 6c100c600c00        ; tile $7b
                hex ee100000            ; tile $7c
                hex 6c10600c6000        ; tile $7d
                hex 3f006498            ; tile $7e
                hex c0042444fc402000    ; tile $7f
                hex ffff                ; tile $80
                hex c700ffffff          ; tile $81
                hex 9e10385400          ; tile $82
                hex f210543800          ; tile $83
                hex 83002040fe4020      ; tile $84
                hex 83000804fe0408      ; tile $85
                hex 00                  ; end of data

macro nt_addr _nt, _y, _x
                ; output name table address ($2000-$27bf), high byte first
                dh $2000+(_nt*$400)+(_y*$20)+(_x)
                dl $2000+(_nt*$400)+(_y*$20)+(_x)
endm

strings         ; each string: PPU address high/low, characters, null terminator
                ; address high = 0 ends all strings
                ;
                nt_addr 0, 2, 7
                db "Qalle's Brainfuck", 0
                nt_addr 0, 4, 11
                db "edit mode", 0
                nt_addr 0, 6, 12
                db "Program:", 0
                nt_addr 0, 18, 4
                db tile_uarr, "=+ ", tile_darr, "=-  "
                db tile_larr, "=< ", tile_rarr, "=>  "
                db "B=[ A=]", 0
                nt_addr 0, 20, 9
                db "select+B=,", 0
                nt_addr 0, 21, 9
                db "select+A=.", 0
                nt_addr 0, 22, 12
                db "start=backspace", 0
                nt_addr 0, 23, 5
                db "select+start=run", 0
                ;
                nt_addr 1, 2, 7
                db "Qalle's Brainfuck", 0
                nt_addr 1, 4, 11
                db "run mode", 0
                nt_addr 1, 6, 12
                db "Output:", 0
                nt_addr 1, 18, 9
                db "Input (", tile_uarr, tile_darr, tile_larr, tile_rarr, "A):", 0
                nt_addr 1, 27, 9
                db "B=to edit mode", 0
                ;
                db 0  ; end of all strings

                if $ - strings > 256
                    error "out of string space"
                endif

                ; VRAM addresses of horizontal bars (above/below Brainfuck code area in both
                ; name tables)
horz_bars_hi    dh $20e0, $2200, $24e0, $2600  ; high bytes
horz_bars_lo    dl $20e0, $2200, $24e0, $2600  ; low  bytes

; --- Main loop - common --------------------------------------------------------------------------

main_loop       asl nmi_done            ; to avoid missing the flag being set by NMI routine,
                bcs +                   ; clear and read it using a single instruction
                ;
                lda program_mode        ; not first round after VBlank;
                cmp #mode_run           ; only run mode-specific stuff if in run mode
                bne main_loop
                jsr ml_run
                jmp main_loop
                ;
+               jsr once_per_frame      ; first round after VBlank; run once-per-frame stuff
                jmp main_loop

once_per_frame  ; stuff that's done only once per frame

                lda pad_status          ; store previous joypad status
                sta prev_pad_status

                lda #1                  ; read first joypad or Famicom expansion port controller
                sta joypad1             ; see https://www.nesdev.org/wiki/Controller_reading_code
                sta pad_status          ; bits: A, B, select, start, up, down, left, right
                lsr a
                sta joypad1
-               lda joypad1
                and #%00000011
                cmp #1
                rol pad_status
                bcc -

                lda frame_counter       ; set cursor tile according to frame counter
                and #(1<<blink_rate)
                beq +
                lda #tile_block
+               sta sprite_data+0+1

                inc frame_counter       ; advance frame counter

                ; jump to one sub depending on program mode
                ; note: RTS in the subs below will act like RTS in this sub
                ; see https://www.nesdev.org/wiki/Jump_table
                ; and https://www.nesdev.org/wiki/RTS_Trick
                ;
                ldx program_mode        ; push target address minus one, high byte first
                lda jump_table_hi,x
                pha
                lda jump_table_lo,x
                pha
                rts                     ; pull address, low byte first; jump to address plus one

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

; --- Main loop - editing Brainfuck program -------------------------------------------------------

ml_edit         lda pad_status          ; react to buttons
                cmp prev_pad_status     ; skip if joypad status not changed
                beq char_entry_end
                ;
                cmp #(pad_sel|pad_start)
                beq run_program
                ;
                cmp #pad_start
                beq del_last_char       ; backspace
                ;
                ldx #(bf_instrs_end-bf_instrs-1)
-               lda edit_buttons,x      ; enter instruction if corresponding button pressed
                cmp pad_status
                beq enter_instr
                dex
                bpl -

char_entry_end  lda program_len         ; update coordinates of input cursor sprite
                jmp upd_io_cursor       ; ends with RTS

run_program     inc program_mode        ; switch to mode_prepare_run1
                rts                     ; (later to mode_prepare_run2 & mode_run)

del_last_char   ldy program_len         ; if there's >= 1 instruction...
                beq char_entry_end
                ;
                ldy program_len
                dey                     ; delete last instruction, tell NMI routine to redraw it
                lda #$00
                sta bf_program,y
                sta vram_buf_value
                sty program_len
                sty vram_buf_adrlo
                lda #$21
                sta vram_buf_adrhi      ; set this byte last to avoid race condition
                ;
                bne char_entry_end      ; unconditional

enter_instr     ldy program_len         ; if program is < $ff characters...
                cpy #$ff
                beq char_entry_end
                ;
                lda bf_instrs,x         ; add instruction specified by X, tell NMI routine to
                sta bf_program,y        ; draw it
                sta vram_buf_value
                sty vram_buf_adrlo
                inc program_len
                lda #$21
                sta vram_buf_adrhi      ; set this byte last to avoid race condition
                ;
                bne char_entry_end      ; unconditional

                ; Brainfuck instructions and corresponding buttons in edit mode
edit_buttons    db pad_u, pad_d, pad_l, pad_r, pad_b, pad_a, pad_sel|pad_b, pad_sel|pad_a
bf_instrs       db "+",   "-",   "<",   ">",   "[",   "]",   ",",           "."
bf_instrs_end

; --- Main loop - prepare to run, part 1/2 --------------------------------------------------------

ml_prep_run1    ; for each bracket in Brainfuck program, store index of corresponding bracket in
                ; another array
                ; - in: bf_program (array), program_len
                ; - out: brackets (array)
                ; - changes: program_mode (according to whether brackets were valid)
                ; - trashes: stack_ptr_copy, bottom half of stack ($0100-$017f)
                ; note: an interrupt which uses stack must never fire during this sub
                ;
                tsx                     ; store original stack pointer
                stx stack_ptr_copy
                ldx #$7f                ; use bottom half of stack for currently open brackets
                txs
                ldy #$ff                ; Y = current program index (preincremented)
                ;
-               iny                     ; next instruction
                cpy program_len
                bne +
                ;
                tsx                     ; end of Brainfuck program
                inx
                bpl bracket_error       ; missing "]" (SP != $7f)
                inc program_mode        ; brackets valid; proceed to mode_prep_run2
                bpl bracket_exit        ; unconditional
                ;
+               lda bf_program,y
                cmp #$5b                ; "["
                bne +
                tsx
                bmi bracket_error       ; maximum number of "["s already open (SP = $ff)
                tya                     ; push current index
                pha
                jmp -
                ;
+               cmp #$5d                ; "]"
                bne -
                pla                     ; pull corresponding index
                tsx
                bmi bracket_error       ; missing "[" (SP = $80)
                sta brackets,y          ; store corresponding index here
                tax                     ; store this index at corresponding index
                tya
                sta brackets,x
                jmp -
                ;
bracket_error   dec program_mode        ; error in brackets; return to mode_edit
bracket_exit    ldx stack_ptr_copy      ; restore original stack pointer
                txs
                rts

; --- Main loop - prepare to run, part 2/2 --------------------------------------------------------

ml_prep_run2    ldy #$ff                ; hide edit cursor, reset Brainfuck program counter
                sty sprite_data+0+0
                sty bf_pc

                iny                     ; reset low byte of BF RAM pointer, output length,
                sty bf_ram_ptr+0        ; keyboard cursor
                sty output_len
                sty keyb_x
                sty keyb_y

                lda #>(bf_ram+$400)     ; set pointer to the page after the end of BF RAM
                sta bf_ram_ptr+1

--              tya                     ; clear Brainfuck RAM and leave pointer at its beginning
                dec bf_ram_ptr+1
-               sta (bf_ram_ptr),y
                iny
                bne -
                lda bf_ram_ptr+1
                cmp #>bf_ram
                bne --

                lda #%10000001          ; show run mode name table
                sta ppu_ctrl_copy

                inc program_mode        ; switch to mode_run

rts1            rts

; --- Main loop - Brainfuck program running -------------------------------------------------------

; the only part of main loop that's run as frequently as possible instead of once per frame

ml_run          lda pad_status          ; stop program if B pressed
                cmp #pad_b
                bne +
                jmp to_edit_mode        ; ends with RTS

+               lda vram_buf_adrhi      ; wait until NMI routine has flushed VRAM buffer
                bne rts1

                tay                     ; always 0; only used in indirect addressing

                ldx bf_pc               ; incremented PC -> X; switch to "program ended" mode
                inx                     ; if necessary
                cpx program_len
                beq to_ended_mode       ; ends with RTS

                lda bf_program,x        ; instruction -> A

                ; process current instruction
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
                ;
                inc program_mode        ; ","; input value to RAM (switch to mode_input)
                ;
instr_done      stx bf_pc               ; store new program counter
                lda output_len          ; update coordinates of output cursor sprite
                jmp upd_io_cursor       ; ends with RTS

to_ended_mode   lda #mode_ended         ; switch to "program ended" mode
                sta program_mode
                lda #$ff                ; hide cursor
                sta sprite_data+0+0
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

start_loop      lda (bf_ram_ptr),y      ; jump to corresponding "]" if RAM value is 0
                bne instr_done
                beq +                   ; unconditional
end_loop        lda (bf_ram_ptr),y      ; jump to corresponding "[" if RAM value is not 0
                beq instr_done
+               lda brackets,x
                tax
                jmp instr_done

output          lda (bf_ram_ptr),y      ; if newline ($0a)...
                cmp #$0a
                bne +
                ;
                lda output_len          ; move output cursor to start of next line
                adc #(32-1)             ; carry is always set
                and #%11100000
                sta output_len
                jmp ++
                ;
+               sta vram_buf_value      ; otherwise output value from RAM via NMI routine
                lda output_len
                sta vram_buf_adrlo
                lda #$25
                sta vram_buf_adrhi      ; set this byte last to avoid race condition
                inc output_len
                ;
++              beq to_ended_mode       ; if $100 characters printed, end program (ends with RTS)
                bne instr_done          ; unconditional

; --- Main loop - Brainfuck program waiting for input ---------------------------------------------

ml_input        lda prev_pad_status     ; ignore buttons if anything was pressed on last frame
                bne upd_keyb_cursor     ; update cursor sprite coordinates (ends with RTS)

                lda pad_status          ; react to buttons (bits: A B sel st up down left right)
                ;
                bmi keyb_input          ; button A
                asl a
                bmi to_edit_mode        ; button B; back to edit mode (ends with RTS)
                asl a
                asl a
                asl a
                bmi keyb_up             ; d-pad up
                asl a
                bmi keyb_down           ; d-pad down
                asl a
                bmi keyb_left           ; d-pad left
                bne keyb_right          ; d-pad right
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

keyb_input      lda keyb_y              ; store character at cursor to Brainfuck RAM
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

upd_keyb_cursor lda keyb_y              ; update coordinates of keyboard cursor sprite
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

; --- Main loop - Brainfuck program ended / subs used in more than one program mode ---------------

ml_ended        lda pad_status          ; if B pressed, switch to edit mode (ends with RTS)
                and #pad_b
                beq rts2
                ;
to_edit_mode    lda #mode_edit          ; switch to edit mode (from run/input/ended mode)
                sta program_mode
                lda #%10000000          ; show edit mode name table
                sta ppu_ctrl_copy
                rts

upd_io_cursor   pha                     ; update coordinates of input/output cursor sprite
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

; --- Interrupt routines --------------------------------------------------------------------------

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
                ldy #$25                ; fill top or bottom half of output area
                ;                       ; (write byte $00 $80 times to VRAM $2500 or $2580)
                cmp #mode_prep_run1
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

                lda #%10000000          ; set flag to let once-per-frame stuff run
                sta nmi_done            ; note: other negative values won't do

                pla                     ; pull Y, X, A
                tay
                pla
                tax
                pla

irq             rti

; --- Subs involving PPU registers (used in initialization & NMI routine) -------------------------

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

; --- Interrupt vectors ---------------------------------------------------------------------------

                pad $fffa, $ff
                dw nmi, reset, irq      ; note: IRQ unused
