LEADER_HEAD_SPRITE = 17*4
KAPPA_SPRITE = 52*4
STAR_INDEX = $700
SEL_INDEX = $701
SEL_START_Y = $8e
LoaderFrameCounter = $30
CurrentHead = $31
CURSOR_SPRITE = $f9

	.vars vars.inc
	.org $8000
dummy:
	.db $ba, BANK_LOADER
;
; Identity-mapped swap-table
;
PTR_Start:
		.dw LDR_Start
PTR_GetAreaDataAddrs:
		.dw LDR_Nop
PTR_AddToScore:
		.dw LDR_Nop
PTR_RunFireworks:
		.dw LDR_Nop
PTR_RunStarFlagObj:
		.dw LDR_Nop
PTR_HandlePipeEntry:
		.dw LDR_Nop
PTR_GiveOneCoin:
		.dw LDR_Nop
PRAC_LoadChrROM:
		.dw LDR_LoadChrROM

NonMaskableInterrupt:
		inc LoaderFrameCounter

		;
		; Turn on rendering (Sprites, background)
		;
		lda #$1E
		sta PPU_CTRL_REG2
		;
		; Rotate star palette
		;
		lda LoaderFrameCounter
		and #$0f
		bne no_rotate_stars
		jsr rotate_star_palette
no_rotate_stars:
		;
		; Copy sprite data
		;
		lda PPU_STATUS
		lda #$00
		sta PPU_SPR_ADDR
		lda #$02
		sta SPR_DMA
		;
		; Change head
		;
		lda LoaderFrameCounter
		and #$1f
		bne NoChangeHead
		ldx CurrentHead
		inx
		cpx #4
		bne NoLooparoundHead
		ldx #0
NoLooparoundHead:
		stx CurrentHead
		jsr set_leader_head_sprite
NoChangeHead:

		;
		; Move heads
		;
		lda #00
		sta $04 ; offset
		lda #5 ; Width
		sta $01
		ldy #7 ; Height
		ldx #LEADER_HEAD_SPRITE	; Offset
		jsr move_head_group

		lda #16
		sta $04
		lda #3
		sta $01
		ldy #4
		ldx #KAPPA_SPRITE
		jsr move_head_group
		;
		;
		;
		lda LoaderFrameCounter
		and #$07
		bne dont_update_cursor
		lda $201
		cmp #CURSOR_SPRITE
		beq hide_cursor
		lda #CURSOR_SPRITE
		sta $201
		jmp dont_update_cursor
hide_cursor:
		lda #$24
		sta $201
dont_update_cursor:
		;
		; Update sound
		;
		; jsr EnterSoundEngine
		jsr EnterSmlSoundPlay
		jsr ReadJoypads
do_menu:
		lda SavedJoypadBits
		cmp #Select_Button
		bne no_select
		ldx SEL_INDEX
		lda $200
		inx 
		cpx #3
		bne no_loop_around
		ldx #0
		lda #SEL_START_Y-16
no_loop_around:
		clc
		adc #16
		sta $200
		stx SEL_INDEX
no_select:
		cmp #Start_Button
		bne no_start
		ldx SEL_INDEX
		lda bank_table, x
		sta BANK_SELECTED
		jmp InitMapper
no_start:
		;
		; Reset
		;
		lda PPU_STATUS
		;
		; Undo scrolling
		;
		lda #$00
		sta PPU_SCROLL_REG ; No scrolling
		sta PPU_SCROLL_REG
		lda #$80
		sta PPU_CTRL_REG1	; Make sure NMI is on...
		rti

rotate_star_palette:
		;
		; Init PPU
		;
		lda PPU_STATUS ; Latch
		lda #$3F
		sta PPU_ADDRESS
		lda #$05
		sta PPU_ADDRESS
		;
		; Rotate star palette
		;
		ldx STAR_INDEX
		inx
		cpx #3
		bne no_star_reset
		ldx #0
no_star_reset:
		stx STAR_INDEX
		ldy #3
shuffle_more_stars:
		lda palette_star_shuffle, x
		sta PPU_DATA
		inx
		cpx #3
		bne no_loop
		ldx #0
no_loop:
		dey
		bpl shuffle_more_stars
		lda PPU_STATUS
		lda #$00
		sta PPU_ADDRESS
		sta PPU_ADDRESS
		rts


head_sprite_indexes:
		.db $5f, $82, $a5, $c8

set_leader_head_sprite:
		lda #35
		sta $0
		ldy head_sprite_indexes, x
		ldx #LEADER_HEAD_SPRITE
		inx
copy_next_sprite:
		tya
		sta $200, x
		iny
		inx
		inx
		inx
		inx
		dec $0
		bne copy_next_sprite
		rts

move_head_group:
		;
		; Copy W
		;
		lda $01
		sta $03
		;
		; Base Y
		;
		lda $203, x ; X
		and #$7f
		stx $00
		tax
		lda sine_curve, x
		ldx $00
		clc
		adc $04
		sta $02 ; New Y
		;
		; Move group
		;
next_col:
		lda $03
		sta $01
move_more:
		dec $203, x
		lda $02
		sta $200, x
		inx
		inx
		inx
		inx
		dec $01
		bne move_more
		lda $02
		clc
		adc #$08
		sta $02
		dey
		bne next_col
		rts

sine_curve:
	.db $24, $24, $25, $26, $27, $27, $28, $29, $2A, $2A, $2B, $2C, $2C, $2D, $2E, $2E, $2F, $2F, $30, $30, $31, $31, $32, $32, $32, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $33, $32, $32, $32, $31, $31, $31, $30, $30, $2F, $2F, $2E, $2D, $2D, $2C, $2B, $2B, $2A, $29, $29, $28, $27, $26, $25, $25, $24, $23, $22, $22, $21, $20, $1F, $1E, $1E, $1D, $1C, $1C, $1B, $1A, $1A, $19, $18, $18, $17, $17, $16, $16, $16, $15, $15, $15, $14, $14, $14, $14, $14, $14, $14, $14, $14, $14, $14, $14, $14, $14, $15, $15, $15, $16, $16, $17, $17, $18, $18, $19, $19, $1A, $1B, $1B, $1C, $1D, $1D, $1E, $1F, $20, $20, $21, $22, $23, $23

nametable_data_0:
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $11, $1D, $1D, $19, $28, $29, $29, $10, $12, $1D, $11, $1E, $0B, $2A, $0C
	.db $18, $16, $29, $19, $0E, $15, $15, $1C, $1C, $18, $17, $29, $1C, $16, $0B, $24
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
	.db $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
nametable_data_1:
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
	.db $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
	.db $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
	.db $24, $24, $24, $24, $0C, $0A, $1D, $0C, $11, $24, $1D, $11, $0E, $16, $24, $20
	.db $12, $1D, $11, $24, $19, $1B, $0A, $0C, $1D, $12, $0C, $0E, $24, $24, $24, $24
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $5A, $24, $24, $24, $24, $24, $4F, $51, $51, $51, $51, $51, $51, $51
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $24, $24, $24, $24, $50, $52, $52, $52, $52, $52, $52, $52
nametable_data_2:
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $5A, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $24, $24, $24, $24, $50, $52, $52, $52, $53, $55, $52, $52
	.db $24, $24, $24, $5B, $58, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $4D, $4E, $24, $24, $50, $52, $52, $52, $54, $56, $52, $52
	.db $24, $5C, $24, $24, $24, $24, $24, $24, $19, $1B, $0A, $0C, $1D, $12, $0C, $0E
	.db $24, $5C, $24, $24, $3F, $42, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B
	.db $24, $24, $5B, $5B, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $59, $24, $24, $40, $42, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C
	.db $24, $24, $5D, $5B, $5B, $24, $24, $24, $1C, $0C, $0E, $17, $0A, $1B, $12, $18
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $3C, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $24, $24, $24, $24, $24, $24, $24, $1F, $0A, $17, $12, $15, $15, $0A, $24
	.db $24, $24, $24, $3D, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $3D, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
nametable_data_3:
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $3E, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $24, $5E, $24, $24, $24, $19, $1B, $0E, $1C, $1C, $24, $1C, $1D, $0A, $1B
	.db $1D, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $58, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $5D, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $1F, $03, $2A, $02, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
	.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	.db $00, $00, $00, $00, $00, $00, $00, $00, $50, $50, $10, $F0, $50, $C0, $F0, $F0
	.db $55, $1D, $A7, $A5, $11, $F3, $FF, $FF, $55, $01, $AA, $AA, $CC, $FF, $FF, $FF
	.db $55, $08, $0A, $4A, $DE, $FF, $FF, $FF, $00, $00, $00, $00, $00, $0F, $0F, $0F

static_sprite_data:
	.db SEL_START_Y, CURSOR_SPRITE, $02, $30
	;
	; Mario
	;
	.db $7f,	$32, $00, $D0
	.db $7f,	$33, $00, $D0+8
	.db $7f+8,	$34, $00, $D0
	.db $7f+8,	$35, $00, $D0+8
	;
	; Left Window
	;
	.db $c8,	$36, $02, $c0
	.db $c8,	$37, $02, $c0+8
	.db $c8+8,	$38, $02, $c0
	.db $c8+8,	$39, $02, $c0+8
	.db $c8+16,	$3a, $02, $c0
	.db $c8+16,	$3b, $02, $c0+8
	;
	; Right Window
	;
	.db $c8,	$36, $02, $e0
	.db $c8,	$37, $02, $e0+8
	.db $c8+8,	$38, $02, $e0
	.db $c8+8,	$39, $02, $e0+8
	.db $c8+16,	$3a, $02, $e0
	.db $c8+16,	$3b, $02, $e0+8
	;
	; Leader face
	;
	.db $40,	$5f, $01, $40
	.db $40,	$60, $01, $40+8
	.db $40,	$61, $01, $40+16
	.db $40,	$62, $01, $40+24
	.db $40,	$63, $01, $40+32
	;
	.db $FF,	$64, $01, $40
	.db $FF,	$65, $01, $40+8
	.db $FF,	$66, $01, $40+16
	.db $FF,	$67, $01, $40+24
	.db $FF,	$68, $01, $40+32
	;
	.db $ff,	$69, $01, $40
	.db $ff,	$6a, $01, $40+8
	.db $ff,	$6b, $01, $40+16
	.db $ff,	$6c, $01, $40+24
	.db $ff,	$6d, $01, $40+32
	;
	.db $ff,	$6e, $01, $40
	.db $ff,	$6f, $01, $40+8
	.db $ff,	$70, $01, $40+16
	.db $ff,	$71, $01, $40+24
	.db $ff,	$72, $01, $40+32
	;
	.db $ff,	$73, $01, $40
	.db $ff,	$74, $01, $40+8
	.db $ff,	$75, $01, $40+16
	.db $ff,	$76, $01, $40+24
	.db $ff,	$77, $01, $40+32
	;
	.db $ff,	$78, $01, $40
	.db $ff,	$79, $01, $40+8
	.db $ff,	$7a, $01, $40+16
	.db $ff,	$7b, $01, $40+24
	.db $ff,	$7c, $01, $40+32
	;
	.db $ff,	$7d, $01, $40
	.db $ff,	$7e, $01, $40+8
	.db $ff,	$7f, $01, $40+16
	.db $ff,	$80, $01, $40+24
	.db $ff,	$81, $01, $40+32
	;
	; Kappa
	;
	.db $FF,	$eb, $03, $90
	.db $FF,	$ec, $03, $90+8
	.db $FF,	$ed, $03, $90+16
	;
	.db $ff,	$ee, $03, $90
	.db $ff,	$ef, $03, $90+8
	.db $ff,	$f0, $03, $90+16
	;
	.db $ff,	$f1, $03, $90
	.db $ff,	$f2, $03, $90+8
	.db $ff,	$f3, $03, $90+16
	;
	.db $ff,	$f4, $03, $90
	.db $ff,	$f5, $03, $90+8
	.db $ff,	 $f6, $03, $90+16
	;
	; 0x100 bytes here...
	;

palette_data:
		.db $0f, $2a, $0a, $20 ; White box, hightlight text
		.db $0f                ; Star unused
palette_star_shuffle:
		.db $30, $2c, $00      ; Stars
		.db $0f, $20, $20, $20 ; Main font
		.db $0f, $00, $10, $30 ; Megaman
		;
		;
		;
		.db $0f, $16, $27, $18 ; Mario
		.db $0f, $27, $17, $0d ; Leader head
		.db $0f, $30, $2c, $11 ; Window
		.db $0f, $10, $00, $0d ; Kappa head

bank_table:
		.db BANK_PRACTICE, BANK_FPG, BANK_VANILLA

LDR_Start:
		lda #$10	; Use 0x1000 for background (not that it matters, same in both)
		sta PPU_CTRL_REG1
		ldx #$ff 	; Reset stack pointer
		txs
		;
		; Wait for stable ppu state
		;
wait_vbl0:
		lda PPU_STATUS
		bpl wait_vbl0
wait_vbl1:
		lda PPU_STATUS
		bpl wait_vbl1
		inx
clear_memory:
		lda #$00
		sta $0000, x
		sta $0100, x
		sta $0300, x
		sta $0400, x
		sta $0500, x
		sta $0600, x
		cpx #<BANK_SELECTED
		beq dont_wipe_bank_selection
		sta $0700, x
dont_wipe_bank_selection:
		lda #$fe
		sta $0200, x
		inx
		bne clear_memory
		;
		; Install nametable
		;
		ldx PPU_STATUS	; Eead PPU status to reset the high/low latch
		ldx #$00
		stx PPU_SCROLL_REG ; No scrolling
		stx PPU_SCROLL_REG
		stx PPU_CTRL_REG2 ; No rendering
		;
		; Copycopycopycopy
		;
		lda PPU_STATUS
		lda #$20
		sta PPU_ADDRESS
		lda #$00
		sta PPU_ADDRESS
write_nt_0:
		lda nametable_data_0, x
		sta PPU_DATA
		inx
		bne write_nt_0
write_nt_1:
		lda nametable_data_1, x
		sta PPU_DATA
		inx
		bne write_nt_1
write_nt_2:
		lda nametable_data_2, x
		sta PPU_DATA
		inx
		bne write_nt_2
write_nt_3:
		lda nametable_data_3, x
		sta PPU_DATA
		inx
		bne write_nt_3
		;
		; Copy static sprite-data over
		;
		ldx #$00
copy_more_sprites:
		lda static_sprite_data, x
		sta $200, x
		inx
		bne copy_more_sprites
		;
		; Install palette
		;
		lda PPU_STATUS ; Latch
		lda #$3F
		sta PPU_ADDRESS
		ldx #$00
		stx PPU_ADDRESS
next_palette_entry:
		lda palette_data, x
		sta PPU_DATA
		inx
		cpx #$20
		bne next_palette_entry
		;
		; Enable sound
		;
		lda #$01
		sta OperMode			; Anything but zero and it will play...

		lda #0
		ldx #0
		jsr EnterSmlSoundInit

		ldx #$00
		stx PPU_SCROLL_REG ; No scrolling
		stx PPU_SCROLL_REG
		
		;
		; Enable NMI
		;
		lda #$80
		sta PPU_CTRL_REG1
hang:
		jmp hang

LDR_LoadChrROM:
		lda #CHR_LOADER
		jmp SetChrFromA

LDR_Nop:
		brk

	.seekoff $bff0 $ea
	.org $fff0
MapperReset:
	sei
	ldx #$FF
	txs
	stx $8000
	jmp HardReset
	;
	; Interrupt table
	;
	.dw MapperReset
	.dw MapperReset
	.dw MapperReset