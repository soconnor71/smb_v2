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
PTR_DigitsMathRoutine:
		.dw LDR_Nop
PTR_UpdateNumber:
		.dw LDR_Nop
PTR_HandlePipeEntry:
		.dw LDR_Nop
PTR_GiveOneCoin:
		.dw LDR_Nop
PRAC_LoadChrROM:
		.dw LDR_LoadChrROM

NonMaskableInterrupt:
		;
		; Turn on rendering (Sprites, background)
		;
		lda #$1E
		sta PPU_CTRL_REG2

		;
		; Copy sprite data
		;
		lda PPU_STATUS
		lda #$00
		sta PPU_SPR_ADDR
		lda #$02
		sta SPR_DMA
		;
		; Update sound
		;
		; jsr EnterSoundEngine

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

nametable_data_0:
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $11, $1D, $1D, $19, $28, $29, $29, $10, $12, $1D, $11, $1E, $0B, $2A, $0C 
	.db $18, $16, $29, $19, $0E, $15, $15, $1C, $1C, $18, $17, $29, $1C, $16, $0B, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $5C, $24, $5A, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $58, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $5A, $24 
	.db $24, $24, $59, $5E, $24, $57, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $5C, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $5C, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $5A, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $5C, $24, $24, $5C, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $5F, $60, $61, $62, $63, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $59, $24, $24, $24, $24, $5C, $24 
nametable_data_1:
	.db $24, $24, $64, $65, $66, $67, $68, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $5A, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $69, $6A, $6B, $6C, $6D, $24, $24, $24, $82, $83, $84, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $6E, $6F, $70, $71, $72, $24, $24, $24, $85, $86, $87, $24, $24, $24 
	.db $5E, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $73, $74, $75, $76, $77, $24, $24, $24, $88, $89, $8A, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $78, $79, $7A, $7B, $7C, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $58, $24 
	.db $24, $24, $7D, $7E, $7F, $80, $24, $24, $24, $24, $5A, $24, $59, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $5A, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $57, $5E, $24, $24, $24, $24, $4F, $51, $51, $51, $51, $51, $51, $51 
	.db $58, $24, $59, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $58, $24, $24, $24, $24, $24, $50, $52, $52, $52, $52, $52, $52, $52 
nametable_data_2:
	.db $24, $24, $24, $58, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $50, $52, $52, $52, $53, $55, $52, $52 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $5A, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $4D, $4E, $24, $24, $50, $52, $52, $52, $54, $56, $52, $52 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $19, $1B, $0A, $0C, $1D, $12, $0C, $0E 
	.db $24, $24, $24, $24, $3F, $42, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $3F, $42, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $0F, $15, $0A, $10, $19, $18, $15, $0E 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $1F, $0A, $17, $12, $15, $15, $0A, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45
nametable_data_3:
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $19, $1B, $0E, $1C, $1C, $24, $1C, $1D, $0A, $1B 
	.db $1D, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24 
	.db $24, $24, $24, $24, $40, $42, $43, $45, $47, $49, $43, $45, $47, $49, $43, $45 
	.db $40, $D0, $00, $00, $40, $10, $40, $40, $F4, $F5, $55, $55, $01, $55, $55, $55 
	.db $CC, $FF, $CF, $75, $51, $54, $05, $15, $5C, $47, $15, $45, $5C, $0F, $F0, $F4 
	.db $04, $11, $F4, $F5, $31, $F3, $FF, $FF, $55, $01, $FF, $FF, $C0, $FF, $FF, $FF 
	.db $55, $0C, $0F, $4F, $CF, $FF, $FF, $FF, $05, $00, $00, $00, $00, $0F, $0F, $0F 

static_sprite_data:
	.db $fe, $fe, $fe, $fe
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

static_sprite_size:
	.db static_sprite_size-static_sprite_data

palette_data:
		.db $0f, $15, $20, $06
		.db $0f, $30, $2c, $15
		.db $0f, $21, $20, $0b
		.db $0f, $00, $10, $30
		;
		;
		;
		.db $0f, $16, $27, $18
		.db $0f, $0f, $30, $38
		.db $0f, $30, $2c, $11
		.db $0f, $00, $10, $30

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
clear_memory:
		lda #$00
		sta $0000, x
		sta $0100, x
		sta $0300, x
		sta $0400, x
		sta $0500, x
		sta $0600, x
		sta $0700, x
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
		; Copy static sprite-data over
		;
		ldx static_sprite_size
copy_more_sprites:
		lda static_sprite_data, x
		sta $200, x
		dex
		bne copy_more_sprites
		;
		; Enable sound
		;
		lda #$0f
		sta OperMode			; Anything but zero and it will play...
		sta SND_MASTERCTRL_REG
		lda #WaterMusic
		sta AreaMusicQueue
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

	.seekoff $4000 $00