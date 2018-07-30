;
; Appended to fpg_data.asm
;
fpg_offset_to_y:
		lda FpgSelected
		asl
		asl
		asl
		asl
		tay
		rts

fpg_validate:
		jsr fpg_offset_to_y
fpg_validate_unsafe:
		lda fpg_validate_func, y
		sta $0
		lda fpg_validate_func+1, y
		sta $1
		jmp ($0)

fpg_load_area:
		jsr fpg_offset_to_y
		lda fpg_load_area_func, y
		sta $0
		lda fpg_load_area_func+1, y
		sta $1
		jmp ($0)

fpg_load_player:
		jsr fpg_offset_to_y
		lda fpg_load_player_func, y
		sta $0
		lda fpg_load_player_func+1, y
		sta $1
		jmp ($0)
;
; 
;
fpg_check_true_over:
		lda FpgSelected
		asl
		asl
		asl
		asl
		tay
		lda fpg_num_routes, y
		inc FpgRuleset
		cmp FpgRuleset
		rts
;
; On bad x
;
fpg_failed_pos_x:
		jsr fpg_check_true_over
		beq fpg_game_over_x
		jmp fpg_validate_unsafe
fpg_game_over_x:
		ldx #$01
		jmp fpg_set_death_flag
;
; On bad y
;
fpg_failed_pos_y:
		jsr fpg_check_true_over
		beq fpg_game_over_y
		jmp fpg_validate_unsafe
fpg_game_over_y:
		ldx #$02
		jmp fpg_set_death_flag
;
; On bad input
;
fpg_failed_input:
		jsr fpg_check_true_over
		beq fpg_game_over_input
		jmp fpg_validate_unsafe
fpg_game_over_input:
		ldx #$03
fpg_set_death_flag:
		stx FpgError
		lda FpgFlags
		ora #$80
		sta FpgFlags
		rts
;
; Victory!
;
fpg_win:
		ldx #$09
		jmp fpg_set_death_flag


.seekoff $3fd0 $ea
EnterFpgLoadArea:
	lda #BANK_FPG_DATA
	jsr SetBankFromA
	jsr fpg_load_area
	lda BANK_SELECTED
	jmp SetBankFromA

EnterFpgLoadPlayer:
	lda #BANK_FPG_DATA
	jsr SetBankFromA
	jsr fpg_load_player
	lda BANK_SELECTED
	jmp SetBankFromA

EnterFpgValidate:
	lda #BANK_FPG_DATA
	jsr SetBankFromA
	jsr fpg_validate
	lda BANK_SELECTED
	jmp SetBankFromA







