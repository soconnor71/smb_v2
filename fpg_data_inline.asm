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
		lda fpg_validate_func, y
		sta $0
		lda fpg_validate_func+1, y
		sta $1
		jmp ($0)

fpg_load_level:
		jsr fpg_offset_to_y
		lda fpg_load_level_func, y
		sta $0
		lda fpg_load_level_func+1, y
		sta $1
		jmp ($0)

fpg_load_player_func:
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
		rts
fpg_game_over_x:
		rts
;
; On bad y
;
fpg_failed_pos_y:
		jsr fpg_check_true_over
		beq fpg_game_over_y
		rts
fpg_game_over_y:
		rts
;
; On bad input
;
fpg_failed_input:
		jsr fpg_check_true_over
		beq fpg_game_over_input
		rts
fpg_game_over_input:
		rts
;
; Victory!
;
fpg_win:
		rts
