	.vars vars.inc
	.org $8000
dummy:
	.db $ba, BANK_DUMMY

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