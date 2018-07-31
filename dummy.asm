	.vars vars.inc
	.org $8000
dummy:
	.db $ba, BANK_DUMMY
	.seekoff $c000 $00