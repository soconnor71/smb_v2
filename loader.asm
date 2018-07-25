	.vars vars.inc
	.org $8000
dummy:
	.db $ba, BANK_LOADER
	.seekoff $4000 $00