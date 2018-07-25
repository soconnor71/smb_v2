;
; Practice ROM
;
	.vars vars.inc
	.org $8000
	.db $ba, BANK_PRACTICE

;
; Identity-mapped swap-table
;
PTR_GetAreaDataAddrs:
        .dw SMB_GetAreaDataAddrs
PTR_AddToScore:
        .dw SMB_AddToScore
PTR_DigitsMathRoutine:
        .dw SMB_DigitsMathRoutine
PTR_UpdateNumber:
        .dw SMB_UpdateNumber
PTR_HandlePipeEntry:
        .dw SMB_HandlePipeEntry
PTR_GiveOneCoin:
        .dw SMB_GiveOneCoin

NonMaskableInterrupt:
	nop