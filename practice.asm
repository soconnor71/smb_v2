;
; Practice ROM
;
	.vars vars.inc
	.org $8000
	.db $ba, BANK_PRACTICE
;
; Identity-mapped swap-table
;
PTR_Start:
		.dw PRAC_Start
PTR_GetAreaDataAddrs:
		.dw PRAC_GetAreaDataAddrs
PTR_AddToScore:
		.dw PRAC_AddToScore
PTR_RunFireworks:
		.dw PRAC_RunFireworks
PTR_RunStarFlagObj:
		.dw PRAC_RunStarFlagObj
PTR_HandlePipeEntry:
		.dw PRAC_HandlePipeEntry
PTR_GiveOneCoin:
		.dw PRAC_GiveOneCoin
PTR_LoadChrROM:
		.dw PRAC_LoadChrROM

;-----------------------------------------------------------------

NonMaskableInterrupt:
               lda Mirror_PPU_CTRL_REG1  ;disable NMIs in mirror reg
               and #%01111111            ;save all other bits
               sta Mirror_PPU_CTRL_REG1
               and #%01111110            ;alter name table address to be $2800
               sta PPU_CTRL_REG1         ;(essentially $2000) but save other bits
               lda Mirror_PPU_CTRL_REG2  ;disable OAM and background display by default
               and #%11100110
               ldy DisableScreenFlag     ;get screen disable flag
               bne ScreenOff             ;if set, used bits as-is
               lda Mirror_PPU_CTRL_REG2  ;otherwise reenable bits and save them
               ora #%00011110
ScreenOff:     sta Mirror_PPU_CTRL_REG2  ;save bits for later but not in register at the moment
               and #%11100111            ;disable screen for now
               sta PPU_CTRL_REG2
               ldx PPU_STATUS            ;reset flip-flop and reset scroll registers to zero
               lda #$00
               jsr InitScroll
               sta PPU_SPR_ADDR          ;reset spr-ram address register
               lda #$02                  ;perform spr-ram DMA access on $0200-$02ff
               sta SPR_DMA
               ldx VRAM_Buffer_AddrCtrl  ;load control for pointer to buffer contents
               lda VRAM_AddrTable_Low,x  ;set indirect at $00 to pointer
               sta $00
               lda VRAM_AddrTable_High,x
               sta $01
               jsr UpdateScreen          ;update screen with buffer contents
               ldy #$00
               ldx VRAM_Buffer_AddrCtrl  ;check for usage of $0341
               cpx #$06
               bne InitBuffer
               iny                       ;get offset based on usage
InitBuffer:    ldx VRAM_Buffer_Offset,y
               lda #$00                  ;clear buffer header at last location
               sta VRAM_Buffer1_Offset,x        
               sta VRAM_Buffer1,x
               sta VRAM_Buffer_AddrCtrl  ;reinit address control to $0301
               lda Mirror_PPU_CTRL_REG2  ;copy mirror of $2001 to register
               sta PPU_CTRL_REG2
               jsr EnterSoundEngine      ;play sound
               lda SavedJoypad1Bits
               sta LastInputBits
               jsr ReadJoypads           ;read joypads
               jsr PauseRoutine          ;handle pause
               lda GamePauseStatus       ;check for pause status
               lsr
               bcs PauseSkip
               lda TimerControl          ;if master timer control not set, decrement
               beq DecTimers             ;all frame and interval timers
               dec TimerControl
               bne NoDecTimers
DecTimers:     ldx #$14                  ;load end offset for end of frame timers
               dec IntervalTimerControl  ;decrement interval timer control,
               bpl DecTimersLoop         ;if not expired, only frame timers will decrement
               lda #$14
               sta IntervalTimerControl  ;if control for interval timers expired,
               ldx #$23                  ;interval timers will decrement along with frame timers
DecTimersLoop: lda Timers,x              ;check current timer
               beq SkipExpTimer          ;if current timer expired, branch to skip,
               dec Timers,x              ;otherwise decrement the current timer
SkipExpTimer:  dex                       ;move onto next timer
               bpl DecTimersLoop         ;do this until all timers are dealt with
               jsr UpdateFrameRule
NoDecTimers:   inc FrameCounter          ;increment frame counter
               jmp NotPaused
PauseSkip:     jsr RedrawAll
NotPaused:
               jsr AdvanceRandom
               lda Sprite0HitDetectFlag  ;check for flag here
               beq SkipSprite0
Sprite0Clr:    lda PPU_STATUS            ;wait for sprite 0 flag to clear, which will
               and #%01000000            ;not happen until vblank has ended
               bne Sprite0Clr
               lda GamePauseStatus       ;if in pause mode, do not bother with sprites at all
               lsr
               bcs Sprite0Hit
               jsr MoveSpritesOffscreen
               jsr SpriteShuffler
Sprite0Hit:    lda PPU_STATUS            ;do sprite #0 hit detection
               and #%01000000
               beq Sprite0Hit
               ldy #$14                  ;small delay, to wait until we hit horizontal blank time
HBlankDelay:   dey
               bne HBlankDelay
SkipSprite0:   lda HorizontalScroll      ;set scroll registers from variables
               sta PPU_SCROLL_REG
               lda VerticalScroll
               sta PPU_SCROLL_REG
               lda Mirror_PPU_CTRL_REG1  ;load saved mirror of $2000
               pha
               sta PPU_CTRL_REG1
               lda GamePauseStatus       ;if in pause mode, do not perform operation mode stuff
               lsr
               bcs DoPowerupChange
               jsr OperModeExecutionTree ;otherwise do one of many, many possible subroutines
SkipMainOper:  lda PPU_STATUS            ;reset flip-flop
               pla
               ora #%10000000            ;reactivate NMIs
               sta PPU_CTRL_REG1
               rti                       ;we are done until the next frame!
DoPowerupChange:
				lda LastInputBits
				bne SkipMainOper
				lda SavedJoypad1Bits
				cmp #A_Button
				bne NoStarPower
				lda #$23                ;otherwise set star mario invincibility
				sta StarInvincibleTimer ;timer, and load the star mario music
				lda #StarPowerMusic     ;into the area music queue, then leave
				sta AreaMusicQueue
				jmp SkipMainOper
NoStarPower:	
				cmp #B_Button
				bne NoToggleFire
				ldx #2
				lda PlayerStatus
				cmp #2
				bne SetFirePowerup
				ldx #0
SetFirePowerup:
				stx PlayerStatus
				jsr GetPlayerColors
				jmp WritePowerupToSavestate
NoToggleFire:
				cmp #Down_Dir
				bne NoToggleSize
				ldy #0
				lda PlayerSize
				eor #1
				sta PlayerSize
				bne UpdateMarioGraphics
DrawBigMario:
				ldy #6
UpdateMarioGraphics:
				jsr GrowPlayer
				jsr PlayerGfxProcessing
				jsr GetPlayerColors
				jmp WritePowerupToSavestate
NoToggleSize:
				cmp #Right_Dir
				bne NoMakeSuper
				lda #0
				sta PlayerSize
				lda #1
				sta PlayerStatus
				jmp DrawBigMario
NoMakeSuper:
				cmp #Left_Dir
				bne SkipMainOper
				lda #1
				sta PlayerSize
				lda #0
				sta PlayerStatus
				tay
				jmp UpdateMarioGraphics

WritePowerupToSavestate:
				lda PlayerStatus
				asl
				sta $0
				lda SaveStateFlags
				and #$F8
				ora PlayerSize
				ora $0
				sta SaveStateFlags
				jmp SkipMainOper


PauseRoutine:
               lda OperMode           ;are we in victory mode?
               cmp #VictoryModeValue  ;if so, go ahead
               beq ChkPauseTimer
               cmp #GameModeValue     ;are we in game mode?
               bne ExitPause          ;if not, leave
               lda OperMode_Task      ;if we are in game mode, are we running game engine?
               cmp #$03
               bne ExitPause          ;if not, leave
               jsr HandleRestarts
ChkPauseTimer: lda GamePauseTimer     ;check if pause timer is still counting down
               beq ChkStart
               dec GamePauseTimer     ;if so, decrement and leave
               rts
ChkStart:      lda SavedJoypad1Bits   ;check to see if start is pressed
               and #Start_Button      ;on controller 1
               beq ClrPauseTimer
               lda SavedJoypad1Bits
               and #Up_Dir
               beq NoModeChange
               jmp ToggleRenderMode
NoModeChange:
               lda GamePauseStatus    ;check to see if timer flag is set
               and #%10000000         ;and if so, do not reset timer (residual,
               bne ExitPause          ;joypad reading routine makes this unnecessary)
               lda #$2b               ;set pause timer
               sta GamePauseTimer
               lda GamePauseStatus
               tay
               iny                    ;set pause sfx queue for next pause mode
               sty PauseSoundQueue
               eor #%00000001         ;invert d0 and set d7
               ora #%10000000
               bne SetPause           ;unconditional branch
ClrPauseTimer: lda GamePauseStatus    ;clear timer flag if timer is at zero and start button
               and #%01111111         ;is not pressed
SetPause:      sta GamePauseStatus
ExitPause:     rts

;
; The Practice Functions
;
;-------------------------------------------------------------------------------------

ToggleRenderMode:
		lda PracticeFlags
		eor #$80
		sta PracticeFlags
		and #$80
		bne SockMode
		;
		; If we change back to rule mode we must remove top sock bytes
		;
		ldx VRAM_Buffer_Offset
		lda #$20
		sta VRAM_Buffer1,x
		lda #$62 ;
		sta VRAM_Buffer1+1,x
		lda #$02 ; len
		sta VRAM_Buffer1+2,x
		lda #$24
		sta VRAM_Buffer1+3, x
		sta VRAM_Buffer1+4, x
		lda #$00
		sta VRAM_Buffer1+5, x
		lda VRAM_Buffer1_Offset
		clc
		adc #$05
		sta VRAM_Buffer1_Offset
		jmp RedrawFrameNumbers
SockMode:
		jmp ForceUpdateSockHash


PRAC_LoadChrROM:
		lda #CHR_PRACTICE
		jmp SetChrFromA

;-------------------------------------------------------------------------------------

UpdateFrameRule:
		lda #$14
		cmp IntervalTimerControl
		bne NotEvenFrameRule
		lda #$01
		sta DigitModifier+5
		ldy #RULE_COUNT_OFFSET
		jsr DigitsMathRoutine3
NotEvenFrameRule:
		rts

;-------------------------------------------------------------------------------------

RedrawRemaining:
		lda IntervalTimerControl
		jsr DivByTen
		sta DisplayDigits+FRAMES_REMAIN_OFFSET
		stx DisplayDigits+FRAMES_REMAIN_OFFSET-1
		lda #$a6
		jsr PrintStatusBarNumbers
		rts

RedrawAll:
		jsr RedrawRemaining
		jsr RedrawFrameNumbers
		rts

;-------------------------------------------------------------------------------------

RedrawPosition:
		lda Player_Rel_XPos
		jsr DivByTen
		sta DisplayDigits+POSITION_OFFSET
		txa
		jsr DivByTen
		sta DisplayDigits+POSITION_OFFSET-1
		stx DisplayDigits+POSITION_OFFSET-2
		lda #$a5
		jsr PrintStatusBarNumbers
		rts

;-------------------------------------------------------------------------------------
;$00 - vram buffer address table low, also used for pseudorandom bit
;$01 - vram buffer address table high

VRAM_AddrTable_Low:
      .db <VRAM_Buffer1, <WaterPaletteData, <GroundPaletteData
      .db <UndergroundPaletteData, <CastlePaletteData, <VRAM_Buffer1_Offset
      .db <VRAM_Buffer2, <VRAM_Buffer2, <BowserPaletteData
      .db <DaySnowPaletteData, <NightSnowPaletteData, <MushroomPaletteData
      .db <MarioThanksMessage, <LuigiThanksMessage, <MushroomRetainerSaved
      .db <PrincessSaved1, <PrincessSaved2, <WorldSelectMessage1
      .db <WorldSelectMessage2

VRAM_AddrTable_High:
      .db >VRAM_Buffer1, >WaterPaletteData, >GroundPaletteData
      .db >UndergroundPaletteData, >CastlePaletteData, >VRAM_Buffer1_Offset
      .db >VRAM_Buffer2, >VRAM_Buffer2, >BowserPaletteData
      .db >DaySnowPaletteData, >NightSnowPaletteData, >MushroomPaletteData
      .db >MarioThanksMessage, >LuigiThanksMessage, >MushroomRetainerSaved
      .db >PrincessSaved1, >PrincessSaved2, >WorldSelectMessage1
      .db >WorldSelectMessage2

VRAM_Buffer_Offset:
      .db <VRAM_Buffer1_Offset, <VRAM_Buffer2_Offset

;-------------------------------------------------------------------------------------
;$00 - used for preset value

SpriteShuffler:
               ldy AreaType                ;load level type, likely residual code
               lda #$28                    ;load preset value which will put it at
               sta $00                     ;sprite #10
               ldx #$0e                    ;start at the end of OAM data offsets
ShuffleLoop:   lda SprDataOffset,x         ;check for offset value against
               cmp $00                     ;the preset value
               bcc NextSprOffset           ;if less, skip this part
               ldy SprShuffleAmtOffset     ;get current offset to preset value we want to add
               clc
               adc SprShuffleAmt,y         ;get shuffle amount, add to current sprite offset
               bcc StrSprOffset            ;if not exceeded $ff, skip second add
               clc
               adc $00                     ;otherwise add preset value $28 to offset
StrSprOffset:  sta SprDataOffset,x         ;store new offset here or old one if branched to here
NextSprOffset: dex                         ;move backwards to next one
               bpl ShuffleLoop
               ldx SprShuffleAmtOffset     ;load offset
               inx
               cpx #$03                    ;check if offset + 1 goes to 3
               bne SetAmtOffset            ;if offset + 1 not 3, store
               ldx #$00                    ;otherwise, init to 0
SetAmtOffset:  stx SprShuffleAmtOffset
               ldx #$08                    ;load offsets for values and storage
               ldy #$02
SetMiscOffset: lda SprDataOffset+5,y       ;load one of three OAM data offsets
               sta Misc_SprDataOffset-2,x  ;store first one unmodified, but
               clc                         ;add eight to the second and eight
               adc #$08                    ;more to the third one
               sta Misc_SprDataOffset-1,x  ;note that due to the way X is set up,
               clc                         ;this code loads into the misc sprite offsets
               adc #$08
               sta Misc_SprDataOffset,x        
               dex
               dex
               dex
               dey
               bpl SetMiscOffset           ;do this until all misc spr offsets are loaded
               rts

;-------------------------------------------------------------------------------------

OperModeExecutionTree:
      lda OperMode     ;this is the heart of the entire program,
      jsr JumpEngine   ;most of what goes on starts here

      .dw TitleScreenMode
      .dw GameMode
      .dw VictoryMode
      ; .dw GameOverMode

;-------------------------------------------------------------------------------------

TitleScreenMode:
      lda OperMode_Task
      jsr JumpEngine

      .dw InitializeGame
      .dw ScreenRoutines
      .dw PrimaryGameSetup
      .dw GameMenuRoutine

;-------------------------------------------------------------------------------------

EraseStartRule:
		ldx #5
		lda #0
EraseStartRuleLoop:
		sta TopScoreDisplay+1,x
		dex
		bne EraseStartRuleLoop
		rts

;-------------------------------------------------------------------------------------

DrawPowerUps:
	ldy PowerUps
	lda #$6b
	jmp DrawSelectedNumber

;-------------------------------------------------------------------------------------

WarplessRules:
	;<BUILD_PATCH_LEVELS>
	.dw 0, 0, 0, 0 ; World 1
	.dw 0, 0, 0, 0 ; World 2
	.dw 0, 0, 0, 0 ; World 3
	.dw 0, 0, 0, 0 ; World 4
	.dw 0, 0, 0, 0 ; World 5
	.dw 0, 0, 0, 0 ; World 6
	.dw 0, 0, 0, 0 ; World 7
	.dw 0, 0, 0, 0 ; World 8
	;</BUILD_PATCH_LEVELS>

PupsCollected:
	;<BUILD_PATCH_PUPS>
	.db 0, 0, 0, 0, 0, 0, 0, 0
	;</BUILD_PATCH_PUPS>

ByteToRule:
	pha
	and #$0f
	sta TopScoreDisplay+3,y
	pla
	and #$f0
	ror
	ror
	ror
	ror
	sta TopScoreDisplay+2,y
	rts

SetPerfectLevelRule:
	lda WorldNumber
	asl
	asl
	asl
	sta $0
	lda LevelNumber
	asl
	clc
	adc $0
	tax
	lda WarplessRules, x
	ldy #2
	jsr ByteToRule
	inx
	ldy #0
	lda WarplessRules, x
	jsr ByteToRule
	;
	; Solve number of powerups
	;
	ldx WorldNumber
	lda PupsCollected, x
	ldy LevelNumber
	beq DoneShifting
MoreShifting:
	ror
	ror
	dey
	bne MoreShifting
DoneShifting:
	and #$03
	sta PowerUps
	jmp DrawPowerUps

;-------
LoadGameState:
		lda #$80
		sta GamePauseStatus
		ldx #0
		stx PauseModeFlag
		inx
		stx GamePauseTimer
		inx
		stx PauseSoundQueue
		lda SaveStateFlags
		ora #$40
		sta SaveStateFlags
		ldx #$00
		stx NoteLengthTblAdder ; Less hysterical music
		stx OperMode_Task
		stx HalfwayPage
		inx
		stx OperMode
		jsr LoadAreaPointer       ;get new level pointer
		inc FetchNewGameTimerFlag ;set flag to load new game timer
		;jsr ChgAreaMode           ;do sub to set secondary mode, disable screen and sprite 0
		;           ;reset halfway page to 0 (beginning)
ExitRestarts:
		rts

HandleRestarts:
		lda JoypadBitMask
		ora SavedJoypadBits
		eor #Select_Button
		cmp #Up_Dir
		beq LoadGameState
		cmp #Down_Dir
		bne ExitRestarts
		jmp InitMapper

LoadSaveState:
		lda #$0
		sta PlayerChangeSizeFlag
		lda SaveIntervalTimerControl
		sta IntervalTimerControl
		lda SaveFrame
		sta FrameCounter
		lda SaveStateFlags
		lsr
		and #3
		sta PlayerStatus
		lda SaveStateFlags
		and #1
		sta PlayerSize
		ldx #6
RestoreMoreRandom:
		lda SavedRandomData, x
		sta PseudoRandomBitReg,x
		dex
		bpl RestoreMoreRandom
		ldx #3
LoadNextRule:
		lda SaveFrameRuleData, x
		sta FrameRuleData, x
		dex
		bpl LoadNextRule

		lda SaveStateFlags
		and #$bf ; Nuke load flag :)
		sta SaveStateFlags
LuaHackDumpLoad: ; Euw...
		rts

LoadSaveStateProxy:
		jmp LoadSaveState

HandleSaveState:
		lda OperMode
		beq AlreadyHasSaveState
		lda SaveStateFlags
		and #$40
		bne LoadSaveStateProxy
		lda SaveStateFlags
		and #$80
		bne AlreadyHasSaveState
SaveCurrentState:
		lda IntervalTimerControl
		sta SaveIntervalTimerControl
		lda FrameCounter
		sta SaveFrame
		lda PlayerStatus
		asl
		ora PlayerSize
		ora #$80
		sta SaveStateFlags
		ldx #6
SaveMoreRandom:
		lda PseudoRandomBitReg,x
		sta SavedRandomData, x
		dex
		bpl SaveMoreRandom

		ldx $3
SaveNextRule:
		lda FrameRuleData, x
		sta SaveFrameRuleData, x
		dex
		bpl SaveNextRule

AlreadyHasSaveState:
		rts

;-------------------------------------------------------------------------------------

DrawSelectedNumber:
		pha
		ldx VRAM_Buffer1_Offset
		lda #$22                ;write address for world-area number on screen
		sta VRAM_Buffer1,x
		pla
		sta VRAM_Buffer1+1,x
		lda #$01
		sta VRAM_Buffer1+2,x
		tya
		sta VRAM_Buffer1+3,x
		lda #0
		sta VRAM_Buffer1+4,x
		txa
		clc
		adc #4
		sta VRAM_Buffer1_Offset
		rts

;-------------------------------------------------------------------------------------
IsBigWorld:
	.db 1, 1, 0, 1, 0, 0, 1, 0

NukeTimer:
		lda #0
		sta SelectTimer
		jmp MenuDone

ChangeSelection:
		ldx MenuSelection
		inx
		cpx #4
		bne SaveSelection
		ldx #0
SaveSelection:
		stx MenuSelection
		jmp MenuDone

GameMenuRoutine:
		lda FirstTimeInit
		bne MenuInitialized
		jsr SetPerfectLevelRule
		lda #$01
		sta FirstTimeInit
MenuInitialized:
		jsr GetPlayerColors
		ldy #$58
		jsr DrawPlayer_Intermediate
		lda JoypadBitMask
		ora SavedJoypadBits
		beq NukeTimer
		ldx SelectTimer
		bne CantMove
		ldx #32
		stx SelectTimer
		cmp #B_Button
		bne IsSelectPressed
		jsr EraseStartRule
		jmp MenuDone
IsSelectPressed:
		cmp #Select_Button
		beq ChangeSelection
		cmp #Start_Button|A_Button
		beq LetsPlayMarioSecondQuest
		cmp #Start_Button
		beq LetsPlayMario
		jmp SelectionInput
LetsPlayMarioSecondQuest:
		inc PrimaryHardMode
LetsPlayMario:
		ldx LevelNumber
		ldy WorldNumber
		sty OffScr_WorldNumber
		lda IsBigWorld, y
		beq SaveAreaNmber
		cpx #2
		bmi SaveAreaNmber
		inx
SaveAreaNmber:
		stx AreaNumber
		stx OffScr_AreaNumber
		;
		; Start it...
		;
		jsr LoadAreaPointer
		inc Hidden1UpFlag
		inc OffScr_Hidden1UpFlag
		inc FetchNewGameTimerFlag
		inc OperMode
		lda #$00
		sta OperMode_Task
		rts
CantMove:
		lda SelectTimer
		beq MenuDone
		dec SelectTimer
MenuDone:
		jsr DrawMushroomIcon
		jsr DrawRuleCursor
		lda #$fa
		jsr UpdateNumber
		sta SavedJoypad1Bits
		jsr RedrawFrameNumbers
		rts

;-------------------------------------------------------------------------------------

RuleInput:
		ldx RuleIndex
		cmp #Left_Dir
		bne RTestRight
		dex
		jmp RuleHori
RTestRight:
		cmp #Right_Dir
		bne RTestDown
		inx
RuleHori:
		cpx #1
		bpl RuleTestHigh
		ldx #4
RuleTestHigh:
		cpx #5
		bne SaveRuleIndex
		ldx #1
SaveRuleIndex:
		stx RuleIndex
		rts
RTestDown:
		cmp #Down_Dir
		bne RTestUp
		lda #$ff
		jmp RUpdate
RTestUp:
		cmp #Up_Dir
		bne RuleUpdated
		lda #$01
RUpdate:
		ldx RuleIndex
		clc
		adc TopScoreDisplay+1,x
		bmi IsNegative
		cmp #10
		bmi SaveRuleDigit
		lda #0
		jmp SaveRuleDigit
IsNegative:
		lda #9
SaveRuleDigit:
		sta TopScoreDisplay+1,x
RuleUpdated:
		rts

;-------------------------------------------------------------------------------------

SelectionInput:
		ldy WorldNumber
		ldx MenuSelection
		beq ValueSelected
		cpx #1
		beq LevelInput
		cpx #2
		bne RuleInput
		ldy PowerUps
		jmp ValueSelected
LevelInput:
		ldy LevelNumber
ValueSelected:
		cmp #Left_Dir
		beq DecreaseLevel
		cmp #Right_Dir
		beq IncreaseLevel
		cmp #Up_Dir
		bne WorldSelectionDone
		lda #1
		eor CurrentPlayer
		sta CurrentPlayer
		rts
IncreaseLevel:
		iny
		bne SaveNewLevel ; Cant be zero, short jmp
DecreaseLevel:
		dey
SaveNewLevel:
		tya
		cpx #0
		bne SaveLevelNotWorld
		and #$07
		sta WorldNumber
		ldx #$2b ; offset
		bne RedrawIt ; Cant be zero, short jump
SaveLevelNotWorld:
		cpx #2
		beq SavePowerUps
		and #$03
		sta LevelNumber
		ldx #$4b ; offset
		bne RedrawIt
SavePowerUps:
		cmp #4
		bne NotThreeLol
		lda #0
NotThreeLol:
		cmp #$ff
		bne NotNegative
		lda #3
NotNegative:
		sta PowerUps
		jmp DrawPowerUps
RedrawIt:
		tay
		iny
		txa
		jsr DrawSelectedNumber
		jsr SetPerfectLevelRule
WorldSelectionDone:
		rts

;-------------------------------------------------------------------------------------

RuleCursorData:
	.db $22, $aa, $06, $24, $24, $24, $24, $24, $24, $00

DrawRuleCursor:
		ldy #9
		lda VRAM_Buffer1_Offset
		clc
		adc #9
		sta VRAM_Buffer1_Offset
		tax
WriteRuleCursor:
		lda RuleCursorData,y
		sta VRAM_Buffer1,x
		dex
		dey
		bpl WriteRuleCursor
		lda VRAM_Buffer1_Offset
		sec
		sbc #6
		adc RuleIndex
		tax
		dex
		lda #$29
		sta VRAM_Buffer1,x
		rts

;-------------------------------------------------------------------------------------

MushroomIconData:
		.db $22, $29, $87, $24, $24, $24, $24, $24, $24, $24, $00
DrawMushroomIcon:
		ldy #$0a
		lda VRAM_Buffer1_Offset
		clc
		adc #$0a
		sta VRAM_Buffer1_Offset
		tax
IconDataRead:
		lda MushroomIconData,y
		sta VRAM_Buffer1,x
		dex
		dey
		bpl IconDataRead
		lda MenuSelection
		cmp #3
		bmi FirstThree
		clc
		adc #2
FirstThree:
		adc VRAM_Buffer1_Offset
		tax
		lda #$ce
		sta VRAM_Buffer1+3-$0a,x
		rts

;-------------------------------------------------------------------------------------

VictoryMode:
            jsr VictoryModeSubroutines  ;run victory mode subroutines
            lda OperMode_Task           ;get current task of victory mode
            beq AutoPlayer              ;if on bridge collapse, skip enemy processing
            ldx #$00
            stx ObjectOffset            ;otherwise reset enemy object offset 
            jsr EnemiesAndLoopsCore     ;and run enemy code
AutoPlayer: jsr RelativePlayerPosition  ;get player's relative coordinates
            jmp PlayerGfxHandler        ;draw the player, then leave

VictoryModeSubroutines:
      lda OperMode_Task
      jsr JumpEngine

      .dw BridgeCollapse
      .dw SetupVictoryMode
      .dw PlayerVictoryWalk
      .dw PrintVictoryMessages
      .dw PlayerEndWorld

;-------------------------------------------------------------------------------------

SetupVictoryMode:
      ldx ScreenRight_PageLoc  ;get page location of right side of screen
      inx                      ;increment to next page
      stx DestinationPageLoc   ;store here
      lda #EndOfCastleMusic
      sta EventMusicQueue      ;play win castle music
      jmp IncModeTask_B        ;jump to set next major task in victory mode

;-------------------------------------------------------------------------------------

PlayerVictoryWalk:
             ldy #$00                ;set value here to not walk player by default
             sty VictoryWalkControl
             lda Player_PageLoc      ;get player's page location
             cmp DestinationPageLoc  ;compare with destination page location
             bne PerformWalk         ;if page locations don't match, branch
             lda Player_X_Position   ;otherwise get player's horizontal position
             cmp #$60                ;compare with preset horizontal position
             bcs DontWalk            ;if still on other page, branch ahead
PerformWalk: inc VictoryWalkControl  ;otherwise increment value and Y
             iny                     ;note Y will be used to walk the player
DontWalk:    tya                     ;put contents of Y in A and
             jsr AutoControlPlayer   ;use A to move player to the right or not
             lda ScreenLeft_PageLoc  ;check page location of left side of screen
             cmp DestinationPageLoc  ;against set value here
             beq ExitVWalk           ;branch if equal to change modes if necessary
             lda ScrollFractional
             clc                     ;do fixed point math on fractional part of scroll
             adc #$80        
             sta ScrollFractional    ;save fractional movement amount
             lda #$01                ;set 1 pixel per frame
             adc #$00                ;add carry from previous addition
             tay                     ;use as scroll amount
             jsr ScrollScreen        ;do sub to scroll the screen
             jsr UpdScrollVar        ;do another sub to update screen and scroll variables
             inc VictoryWalkControl  ;increment value to stay in this routine
ExitVWalk:   lda VictoryWalkControl  ;load value set here
             beq IncModeTask_A       ;if zero, branch to change modes
             rts                     ;otherwise leave

;-------------------------------------------------------------------------------------

PrintVictoryMessages:
               lda SecondaryMsgCounter   ;load secondary message counter
               bne IncMsgCounter         ;if set, branch to increment message counters
               lda PrimaryMsgCounter     ;otherwise load primary message counter
               beq ThankPlayer           ;if set to zero, branch to print first message
               cmp #$09                  ;if at 9 or above, branch elsewhere (this comparison
               bcs IncMsgCounter         ;is residual code, counter never reaches 9)
               ldy WorldNumber           ;check world number
               cpy #World8
               bne MRetainerMsg          ;if not at world 8, skip to next part
               cmp #$03                  ;check primary message counter again
               bcc IncMsgCounter         ;if not at 3 yet (world 8 only), branch to increment
               sbc #$01                  ;otherwise subtract one
               jmp ThankPlayer           ;and skip to next part
MRetainerMsg:  cmp #$02                  ;check primary message counter
               bcc IncMsgCounter         ;if not at 2 yet (world 1-7 only), branch
ThankPlayer:   tay                       ;put primary message counter into Y
               bne SecondPartMsg         ;if counter nonzero, skip this part, do not print first message
               lda CurrentPlayer         ;otherwise get player currently on the screen
               beq EvalForMusic          ;if mario, branch
               iny                       ;otherwise increment Y once for luigi and
               bne EvalForMusic          ;do an unconditional branch to the same place
SecondPartMsg: iny                       ;increment Y to do world 8's message
               lda WorldNumber
               cmp #World8               ;check world number
               beq EvalForMusic          ;if at world 8, branch to next part
               dey                       ;otherwise decrement Y for world 1-7's message
               cpy #$04                  ;if counter at 4 (world 1-7 only)
               bcs SetEndTimer           ;branch to set victory end timer
               cpy #$03                  ;if counter at 3 (world 1-7 only)
               bcs IncMsgCounter         ;branch to keep counting
EvalForMusic:  cpy #$03                  ;if counter not yet at 3 (world 8 only), branch
               bne PrintMsg              ;to print message only (note world 1-7 will only
               lda #VictoryMusic         ;reach this code if counter = 0, and will always branch)
               sta EventMusicQueue       ;otherwise load victory music first (world 8 only)
PrintMsg:      tya                       ;put primary message counter in A
               clc                       ;add $0c or 12 to counter thus giving an appropriate value,
               adc #$0c                  ;($0c-$0d = first), ($0e = world 1-7's), ($0f-$12 = world 8's)
               sta VRAM_Buffer_AddrCtrl  ;write message counter to vram address controller
IncMsgCounter: lda SecondaryMsgCounter
               clc
               adc #$04                      ;add four to secondary message counter
               sta SecondaryMsgCounter
               lda PrimaryMsgCounter
               adc #$00                      ;add carry to primary message counter
               sta PrimaryMsgCounter
               cmp #$07                      ;check primary counter one more time
SetEndTimer:   bcc ExitMsgs                  ;if not reached value yet, branch to leave
               jsr RedrawAll
               lda #$06
               sta WorldEndTimer             ;otherwise set world end timer
IncModeTask_A: inc OperMode_Task             ;move onto next task in mode
ExitMsgs:      rts                           ;leave

;-------------------------------------------------------------------------------------

PlayerEndWorld:
               lda WorldEndTimer          ;check to see if world end timer expired
               bne EndExitOne             ;branch to leave if not
               ldy WorldNumber            ;check world number
               cpy #World8                ;if on world 8, player is done with game, 
               bcs EndChkBButton          ;thus branch to read controller
               lda #$00
               sta SaveStateFlags
               sta AreaNumber             ;otherwise initialize area number used as offset
               sta LevelNumber            ;and level number control to start at area 1
               sta OperMode_Task          ;initialize secondary mode of operation
               inc WorldNumber            ;increment world number to move onto the next world
               jsr LoadAreaPointer        ;get area address offset for the next area
               inc FetchNewGameTimerFlag  ;set flag to load game timer from header
               lda #GameModeValue
               sta OperMode               ;set mode of operation to game mode
EndExitOne:    rts                        ;and leave
EndChkBButton: 
               lda SavedJoypad1Bits
               ora SavedJoypad2Bits       ;check to see if B button was pressed on
               and #B_Button              ;either controller
               beq EndExitOne
               jmp InitMapper ; hackreset

;-------------------------------------------------------------------------------------

;data is used as tiles for numbers
;that appear when you defeat enemies
FloateyNumTileData:
      .db $ff, $ff ;dummy
      .db $f6, $fb ; "100"
      .db $f7, $fb ; "200"
      .db $f8, $fb ; "400"
      .db $f9, $fb ; "500"
      .db $fa, $fb ; "800"
      .db $f6, $50 ; "1000"
      .db $f7, $50 ; "2000"
      .db $f8, $50 ; "4000"
      .db $f9, $50 ; "5000"
      .db $fa, $50 ; "8000"
      .db $fd, $fe ; "1-UP"

;high nybble is digit number, low nybble is number to
;add to the digit of the player's score
ScoreUpdateData:
      .db $ff ;dummy
      .db $41, $42, $44, $45, $48
      .db $31, $32, $34, $35, $38, $00

FloateyNumbersRoutine:
              lda FloateyNum_Control,x     ;load control for floatey number
              beq EndExitOne               ;if zero, branch to leave
              cmp #$0b                     ;if less than $0b, branch
              bcc ChkNumTimer
              lda #$0b                     ;otherwise set to $0b, thus keeping
              sta FloateyNum_Control,x     ;it in range
ChkNumTimer:  tay                          ;use as Y
              lda FloateyNum_Timer,x       ;check value here
              bne DecNumTimer              ;if nonzero, branch ahead
              sta FloateyNum_Control,x     ;initialize floatey number control and leave
              rts
DecNumTimer:  dec FloateyNum_Timer,x       ;decrement value here
              cmp #$2b                     ;if not reached a certain point, branch  
              bne ChkTallEnemy
              cpy #$0b                     ;check offset for $0b
              bne LoadNumTiles             ;branch ahead if not found
              lda #Sfx_ExtraLife
              sta Square2SoundQueue        ;and play the 1-up sound
LoadNumTiles: jsr RedrawFrameNumbers
ChkTallEnemy: ldy Enemy_SprDataOffset,x    ;get OAM data offset for enemy object
              lda Enemy_ID,x               ;get enemy object identifier
              cmp #Spiny
              beq FloateyPart              ;branch if spiny
              cmp #PiranhaPlant
              beq FloateyPart              ;branch if piranha plant
              cmp #HammerBro
              beq GetAltOffset             ;branch elsewhere if hammer bro
              cmp #GreyCheepCheep
              beq FloateyPart              ;branch if cheep-cheep of either color
              cmp #RedCheepCheep
              beq FloateyPart
              cmp #TallEnemy
              bcs GetAltOffset             ;branch elsewhere if enemy object => $09
              lda Enemy_State,x
              cmp #$02                     ;if enemy state defeated or otherwise
              bcs FloateyPart              ;$02 or greater, branch beyond this part
GetAltOffset: ldx SprDataOffset_Ctrl       ;load some kind of control bit
              ldy Alt_SprDataOffset,x      ;get alternate OAM data offset
              ldx ObjectOffset             ;get enemy object offset again
FloateyPart:  lda FloateyNum_Y_Pos,x       ;get vertical coordinate for
              cmp #$18                     ;floatey number, if coordinate in the
              bcc SetupNumSpr              ;status bar, branch
              sbc #$01
              sta FloateyNum_Y_Pos,x       ;otherwise subtract one and store as new
SetupNumSpr:  lda FloateyNum_Y_Pos,x       ;get vertical coordinate
              sbc #$08                     ;subtract eight and dump into the
              jsr DumpTwoSpr               ;left and right sprite's Y coordinates
              lda FloateyNum_X_Pos,x       ;get horizontal coordinate
              sta Sprite_X_Position,y      ;store into X coordinate of left sprite
              clc
              adc #$08                     ;add eight pixels and store into X
              sta Sprite_X_Position+4,y    ;coordinate of right sprite
              lda #$02
              sta Sprite_Attributes,y      ;set palette control in attribute bytes
              sta Sprite_Attributes+4,y    ;of left and right sprites
              lda FloateyNum_Control,x
              asl                          ;multiply our floatey number control by 2
              tax                          ;and use as offset for look-up table
              lda FloateyNumTileData,x
              sta Sprite_Tilenumber,y      ;display first half of number of points
              lda FloateyNumTileData+1,x
              sta Sprite_Tilenumber+4,y    ;display the second half
              ldx ObjectOffset             ;get enemy object offset and leave
              rts

;-------------------------------------------------------------------------------------

ScreenRoutines:
      lda ScreenRoutineTask        ;run one of the following subroutines
      jsr JumpEngine
    
      .dw InitScreen
      .dw SetupIntermediate
      .dw WriteTopStatusLine
      .dw WriteBottomStatusLine
      .dw DisplayTimeUp
      .dw ResetSpritesAndScreenTimer
      .dw DisplayIntermediate
      .dw ResetSpritesAndScreenTimer
      .dw AreaParserTaskControl
      .dw GetAreaPalette
      .dw GetBackgroundColor
      .dw GetAlternatePalette1
      .dw DrawTitleScreen
      .dw ClearBuffersDrawIcon
      .dw WriteTopScore

;-------------------------------------------------------------------------------------

InitScreen:
      jsr MoveAllSpritesOffscreen ;initialize all sprites including sprite #0
      jsr InitializeNameTables    ;and erase both name and attribute tables
      lda OperMode
      beq NextSubtask             ;if mode still 0, do not load
      ldx #$03                    ;into buffer pointer
      jmp SetVRAMAddr_A
NextSubtask:
      jmp IncSubtask           ;move onto next task

;-------------------------------------------------------------------------------------

SetupIntermediate:
      lda BackgroundColorCtrl  ;save current background color control
      pha                      ;and player status to stack
      lda PlayerStatus
      pha
      lda #$00                 ;set background color to black
      sta PlayerStatus         ;and player status to not fiery
      lda #$02                 ;this is the ONLY time background color control
      sta BackgroundColorCtrl  ;is set to less than 4
      jsr GetPlayerColors
      pla                      ;we only execute this routine for
      sta PlayerStatus         ;the intermediate lives display
      pla                      ;and once we're done, we return bg
      sta BackgroundColorCtrl  ;color ctrl and player status from stack
      jmp IncSubtask           ;then move onto the next task

;-------------------------------------------------------------------------------------

WriteTopStatusLine:
      lda #$00          ;select main status bar
      jsr WriteGameText ;output it
      jmp IncSubtask    ;onto the next task

;-------------------------------------------------------------------------------------

WriteBottomStatusLine:
      jsr GetSBNybbles        ;write player's score and coin tally to screen
      jmp IncSubtask

;-------------------------------------------------------------------------------------

DisplayTimeUp:
          lda GameTimerExpiredFlag  ;if game timer not expired, increment task
          beq NoTimeUp              ;control 2 tasks forward, otherwise, stay here
          lda #$00
          sta GameTimerExpiredFlag  ;reset timer expiration flag
          lda #$02                  ;output time-up screen to buffer
          jmp OutputInter
NoTimeUp: inc ScreenRoutineTask     ;increment control task 2 tasks forward
          jmp IncSubtask

;-------------------------------------------------------------------------------------

DisplayIntermediate:
               lda OperMode                 ;check primary mode of operation
               beq NoInter                  ;if in title screen mode, skip this
               cmp #GameOverModeValue       ;are we in game over mode?
               beq GameOverInter            ;if so, proceed to display game over screen
               lda AltEntranceControl       ;otherwise check for mode of alternate entry
               bne NoInter                  ;and branch if found
               ldy AreaType                 ;check if we are on castle level
               cpy #$03                     ;and if so, branch (possibly residual)
               beq PlayerInter
               lda DisableIntermediate      ;if this flag is set, skip intermediate lives display
               bne NoInter                  ;and jump to specific task, otherwise
PlayerInter:   jsr DrawPlayer_Intermediate  ;put player in appropriate place for
               lda #$01                     ;lives display, then output lives display to buffer
OutputInter:   jsr WriteGameText
               jsr ResetScreenTimer
               lda #$00
               sta DisableScreenFlag        ;reenable screen output
               rts
GameOverInter: lda #$12                     ;set screen timer
               sta ScreenTimer
               lda #$03                     ;output game over screen to buffer
               jsr WriteGameText
               jmp IncModeTask_B
NoInter:       lda #$08                     ;set for specific task and leave
               sta ScreenRoutineTask
               rts

;-------------------------------------------------------------------------------------

AreaParserTaskControl:
           inc DisableScreenFlag     ;turn off screen
TaskLoop:  jsr AreaParserTaskHandler ;render column set of current area
           lda AreaParserTaskNum     ;check number of tasks
           bne TaskLoop              ;if tasks still not all done, do another one
           dec ColumnSets            ;do we need to render more column sets?
           bpl OutputCol
           inc ScreenRoutineTask     ;if not, move on to the next task
OutputCol: lda #$06                  ;set vram buffer to output rendered column set
           sta VRAM_Buffer_AddrCtrl  ;on next NMI
           rts

;-------------------------------------------------------------------------------------

;$00 - vram buffer address table low
;$01 - vram buffer address table high

DrawTitleScreen:
            lda OperMode                 ;are we in title screen mode?
            bne IncModeTask_B            ;if not, exit
            lda #>TitleScreenDataOffset  ;load address $1ec0 into
            sta PPU_ADDRESS              ;the vram address register
            lda #<TitleScreenDataOffset
            sta PPU_ADDRESS
            lda #$03                     ;put address $0300 into
            sta $01                      ;the indirect at $00
            ldy #$00
            sty $00
            lda PPU_DATA                 ;do one garbage read
OutputTScr: lda PPU_DATA                 ;get title screen from chr-rom
            sta ($00),y                  ;store 256 bytes into buffer
            iny
            bne ChkHiByte                ;if not past 256 bytes, do not increment
            inc $01                      ;otherwise increment high byte of indirect
ChkHiByte:  lda $01                      ;check high byte?
            cmp #$04                     ;at $0400?
            bne OutputTScr               ;if not, loop back and do another
            cpy #$3a                     ;check if offset points past end of data
            bcc OutputTScr               ;if not, loop back and do another
            lda #$05                     ;set buffer transfer control to $0300,
            jmp SetVRAMAddr_B            ;increment task and exit

;-------------------------------------------------------------------------------------

ClearBuffersDrawIcon:
             lda OperMode               ;check game mode
             bne IncModeTask_B          ;if not title screen mode, leave
             ldx #$00                   ;otherwise, clear buffer space
TScrClear:   sta VRAM_Buffer1-1,x
             sta VRAM_Buffer1-1+$100,x
             dex
             bne TScrClear
             jsr DrawMushroomIcon       ;draw player select icon
             inc ScreenRoutineTask      ;move onto next task
             rts

;-------------------------------------------------------------------------------------

WriteTopScore:
               lda #$fa           ;run display routine to display top score on title
               jsr UpdateNumber
IncModeTask_B: inc OperMode_Task  ;move onto next mode
               rts

;-------------------------------------------------------------------------------------

GameText:
TopStatusBarLine:
  ; <off, size>
  .db $20, $44, $0c
  ;
  .db $1b, $1e, $15, $0e      ; "RULE"
  .db $24, $29, $24           ; "x"
  .db $0f, $1b, $0a, $16, $0e ; "FRAME"
  ; <off, size>
  .db $20, $52, $0c
  ; 'RM POS  TIME J'
  .db $1b, $16, $24, $19, $18, $1c, $24, $1d, $12, $16, $0e, $24
   ; <off, size>
  .db $20, $68, $05, $24, $fe, $24, $2e, $29 ; score trailing digit and coin display
  .db $23, $c0, $7f, $aa ; attribute table data, clears name table 0 to palette 2
  .db $23, $c2, $01, $ea ; attribute table data, used for coin icon in status bar
  .db $ff ; end of data block

WorldLivesDisplay:
  .db $21, $cd, $07, $24, $24 ; cross with spaces used on
  .db $29, $24, $24, $24, $24 ; lives display
  .db $21, $4b, $09, $20, $18 ; "WORLD  - " used on lives display
  .db $1b, $15, $0d, $24, $24, $28, $24
  .db $22, $0c, $47, $24 ; possibly used to clear time up
  .db $23, $dc, $01, $ba ; attribute table data for crown if more than 9 lives
  .db $ff

TwoPlayerTimeUp:
OnePlayerTimeUp:
  .db $22, $0c, $01, $1d

TwoPlayerGameOver:
OnePlayerGameOver:

WarpZoneWelcome:
  .db $25, $84, $15, $20, $0e, $15, $0c, $18, $16 ; "WELCOME TO WARP ZONE!"
  .db $0e, $24, $1d, $18, $24, $20, $0a, $1b, $19
  .db $24, $23, $18, $17, $0e, $2b
  .db $26, $25, $01, $24         ; placeholder for left pipe
  .db $26, $2d, $01, $24         ; placeholder for middle pipe
  .db $26, $35, $01, $24         ; placeholder for right pipe
  .db $27, $d9, $46, $aa         ; attribute data
  .db $27, $e1, $45, $aa
  .db $ff

LuigiName:
WarpZoneNumbers:
  .db $04, $03, $02, $00         ; warp zone numbers, note spaces on middle
  .db $24, $05, $24, $00         ; zone, partly responsible for
  .db $08, $07, $06, $00         ; the minus world

GameTextOffsets:
  .db TopStatusBarLine-GameText
  .db WorldLivesDisplay-GameText
  .db TwoPlayerTimeUp-GameText
  .db TwoPlayerGameOver-GameText
  .db WarpZoneWelcome-GameText

WriteGameText:
               pha                      ;save text number to stack
               tay
               cpy #$02                 ;if set to do top status bar or world/lives display,
               bcc LdGameText           ;branch to use current offset as-is
               cpy #$04                 ;if set to do time-up or game over,
               bcc LdGameText           ;branch to check players
               ldy #$04                 ;otherwise warp zone, therefore set offset
LdGameText:    ldx GameTextOffsets,y    ;get offset to message we want to print
               ldy #$00
GameTextLoop:  lda GameText,x           ;load message data
               cmp #$ff                 ;check for terminator
               beq EndGameText          ;branch to end text if found
               cmp #$fe
               bne WriteTextByte
               ;
               ; Are we loading state?
               ;
               lda SaveStateFlags
               and #$40
               beq UseCurrentTimer
               lda SavedEnterTimer
               jmp WriteTextByte
UseCurrentTimer:
               lda IntervalTimerControl
               sbc #1
               bpl SetEnterTimer
               lda #$14
SetEnterTimer:
               sta $0
               lda SaveStateFlags
               and #$80
               bne TimerSavedAlready
               lda $0
               sta SavedEnterTimer
TimerSavedAlready:
               lda $0
WriteTextByte:
               sta VRAM_Buffer1,y       ;otherwise write data to buffer
               inx
               iny
               bne GameTextLoop         ;do this for 256 bytes if no terminator found
EndGameText:   lda #$00                 ;put null terminator at end
               sta VRAM_Buffer1,y
               pla                      ;pull original text number from stack
               tax
               cmp #$04                 ;are we printing warp zone?
               bcs PrintWarpZoneNumbers
               dex                      ;are we printing the world/lives display?
               bne WriteTextDone      ;if not, branch to check player's name
               lda #$ce
PutLives:      sta VRAM_Buffer1+7
               ldy WorldNumber          ;write world and level numbers (incremented for display)
               iny                      ;to the buffer in the spaces surrounding the dash
               sty VRAM_Buffer1+19
               ldy LevelNumber
               iny
               sty VRAM_Buffer1+21      ;we're done here
WriteTextDone:
               rts

PrintWarpZoneNumbers:
             sbc #$04               ;subtract 4 and then shift to the left
             asl                    ;twice to get proper warp zone number
             asl                    ;offset
             tax
             ldy #$00
WarpNumLoop: lda WarpZoneNumbers,x  ;print warp zone numbers into the
             sta VRAM_Buffer1+27,y  ;placeholders from earlier
             inx
             iny                    ;put a number in every fourth space
             iny
             iny
             iny
             cpy #$0c
             bcc WarpNumLoop
             lda #$2c               ;load new buffer pointer at end of message
             jmp SetVRAMOffset

;-------------------------------------------------------------------------------------

ResetSpritesAndScreenTimer:
         lda ScreenTimer             ;check if screen timer has expired
         bne NoReset                 ;if not, branch to leave
         jsr MoveAllSpritesOffscreen ;otherwise reset sprites now

ResetScreenTimer:
         lda #$07                    ;reset timer again
         sta ScreenTimer
         inc ScreenRoutineTask       ;move onto next task
NoReset: rts

;-------------------------------------------------------------------------------------
;$00 - temp vram buffer offset
;$01 - temp metatile buffer offset
;$02 - temp metatile graphics table offset
;$03 - used to store attribute bits
;$04 - used to determine attribute table row
;$05 - used to determine attribute table column
;$06 - metatile graphics table address low
;$07 - metatile graphics table address high

RenderAreaGraphics:
            lda CurrentColumnPos         ;store LSB of where we're at
            and #$01
            sta $05
            ldy VRAM_Buffer2_Offset      ;store vram buffer offset
            sty $00
            lda CurrentNTAddr_Low        ;get current name table address we're supposed to render
            sta VRAM_Buffer2+1,y
            lda CurrentNTAddr_High
            sta VRAM_Buffer2,y
            lda #$9a                     ;store length byte of 26 here with d7 set
            sta VRAM_Buffer2+2,y         ;to increment by 32 (in columns)
            lda #$00                     ;init attribute row
            sta $04
            tax
DrawMTLoop: stx $01                      ;store init value of 0 or incremented offset for buffer
            lda MetatileBuffer,x         ;get first metatile number, and mask out all but 2 MSB
            and #%11000000
            sta $03                      ;store attribute table bits here
            asl                          ;note that metatile format is:
            rol                          ;%xx000000 - attribute table bits, 
            rol                          ;%00xxxxxx - metatile number
            tay                          ;rotate bits to d1-d0 and use as offset here
            lda MetatileGraphics_Low,y   ;get address to graphics table from here
            sta $06
            lda MetatileGraphics_High,y
            sta $07
            lda MetatileBuffer,x         ;get metatile number again
            asl                          ;multiply by 4 and use as tile offset
            asl
            sta $02
            lda AreaParserTaskNum        ;get current task number for level processing and
            and #%00000001               ;mask out all but LSB, then invert LSB, multiply by 2
            eor #%00000001               ;to get the correct column position in the metatile,
            asl                          ;then add to the tile offset so we can draw either side
            adc $02                      ;of the metatiles
            tay
            ldx $00                      ;use vram buffer offset from before as X
            lda ($06),y
            sta VRAM_Buffer2+3,x         ;get first tile number (top left or top right) and store
            iny
            lda ($06),y                  ;now get the second (bottom left or bottom right) and store
            sta VRAM_Buffer2+4,x
            ldy $04                      ;get current attribute row
            lda $05                      ;get LSB of current column where we're at, and
            bne RightCheck               ;branch if set (clear = left attrib, set = right)
            lda $01                      ;get current row we're rendering
            lsr                          ;branch if LSB set (clear = top left, set = bottom left)
            bcs LLeft
            rol $03                      ;rotate attribute bits 3 to the left
            rol $03                      ;thus in d1-d0, for upper left square
            rol $03
            jmp SetAttrib
RightCheck: lda $01                      ;get LSB of current row we're rendering
            lsr                          ;branch if set (clear = top right, set = bottom right)
            bcs NextMTRow
            lsr $03                      ;shift attribute bits 4 to the right
            lsr $03                      ;thus in d3-d2, for upper right square
            lsr $03
            lsr $03
            jmp SetAttrib
LLeft:      lsr $03                      ;shift attribute bits 2 to the right
            lsr $03                      ;thus in d5-d4 for lower left square
NextMTRow:  inc $04                      ;move onto next attribute row  
SetAttrib:  lda AttributeBuffer,y        ;get previously saved bits from before
            ora $03                      ;if any, and put new bits, if any, onto
            sta AttributeBuffer,y        ;the old, and store
            inc $00                      ;increment vram buffer offset by 2
            inc $00
            ldx $01                      ;get current gfx buffer row, and check for
            inx                          ;the bottom of the screen
            cpx #$0d
            bcc DrawMTLoop               ;if not there yet, loop back
            ldy $00                      ;get current vram buffer offset, increment by 3
            iny                          ;(for name table address and length bytes)
            iny
            iny
            lda #$00
            sta VRAM_Buffer2,y           ;put null terminator at end of data for name table
            sty VRAM_Buffer2_Offset      ;store new buffer offset
            inc CurrentNTAddr_Low        ;increment name table address low
            lda CurrentNTAddr_Low        ;check current low byte
            and #%00011111               ;if no wraparound, just skip this part
            bne ExitDrawM
            lda #$80                     ;if wraparound occurs, make sure low byte stays
            sta CurrentNTAddr_Low        ;just under the status bar
            lda CurrentNTAddr_High       ;and then invert d2 of the name table address high
            eor #%00000100               ;to move onto the next appropriate name table
            sta CurrentNTAddr_High
ExitDrawM:  jmp SetVRAMCtrl              ;jump to set buffer to $0341 and leave

;-------------------------------------------------------------------------------------
;$00 - temp attribute table address high (big endian order this time!)
;$01 - temp attribute table address low

RenderAttributeTables:
             lda CurrentNTAddr_Low    ;get low byte of next name table address
             and #%00011111           ;to be written to, mask out all but 5 LSB,
             sec                      ;subtract four 
             sbc #$04
             and #%00011111           ;mask out bits again and store
             sta $01
             lda CurrentNTAddr_High   ;get high byte and branch if borrow not set
             bcs SetATHigh
             eor #%00000100           ;otherwise invert d2
SetATHigh:   and #%00000100           ;mask out all other bits
             ora #$23                 ;add $2300 to the high byte and store
             sta $00
             lda $01                  ;get low byte - 4, divide by 4, add offset for
             lsr                      ;attribute table and store
             lsr
             adc #$c0                 ;we should now have the appropriate block of
             sta $01                  ;attribute table in our temp address
             ldx #$00
             ldy VRAM_Buffer2_Offset  ;get buffer offset
AttribLoop:  lda $00
             sta VRAM_Buffer2,y       ;store high byte of attribute table address
             lda $01
             clc                      ;get low byte, add 8 because we want to start
             adc #$08                 ;below the status bar, and store
             sta VRAM_Buffer2+1,y
             sta $01                  ;also store in temp again
             lda AttributeBuffer,x    ;fetch current attribute table byte and store
             sta VRAM_Buffer2+3,y     ;in the buffer
             lda #$01
             sta VRAM_Buffer2+2,y     ;store length of 1 in buffer
             lsr
             sta AttributeBuffer,x    ;clear current byte in attribute buffer
             iny                      ;increment buffer offset by 4 bytes
             iny
             iny
             iny
             inx                      ;increment attribute offset and check to see
             cpx #$07                 ;if we're at the end yet
             bcc AttribLoop
             sta VRAM_Buffer2,y       ;put null terminator at the end
             sty VRAM_Buffer2_Offset  ;store offset in case we want to do any more
SetVRAMCtrl: lda #$06
             sta VRAM_Buffer_AddrCtrl ;set buffer to $0341 and leave
             rts

;-------------------------------------------------------------------------------------

;$00 - used as temporary counter in ColorRotation

ColorRotatePalette:
       .db $27, $27, $27, $17, $07, $17

BlankPalette:
       .db $3f, $0c, $04, $ff, $ff, $ff, $ff, $00

;used based on area type
Palette3Data:
       .db $0f, $07, $12, $0f 
       .db $0f, $07, $17, $0f
       .db $0f, $07, $17, $1c
       .db $0f, $07, $17, $00

ColorRotation:
              lda FrameCounter         ;get frame counter
              and #$07                 ;mask out all but three LSB
              bne ExitColorRot         ;branch if not set to zero to do this every eighth frame
              ldx VRAM_Buffer1_Offset  ;check vram buffer offset
              cpx #$31
              bcs ExitColorRot         ;if offset over 48 bytes, branch to leave
              tay                      ;otherwise use frame counter's 3 LSB as offset here
GetBlankPal:  lda BlankPalette,y       ;get blank palette for palette 3
              sta VRAM_Buffer1,x       ;store it in the vram buffer
              inx                      ;increment offsets
              iny
              cpy #$08
              bcc GetBlankPal          ;do this until all bytes are copied
              ldx VRAM_Buffer1_Offset  ;get current vram buffer offset
              lda #$03
              sta $00                  ;set counter here
              lda AreaType             ;get area type
              asl                      ;multiply by 4 to get proper offset
              asl
              tay                      ;save as offset here
GetAreaPal:   lda Palette3Data,y       ;fetch palette to be written based on area type
              sta VRAM_Buffer1+3,x     ;store it to overwrite blank palette in vram buffer
              iny
              inx
              dec $00                  ;decrement counter
              bpl GetAreaPal           ;do this until the palette is all copied
              ldx VRAM_Buffer1_Offset  ;get current vram buffer offset
              ldy ColorRotateOffset    ;get color cycling offset
              lda ColorRotatePalette,y
              sta VRAM_Buffer1+4,x     ;get and store current color in second slot of palette
              lda VRAM_Buffer1_Offset
              clc                      ;add seven bytes to vram buffer offset
              adc #$07
              sta VRAM_Buffer1_Offset
              inc ColorRotateOffset    ;increment color cycling offset
              lda ColorRotateOffset
              cmp #$06                 ;check to see if it's still in range
              bcc ExitColorRot         ;if so, branch to leave
              lda #$00
              sta ColorRotateOffset    ;otherwise, init to keep it in range
ExitColorRot: rts                      ;leave

;-------------------------------------------------------------------------------------
;METATILE GRAPHICS TABLE

MetatileGraphics_Low:
  .db <Palette0_MTiles, <Palette1_MTiles, <Palette2_MTiles, <Palette3_MTiles

MetatileGraphics_High:
  .db >Palette0_MTiles, >Palette1_MTiles, >Palette2_MTiles, >Palette3_MTiles

Palette0_MTiles:
  .db $24, $24, $24, $24 ;blank
  .db $27, $27, $27, $27 ;black metatile
  .db $24, $24, $24, $35 ;bush left
  .db $36, $25, $37, $25 ;bush middle
  .db $24, $38, $24, $24 ;bush right
  .db $24, $30, $30, $26 ;mountain left
  .db $26, $26, $34, $26 ;mountain left bottom/middle center
  .db $24, $31, $24, $32 ;mountain middle top
  .db $33, $26, $24, $33 ;mountain right
  .db $34, $26, $26, $26 ;mountain right bottom
  .db $26, $26, $26, $26 ;mountain middle bottom
  .db $24, $c0, $24, $c0 ;bridge guardrail
  .db $24, $7f, $7f, $24 ;chain
  .db $b8, $ba, $b9, $bb ;tall tree top, top half
  .db $b8, $bc, $b9, $bd ;short tree top
  .db $ba, $bc, $bb, $bd ;tall tree top, bottom half
  .db $60, $64, $61, $65 ;warp pipe end left, points up
  .db $62, $66, $63, $67 ;warp pipe end right, points up
  .db $60, $64, $61, $65 ;decoration pipe end left, points up
  .db $62, $66, $63, $67 ;decoration pipe end right, points up
  .db $68, $68, $69, $69 ;pipe shaft left
  .db $26, $26, $6a, $6a ;pipe shaft right
  .db $4b, $4c, $4d, $4e ;tree ledge left edge
  .db $4d, $4f, $4d, $4f ;tree ledge middle
  .db $4d, $4e, $50, $51 ;tree ledge right edge
  .db $6b, $70, $2c, $2d ;mushroom left edge
  .db $6c, $71, $6d, $72 ;mushroom middle
  .db $6e, $73, $6f, $74 ;mushroom right edge
  .db $86, $8a, $87, $8b ;sideways pipe end top
  .db $88, $8c, $88, $8c ;sideways pipe shaft top
  .db $89, $8d, $69, $69 ;sideways pipe joint top
  .db $8e, $91, $8f, $92 ;sideways pipe end bottom
  .db $26, $93, $26, $93 ;sideways pipe shaft bottom
  .db $90, $94, $69, $69 ;sideways pipe joint bottom
  .db $a4, $e9, $ea, $eb ;seaplant
  .db $24, $24, $24, $24 ;blank, used on bricks or blocks that are hit
  .db $24, $2f, $24, $3d ;flagpole ball
  .db $a2, $a2, $a3, $a3 ;flagpole shaft
  .db $24, $24, $24, $24 ;blank, used in conjunction with vines

Palette1_MTiles:
  .db $a2, $a2, $a3, $a3 ;vertical rope
  .db $99, $24, $99, $24 ;horizontal rope
  .db $24, $a2, $3e, $3f ;left pulley
  .db $5b, $5c, $24, $a3 ;right pulley
  .db $24, $24, $24, $24 ;blank used for balance rope
  .db $9d, $47, $9e, $47 ;castle top
  .db $47, $47, $27, $27 ;castle window left
  .db $47, $47, $47, $47 ;castle brick wall
  .db $27, $27, $47, $47 ;castle window right
  .db $a9, $47, $aa, $47 ;castle top w/ brick
  .db $9b, $27, $9c, $27 ;entrance top
  .db $27, $27, $27, $27 ;entrance bottom
  .db $52, $52, $52, $52 ;green ledge stump
  .db $80, $a0, $81, $a1 ;fence
  .db $be, $be, $bf, $bf ;tree trunk
  .db $75, $ba, $76, $bb ;mushroom stump top
  .db $ba, $ba, $bb, $bb ;mushroom stump bottom
  .db $45, $47, $45, $47 ;breakable brick w/ line 
  .db $47, $47, $47, $47 ;breakable brick 
  .db $45, $47, $45, $47 ;breakable brick (not used)
  .db $b4, $b6, $b5, $b7 ;cracked rock terrain
  .db $45, $47, $45, $47 ;brick with line (power-up)
  .db $45, $47, $45, $47 ;brick with line (vine)
  .db $45, $47, $45, $47 ;brick with line (star)
  .db $45, $47, $45, $47 ;brick with line (coins)
  .db $45, $47, $45, $47 ;brick with line (1-up)
  .db $47, $47, $47, $47 ;brick (power-up)
  .db $47, $47, $47, $47 ;brick (vine)
  .db $47, $47, $47, $47 ;brick (star)
  .db $47, $47, $47, $47 ;brick (coins)
  .db $47, $47, $47, $47 ;brick (1-up)
  .db $24, $24, $24, $24 ;hidden block (1 coin)
  .db $24, $24, $24, $24 ;hidden block (1-up)
  .db $ab, $ac, $ad, $ae ;solid block (3-d block)
  .db $5d, $5e, $5d, $5e ;solid block (white wall)
  .db $c1, $24, $c1, $24 ;bridge
  .db $c6, $c8, $c7, $c9 ;bullet bill cannon barrel
  .db $ca, $cc, $cb, $cd ;bullet bill cannon top
  .db $2a, $2a, $40, $40 ;bullet bill cannon bottom
  .db $24, $24, $24, $24 ;blank used for jumpspring
  .db $24, $47, $24, $47 ;half brick used for jumpspring
  .db $82, $83, $84, $85 ;solid block (water level, green rock)
  .db $24, $47, $24, $47 ;half brick (???)
  .db $86, $8a, $87, $8b ;water pipe top
  .db $8e, $91, $8f, $92 ;water pipe bottom
  .db $24, $2f, $24, $3d ;flag ball (residual object)

Palette2_MTiles:
  .db $24, $24, $24, $35 ;cloud left
  .db $36, $25, $37, $25 ;cloud middle
  .db $24, $38, $24, $24 ;cloud right
  .db $24, $24, $39, $24 ;cloud bottom left
  .db $3a, $24, $3b, $24 ;cloud bottom middle
  .db $3c, $24, $24, $24 ;cloud bottom right
  .db $41, $26, $41, $26 ;water/lava top
  .db $26, $26, $26, $26 ;water/lava
  .db $b0, $b1, $b2, $b3 ;cloud level terrain
  .db $77, $79, $77, $79 ;bowser's bridge
      
Palette3_MTiles:
  .db $53, $55, $54, $56 ;question block (coin)
  .db $53, $55, $54, $56 ;question block (power-up)
  .db $a5, $a7, $a6, $a8 ;coin
  .db $c2, $c4, $c3, $c5 ;underwater coin
  .db $57, $59, $58, $5a ;empty block
  .db $7b, $7d, $7c, $7e ;axe

;-------------------------------------------------------------------------------------
;VRAM BUFFER DATA FOR LOCATIONS IN PRG-ROM

WaterPaletteData:
  .db $3f, $00, $20
  .db $0f, $15, $12, $25  
  .db $0f, $3a, $1a, $0f
  .db $0f, $30, $12, $0f
  .db $0f, $27, $12, $0f
  .db $22, $16, $27, $18
  .db $0f, $10, $30, $27
  .db $0f, $16, $30, $27
  .db $0f, $0f, $30, $10
  .db $00

GroundPaletteData:
  .db $3f, $00, $20
  .db $0f, $29, $1a, $0f
  .db $0f, $36, $17, $0f
  .db $0f, $30, $21, $0f
  .db $0f, $27, $17, $0f
  .db $0f, $16, $27, $18
  .db $0f, $1a, $30, $27
  .db $0f, $16, $30, $27
  .db $0f, $0f, $36, $17
  .db $00

UndergroundPaletteData:
  .db $3f, $00, $20
  .db $0f, $29, $1a, $09
  .db $0f, $3c, $1c, $0f
  .db $0f, $30, $21, $1c
  .db $0f, $27, $17, $1c
  .db $0f, $16, $27, $18
  .db $0f, $1c, $36, $17
  .db $0f, $16, $30, $27
  .db $0f, $0c, $3c, $1c
  .db $00

CastlePaletteData:
  .db $3f, $00, $20
  .db $0f, $30, $10, $00
  .db $0f, $30, $10, $00
  .db $0f, $30, $16, $00
  .db $0f, $27, $17, $00
  .db $0f, $16, $27, $18
  .db $0f, $1c, $36, $17
  .db $0f, $16, $30, $27
  .db $0f, $00, $30, $10
  .db $00

DaySnowPaletteData:
  .db $3f, $00, $04
  .db $22, $30, $00, $10
  .db $00

NightSnowPaletteData:
  .db $3f, $00, $04
  .db $0f, $30, $00, $10
  .db $00

MushroomPaletteData:
  .db $3f, $00, $04
  .db $22, $27, $16, $0f
  .db $00

BowserPaletteData:
  .db $3f, $14, $04
  .db $0f, $1a, $30, $27
  .db $00

MarioThanksMessage:
;"THANK YOU MARIO!"
  .db $25, $48, $10
  .db $1d, $11, $0a, $17, $14, $24
  .db $22, $18, $1e, $24
  .db $16, $0a, $1b, $12, $18, $2b
  .db $00

LuigiThanksMessage:
;"THANK YOU LUIGI!"
  .db $25, $48, $10
  .db $1d, $11, $0a, $17, $14, $24
  .db $22, $18, $1e, $24
  .db $15, $1e, $12, $10, $12, $2b
  .db $00

MushroomRetainerSaved:
;"BUT OUR PRINCESS IS IN"
  .db $25, $c5, $16
  .db $0b, $1e, $1d, $24, $18, $1e, $1b, $24
  .db $19, $1b, $12, $17, $0c, $0e, $1c, $1c, $24
  .db $12, $1c, $24, $12, $17
;"ANOTHER CASTLE!"
  .db $26, $05, $0f
  .db $0a, $17, $18, $1d, $11, $0e, $1b, $24
  .db $0c, $0a, $1c, $1d, $15, $0e, $2b, $00

PrincessSaved1:
;"YOUR QUEST IS OVER."
  .db $25, $a7, $13
  .db $22, $18, $1e, $1b, $24
  .db $1a, $1e, $0e, $1c, $1d, $24
  .db $12, $1c, $24, $18, $1f, $0e, $1b, $af
  .db $00

PrincessSaved2:
;"WE PRESENT YOU A NEW QUEST."
  .db $25, $e3, $1b
  .db $20, $0e, $24
  .db $19, $1b, $0e, $1c, $0e, $17, $1d, $24
  .db $22, $18, $1e, $24, $0a, $24, $17, $0e, $20, $24
  .db $1a, $1e, $0e, $1c, $1d, $af
  .db $00

WorldSelectMessage1:
;"PUSH BUTTON B"
  .db $26, $4a, $0d
  .db $19, $1e, $1c, $11, $24
  .db $0b, $1e, $1d, $1d, $18, $17, $24, $0b
  .db $00

WorldSelectMessage2:
;"TO SELECT A WORLD"
  .db $26, $88, $11
  .db $1d, $18, $24, $1c, $0e, $15, $0e, $0c, $1d, $24
  .db $0a, $24, $20, $18, $1b, $15, $0d
  .db $00

;-------------------------------------------------------------------------------------
;$00 - used to store status bar nybbles
;$02 - used as temp vram offset
;$03 - used to store length of status bar number

;status bar name table offset and length data
StatusBarData:
      .db $cb, $04 ; top score display on title screen
      .db $64, $04 ; player score
      .db $64, $06
      .db $6d, $03 ; coin tally
      .db $6d, $03
      .db $7a, $03 ; game timer
      .db $75, $03 ; POS
      .db $72, $02 ; LEFT

StatusBarOffset:
      .db $06, $0c, $12, FRAME_NUMBER_OFFSET+1, $1e, $24
      .db POSITION_OFFSET+1, FRAMES_REMAIN_OFFSET+1

PrintStatusBarNumbers:
      sta $00            ;store player-specific offset
      jsr OutputNumbers  ;use first nybble to print the coin display
      lda $00            ;move high nybble to low
      lsr                ;and print to score display
      lsr
      lsr
      lsr

OutputNumbers:
             clc                      ;add 1 to low nybble
             adc #$01
             and #%00001111           ;mask out high nybble
             cmp #$08
             bcs ExitOutputN
             pha                      ;save incremented value to stack for now and
             asl                      ;shift to left and use as offset
             tay
             ldx VRAM_Buffer1_Offset  ;get current buffer pointer
             lda #$20                 ;put at top of screen by default
             cpy #$00                 ;are we writing top score on title screen?
             bne SetupNums
             lda #$22                 ;if so, put further down on the screen
SetupNums:   sta VRAM_Buffer1,x
             lda StatusBarData,y      ;write low vram address and length of thing
             sta VRAM_Buffer1+1,x     ;we're printing to the buffer
             lda StatusBarData+1,y
             sta VRAM_Buffer1+2,x
             sta $03                  ;save length byte in counter
             stx $02                  ;and buffer pointer elsewhere for now
             pla                      ;pull original incremented value from stack
             tax
             lda StatusBarOffset,x    ;load offset to value we want to write
             sec
             sbc StatusBarData+1,y    ;subtract from length byte we read before
             tay                      ;use value as offset to display digits
             ldx $02
DigitPLoop:  lda DisplayDigits,y      ;write digits to the buffer
             sta VRAM_Buffer1+3,x    
             inx
             iny
             dec $03                  ;do this until all the digits are written
             bne DigitPLoop
             lda #$00                 ;put null terminator at end
             sta VRAM_Buffer1+3,x
             inx                      ;increment buffer pointer by 3
             inx
             inx
             stx VRAM_Buffer1_Offset  ;store it in case we want to use it again
ExitOutputN: rts

;-------------------------------------------------------------------------------------

DigitsMathRoutine3:
            ldx #$05
AddModLoop3:
            lda DigitModifier,x       ;load digit amount to increment
            clc
            adc DisplayDigits,y       ;add to current digit
            bmi BorrowOne3             ;if result is a negative number, branch to subtract
            cmp #10
            bcs CarryOne3              ;if digit greater than $09, branch to add
StoreNewD3:
            sta DisplayDigits,y       ;store as new score or game timer digit
            dey                       ;move onto next digits in score or game timer
            dex                       ;and digit amounts to increment
            cpx #3
            bpl AddModLoop3            ;loop back if we're not done yet
            lda #$00                  ;store zero here
            ldx #$06                  ;start with the last digit
EraseMLoop3:
            sta DigitModifier-1,x     ;initialize the digit amounts to increment
            dex
            bpl EraseMLoop3            ;do this until they're all reset, then leave
            rts
BorrowOne3:
            dec DigitModifier-1,x     ;decrement the previous digit, then put $09 in
            lda #$09                  ;the game timer digit we're currently on to "borrow
            bne StoreNewD3             ;the one", then do an unconditional branch back
CarryOne3:
            sec                       ;subtract ten from our digit to make it a
            sbc #10                   ;proper BCD number, then increment the digit
            inc DigitModifier-1,x     ;preceding current digit to "carry the one" properly
            jmp StoreNewD3             ;go back to just after we branched here

;-------------------------------------------------------------------------------------

DigitsMathRoutine:
            ldx #$05
AddModLoop: lda DigitModifier,x       ;load digit amount to increment
            clc
            adc DisplayDigits,y       ;add to current digit
            bmi BorrowOne             ;if result is a negative number, branch to subtract
            cmp #10
            bcs CarryOne              ;if digit greater than $09, branch to add
StoreNewD:  sta DisplayDigits,y       ;store as new score or game timer digit
            dey                       ;move onto next digits in score or game timer
            dex                       ;and digit amounts to increment
            bpl AddModLoop            ;loop back if we're not done yet
            lda #$00                  ;store zero here
            ldx #$06                  ;start with the last digit
EraseMLoop: sta DigitModifier-1,x     ;initialize the digit amounts to increment
            dex
            bpl EraseMLoop            ;do this until they're all reset, then leave
            rts
BorrowOne:  dec DigitModifier-1,x     ;decrement the previous digit, then put $09 in
            lda #$09                  ;the game timer digit we're currently on to "borrow
            bne StoreNewD             ;the one", then do an unconditional branch back
CarryOne:   sec                       ;subtract ten from our digit to make it a
            sbc #10                   ;proper BCD number, then increment the digit
            inc DigitModifier-1,x     ;preceding current digit to "carry the one" properly
            jmp StoreNewD             ;go back to just after we branched here

;-------------------------------------------------------------------------------------

UpdateTopScore:
      ldx #$05          ;start with mario's score
      jsr TopScoreCheck
      ldx #$0b          ;now do luigi's score

TopScoreCheck:
              ldy #$05                 ;start with the lowest digit
              sec           
GetScoreDiff: lda PlayerScoreDisplay,x ;subtract each player digit from each high score digit
              sbc TopScoreDisplay,y    ;from lowest to highest, if any top score digit exceeds
              dex                      ;any player digit, borrow will be set until a subsequent
              dey                      ;subtraction clears it (player digit is higher than top)
              bpl GetScoreDiff      
              bcc NoTopSc              ;check to see if borrow is still set, if so, no new high score
              inx                      ;increment X and Y once to the start of the score
              iny
CopyScore:    lda PlayerScoreDisplay,x ;store player's score digits into high score memory area
              sta TopScoreDisplay,y
              inx
              iny
              cpy #$06                 ;do this until we have stored them all
              bcc CopyScore
NoTopSc:      rts

;-------------------------------------------------------------------------------------

DefaultSprOffsets:
      .db $04, $30, $48, $60, $78, $90, $a8, $c0
      .db $d8, $e8, $24, $f8, $fc, $28, $2c

Sprite0Data:
      .db $18, $ff, $23, $58

;-------------------------------------------------------------------------------------

PrimaryGameSetup:
      lda #$01
      sta FetchNewGameTimerFlag   ;set flag to load game timer from header
      sta PlayerSize              ;set player's size to small

SecondaryGameSetup:
			jsr EnterAdvanceToRule
			jsr HandleSaveState
             lda #$00
             sta DisableScreenFlag     ;enable screen output
             tay
ClearVRLoop: sta VRAM_Buffer1-1,y      ;clear buffer at $0300-$03ff
             iny
             bne ClearVRLoop
             sta GameTimerExpiredFlag  ;clear game timer exp flag
             sta DisableIntermediate   ;clear skip lives display flag
             sta BackloadingFlag       ;clear value here
             lda #$ff
             sta BalPlatformAlignment  ;initialize balance platform assignment flag
             lda ScreenLeft_PageLoc    ;get left side page location
             lsr Mirror_PPU_CTRL_REG1  ;shift LSB of ppu register #1 mirror out
             and #$01                  ;mask out all but LSB of page location
             ror                       ;rotate LSB of page location into carry then onto mirror
             rol Mirror_PPU_CTRL_REG1  ;this is to set the proper PPU name table
             jsr GetAreaMusic          ;load proper music into queue
             lda #$38                  ;load sprite shuffle amounts to be used later
             sta SprShuffleAmt+2
             lda #$48
             sta SprShuffleAmt+1
             lda #$58
             sta SprShuffleAmt
             ldx #$0e                  ;load default OAM offsets into $06e4-$06f2
ShufAmtLoop: lda DefaultSprOffsets,x
             sta SprDataOffset,x
             dex                       ;do this until they're all set
             bpl ShufAmtLoop
             ldy #$03                  ;set up sprite #0
ISpr0Loop:   lda Sprite0Data,y
             sta Sprite_Data,y
             dey
             bpl ISpr0Loop
             jsr DoNothing2            ;these jsrs doesn't do anything useful
             jsr DoNothing1
             inc Sprite0HitDetectFlag  ;set sprite #0 check flag
             inc OperMode_Task         ;increment to next task
             rts

;-------------------------------------------------------------------------------------

MusicSelectData:
      .db WaterMusic, GroundMusic, UndergroundMusic, CastleMusic
      .db CloudMusic, PipeIntroMusic

GetAreaMusic:
             lda OperMode           ;if in title screen mode, leave
             beq ExitGetM
             lda AltEntranceControl ;check for specific alternate mode of entry
             cmp #$02               ;if found, branch without checking starting position
             beq ChkAreaType        ;from area object data header
             ldy #$05               ;select music for pipe intro scene by default
             lda PlayerEntranceCtrl ;check value from level header for certain values
             cmp #$06
             beq StoreMusic         ;load music for pipe intro scene if header
             cmp #$07               ;start position either value $06 or $07
             beq StoreMusic
ChkAreaType: ldy AreaType           ;load area type as offset for music bit
             lda CloudTypeOverride
             beq StoreMusic         ;check for cloud type override
             ldy #$04               ;select music for cloud type level if found
StoreMusic:  lda MusicSelectData,y  ;otherwise select appropriate music for level type
             sta AreaMusicQueue     ;store in queue and leave
ExitGetM:    rts

;-------------------------------------------------------------------------------------

PlayerStarting_X_Pos:
      .db $28, $18
      .db $38, $28

AltYPosOffset:
      .db $08, $00

PlayerStarting_Y_Pos:
      .db $00, $20, $b0, $50, $00, $00, $b0, $b0
      .db $f0

PlayerBGPriorityData:
      .db $00, $20, $00, $00, $00, $00, $00, $00

GameTimerData:
      .db $20 ;dummy byte, used as part of bg priority data
      .db $04, $03, $02

Entrance_GameTimerSetup:
          lda ScreenLeft_PageLoc      ;set current page for area objects
          sta Player_PageLoc          ;as page location for player
          lda #$28                    ;store value here
          sta VerticalForceDown       ;for fractional movement downwards if necessary
          lda #$01                    ;set high byte of player position and
          sta PlayerFacingDir         ;set facing direction so that player faces right
          sta Player_Y_HighPos
          lda #$00                    ;set player state to on the ground by default
          sta Player_State
          dec Player_CollisionBits    ;initialize player's collision bits
          ldy #$00                    ;initialize halfway page
          sty HalfwayPage      
          lda AreaType                ;check area type
          bne ChkStPos                ;if water type, set swimming flag, otherwise do not set
          iny
ChkStPos: sty SwimmingFlag
          ldx PlayerEntranceCtrl      ;get starting position loaded from header
          ldy AltEntranceControl      ;check alternate mode of entry flag for 0 or 1
          beq SetStPos
          cpy #$01
          beq SetStPos
          ldx AltYPosOffset-2,y       ;if not 0 or 1, override $0710 with new offset in X
SetStPos: lda PlayerStarting_X_Pos,y  ;load appropriate horizontal position
          sta Player_X_Position       ;and vertical positions for the player, using
          lda PlayerStarting_Y_Pos,x  ;AltEntranceControl as offset for horizontal and either $0710
          sta Player_Y_Position       ;or value that overwrote $0710 as offset for vertical
          lda PlayerBGPriorityData,x
          sta Player_SprAttrib        ;set player sprite attributes using offset in X
          jsr GetPlayerColors         ;get appropriate player palette
          ldy GameTimerSetting        ;get timer control value from header
          beq ChkOverR                ;if set to zero, branch (do not use dummy byte for this)
          lda FetchNewGameTimerFlag   ;do we need to set the game timer? if not, use 
          beq ChkOverR                ;old game timer setting
          lda GameTimerData,y         ;if game timer is set and game timer flag is also set,
          sta GameTimerDisplay        ;use value of game timer control for first digit of game timer
          lda #$01
          sta GameTimerDisplay+2      ;set last digit of game timer to 1
          lsr
          sta GameTimerDisplay+1      ;set second digit of game timer
          sta FetchNewGameTimerFlag   ;clear flag for game timer reset
          sta StarInvincibleTimer     ;clear star mario timer
ChkOverR: ldy JoypadOverride          ;if controller bits not set, branch to skip this part
          beq ChkSwimE
          lda #$03                    ;set player state to climbing
          sta Player_State
          ldx #$00                    ;set offset for first slot, for block object
          jsr InitBlock_XY_Pos
          lda #$f0                    ;set vertical coordinate for block object
          sta Block_Y_Position
          ldx #$05                    ;set offset in X for last enemy object buffer slot
          ldy #$00                    ;set offset in Y for object coordinates used earlier
          jsr Setup_Vine              ;do a sub to grow vine
ChkSwimE: ldy AreaType                ;if level not water-type,
          bne SetPESub                ;skip this subroutine
          jsr SetupBubble             ;otherwise, execute sub to set up air bubbles
SetPESub: lda #$07                    ;set to run player entrance subroutine
          sta GameEngineSubroutine    ;on the next frame of game engine
          jsr RedrawFrameNumbers
          rts

;-------------------------------------------------------------------------------------

;page numbers are in order from -1 to -4
HalfwayPageNybbles:
      .db $56, $40
      .db $65, $70
      .db $66, $40
      .db $66, $40
      .db $66, $40
      .db $66, $60
      .db $65, $70
      .db $00, $00

PlayerLoseLife:
             inc DisableScreenFlag    ;disable screen and sprite 0 check
             lda #$00
             sta Sprite0HitDetectFlag
             lda #Silence             ;silence music
             sta EventMusicQueue
StillInGame: lda WorldNumber          ;multiply world number by 2 and use
             asl                      ;as offset
             tax
             lda LevelNumber          ;if in area -3 or -4, increment
             and #$02                 ;offset by one byte, otherwise
             beq GetHalfway           ;leave offset alone
             inx
GetHalfway:  ldy HalfwayPageNybbles,x ;get halfway page number with offset
             lda LevelNumber          ;check area number's LSB
             lsr
             tya                      ;if in area -2 or -4, use lower nybble
             bcs MaskHPNyb
             lsr                      ;move higher nybble to lower if area
             lsr                      ;number is -1 or -3
             lsr
             lsr
MaskHPNyb:   and #%00001111           ;mask out all but lower nybble
             cmp ScreenLeft_PageLoc
             beq SetHalfway           ;left side of screen must be at the halfway page,
             bcc SetHalfway           ;otherwise player must start at the
             lda #$00                 ;beginning of the level
SetHalfway:  sta HalfwayPage          ;store as halfway page for player
			;
			; TODO XXX What does this do? :)
			;
			jsr LoadAreaPointer       ;update level pointer with
			lda #$01                  ;actual world and area numbers, then
			sta PlayerSize            ;reset player's size, status, and
			inc FetchNewGameTimerFlag ;set game timer flag to reload
			lda #$00                  ;game timer from header
			sta TimerControl          ;also set flag for timers to count again
			sta PlayerStatus
			sta GameEngineSubroutine  ;reset task for game core
			sta OperMode_Task         ;set modes and leave
			lda #$01                  ;if in game over mode, switch back to
			sta OperMode   
			rts
			;
			; Old code
			;
			;jmp ContinueGame         ;continue the game

;-------------------------------------------------------------------------------------

SetupGameOver:
      lda #$00                  ;reset screen routine task control for title screen, game,
      sta ScreenRoutineTask     ;and game over modes
      sta Sprite0HitDetectFlag  ;disable sprite 0 check
      lda #GameOverMusic
      sta EventMusicQueue       ;put game over music in secondary queue
      inc DisableScreenFlag     ;disable screen output
      inc OperMode_Task         ;set secondary mode to 1
      rts

;-------------------------------------------------------------------------------------

ContinueGame:
           jsr LoadAreaPointer       ;update level pointer with
           lda #$01                  ;actual world and area numbers, then
           sta PlayerSize            ;reset player's size, status, and
           inc FetchNewGameTimerFlag ;set game timer flag to reload
           lda #$00                  ;game timer from header
           sta TimerControl          ;also set flag for timers to count again
           sta PlayerStatus
           sta GameEngineSubroutine  ;reset task for game core
           sta OperMode_Task         ;set modes and leave
           lda #$01                  ;if in game over mode, switch back to
           sta OperMode              ;game mode, because game is still on
GameIsOn:  rts

;-------------------------------------------------------------------------------------

DoNothing1:
      lda #$ff       ;this is residual code, this value is
      sta $06c9      ;not used anywhere in the program
DoNothing2:
      rts

;-------------------------------------------------------------------------------------

AreaParserTaskHandler:
              ldy AreaParserTaskNum     ;check number of tasks here
              bne DoAPTasks             ;if already set, go ahead
              ldy #$08
              sty AreaParserTaskNum     ;otherwise, set eight by default
DoAPTasks:    dey
              tya
              jsr AreaParserTasks
              dec AreaParserTaskNum     ;if all tasks not complete do not
              bne SkipATRender          ;render attribute table yet
              jsr RenderAttributeTables
SkipATRender: rts

AreaParserTasks:
      jsr JumpEngine

      .dw IncrementColumnPos
      .dw RenderAreaGraphics
      .dw RenderAreaGraphics
      .dw AreaParserCore
      .dw IncrementColumnPos
      .dw RenderAreaGraphics
      .dw RenderAreaGraphics
      .dw AreaParserCore

;-------------------------------------------------------------------------------------

IncrementColumnPos:
           inc CurrentColumnPos     ;increment column where we're at
           lda CurrentColumnPos
           and #%00001111           ;mask out higher nybble
           bne NoColWrap
           sta CurrentColumnPos     ;if no bits left set, wrap back to zero (0-f)
           inc CurrentPageLoc       ;and increment page number where we're at
NoColWrap: inc BlockBufferColumnPos ;increment column offset where we're at
           lda BlockBufferColumnPos
           and #%00011111           ;mask out all but 5 LSB (0-1f)
           sta BlockBufferColumnPos ;and save
           rts

;-------------------------------------------------------------------------------------
;$00 - used as counter, store for low nybble for background, ceiling byte for terrain
;$01 - used to store floor byte for terrain
;$07 - used to store terrain metatile
;$06-$07 - used to store block buffer address

BSceneDataOffsets:
      .db $00, $30, $60 

BackSceneryData:
   .db $93, $00, $00, $11, $12, $12, $13, $00 ;clouds
   .db $00, $51, $52, $53, $00, $00, $00, $00
   .db $00, $00, $01, $02, $02, $03, $00, $00
   .db $00, $00, $00, $00, $91, $92, $93, $00
   .db $00, $00, $00, $51, $52, $53, $41, $42
   .db $43, $00, $00, $00, $00, $00, $91, $92

   .db $97, $87, $88, $89, $99, $00, $00, $00 ;mountains and bushes
   .db $11, $12, $13, $a4, $a5, $a5, $a5, $a6
   .db $97, $98, $99, $01, $02, $03, $00, $a4
   .db $a5, $a6, $00, $11, $12, $12, $12, $13
   .db $00, $00, $00, $00, $01, $02, $02, $03
   .db $00, $a4, $a5, $a5, $a6, $00, $00, $00

   .db $11, $12, $12, $13, $00, $00, $00, $00 ;trees and fences
   .db $00, $00, $00, $9c, $00, $8b, $aa, $aa
   .db $aa, $aa, $11, $12, $13, $8b, $00, $9c
   .db $9c, $00, $00, $01, $02, $03, $11, $12
   .db $12, $13, $00, $00, $00, $00, $aa, $aa
   .db $9c, $aa, $00, $8b, $00, $01, $02, $03

BackSceneryMetatiles:
   .db $80, $83, $00 ;cloud left
   .db $81, $84, $00 ;cloud middle
   .db $82, $85, $00 ;cloud right
   .db $02, $00, $00 ;bush left
   .db $03, $00, $00 ;bush middle
   .db $04, $00, $00 ;bush right
   .db $00, $05, $06 ;mountain left
   .db $07, $06, $0a ;mountain middle
   .db $00, $08, $09 ;mountain right
   .db $4d, $00, $00 ;fence
   .db $0d, $0f, $4e ;tall tree
   .db $0e, $4e, $4e ;short tree

FSceneDataOffsets:
      .db $00, $0d, $1a

ForeSceneryData:
   .db $86, $87, $87, $87, $87, $87, $87   ;in water
   .db $87, $87, $87, $87, $69, $69

   .db $00, $00, $00, $00, $00, $45, $47   ;wall
   .db $47, $47, $47, $47, $00, $00

   .db $00, $00, $00, $00, $00, $00, $00   ;over water
   .db $00, $00, $00, $00, $86, $87

TerrainMetatiles:
      .db $69, $54, $52, $62

TerrainRenderBits:
      .db %00000000, %00000000 ;no ceiling or floor
      .db %00000000, %00011000 ;no ceiling, floor 2
      .db %00000001, %00011000 ;ceiling 1, floor 2
      .db %00000111, %00011000 ;ceiling 3, floor 2
      .db %00001111, %00011000 ;ceiling 4, floor 2
      .db %11111111, %00011000 ;ceiling 8, floor 2
      .db %00000001, %00011111 ;ceiling 1, floor 5
      .db %00000111, %00011111 ;ceiling 3, floor 5
      .db %00001111, %00011111 ;ceiling 4, floor 5
      .db %10000001, %00011111 ;ceiling 1, floor 6
      .db %00000001, %00000000 ;ceiling 1, no floor
      .db %10001111, %00011111 ;ceiling 4, floor 6
      .db %11110001, %00011111 ;ceiling 1, floor 9
      .db %11111001, %00011000 ;ceiling 1, middle 5, floor 2
      .db %11110001, %00011000 ;ceiling 1, middle 4, floor 2
      .db %11111111, %00011111 ;completely solid top to bottom

AreaParserCore:
      lda BackloadingFlag       ;check to see if we are starting right of start
      beq RenderSceneryTerrain  ;if not, go ahead and render background, foreground and terrain
      jsr ProcessAreaData       ;otherwise skip ahead and load level data

RenderSceneryTerrain:
          ldx #$0c
          lda #$00
ClrMTBuf: sta MetatileBuffer,x       ;clear out metatile buffer
          dex
          bpl ClrMTBuf
          ldy BackgroundScenery      ;do we need to render the background scenery?
          beq RendFore               ;if not, skip to check the foreground
          lda CurrentPageLoc         ;otherwise check for every third page
ThirdP:   cmp #$03
          bmi RendBack               ;if less than three we're there
          sec
          sbc #$03                   ;if 3 or more, subtract 3 and 
          bpl ThirdP                 ;do an unconditional branch
RendBack: asl                        ;move results to higher nybble
          asl
          asl
          asl
          adc BSceneDataOffsets-1,y  ;add to it offset loaded from here
          adc CurrentColumnPos       ;add to the result our current column position
          tax
          lda BackSceneryData,x      ;load data from sum of offsets
          beq RendFore               ;if zero, no scenery for that part
          pha
          and #$0f                   ;save to stack and clear high nybble
          sec
          sbc #$01                   ;subtract one (because low nybble is $01-$0c)
          sta $00                    ;save low nybble
          asl                        ;multiply by three (shift to left and add result to old one)
          adc $00                    ;note that since d7 was nulled, the carry flag is always clear
          tax                        ;save as offset for background scenery metatile data
          pla                        ;get high nybble from stack, move low
          lsr
          lsr
          lsr
          lsr
          tay                        ;use as second offset (used to determine height)
          lda #$03                   ;use previously saved memory location for counter
          sta $00
SceLoop1: lda BackSceneryMetatiles,x ;load metatile data from offset of (lsb - 1) * 3
          sta MetatileBuffer,y       ;store into buffer from offset of (msb / 16)
          inx
          iny
          cpy #$0b                   ;if at this location, leave loop
          beq RendFore
          dec $00                    ;decrement until counter expires, barring exception
          bne SceLoop1
RendFore: ldx ForegroundScenery      ;check for foreground data needed or not
          beq RendTerr               ;if not, skip this part
          ldy FSceneDataOffsets-1,x  ;load offset from location offset by header value, then
          ldx #$00                   ;reinit X
SceLoop2: lda ForeSceneryData,y      ;load data until counter expires
          beq NoFore                 ;do not store if zero found
          sta MetatileBuffer,x
NoFore:   iny
          inx
          cpx #$0d                   ;store up to end of metatile buffer
          bne SceLoop2
RendTerr: ldy AreaType               ;check world type for water level
          bne TerMTile               ;if not water level, skip this part
          lda WorldNumber            ;check world number, if not world number eight
          cmp #World8                ;then skip this part
          bne TerMTile
          lda #$62                   ;if set as water level and world number eight,
          jmp StoreMT                ;use castle wall metatile as terrain type
TerMTile: lda TerrainMetatiles,y     ;otherwise get appropriate metatile for area type
          ldy CloudTypeOverride      ;check for cloud type override
          beq StoreMT                ;if not set, keep value otherwise
          lda #$88                   ;use cloud block terrain
StoreMT:  sta $07                    ;store value here
          ldx #$00                   ;initialize X, use as metatile buffer offset
          lda TerrainControl         ;use yet another value from the header
          asl                        ;multiply by 2 and use as yet another offset
          tay
TerrLoop: lda TerrainRenderBits,y    ;get one of the terrain rendering bit data
          sta $00
          iny                        ;increment Y and use as offset next time around
          sty $01
          lda CloudTypeOverride      ;skip if value here is zero
          beq NoCloud2
          cpx #$00                   ;otherwise, check if we're doing the ceiling byte
          beq NoCloud2
          lda $00                    ;if not, mask out all but d3
          and #%00001000
          sta $00
NoCloud2: ldy #$00                   ;start at beginning of bitmasks
TerrBChk: lda Bitmasks,y             ;load bitmask, then perform AND on contents of first byte
          bit $00
          beq NextTBit               ;if not set, skip this part (do not write terrain to buffer)
          lda $07
          sta MetatileBuffer,x       ;load terrain type metatile number and store into buffer here
NextTBit: inx                        ;continue until end of buffer
          cpx #$0d
          beq RendBBuf               ;if we're at the end, break out of this loop
          lda AreaType               ;check world type for underground area
          cmp #$02
          bne EndUChk                ;if not underground, skip this part
          cpx #$0b
          bne EndUChk                ;if we're at the bottom of the screen, override
          lda #$54                   ;old terrain type with ground level terrain type
          sta $07
EndUChk:  iny                        ;increment bitmasks offset in Y
          cpy #$08
          bne TerrBChk               ;if not all bits checked, loop back    
          ldy $01
          bne TerrLoop               ;unconditional branch, use Y to load next byte
RendBBuf: jsr ProcessAreaData        ;do the area data loading routine now
          lda BlockBufferColumnPos
          jsr GetBlockBufferAddr     ;get block buffer address from where we're at
          ldx #$00
          ldy #$00                   ;init index regs and start at beginning of smaller buffer
ChkMTLow: sty $00
          lda MetatileBuffer,x       ;load stored metatile number
          and #%11000000             ;mask out all but 2 MSB
          asl
          rol                        ;make %xx000000 into %000000xx
          rol
          tay                        ;use as offset in Y
          lda MetatileBuffer,x       ;reload original unmasked value here
          cmp BlockBuffLowBounds,y   ;check for certain values depending on bits set
          bcs StrBlock               ;if equal or greater, branch
          lda #$00                   ;if less, init value before storing
StrBlock: ldy $00                    ;get offset for block buffer
          sta ($06),y                ;store value into block buffer
          tya
          clc                        ;add 16 (move down one row) to offset
          adc #$10
          tay
          inx                        ;increment column value
          cpx #$0d
          bcc ChkMTLow               ;continue until we pass last row, then leave
          rts

;numbers lower than these with the same attribute bits
;will not be stored in the block buffer
BlockBuffLowBounds:
      .db $10, $51, $88, $c0

;-------------------------------------------------------------------------------------
;$00 - used to store area object identifier
;$07 - used as adder to find proper area object code

ProcessAreaData:
            ldx #$02                 ;start at the end of area object buffer
ProcADLoop: stx ObjectOffset
            lda #$00                 ;reset flag
            sta BehindAreaParserFlag
            ldy AreaDataOffset       ;get offset of area data pointer
            lda (AreaData),y         ;get first byte of area object
            cmp #$fd                 ;if end-of-area, skip all this crap
            beq RdyDecode
            lda AreaObjectLength,x   ;check area object buffer flag
            bpl RdyDecode            ;if buffer not negative, branch, otherwise
            iny
            lda (AreaData),y         ;get second byte of area object
            asl                      ;check for page select bit (d7), branch if not set
            bcc Chk1Row13
            lda AreaObjectPageSel    ;check page select
            bne Chk1Row13
            inc AreaObjectPageSel    ;if not already set, set it now
            inc AreaObjectPageLoc    ;and increment page location
Chk1Row13:  dey
            lda (AreaData),y         ;reread first byte of level object
            and #$0f                 ;mask out high nybble
            cmp #$0d                 ;row 13?
            bne Chk1Row14
            iny                      ;if so, reread second byte of level object
            lda (AreaData),y
            dey                      ;decrement to get ready to read first byte
            and #%01000000           ;check for d6 set (if not, object is page control)
            bne CheckRear
            lda AreaObjectPageSel    ;if page select is set, do not reread
            bne CheckRear
            iny                      ;if d6 not set, reread second byte
            lda (AreaData),y
            and #%00011111           ;mask out all but 5 LSB and store in page control
            sta AreaObjectPageLoc
            inc AreaObjectPageSel    ;increment page select
            jmp NextAObj
Chk1Row14:  cmp #$0e                 ;row 14?
            bne CheckRear
            lda BackloadingFlag      ;check flag for saved page number and branch if set
            bne RdyDecode            ;to render the object (otherwise bg might not look right)
CheckRear:  lda AreaObjectPageLoc    ;check to see if current page of level object is
            cmp CurrentPageLoc       ;behind current page of renderer
            bcc SetBehind            ;if so branch
RdyDecode:  jsr DecodeAreaData       ;do sub and do not turn on flag
            jmp ChkLength
SetBehind:  inc BehindAreaParserFlag ;turn on flag if object is behind renderer
NextAObj:   jsr IncAreaObjOffset     ;increment buffer offset and move on
ChkLength:  ldx ObjectOffset         ;get buffer offset
            lda AreaObjectLength,x   ;check object length for anything stored here
            bmi ProcLoopb            ;if not, branch to handle loopback
            dec AreaObjectLength,x   ;otherwise decrement length or get rid of it
ProcLoopb:  dex                      ;decrement buffer offset
            bpl ProcADLoop           ;and loopback unless exceeded buffer
            lda BehindAreaParserFlag ;check for flag set if objects were behind renderer
            bne ProcessAreaData      ;branch if true to load more level data, otherwise
            lda BackloadingFlag      ;check for flag set if starting right of page $00
            bne ProcessAreaData      ;branch if true to load more level data, otherwise leave
EndAParse:  rts

IncAreaObjOffset:
      inc AreaDataOffset    ;increment offset of level pointer
      inc AreaDataOffset
      lda #$00              ;reset page select
      sta AreaObjectPageSel
      rts

DecodeAreaData:
          lda AreaObjectLength,x     ;check current buffer flag
          bmi Chk1stB
          ldy AreaObjOffsetBuffer,x  ;if not, get offset from buffer
Chk1stB:  ldx #$10                   ;load offset of 16 for special row 15
          lda (AreaData),y           ;get first byte of level object again
          cmp #$fd
          beq EndAParse              ;if end of level, leave this routine
          and #$0f                   ;otherwise, mask out low nybble
          cmp #$0f                   ;row 15?
          beq ChkRow14               ;if so, keep the offset of 16
          ldx #$08                   ;otherwise load offset of 8 for special row 12
          cmp #$0c                   ;row 12?
          beq ChkRow14               ;if so, keep the offset value of 8
          ldx #$00                   ;otherwise nullify value by default
ChkRow14: stx $07                    ;store whatever value we just loaded here
          ldx ObjectOffset           ;get object offset again
          cmp #$0e                   ;row 14?
          bne ChkRow13
          lda #$00                   ;if so, load offset with $00
          sta $07
          lda #$2e                   ;and load A with another value
          bne NormObj                ;unconditional branch
ChkRow13: cmp #$0d                   ;row 13?
          bne ChkSRows
          lda #$22                   ;if so, load offset with 34
          sta $07
          iny                        ;get next byte
          lda (AreaData),y
          and #%01000000             ;mask out all but d6 (page control obj bit)
          beq LeavePar               ;if d6 clear, branch to leave (we handled this earlier)
          lda (AreaData),y           ;otherwise, get byte again
          and #%01111111             ;mask out d7
          cmp #$4b                   ;check for loop command in low nybble
          bne Mask2MSB               ;(plus d6 set for object other than page control)
          inc LoopCommand            ;if loop command, set loop command flag
Mask2MSB: and #%00111111             ;mask out d7 and d6
          jmp NormObj                ;and jump
ChkSRows: cmp #$0c                   ;row 12-15?
          bcs SpecObj
          iny                        ;if not, get second byte of level object
          lda (AreaData),y
          and #%01110000             ;mask out all but d6-d4
          bne LrgObj                 ;if any bits set, branch to handle large object
          lda #$16
          sta $07                    ;otherwise set offset of 24 for small object
          lda (AreaData),y           ;reload second byte of level object
          and #%00001111             ;mask out higher nybble and jump
          jmp NormObj
LrgObj:   sta $00                    ;store value here (branch for large objects)
          cmp #$70                   ;check for vertical pipe object
          bne NotWPipe
          lda (AreaData),y           ;if not, reload second byte
          and #%00001000             ;mask out all but d3 (usage control bit)
          beq NotWPipe               ;if d3 clear, branch to get original value
          lda #$00                   ;otherwise, nullify value for warp pipe
          sta $00
NotWPipe: lda $00                    ;get value and jump ahead
          jmp MoveAOId
SpecObj:  iny                        ;branch here for rows 12-15
          lda (AreaData),y
          and #%01110000             ;get next byte and mask out all but d6-d4
MoveAOId: lsr                        ;move d6-d4 to lower nybble
          lsr
          lsr
          lsr
NormObj:  sta $00                    ;store value here (branch for small objects and rows 13 and 14)
          lda AreaObjectLength,x     ;is there something stored here already?
          bpl RunAObj                ;if so, branch to do its particular sub
          lda AreaObjectPageLoc      ;otherwise check to see if the object we've loaded is on the
          cmp CurrentPageLoc         ;same page as the renderer, and if so, branch
          beq InitRear
          ldy AreaDataOffset         ;if not, get old offset of level pointer
          lda (AreaData),y           ;and reload first byte
          and #%00001111
          cmp #$0e                   ;row 14?
          bne LeavePar
          lda BackloadingFlag        ;if so, check backloading flag
          bne StrAObj                ;if set, branch to render object, else leave
LeavePar: rts
InitRear: lda BackloadingFlag        ;check backloading flag to see if it's been initialized
          beq BackColC               ;branch to column-wise check
          lda #$00                   ;if not, initialize both backloading and 
          sta BackloadingFlag        ;behind-renderer flags and leave
          sta BehindAreaParserFlag
          sta ObjectOffset
LoopCmdE: rts
BackColC: ldy AreaDataOffset         ;get first byte again
          lda (AreaData),y
          and #%11110000             ;mask out low nybble and move high to low
          lsr
          lsr
          lsr
          lsr
          cmp CurrentColumnPos       ;is this where we're at?
          bne LeavePar               ;if not, branch to leave
StrAObj:  lda AreaDataOffset         ;if so, load area obj offset and store in buffer
          sta AreaObjOffsetBuffer,x
          jsr IncAreaObjOffset       ;do sub to increment to next object data
RunAObj:  lda $00                    ;get stored value and add offset to it
          clc                        ;then use the jump engine with current contents of A
          adc $07
          jsr JumpEngine

;large objects (rows $00-$0b or 00-11, d6-d4 set)
      .dw VerticalPipe         ;used by warp pipes
      .dw AreaStyleObject
      .dw RowOfBricks
      .dw RowOfSolidBlocks
      .dw RowOfCoins
      .dw ColumnOfBricks
      .dw ColumnOfSolidBlocks
      .dw VerticalPipe         ;used by decoration pipes

;objects for special row $0c or 12
      .dw Hole_Empty
      .dw PulleyRopeObject
      .dw Bridge_High
      .dw Bridge_Middle
      .dw Bridge_Low
      .dw Hole_Water
      .dw QuestionBlockRow_High
      .dw QuestionBlockRow_Low

;objects for special row $0f or 15
      .dw EndlessRope
      .dw BalancePlatRope
      .dw CastleObject
      .dw StaircaseObject
      .dw ExitPipe
      .dw FlagBalls_Residual

;small objects (rows $00-$0b or 00-11, d6-d4 all clear)
      .dw QuestionBlock     ;power-up
      .dw QuestionBlock     ;coin
      .dw QuestionBlock     ;hidden, coin
      .dw Hidden1UpBlock    ;hidden, 1-up
      .dw BrickWithItem     ;brick, power-up
      .dw BrickWithItem     ;brick, vine
      .dw BrickWithItem     ;brick, star
      .dw BrickWithCoins    ;brick, coins
      .dw BrickWithItem     ;brick, 1-up
      .dw WaterPipe
      .dw EmptyBlock
      .dw Jumpspring

;objects for special row $0d or 13 (d6 set)
      .dw IntroPipe
      .dw FlagpoleObject
      .dw AxeObj
      .dw ChainObj
      .dw CastleBridgeObj
      .dw ScrollLockObject_Warp
      .dw ScrollLockObject
      .dw ScrollLockObject
      .dw AreaFrenzy            ;flying cheep-cheeps 
      .dw AreaFrenzy            ;bullet bills or swimming cheep-cheeps
      .dw AreaFrenzy            ;stop frenzy
      .dw LoopCmdE

;object for special row $0e or 14
      .dw AlterAreaAttributes

;-------------------------------------------------------------------------------------
;(these apply to all area object subroutines in this section unless otherwise stated)
;$00 - used to store offset used to find object code
;$07 - starts with adder from area parser, used to store row offset

AlterAreaAttributes:
         ldy AreaObjOffsetBuffer,x ;load offset for level object data saved in buffer
         iny                       ;load second byte
         lda (AreaData),y
         pha                       ;save in stack for now
         and #%01000000
         bne Alter2                ;branch if d6 is set
         pla
         pha                       ;pull and push offset to copy to A
         and #%00001111            ;mask out high nybble and store as
         sta TerrainControl        ;new terrain height type bits
         pla
         and #%00110000            ;pull and mask out all but d5 and d4
         lsr                       ;move bits to lower nybble and store
         lsr                       ;as new background scenery bits
         lsr
         lsr
         sta BackgroundScenery     ;then leave
         rts
Alter2:  pla
         and #%00000111            ;mask out all but 3 LSB
         cmp #$04                  ;if four or greater, set color control bits
         bcc SetFore               ;and nullify foreground scenery bits
         sta BackgroundColorCtrl
         lda #$00
SetFore: sta ForegroundScenery     ;otherwise set new foreground scenery bits
         rts

;--------------------------------

ScrollLockObject_Warp:
         ldx #$04            ;load value of 4 for game text routine as default
         lda WorldNumber     ;warp zone (4-3-2), then check world number
         beq WarpNum
         inx                 ;if world number > 1, increment for next warp zone (5)
         ldy AreaType        ;check area type
         dey
         bne WarpNum         ;if ground area type, increment for last warp zone
         inx                 ;(8-7-6) and move on
WarpNum: txa
         sta WarpZoneControl ;store number here to be used by warp zone routine
         jsr WriteGameText   ;print text and warp zone numbers
         lda #PiranhaPlant
         jsr KillEnemies     ;load identifier for piranha plants and do sub

ScrollLockObject:
      lda ScrollLock      ;invert scroll lock to turn it on
      eor #%00000001
      sta ScrollLock
      rts

;--------------------------------

FrenzyIDData:
      .db FlyCheepCheepFrenzy, BBill_CCheep_Frenzy, Stop_Frenzy

AreaFrenzy:  ldx $00               ;use area object identifier bit as offset
             lda FrenzyIDData-8,x  ;note that it starts at 8, thus weird address here
             ldy #$05
FreCompLoop: dey                   ;check regular slots of enemy object buffer
             bmi ExitAFrenzy       ;if all slots checked and enemy object not found, branch to store
             cmp Enemy_ID,y    ;check for enemy object in buffer versus frenzy object
             bne FreCompLoop
             lda #$00              ;if enemy object already present, nullify queue and leave
ExitAFrenzy: sta EnemyFrenzyQueue  ;store enemy into frenzy queue
             rts

;--------------------------------
;$06 - used by MushroomLedge to store length

AreaStyleObject:
      lda AreaStyle        ;load level object style and jump to the right sub
      jsr JumpEngine 
      .dw TreeLedge        ;also used for cloud type levels
      .dw MushroomLedge
      .dw BulletBillCannon

TreeLedge:
          jsr GetLrgObjAttrib     ;get row and length of green ledge
          lda AreaObjectLength,x  ;check length counter for expiration
          beq EndTreeL   
          bpl MidTreeL
          tya
          sta AreaObjectLength,x  ;store lower nybble into buffer flag as length of ledge
          lda CurrentPageLoc
          ora CurrentColumnPos    ;are we at the start of the level?
          beq MidTreeL
          lda #$16                ;render start of tree ledge
          jmp NoUnder
MidTreeL: ldx $07
          lda #$17                ;render middle of tree ledge
          sta MetatileBuffer,x    ;note that this is also used if ledge position is
          lda #$4c                ;at the start of level for continuous effect
          jmp AllUnder            ;now render the part underneath
EndTreeL: lda #$18                ;render end of tree ledge
          jmp NoUnder

MushroomLedge:
          jsr ChkLrgObjLength        ;get shroom dimensions
          sty $06                    ;store length here for now
          bcc EndMushL
          lda AreaObjectLength,x     ;divide length by 2 and store elsewhere
          lsr
          sta MushroomLedgeHalfLen,x
          lda #$19                   ;render start of mushroom
          jmp NoUnder
EndMushL: lda #$1b                   ;if at the end, render end of mushroom
          ldy AreaObjectLength,x
          beq NoUnder
          lda MushroomLedgeHalfLen,x ;get divided length and store where length
          sta $06                    ;was stored originally
          ldx $07
          lda #$1a
          sta MetatileBuffer,x       ;render middle of mushroom
          cpy $06                    ;are we smack dab in the center?
          bne MushLExit              ;if not, branch to leave
          inx
          lda #$4f
          sta MetatileBuffer,x       ;render stem top of mushroom underneath the middle
          lda #$50
AllUnder: inx
          ldy #$0f                   ;set $0f to render all way down
          jmp RenderUnderPart       ;now render the stem of mushroom
NoUnder:  ldx $07                    ;load row of ledge
          ldy #$00                   ;set 0 for no bottom on this part
          jmp RenderUnderPart

;--------------------------------

;tiles used by pulleys and rope object
PulleyRopeMetatiles:
      .db $42, $41, $43

PulleyRopeObject:
           jsr ChkLrgObjLength       ;get length of pulley/rope object
           ldy #$00                  ;initialize metatile offset
           bcs RenderPul             ;if starting, render left pulley
           iny
           lda AreaObjectLength,x    ;if not at the end, render rope
           bne RenderPul
           iny                       ;otherwise render right pulley
RenderPul: lda PulleyRopeMetatiles,y
           sta MetatileBuffer        ;render at the top of the screen
MushLExit: rts                       ;and leave

;--------------------------------
;$06 - used to store upper limit of rows for CastleObject

CastleMetatiles:
      .db $00, $45, $45, $45, $00
      .db $00, $48, $47, $46, $00
      .db $45, $49, $49, $49, $45
      .db $47, $47, $4a, $47, $47
      .db $47, $47, $4b, $47, $47
      .db $49, $49, $49, $49, $49
      .db $47, $4a, $47, $4a, $47
      .db $47, $4b, $47, $4b, $47
      .db $47, $47, $47, $47, $47
      .db $4a, $47, $4a, $47, $4a
      .db $4b, $47, $4b, $47, $4b

CastleObject:
            jsr GetLrgObjAttrib      ;save lower nybble as starting row
            sty $07                  ;if starting row is above $0a, game will crash!!!
            ldy #$04
            jsr ChkLrgObjFixedLength ;load length of castle if not already loaded
            txa                  
            pha                      ;save obj buffer offset to stack
            ldy AreaObjectLength,x   ;use current length as offset for castle data
            ldx $07                  ;begin at starting row
            lda #$0b
            sta $06                  ;load upper limit of number of rows to print
CRendLoop:  lda CastleMetatiles,y    ;load current byte using offset
            sta MetatileBuffer,x
            inx                      ;store in buffer and increment buffer offset
            lda $06
            beq ChkCFloor            ;have we reached upper limit yet?
            iny                      ;if not, increment column-wise
            iny                      ;to byte in next row
            iny
            iny
            iny
            dec $06                  ;move closer to upper limit
ChkCFloor:  cpx #$0b                 ;have we reached the row just before floor?
            bne CRendLoop            ;if not, go back and do another row
            pla
            tax                      ;get obj buffer offset from before
            lda CurrentPageLoc
            beq ExitCastle           ;if we're at page 0, we do not need to do anything else
            lda AreaObjectLength,x   ;check length
            cmp #$01                 ;if length almost about to expire, put brick at floor
            beq PlayerStop
            ldy $07                  ;check starting row for tall castle ($00)
            bne NotTall
            cmp #$03                 ;if found, then check to see if we're at the second column
            beq PlayerStop
NotTall:    cmp #$02                 ;if not tall castle, check to see if we're at the third column
            bne ExitCastle           ;if we aren't and the castle is tall, don't create flag yet
            jsr GetAreaObjXPosition  ;otherwise, obtain and save horizontal pixel coordinate
            pha
            jsr FindEmptyEnemySlot   ;find an empty place on the enemy object buffer
            pla
            sta Enemy_X_Position,x   ;then write horizontal coordinate for star flag
            lda CurrentPageLoc
            sta Enemy_PageLoc,x      ;set page location for star flag
            lda #$01
            sta Enemy_Y_HighPos,x    ;set vertical high byte
            sta Enemy_Flag,x         ;set flag for buffer
            lda #$90
            sta Enemy_Y_Position,x   ;set vertical coordinate
            lda #StarFlagObject      ;set star flag value in buffer itself
            sta Enemy_ID,x
            rts
PlayerStop: ldy #$52                 ;put brick at floor to stop player at end of level
            sty MetatileBuffer+10    ;this is only done if we're on the second column
ExitCastle: rts

;--------------------------------

WaterPipe:
      jsr GetLrgObjAttrib     ;get row and lower nybble
      ldy AreaObjectLength,x  ;get length (residual code, water pipe is 1 col thick)
      ldx $07                 ;get row
      lda #$6b
      sta MetatileBuffer,x    ;draw something here and below it
      lda #$6c
      sta MetatileBuffer+1,x
      rts

;--------------------------------
;$05 - used to store length of vertical shaft in RenderSidewaysPipe
;$06 - used to store leftover horizontal length in RenderSidewaysPipe
; and vertical length in VerticalPipe and GetPipeHeight

IntroPipe:
               ldy #$03                 ;check if length set, if not set, set it
               jsr ChkLrgObjFixedLength
               ldy #$0a                 ;set fixed value and render the sideways part
               jsr RenderSidewaysPipe
               bcs NoBlankP             ;if carry flag set, not time to draw vertical pipe part
               ldx #$06                 ;blank everything above the vertical pipe part
VPipeSectLoop: lda #$00                 ;all the way to the top of the screen
               sta MetatileBuffer,x     ;because otherwise it will look like exit pipe
               dex
               bpl VPipeSectLoop
               lda VerticalPipeData,y   ;draw the end of the vertical pipe part
               sta MetatileBuffer+7
NoBlankP:      rts

SidePipeShaftData:
      .db $15, $14  ;used to control whether or not vertical pipe shaft
      .db $00, $00  ;is drawn, and if so, controls the metatile number
SidePipeTopPart:
      .db $15, $1e  ;top part of sideways part of pipe
      .db $1d, $1c
SidePipeBottomPart: 
      .db $15, $21  ;bottom part of sideways part of pipe
      .db $20, $1f

ExitPipe:
      ldy #$03                 ;check if length set, if not set, set it
      jsr ChkLrgObjFixedLength
      jsr GetLrgObjAttrib      ;get vertical length, then plow on through RenderSidewaysPipe

RenderSidewaysPipe:
              dey                       ;decrement twice to make room for shaft at bottom
              dey                       ;and store here for now as vertical length
              sty $05
              ldy AreaObjectLength,x    ;get length left over and store here
              sty $06
              ldx $05                   ;get vertical length plus one, use as buffer offset
              inx
              lda SidePipeShaftData,y   ;check for value $00 based on horizontal offset
              cmp #$00
              beq DrawSidePart          ;if found, do not draw the vertical pipe shaft
              ldx #$00
              ldy $05                   ;init buffer offset and get vertical length
              jsr RenderUnderPart       ;and render vertical shaft using tile number in A
              clc                       ;clear carry flag to be used by IntroPipe
DrawSidePart: ldy $06                   ;render side pipe part at the bottom
              lda SidePipeTopPart,y
              sta MetatileBuffer,x      ;note that the pipe parts are stored
              lda SidePipeBottomPart,y  ;backwards horizontally
              sta MetatileBuffer+1,x
              rts

VerticalPipeData:
      .db $11, $10 ;used by pipes that lead somewhere
      .db $15, $14
      .db $13, $12 ;used by decoration pipes
      .db $15, $14

VerticalPipe:
          jsr GetPipeHeight
          lda $00                  ;check to see if value was nullified earlier
          beq WarpPipe             ;(if d3, the usage control bit of second byte, was set)
          iny
          iny
          iny
          iny                      ;add four if usage control bit was not set
WarpPipe: tya                      ;save value in stack
          pha
          lda AreaNumber
          ora WorldNumber          ;if at world 1-1, do not add piranha plant ever
          beq DrawPipe
          ldy AreaObjectLength,x   ;if on second column of pipe, branch
          beq DrawPipe             ;(because we only need to do this once)
          jsr FindEmptyEnemySlot   ;check for an empty moving data buffer space
          bcs DrawPipe             ;if not found, too many enemies, thus skip
          jsr GetAreaObjXPosition  ;get horizontal pixel coordinate
          clc
          adc #$08                 ;add eight to put the piranha plant in the center
          sta Enemy_X_Position,x   ;store as enemy's horizontal coordinate
          lda CurrentPageLoc       ;add carry to current page number
          adc #$00
          sta Enemy_PageLoc,x      ;store as enemy's page coordinate
          lda #$01
          sta Enemy_Y_HighPos,x
          sta Enemy_Flag,x         ;activate enemy flag
          jsr GetAreaObjYPosition  ;get piranha plant's vertical coordinate and store here
          sta Enemy_Y_Position,x
          lda #PiranhaPlant        ;write piranha plant's value into buffer
          sta Enemy_ID,x
          jsr InitPiranhaPlant
DrawPipe: pla                      ;get value saved earlier and use as Y
          tay
          ldx $07                  ;get buffer offset
          lda VerticalPipeData,y   ;draw the appropriate pipe with the Y we loaded earlier
          sta MetatileBuffer,x     ;render the top of the pipe
          inx
          lda VerticalPipeData+2,y ;render the rest of the pipe
          ldy $06                  ;subtract one from length and render the part underneath
          dey
          jmp RenderUnderPart
      
GetPipeHeight:
      ldy #$01       ;check for length loaded, if not, load
      jsr ChkLrgObjFixedLength ;pipe length of 2 (horizontal)
      jsr GetLrgObjAttrib
      tya            ;get saved lower nybble as height
      and #$07       ;save only the three lower bits as
      sta $06        ;vertical length, then load Y with
      ldy AreaObjectLength,x    ;length left over
      rts

FindEmptyEnemySlot:
              ldx #$00          ;start at first enemy slot
EmptyChkLoop: clc               ;clear carry flag by default
              lda Enemy_Flag,x  ;check enemy buffer for nonzero
              beq ExitEmptyChk  ;if zero, leave
              inx
              cpx #$05          ;if nonzero, check next value
              bne EmptyChkLoop
ExitEmptyChk: rts               ;if all values nonzero, carry flag is set

;--------------------------------

Hole_Water:
      jsr ChkLrgObjLength   ;get low nybble and save as length
      lda #$86              ;render waves
      sta MetatileBuffer+10
      ldx #$0b
      ldy #$01              ;now render the water underneath
      lda #$87
      jmp RenderUnderPart

;--------------------------------

QuestionBlockRow_High:
      lda #$03    ;start on the fourth row
      .db $2c     ;BIT instruction opcode

QuestionBlockRow_Low:
      lda #$07             ;start on the eighth row
      pha                  ;save whatever row to the stack for now
      jsr ChkLrgObjLength  ;get low nybble and save as length
      pla
      tax                  ;render question boxes with coins
      lda #$c0
      sta MetatileBuffer,x
      rts

;--------------------------------

Bridge_High:
      lda #$06  ;start on the seventh row from top of screen
      .db $2c   ;BIT instruction opcode

Bridge_Middle:
      lda #$07  ;start on the eighth row
      .db $2c   ;BIT instruction opcode

Bridge_Low:
      lda #$09             ;start on the tenth row
      pha                  ;save whatever row to the stack for now
      jsr ChkLrgObjLength  ;get low nybble and save as length
      pla
      tax                  ;render bridge railing
      lda #$0b
      sta MetatileBuffer,x
      inx
      ldy #$00             ;now render the bridge itself
      lda #$63
      jmp RenderUnderPart

;--------------------------------

FlagBalls_Residual:
      jsr GetLrgObjAttrib  ;get low nybble from object byte
      ldx #$02             ;render flag balls on third row from top
      lda #$6d             ;of screen downwards based on low nybble
      jmp RenderUnderPart

;--------------------------------

FlagpoleObject:
      lda #$24                 ;render flagpole ball on top
      sta MetatileBuffer
      ldx #$01                 ;now render the flagpole shaft
      ldy #$08
      lda #$25
      jsr RenderUnderPart
      lda #$61                 ;render solid block at the bottom
      sta MetatileBuffer+10
      jsr GetAreaObjXPosition
      sec                      ;get pixel coordinate of where the flagpole is,
      sbc #$08                 ;subtract eight pixels and use as horizontal
      sta Enemy_X_Position+5   ;coordinate for the flag
      lda CurrentPageLoc
      sbc #$00                 ;subtract borrow from page location and use as
      sta Enemy_PageLoc+5      ;page location for the flag
      lda #$30
      sta Enemy_Y_Position+5   ;set vertical coordinate for flag
      lda #$b0
      sta FlagpoleFNum_Y_Pos   ;set initial vertical coordinate for flagpole's floatey number
      lda #FlagpoleFlagObject
      sta Enemy_ID+5           ;set flag identifier, note that identifier and coordinates
      inc Enemy_Flag+5         ;use last space in enemy object buffer
      rts

;--------------------------------

EndlessRope:
      ldx #$00       ;render rope from the top to the bottom of screen
      ldy #$0f
      jmp DrawRope

BalancePlatRope:
          txa                 ;save object buffer offset for now
          pha
          ldx #$01            ;blank out all from second row to the bottom
          ldy #$0f            ;with blank used for balance platform rope
          lda #$44
          jsr RenderUnderPart
          pla                 ;get back object buffer offset
          tax
          jsr GetLrgObjAttrib ;get vertical length from lower nybble
          ldx #$01
DrawRope: lda #$40            ;render the actual rope
          jmp RenderUnderPart

;--------------------------------

CoinMetatileData:
      .db $c3, $c2, $c2, $c2

RowOfCoins:
      ldy AreaType            ;get area type
      lda CoinMetatileData,y  ;load appropriate coin metatile
      jmp GetRow

;--------------------------------

C_ObjectRow:
      .db $06, $07, $08

C_ObjectMetatile:
      .db $c5, $0c, $89

CastleBridgeObj:
      ldy #$0c                  ;load length of 13 columns
      jsr ChkLrgObjFixedLength
      jmp ChainObj

AxeObj:
      lda #$08                  ;load bowser's palette into sprite portion of palette
      sta VRAM_Buffer_AddrCtrl

ChainObj:
      ldy $00                   ;get value loaded earlier from decoder
      ldx C_ObjectRow-2,y       ;get appropriate row and metatile for object
      lda C_ObjectMetatile-2,y
      jmp ColObj

EmptyBlock:
        jsr GetLrgObjAttrib  ;get row location
        ldx $07
        lda #$c4
ColObj: ldy #$00             ;column length of 1
        jmp RenderUnderPart

;--------------------------------

SolidBlockMetatiles:
      .db $69, $61, $61, $62

BrickMetatiles:
      .db $22, $51, $52, $52
      .db $88 ;used only by row of bricks object

RowOfBricks:
            ldy AreaType           ;load area type obtained from area offset pointer
            lda CloudTypeOverride  ;check for cloud type override
            beq DrawBricks
            ldy #$04               ;if cloud type, override area type
DrawBricks: lda BrickMetatiles,y   ;get appropriate metatile
            jmp GetRow             ;and go render it

RowOfSolidBlocks:
         ldy AreaType               ;load area type obtained from area offset pointer
         lda SolidBlockMetatiles,y  ;get metatile
GetRow:  pha                        ;store metatile here
         jsr ChkLrgObjLength        ;get row number, load length
DrawRow: ldx $07
         ldy #$00                   ;set vertical height of 1
         pla
         jmp RenderUnderPart        ;render object

ColumnOfBricks:
      ldy AreaType          ;load area type obtained from area offset
      lda BrickMetatiles,y  ;get metatile (no cloud override as for row)
      jmp GetRow2

ColumnOfSolidBlocks:
         ldy AreaType               ;load area type obtained from area offset
         lda SolidBlockMetatiles,y  ;get metatile
GetRow2: pha                        ;save metatile to stack for now
         jsr GetLrgObjAttrib        ;get length and row
         pla                        ;restore metatile
         ldx $07                    ;get starting row
         jmp RenderUnderPart        ;now render the column

;--------------------------------

BulletBillCannon:
             jsr GetLrgObjAttrib      ;get row and length of bullet bill cannon
             ldx $07                  ;start at first row
             lda #$64                 ;render bullet bill cannon
             sta MetatileBuffer,x
             inx
             dey                      ;done yet?
             bmi SetupCannon
             lda #$65                 ;if not, render middle part
             sta MetatileBuffer,x
             inx
             dey                      ;done yet?
             bmi SetupCannon
             lda #$66                 ;if not, render bottom until length expires
             jsr RenderUnderPart
SetupCannon: ldx Cannon_Offset        ;get offset for data used by cannons and whirlpools
             jsr GetAreaObjYPosition  ;get proper vertical coordinate for cannon
             sta Cannon_Y_Position,x  ;and store it here
             lda CurrentPageLoc
             sta Cannon_PageLoc,x     ;store page number for cannon here
             jsr GetAreaObjXPosition  ;get proper horizontal coordinate for cannon
             sta Cannon_X_Position,x  ;and store it here
             inx
             cpx #$06                 ;increment and check offset
             bcc StrCOffset           ;if not yet reached sixth cannon, branch to save offset
             ldx #$00                 ;otherwise initialize it
StrCOffset:  stx Cannon_Offset        ;save new offset and leave
             rts

;--------------------------------

StaircaseHeightData:
      .db $07, $07, $06, $05, $04, $03, $02, $01, $00

StaircaseRowData:
      .db $03, $03, $04, $05, $06, $07, $08, $09, $0a

StaircaseObject:
           jsr ChkLrgObjLength       ;check and load length
           bcc NextStair             ;if length already loaded, skip init part
           lda #$09                  ;start past the end for the bottom
           sta StaircaseControl      ;of the staircase
NextStair: dec StaircaseControl      ;move onto next step (or first if starting)
           ldy StaircaseControl
           ldx StaircaseRowData,y    ;get starting row and height to render
           lda StaircaseHeightData,y
           tay
           lda #$61                  ;now render solid block staircase
           jmp RenderUnderPart

;--------------------------------

Jumpspring:
      jsr GetLrgObjAttrib
      jsr FindEmptyEnemySlot      ;find empty space in enemy object buffer
      jsr GetAreaObjXPosition     ;get horizontal coordinate for jumpspring
      sta Enemy_X_Position,x      ;and store
      lda CurrentPageLoc          ;store page location of jumpspring
      sta Enemy_PageLoc,x
      jsr GetAreaObjYPosition     ;get vertical coordinate for jumpspring
      sta Enemy_Y_Position,x      ;and store
      sta Jumpspring_FixedYPos,x  ;store as permanent coordinate here
      lda #JumpspringObject
      sta Enemy_ID,x              ;write jumpspring object to enemy object buffer
      ldy #$01
      sty Enemy_Y_HighPos,x       ;store vertical high byte
      inc Enemy_Flag,x            ;set flag for enemy object buffer
      ldx $07
      lda #$67                    ;draw metatiles in two rows where jumpspring is
      sta MetatileBuffer,x
      lda #$68
      sta MetatileBuffer+1,x
      rts

;--------------------------------
;$07 - used to save ID of brick object

Hidden1UpBlock:
      lda Hidden1UpFlag  ;if flag not set, do not render object
      beq ExitDecBlock
      lda #$00           ;if set, init for the next one
      sta Hidden1UpFlag
      jmp BrickWithItem  ;jump to code shared with unbreakable bricks

QuestionBlock:
      jsr GetAreaObjectID ;get value from level decoder routine
      jmp DrawQBlk        ;go to render it

BrickWithCoins:
      lda #$00                 ;initialize multi-coin timer flag
      sta BrickCoinTimerFlag

BrickWithItem:
          jsr GetAreaObjectID         ;save area object ID
          sty $07              
          lda #$00                    ;load default adder for bricks with lines
          ldy AreaType                ;check level type for ground level
          dey
          beq BWithL                  ;if ground type, do not start with 5
          lda #$05                    ;otherwise use adder for bricks without lines
BWithL:   clc                         ;add object ID to adder
          adc $07
          tay                         ;use as offset for metatile
DrawQBlk: lda BrickQBlockMetatiles,y  ;get appropriate metatile for brick (question block
          pha                         ;if branched to here from question block routine)
          jsr GetLrgObjAttrib         ;get row from location byte
          jmp DrawRow                 ;now render the object

GetAreaObjectID:
              lda $00    ;get value saved from area parser routine
              sec
              sbc #$00   ;possibly residual code
              tay        ;save to Y
ExitDecBlock: rts

;--------------------------------

HoleMetatiles:
      .db $87, $00, $00, $00

Hole_Empty:
            jsr ChkLrgObjLength          ;get lower nybble and save as length
            bcc NoWhirlP                 ;skip this part if length already loaded
            lda AreaType                 ;check for water type level
            bne NoWhirlP                 ;if not water type, skip this part
            ldx Whirlpool_Offset         ;get offset for data used by cannons and whirlpools
            jsr GetAreaObjXPosition      ;get proper vertical coordinate of where we're at
            sec
            sbc #$10                     ;subtract 16 pixels
            sta Whirlpool_LeftExtent,x   ;store as left extent of whirlpool
            lda CurrentPageLoc           ;get page location of where we're at
            sbc #$00                     ;subtract borrow
            sta Whirlpool_PageLoc,x      ;save as page location of whirlpool
            iny
            iny                          ;increment length by 2
            tya
            asl                          ;multiply by 16 to get size of whirlpool
            asl                          ;note that whirlpool will always be
            asl                          ;two blocks bigger than actual size of hole
            asl                          ;and extend one block beyond each edge
            sta Whirlpool_Length,x       ;save size of whirlpool here
            inx
            cpx #$05                     ;increment and check offset
            bcc StrWOffset               ;if not yet reached fifth whirlpool, branch to save offset
            ldx #$00                     ;otherwise initialize it
StrWOffset: stx Whirlpool_Offset         ;save new offset here
NoWhirlP:   ldx AreaType                 ;get appropriate metatile, then
            lda HoleMetatiles,x          ;render the hole proper
            ldx #$08
            ldy #$0f                     ;start at ninth row and go to bottom, run RenderUnderPart

;--------------------------------

RenderUnderPart:
             sty AreaObjectHeight  ;store vertical length to render
             ldy MetatileBuffer,x  ;check current spot to see if there's something
             beq DrawThisRow       ;we need to keep, if nothing, go ahead
             cpy #$17
             beq WaitOneRow        ;if middle part (tree ledge), wait until next row
             cpy #$1a
             beq WaitOneRow        ;if middle part (mushroom ledge), wait until next row
             cpy #$c0
             beq DrawThisRow       ;if question block w/ coin, overwrite
             cpy #$c0
             bcs WaitOneRow        ;if any other metatile with palette 3, wait until next row
             cpy #$54
             bne DrawThisRow       ;if cracked rock terrain, overwrite
             cmp #$50
             beq WaitOneRow        ;if stem top of mushroom, wait until next row
DrawThisRow: sta MetatileBuffer,x  ;render contents of A from routine that called this
WaitOneRow:  inx
             cpx #$0d              ;stop rendering if we're at the bottom of the screen
             bcs ExitUPartR
             ldy AreaObjectHeight  ;decrement, and stop rendering if there is no more length
             dey
             bpl RenderUnderPart
ExitUPartR:  rts

;--------------------------------

ChkLrgObjLength:
        jsr GetLrgObjAttrib     ;get row location and size (length if branched to from here)

ChkLrgObjFixedLength:
        lda AreaObjectLength,x  ;check for set length counter
        clc                     ;clear carry flag for not just starting
        bpl LenSet              ;if counter not set, load it, otherwise leave alone
        tya                     ;save length into length counter
        sta AreaObjectLength,x
        sec                     ;set carry flag if just starting
LenSet: rts


GetLrgObjAttrib:
      ldy AreaObjOffsetBuffer,x ;get offset saved from area obj decoding routine
      lda (AreaData),y          ;get first byte of level object
      and #%00001111
      sta $07                   ;save row location
      iny
      lda (AreaData),y          ;get next byte, save lower nybble (length or height)
      and #%00001111            ;as Y, then leave
      tay
      rts

;--------------------------------

GetAreaObjXPosition:
      lda CurrentColumnPos    ;multiply current offset where we're at by 16
      asl                     ;to obtain horizontal pixel coordinate
      asl
      asl
      asl
      rts

;--------------------------------

GetAreaObjYPosition:
      lda $07  ;multiply value by 16
      asl
      asl      ;this will give us the proper vertical pixel coordinate
      asl
      asl
      clc
      adc #32  ;add 32 pixels for the status bar
      rts

;-------------------------------------------------------------------------------------

;unused space
      .db $ff, $ff

;-------------------------------------------------------------------------------------

AreaDataOfsLoopback:
      .db $12, $36, $0e, $0e, $0e, $32, $32, $32, $0a, $26, $40

;bonus area data offsets, included here for comparison purposes
;underground bonus area  - c2
;cloud area 1 (day)      - 2b
;cloud area 2 (night)    - 34
;water area (5-2/6-2)    - 00
;water area (8-4)        - 02
;warp zone area (4-2)    - 2f

EnemyDataAddrLow:
      .db <E_CastleArea1, <E_CastleArea2, <E_CastleArea3, <E_CastleArea4, <E_CastleArea5, <E_CastleArea6
      .db <E_GroundArea1, <E_GroundArea2, <E_GroundArea3, <E_GroundArea4, <E_GroundArea5, <E_GroundArea6
      .db <E_GroundArea7, <E_GroundArea8, <E_GroundArea9, <E_GroundArea10, <E_GroundArea11, <E_GroundArea12
      .db <E_GroundArea13, <E_GroundArea14, <E_GroundArea15, <E_GroundArea16, <E_GroundArea17, <E_GroundArea18
      .db <E_GroundArea19, <E_GroundArea20, <E_GroundArea21, <E_GroundArea22, <E_UndergroundArea1
      .db <E_UndergroundArea2, <E_UndergroundArea3, <E_WaterArea1, <E_WaterArea2, <E_WaterArea3

EnemyDataAddrHigh:
      .db >E_CastleArea1, >E_CastleArea2, >E_CastleArea3, >E_CastleArea4, >E_CastleArea5, >E_CastleArea6
      .db >E_GroundArea1, >E_GroundArea2, >E_GroundArea3, >E_GroundArea4, >E_GroundArea5, >E_GroundArea6
      .db >E_GroundArea7, >E_GroundArea8, >E_GroundArea9, >E_GroundArea10, >E_GroundArea11, >E_GroundArea12
      .db >E_GroundArea13, >E_GroundArea14, >E_GroundArea15, >E_GroundArea16, >E_GroundArea17, >E_GroundArea18
      .db >E_GroundArea19, >E_GroundArea20, >E_GroundArea21, >E_GroundArea22, >E_UndergroundArea1
      .db >E_UndergroundArea2, >E_UndergroundArea3, >E_WaterArea1, >E_WaterArea2, >E_WaterArea3

AreaDataHOffsets:
      .db $00, $03, $19, $1c

AreaDataAddrLow:
      .db <L_WaterArea1, <L_WaterArea2, <L_WaterArea3, <L_GroundArea1, <L_GroundArea2, <L_GroundArea3
      .db <L_GroundArea4, <L_GroundArea5, <L_GroundArea6, <L_GroundArea7, <L_GroundArea8, <L_GroundArea9
      .db <L_GroundArea10, <L_GroundArea11, <L_GroundArea12, <L_GroundArea13, <L_GroundArea14, <L_GroundArea15
      .db <L_GroundArea16, <L_GroundArea17, <L_GroundArea18, <L_GroundArea19, <L_GroundArea20, <L_GroundArea21
      .db <L_GroundArea22, <L_UndergroundArea1, <L_UndergroundArea2, <L_UndergroundArea3, <L_CastleArea1
      .db <L_CastleArea2, <L_CastleArea3, <L_CastleArea4, <L_CastleArea5, <L_CastleArea6

AreaDataAddrHigh:
      .db >L_WaterArea1, >L_WaterArea2, >L_WaterArea3, >L_GroundArea1, >L_GroundArea2, >L_GroundArea3
      .db >L_GroundArea4, >L_GroundArea5, >L_GroundArea6, >L_GroundArea7, >L_GroundArea8, >L_GroundArea9
      .db >L_GroundArea10, >L_GroundArea11, >L_GroundArea12, >L_GroundArea13, >L_GroundArea14, >L_GroundArea15
      .db >L_GroundArea16, >L_GroundArea17, >L_GroundArea18, >L_GroundArea19, >L_GroundArea20, >L_GroundArea21
      .db >L_GroundArea22, >L_UndergroundArea1, >L_UndergroundArea2, >L_UndergroundArea3, >L_CastleArea1
      .db >L_CastleArea2, >L_CastleArea3, >L_CastleArea4, >L_CastleArea5, >L_CastleArea6

;ENEMY OBJECT DATA

;level 1-4/6-4
E_CastleArea1:
      .db $76, $dd, $bb, $4c, $ea, $1d, $1b, $cc, $56, $5d
      .db $16, $9d, $c6, $1d, $36, $9d, $c9, $1d, $04, $db
      .db $49, $1d, $84, $1b, $c9, $5d, $88, $95, $0f, $08
      .db $30, $4c, $78, $2d, $a6, $28, $90, $b5
      .db $ff

;level 4-4
E_CastleArea2:
      .db $0f, $03, $56, $1b, $c9, $1b, $0f, $07, $36, $1b
      .db $aa, $1b, $48, $95, $0f, $0a, $2a, $1b, $5b, $0c
      .db $78, $2d, $90, $b5
      .db $ff

;level 2-4/5-4
E_CastleArea3:
      .db $0b, $8c, $4b, $4c, $77, $5f, $eb, $0c, $bd, $db
      .db $19, $9d, $75, $1d, $7d, $5b, $d9, $1d, $3d, $dd
      .db $99, $1d, $26, $9d, $5a, $2b, $8a, $2c, $ca, $1b
      .db $20, $95, $7b, $5c, $db, $4c, $1b, $cc, $3b, $cc
      .db $78, $2d, $a6, $28, $90, $b5
      .db $ff

;level 3-4
E_CastleArea4:
      .db $0b, $8c, $3b, $1d, $8b, $1d, $ab, $0c, $db, $1d
      .db $0f, $03, $65, $1d, $6b, $1b, $05, $9d, $0b, $1b
      .db $05, $9b, $0b, $1d, $8b, $0c, $1b, $8c, $70, $15
      .db $7b, $0c, $db, $0c, $0f, $08, $78, $2d, $a6, $28
      .db $90, $b5
      .db $ff

;level 7-4
E_CastleArea5:
      .db $27, $a9, $4b, $0c, $68, $29, $0f, $06, $77, $1b
      .db $0f, $0b, $60, $15, $4b, $8c, $78, $2d, $90, $b5
      .db $ff

;level 8-4
E_CastleArea6:
      .db $0f, $03, $8e, $65, $e1, $bb, $38, $6d, $a8, $3e, $e5, $e7
      .db $0f, $08, $0b, $02, $2b, $02, $5e, $65, $e1, $bb, $0e
      .db $db, $0e, $bb, $8e, $db, $0e, $fe, $65, $ec, $0f, $0d
      .db $4e, $65, $e1, $0f, $0e, $4e, $02, $e0, $0f, $10, $fe, $e5, $e1
      .db $1b, $85, $7b, $0c, $5b, $95, $78, $2d, $90, $b5
      .db $ff

;level 3-3
E_GroundArea1:
      .db $a5, $86, $e4, $28, $18, $a8, $45, $83, $69, $03
      .db $c6, $29, $9b, $83, $16, $a4, $88, $24, $e9, $28
      .db $05, $a8, $7b, $28, $24, $8f, $c8, $03, $e8, $03
      .db $46, $a8, $85, $24, $c8, $24
      .db $ff

;level 8-3
E_GroundArea2:
      .db $eb, $8e, $0f, $03, $fb, $05, $17, $85, $db, $8e
      .db $0f, $07, $57, $05, $7b, $05, $9b, $80, $2b, $85
      .db $fb, $05, $0f, $0b, $1b, $05, $9b, $05
      .db $ff

;level 4-1
E_GroundArea3:
      .db $2e, $c2, $66, $e2, $11, $0f, $07, $02, $11, $0f, $0c
      .db $12, $11
      .db $ff

;level 6-2
E_GroundArea4:
      .db $0e, $c2, $a8, $ab, $00, $bb, $8e, $6b, $82, $de, $00, $a0
      .db $33, $86, $43, $06, $3e, $b4, $a0, $cb, $02, $0f, $07
      .db $7e, $42, $a6, $83, $02, $0f, $0a, $3b, $02, $cb, $37
      .db $0f, $0c, $e3, $0e
      .db $ff

;level 3-1
E_GroundArea5:
      .db $9b, $8e, $ca, $0e, $ee, $42, $44, $5b, $86, $80, $b8
      .db $1b, $80, $50, $ba, $10, $b7, $5b, $00, $17, $85
      .db $4b, $05, $fe, $34, $40, $b7, $86, $c6, $06, $5b, $80
      .db $83, $00, $d0, $38, $5b, $8e, $8a, $0e, $a6, $00
      .db $bb, $0e, $c5, $80, $f3, $00
      .db $ff

;level 1-1
E_GroundArea6:
      .db $1e, $c2, $00, $6b, $06, $8b, $86, $63, $b7, $0f, $05
      .db $03, $06, $23, $06, $4b, $b7, $bb, $00, $5b, $b7
      .db $fb, $37, $3b, $b7, $0f, $0b, $1b, $37
      .db $ff

;level 1-3/5-3
E_GroundArea7:
      .db $2b, $d7, $e3, $03, $c2, $86, $e2, $06, $76, $a5
      .db $a3, $8f, $03, $86, $2b, $57, $68, $28, $e9, $28
      .db $e5, $83, $24, $8f, $36, $a8, $5b, $03
      .db $ff

;level 2-3/7-3
E_GroundArea8:
      .db $0f, $02, $78, $40, $48, $ce, $f8, $c3, $f8, $c3
      .db $0f, $07, $7b, $43, $c6, $d0, $0f, $8a, $c8, $50
      .db $ff

;level 2-1
E_GroundArea9:
      .db $85, $86, $0b, $80, $1b, $00, $db, $37, $77, $80
      .db $eb, $37, $fe, $2b, $20, $2b, $80, $7b, $38, $ab, $b8
      .db $77, $86, $fe, $42, $20, $49, $86, $8b, $06, $9b, $80
      .db $7b, $8e, $5b, $b7, $9b, $0e, $bb, $0e, $9b, $80
;end of data terminator here is also used by pipe intro area
E_GroundArea10:
      .db $ff

;level 5-1
E_GroundArea11:
      .db $0b, $80, $60, $38, $10, $b8, $c0, $3b, $db, $8e
      .db $40, $b8, $f0, $38, $7b, $8e, $a0, $b8, $c0, $b8
      .db $fb, $00, $a0, $b8, $30, $bb, $ee, $42, $88, $0f, $0b
      .db $2b, $0e, $67, $0e
      .db $ff

;cloud level used in levels 2-1 and 5-2
E_GroundArea12:
      .db $0a, $aa, $0e, $28, $2a, $0e, $31, $88
      .db $ff

;level 4-3
E_GroundArea13:
      .db $c7, $83, $d7, $03, $42, $8f, $7a, $03, $05, $a4
      .db $78, $24, $a6, $25, $e4, $25, $4b, $83, $e3, $03
      .db $05, $a4, $89, $24, $b5, $24, $09, $a4, $65, $24
      .db $c9, $24, $0f, $08, $85, $25
      .db $ff

;level 6-3
E_GroundArea14:
      .db $cd, $a5, $b5, $a8, $07, $a8, $76, $28, $cc, $25
      .db $65, $a4, $a9, $24, $e5, $24, $19, $a4, $0f, $07
      .db $95, $28, $e6, $24, $19, $a4, $d7, $29, $16, $a9
      .db $58, $29, $97, $29
      .db $ff

;level 6-1
E_GroundArea15:
      .db $0f, $02, $02, $11, $0f, $07, $02, $11
      .db $ff

;warp zone area used in level 4-2
E_GroundArea16:
      .db $ff

;level 8-1
E_GroundArea17:
      .db $2b, $82, $ab, $38, $de, $42, $e2, $1b, $b8, $eb
      .db $3b, $db, $80, $8b, $b8, $1b, $82, $fb, $b8, $7b
      .db $80, $fb, $3c, $5b, $bc, $7b, $b8, $1b, $8e, $cb
      .db $0e, $1b, $8e, $0f, $0d, $2b, $3b, $bb, $b8, $eb, $82
      .db $4b, $b8, $bb, $38, $3b, $b7, $bb, $02, $0f, $13
      .db $1b, $00, $cb, $80, $6b, $bc
      .db $ff

;level 5-2
E_GroundArea18:
      .db $7b, $80, $ae, $00, $80, $8b, $8e, $e8, $05, $f9, $86 
      .db $17, $86, $16, $85, $4e, $2b, $80, $ab, $8e, $87, $85
      .db $c3, $05, $8b, $82, $9b, $02, $ab, $02, $bb, $86
      .db $cb, $06, $d3, $03, $3b, $8e, $6b, $0e, $a7, $8e
      .db $ff

;level 8-2
E_GroundArea19:
      .db $29, $8e, $52, $11, $83, $0e, $0f, $03, $9b, $0e
      .db $2b, $8e, $5b, $0e, $cb, $8e, $fb, $0e, $fb, $82
      .db $9b, $82, $bb, $02, $fe, $42, $e8, $bb, $8e, $0f, $0a
      .db $ab, $0e, $cb, $0e, $f9, $0e, $88, $86, $a6, $06
      .db $db, $02, $b6, $8e
      .db $ff

;level 7-1
E_GroundArea20:
      .db $ab, $ce, $de, $42, $c0, $cb, $ce, $5b, $8e, $1b, $ce
      .db $4b, $85, $67, $45, $0f, $07, $2b, $00, $7b, $85
      .db $97, $05, $0f, $0a, $92, $02
      .db $ff

;cloud level used in levels 3-1 and 6-2
E_GroundArea21:
      .db $0a, $aa, $0e, $24, $4a, $1e, $23, $aa
      .db $ff

;level 3-2
E_GroundArea22:
      .db $1b, $80, $bb, $38, $4b, $bc, $eb, $3b, $0f, $04
      .db $2b, $00, $ab, $38, $eb, $00, $cb, $8e, $fb, $80
      .db $ab, $b8, $6b, $80, $fb, $3c, $9b, $bb, $5b, $bc
      .db $fb, $00, $6b, $b8, $fb, $38
      .db $ff

;level 1-2
E_UndergroundArea1:
      .db $0b, $86, $1a, $06, $db, $06, $de, $c2, $02, $f0, $3b
      .db $bb, $80, $eb, $06, $0b, $86, $93, $06, $f0, $39
      .db $0f, $06, $60, $b8, $1b, $86, $a0, $b9, $b7, $27
      .db $bd, $27, $2b, $83, $a1, $26, $a9, $26, $ee, $25, $0b
      .db $27, $b4
      .db $ff

;level 4-2
E_UndergroundArea2:
      .db $0f, $02, $1e, $2f, $60, $e0, $3a, $a5, $a7, $db, $80
      .db $3b, $82, $8b, $02, $fe, $42, $68, $70, $bb, $25, $a7
      .db $2c, $27, $b2, $26, $b9, $26, $9b, $80, $a8, $82
      .db $b5, $27, $bc, $27, $b0, $bb, $3b, $82, $87, $34
      .db $ee, $25, $6b
      .db $ff

;underground bonus rooms area used in many levels
E_UndergroundArea3:
      .db $1e, $a5, $0a, $2e, $28, $27, $2e, $33, $c7, $0f, $03, $1e, $40, $07
      .db $2e, $30, $e7, $0f, $05, $1e, $24, $44, $0f, $07, $1e, $22, $6a
      .db $2e, $23, $ab, $0f, $09, $1e, $41, $68, $1e, $2a, $8a, $2e, $23, $a2
      .db $2e, $32, $ea
      .db $ff

;water area used in levels 5-2 and 6-2
E_WaterArea1:
      .db $3b, $87, $66, $27, $cc, $27, $ee, $31, $87, $ee, $23, $a7
      .db $3b, $87, $db, $07
      .db $ff

;level 2-2/7-2
E_WaterArea2:
      .db $0f, $01, $2e, $25, $2b, $2e, $25, $4b, $4e, $25, $cb, $6b, $07
      .db $97, $47, $e9, $87, $47, $c7, $7a, $07, $d6, $c7
      .db $78, $07, $38, $87, $ab, $47, $e3, $07, $9b, $87
      .db $0f, $09, $68, $47, $db, $c7, $3b, $c7
      .db $ff

;water area used in level 8-4
E_WaterArea3:
      .db $47, $9b, $cb, $07, $fa, $1d, $86, $9b, $3a, $87
      .db $56, $07, $88, $1b, $07, $9d, $2e, $65, $f0
      .db $ff

;AREA OBJECT DATA

;level 1-4/6-4
L_CastleArea1:
      .db $9b, $07
      .db $05, $32, $06, $33, $07, $34, $ce, $03, $dc, $51
      .db $ee, $07, $73, $e0, $74, $0a, $7e, $06, $9e, $0a
      .db $ce, $06, $e4, $00, $e8, $0a, $fe, $0a, $2e, $89
      .db $4e, $0b, $54, $0a, $14, $8a, $c4, $0a, $34, $8a
      .db $7e, $06, $c7, $0a, $01, $e0, $02, $0a, $47, $0a
      .db $81, $60, $82, $0a, $c7, $0a, $0e, $87, $7e, $02
      .db $a7, $02, $b3, $02, $d7, $02, $e3, $02, $07, $82
      .db $13, $02, $3e, $06, $7e, $02, $ae, $07, $fe, $0a
      .db $0d, $c4, $cd, $43, $ce, $09, $de, $0b, $dd, $42
      .db $fe, $02, $5d, $c7
      .db $fd

;level 4-4
L_CastleArea2:
      .db $5b, $07
      .db $05, $32, $06, $33, $07, $34, $5e, $0a, $68, $64
      .db $98, $64, $a8, $64, $ce, $06, $fe, $02, $0d, $01
      .db $1e, $0e, $7e, $02, $94, $63, $b4, $63, $d4, $63
      .db $f4, $63, $14, $e3, $2e, $0e, $5e, $02, $64, $35
      .db $88, $72, $be, $0e, $0d, $04, $ae, $02, $ce, $08
      .db $cd, $4b, $fe, $02, $0d, $05, $68, $31, $7e, $0a
      .db $96, $31, $a9, $63, $a8, $33, $d5, $30, $ee, $02
      .db $e6, $62, $f4, $61, $04, $b1, $08, $3f, $44, $33
      .db $94, $63, $a4, $31, $e4, $31, $04, $bf, $08, $3f
      .db $04, $bf, $08, $3f, $cd, $4b, $03, $e4, $0e, $03
      .db $2e, $01, $7e, $06, $be, $02, $de, $06, $fe, $0a
      .db $0d, $c4, $cd, $43, $ce, $09, $de, $0b, $dd, $42
      .db $fe, $02, $5d, $c7
      .db $fd

;level 2-4/5-4
L_CastleArea3:
      .db $9b, $07
      .db $05, $32, $06, $33, $07, $34, $fe, $00, $27, $b1
      .db $65, $32, $75, $0a, $71, $00, $b7, $31, $08, $e4
      .db $18, $64, $1e, $04, $57, $3b, $bb, $0a, $17, $8a
      .db $27, $3a, $73, $0a, $7b, $0a, $d7, $0a, $e7, $3a
      .db $3b, $8a, $97, $0a, $fe, $08, $24, $8a, $2e, $00
      .db $3e, $40, $38, $64, $6f, $00, $9f, $00, $be, $43
      .db $c8, $0a, $c9, $63, $ce, $07, $fe, $07, $2e, $81
      .db $66, $42, $6a, $42, $79, $0a, $be, $00, $c8, $64
      .db $f8, $64, $08, $e4, $2e, $07, $7e, $03, $9e, $07
      .db $be, $03, $de, $07, $fe, $0a, $03, $a5, $0d, $44
      .db $cd, $43, $ce, $09, $dd, $42, $de, $0b, $fe, $02
      .db $5d, $c7
      .db $fd

;level 3-4
L_CastleArea4:
      .db $9b, $07
      .db $05, $32, $06, $33, $07, $34, $fe, $06, $0c, $81
      .db $39, $0a, $5c, $01, $89, $0a, $ac, $01, $d9, $0a
      .db $fc, $01, $2e, $83, $a7, $01, $b7, $00, $c7, $01
      .db $de, $0a, $fe, $02, $4e, $83, $5a, $32, $63, $0a
      .db $69, $0a, $7e, $02, $ee, $03, $fa, $32, $03, $8a
      .db $09, $0a, $1e, $02, $ee, $03, $fa, $32, $03, $8a
      .db $09, $0a, $14, $42, $1e, $02, $7e, $0a, $9e, $07
      .db $fe, $0a, $2e, $86, $5e, $0a, $8e, $06, $be, $0a
      .db $ee, $07, $3e, $83, $5e, $07, $fe, $0a, $0d, $c4
      .db $41, $52, $51, $52, $cd, $43, $ce, $09, $de, $0b
      .db $dd, $42, $fe, $02, $5d, $c7
      .db $fd

;level 7-4
L_CastleArea5:
      .db $5b, $07
      .db $05, $32, $06, $33, $07, $34, $fe, $0a, $ae, $86
      .db $be, $07, $fe, $02, $0d, $02, $27, $32, $46, $61
      .db $55, $62, $5e, $0e, $1e, $82, $68, $3c, $74, $3a
      .db $7d, $4b, $5e, $8e, $7d, $4b, $7e, $82, $84, $62
      .db $94, $61, $a4, $31, $bd, $4b, $ce, $06, $fe, $02
      .db $0d, $06, $34, $31, $3e, $0a, $64, $32, $75, $0a
      .db $7b, $61, $a4, $33, $ae, $02, $de, $0e, $3e, $82
      .db $64, $32, $78, $32, $b4, $36, $c8, $36, $dd, $4b
      .db $44, $b2, $58, $32, $94, $63, $a4, $3e, $ba, $30
      .db $c9, $61, $ce, $06, $dd, $4b, $ce, $86, $dd, $4b
      .db $fe, $02, $2e, $86, $5e, $02, $7e, $06, $fe, $02
      .db $1e, $86, $3e, $02, $5e, $06, $7e, $02, $9e, $06
      .db $fe, $0a, $0d, $c4, $cd, $43, $ce, $09, $de, $0b
      .db $dd, $42, $fe, $02, $5d, $c7
      .db $fd

;level 8-4
L_CastleArea6:
      .db $5b, $06
      .db $05, $32, $06, $33, $07, $34, $5e, $0a, $ae, $02
      .db $0d, $01, $39, $73, $0d, $03, $39, $7b, $4d, $4b
      .db $de, $06, $1e, $8a, $ae, $06, $c4, $33, $16, $fe
      .db $a5, $77, $fe, $02, $fe, $82, $0d, $07, $39, $73
      .db $a8, $74, $ed, $4b, $49, $fb, $e8, $74, $fe, $0a
      .db $2e, $82, $67, $02, $84, $7a, $87, $31, $0d, $0b
      .db $fe, $02, $0d, $0c, $39, $73, $5e, $06, $c6, $76
      .db $45, $ff, $be, $0a, $dd, $48, $fe, $06, $3d, $cb
      .db $46, $7e, $ad, $4a, $fe, $82, $39, $f3, $a9, $7b
      .db $4e, $8a, $9e, $07, $fe, $0a, $0d, $c4, $cd, $43
      .db $ce, $09, $de, $0b, $dd, $42, $fe, $02, $5d, $c7
      .db $fd

;level 3-3
L_GroundArea1:
      .db $94, $11
      .db $0f, $26, $fe, $10, $28, $94, $65, $15, $eb, $12
      .db $fa, $41, $4a, $96, $54, $40, $a4, $42, $b7, $13
      .db $e9, $19, $f5, $15, $11, $80, $47, $42, $71, $13
      .db $80, $41, $15, $92, $1b, $1f, $24, $40, $55, $12
      .db $64, $40, $95, $12, $a4, $40, $d2, $12, $e1, $40
      .db $13, $c0, $2c, $17, $2f, $12, $49, $13, $83, $40
      .db $9f, $14, $a3, $40, $17, $92, $83, $13, $92, $41
      .db $b9, $14, $c5, $12, $c8, $40, $d4, $40, $4b, $92
      .db $78, $1b, $9c, $94, $9f, $11, $df, $14, $fe, $11
      .db $7d, $c1, $9e, $42, $cf, $20
      .db $fd

;level 8-3
L_GroundArea2:
      .db $90, $b1
      .db $0f, $26, $29, $91, $7e, $42, $fe, $40, $28, $92
      .db $4e, $42, $2e, $c0, $57, $73, $c3, $25, $c7, $27
      .db $23, $84, $33, $20, $5c, $01, $77, $63, $88, $62
      .db $99, $61, $aa, $60, $bc, $01, $ee, $42, $4e, $c0
      .db $69, $11, $7e, $42, $de, $40, $f8, $62, $0e, $c2
      .db $ae, $40, $d7, $63, $e7, $63, $33, $a7, $37, $27
      .db $43, $04, $cc, $01, $e7, $73, $0c, $81, $3e, $42
      .db $0d, $0a, $5e, $40, $88, $72, $be, $42, $e7, $87
      .db $fe, $40, $39, $e1, $4e, $00, $69, $60, $87, $60
      .db $a5, $60, $c3, $31, $fe, $31, $6d, $c1, $be, $42
      .db $ef, $20
      .db $fd

;level 4-1
L_GroundArea3:
      .db $52, $21
      .db $0f, $20, $6e, $40, $58, $f2, $93, $01, $97, $00
      .db $0c, $81, $97, $40, $a6, $41, $c7, $40, $0d, $04
      .db $03, $01, $07, $01, $23, $01, $27, $01, $ec, $03
      .db $ac, $f3, $c3, $03, $78, $e2, $94, $43, $47, $f3
      .db $74, $43, $47, $fb, $74, $43, $2c, $f1, $4c, $63
      .db $47, $00, $57, $21, $5c, $01, $7c, $72, $39, $f1
      .db $ec, $02, $4c, $81, $d8, $62, $ec, $01, $0d, $0d
      .db $0f, $38, $c7, $07, $ed, $4a, $1d, $c1, $5f, $26
      .db $fd

;level 6-2
L_GroundArea4:
      .db $54, $21
      .db $0f, $26, $a7, $22, $37, $fb, $73, $20, $83, $07
      .db $87, $02, $93, $20, $c7, $73, $04, $f1, $06, $31
      .db $39, $71, $59, $71, $e7, $73, $37, $a0, $47, $04
      .db $86, $7c, $e5, $71, $e7, $31, $33, $a4, $39, $71
      .db $a9, $71, $d3, $23, $08, $f2, $13, $05, $27, $02
      .db $49, $71, $75, $75, $e8, $72, $67, $f3, $99, $71
      .db $e7, $20, $f4, $72, $f7, $31, $17, $a0, $33, $20
      .db $39, $71, $73, $28, $bc, $05, $39, $f1, $79, $71
      .db $a6, $21, $c3, $06, $d3, $20, $dc, $00, $fc, $00
      .db $07, $a2, $13, $21, $5f, $32, $8c, $00, $98, $7a
      .db $c7, $63, $d9, $61, $03, $a2, $07, $22, $74, $72
      .db $77, $31, $e7, $73, $39, $f1, $58, $72, $77, $73
      .db $d8, $72, $7f, $b1, $97, $73, $b6, $64, $c5, $65
      .db $d4, $66, $e3, $67, $f3, $67, $8d, $c1, $cf, $26
      .db $fd

;level 3-1
L_GroundArea5:
      .db $52, $31
      .db $0f, $20, $6e, $66, $07, $81, $36, $01, $66, $00
      .db $a7, $22, $08, $f2, $67, $7b, $dc, $02, $98, $f2
      .db $d7, $20, $39, $f1, $9f, $33, $dc, $27, $dc, $57
      .db $23, $83, $57, $63, $6c, $51, $87, $63, $99, $61
      .db $a3, $06, $b3, $21, $77, $f3, $f3, $21, $f7, $2a
      .db $13, $81, $23, $22, $53, $00, $63, $22, $e9, $0b
      .db $0c, $83, $13, $21, $16, $22, $33, $05, $8f, $35
      .db $ec, $01, $63, $a0, $67, $20, $73, $01, $77, $01
      .db $83, $20, $87, $20, $b3, $20, $b7, $20, $c3, $01
      .db $c7, $00, $d3, $20, $d7, $20, $67, $a0, $77, $07
      .db $87, $22, $e8, $62, $f5, $65, $1c, $82, $7f, $38
      .db $8d, $c1, $cf, $26
      .db $fd

;level 1-1
L_GroundArea6:
      .db $50, $21
      .db $07, $81, $47, $24, $57, $00, $63, $01, $77, $01
      .db $c9, $71, $68, $f2, $e7, $73, $97, $fb, $06, $83
      .db $5c, $01, $d7, $22, $e7, $00, $03, $a7, $6c, $02
      .db $b3, $22, $e3, $01, $e7, $07, $47, $a0, $57, $06
      .db $a7, $01, $d3, $00, $d7, $01, $07, $81, $67, $20
      .db $93, $22, $03, $a3, $1c, $61, $17, $21, $6f, $33
      .db $c7, $63, $d8, $62, $e9, $61, $fa, $60, $4f, $b3
      .db $87, $63, $9c, $01, $b7, $63, $c8, $62, $d9, $61
      .db $ea, $60, $39, $f1, $87, $21, $a7, $01, $b7, $20
      .db $39, $f1, $5f, $38, $6d, $c1, $af, $26
      .db $fd

;level 1-3/5-3
L_GroundArea7:
      .db $90, $11
      .db $0f, $26, $fe, $10, $2a, $93, $87, $17, $a3, $14
      .db $b2, $42, $0a, $92, $19, $40, $36, $14, $50, $41
      .db $82, $16, $2b, $93, $24, $41, $bb, $14, $b8, $00
      .db $c2, $43, $c3, $13, $1b, $94, $67, $12, $c4, $15
      .db $53, $c1, $d2, $41, $12, $c1, $29, $13, $85, $17
      .db $1b, $92, $1a, $42, $47, $13, $83, $41, $a7, $13
      .db $0e, $91, $a7, $63, $b7, $63, $c5, $65, $d5, $65
      .db $dd, $4a, $e3, $67, $f3, $67, $8d, $c1, $ae, $42
      .db $df, $20
      .db $fd

;level 2-3/7-3
L_GroundArea8:
      .db $90, $11
      .db $0f, $26, $6e, $10, $8b, $17, $af, $32, $d8, $62
      .db $e8, $62, $fc, $3f, $ad, $c8, $f8, $64, $0c, $be
      .db $43, $43, $f8, $64, $0c, $bf, $73, $40, $84, $40
      .db $93, $40, $a4, $40, $b3, $40, $f8, $64, $48, $e4
      .db $5c, $39, $83, $40, $92, $41, $b3, $40, $f8, $64
      .db $48, $e4, $5c, $39, $f8, $64, $13, $c2, $37, $65
      .db $4c, $24, $63, $00, $97, $65, $c3, $42, $0b, $97
      .db $ac, $32, $f8, $64, $0c, $be, $53, $45, $9d, $48
      .db $f8, $64, $2a, $e2, $3c, $47, $56, $43, $ba, $62
      .db $f8, $64, $0c, $b7, $88, $64, $bc, $31, $d4, $45
      .db $fc, $31, $3c, $b1, $78, $64, $8c, $38, $0b, $9c
      .db $1a, $33, $18, $61, $28, $61, $39, $60, $5d, $4a
      .db $ee, $11, $0f, $b8, $1d, $c1, $3e, $42, $6f, $20
      .db $fd

;level 2-1
L_GroundArea9:
      .db $52, $31
      .db $0f, $20, $6e, $40, $f7, $20, $07, $84, $17, $20
      .db $4f, $34, $c3, $03, $c7, $02, $d3, $22, $27, $e3
      .db $39, $61, $e7, $73, $5c, $e4, $57, $00, $6c, $73
      .db $47, $a0, $53, $06, $63, $22, $a7, $73, $fc, $73
      .db $13, $a1, $33, $05, $43, $21, $5c, $72, $c3, $23
      .db $cc, $03, $77, $fb, $ac, $02, $39, $f1, $a7, $73
      .db $d3, $04, $e8, $72, $e3, $22, $26, $f4, $bc, $02
      .db $8c, $81, $a8, $62, $17, $87, $43, $24, $a7, $01
      .db $c3, $04, $08, $f2, $97, $21, $a3, $02, $c9, $0b
      .db $e1, $69, $f1, $69, $8d, $c1, $cf, $26
      .db $fd

;pipe intro area
L_GroundArea10:
      .db $38, $11
      .db $0f, $26, $ad, $40, $3d, $c7
      .db $fd

;level 5-1
L_GroundArea11:
      .db $95, $b1
      .db $0f, $26, $0d, $02, $c8, $72, $1c, $81, $38, $72
      .db $0d, $05, $97, $34, $98, $62, $a3, $20, $b3, $06
      .db $c3, $20, $cc, $03, $f9, $91, $2c, $81, $48, $62
      .db $0d, $09, $37, $63, $47, $03, $57, $21, $8c, $02
      .db $c5, $79, $c7, $31, $f9, $11, $39, $f1, $a9, $11
      .db $6f, $b4, $d3, $65, $e3, $65, $7d, $c1, $bf, $26
      .db $fd

;cloud level used in levels 2-1 and 5-2
L_GroundArea12:
      .db $00, $c1
      .db $4c, $00, $f4, $4f, $0d, $02, $02, $42, $43, $4f
      .db $52, $c2, $de, $00, $5a, $c2, $4d, $c7
      .db $fd

;level 4-3
L_GroundArea13:
      .db $90, $51
      .db $0f, $26, $ee, $10, $0b, $94, $33, $14, $42, $42
      .db $77, $16, $86, $44, $02, $92, $4a, $16, $69, $42
      .db $73, $14, $b0, $00, $c7, $12, $05, $c0, $1c, $17
      .db $1f, $11, $36, $12, $8f, $14, $91, $40, $1b, $94
      .db $35, $12, $34, $42, $60, $42, $61, $12, $87, $12
      .db $96, $40, $a3, $14, $1c, $98, $1f, $11, $47, $12
      .db $9f, $15, $cc, $15, $cf, $11, $05, $c0, $1f, $15
      .db $39, $12, $7c, $16, $7f, $11, $82, $40, $98, $12
      .db $df, $15, $16, $c4, $17, $14, $54, $12, $9b, $16
      .db $28, $94, $ce, $01, $3d, $c1, $5e, $42, $8f, $20
      .db $fd

;level 6-3
L_GroundArea14:
      .db $97, $11
      .db $0f, $26, $fe, $10, $2b, $92, $57, $12, $8b, $12
      .db $c0, $41, $f7, $13, $5b, $92, $69, $0b, $bb, $12
      .db $b2, $46, $19, $93, $71, $00, $17, $94, $7c, $14
      .db $7f, $11, $93, $41, $bf, $15, $fc, $13, $ff, $11
      .db $2f, $95, $50, $42, $51, $12, $58, $14, $a6, $12
      .db $db, $12, $1b, $93, $46, $43, $7b, $12, $8d, $49
      .db $b7, $14, $1b, $94, $49, $0b, $bb, $12, $fc, $13
      .db $ff, $12, $03, $c1, $2f, $15, $43, $12, $4b, $13
      .db $77, $13, $9d, $4a, $15, $c1, $a1, $41, $c3, $12
      .db $fe, $01, $7d, $c1, $9e, $42, $cf, $20
      .db $fd

;level 6-1
L_GroundArea15:
      .db $52, $21
      .db $0f, $20, $6e, $44, $0c, $f1, $4c, $01, $aa, $35
      .db $d9, $34, $ee, $20, $08, $b3, $37, $32, $43, $04
      .db $4e, $21, $53, $20, $7c, $01, $97, $21, $b7, $07
      .db $9c, $81, $e7, $42, $5f, $b3, $97, $63, $ac, $02
      .db $c5, $41, $49, $e0, $58, $61, $76, $64, $85, $65
      .db $94, $66, $a4, $22, $a6, $03, $c8, $22, $dc, $02
      .db $68, $f2, $96, $42, $13, $82, $17, $02, $af, $34
      .db $f6, $21, $fc, $06, $26, $80, $2a, $24, $36, $01
      .db $8c, $00, $ff, $35, $4e, $a0, $55, $21, $77, $20
      .db $87, $07, $89, $22, $ae, $21, $4c, $82, $9f, $34
      .db $ec, $01, $03, $e7, $13, $67, $8d, $4a, $ad, $41
      .db $0f, $a6
      .db $fd

;warp zone area used in level 4-2
L_GroundArea16:
      .db $10, $51
      .db $4c, $00, $c7, $12, $c6, $42, $03, $92, $02, $42
      .db $29, $12, $63, $12, $62, $42, $69, $14, $a5, $12
      .db $a4, $42, $e2, $14, $e1, $44, $f8, $16, $37, $c1
      .db $8f, $38, $02, $bb, $28, $7a, $68, $7a, $a8, $7a
      .db $e0, $6a, $f0, $6a, $6d, $c5
      .db $fd

;level 8-1
L_GroundArea17:
      .db $92, $31
      .db $0f, $20, $6e, $40, $0d, $02, $37, $73, $ec, $00
      .db $0c, $80, $3c, $00, $6c, $00, $9c, $00, $06, $c0
      .db $c7, $73, $06, $83, $28, $72, $96, $40, $e7, $73
      .db $26, $c0, $87, $7b, $d2, $41, $39, $f1, $c8, $f2
      .db $97, $e3, $a3, $23, $e7, $02, $e3, $07, $f3, $22
      .db $37, $e3, $9c, $00, $bc, $00, $ec, $00, $0c, $80
      .db $3c, $00, $86, $21, $a6, $06, $b6, $24, $5c, $80
      .db $7c, $00, $9c, $00, $29, $e1, $dc, $05, $f6, $41
      .db $dc, $80, $e8, $72, $0c, $81, $27, $73, $4c, $01
      .db $66, $74, $0d, $11, $3f, $35, $b6, $41, $2c, $82
      .db $36, $40, $7c, $02, $86, $40, $f9, $61, $39, $e1
      .db $ac, $04, $c6, $41, $0c, $83, $16, $41, $88, $f2
      .db $39, $f1, $7c, $00, $89, $61, $9c, $00, $a7, $63
      .db $bc, $00, $c5, $65, $dc, $00, $e3, $67, $f3, $67
      .db $8d, $c1, $cf, $26
      .db $fd

;level 5-2
L_GroundArea18:
      .db $55, $b1
      .db $0f, $26, $cf, $33, $07, $b2, $15, $11, $52, $42
      .db $99, $0b, $ac, $02, $d3, $24, $d6, $42, $d7, $25
      .db $23, $84, $cf, $33, $07, $e3, $19, $61, $78, $7a
      .db $ef, $33, $2c, $81, $46, $64, $55, $65, $65, $65
      .db $ec, $74, $47, $82, $53, $05, $63, $21, $62, $41
      .db $96, $22, $9a, $41, $cc, $03, $b9, $91, $39, $f1
      .db $63, $26, $67, $27, $d3, $06, $fc, $01, $18, $e2
      .db $d9, $07, $e9, $04, $0c, $86, $37, $22, $93, $24
      .db $87, $84, $ac, $02, $c2, $41, $c3, $23, $d9, $71
      .db $fc, $01, $7f, $b1, $9c, $00, $a7, $63, $b6, $64
      .db $cc, $00, $d4, $66, $e3, $67, $f3, $67, $8d, $c1
      .db $cf, $26
      .db $fd

;level 8-2
L_GroundArea19:
      .db $50, $b1
      .db $0f, $26, $fc, $00, $1f, $b3, $5c, $00, $65, $65
      .db $74, $66, $83, $67, $93, $67, $dc, $73, $4c, $80
      .db $b3, $20, $c9, $0b, $c3, $08, $d3, $2f, $dc, $00
      .db $2c, $80, $4c, $00, $8c, $00, $d3, $2e, $ed, $4a
      .db $fc, $00, $d7, $a1, $ec, $01, $4c, $80, $59, $11
      .db $d8, $11, $da, $10, $37, $a0, $47, $04, $99, $11
      .db $e7, $21, $3a, $90, $67, $20, $76, $10, $77, $60
      .db $87, $07, $d8, $12, $39, $f1, $ac, $00, $e9, $71
      .db $0c, $80, $2c, $00, $4c, $05, $c7, $7b, $39, $f1
      .db $ec, $00, $f9, $11, $0c, $82, $6f, $34, $f8, $11
      .db $fa, $10, $7f, $b2, $ac, $00, $b6, $64, $cc, $01
      .db $e3, $67, $f3, $67, $8d, $c1, $cf, $26
      .db $fd

;level 7-1
L_GroundArea20:
      .db $52, $b1
      .db $0f, $20, $6e, $45, $39, $91, $b3, $04, $c3, $21
      .db $c8, $11, $ca, $10, $49, $91, $7c, $73, $e8, $12
      .db $88, $91, $8a, $10, $e7, $21, $05, $91, $07, $30
      .db $17, $07, $27, $20, $49, $11, $9c, $01, $c8, $72
      .db $23, $a6, $27, $26, $d3, $03, $d8, $7a, $89, $91
      .db $d8, $72, $39, $f1, $a9, $11, $09, $f1, $63, $24
      .db $67, $24, $d8, $62, $28, $91, $2a, $10, $56, $21
      .db $70, $04, $79, $0b, $8c, $00, $94, $21, $9f, $35
      .db $2f, $b8, $3d, $c1, $7f, $26
      .db $fd

;cloud level used in levels 3-1 and 6-2
L_GroundArea21:
      .db $06, $c1
      .db $4c, $00, $f4, $4f, $0d, $02, $06, $20, $24, $4f
      .db $35, $a0, $36, $20, $53, $46, $d5, $20, $d6, $20
      .db $34, $a1, $73, $49, $74, $20, $94, $20, $b4, $20
      .db $d4, $20, $f4, $20, $2e, $80, $59, $42, $4d, $c7
      .db $fd

;level 3-2
L_GroundArea22:
      .db $96, $31
      .db $0f, $26, $0d, $03, $1a, $60, $77, $42, $c4, $00
      .db $c8, $62, $b9, $e1, $d3, $06, $d7, $07, $f9, $61
      .db $0c, $81, $4e, $b1, $8e, $b1, $bc, $01, $e4, $50
      .db $e9, $61, $0c, $81, $0d, $0a, $84, $43, $98, $72
      .db $0d, $0c, $0f, $38, $1d, $c1, $5f, $26
      .db $fd

;level 1-2
L_UndergroundArea1:
      .db $48, $0f
      .db $0e, $01, $5e, $02, $a7, $00, $bc, $73, $1a, $e0
      .db $39, $61, $58, $62, $77, $63, $97, $63, $b8, $62
      .db $d6, $07, $f8, $62, $19, $e1, $75, $52, $86, $40
      .db $87, $50, $95, $52, $93, $43, $a5, $21, $c5, $52
      .db $d6, $40, $d7, $20, $e5, $06, $e6, $51, $3e, $8d
      .db $5e, $03, $67, $52, $77, $52, $7e, $02, $9e, $03
      .db $a6, $43, $a7, $23, $de, $05, $fe, $02, $1e, $83
      .db $33, $54, $46, $40, $47, $21, $56, $04, $5e, $02
      .db $83, $54, $93, $52, $96, $07, $97, $50, $be, $03
      .db $c7, $23, $fe, $02, $0c, $82, $43, $45, $45, $24
      .db $46, $24, $90, $08, $95, $51, $78, $fa, $d7, $73
      .db $39, $f1, $8c, $01, $a8, $52, $b8, $52, $cc, $01
      .db $5f, $b3, $97, $63, $9e, $00, $0e, $81, $16, $24
      .db $66, $04, $8e, $00, $fe, $01, $08, $d2, $0e, $06
      .db $6f, $47, $9e, $0f, $0e, $82, $2d, $47, $28, $7a
      .db $68, $7a, $a8, $7a, $ae, $01, $de, $0f, $6d, $c5
      .db $fd

;level 4-2
L_UndergroundArea2:
      .db $48, $0f
      .db $0e, $01, $5e, $02, $bc, $01, $fc, $01, $2c, $82
      .db $41, $52, $4e, $04, $67, $25, $68, $24, $69, $24
      .db $ba, $42, $c7, $04, $de, $0b, $b2, $87, $fe, $02
      .db $2c, $e1, $2c, $71, $67, $01, $77, $00, $87, $01
      .db $8e, $00, $ee, $01, $f6, $02, $03, $85, $05, $02
      .db $13, $21, $16, $02, $27, $02, $2e, $02, $88, $72
      .db $c7, $20, $d7, $07, $e4, $76, $07, $a0, $17, $06
      .db $48, $7a, $76, $20, $98, $72, $79, $e1, $88, $62
      .db $9c, $01, $b7, $73, $dc, $01, $f8, $62, $fe, $01
      .db $08, $e2, $0e, $00, $6e, $02, $73, $20, $77, $23
      .db $83, $04, $93, $20, $ae, $00, $fe, $0a, $0e, $82
      .db $39, $71, $a8, $72, $e7, $73, $0c, $81, $8f, $32
      .db $ae, $00, $fe, $04, $04, $d1, $17, $04, $26, $49
      .db $27, $29, $df, $33, $fe, $02, $44, $f6, $7c, $01
      .db $8e, $06, $bf, $47, $ee, $0f, $4d, $c7, $0e, $82
      .db $68, $7a, $ae, $01, $de, $0f, $6d, $c5
      .db $fd

;underground bonus rooms area used in many levels
L_UndergroundArea3:
      .db $48, $01
      .db $0e, $01, $00, $5a, $3e, $06, $45, $46, $47, $46
      .db $53, $44, $ae, $01, $df, $4a, $4d, $c7, $0e, $81
      .db $00, $5a, $2e, $04, $37, $28, $3a, $48, $46, $47
      .db $c7, $07, $ce, $0f, $df, $4a, $4d, $c7, $0e, $81
      .db $00, $5a, $33, $53, $43, $51, $46, $40, $47, $50
      .db $53, $04, $55, $40, $56, $50, $62, $43, $64, $40
      .db $65, $50, $71, $41, $73, $51, $83, $51, $94, $40
      .db $95, $50, $a3, $50, $a5, $40, $a6, $50, $b3, $51
      .db $b6, $40, $b7, $50, $c3, $53, $df, $4a, $4d, $c7
      .db $0e, $81, $00, $5a, $2e, $02, $36, $47, $37, $52
      .db $3a, $49, $47, $25, $a7, $52, $d7, $04, $df, $4a
      .db $4d, $c7, $0e, $81, $00, $5a, $3e, $02, $44, $51
      .db $53, $44, $54, $44, $55, $24, $a1, $54, $ae, $01
      .db $b4, $21, $df, $4a, $e5, $07, $4d, $c7
      .db $fd

;water area used in levels 5-2 and 6-2
L_WaterArea1:
      .db $41, $01
      .db $b4, $34, $c8, $52, $f2, $51, $47, $d3, $6c, $03
      .db $65, $49, $9e, $07, $be, $01, $cc, $03, $fe, $07
      .db $0d, $c9, $1e, $01, $6c, $01, $62, $35, $63, $53
      .db $8a, $41, $ac, $01, $b3, $53, $e9, $51, $26, $c3
      .db $27, $33, $63, $43, $64, $33, $ba, $60, $c9, $61
      .db $ce, $0b, $e5, $09, $ee, $0f, $7d, $ca, $7d, $47
      .db $fd

;level 2-2/7-2
L_WaterArea2:
      .db $41, $01
      .db $b8, $52, $ea, $41, $27, $b2, $b3, $42, $16, $d4
      .db $4a, $42, $a5, $51, $a7, $31, $27, $d3, $08, $e2
      .db $16, $64, $2c, $04, $38, $42, $76, $64, $88, $62
      .db $de, $07, $fe, $01, $0d, $c9, $23, $32, $31, $51
      .db $98, $52, $0d, $c9, $59, $42, $63, $53, $67, $31
      .db $14, $c2, $36, $31, $87, $53, $17, $e3, $29, $61
      .db $30, $62, $3c, $08, $42, $37, $59, $40, $6a, $42
      .db $99, $40, $c9, $61, $d7, $63, $39, $d1, $58, $52
      .db $c3, $67, $d3, $31, $dc, $06, $f7, $42, $fa, $42
      .db $23, $b1, $43, $67, $c3, $34, $c7, $34, $d1, $51
      .db $43, $b3, $47, $33, $9a, $30, $a9, $61, $b8, $62
      .db $be, $0b, $d5, $09, $de, $0f, $0d, $ca, $7d, $47
      .db $fd

;water area used in level 8-4
L_WaterArea3:
      .db $49, $0f
      .db $1e, $01, $39, $73, $5e, $07, $ae, $0b, $1e, $82
      .db $6e, $88, $9e, $02, $0d, $04, $2e, $0b, $45, $09
      .db $4e, $0f, $ed, $47
      .db $fd

;-------------------------------------------------------------------------------------

;unused space
      .db $ff

;-------------------------------------------------------------------------------------

;indirect jump routine called when
;$0770 is set to 1
GameMode:
      lda OperMode_Task
      jsr JumpEngine

      .dw InitializeArea
      .dw ScreenRoutines
      .dw SecondaryGameSetup
      .dw GameCoreRoutine

;-------------------------------------------------------------------------------------

GameCoreRoutine:
      jsr GameRoutines           ;execute one of many possible subs
      lda OperMode_Task          ;check major task of operating mode
      cmp #$03                   ;if we are supposed to be here,
      bcs GameEngine             ;branch to the game engine itself
      rts

GameEngine:
              jsr ProcFireball_Bubble    ;process fireballs and air bubbles
              ldx #$00
ProcELoop:    stx ObjectOffset           ;put incremented offset in X as enemy object offset
              jsr EnemiesAndLoopsCore    ;process enemy objects
              jsr FloateyNumbersRoutine  ;process floatey numbers
              inx
              cpx #$06                   ;do these two subroutines until the whole buffer is done
              bne ProcELoop
              jsr GetPlayerOffscreenBits ;get offscreen bits for player object
              jsr RelativePlayerPosition ;get relative coordinates for player object
              jsr PlayerGfxHandler       ;draw the player
              jsr BlockObjMT_Updater     ;replace block objects with metatiles if necessary
              ldx #$01
              stx ObjectOffset           ;set offset for second
              jsr BlockObjectsCore       ;process second block object
              dex
              stx ObjectOffset           ;set offset for first
              jsr BlockObjectsCore       ;process first block object
              jsr MiscObjectsCore        ;process misc objects (hammer, jumping coins)
              jsr ProcessCannons         ;process bullet bill cannons
              jsr ProcessWhirlpools      ;process whirlpools
              jsr FlagpoleRoutine        ;process the flagpole
              jsr RunGameTimer           ;count down the game timer
              lda VRAM_Buffer1_Offset
              bne DontRedrawPosition
              jsr RedrawPosition
DontRedrawPosition:
              jsr ColorRotation          ;cycle one of the background colors
              lda Player_Y_HighPos
              cmp #$02                   ;if player is below the screen, don't bother with the music
              bpl NoChgMus
              lda StarInvincibleTimer    ;if star mario invincibility timer at zero,
              beq ClrPlrPal              ;skip this part
              cmp #$04
              bne NoChgMus               ;if not yet at a certain point, continue
              lda IntervalTimerControl   ;if interval timer not yet expired,
              bne NoChgMus               ;branch ahead, don't bother with the music
              jsr GetAreaMusic           ;to re-attain appropriate level music
NoChgMus:     ldy StarInvincibleTimer    ;get invincibility timer
              lda FrameCounter           ;get frame counter
              cpy #$08                   ;if timer still above certain point,
              bcs CycleTwo               ;branch to cycle player's palette quickly
              lsr                        ;otherwise, divide by 8 to cycle every eighth frame
              lsr
CycleTwo:     lsr                        ;if branched here, divide by 2 to cycle every other frame
              jsr CyclePlayerPalette     ;do sub to cycle the palette (note: shares fire flower code)
              jmp SaveAB                 ;then skip this sub to finish up the game engine
ClrPlrPal:    jsr ResetPalStar           ;do sub to clear player's palette bits in attributes
SaveAB:       lda A_B_Buttons            ;save current A and B button
              sta PreviousA_B_Buttons    ;into temp variable to be used on next frame
              lda #$00
              sta Left_Right_Buttons     ;nullify left and right buttons temp variable
UpdScrollVar: lda VRAM_Buffer_AddrCtrl
              cmp #$06                   ;if vram address controller set to 6 (one of two $0341s)
              beq ExitEng                ;then branch to leave
              lda AreaParserTaskNum      ;otherwise check number of tasks
              bne RunParser
              lda ScrollThirtyTwo        ;get horizontal scroll in 0-31 or $00-$20 range
              cmp #$20                   ;check to see if exceeded $21
              bmi ExitEng                ;branch to leave if not
              lda ScrollThirtyTwo
              sbc #$20                   ;otherwise subtract $20 to set appropriately
              sta ScrollThirtyTwo        ;and store
              lda #$00                   ;reset vram buffer offset used in conjunction with
              sta VRAM_Buffer2_Offset    ;level graphics buffer at $0341-$035f
RunParser:    jsr AreaParserTaskHandler  ;update the name table with more level graphics
ExitEng:      rts                        ;and after all that, we're finally done!

;-------------------------------------------------------------------------------------

ScrollHandler:
			jsr UpdateSockHash
            lda Player_X_Scroll       ;load value saved here
            clc
            adc Platform_X_Scroll     ;add value used by left/right platforms
            sta Player_X_Scroll       ;save as new value here to impose force on scroll
            lda ScrollLock            ;check scroll lock flag
            bne InitScrlAmt           ;skip a bunch of code here if set
            lda Player_Pos_ForScroll
            cmp #$50                  ;check player's horizontal screen position
            bcc InitScrlAmt           ;if less than 80 pixels to the right, branch
            lda SideCollisionTimer    ;if timer related to player's side collision
            bne InitScrlAmt           ;not expired, branch
            ldy Player_X_Scroll       ;get value and decrement by one
            dey                       ;if value originally set to zero or otherwise
            bmi InitScrlAmt           ;negative for left movement, branch
            iny
            cpy #$02                  ;if value $01, branch and do not decrement
            bcc ChkNearMid
            dey                       ;otherwise decrement by one
ChkNearMid: lda Player_Pos_ForScroll
            cmp #$70                  ;check player's horizontal screen position
            bcc ScrollScreen          ;if less than 112 pixels to the right, branch
            ldy Player_X_Scroll       ;otherwise get original value undecremented

ScrollScreen:
              tya
              sta ScrollAmount          ;save value here
              clc
              adc ScrollThirtyTwo       ;add to value already set here
              sta ScrollThirtyTwo       ;save as new value here
              tya
              clc
              adc ScreenLeft_X_Pos      ;add to left side coordinate
              sta ScreenLeft_X_Pos      ;save as new left side coordinate
              sta HorizontalScroll      ;save here also
              lda ScreenLeft_PageLoc
              adc #$00                  ;add carry to page location for left
              sta ScreenLeft_PageLoc    ;side of the screen
              and #$01                  ;get LSB of page location
              sta $00                   ;save as temp variable for PPU register 1 mirror
              lda Mirror_PPU_CTRL_REG1  ;get PPU register 1 mirror
              and #%11111110            ;save all bits except d0
              ora $00                   ;get saved bit here and save in PPU register 1
              sta Mirror_PPU_CTRL_REG1  ;mirror to be used to set name table later
              jsr GetScreenPosition     ;figure out where the right side is
              lda #$08
              sta ScrollIntervalTimer   ;set scroll timer (residual, not used elsewhere)
              jmp ChkPOffscr            ;skip this part
InitScrlAmt:  lda #$00
              sta ScrollAmount          ;initialize value here
ChkPOffscr:   ldx #$00                  ;set X for player offset
              jsr GetXOffscreenBits     ;get horizontal offscreen bits for player
              sta $00                   ;save them here
              ldy #$00                  ;load default offset (left side)
              asl                       ;if d7 of offscreen bits are set,
              bcs KeepOnscr             ;branch with default offset
              iny                         ;otherwise use different offset (right side)
              lda $00
              and #%00100000              ;check offscreen bits for d5 set
              beq InitPlatScrl            ;if not set, branch ahead of this part
KeepOnscr:    lda ScreenEdge_X_Pos,y      ;get left or right side coordinate based on offset
              sec
              sbc X_SubtracterData,y      ;subtract amount based on offset
              sta Player_X_Position       ;store as player position to prevent movement further
              lda ScreenEdge_PageLoc,y    ;get left or right page location based on offset
              sbc #$00                    ;subtract borrow
              sta Player_PageLoc          ;save as player's page location
              lda Left_Right_Buttons      ;check saved controller bits
              cmp OffscrJoypadBitsData,y  ;against bits based on offset
              beq InitPlatScrl            ;if not equal, branch
              lda #$00
              sta Player_X_Speed          ;otherwise nullify horizontal speed of player
InitPlatScrl: lda #$00                    ;nullify platform force imposed on scroll
              sta Platform_X_Scroll
              rts

X_SubtracterData:
      .db $00, $10

OffscrJoypadBitsData:
      .db $01, $02

;-------------------------------------------------------------------------------------

GameRoutines:
      lda GameEngineSubroutine  ;run routine based on number (a few of these routines are   
      jsr JumpEngine            ;merely placeholders as conditions for other routines)

      .dw Entrance_GameTimerSetup
      .dw Vine_AutoClimb
      .dw SideExitPipeEntry
      .dw VerticalPipeEntry
      .dw FlagpoleSlide
      .dw PlayerEndLevel
      .dw PlayerLoseLife
      .dw PlayerEntrance
      .dw PlayerCtrlRoutine
      .dw PlayerChangeSize
      .dw PlayerInjuryBlink
      .dw PlayerDeath
      .dw PlayerFireFlower

;-------------------------------------------------------------------------------------

PlayerEntrance:
            lda AltEntranceControl    ;check for mode of alternate entry
            cmp #$02
            beq EntrMode2             ;if found, branch to enter from pipe or with vine
            lda #$00       
            ldy Player_Y_Position     ;if vertical position above a certain
            cpy #$30                  ;point, nullify controller bits and continue
            bcc AutoControlPlayer     ;with player movement code, do not return
            lda PlayerEntranceCtrl    ;check player entry bits from header
            cmp #$06
            beq ChkBehPipe            ;if set to 6 or 7, execute pipe intro code
            cmp #$07                  ;otherwise branch to normal entry
            bne PlayerRdy
ChkBehPipe: lda Player_SprAttrib      ;check for sprite attributes
            bne IntroEntr             ;branch if found
            lda #$01
            jmp AutoControlPlayer     ;force player to walk to the right
IntroEntr:  jsr EnterSidePipe         ;execute sub to move player to the right
            dec ChangeAreaTimer       ;decrement timer for change of area
            bne ExitEntr              ;branch to exit if not yet expired
            inc DisableIntermediate   ;set flag to skip world and lives display
            jmp NextArea              ;jump to increment to next area and set modes
EntrMode2:  lda JoypadOverride        ;if controller override bits set here,
            bne VineEntr              ;branch to enter with vine
            lda #$ff                  ;otherwise, set value here then execute sub
            jsr MovePlayerYAxis       ;to move player upwards (note $ff = -1)
            lda Player_Y_Position     ;check to see if player is at a specific coordinate
            cmp #$91                  ;if player risen to a certain point (this requires pipes
            bcc PlayerRdy             ;to be at specific height to look/function right) branch
            rts                       ;to the last part, otherwise leave
VineEntr:   lda VineHeight
            cmp #$60                  ;check vine height
            bne ExitEntr              ;if vine not yet reached maximum height, branch to leave
            lda Player_Y_Position     ;get player's vertical coordinate
            cmp #$99                  ;check player's vertical coordinate against preset value
            ldy #$00                  ;load default values to be written to 
            lda #$01                  ;this value moves player to the right off the vine
            bcc OffVine               ;if vertical coordinate < preset value, use defaults
            lda #$03
            sta Player_State          ;otherwise set player state to climbing
            iny                       ;increment value in Y
            lda #$08                  ;set block in block buffer to cover hole, then 
            sta Block_Buffer_1+$b4    ;use same value to force player to climb
OffVine:    sty DisableCollisionDet   ;set collision detection disable flag
            jsr AutoControlPlayer     ;use contents of A to move player up or right, execute sub
            lda Player_X_Position
            cmp #$48                  ;check player's horizontal position
            bcc ExitEntr              ;if not far enough to the right, branch to leave
PlayerRdy:  lda #$08                  ;set routine to be executed by game engine next frame
            sta GameEngineSubroutine
            lda #$01                  ;set to face player to the right
            sta PlayerFacingDir
            lsr                       ;init A
            sta AltEntranceControl    ;init mode of entry
            sta DisableCollisionDet   ;init collision detection disable flag
            sta JoypadOverride        ;nullify controller override bits
ExitEntr:   rts                       ;leave!

;-------------------------------------------------------------------------------------
;$07 - used to hold upper limit of high byte when player falls down hole

AutoControlPlayer:
      sta SavedJoypadBits         ;override controller bits with contents of A if executing here

PlayerCtrlRoutine:
            lda GameEngineSubroutine    ;check task here
            cmp #$0b                    ;if certain value is set, branch to skip controller bit loading
            beq SizeChk
            lda AreaType                ;are we in a water type area?
            bne SaveJoyp                ;if not, branch
            ldy Player_Y_HighPos
            dey                         ;if not in vertical area between
            bne DisJoyp                 ;status bar and bottom, branch
            lda Player_Y_Position
            cmp #$d0                    ;if nearing the bottom of the screen or
            bcc SaveJoyp                ;not in the vertical area between status bar or bottom,
DisJoyp:    lda #$00                    ;disable controller bits
            sta SavedJoypadBits
SaveJoyp:   lda SavedJoypadBits         ;otherwise store A and B buttons in $0a
            and #%11000000
            sta A_B_Buttons
            lda SavedJoypadBits         ;store left and right buttons in $0c
            and #%00000011
            sta Left_Right_Buttons
            lda SavedJoypadBits         ;store up and down buttons in $0b
            and #%00001100
            sta Up_Down_Buttons
            and #%00000100              ;check for pressing down
            beq SizeChk                 ;if not, branch
            lda Player_State            ;check player's state
            bne SizeChk                 ;if not on the ground, branch
            ldy Left_Right_Buttons      ;check left and right
            beq SizeChk                 ;if neither pressed, branch
            lda #$00
            sta Left_Right_Buttons      ;if pressing down while on the ground,
            sta Up_Down_Buttons         ;nullify directional bits
SizeChk:    jsr PlayerMovementSubs      ;run movement subroutines
            ldy #$01                    ;is player small?
            lda PlayerSize
            bne ChkMoveDir
            ldy #$00                    ;check for if crouching
            lda CrouchingFlag
            beq ChkMoveDir              ;if not, branch ahead
            ldy #$02                    ;if big and crouching, load y with 2
ChkMoveDir: sty Player_BoundBoxCtrl     ;set contents of Y as player's bounding box size control
            lda #$01                    ;set moving direction to right by default
            ldy Player_X_Speed          ;check player's horizontal speed
            beq PlayerSubs              ;if not moving at all horizontally, skip this part
            bpl SetMoveDir              ;if moving to the right, use default moving direction
            asl                         ;otherwise change to move to the left
SetMoveDir: sta Player_MovingDir        ;set moving direction
PlayerSubs: jsr ScrollHandler           ;move the screen if necessary
            jsr GetPlayerOffscreenBits  ;get player's offscreen bits
            jsr RelativePlayerPosition  ;get coordinates relative to the screen
            ldx #$00                    ;set offset for player object
            jsr BoundingBoxCore         ;get player's bounding box coordinates
            jsr PlayerBGCollision       ;do collision detection and process
            lda Player_Y_Position
            cmp #$40                    ;check to see if player is higher than 64th pixel
            bcc PlayerHole              ;if so, branch ahead
            lda GameEngineSubroutine
            cmp #$05                    ;if running end-of-level routine, branch ahead
            beq PlayerHole
            cmp #$07                    ;if running player entrance routine, branch ahead
            beq PlayerHole
            cmp #$04                    ;if running routines $00-$03, branch ahead
            bcc PlayerHole
            lda Player_SprAttrib
            and #%11011111              ;otherwise nullify player's
            sta Player_SprAttrib        ;background priority flag
PlayerHole: lda Player_Y_HighPos        ;check player's vertical high byte
            cmp #$02                    ;for below the screen
            bmi ExitCtrl                ;branch to leave if not that far down
            ldx #$01
            stx ScrollLock              ;set scroll lock
            ldy #$04
            sty $07                     ;set value here
            ldx #$00                    ;use X as flag, and clear for cloud level
            ldy GameTimerExpiredFlag    ;check game timer expiration flag
            bne HoleDie                 ;if set, branch
            ldy CloudTypeOverride       ;check for cloud type override
            bne ChkHoleX                ;skip to last part if found
HoleDie:    inx                         ;set flag in X for player death
            ldy GameEngineSubroutine
            cpy #$0b                    ;check for some other routine running
            beq ChkHoleX                ;if so, branch ahead
            ldy DeathMusicLoaded        ;check value here
            bne HoleBottom              ;if already set, branch to next part
            iny
            sty EventMusicQueue         ;otherwise play death music
            sty DeathMusicLoaded        ;and set value here
HoleBottom: ldy #$06
            sty $07                     ;change value here
ChkHoleX:   cmp $07                     ;compare vertical high byte with value set here
            bmi ExitCtrl                ;if less, branch to leave
            dex                         ;otherwise decrement flag in X
            bmi CloudExit               ;if flag was clear, branch to set modes and other values
            ldy EventMusicBuffer        ;check to see if music is still playing
            bne ExitCtrl                ;branch to leave if so
            lda #$06                    ;otherwise set to run lose life routine
            sta GameEngineSubroutine    ;on next frame
ExitCtrl:   rts                         ;leave

CloudExit:
      lda #$00
      sta JoypadOverride      ;clear controller override bits if any are set
      jsr SetEntr             ;do sub to set secondary mode
      inc AltEntranceControl  ;set mode of entry to 3
      rts

;-------------------------------------------------------------------------------------

Vine_AutoClimb:
           lda Player_Y_HighPos   ;check to see whether player reached position
           bne AutoClimb          ;above the status bar yet and if so, set modes
           lda Player_Y_Position
           cmp #$e4
           bcc SetEntr
AutoClimb: lda #%00001000         ;set controller bits override to up
           sta JoypadOverride
           ldy #$03               ;set player state to climbing
           sty Player_State
           jmp AutoControlPlayer
SetEntr:   lda #$02               ;set starting position to override
           sta AltEntranceControl
           jmp ChgAreaMode        ;set modes

;-------------------------------------------------------------------------------------

VerticalPipeEntry:
      lda #$01             ;set 1 as movement amount
      jsr MovePlayerYAxis  ;do sub to move player downwards
      jsr ScrollHandler    ;do sub to scroll screen with saved force if necessary
      ldy #$00             ;load default mode of entry
      lda WarpZoneControl  ;check warp zone control variable/flag
      bne ChgAreaPipe      ;if set, branch to use mode 0
      iny
      lda AreaType         ;check for castle level type
      cmp #$03
      bne ChgAreaPipe      ;if not castle type level, use mode 1
      iny
      jmp ChgAreaPipe      ;otherwise use mode 2

MovePlayerYAxis:
      clc
      adc Player_Y_Position ;add contents of A to player position
      sta Player_Y_Position
      rts

;-------------------------------------------------------------------------------------

SideExitPipeEntry:
             jsr EnterSidePipe         ;execute sub to move player to the right
             ldy #$02
ChgAreaPipe: dec ChangeAreaTimer       ;decrement timer for change of area
             bne ExitCAPipe
             sty AltEntranceControl    ;when timer expires set mode of alternate entry
ChgAreaMode: inc DisableScreenFlag     ;set flag to disable screen output
             lda #$00
             sta OperMode_Task         ;set secondary mode of operation
             sta Sprite0HitDetectFlag  ;disable sprite 0 check
ExitCAPipe:  rts                       ;leave

EnterSidePipe:
           lda #$08               ;set player's horizontal speed
           sta Player_X_Speed
           ldy #$01               ;set controller right button by default
           lda Player_X_Position  ;mask out higher nybble of player's
           and #%00001111         ;horizontal position
           bne RightPipe
           sta Player_X_Speed     ;if lower nybble = 0, set as horizontal speed
           tay                    ;and nullify controller bit override here
RightPipe: tya                    ;use contents of Y to
           jsr AutoControlPlayer  ;execute player control routine with ctrl bits nulled
           rts

;-------------------------------------------------------------------------------------

PlayerChangeSize:
             lda TimerControl    ;check master timer control
             cmp #$f8            ;for specific moment in time
             bne EndChgSize      ;branch if before or after that point
             jmp InitChangeSize  ;otherwise run code to get growing/shrinking going
EndChgSize:  cmp #$c4            ;check again for another specific moment
             bne ExitChgSize     ;and branch to leave if before or after that point
             jsr DonePlayerTask  ;otherwise do sub to init timer control and set routine
ExitChgSize: rts                 ;and then leave

;-------------------------------------------------------------------------------------

PlayerInjuryBlink:
           lda TimerControl       ;check master timer control
           cmp #$f0               ;for specific moment in time
           bcs ExitBlink          ;branch if before that point
           cmp #$c8               ;check again for another specific point
           beq DonePlayerTask     ;branch if at that point, and not before or after
           jmp PlayerCtrlRoutine  ;otherwise run player control routine
ExitBlink: bne ExitBoth           ;do unconditional branch to leave

InitChangeSize:
          ldy PlayerChangeSizeFlag  ;if growing/shrinking flag already set
          bne ExitBoth              ;then branch to leave
          sty PlayerAnimCtrl        ;otherwise initialize player's animation frame control
          inc PlayerChangeSizeFlag  ;set growing/shrinking flag
          lda PlayerSize
          eor #$01                  ;invert player's size
          sta PlayerSize
ExitBoth: rts                       ;leave

;-------------------------------------------------------------------------------------
;$00 - used in CyclePlayerPalette to store current palette to cycle

PlayerDeath:
      lda TimerControl       ;check master timer control
      cmp #$f0               ;for specific moment in time
      bcs ExitDeath          ;branch to leave if before that point
      jmp PlayerCtrlRoutine  ;otherwise run player control routine

DonePlayerTask:
      lda #$00
      sta TimerControl          ;initialize master timer control to continue timers
      lda #$08
      sta GameEngineSubroutine  ;set player control routine to run next frame
      rts                       ;leave

PlayerFireFlower: 
      lda TimerControl       ;check master timer control
      cmp #$c0               ;for specific moment in time
      beq ResetPalFireFlower ;branch if at moment, not before or after
      lda FrameCounter       ;get frame counter
      lsr
      lsr                    ;divide by four to change every four frames

CyclePlayerPalette:
      and #$03              ;mask out all but d1-d0 (previously d3-d2)
      sta $00               ;store result here to use as palette bits
      lda Player_SprAttrib  ;get player attributes
      and #%11111100        ;save any other bits but palette bits
      ora $00               ;add palette bits
      sta Player_SprAttrib  ;store as new player attributes
      rts                   ;and leave

ResetPalFireFlower:
      jsr DonePlayerTask    ;do sub to init timer control and run player control routine

ResetPalStar:
      lda Player_SprAttrib  ;get player attributes
      and #%11111100        ;mask out palette bits to force palette 0
      sta Player_SprAttrib  ;store as new player attributes
      rts                   ;and leave

ExitDeath:
      rts          ;leave from death routine

;-------------------------------------------------------------------------------------

FlagpoleSlide:
             lda Enemy_ID+5           ;check special use enemy slot
             cmp #FlagpoleFlagObject  ;for flagpole flag object
             bne NoFPObj              ;if not found, branch to something residual
             lda FlagpoleSoundQueue   ;load flagpole sound
             sta Square1SoundQueue    ;into square 1's sfx queue
             lda #$00
             sta FlagpoleSoundQueue   ;init flagpole sound queue
             ldy Player_Y_Position
             cpy #$9e                 ;check to see if player has slid down
             bcs SlidePlayer          ;far enough, and if so, branch with no controller bits set
             lda #$04                 ;otherwise force player to climb down (to slide)
SlidePlayer: jmp AutoControlPlayer    ;jump to player control routine
NoFPObj:     inc GameEngineSubroutine ;increment to next routine (this may
             rts                      ;be residual code)

;-------------------------------------------------------------------------------------

Hidden1UpCoinAmts:
      .db $15, $23, $16, $1b, $17, $18, $23, $63

PlayerEndLevel:
          lda #$01                  ;force player to walk to the right
          jsr AutoControlPlayer
          lda Player_Y_Position     ;check player's vertical position
          cmp #$ae
          bcc ChkStop               ;if player is not yet off the flagpole, skip this part
          lda ScrollLock            ;if scroll lock not set, branch ahead to next part
          beq ChkStop               ;because we only need to do this part once
          lda #EndOfLevelMusic
          sta EventMusicQueue       ;load win level music in event music queue
          lda #$00
          sta ScrollLock            ;turn off scroll lock to skip this part later
ChkStop:  lda Player_CollisionBits  ;get player collision bits
          lsr                       ;check for d0 set
          bcs RdyNextA              ;if d0 set, skip to next part
          lda StarFlagTaskControl   ;if star flag task control already set,
          bne InCastle              ;go ahead with the rest of the code
          inc StarFlagTaskControl   ;otherwise set task control now (this gets ball rolling!)
InCastle: lda #%00100000            ;set player's background priority bit to
          sta Player_SprAttrib      ;give illusion of being inside the castle
RdyNextA: lda StarFlagTaskControl
          cmp #$05                  ;if star flag task control not yet set
          bne ExitNA                ;beyond last valid task number, branch to leave
          inc LevelNumber           ;increment level number used for game logic
          lda LevelNumber
          cmp #$03                  ;check to see if we have yet reached level -4
          bne NextArea              ;and skip this last part here if not
          ldy WorldNumber           ;get world number as offset
          lda CoinTallyFor1Ups      ;check third area coin tally for bonus 1-ups
          cmp Hidden1UpCoinAmts,y   ;against minimum value, if player has not collected
          bcc NextArea              ;at least this number of coins, leave flag clear
          inc Hidden1UpFlag         ;otherwise set hidden 1-up box control flag
NextArea: inc AreaNumber            ;increment area number used for address loader
          jsr LoadAreaPointer       ;get new level pointer
          inc FetchNewGameTimerFlag ;set flag to load new game timer
          jsr ChgAreaMode           ;do sub to set secondary mode, disable screen and sprite 0
          sta HalfwayPage           ;reset halfway page to 0 (beginning)
          lda #Silence
          sta EventMusicQueue       ;silence music and leave
ExitNA:   rts

;-------------------------------------------------------------------------------------

PlayerMovementSubs:
           lda #$00                  ;set A to init crouch flag by default
           ldy PlayerSize            ;is player small?
           bne SetCrouch             ;if so, branch
           lda Player_State          ;check state of player
           bne ProcMove              ;if not on the ground, branch
           lda Up_Down_Buttons       ;load controller bits for up and down
           and #%00000100            ;single out bit for down button
SetCrouch: sta CrouchingFlag         ;store value in crouch flag
ProcMove:  jsr PlayerPhysicsSub      ;run sub related to jumping and swimming
           lda PlayerChangeSizeFlag  ;if growing/shrinking flag set,
           bne NoMoveSub             ;branch to leave
           lda Player_State
           cmp #$03                  ;get player state
           beq MoveSubs              ;if climbing, branch ahead, leave timer unset
           ldy #$18
           sty ClimbSideTimer        ;otherwise reset timer now
MoveSubs:  jsr JumpEngine

      .dw OnGroundStateSub
      .dw JumpSwimSub
      .dw FallingSub
      .dw ClimbingSub

NoMoveSub: rts

;-------------------------------------------------------------------------------------
;$00 - used by ClimbingSub to store high vertical adder

OnGroundStateSub:
         jsr GetPlayerAnimSpeed     ;do a sub to set animation frame timing
         lda Left_Right_Buttons
         beq GndMove                ;if left/right controller bits not set, skip instruction
         sta PlayerFacingDir        ;otherwise set new facing direction
GndMove: jsr ImposeFriction         ;do a sub to impose friction on player's walk/run
         jsr MovePlayerHorizontally ;do another sub to move player horizontally
         sta Player_X_Scroll        ;set returned value as player's movement speed for scroll
         rts

;--------------------------------

FallingSub:
      lda VerticalForceDown
      sta VerticalForce      ;dump vertical movement force for falling into main one
      jmp LRAir              ;movement force, then skip ahead to process left/right movement

;--------------------------------

JumpSwimSub:
          ldy Player_Y_Speed         ;if player's vertical speed zero
          bpl DumpFall               ;or moving downwards, branch to falling
          lda A_B_Buttons
          and #A_Button              ;check to see if A button is being pressed
          and PreviousA_B_Buttons    ;and was pressed in previous frame
          bne ProcSwim               ;if so, branch elsewhere
          lda JumpOrigin_Y_Position  ;get vertical position player jumped from
          sec
          sbc Player_Y_Position      ;subtract current from original vertical coordinate
          cmp DiffToHaltJump         ;compare to value set here to see if player is in mid-jump
          bcc ProcSwim               ;or just starting to jump, if just starting, skip ahead
DumpFall: lda VerticalForceDown      ;otherwise dump falling into main fractional
          sta VerticalForce
ProcSwim: lda SwimmingFlag           ;if swimming flag not set,
          beq LRAir                  ;branch ahead to last part
          jsr GetPlayerAnimSpeed     ;do a sub to get animation frame timing
          lda Player_Y_Position
          cmp #$14                   ;check vertical position against preset value
          bcs LRWater                ;if not yet reached a certain position, branch ahead
          lda #$18
          sta VerticalForce          ;otherwise set fractional
LRWater:  lda Left_Right_Buttons     ;check left/right controller bits (check for swimming)
          beq LRAir                  ;if not pressing any, skip
          sta PlayerFacingDir        ;otherwise set facing direction accordingly
LRAir:    lda Left_Right_Buttons     ;check left/right controller bits (check for jumping/falling)
          beq JSMove                 ;if not pressing any, skip
          jsr ImposeFriction         ;otherwise process horizontal movement
JSMove:   jsr MovePlayerHorizontally ;do a sub to move player horizontally
          sta Player_X_Scroll        ;set player's speed here, to be used for scroll later
          lda GameEngineSubroutine
          cmp #$0b                   ;check for specific routine selected
          bne ExitMov1               ;branch if not set to run
          lda #$28
          sta VerticalForce          ;otherwise set fractional
ExitMov1: jmp MovePlayerVertically   ;jump to move player vertically, then leave

;--------------------------------

ClimbAdderLow:
      .db $0e, $04, $fc, $f2
ClimbAdderHigh:
      .db $00, $00, $ff, $ff

ClimbingSub:
             lda Player_YMF_Dummy
             clc                      ;add movement force to dummy variable
             adc Player_Y_MoveForce   ;save with carry
             sta Player_YMF_Dummy
             ldy #$00                 ;set default adder here
             lda Player_Y_Speed       ;get player's vertical speed
             bpl MoveOnVine           ;if not moving upwards, branch
             dey                      ;otherwise set adder to $ff
MoveOnVine:  sty $00                  ;store adder here
             adc Player_Y_Position    ;add carry to player's vertical position
             sta Player_Y_Position    ;and store to move player up or down
             lda Player_Y_HighPos
             adc $00                  ;add carry to player's page location
             sta Player_Y_HighPos     ;and store
             lda Left_Right_Buttons   ;compare left/right controller bits
             and Player_CollisionBits ;to collision flag
             beq InitCSTimer          ;if not set, skip to end
             ldy ClimbSideTimer       ;otherwise check timer 
             bne ExitCSub             ;if timer not expired, branch to leave
             ldy #$18
             sty ClimbSideTimer       ;otherwise set timer now
             ldx #$00                 ;set default offset here
             ldy PlayerFacingDir      ;get facing direction
             lsr                      ;move right button controller bit to carry
             bcs ClimbFD              ;if controller right pressed, branch ahead
             inx
             inx                      ;otherwise increment offset by 2 bytes
ClimbFD:     dey                      ;check to see if facing right
             beq CSetFDir             ;if so, branch, do not increment
             inx                      ;otherwise increment by 1 byte
CSetFDir:    lda Player_X_Position
             clc                      ;add or subtract from player's horizontal position
             adc ClimbAdderLow,x      ;using value here as adder and X as offset
             sta Player_X_Position
             lda Player_PageLoc       ;add or subtract carry or borrow using value here
             adc ClimbAdderHigh,x     ;from the player's page location
             sta Player_PageLoc
             lda Left_Right_Buttons   ;get left/right controller bits again
             eor #%00000011           ;invert them and store them while player
             sta PlayerFacingDir      ;is on vine to face player in opposite direction
ExitCSub:    rts                      ;then leave
InitCSTimer: sta ClimbSideTimer       ;initialize timer here
             rts

;-------------------------------------------------------------------------------------
;$00 - used to store offset to friction data

JumpMForceData:
      .db $20, $20, $1e, $28, $28, $0d, $04

FallMForceData:
      .db $70, $70, $60, $90, $90, $0a, $09

PlayerYSpdData:
      .db $fc, $fc, $fc, $fb, $fb, $fe, $ff

InitMForceData:
      .db $00, $00, $00, $00, $00, $80, $00

MaxLeftXSpdData:
      .db $d8, $e8, $f0

MaxRightXSpdData:
      .db $28, $18, $10
      .db $0c ;used for pipe intros

FrictionData:
      .db $e4, $98, $d0

Climb_Y_SpeedData:
      .db $00, $ff, $01

Climb_Y_MForceData:
      .db $00, $20, $ff

PlayerPhysicsSub:
           lda Player_State          ;check player state
           cmp #$03
           bne CheckForJumping       ;if not climbing, branch
           ldy #$00
           lda Up_Down_Buttons       ;get controller bits for up/down
           and Player_CollisionBits  ;check against player's collision detection bits
           beq ProcClimb             ;if not pressing up or down, branch
           iny
           and #%00001000            ;check for pressing up
           bne ProcClimb
           iny
ProcClimb: ldx Climb_Y_MForceData,y  ;load value here
           stx Player_Y_MoveForce    ;store as vertical movement force
           lda #$08                  ;load default animation timing
           ldx Climb_Y_SpeedData,y   ;load some other value here
           stx Player_Y_Speed        ;store as vertical speed
           bmi SetCAnim              ;if climbing down, use default animation timing value
           lsr                       ;otherwise divide timer setting by 2
SetCAnim:  sta PlayerAnimTimerSet    ;store animation timer setting and leave
           rts

CheckForJumping:
        lda JumpspringAnimCtrl    ;if jumpspring animating, 
        bne NoJump                ;skip ahead to something else
        lda A_B_Buttons           ;check for A button press
        and #A_Button
        beq NoJump                ;if not, branch to something else
        and PreviousA_B_Buttons   ;if button not pressed in previous frame, branch
        beq ProcJumping
NoJump: jmp X_Physics             ;otherwise, jump to something else

ProcJumping:
           lda Player_State           ;check player state
           beq InitJS                 ;if on the ground, branch
           lda SwimmingFlag           ;if swimming flag not set, jump to do something else
           beq NoJump                 ;to prevent midair jumping, otherwise continue
           lda JumpSwimTimer          ;if jump/swim timer nonzero, branch
           bne InitJS
           lda Player_Y_Speed         ;check player's vertical speed
           bpl InitJS                 ;if player's vertical speed motionless or down, branch
           jmp X_Physics              ;if timer at zero and player still rising, do not swim
InitJS:    jsr RedrawFrameNumbers
           lda #$20                   ;set jump/swim timer
           sta JumpSwimTimer
           ldy #$00                   ;initialize vertical force and dummy variable
           sty Player_YMF_Dummy
           sty Player_Y_MoveForce
           lda Player_Y_HighPos       ;get vertical high and low bytes of jump origin
           sta JumpOrigin_Y_HighPos   ;and store them next to each other here
           lda Player_Y_Position
           sta JumpOrigin_Y_Position
           lda #$01                   ;set player state to jumping/swimming
           sta Player_State
           lda Player_XSpeedAbsolute  ;check value related to walking/running speed
           cmp #$09
           bcc ChkWtr                 ;branch if below certain values, increment Y
           iny                        ;for each amount equal or exceeded
           cmp #$10
           bcc ChkWtr
           iny
           cmp #$19
           bcc ChkWtr
           iny
           cmp #$1c
           bcc ChkWtr                 ;note that for jumping, range is 0-4 for Y
           iny
ChkWtr:    lda #$01                   ;set value here (apparently always set to 1)
           sta DiffToHaltJump
           lda SwimmingFlag           ;if swimming flag disabled, branch
           beq GetYPhy
           ldy #$05                   ;otherwise set Y to 5, range is 5-6
           lda Whirlpool_Flag         ;if whirlpool flag not set, branch
           beq GetYPhy
           iny                        ;otherwise increment to 6
GetYPhy:   lda JumpMForceData,y       ;store appropriate jump/swim
           sta VerticalForce          ;data here
           lda FallMForceData,y
           sta VerticalForceDown
           lda InitMForceData,y
           sta Player_Y_MoveForce
           lda PlayerYSpdData,y
           sta Player_Y_Speed
           lda SwimmingFlag           ;if swimming flag disabled, branch
           beq PJumpSnd
           lda #Sfx_EnemyStomp        ;load swim/goomba stomp sound into
           sta Square1SoundQueue      ;square 1's sfx queue
           lda Player_Y_Position
           cmp #$14                   ;check vertical low byte of player position
           bcs X_Physics              ;if below a certain point, branch
           lda #$00                   ;otherwise reset player's vertical speed
           sta Player_Y_Speed         ;and jump to something else to keep player
           jmp X_Physics              ;from swimming above water level
PJumpSnd:  lda #Sfx_BigJump           ;load big mario's jump sound by default
           ldy PlayerSize             ;is mario big?
           beq SJumpSnd
           lda #Sfx_SmallJump         ;if not, load small mario's jump sound
SJumpSnd:  sta Square1SoundQueue      ;store appropriate jump sound in square 1 sfx queue
X_Physics: ldy #$00
           sty $00                    ;init value here
           lda Player_State           ;if mario is on the ground, branch
           beq ProcPRun
           lda Player_XSpeedAbsolute  ;check something that seems to be related
           cmp #$19                   ;to mario's speed
           bcs GetXPhy                ;if =>$19 branch here
           bcc ChkRFast               ;if not branch elsewhere
ProcPRun:  iny                        ;if mario on the ground, increment Y
           lda AreaType               ;check area type
           beq ChkRFast               ;if water type, branch
           dey                        ;decrement Y by default for non-water type area
           lda Left_Right_Buttons     ;get left/right controller bits
           cmp Player_MovingDir       ;check against moving direction
           bne ChkRFast               ;if controller bits <> moving direction, skip this part
           lda A_B_Buttons            ;check for b button pressed
           and #B_Button
           bne SetRTmr                ;if pressed, skip ahead to set timer
           lda RunningTimer           ;check for running timer set
           bne GetXPhy                ;if set, branch
ChkRFast:  iny                        ;if running timer not set or level type is water, 
           inc $00                    ;increment Y again and temp variable in memory
           lda RunningSpeed
           bne FastXSp                ;if running speed set here, branch
           lda Player_XSpeedAbsolute
           cmp #$21                   ;otherwise check player's walking/running speed
           bcc GetXPhy                ;if less than a certain amount, branch ahead
FastXSp:   inc $00                    ;if running speed set or speed => $21 increment $00
           jmp GetXPhy                ;and jump ahead
SetRTmr:   lda #$0a                   ;if b button pressed, set running timer
           sta RunningTimer
GetXPhy:   lda MaxLeftXSpdData,y      ;get maximum speed to the left
           sta MaximumLeftSpeed
           lda GameEngineSubroutine   ;check for specific routine running
           cmp #$07                   ;(player entrance)
           bne GetXPhy2               ;if not running, skip and use old value of Y
           ldy #$03                   ;otherwise set Y to 3
GetXPhy2:  lda MaxRightXSpdData,y     ;get maximum speed to the right
           sta MaximumRightSpeed
           ldy $00                    ;get other value in memory
           lda FrictionData,y         ;get value using value in memory as offset
           sta FrictionAdderLow
           lda #$00
           sta FrictionAdderHigh      ;init something here
           lda PlayerFacingDir
           cmp Player_MovingDir       ;check facing direction against moving direction
           beq ExitPhy                ;if the same, branch to leave
           asl FrictionAdderLow       ;otherwise shift d7 of friction adder low into carry
           rol FrictionAdderHigh      ;then rotate carry onto d0 of friction adder high
ExitPhy:   rts                        ;and then leave

;-------------------------------------------------------------------------------------

PlayerAnimTmrData:
      .db $02, $04, $07

GetPlayerAnimSpeed:
            ldy #$00                   ;initialize offset in Y
            lda Player_XSpeedAbsolute  ;check player's walking/running speed
            cmp #$1c                   ;against preset amount
            bcs SetRunSpd              ;if greater than a certain amount, branch ahead
            iny                        ;otherwise increment Y
            cmp #$0e                   ;compare against lower amount
            bcs ChkSkid                ;if greater than this but not greater than first, skip increment
            iny                        ;otherwise increment Y again
ChkSkid:    lda SavedJoypadBits        ;get controller bits
            and #%01111111             ;mask out A button
            beq SetAnimSpd             ;if no other buttons pressed, branch ahead of all this
            and #$03                   ;mask out all others except left and right
            cmp Player_MovingDir       ;check against moving direction
            bne ProcSkid               ;if left/right controller bits <> moving direction, branch
            lda #$00                   ;otherwise set zero value here
SetRunSpd:  sta RunningSpeed           ;store zero or running speed here
            jmp SetAnimSpd
ProcSkid:   lda Player_XSpeedAbsolute  ;check player's walking/running speed
            cmp #$0b                   ;against one last amount
            bcs SetAnimSpd             ;if greater than this amount, branch
            lda PlayerFacingDir
            sta Player_MovingDir       ;otherwise use facing direction to set moving direction
            lda #$00
            sta Player_X_Speed         ;nullify player's horizontal speed
            sta Player_X_MoveForce     ;and dummy variable for player
SetAnimSpd: lda PlayerAnimTmrData,y    ;get animation timer setting using Y as offset
            sta PlayerAnimTimerSet
            rts

;-------------------------------------------------------------------------------------

ImposeFriction:
           and Player_CollisionBits  ;perform AND between left/right controller bits and collision flag
           cmp #$00                  ;then compare to zero (this instruction is redundant)
           bne JoypFrict             ;if any bits set, branch to next part
           lda Player_X_Speed
           beq SetAbsSpd             ;if player has no horizontal speed, branch ahead to last part
           bpl RghtFrict             ;if player moving to the right, branch to slow
           bmi LeftFrict             ;otherwise logic dictates player moving left, branch to slow
JoypFrict: lsr                       ;put right controller bit into carry
           bcc RghtFrict             ;if left button pressed, carry = 0, thus branch
LeftFrict: lda Player_X_MoveForce    ;load value set here
           clc
           adc FrictionAdderLow      ;add to it another value set here
           sta Player_X_MoveForce    ;store here
           lda Player_X_Speed
           adc FrictionAdderHigh     ;add value plus carry to horizontal speed
           sta Player_X_Speed        ;set as new horizontal speed
           cmp MaximumRightSpeed     ;compare against maximum value for right movement
           bmi XSpdSign              ;if horizontal speed greater negatively, branch
           lda MaximumRightSpeed     ;otherwise set preset value as horizontal speed
           sta Player_X_Speed        ;thus slowing the player's left movement down
           jmp SetAbsSpd             ;skip to the end
RghtFrict: lda Player_X_MoveForce    ;load value set here
           sec
           sbc FrictionAdderLow      ;subtract from it another value set here
           sta Player_X_MoveForce    ;store here
           lda Player_X_Speed
           sbc FrictionAdderHigh     ;subtract value plus borrow from horizontal speed
           sta Player_X_Speed        ;set as new horizontal speed
           cmp MaximumLeftSpeed      ;compare against maximum value for left movement
           bpl XSpdSign              ;if horizontal speed greater positively, branch
           lda MaximumLeftSpeed      ;otherwise set preset value as horizontal speed
           sta Player_X_Speed        ;thus slowing the player's right movement down
XSpdSign:  cmp #$00                  ;if player not moving or moving to the right,
           bpl SetAbsSpd             ;branch and leave horizontal speed value unmodified
           eor #$ff
           clc                       ;otherwise get two's compliment to get absolute
           adc #$01                  ;unsigned walking/running speed
SetAbsSpd: sta Player_XSpeedAbsolute ;store walking/running speed here and leave
           rts

;-------------------------------------------------------------------------------------
;$00 - used to store downward movement force in FireballObjCore
;$02 - used to store maximum vertical speed in FireballObjCore
;$07 - used to store pseudorandom bit in BubbleCheck

ProcFireball_Bubble:
      lda PlayerStatus           ;check player's status
      cmp #$02
      bcc ProcAirBubbles         ;if not fiery, branch
      lda A_B_Buttons
      and #B_Button              ;check for b button pressed
      beq ProcFireballs          ;branch if not pressed
      and PreviousA_B_Buttons
      bne ProcFireballs          ;if button pressed in previous frame, branch
      lda FireballCounter        ;load fireball counter
      and #%00000001             ;get LSB and use as offset for buffer
      tax
      lda Fireball_State,x       ;load fireball state
      bne ProcFireballs          ;if not inactive, branch
      ldy Player_Y_HighPos       ;if player too high or too low, branch
      dey
      bne ProcFireballs
      lda CrouchingFlag          ;if player crouching, branch
      bne ProcFireballs
      lda Player_State           ;if player's state = climbing, branch
      cmp #$03
      beq ProcFireballs
      lda #Sfx_Fireball          ;play fireball sound effect
      sta Square1SoundQueue
      lda #$02                   ;load state
      sta Fireball_State,x
      ldy PlayerAnimTimerSet     ;copy animation frame timer setting
      sty FireballThrowingTimer  ;into fireball throwing timer
      dey
      sty PlayerAnimTimer        ;decrement and store in player's animation timer
      inc FireballCounter        ;increment fireball counter

ProcFireballs:
      ldx #$00
      jsr FireballObjCore  ;process first fireball object
      ldx #$01
      jsr FireballObjCore  ;process second fireball object, then do air bubbles

ProcAirBubbles:
          lda AreaType                ;if not water type level, skip the rest of this
          bne BublExit
          ldx #$02                    ;otherwise load counter and use as offset
BublLoop: stx ObjectOffset            ;store offset
          jsr BubbleCheck             ;check timers and coordinates, create air bubble
          jsr RelativeBubblePosition  ;get relative coordinates
          jsr GetBubbleOffscreenBits  ;get offscreen information
          jsr DrawBubble              ;draw the air bubble
          dex
          bpl BublLoop                ;do this until all three are handled
BublExit: rts                         ;then leave

FireballXSpdData:
      .db $40, $c0

FireballObjCore:
         stx ObjectOffset             ;store offset as current object
         lda Fireball_State,x         ;check for d7 = 1
         asl
         bcs FireballExplosion        ;if so, branch to get relative coordinates and draw explosion
         ldy Fireball_State,x         ;if fireball inactive, branch to leave
         beq NoFBall
         dey                          ;if fireball state set to 1, skip this part and just run it
         beq RunFB
         lda Player_X_Position        ;get player's horizontal position
         adc #$04                     ;add four pixels and store as fireball's horizontal position
         sta Fireball_X_Position,x
         lda Player_PageLoc           ;get player's page location
         adc #$00                     ;add carry and store as fireball's page location
         sta Fireball_PageLoc,x
         lda Player_Y_Position        ;get player's vertical position and store
         sta Fireball_Y_Position,x
         lda #$01                     ;set high byte of vertical position
         sta Fireball_Y_HighPos,x
         ldy PlayerFacingDir          ;get player's facing direction
         dey                          ;decrement to use as offset here
         lda FireballXSpdData,y       ;set horizontal speed of fireball accordingly
         sta Fireball_X_Speed,x
         lda #$04                     ;set vertical speed of fireball
         sta Fireball_Y_Speed,x
         lda #$07
         sta Fireball_BoundBoxCtrl,x  ;set bounding box size control for fireball
         dec Fireball_State,x         ;decrement state to 1 to skip this part from now on
RunFB:   txa                          ;add 7 to offset to use
         clc                          ;as fireball offset for next routines
         adc #$07
         tax
         lda #$50                     ;set downward movement force here
         sta $00
         lda #$03                     ;set maximum speed here
         sta $02
         lda #$00
         jsr ImposeGravity            ;do sub here to impose gravity on fireball and move vertically
         jsr MoveObjectHorizontally   ;do another sub to move it horizontally
         ldx ObjectOffset             ;return fireball offset to X
         jsr RelativeFireballPosition ;get relative coordinates
         jsr GetFireballOffscreenBits ;get offscreen information
         jsr GetFireballBoundBox      ;get bounding box coordinates
         jsr FireballBGCollision      ;do fireball to background collision detection
         lda FBall_OffscreenBits      ;get fireball offscreen bits
         and #%11001100               ;mask out certain bits
         bne EraseFB                  ;if any bits still set, branch to kill fireball
         jsr FireballEnemyCollision   ;do fireball to enemy collision detection and deal with collisions
         jmp DrawFireball             ;draw fireball appropriately and leave
EraseFB: lda #$00                     ;erase fireball state
         sta Fireball_State,x
NoFBall: rts                          ;leave

FireballExplosion:
      jsr RelativeFireballPosition
      jmp DrawExplosion_Fireball

BubbleCheck:
      lda PseudoRandomBitReg+1,x  ;get part of LSFR
      and #$01
      sta $07                     ;store pseudorandom bit here
      lda Bubble_Y_Position,x     ;get vertical coordinate for air bubble
      cmp #$f8                    ;if offscreen coordinate not set,
      bne MoveBubl                ;branch to move air bubble
      lda AirBubbleTimer          ;if air bubble timer not expired,
      bne ExitBubl                ;branch to leave, otherwise create new air bubble

SetupBubble:
          ldy #$00                 ;load default value here
          lda PlayerFacingDir      ;get player's facing direction
          lsr                      ;move d0 to carry
          bcc PosBubl              ;branch to use default value if facing left
          ldy #$08                 ;otherwise load alternate value here
PosBubl:  tya                      ;use value loaded as adder
          adc Player_X_Position    ;add to player's horizontal position
          sta Bubble_X_Position,x  ;save as horizontal position for airbubble
          lda Player_PageLoc
          adc #$00                 ;add carry to player's page location
          sta Bubble_PageLoc,x     ;save as page location for airbubble
          lda Player_Y_Position
          clc                      ;add eight pixels to player's vertical position
          adc #$08
          sta Bubble_Y_Position,x  ;save as vertical position for air bubble
          lda #$01
          sta Bubble_Y_HighPos,x   ;set vertical high byte for air bubble
          ldy $07                  ;get pseudorandom bit, use as offset
          lda BubbleTimerData,y    ;get data for air bubble timer
          sta AirBubbleTimer       ;set air bubble timer
MoveBubl: ldy $07                  ;get pseudorandom bit again, use as offset
          lda Bubble_YMF_Dummy,x
          sec                      ;subtract pseudorandom amount from dummy variable
          sbc Bubble_MForceData,y
          sta Bubble_YMF_Dummy,x   ;save dummy variable
          lda Bubble_Y_Position,x
          sbc #$00                 ;subtract borrow from airbubble's vertical coordinate
          cmp #$20                 ;if below the status bar,
          bcs Y_Bubl               ;branch to go ahead and use to move air bubble upwards
          lda #$f8                 ;otherwise set offscreen coordinate
Y_Bubl:   sta Bubble_Y_Position,x  ;store as new vertical coordinate for air bubble
ExitBubl: rts                      ;leave

Bubble_MForceData:
      .db $ff, $50

BubbleTimerData:
      .db $40, $20

;-------------------------------------------------------------------------------------

RunGameTimer:
           lda OperMode               ;get primary mode of operation
           beq ExGTimer               ;branch to leave if in title screen mode
           lda GameEngineSubroutine
           cmp #$08                   ;if routine number less than eight running,
           bcc ExGTimer               ;branch to leave
           cmp #$0b                   ;if running death routine,
           beq ExGTimer               ;branch to leave
           lda Player_Y_HighPos
           cmp #$02                   ;if player below the screen,
           bcs ExGTimer               ;branch to leave regardless of level type
           lda GameTimerCtrlTimer     ;if game timer control not yet expired,
           bne ExGTimer               ;branch to leave
           lda GameTimerDisplay
           ora GameTimerDisplay+1     ;otherwise check game timer digits
           ora GameTimerDisplay+2
           beq TimeUpOn               ;if game timer digits at 000, branch to time-up code
           ldy GameTimerDisplay       ;otherwise check first digit
           dey                        ;if first digit not on 1,
           bne ResGTCtrl              ;branch to reset game timer control
           lda GameTimerDisplay+1     ;otherwise check second and third digits
           ora GameTimerDisplay+2
           bne ResGTCtrl              ;if timer not at 100, branch to reset game timer control
           lda #TimeRunningOutMusic
           sta EventMusicQueue        ;otherwise load time running out music
ResGTCtrl: lda #$18                   ;reset game timer control
           sta GameTimerCtrlTimer
           ldy #$23                   ;set offset for last digit
           lda #$ff                   ;set value to decrement game timer digit
           sta DigitModifier+5
           jsr DigitsMathRoutine3 ;do sub to decrement game timer slowly
           lda #$a4                   ;set status nybbles to update game timer display
           jmp PrintStatusBarNumbers  ;do sub to update the display
TimeUpOn:  sta PlayerStatus           ;init player status (note A will always be zero here)
           jsr ForceInjury            ;do sub to kill the player (note player is small here)
           inc GameTimerExpiredFlag   ;set game timer expiration flag
ExGTimer:  rts                        ;leave

;-------------------------------------------------------------------------------------
;$00 - used in WhirlpoolActivate to store whirlpool length / 2, page location of center of whirlpool
;and also to store movement force exerted on player
;$01 - used in ProcessWhirlpools to store page location of right extent of whirlpool
;and in WhirlpoolActivate to store center of whirlpool
;$02 - used in ProcessWhirlpools to store right extent of whirlpool and in
;WhirlpoolActivate to store maximum vertical speed

ProcessWhirlpools:
        lda AreaType                ;check for water type level
        bne ExitWh                  ;branch to leave if not found
        sta Whirlpool_Flag          ;otherwise initialize whirlpool flag
        lda TimerControl            ;if master timer control set,
        bne ExitWh                  ;branch to leave
        ldy #$04                    ;otherwise start with last whirlpool data
WhLoop: lda Whirlpool_LeftExtent,y  ;get left extent of whirlpool
        clc
        adc Whirlpool_Length,y      ;add length of whirlpool
        sta $02                     ;store result as right extent here
        lda Whirlpool_PageLoc,y     ;get page location
        beq NextWh                  ;if none or page 0, branch to get next data
        adc #$00                    ;add carry
        sta $01                     ;store result as page location of right extent here
        lda Player_X_Position       ;get player's horizontal position
        sec
        sbc Whirlpool_LeftExtent,y  ;subtract left extent
        lda Player_PageLoc          ;get player's page location
        sbc Whirlpool_PageLoc,y     ;subtract borrow
        bmi NextWh                  ;if player too far left, branch to get next data
        lda $02                     ;otherwise get right extent
        sec
        sbc Player_X_Position       ;subtract player's horizontal coordinate
        lda $01                     ;get right extent's page location
        sbc Player_PageLoc          ;subtract borrow
        bpl WhirlpoolActivate       ;if player within right extent, branch to whirlpool code
NextWh: dey                         ;move onto next whirlpool data
        bpl WhLoop                  ;do this until all whirlpools are checked
ExitWh: rts                         ;leave

WhirlpoolActivate:
        lda Whirlpool_Length,y      ;get length of whirlpool
        lsr                         ;divide by 2
        sta $00                     ;save here
        lda Whirlpool_LeftExtent,y  ;get left extent of whirlpool
        clc
        adc $00                     ;add length divided by 2
        sta $01                     ;save as center of whirlpool
        lda Whirlpool_PageLoc,y     ;get page location
        adc #$00                    ;add carry
        sta $00                     ;save as page location of whirlpool center
        lda FrameCounter            ;get frame counter
        lsr                         ;shift d0 into carry (to run on every other frame)
        bcc WhPull                  ;if d0 not set, branch to last part of code
        lda $01                     ;get center
        sec
        sbc Player_X_Position       ;subtract player's horizontal coordinate
        lda $00                     ;get page location of center
        sbc Player_PageLoc          ;subtract borrow
        bpl LeftWh                  ;if player to the left of center, branch
        lda Player_X_Position       ;otherwise slowly pull player left, towards the center
        sec
        sbc #$01                    ;subtract one pixel
        sta Player_X_Position       ;set player's new horizontal coordinate
        lda Player_PageLoc
        sbc #$00                    ;subtract borrow
        jmp SetPWh                  ;jump to set player's new page location
LeftWh: lda Player_CollisionBits    ;get player's collision bits
        lsr                         ;shift d0 into carry
        bcc WhPull                  ;if d0 not set, branch
        lda Player_X_Position       ;otherwise slowly pull player right, towards the center
        clc
        adc #$01                    ;add one pixel
        sta Player_X_Position       ;set player's new horizontal coordinate
        lda Player_PageLoc
        adc #$00                    ;add carry
SetPWh: sta Player_PageLoc          ;set player's new page location
WhPull: lda #$10
        sta $00                     ;set vertical movement force
        lda #$01
        sta Whirlpool_Flag          ;set whirlpool flag to be used later
        sta $02                     ;also set maximum vertical speed
        lsr
        tax                         ;set X for player offset
        jmp ImposeGravity           ;jump to put whirlpool effect on player vertically, do not return

;-------------------------------------------------------------------------------------

FlagpoleScoreMods:
      .db $05, $02, $08, $04, $01

FlagpoleScoreDigits:
      .db $03, $03, $04, $04, $04

FlagpoleRoutine:
           ldx #$05                  ;set enemy object offset
           stx ObjectOffset          ;to special use slot
           lda Enemy_ID,x
           cmp #FlagpoleFlagObject   ;if flagpole flag not found,
           bne ExitFlagP             ;branch to leave
           lda GameEngineSubroutine
           cmp #$04                  ;if flagpole slide routine not running,
           bne SkipScore             ;branch to near the end of code
           lda Player_State
           cmp #$03                  ;if player state not climbing,
           bne SkipScore             ;branch to near the end of code
           lda Enemy_Y_Position,x    ;check flagpole flag's vertical coordinate
           cmp #$aa                  ;if flagpole flag down to a certain point,
           bcs GiveFPScr             ;branch to end the level
           lda Player_Y_Position     ;check player's vertical coordinate
           cmp #$a2                  ;if player down to a certain point,
           bcs GiveFPScr             ;branch to end the level
           lda Enemy_YMF_Dummy,x
           adc #$ff                  ;add movement amount to dummy variable
           sta Enemy_YMF_Dummy,x     ;save dummy variable
           lda Enemy_Y_Position,x    ;get flag's vertical coordinate
           adc #$01                  ;add 1 plus carry to move flag, and
           sta Enemy_Y_Position,x    ;store vertical coordinate
           lda FlagpoleFNum_YMFDummy
           sec                       ;subtract movement amount from dummy variable
           sbc #$ff
           sta FlagpoleFNum_YMFDummy ;save dummy variable
           lda FlagpoleFNum_Y_Pos
           sbc #$01                  ;subtract one plus borrow to move floatey number,
           sta FlagpoleFNum_Y_Pos    ;and store vertical coordinate here
SkipScore: jmp FPGfx                 ;jump to skip ahead and draw flag and floatey number
GiveFPScr: lda #$05
           sta GameEngineSubroutine  ;set to run end-of-level subroutine on next frame
FPGfx:     jsr GetEnemyOffscreenBits ;get offscreen information
           jsr RelativeEnemyPosition ;get relative coordinates
           jsr FlagpoleGfxHandler    ;draw flagpole flag and floatey number
ExitFlagP: rts

;-------------------------------------------------------------------------------------

CannonBitmasks:
      .db %00001111, %00000111

ProcessCannons:
           lda AreaType                ;get area type
           beq ExCannon                ;if water type area, branch to leave
           ldx #$02
ThreeSChk: stx ObjectOffset            ;start at third enemy slot
           lda Enemy_Flag,x            ;check enemy buffer flag
           bne Chk_BB                  ;if set, branch to check enemy
           lda PseudoRandomBitReg+1,x  ;otherwise get part of LSFR
           ldy SecondaryHardMode       ;get secondary hard mode flag, use as offset
           and CannonBitmasks,y        ;mask out bits of LSFR as decided by flag
           cmp #$06                    ;check to see if lower nybble is above certain value
           bcs Chk_BB                  ;if so, branch to check enemy
           tay                         ;transfer masked contents of LSFR to Y as pseudorandom offset
           lda Cannon_PageLoc,y        ;get page location
           beq Chk_BB                  ;if not set or on page 0, branch to check enemy
           lda Cannon_Timer,y          ;get cannon timer
           beq FireCannon              ;if expired, branch to fire cannon
           sbc #$00                    ;otherwise subtract borrow (note carry will always be clear here)
           sta Cannon_Timer,y          ;to count timer down
           jmp Chk_BB                  ;then jump ahead to check enemy

FireCannon:
          lda TimerControl           ;if master timer control set,
          bne Chk_BB                 ;branch to check enemy
          lda #$0e                   ;otherwise we start creating one
          sta Cannon_Timer,y         ;first, reset cannon timer
          lda Cannon_PageLoc,y       ;get page location of cannon
          sta Enemy_PageLoc,x        ;save as page location of bullet bill
          lda Cannon_X_Position,y    ;get horizontal coordinate of cannon
          sta Enemy_X_Position,x     ;save as horizontal coordinate of bullet bill
          lda Cannon_Y_Position,y    ;get vertical coordinate of cannon
          sec
          sbc #$08                   ;subtract eight pixels (because enemies are 24 pixels tall)
          sta Enemy_Y_Position,x     ;save as vertical coordinate of bullet bill
          lda #$01
          sta Enemy_Y_HighPos,x      ;set vertical high byte of bullet bill
          sta Enemy_Flag,x           ;set buffer flag
          lsr                        ;shift right once to init A
          sta Enemy_State,x          ;then initialize enemy's state
          lda #$09
          sta Enemy_BoundBoxCtrl,x   ;set bounding box size control for bullet bill
          lda #BulletBill_CannonVar
          sta Enemy_ID,x             ;load identifier for bullet bill (cannon variant)
          jmp Next3Slt               ;move onto next slot
Chk_BB:   lda Enemy_ID,x             ;check enemy identifier for bullet bill (cannon variant)
          cmp #BulletBill_CannonVar
          bne Next3Slt               ;if not found, branch to get next slot
          jsr OffscreenBoundsCheck   ;otherwise, check to see if it went offscreen
          lda Enemy_Flag,x           ;check enemy buffer flag
          beq Next3Slt               ;if not set, branch to get next slot
          jsr GetEnemyOffscreenBits  ;otherwise, get offscreen information
          jsr BulletBillHandler      ;then do sub to handle bullet bill
Next3Slt: dex                        ;move onto next slot
          bpl ThreeSChk              ;do this until first three slots are checked
ExCannon: rts                        ;then leave

;--------------------------------

BulletBillXSpdData:
      .db $18, $e8

BulletBillHandler:
           lda TimerControl          ;if master timer control set,
           bne RunBBSubs             ;branch to run subroutines except movement sub
           lda Enemy_State,x
           bne ChkDSte               ;if bullet bill's state set, branch to check defeated state
           lda Enemy_OffscreenBits   ;otherwise load offscreen bits
           and #%00001100            ;mask out bits
           cmp #%00001100            ;check to see if all bits are set
           beq KillBB                ;if so, branch to kill this object
           ldy #$01                  ;set to move right by default
           jsr PlayerEnemyDiff       ;get horizontal difference between player and bullet bill
           bmi SetupBB               ;if enemy to the left of player, branch
           iny                       ;otherwise increment to move left
SetupBB:   sty Enemy_MovingDir,x     ;set bullet bill's moving direction
           dey                       ;decrement to use as offset
           lda BulletBillXSpdData,y  ;get horizontal speed based on moving direction
           sta Enemy_X_Speed,x       ;and store it
           lda $00                   ;get horizontal difference
           adc #$28                  ;add 40 pixels
           cmp #$50                  ;if less than a certain amount, player is too close
           bcc KillBB                ;to cannon either on left or right side, thus branch
           lda #$01
           sta Enemy_State,x         ;otherwise set bullet bill's state
           lda #$0a
           sta EnemyFrameTimer,x     ;set enemy frame timer
           lda #Sfx_Blast
           sta Square2SoundQueue     ;play fireworks/gunfire sound
ChkDSte:   lda Enemy_State,x         ;check enemy state for d5 set
           and #%00100000
           beq BBFly                 ;if not set, skip to move horizontally
           jsr MoveD_EnemyVertically ;otherwise do sub to move bullet bill vertically
BBFly:     jsr MoveEnemyHorizontally ;do sub to move bullet bill horizontally
RunBBSubs: jsr GetEnemyOffscreenBits ;get offscreen information
           jsr RelativeEnemyPosition ;get relative coordinates
           jsr GetEnemyBoundBox      ;get bounding box coordinates
           jsr PlayerEnemyCollision  ;handle player to enemy collisions
           jmp EnemyGfxHandler       ;draw the bullet bill and leave
KillBB:    jsr EraseEnemyObject      ;kill bullet bill and leave
           rts

;--------------------------------
;$00 - used to set downward force
;$01 - used to set upward force (residual)
;$02 - used to set maximum speed

ProcHammerObj:
          lda TimerControl           ;if master timer control set
          bne RunHSubs               ;skip all of this code and go to last subs at the end
          lda Misc_State,x           ;otherwise get hammer's state
          and #%01111111             ;mask out d7
          ldy HammerEnemyOffset,x    ;get enemy object offset that spawned this hammer
          cmp #$02                   ;check hammer's state
          beq SetHSpd                ;if currently at 2, branch
          bcs SetHPos                ;if greater than 2, branch elsewhere
          txa
          clc                        ;add 13 bytes to use
          adc #$0d                   ;proper misc object
          tax                        ;return offset to X
          lda #$10
          sta $00                    ;set downward movement force
          lda #$0f
          sta $01                    ;set upward movement force (not used)
          lda #$04
          sta $02                    ;set maximum vertical speed
          lda #$00                   ;set A to impose gravity on hammer
          jsr ImposeGravity          ;do sub to impose gravity on hammer and move vertically
          jsr MoveObjectHorizontally ;do sub to move it horizontally
          ldx ObjectOffset           ;get original misc object offset
          jmp RunAllH                ;branch to essential subroutines
SetHSpd:  lda #$fe
          sta Misc_Y_Speed,x         ;set hammer's vertical speed
          lda Enemy_State,y          ;get enemy object state
          and #%11110111             ;mask out d3
          sta Enemy_State,y          ;store new state
          ldx Enemy_MovingDir,y      ;get enemy's moving direction
          dex                        ;decrement to use as offset
          lda HammerXSpdData,x       ;get proper speed to use based on moving direction
          ldx ObjectOffset           ;reobtain hammer's buffer offset
          sta Misc_X_Speed,x         ;set hammer's horizontal speed
SetHPos:  dec Misc_State,x           ;decrement hammer's state
          lda Enemy_X_Position,y     ;get enemy's horizontal position
          clc
          adc #$02                   ;set position 2 pixels to the right
          sta Misc_X_Position,x      ;store as hammer's horizontal position
          lda Enemy_PageLoc,y        ;get enemy's page location
          adc #$00                   ;add carry
          sta Misc_PageLoc,x         ;store as hammer's page location
          lda Enemy_Y_Position,y     ;get enemy's vertical position
          sec
          sbc #$0a                   ;move position 10 pixels upward
          sta Misc_Y_Position,x      ;store as hammer's vertical position
          lda #$01
          sta Misc_Y_HighPos,x       ;set hammer's vertical high byte
          bne RunHSubs               ;unconditional branch to skip first routine
RunAllH:  jsr PlayerHammerCollision  ;handle collisions
RunHSubs: jsr GetMiscOffscreenBits   ;get offscreen information
          jsr RelativeMiscPosition   ;get relative coordinates
          jsr GetMiscBoundBox        ;get bounding box coordinates
          jsr DrawHammer             ;draw the hammer
          rts                        ;and we are done here

;-------------------------------------------------------------------------------------

MiscObjectsCore:
          ldx #$08          ;set at end of misc object buffer
MiscLoop: stx ObjectOffset  ;store misc object offset here
          lda Misc_State,x  ;check misc object state
          beq MiscLoopBack  ;branch to check next slot
          asl               ;otherwise shift d7 into carry
          bcc ProcJumpCoin  ;if d7 not set, jumping coin, thus skip to rest of code here
          jsr ProcHammerObj ;otherwise go to process hammer,
          jmp MiscLoopBack  ;then check next slot

;--------------------------------
;$00 - used to set downward force
;$01 - used to set upward force (residual)
;$02 - used to set maximum speed

ProcJumpCoin:
           ldy Misc_State,x          ;check misc object state
           dey                       ;decrement to see if it's set to 1
           beq JCoinRun              ;if so, branch to handle jumping coin
           inc Misc_State,x          ;otherwise increment state to either start off or as timer
           lda Misc_X_Position,x     ;get horizontal coordinate for misc object
           clc                       ;whether its jumping coin (state 0 only) or floatey number
           adc ScrollAmount          ;add current scroll speed
           sta Misc_X_Position,x     ;store as new horizontal coordinate
           lda Misc_PageLoc,x        ;get page location
           adc #$00                  ;add carry
           sta Misc_PageLoc,x        ;store as new page location
           lda Misc_State,x
           cmp #$30                  ;check state of object for preset value
           bne RunJCSubs             ;if not yet reached, branch to subroutines
           lda #$00
           sta Misc_State,x          ;otherwise nullify object state
           jmp MiscLoopBack          ;and move onto next slot
JCoinRun:  txa             
           clc                       ;add 13 bytes to offset for next subroutine
           adc #$0d
           tax
           lda #$50                  ;set downward movement amount
           sta $00
           lda #$06                  ;set maximum vertical speed
           sta $02
           lsr                       ;divide by 2 and set
           sta $01                   ;as upward movement amount (apparently residual)
           lda #$00                  ;set A to impose gravity on jumping coin
           jsr ImposeGravity         ;do sub to move coin vertically and impose gravity on it
           ldx ObjectOffset          ;get original misc object offset
           lda Misc_Y_Speed,x        ;check vertical speed
           cmp #$05
           bne RunJCSubs             ;if not moving downward fast enough, keep state as-is
           inc Misc_State,x          ;otherwise increment state to change to floatey number
RunJCSubs: jsr RelativeMiscPosition  ;get relative coordinates
           jsr GetMiscOffscreenBits  ;get offscreen information
           jsr GetMiscBoundBox       ;get bounding box coordinates (why?)
           jsr JCoinGfxHandler       ;draw the coin or floatey number

MiscLoopBack: 
           dex                       ;decrement misc object offset
           bpl MiscLoop              ;loop back until all misc objects handled
           rts                       ;then leave

;-------------------------------------------------------------------------------------

PRAC_GiveOneCoin:
PRAC_AddToScore:
RedrawFrameNumbers:
GetSBNybbles:
		;
		; Update frame
		; 
		lda FrameCounter
		jsr DivByTen
		sta DisplayDigits+FRAME_NUMBER_OFFSET
		txa
		jsr DivByTen
		sta DisplayDigits+FRAME_NUMBER_OFFSET-1
		stx DisplayDigits+FRAME_NUMBER_OFFSET-2
		;
		; Print it i think...
		;
      lda PracticeFlags
      and #$80
      ora #$02
UpdateNumber:
		jsr PrintStatusBarNumbers ;print status bar numbers based on nybbles, whatever they be
NoZSup: ldx ObjectOffset          ;get enemy object buffer offset
		rts


EnemyAddrHOffsets:
      .db $1f, $06, $1c, $00

PRAC_GetAreaDataAddrs:
            lda AreaPointer          ;use 2 MSB for Y
            jsr GetAreaType
            tay
            lda AreaPointer          ;mask out all but 5 LSB
            and #%00011111
            sta AreaAddrsLOffset     ;save as low offset
            lda EnemyAddrHOffsets,y  ;load base value with 2 altered MSB,
            clc                      ;then add base value to 5 LSB, result
            adc AreaAddrsLOffset     ;becomes offset for level data
            tay
            lda EnemyDataAddrLow,y   ;use offset to load pointer
            sta EnemyDataLow
            lda EnemyDataAddrHigh,y
            sta EnemyDataHigh
            ldy AreaType             ;use area type as offset
            lda AreaDataHOffsets,y   ;do the same thing but with different base value
            clc
            adc AreaAddrsLOffset        
            tay
            lda AreaDataAddrLow,y    ;use this offset to load another pointer
            sta AreaDataLow
            lda AreaDataAddrHigh,y
            sta AreaDataHigh
            ldy #$00                 ;load first byte of header
            lda (AreaData),y     
            pha                      ;save it to the stack for now
            and #%00000111           ;save 3 LSB for foreground scenery or bg color control
            cmp #$04
            bcc StoreFore
            sta BackgroundColorCtrl  ;if 4 or greater, save value here as bg color control
            lda #$00
StoreFore:  sta ForegroundScenery    ;if less, save value here as foreground scenery
            pla                      ;pull byte from stack and push it back
            pha
            and #%00111000           ;save player entrance control bits
            lsr                      ;shift bits over to LSBs
            lsr
            lsr
            sta PlayerEntranceCtrl       ;save value here as player entrance control
            pla                      ;pull byte again but do not push it back
            and #%11000000           ;save 2 MSB for game timer setting
            clc
            rol                      ;rotate bits over to LSBs
            rol
            rol
            sta GameTimerSetting     ;save value here as game timer setting
            iny
            lda (AreaData),y         ;load second byte of header
            pha                      ;save to stack
            and #%00001111           ;mask out all but lower nybble
            sta TerrainControl
            pla                      ;pull and push byte to copy it to A
            pha
            and #%00110000           ;save 2 MSB for background scenery type
            lsr
            lsr                      ;shift bits to LSBs
            lsr
            lsr
            sta BackgroundScenery    ;save as background scenery
            pla           
            and #%11000000
            clc
            rol                      ;rotate bits over to LSBs
            rol
            rol
            cmp #%00000011           ;if set to 3, store here
            bne StoreStyle           ;and nullify other value
            sta CloudTypeOverride    ;otherwise store value in other place
            lda #$00
StoreStyle: sta AreaStyle
            lda AreaDataLow          ;increment area data address by 2 bytes
            clc
            adc #$02
            sta AreaDataLow
            lda AreaDataHigh
            adc #$00
            sta AreaDataHigh
            rts

PRAC_HandlePipeEntry:
         lda Up_Down_Buttons       ;check saved controller bits from earlier
         and #%00000100            ;for pressing down
         beq ExPipeE               ;if not pressing down, branch to leave
         lda $00
         cmp #$11                  ;check right foot metatile for warp pipe right metatile
         bne ExPipeE               ;branch to leave if not found
         lda $01
         cmp #$10                  ;check left foot metatile for warp pipe left metatile
         bne ExPipeE               ;branch to leave if not found
         lda #$30
         sta ChangeAreaTimer       ;set timer for change of area
         lda #$03
         sta GameEngineSubroutine  ;set to run vertical pipe entry routine on next frame
         jsr RedrawAll
         lda #Sfx_PipeDown_Injury
         sta Square1SoundQueue     ;load pipedown/injury sound
         lda #%00100000
         sta Player_SprAttrib      ;set background priority bit in player's attributes
         lda WarpZoneControl       ;check warp zone control
         beq ExPipeE               ;branch to leave if none found
         and #%00000011            ;mask out all but 2 LSB
         asl
         asl                       ;multiply by four
         tax                       ;save as offset to warp zone numbers (starts at left pipe)
         lda Player_X_Position     ;get player's horizontal position
         cmp #$60      
         bcc GetWNum               ;if player at left, not near middle, use offset and skip ahead
         inx                       ;otherwise increment for middle pipe
         cmp #$a0      
         bcc GetWNum               ;if player at middle, but not too far right, use offset and skip
         inx                       ;otherwise increment for last pipe
GetWNum: ldy WarpZoneNumbers,x     ;get warp zone numbers
         dey                       ;decrement for use as world number
         sty WorldNumber           ;store as world number and offset
         ldx WorldAddrOffsets,y    ;get offset to where this world's area offsets are
         lda AreaAddrOffsets,x     ;get area offset based on world offset
         sta AreaPointer           ;store area offset here to be used to change areas
         lda #Silence
         sta EventMusicQueue       ;silence music
         lda #$00
         sta EntrancePage          ;initialize starting page number
         sta AreaNumber            ;initialize area number used for area address offset
         sta LevelNumber           ;initialize level number used for world display
         sta AltEntranceControl    ;initialize mode of entry
         inc Hidden1UpFlag         ;set flag for hidden 1-up blocks
         inc FetchNewGameTimerFlag ;set flag to load new game timer
ExPipeE: rts                       ;leave!!!

;-------------------------------------------------------------------------------------

EnemiesAndLoopsCore:
            lda Enemy_Flag,x         ;check data here for MSB set
            pha                      ;save in stack
            asl
            bcs ChkBowserF           ;if MSB set in enemy flag, branch ahead of jumps
            pla                      ;get from stack
            beq ChkAreaTsk           ;if data zero, branch
            jmp RunEnemyObjectsCore  ;otherwise, jump to run enemy subroutines
ChkAreaTsk: lda AreaParserTaskNum    ;check number of tasks to perform
            and #$07
            cmp #$07                 ;if at a specific task, jump and leave
            beq ExitELCore
            jmp ProcLoopCommand      ;otherwise, jump to process loop command/load enemies
ChkBowserF: pla                      ;get data from stack
            and #%00001111           ;mask out high nybble
            tay
            lda Enemy_Flag,y         ;use as pointer and load same place with different offset
            bne ExitELCore
            sta Enemy_Flag,x         ;if second enemy flag not set, also clear first one
ExitELCore: rts

;--------------------------------

;loop command data
LoopCmdWorldNumber:
      .db $03, $03, $06, $06, $06, $06, $06, $06, $07, $07, $07

LoopCmdPageNumber:
      .db $05, $09, $04, $05, $06, $08, $09, $0a, $06, $0b, $10

LoopCmdYPosition:
      .db $40, $b0, $b0, $80, $40, $40, $80, $40, $f0, $f0, $f0

ExecGameLoopback:
      lda Player_PageLoc        ;send player back four pages
      sec
      sbc #$04
      sta Player_PageLoc
      lda CurrentPageLoc        ;send current page back four pages
      sec
      sbc #$04
      sta CurrentPageLoc
      lda ScreenLeft_PageLoc    ;subtract four from page location
      sec                       ;of screen's left border
      sbc #$04
      sta ScreenLeft_PageLoc
      lda ScreenRight_PageLoc   ;do the same for the page location
      sec                       ;of screen's right border
      sbc #$04
      sta ScreenRight_PageLoc
      lda AreaObjectPageLoc     ;subtract four from page control
      sec                       ;for area objects
      sbc #$04
      sta AreaObjectPageLoc
      lda #$00                  ;initialize page select for both
      sta EnemyObjectPageSel    ;area and enemy objects
      sta AreaObjectPageSel
      sta EnemyDataOffset       ;initialize enemy object data offset
      sta EnemyObjectPageLoc    ;and enemy object page control
      lda AreaDataOfsLoopback,y ;adjust area object offset based on
      sta AreaDataOffset        ;which loop command we encountered
      rts

ProcLoopCommand:
          lda LoopCommand           ;check if loop command was found
          beq ChkEnemyFrenzy
          lda CurrentColumnPos      ;check to see if we're still on the first page
          bne ChkEnemyFrenzy        ;if not, do not loop yet
          ldy #$0b                  ;start at the end of each set of loop data
FindLoop: dey
          bmi ChkEnemyFrenzy        ;if all data is checked and not match, do not loop
          lda WorldNumber           ;check to see if one of the world numbers
          cmp LoopCmdWorldNumber,y  ;matches our current world number
          bne FindLoop
          lda CurrentPageLoc        ;check to see if one of the page numbers
          cmp LoopCmdPageNumber,y   ;matches the page we're currently on
          bne FindLoop
          lda Player_Y_Position     ;check to see if the player is at the correct position
          cmp LoopCmdYPosition,y    ;if not, branch to check for world 7
          bne WrongChk
          lda Player_State          ;check to see if the player is
          cmp #$00                  ;on solid ground (i.e. not jumping or falling)
          bne WrongChk              ;if not, player fails to pass loop, and loopback
          lda WorldNumber           ;are we in world 7? (check performed on correct
          cmp #World7               ;vertical position and on solid ground)
          bne InitMLp               ;if not, initialize flags used there, otherwise
          inc MultiLoopCorrectCntr  ;increment counter for correct progression
IncMLoop: inc MultiLoopPassCntr     ;increment master multi-part counter
          lda MultiLoopPassCntr     ;have we done all three parts?
          cmp #$03
          bne InitLCmd              ;if not, skip this part
          lda MultiLoopCorrectCntr  ;if so, have we done them all correctly?
          cmp #$03
          beq InitMLp               ;if so, branch past unnecessary check here
          bne DoLpBack              ;unconditional branch if previous branch fails
WrongChk: lda WorldNumber           ;are we in world 7? (check performed on
          cmp #World7               ;incorrect vertical position or not on solid ground)
          beq IncMLoop
DoLpBack: jsr ExecGameLoopback      ;if player is not in right place, loop back
          jsr KillAllEnemies
InitMLp:  lda #$00                  ;initialize counters used for multi-part loop commands
          sta MultiLoopPassCntr
          sta MultiLoopCorrectCntr
InitLCmd: lda #$00                  ;initialize loop command flag
          sta LoopCommand

;--------------------------------

ChkEnemyFrenzy:
      lda EnemyFrenzyQueue  ;check for enemy object in frenzy queue
      beq ProcessEnemyData  ;if not, skip this part
      sta Enemy_ID,x        ;store as enemy object identifier here
      lda #$01
      sta Enemy_Flag,x      ;activate enemy object flag
      lda #$00
      sta Enemy_State,x     ;initialize state and frenzy queue
      sta EnemyFrenzyQueue
      jmp InitEnemyObject   ;and then jump to deal with this enemy

;--------------------------------
;$06 - used to hold page location of extended right boundary
;$07 - used to hold high nybble of position of extended right boundary

ProcessEnemyData:
        ldy EnemyDataOffset      ;get offset of enemy object data
        lda (EnemyData),y        ;load first byte
        cmp #$ff                 ;check for EOD terminator
        bne CheckEndofBuffer
        jmp CheckFrenzyBuffer    ;if found, jump to check frenzy buffer, otherwise

CheckEndofBuffer:
        and #%00001111           ;check for special row $0e
        cmp #$0e
        beq CheckRightBounds     ;if found, branch, otherwise
        cpx #$05                 ;check for end of buffer
        bcc CheckRightBounds     ;if not at end of buffer, branch
        iny
        lda (EnemyData),y        ;check for specific value here
        and #%00111111           ;not sure what this was intended for, exactly
        cmp #$2e                 ;this part is quite possibly residual code
        beq CheckRightBounds     ;but it has the effect of keeping enemies out of
        rts                      ;the sixth slot

CheckRightBounds:
        lda ScreenRight_X_Pos    ;add 48 to pixel coordinate of right boundary
        clc
        adc #$30
        and #%11110000           ;store high nybble
        sta $07
        lda ScreenRight_PageLoc  ;add carry to page location of right boundary
        adc #$00
        sta $06                  ;store page location + carry
        ldy EnemyDataOffset
        iny
        lda (EnemyData),y        ;if MSB of enemy object is clear, branch to check for row $0f
        asl
        bcc CheckPageCtrlRow
        lda EnemyObjectPageSel   ;if page select already set, do not set again
        bne CheckPageCtrlRow
        inc EnemyObjectPageSel   ;otherwise, if MSB is set, set page select 
        inc EnemyObjectPageLoc   ;and increment page control

CheckPageCtrlRow:
        dey
        lda (EnemyData),y        ;reread first byte
        and #$0f
        cmp #$0f                 ;check for special row $0f
        bne PositionEnemyObj     ;if not found, branch to position enemy object
        lda EnemyObjectPageSel   ;if page select set,
        bne PositionEnemyObj     ;branch without reading second byte
        iny
        lda (EnemyData),y        ;otherwise, get second byte, mask out 2 MSB
        and #%00111111
        sta EnemyObjectPageLoc   ;store as page control for enemy object data
        inc EnemyDataOffset      ;increment enemy object data offset 2 bytes
        inc EnemyDataOffset
        inc EnemyObjectPageSel   ;set page select for enemy object data and 
        jmp ProcLoopCommand      ;jump back to process loop commands again

PositionEnemyObj:
        lda EnemyObjectPageLoc   ;store page control as page location
        sta Enemy_PageLoc,x      ;for enemy object
        lda (EnemyData),y        ;get first byte of enemy object
        and #%11110000
        sta Enemy_X_Position,x   ;store column position
        cmp ScreenRight_X_Pos    ;check column position against right boundary
        lda Enemy_PageLoc,x      ;without subtracting, then subtract borrow
        sbc ScreenRight_PageLoc  ;from page location
        bcs CheckRightExtBounds  ;if enemy object beyond or at boundary, branch
        lda (EnemyData),y
        and #%00001111           ;check for special row $0e
        cmp #$0e                 ;if found, jump elsewhere
        beq ParseRow0e
        jmp CheckThreeBytes      ;if not found, unconditional jump

CheckRightExtBounds:
        lda $07                  ;check right boundary + 48 against
        cmp Enemy_X_Position,x   ;column position without subtracting,
        lda $06                  ;then subtract borrow from page control temp
        sbc Enemy_PageLoc,x      ;plus carry
        bcc CheckFrenzyBuffer    ;if enemy object beyond extended boundary, branch
        lda #$01                 ;store value in vertical high byte
        sta Enemy_Y_HighPos,x
        lda (EnemyData),y        ;get first byte again
        asl                      ;multiply by four to get the vertical
        asl                      ;coordinate
        asl
        asl
        sta Enemy_Y_Position,x
        cmp #$e0                 ;do one last check for special row $0e
        beq ParseRow0e           ;(necessary if branched to $c1cb)
        iny
        lda (EnemyData),y        ;get second byte of object
        and #%01000000           ;check to see if hard mode bit is set
        beq CheckForEnemyGroup   ;if not, branch to check for group enemy objects
        lda SecondaryHardMode    ;if set, check to see if secondary hard mode flag
        beq Inc2B                ;is on, and if not, branch to skip this object completely

CheckForEnemyGroup:
        lda (EnemyData),y      ;get second byte and mask out 2 MSB
        and #%00111111
        cmp #$37               ;check for value below $37
        bcc BuzzyBeetleMutate
        cmp #$3f               ;if $37 or greater, check for value
        bcc DoGroup            ;below $3f, branch if below $3f

BuzzyBeetleMutate:
        cmp #Goomba          ;if below $37, check for goomba
        bne StrID            ;value ($3f or more always fails)
        ldy PrimaryHardMode  ;check if primary hard mode flag is set
        beq StrID            ;and if so, change goomba to buzzy beetle
        lda #BuzzyBeetle
StrID:  sta Enemy_ID,x       ;store enemy object number into buffer
        lda #$01
        sta Enemy_Flag,x     ;set flag for enemy in buffer
        jsr InitEnemyObject
        lda Enemy_Flag,x     ;check to see if flag is set
        bne Inc2B            ;if not, leave, otherwise branch
        rts

CheckFrenzyBuffer:
        lda EnemyFrenzyBuffer    ;if enemy object stored in frenzy buffer
        bne StrFre               ;then branch ahead to store in enemy object buffer
        lda VineFlagOffset       ;otherwise check vine flag offset
        cmp #$01
        bne ExEPar               ;if other value <> 1, leave
        lda #VineObject          ;otherwise put vine in enemy identifier
StrFre: sta Enemy_ID,x           ;store contents of frenzy buffer into enemy identifier value

InitEnemyObject:
        lda #$00                 ;initialize enemy state
        sta Enemy_State,x
        jsr CheckpointEnemyID    ;jump ahead to run jump engine and subroutines
ExEPar: rts                      ;then leave

DoGroup:
        jmp HandleGroupEnemies   ;handle enemy group objects

ParseRow0e:
        iny                      ;increment Y to load third byte of object
        iny
        lda (EnemyData),y
        lsr                      ;move 3 MSB to the bottom, effectively
        lsr                      ;making %xxx00000 into %00000xxx
        lsr
        lsr
        lsr
        cmp WorldNumber          ;is it the same world number as we're on?
        bne NotUse               ;if not, do not use (this allows multiple uses
        dey                      ;of the same area, like the underground bonus areas)
        lda (EnemyData),y        ;otherwise, get second byte and use as offset
        sta AreaPointer          ;to addresses for level and enemy object data
        iny
        lda (EnemyData),y        ;get third byte again, and this time mask out
        and #%00011111           ;the 3 MSB from before, save as page number to be
        sta EntrancePage         ;used upon entry to area, if area is entered
NotUse: jmp Inc3B

CheckThreeBytes:
        ldy EnemyDataOffset      ;load current offset for enemy object data
        lda (EnemyData),y        ;get first byte
        and #%00001111           ;check for special row $0e
        cmp #$0e
        bne Inc2B
Inc3B:  inc EnemyDataOffset      ;if row = $0e, increment three bytes
Inc2B:  inc EnemyDataOffset      ;otherwise increment two bytes
        inc EnemyDataOffset
        lda #$00                 ;init page select for enemy objects
        sta EnemyObjectPageSel
        ldx ObjectOffset         ;reload current offset in enemy buffers
        rts                      ;and leave
;--------------------------------

Bitmasks:
      .db %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000

Enemy17YPosData:
      .db $40, $30, $90, $50, $20, $60, $a0, $70

SwimCC_IDData:
      .db $0a, $0b

BulletBillCheepCheep:
         lda FrenzyEnemyTimer      ;if timer not expired yet, branch to leave
         bne ExF17
         lda AreaType              ;are we in a water-type level?
         bne DoBulletBills         ;if not, branch elsewhere
         cpx #$03                  ;are we past third enemy slot?
         bcs ExF17                 ;if so, branch to leave
         ldy #$00                  ;load default offset
         lda PseudoRandomBitReg,x
         cmp #$aa                  ;check first part of LSFR against preset value
         bcc ChkW2                 ;if less than preset, do not increment offset
         iny                       ;otherwise increment
ChkW2:   lda WorldNumber           ;check world number
         cmp #World2
         beq Get17ID               ;if we're on world 2, do not increment offset
         iny                       ;otherwise increment
Get17ID: tya
         and #%00000001            ;mask out all but last bit of offset
         tay
         lda SwimCC_IDData,y       ;load identifier for cheep-cheeps
Set17ID: sta Enemy_ID,x            ;store whatever's in A as enemy identifier
         lda BitMFilter
         cmp #$ff                  ;if not all bits set, skip init part and compare bits
         bne GetRBit
         lda #$00                  ;initialize vertical position filter
         sta BitMFilter
GetRBit: lda PseudoRandomBitReg,x  ;get first part of LSFR
         and #%00000111            ;mask out all but 3 LSB
ChkRBit: tay                       ;use as offset
         lda Bitmasks,y            ;load bitmask
         bit BitMFilter            ;perform AND on filter without changing it
         beq AddFBit
         iny                       ;increment offset
         tya
         and #%00000111            ;mask out all but 3 LSB thus keeping it 0-7
         jmp ChkRBit               ;do another check
AddFBit: ora BitMFilter            ;add bit to already set bits in filter
         sta BitMFilter            ;and store
         lda Enemy17YPosData,y     ;load vertical position using offset
         jsr PutAtRightExtent      ;set vertical position and other values
         sta Enemy_YMF_Dummy,x     ;initialize dummy variable
         lda #$20                  ;set timer
         sta FrenzyEnemyTimer
         jmp CheckpointEnemyID     ;process our new enemy object


DoBulletBills:
          ldy #$ff                   ;start at beginning of enemy slots
BB_SLoop: iny                        ;move onto the next slot
          cpy #$05                   ;branch to play sound if we've done all slots
          bcs FireBulletBill
          lda Enemy_Flag,y           ;if enemy buffer flag not set,
          beq BB_SLoop               ;loop back and check another slot
          lda Enemy_ID,y
          cmp #BulletBill_FrenzyVar  ;check enemy identifier for
          bne BB_SLoop               ;bullet bill object (frenzy variant)
ExF17:    rts                        ;if found, leave

FireBulletBill:
      lda Square2SoundQueue
      ora #Sfx_Blast            ;play fireworks/gunfire sound
      sta Square2SoundQueue
      lda #BulletBill_FrenzyVar ;load identifier for bullet bill object
      bne Set17ID               ;unconditional branch

;--------------------------------
;$00 - used to store Y position of group enemies
;$01 - used to store enemy ID
;$02 - used to store page location of right side of screen
;$03 - used to store X position of right side of screen

HandleGroupEnemies:
        ldy #$00                  ;load value for green koopa troopa
        sec
        sbc #$37                  ;subtract $37 from second byte read
        pha                       ;save result in stack for now
        cmp #$04                  ;was byte in $3b-$3e range?
        bcs SnglID                ;if so, branch
        pha                       ;save another copy to stack
        ldy #Goomba               ;load value for goomba enemy
        lda PrimaryHardMode       ;if primary hard mode flag not set,
        beq PullID                ;branch, otherwise change to value
        ldy #BuzzyBeetle          ;for buzzy beetle
PullID: pla                       ;get second copy from stack
SnglID: sty $01                   ;save enemy id here
        ldy #$b0                  ;load default y coordinate
        and #$02                  ;check to see if d1 was set
        beq SetYGp                ;if so, move y coordinate up,
        ldy #$70                  ;otherwise branch and use default
SetYGp: sty $00                   ;save y coordinate here
        lda ScreenRight_PageLoc   ;get page number of right edge of screen
        sta $02                   ;save here
        lda ScreenRight_X_Pos     ;get pixel coordinate of right edge
        sta $03                   ;save here
        ldy #$02                  ;load two enemies by default
        pla                       ;get first copy from stack
        lsr                       ;check to see if d0 was set
        bcc CntGrp                ;if not, use default value
        iny                       ;otherwise increment to three enemies
CntGrp: sty NumberofGroupEnemies  ;save number of enemies here
GrLoop: ldx #$ff                  ;start at beginning of enemy buffers
GSltLp: inx                       ;increment and branch if past
        cpx #$05                  ;end of buffers
        bcs NextED
        lda Enemy_Flag,x          ;check to see if enemy is already
        bne GSltLp                ;stored in buffer, and branch if so
        lda $01
        sta Enemy_ID,x            ;store enemy object identifier
        lda $02
        sta Enemy_PageLoc,x       ;store page location for enemy object
        lda $03
        sta Enemy_X_Position,x    ;store x coordinate for enemy object
        clc
        adc #$18                  ;add 24 pixels for next enemy
        sta $03
        lda $02                   ;add carry to page location for
        adc #$00                  ;next enemy
        sta $02
        lda $00                   ;store y coordinate for enemy object
        sta Enemy_Y_Position,x
        lda #$01                  ;activate flag for buffer, and
        sta Enemy_Y_HighPos,x     ;put enemy within the screen vertically
        sta Enemy_Flag,x
        jsr CheckpointEnemyID     ;process each enemy object separately
        dec NumberofGroupEnemies  ;do this until we run out of enemy objects
        bne GrLoop
NextED: jmp Inc2B                 ;jump to increment data offset and leave

; ------------------

CheckpointEnemyID:
        lda Enemy_ID,x
        cmp #$15                     ;check enemy object identifier for $15 or greater
        bcs InitEnemyRoutines        ;and branch straight to the jump engine if found
        tay                          ;save identifier in Y register for now
        lda Enemy_Y_Position,x
        adc #$08                     ;add eight pixels to what will eventually be the
        sta Enemy_Y_Position,x       ;enemy object's vertical coordinate ($00-$14 only)
        lda #$01
        sta EnemyOffscrBitsMasked,x  ;set offscreen masked bit
        tya                          ;get identifier back and use as offset for jump engine

InitEnemyRoutines:
        jsr JumpEngine
      
;jump engine table for newly loaded enemy objects

      .dw InitNormalEnemy  ;for objects $00-$0f
      .dw InitNormalEnemy
      .dw InitNormalEnemy
      .dw InitRedKoopa
      .dw NoInitCode
      .dw InitHammerBro
      .dw InitGoomba
      .dw InitBloober
      .dw InitBulletBill
      .dw NoInitCode
      .dw InitCheepCheep
      .dw InitCheepCheep
      .dw InitPodoboo
      .dw InitPiranhaPlant
      .dw InitJumpGPTroopa
      .dw InitRedPTroopa

      .dw InitHorizFlySwimEnemy  ;for objects $10-$1f
      .dw InitLakitu
      .dw InitEnemyFrenzy
      .dw NoInitCode
      .dw InitEnemyFrenzy
      .dw InitEnemyFrenzy
      .dw InitEnemyFrenzy
      .dw InitEnemyFrenzy
      .dw EndFrenzy
      .dw NoInitCode
      .dw NoInitCode
      .dw InitShortFirebar
      .dw InitShortFirebar
      .dw InitShortFirebar
      .dw InitShortFirebar
      .dw InitLongFirebar

      .dw NoInitCode ;for objects $20-$2f
      .dw NoInitCode
      .dw NoInitCode
      .dw NoInitCode
      .dw InitBalPlatform
      .dw InitVertPlatform
      .dw LargeLiftUp
      .dw LargeLiftDown
      .dw InitHoriPlatform
      .dw InitDropPlatform
      .dw InitHoriPlatform
      .dw PlatLiftUp
      .dw PlatLiftDown
      .dw InitBowser
      .dw PwrUpJmp   ;possibly dummy value
      .dw Setup_Vine

      .dw NoInitCode ;for objects $30-$36
      .dw NoInitCode
      .dw NoInitCode
      .dw NoInitCode
      .dw NoInitCode
      .dw InitRetainerObj
      .dw EndOfEnemyInitCode

;--------------------------------

InitEnemyFrenzy:
      lda Enemy_ID,x        ;load enemy identifier
      sta EnemyFrenzyBuffer ;save in enemy frenzy buffer
      sec
      sbc #$12              ;subtract 12 and use as offset for jump engine
      jsr JumpEngine

;frenzy object jump table
      .dw LakituAndSpinyHandler
      .dw NoFrenzyCode
      .dw InitFlyingCheepCheep
      .dw InitBowserFlame
      .dw InitFireworks
      .dw BulletBillCheepCheep

;--------------------------------

IntermediatePlayerData:
        .db $58, $01, $00, $60, $ff, $04

DrawPlayer_Intermediate:
          lda IntermediatePlayerData
          sty $02
          clc
          adc $02
          sta $02
          ldx #$04                       ;store data into zero page memory
PIntLoop: lda IntermediatePlayerData+1,x   ;load data to display player as he always
          sta $03,x                      ;appears on world/lives display
          dex
          bpl PIntLoop                   ;do this until all data is loaded
          ldx #$b8                       ;load offset for small standing
          ldy #$04                       ;load sprite data offset
          jsr DrawPlayerLoop             ;draw player accordingly
          lda Sprite_Attributes+36       ;get empty sprite attributes
          ora #%01000000                 ;set horizontal flip bit for bottom-right sprite
          sta Sprite_Attributes+32       ;store and leave
          rts

;-------------------------------------------------------------------------------------

InitializeGame:
             ldy #$6f              ;clear all memory as in initialization procedure,
             jsr InitializeMemory  ;but this time, clear only as far as $076f
             ldy #$1f
ClrSndLoop:  sta SoundMemory,y     ;clear out memory used
             dey                   ;by the sound engines
             bpl ClrSndLoop
             jsr LoadAreaPointer

InitializeArea:
               ldy #$4b                 ;clear all memory again, only as far as $074b
               jsr InitializeMemory     ;this is only necessary if branching from
               ldx #$21
               lda #$00
ClrTimersLoop: sta Timers,x             ;clear out memory between
               dex                      ;$0780 and $07a1
               bpl ClrTimersLoop
               lda HalfwayPage
               ldy AltEntranceControl   ;if AltEntranceControl not set, use halfway page, if any found
               beq StartPage
               lda EntrancePage         ;otherwise use saved entry page number here
StartPage:     sta ScreenLeft_PageLoc   ;set as value here
               sta CurrentPageLoc       ;also set as current page
               sta BackloadingFlag      ;set flag here if halfway page or saved entry page number found
               jsr GetScreenPosition    ;get pixel coordinates for screen borders
               ldy #$20                 ;if on odd numbered page, use $2480 as start of rendering
               and #%00000001           ;otherwise use $2080, this address used later as name table
               beq SetInitNTHigh        ;address for rendering of game area
               ldy #$24
SetInitNTHigh: sty CurrentNTAddr_High   ;store name table address
               ldy #$80
               sty CurrentNTAddr_Low
               asl                      ;store LSB of page number in high nybble
               asl                      ;of block buffer column position
               asl
               asl
               sta BlockBufferColumnPos
               dec AreaObjectLength     ;set area object lengths for all empty
               dec AreaObjectLength+1
               dec AreaObjectLength+2
               lda #$0b                 ;set value for renderer to update 12 column sets
               sta ColumnSets           ;12 column sets = 24 metatile columns = 1 1/2 screens
               jsr GetAreaDataAddrs     ;get enemy and level addresses and load header
               lda PrimaryHardMode      ;check to see if primary hard mode has been activated
               bne SetSecHard           ;if so, activate the secondary no matter where we're at
               lda WorldNumber          ;otherwise check world number
               cmp #World5              ;if less than 5, do not activate secondary
               bcc CheckHalfway
               bne SetSecHard           ;if not equal to, then world > 5, thus activate
               lda LevelNumber          ;otherwise, world 5, so check level number
               cmp #Level3              ;if 1 or 2, do not set secondary hard mode flag
               bcc CheckHalfway
SetSecHard:    inc SecondaryHardMode    ;set secondary hard mode flag for areas 5-3 and beyond
CheckHalfway:  lda HalfwayPage
               beq DoneInitArea
               lda #$02                 ;if halfway page set, overwrite start position from header
               sta PlayerEntranceCtrl
DoneInitArea:  lda #Silence             ;silence music
               sta AreaMusicQueue
               lda #$01                 ;disable screen output
               sta DisableScreenFlag
               inc OperMode_Task        ;increment one of the modes
               ldx #4
               stx RuleIndex
               rts
;--------------------------------

PRAC_RunFireworks:
           dec ExplosionTimerCounter,x ;decrement explosion timing counter here
           bne SetupExpl               ;if not expired, skip this part
           lda #$08
           sta ExplosionTimerCounter,x ;reset counter
           inc ExplosionGfxCounter,x   ;increment explosion graphics counter
           lda ExplosionGfxCounter,x
           cmp #$03                    ;check explosion graphics counter
           bcs FireworksSoundScore     ;if at a certain point, branch to kill this object
SetupExpl: jsr RelativeEnemyPosition   ;get relative coordinates of explosion
           lda Enemy_Rel_YPos          ;copy relative coordinates
           sta Fireball_Rel_YPos       ;from the enemy object to the fireball object
           lda Enemy_Rel_XPos          ;first vertical, then horizontal
           sta Fireball_Rel_XPos
           ldy Enemy_SprDataOffset,x   ;get OAM data offset
           lda ExplosionGfxCounter,x   ;get explosion graphics counter
           jsr DrawExplosion_Fireworks ;do a sub to draw the explosion then leave
           rts

FireworksSoundScore:
      lda #$00               ;disable enemy buffer flag
      sta Enemy_Flag,x
      lda #Sfx_Blast         ;play fireworks/gunfire sound
      sta Square2SoundQueue
      rts

;--------------------------------

StarFlagYPosAdder:
      .db $00, $00, $08, $08

StarFlagXPosAdder:
      .db $00, $08, $00, $08

StarFlagTileData:
      .db $54, $55, $56, $57

PRAC_RunStarFlagObj:
      lda #$00                 ;initialize enemy frenzy buffer
      sta EnemyFrenzyBuffer
      lda StarFlagTaskControl  ;check star flag object task number here
      cmp #$05                 ;if greater than 5, branch to exit
      bcs StarFlagExit
      jsr JumpEngine           ;otherwise jump to appropriate sub
      
      .dw StarFlagExit
      .dw GameTimerFireworks
      .dw AwardGameTimerPoints
      .dw RaiseFlagSetoffFWorks
      .dw DelayToAreaEnd

GameTimerFireworks:
        ldy #$05               ;set default state for star flag object
        lda GameTimerDisplay+2 ;get game timer's last digit
        cmp #$01
        beq SetFWC             ;if last digit of game timer set to 1, skip ahead
        ldy #$03               ;otherwise load new value for state
        cmp #$03
        beq SetFWC             ;if last digit of game timer set to 3, skip ahead
        ldy #$00               ;otherwise load one more potential value for state
        cmp #$06
        beq SetFWC             ;if last digit of game timer set to 6, skip ahead
        lda #$ff               ;otherwise set value for no fireworks
SetFWC: sta FireworksCounter   ;set fireworks counter here
        sty Enemy_State,x      ;set whatever state we have in star flag object

IncrementSFTask1:
      inc StarFlagTaskControl  ;increment star flag object task number

StarFlagExit:
      rts                      ;leave

AwardGameTimerPoints:
         lda GameTimerDisplay   ;check all game timer digits for any intervals left
         ora GameTimerDisplay+1
         ora GameTimerDisplay+2
         beq IncrementSFTask1   ;if no time left on game timer at all, branch to next task
         lda FrameCounter
         and #%00000100         ;check frame counter for d2 set (skip ahead
         beq NoTTick            ;for four frames every four frames) branch if not set
         lda #Sfx_TimerTick
         sta Square2SoundQueue  ;load timer tick sound
NoTTick: ldy #$23               ;set offset here to subtract from game timer's last digit
         lda #$ff               ;set adder here to $ff, or -1, to subtract one
         sta DigitModifier+5    ;from the last digit of the game timer
         jsr DigitsMathRoutine3  ;subtract digit
         lda #4
         jmp UpdateNumber

RaiseFlagSetoffFWorks:
         lda Enemy_Y_Position,x  ;check star flag's vertical position
         cmp #$72                ;against preset value
         bcc SetoffF             ;if star flag higher vertically, branch to other code
         dec Enemy_Y_Position,x  ;otherwise, raise star flag by one pixel
         jmp DrawStarFlag        ;and skip this part here
SetoffF: lda FireworksCounter    ;check fireworks counter
         beq DrawFlagSetTimer    ;if no fireworks left to go off, skip this part
         bmi DrawFlagSetTimer    ;if no fireworks set to go off, skip this part
         lda #Fireworks
         sta EnemyFrenzyBuffer   ;otherwise set fireworks object in frenzy queue

DrawStarFlag:
         jsr RelativeEnemyPosition  ;get relative coordinates of star flag
         ldy Enemy_SprDataOffset,x  ;get OAM data offset
         ldx #$03                   ;do four sprites
DSFLoop: lda Enemy_Rel_YPos         ;get relative vertical coordinate
         clc
         adc StarFlagYPosAdder,x    ;add Y coordinate adder data
         sta Sprite_Y_Position,y    ;store as Y coordinate
         lda StarFlagTileData,x     ;get tile number
         sta Sprite_Tilenumber,y    ;store as tile number
         lda #$22                   ;set palette and background priority bits
         sta Sprite_Attributes,y    ;store as attributes
         lda Enemy_Rel_XPos         ;get relative horizontal coordinate
         clc
         adc StarFlagXPosAdder,x    ;add X coordinate adder data
         sta Sprite_X_Position,y    ;store as X coordinate
         iny
         iny                        ;increment OAM data offset four bytes
         iny                        ;for next sprite
         iny
         dex                        ;move onto next sprite
         bpl DSFLoop                ;do this until all sprites are done
         ldx ObjectOffset           ;get enemy object offset and leave
         rts

DrawFlagSetTimer:
      jsr DrawStarFlag          ;do sub to draw star flag
      lda #$06
      sta EnemyIntervalTimer,x  ;set interval timer here

IncrementSFTask2:
      jsr RedrawAll
      inc StarFlagTaskControl   ;move onto next task
      rts

DelayToAreaEnd:
      jsr DrawStarFlag          ;do sub to draw star flag
      lda EnemyIntervalTimer,x  ;if interval timer set in previous task
      bne StarFlagExit2         ;not yet expired, branch to leave
      lda EventMusicBuffer      ;if event music buffer empty,
      beq IncrementSFTask2      ;branch to increment task

StarFlagExit2:
      rts                       ;otherwise leave

;-------------------------------------------------------------------------------------
PrintHexByte:
		sta $0
		lsr
		lsr
		lsr
		lsr
		jsr DoNibble
		lda $0
DoNibble:
		and #$0f
		sta VRAM_Buffer1+3,x
		inx
DontUpdateSockHash:
		rts

UpdateSockHash:
		lda PracticeFlags
		and #$80
		beq DontUpdateSockHash
		lda IntervalTimerControl
		and #3
		cmp #2
		bne DontUpdateSockHash
ForceUpdateSockHash:      
		lda SprObject_X_MoveForce ; Player force
		sta $3
		lda SprObject_X_Position ; Player X
		sta $2
		lda SprObject_PageLoc ; Player page
		sta $1
		lda SprObject_Y_Position ; Player Y
		eor #$ff
		lsr
		lsr
		lsr
		bcc something_or_other
		pha
		clc
		lda #$80
		adc $3
		sta $3
		lda $2
		adc #2
		sta $2
		lda $1
		adc #0
		sta $1
		pla
something_or_other:
		sta $04
		asl
		asl
		adc $04
		adc $2
		sta $2
		lda $1
		adc #0
		sta $1
		ldx VRAM_Buffer1_Offset 
		bne skip_sock_hash
draw_sock_hash:
		lda #$20
		sta VRAM_Buffer1
		lda #$62 ;
		sta VRAM_Buffer1+1
		lda #$06 ; len
		sta VRAM_Buffer1+2
		ldx #0
		lda $1
		jsr PrintHexByte
		lda $2
		jsr PrintHexByte
		lda $3
		jsr PrintHexByte
		lda #0
		sta VRAM_Buffer1+3, x
		lda #$09
		sta VRAM_Buffer1_Offset
skip_sock_hash:
		rts

;-------------------------------------------------------------------------------------

PRAC_Start:
             lda #%00010000               ;init PPU control register 1 
             sta PPU_CTRL_REG1
             ldx #$ff                     ;reset stack pointer
             txs
VBlank1:     lda PPU_STATUS               ;wait two frames
             bpl VBlank1
VBlank2:     lda PPU_STATUS
             bpl VBlank2
             ldy #ColdBootOffset          ;load default cold boot pointer
ColdBoot:    jsr InitializeMemory         ;clear memory using pointer in Y
             sta SND_DELTA_REG+1          ;reset delta counter load register
             sta OperMode                 ;reset primary mode of operation
             lda #$a5                     ;set warm boot flag
             sta WarmBootValidation     
             sta PseudoRandomBitReg       ;set seed for pseudorandom register
             lda #%00001111
             sta SND_MASTERCTRL_REG       ;enable all sound channels except dmc
             lda #%00000110
             sta PPU_CTRL_REG2            ;turn off clipping for OAM and background
             jsr MoveAllSpritesOffscreen
             jsr InitializeNameTables     ;initialize both name tables
             inc DisableScreenFlag        ;set flag to disable screen output
             lda Mirror_PPU_CTRL_REG1
             ora #%10000000               ;enable NMIs
             jsr WritePPUReg1
EndlessLoop: jmp EndlessLoop              ;endless loop, need I say more?

;-------------------------------------------------------------------------------------
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
