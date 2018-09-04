local enemy_table_base = 0x9D70 -- SMB Original.

json = require "json"

function read_array(addr, n)
	arr = {}
	for i=0, (n - 1) do
		arr[i + 1] = memory.readbyteunsigned(addr + i)
	end
	return arr
end

function read_memory()
	return {
		FrameCounter          = memory.readbyteunsigned(0x09),
	
		Player_State          = read_array(0x1d, 7),
		Player_MovingDir      = read_array(0x45, 7),
		Player_X_Speed        = read_array(0x57, 7),
		Player_X_Position     = read_array(0x86, 7),
		Player_Y_Speed        = read_array(0x9f, 7),
		Player_Y_HighPos      = read_array(0xb5, 7),
		Player_Y_Position     = read_array(0xce, 7),
		Player_Rel_XPos       = read_array(0x03ad, 7),
		Player_Rel_YPos       = read_array(0x03b8, 7),
		Player_SprAttrib      = read_array(0x03c4, 7),
		SprObject_X_MoveForce = read_array(0x0400, 7), -- 0x401 shared by a ton of shit... just copy all...
		Player_YMF_Dummy      = read_array(0x0416, 7),
		Player_Y_MoveForce    = read_array(0x0433, 7),
		Player_CollisionBits  = read_array(0x0490, 7),
		PseudoRandomBitReg    = read_array(0x07a7, 7),
		Player_BoundBoxCtrl   = read_array(0x0499, 7),
		Player_OffscreenBits  = read_array(0x03d0, 7),
		EnemyOffscrBitsMasked = read_array(0x03d8, 7),
		SprObject_PageLoc     = read_array(0x6d, 7), -- Must not use the player variable here or it will be interpreted as area initialization.
		Enemy_Flag            = read_array(0x0f, 7),
		Enemy_ID              = read_array(0x16, 7),
		PlatformCollisionFlag = read_array(0x03a2, 7),
		YPlatformTopYPos      = read_array(0x0401, 7),
		YPlatformCenterYPos   = read_array(0x58, 7),
		
		BoundingBox_UL_XPos   = read_array(0x04ac, 0x54),
		Timers                = read_array(0x0780, 21), -- Read all timers... Might as well..
		
		EnemyFrenzyBuffer     = memory.readbyteunsigned(0x06cb),
		EnemyFrenzyQueue      = memory.readbyteunsigned(0x06cd),
	
		PlayerFacingDir       = memory.readbyteunsigned(0x33),
		Player_PageLoc        = memory.readbyteunsigned(0x6d),

		BalPlatformAlignment  = memory.readbyteunsigned(0x03a0),
		Platform_X_Scroll     = memory.readbyteunsigned(0x03a1),

		MaximumLeftSpeed      = memory.readbyteunsigned(0x0450), -- Are these two not constant?
		MaximumRightSpeed     = memory.readbyteunsigned(0x0456),
		--
		-- 0x700
		--
		Player_XSpeedAbsolute = memory.readbyteunsigned(0x0700),
		FrictionAdderHigh     = memory.readbyteunsigned(0x0701),
		FrictionAdderLow      = memory.readbyteunsigned(0x0702),
		RunningSpeed          = memory.readbyteunsigned(0x0703),
		SwimmingFlag          = memory.readbyteunsigned(0x0704),
		Player_X_MoveForce    = memory.readbyteunsigned(0x0705),
		DiffToHaltJump        = memory.readbyteunsigned(0x0706),
		JumpOrigin_Y_HighPos  = memory.readbyteunsigned(0x0707),
		JumpOrigin_Y_Position = memory.readbyteunsigned(0x0708),
		VerticalForce         = memory.readbyteunsigned(0x0709),
		VerticalForceDown     = memory.readbyteunsigned(0x070a),
		PlayerChangeSizeFlag  = memory.readbyteunsigned(0x070b),
		PlayerAnimTimerSet    = memory.readbyteunsigned(0x070c),
		PlayerAnimCtrl        = memory.readbyteunsigned(0x070d),
		CrouchingFlag         = memory.readbyteunsigned(0x0714),
		EnemyDataOffset       = memory.readbyteunsigned(0x0739),
		EnemyObjectPageLoc    = memory.readbyteunsigned(0x073a),
		EnemyObjectPageSel    = memory.readbyteunsigned(0x073b),
		PlayerSize            = memory.readbyteunsigned(0x0754),
		PlayerStatus          = memory.readbyteunsigned(0x0756),
				
		ScreenEdge_PageLoc    = memory.readbyteunsigned(0x071a),
		ScreenRight_PageLoc   = memory.readbyteunsigned(0x071b),
		ScreenLeft_X_Pos      = memory.readbyteunsigned(0x071c),
		ScreenRight_X_Pos     = memory.readbyteunsigned(0x071d),

		HorizontalScroll      = memory.readbyteunsigned(0x073f),
		VerticalScroll        = memory.readbyteunsigned(0x0740),
		ScrollLock            = memory.readbyteunsigned(0x0723),
		ScrollThirtyTwo       = memory.readbyteunsigned(0x073d),
		Player_X_Scroll       = memory.readbyteunsigned(0x06ff),
		Player_Pos_ForScroll  = memory.readbyteunsigned(0x0755),
		ScrollAmount          = memory.readbyteunsigned(0x0775),
		ScrollFractional      = memory.readbyteunsigned(0x0768),
		
		LevelNumber           = memory.readbyteunsigned(0x075c),
		WorldNumber           = memory.readbyteunsigned(0x075f),
		AreaNumber            = memory.readbyteunsigned(0x0760),
		
		TimerControl          = memory.readbyteunsigned(0x0747),
		IntervalTimerControl  = memory.readbyteunsigned(0x077f)
	}
end

function read_enemy_ptr_offset()
	local current = bit.bor(bit.lshift(memory.readbyteunsigned(0xea), 8), memory.readbyteunsigned(0xe9))
	return (current - enemy_table_base)
end

local state = {
	name =  string.sub('FCEUXDBG', 0, 16), -- 16 characters at most.
	frame = memory.readbyteunsigned(0x09),
	enemy_ptr_offset = read_enemy_ptr_offset(),
	memory = read_memory(),
	rules = {}
}
print('common man')
fp = io.open('C:\\code\\smb\\scenarios\\pipe1-2.json', 'w')
fp:write(json.encode(state))
fp:close()

