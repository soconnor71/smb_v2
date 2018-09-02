local form = nil
local lbl = nil
local txt_name = nil
local txt_input = nil
local state = nil
local rule = nil

local enemy_table_base = 0x9D70 -- SMB Original.

json = require "json"

function read_frame()
	return memory.readbyte(0x09)
end

function read_player_x()
	return memory.readbyte(0x86)
end

function read_player_y()
	return memory.readbyte(0xce)
end

function add_line(frame, line)
	local old = forms.gettext(lbl)
	console.log(line)
	forms.settext(lbl, old .. 'Frame [' .. tostring(frame) .. ']: ' .. line .. '\n')
end

function write_rule(frame, line)
	add_line(frame, line .. ' (Rule set: ' .. tostring(#state.rules) .. ')')
end

function add_rule_set()
	savestate.loadslot(0)
	state.rules[#state.rules + 1] = {}
	rule = state.rules[#state.rules]
end

function read_array(addr, n)
	arr = {}
	for i=0, (n - 1) do
		--
		-- One based indexing is SO fucking retarded.
		--
		arr[i + 1] = memory.readbyte(addr + i)
	end
	return arr
end

function read_memory()
	return {
		FrameCounter          = read_frame(),
	
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
		
		Timers                = read_array(0x0780, 21), -- Read all timers... Might as well..
		
		EnemyFrenzyBuffer     = memory.readbyte(0x06cb),
		EnemyFrenzyQueue      = memory.readbyte(0x06cd),
	
		PlayerFacingDir       = memory.readbyte(0x33),
		Player_PageLoc        = memory.readbyte(0x6d),

		BalPlatformAlignment  = memory.readbyte(0x03a0),
		Platform_X_Scroll     = memory.readbyte(0x03a1),

		MaximumLeftSpeed      = memory.readbyte(0x0450), -- Are these two not constant?
		MaximumRightSpeed     = memory.readbyte(0x0456),
		--
		-- 0x700
		--
		Player_XSpeedAbsolute = memory.readbyte(0x0700),
		FrictionAdderHigh     = memory.readbyte(0x0701),
		FrictionAdderLow      = memory.readbyte(0x0702),
		RunningSpeed          = memory.readbyte(0x0703),
		SwimmingFlag          = memory.readbyte(0x0704),
		Player_X_MoveForce    = memory.readbyte(0x0705),
		DiffToHaltJump        = memory.readbyte(0x0706),
		JumpOrigin_Y_HighPos  = memory.readbyte(0x0707),
		JumpOrigin_Y_Position = memory.readbyte(0x0708),
		VerticalForce         = memory.readbyte(0x0709),
		VerticalForceDown     = memory.readbyte(0x070a),
		PlayerChangeSizeFlag  = memory.readbyte(0x070b),
		PlayerAnimTimerSet    = memory.readbyte(0x070c),
		PlayerAnimCtrl        = memory.readbyte(0x070d),
		CrouchingFlag         = memory.readbyte(0x0714),
		EnemyDataOffset       = memory.readbyte(0x0739),
		EnemyObjectPageLoc    = memory.readbyte(0x073a),
		EnemyObjectPageSel    = memory.readbyte(0x073b),
		PlayerSize            = memory.readbyte(0x0754),
		PlayerStatus          = memory.readbyte(0x0756),
				
		ScreenEdge_PageLoc    = memory.readbyte(0x071a),
		ScreenRight_PageLoc   = memory.readbyte(0x071b),
		ScreenLeft_X_Pos      = memory.readbyte(0x071c),
		ScreenRight_X_Pos     = memory.readbyte(0x071d),

		HorizontalScroll      = memory.readbyte(0x073f),
		VerticalScroll        = memory.readbyte(0x0740),
		ScrollLock            = memory.readbyte(0x0723),
		ScrollThirtyTwo       = memory.readbyte(0x073d),
		Player_X_Scroll       = memory.readbyte(0x06ff),
		Player_Pos_ForScroll  = memory.readbyte(0x0755),
		ScrollAmount          = memory.readbyte(0x0775),
		ScrollFractional      = memory.readbyte(0x0768),
		
		LevelNumber           = memory.readbyte(0x075c),
		WorldNumber           = memory.readbyte(0x075f),
		AreaNumber            = memory.readbyte(0x0760),
		
		TimerControl          = memory.readbyte(0x0747),
		IntervalTimerControl  = memory.readbyte(0x077f)
	}
end

function read_enemy_ptr_offset()
	local current = bit.bor(bit.lshift(memory.readbyte(0xea), 8), memory.readbyte(0xe9))
	return (current - enemy_table_base)
end

function on_start_new()
	console.log('Starting new practice target here!')
	if not client.ispaused() then
		console.log('Nope. Must be paused.')
		return
	end
	savestate.saveslot(0)
	state = {
		name =  string.sub(forms.gettext(txt_name), 0, 16), -- 16 characters at most.
		frame = memory.readbyte(0x09),
		enemy_ptr_offset = read_enemy_ptr_offset(),
		memory = read_memory(),
		rules = {}
	}

	add_rule_set()
	
	forms.settext(lbl, '')
	add_line(state["frame"], 'Session created: ' .. state['name'])
end

function on_validate_x()
	rule[#rule + 1] = { method = 'x', x = read_player_x(), frame = read_frame() }
	write_rule(read_frame(), 'X rule ' .. tostring(read_player_x()))
end

function on_validate_y()
	rule[#rule + 1] = { method = 'y', y = read_player_y(), frame = read_frame() }
	write_rule(read_frame(), 'Y rule ' .. tostring(read_player_y()))
end

function on_validate_pixel()
	rule[#rule + 1] = { method = 'pixel', x = read_player_x(), y = read_player_y(), frame = read_frame() }
	write_rule(read_frame(), 'Pixel rule ' .. tostring(read_player_x()) .. ', ' .. tostring(read_player_y()))
end

function on_validate_input()
	rule[#rule + 1] = { method = 'input', input = forms.gettext(txt_input), frame = read_frame() }
	write_rule(read_frame(), 'Input rule ' .. forms.gettext(txt_input))
end

function on_super_player()
	rule[#rule + 1] = { method = 'win', frame = read_frame() }
	write_rule(read_frame(), 'Winner! (if you add anything else it will not count)')
end

function save_target()
	fp = io.open('C:\\temp\\' .. state['name'] .. '.json', 'w')
	fp:write(json.encode(state))
	fp:close()
end

function initialize()
	if nil ~= form then
		return
	end
	--
	-- MLG-tier UI design.
	--
	form = forms.newform(640, 480, 'SMB State Practice')
	txt_name = forms.textbox(form, '', 300, 32, nil, 10, 10 + 6)
	forms.button(form, 'Create new practice target', on_start_new, 10 + 300, 10, 300, 32)
	forms.button(form, 'Validate X Pos', on_validate_x, 10, 10 + 32, 200, 32)
	forms.button(form, 'Validate Y Pos', on_validate_y, 10 + 200, 10 + 32, 200, 32)
	forms.button(form, 'Validate Pixel', on_validate_pixel, 10 + 400, 10 + 32, 200, 32)
	txt_input = forms.textbox(form, '', 300, 32, nil, 10, 10 + 64 + 6)
	forms.button(form, 'Validate Input', on_validate_input, 10 + 300, 10 + 64, 300, 32)
	forms.button(form, 'YOU ARE SUPER PLAYER', on_super_player, 10, 10 + 64 + 32, 600, 32)
	forms.button(form, 'New Rule Set', add_rule_set, 10, 10 + 64 + 32 + 32, 300, 32)
	forms.button(form, 'Save Target', save_target, 10 + 300, 10 + 64 + 32 + 32, 300, 32)
	lbl = forms.label(form, 'No target', 10, 10 + 64 + 64 + 32, 600, 300)
end

initialize()