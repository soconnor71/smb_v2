local form = nil
local lbl = nil
local txt_name = nil
local txt_input = nil
local state = nil
local rule = nil

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

function on_start_new()
	console.log('Starting new practice target here!')
	if not client.ispaused() then
		console.log('Nope. Must be paused.')
		return
	end
	savestate.saveslot(0)
	state = {
		name =  string.sub(forms.gettext(txt_name), 0, 8), -- 8 characters at most.
		frame = memory.readbyte(0x09),
			
		Player_State          = memory.readbyte(0x1d),
		PlayerFacingDir       = memory.readbyte(0x33),
		Player_MovingDir      = memory.readbyte(0x45),
		Player_X_Speed        = memory.readbyte(0x57),
		Player_PageLoc        = memory.readbyte(0x6d),
		Player_X_Position     = memory.readbyte(0x86),
		Player_Y_Speed        = memory.readbyte(0x9f),
		Player_Y_HighPos      = memory.readbyte(0xb5),
		Player_Y_Position     = memory.readbyte(0xce),
		Player_Rel_XPos       = memory.readbyte(0x03ad),
		Player_Rel_YPos       = memory.readbyte(0x03b8),
		Player_SprAttrib      = memory.readbyte(0x03c4),
		Player_YMF_Dummy      = memory.readbyte(0x0416),
		Player_Y_MoveForce    = memory.readbyte(0x0433),
		--
		-- 0x400
		--
		MaximumLeftSpeed      = memory.readbyte(0x0450),
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
		
		IntervalTimerControl  = memory.readbyte(0x077f),
		
		PseudoRandomBitReg0   = memory.readbyte(0x07a7),
		PseudoRandomBitReg1   = memory.readbyte(0x07a8),
		PseudoRandomBitReg2   = memory.readbyte(0x07a9),
		PseudoRandomBitReg3   = memory.readbyte(0x07aa),
		PseudoRandomBitReg4   = memory.readbyte(0x07ab),
		PseudoRandomBitReg5   = memory.readbyte(0x07ac),
		PseudoRandomBitReg6   = memory.readbyte(0x07ad),
		PseudoRandomBitReg7   = memory.readbyte(0x07ae), -- One to many I think. Can't think. Too tired.

		rules = {}
	}

	add_rule_set()
	
	forms.settext(lbl, '')
	add_line(state["frame"], 'Session created: ' .. state['name'])
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
	forms.button(form, 'Validate Pixel', on_validate_pixel, 10, 10 + 32, 600, 32)
	txt_input = forms.textbox(form, '', 300, 32, nil, 10, 10 + 64 + 6)
	forms.button(form, 'Validate Input', on_validate_input, 10 + 300, 10 + 64, 300, 32)
	forms.button(form, 'YOU ARE SUPER PLAYER', on_super_player, 10, 10 + 64 + 32, 600, 32)
	forms.button(form, 'New Rule Set', add_rule_set, 10, 10 + 64 + 32 + 32, 300, 32)
	forms.button(form, 'Save Target', save_target, 10 + 300, 10 + 64 + 32 + 32, 300, 32)
	lbl = forms.label(form, 'No target', 10, 10 + 64 + 64 + 32, 600, 300)
end

initialize()