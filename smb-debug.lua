local secondary_game_setup = 0x8FC5 + 3 -- Post jsr (so it works from resume too)
local world = 0
local level = 0

function u8(addr)
	return memory.readbyteunsigned(addr)
end

function get_size_string()
	local size = u8(0x754)
	if 0 == size then
		return 'BIG'
	elseif 1 == size then
		return 'SMALL'
	end
	return 'WTF?'
end

function get_power_string()
	local power = u8(0x756)
	if 0 == power then
		return 'NOTHING'
	elseif 1 == power then
		return 'SUPER'
	elseif 2 == power then
		return 'FIRE'
	end
	return 'WTF?'
end

function on_secondary_game_setup()
	local w = u8(0x75F)
	local l = u8(0x75C)
	if world == w and level == l then
		return
	end
	world = w
	level = l
	print(string.format('Enter level %d-%d with as %s with %s', world + 1, level + 1, get_size_string(), get_power_string()))
	print(string.format('Rule: %d%d%d, Frame: %d, Timer: %d (%02X)', u8(0x7E0), u8(0x7E1), u8(0x7E2), u8(0x09), u8(0x77f), u8(0x747)))
	print(string.format('Random Seed: %02X, %02X, %02X, %02X, %02X, %02X, %02X',
		u8(0x7A7), u8(0x7A8), u8(0x7A9), u8(0x7AA), u8(0x7AB), u8(0x7AC), u8(0x7AD)))
	print('------------------------------------------------------------')
end

print('Scripted Loaded!')
memory.registerexecute(secondary_game_setup, on_secondary_game_setup)
