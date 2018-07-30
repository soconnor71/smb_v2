import sys
import json

level_vars = [
	"ScreenRight_PageLoc",
	"ScreenLeft_X_Pos",
	"ScreenRight_X_Pos",
	"OffScr_WorldNumber",
	"WorldNumber",
	"OffScr_AreaNumber",
	"AreaNumber",
	"LevelNumber",
	"ScreenEdge_PageLoc",
	"Player_PageLoc",
	"FrameCounter",
	"IntervalTimerControl",
	"PseudoRandomBitReg+0",
	"PseudoRandomBitReg+2",
	"PseudoRandomBitReg+1",
	"PseudoRandomBitReg+3",
	"PseudoRandomBitReg+4",
	"PseudoRandomBitReg+5",
	"PseudoRandomBitReg+6",
	"PseudoRandomBitReg+7"
]

player_vars = [
	"Player_X_Speed",
	"Player_State",
	"Player_YMF_Dummy",
	"JumpOrigin_Y_Position",
	"FrictionAdderLow",
	"Player_Rel_YPos",
	"MaximumLeftSpeed",
	"Player_Rel_XPos",
	"Player_MovingDir",
	"Player_Y_Speed",
	"Player_SprAttrib",
	"Player_XSpeedAbsolute",
	"CrouchingFlag",
	"Player_Y_MoveForce",
	"PlayerAnimCtrl",
	"RunningSpeed",
	"VerticalForce",
	"FrictionAdderHigh",
	"Player_Y_HighPos",
	"SwimmingFlag",
	"PlayerChangeSizeFlag",
	"JumpOrigin_Y_HighPos",
	"MaximumRightSpeed",
	"DiffToHaltJump",
	"Player_X_Position",
	"PlayerAnimTimerSet",
	"VerticalForceDown",
	"Player_X_MoveForce",
	"Player_Y_Position"
]

configs = []

def make_function(j, name, v):
	print('%s_%s:' % (j['name'], name))
	for it in v:
		print('\t\tlda #$%02X' % (j[it]))
		print('\t\tsta %s' % (it))
	print('\t\trts')

def get_input(s):
	k = 0
	b = { 'a': 0x80, 'b': 0x40, 'l': 0x02, 'r': 0x01 }
	for it in s.lower():
		k |= b[it]
	return k

def get_text(s):
	b = []
	for it in s.lower()[0:8]:
		n = ord(it)
		if ' ' == it:
			b.append(0x24)
		elif '-' == it:
			b.append(0x28)
		elif '!' == it:
			b.append(0x2b)
		elif n >= ord('0') and n <= ord('9'):
			b.append(n - ord('0'))
		else:
			b.append(0x0a + n - ord('a'))
	b = b + [ 0x24 ] * (8 - len(b))
	return ', '.join([ '$%02X' % (it) for it in b ])


def make_rules(name, rules):
	ind = '\t\t'

	for i in range(0, len(rules)):
		print('%s_ruleset%d:' % (name, i))
		print(ind + 'ldy FrameCounter')
		print(ind + 'lda SavedJoypad1Bits')
		print(ind + 'and #$C3') #only care for L,R,A,B
		for j in range(0, len(rules[i])):
			rule = rules[i][j]
			print('%s_ruleset%d_rule%d:' % (name, i, j))
			print(ind + 'cpy #$%02X' % (rule['frame']))
			print(ind + 'bne %s_ruleset%d_rule%d' % (name, i, j + 1))
			if 'input' == rule['method']:
				print(ind + 'cmp #$%02X' % (get_input(rule['input'])))
				print(ind + 'beq %s_ruleset%d_rule%d' % (name, i, j +1))
				print(ind + 'lda #$%02X' % (get_input(rule['input'])))
				print(ind + 'jmp fpg_failed_input')
			elif 'pixel' == rule['method']:
				print(ind + 'ldx Player_X_Position')
				print(ind + 'cpx #$%02X' % (rule['x']))
				print(ind + 'beq %s_ruleset%d_rule%d_y' % (name, i, j))
				print(ind + 'jmp fpg_failed_pos_x')
				print('%s_ruleset%d_rule%d_y:' % (name, i, j))
				print(ind + 'ldx Player_Y_Position')
				print(ind + 'cpx #$%02X' % (rule['y']))
				print(ind + 'beq %s_ruleset%d_rule%d' % (name, i, j + 1))
				print(ind + 'jmp fpg_failed_pos_y')
			elif 'win' == rule['method']:
				print(ind + 'jmp fpg_win')
		print('%s_ruleset%d_rule%d:' % (name, i, len(rules[i])))
		print(ind + 'rts')
	print('%s_rulesets:' % (name))
	for i in range(0, len(rules)):
		print('\t.dw %s_ruleset%d' % (name, i))
	print('''%s_validate:
		lda FpgRuleset
		asl
		tay
		lda %s_rulesets, y
		sta $0
		lda %s_rulesets+1, y
		sta $1
		jmp ($0)
	''' % (name, name, name))


print('''
	.index 8
	.mem 8
	.vars vars.inc
FpgRuleset = $124
FpgSelected = $124
	.org $8000
	.db $ba, BANK_FPG_DATA
''')

for it in sys.argv[1:]:
	j = json.loads(open(it, 'r').read())
	j['FrameCounter'] = j['frame']
	j["PseudoRandomBitReg+0"] = j['PseudoRandomBitReg0']
	j["PseudoRandomBitReg+1"] = j['PseudoRandomBitReg1']
	j["PseudoRandomBitReg+2"] = j['PseudoRandomBitReg2']
	j["PseudoRandomBitReg+3"] = j['PseudoRandomBitReg3']
	j["PseudoRandomBitReg+4"] = j['PseudoRandomBitReg4']
	j["PseudoRandomBitReg+5"] = j['PseudoRandomBitReg5']
	j["PseudoRandomBitReg+6"] = j['PseudoRandomBitReg6']
	j["PseudoRandomBitReg+7"] = j['PseudoRandomBitReg7']

	make_function(j, 'load_level', level_vars)
	make_function(j, 'load_player', player_vars)
	make_rules(j['name'], j['rules'])
	configs.append({ 'name': j['name'], 'rulesets': len(j['rules']) })

def print_if_true(c, str):
	if c:
		print(str)

for i in range(0, len(configs)):
	name = configs[i]['name']
	if (0 == i): print('fpg_configs:')
	print('\t\t.db %s ; %s' % (get_text(name), name))
	if (0 == i): print('fpg_load_level_func:')
	print('\t\t.dw %s_load_level' % (name))
	if (0 == i): print('fpg_load_player_func:')
	print('\t\t.dw %s_load_player' % (name))
	if (0 == i): print('fpg_validate_func:')
	print('\t\t.dw %s_validate' % (name))
	if (0 == i): print('fpg_num_routes:')
	print('\t\t.db $%02X' % (configs[i]['rulesets'])) # align to 0x10
	print('\t\t.db 0') # align to 0x10

print(open('fpg_data_inline.asm', 'r').read())


