import sys
import re

def fix_instruction(inst, comment):
	if 0 == len(inst):
		return ''
	tokens = [ it.strip() for it in inst.split() ]
	if '.BYTE' == tokens[0]:
		return '\t\t' + '.db ' + tokens[1]
	elif '.WORD' == tokens[0]:
		return '\t\t' + '.dw ' + tokens[1]

	if len(tokens) > 2:
		print(inst)
		raise 'dont understand'	
	mnem = tokens[0].lower()
	s = '\t\t' + mnem
	if mnem in [ 'lsr' ]:
		tokens.pop()
	if 2 == len(tokens):
		hack = False
		c = comment.split()
		if len(c) >= 2:
			if 'RENAME' == c[0]:
				tokens[1] = ' '.join(c[1:])
		m = re.match(r'\$[0-9a-fA-F]{4}.*', tokens[1])
		if m:
			print(tokens[1])
		s += ' ' + tokens[1]
	return s

lines = open(sys.argv[1], 'rb').readlines()
fixed = []
beg = 0

for beg in range(0, len(lines)):
	line = lines[beg]
	if re.match(r'\s*\* =\s*\$6000.*', line.decode('utf-8')):
		break
if beg == len(lines):
	print('Nah')
	sys.exit(-1)

fixed.append('\t.org $c000')

for i in range(beg + 1, len(lines)):
	line = lines[i].decode('utf-8', errors = 'ignore')
	parts = [ it.strip() for it in line.split(';', 1) ]
	line = parts[0]
	comment = '' if len(parts) < 2 else parts[1]
	if 0 == len(line):
		continue
	if '*' == line[0]:
		break
	label = line.split(':', 1)
	if 2 == len(label):
		fixed.append(label[0].strip() + ':')
		fixed.append(fix_instruction(label[1], comment))
	else:
		fixed.append(fix_instruction(line, comment))

for it in fixed:
	print(it)
