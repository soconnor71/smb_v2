import sys
import os
import json

# ye this file is fucking atrocious.

banks = []
symbols = {}

print("Running Shit Link")

prg = bytearray([])
ines = open('ines.raw', 'rb').read()
outfile = sys.argv[1]
gfx = open(sys.argv[2], 'rb').read()

def resolve_undef(bank, symbols, undfile):
	if not os.path.isfile(undfile):
		return
	und = json.loads(open(undfile, 'r').read())
	for it in und:
		if not it['ref'] in symbols:
			raise Exception("Unable to resolve %s (referenced in %s)" % (it['ref'], undfile))
		bank[it['off'] + 0] = symbols[it['ref']] & 0xFF
		bank[it['off'] + 1] = (symbols[it['ref']] >> 8) & 0xFF

for it in sys.argv[3:]:
	sym_file = it + '.map'
	if os.path.isfile(sym_file):
		print('Loading symbols from: %s' % (sym_file))
		for line in open(sym_file, 'r').readlines():
			line = line.strip()
			if 0 == len(line):
				continue
			v = line.split('@')
			if 2 != len(v):
				raise Exception('Malformed map file')
			sym = v[0].strip()
			if sym in symbols:
				raise Exception('Symbol %s defined multiple times' % (sym))
			symbols[sym] = int(v[1].strip(), 16)
	else:
		print('Warning: %s does not exist' % (sym_file))

print('All symbols loaded (found %d)' % (len(symbols)))
bank_id = 0
for it in sys.argv[3:]:
	bank_id += 1
	bank = bytearray(open(it + '.bin', 'rb').read())
	pad_count = 0x4000 - len(bank)
	print('Bank ID %d - %s (%d bytes, %d free)' % (bank_id, it, len(bank), pad_count))
	if pad_count < 0:
		raise Exception('Image %s is too large' % it)
	bank = bank + bytearray([ 0xEA ] * pad_count)
	resolve_undef(bank, symbols, it + '.und')
	prg = prg + bank

open(outfile, 'wb').write(ines + prg + gfx)

