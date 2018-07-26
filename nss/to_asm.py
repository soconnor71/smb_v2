nt = open('namedump.nam', 'rb').read()

for i in range(0, len(nt)):
	if 0 == (i & 0xff):
		print('\nnametable_data_%d:\n\t.db ' % (i / 0x100), end='')
	elif i and 0 == (i & 0xf):
		print('\n\t.db ', end='')
	print("$%02X, " %(nt[i]), end='')
