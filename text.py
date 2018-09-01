import sys

if len(sys.argv) < 2:
	sys.exit(-1)



for it in sys.argv[1].lower():
	n = ord(it)
	if ' ' == it:
		sys.stdout.write('$24, ')
	elif '-' == it:
		sys.stdout.write('$28, ')
	elif '!' == it:
		sys.stdout.write('$2b, ')
	elif n >= ord('0') and n <= ord('9'):
		sys.stdout.write('$%02x, ' % (n - ord('0')))
	else:
		sys.stdout.write('$%02x, ' % (0x0a + n - ord('a')))
print('')
print('Length: %02X' % (len(sys.argv[1])))