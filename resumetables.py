import sys

def _ror(val, carry):
	next_carry	= bool(val & 1)
	val			= (val >> 1)
	if carry:
		val |= 0x80
	return val, next_carry

def random_init():
	return [ 0xA5 ] + ([ 0 ] * 6)

def random_advance(seed):
	carry = bool((seed[0] & 0x02) ^ (seed[1] & 0x02))

	for i in range(0, len(seed)):
		seed[i], carry = _ror(seed[i], carry)

	return seed

find = [ ]
seed = random_init()
total = 0

#while True:
for i in range(0, 10000):
	if 0 == (i % 3200):
		print('quick_resume_%d:' % (int(i / 100)))
	if 0 == (i % 100):
		print('\t.db '
			+ ', '.join([ '$%02X' % it for it in seed + [0] ])
			+ ' ; Base for %d' % (i))
	
	for x in range(0, 21):
		seed = random_advance(seed)
		#if seed in find:
		#	print('[%d] Found block!' % (i))
		#	print('#' * 60)
	total += 1
