import math

samples = 128
i = 0
starty = 36
height = 16

while i < samples:
	angel = (i * (360.0 / (samples - 1))) * (math.pi / 180.0)
	y = int(starty + (height * math.sin(angel)))
	print('$%02X, ' % (y), end='')
	# print('%f' % math.sin(angel))
	#print('%f' % (math.sin(math.cos(inc * i))))
	i += 1
