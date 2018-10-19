from os import listdir
from os.path import isfile, join
import json

def td(s): print('<td>%s</td>' % (s))

def compare_rules(a, b):
    ka = set(a).difference([ 'frame', 'comment' ])
    kb = set(b).difference([ 'frame', 'comment' ])
    return ka == kb and all(a[k] == b[k] for k in ka)

conditions =  {
	'input': 'Input',
	'input_opt': 'Input',
	'pixel': 'Position',
	'y': 'Position',
	'x_ge': 'Position',
	'win': 'Super Player!'
}
scen_path = '../scenarios'

print('''<html>
<head>
<title>SMB EX - Practice Scenarios</title>
<link rel="stylesheet" href="bootstrap-3.3.7-dist/css/bootstrap.min.css" />
<script src="jquery-3.2.1.min.js"></script>
<script src="bootstrap-3.3.7-dist/js/bootstrap.min.js"></script>
</head>
<body>
<div class="container">
''')

files = [ f for f in listdir(scen_path) if isfile(join(scen_path, f)) ]
for it in files:
	j = json.loads(open(join(scen_path, it), 'rb').read())

	print('<h1>%s</h1>' % (j['name']))
	print('<p>%s</p>' % (j['comment']))
	route_id = 1
	for route in j['rules']:
		print('<h3>Route %d</h3>' % (route_id))
		print('<table class="table table-hover table-striped">')
		print('<thead><tr>')
		print('<th>Frame Offset</th><th>Validation type</th><th>Data</th><th>Comment</th>')
		print('</tr></thead><tbody>')
		i = 0
		while i < len(route):
			rule = route[i]
			if 'lock' == rule['method']:
				i += 1
				continue
			n = i + 1
			while n < len(route):
				if not compare_rules(rule, route[n]):
					break
				n += 1
			frame = (rule['frame'] - j['frame']) & 0xff
			if n != (i + 1):
				i = (n - 1)
				last_frame = (route[i]['frame'] - j['frame'] + 1) % 0xff
				frame = '%d -> %d (%d frames)' % (frame, last_frame, last_frame - frame + 1)

			print('<tr>')
			td(frame)
			td(conditions[rule['method']])
			data = ''
			if 'input' == rule['method'][:5]:
				opt = '' if 'opt' not in rule else rule['opt']
				keys = ['l', 'r', 'b', 'a']
				for k in keys:
					if k in rule['input']:
						img = k + '_yes.png'
					elif k in opt:
						img = k + '_opt.png'
					else:
						img = k + '_ign.png'
					data += '<img src="images/' + img + '" width="32" />'
			elif 'pixel' == rule['method']:
				data = 'X: %d, Y: %d' % (rule['x'], rule['y'])
			elif 'y' == rule['method']:
				data = 'Y: %d' % (rule['y'])
			elif 'x_ge' == rule['method']:
				data = 'X >= %d' % (rule['x'])
			elif 'win' == rule['method']:
				data = ''
			else:
				print(rule['method'])
				raise 'update me'
			td(data)
			td(rule['comment'] if 'comment' in rule else '')
			print('</tr>')
			i += 1
		print('</tbody></table>')
		route_id += 1
print('</div>')
print('</body>')
print('</html>')