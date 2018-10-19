const express = require('express')
const fs = require('fs')
const path = require('path')
const util = require('util')

const execFile = util.promisify(require('child_process').execFile);
const readFile = util.promisify(fs.readFile);
const writeFile = util.promisify(fs.writeFile);

const app = express()
const port = 8888

app.set('view engine', 'pug')

app.use(express.static('public'))

async function write_splits(res, load, reset, music, sfx, worlds) {
	var world_rules = ''
	var pup_bytes = '.db '

	if(undefined != worlds[0])
	{
		for(let i = 0; i < worlds.length; ++i)
		{
			let levels = worlds[i]
			if(!levels || 4 != levels.length)
			{
				res.status(500).send('Invalid number of levels for world ' + (i + 1))
				return
			}

			world_rules += '.dw '
			let enc_lives = 0

			for(let x = 0; x < levels.length; ++x)
			{
				let v = levels[x].split(':')
				if(2 != v.length)
				{
					res.status(500).send('Invalid level value')
					return
				}

				let rules = Number(v[0])
				let pups = Number(v[1])

				if(isNaN(rules) || isNaN(pups))
				{
					res.status(500).send('Not an integer')
					return
				}

				if(rules > 9999 || pups > 3)
				{
					res.status(500).send('Too high rule, or too many pups')
					return
				}

				enc_lives = enc_lives | (pups << (x * 2))

				world_rules += '$' + rules

				if((x + 1) != levels.length)
				{
					world_rules += ', '
				}
			}
			
			pup_bytes += '$' + enc_lives.toString(16)
			if((i + 1) != worlds.length)
			{
				pup_bytes += ', '
			}

			world_rules += '\n'
		}
	}

	let defines = []

	if(undefined !== load) { defines.push('-DLOAD_GAME_BUTTONS=' + load) }
	if(undefined !== reset) { defines.push('-DRESTART_GAME_BUTTONS=' + reset) }
	if(undefined !== music) { defines.push('-DENABLE_MUSIC=' + music )}
	if(undefined !== sfx) { defines.push('-DENABLE_SFX=' + sfx) }

	console.log(defines.join(' '))

	// console.log(pup_bytes)
	// console.log(world_rules)
	try {
		let data = await readFile('../practice.asm', 'utf8')

		if(world_rules)
		{
			data = data.replace(/;<BUILD_PATCH_LEVELS>[\s\S]+;<\/BUILD_PATCH_LEVELS>/m, world_rules)
			data = data.replace(/;<BUILD_PATCH_PUPS>[\s\S]+;<\/BUILD_PATCH_PUPS>/m, pup_bytes)
		}

		var practice_name = 'practice-' + (new Date().getTime())

		await writeFile('builds/' + practice_name + '.asm', data)
		await execFile('python', [
				'../../badassm/badassm.py',
				practice_name + '.asm',
				'--use-linker' ].concat(defines),
				{ cwd: 'builds' })

		await execFile('python', [
			'../../badassm/badassm.py',
			'sound.asm',
			'--use-linker' ].concat(defines),
			{ cwd: 'builds' })

		await execFile('python', [ 'idiotlink.py',
			practice_name + '.nes', 'vanilla',
			'sound', practice_name, 'fpg', 'fpg_data',
			'smlsound', 'loader', 'main' ],
			{ cwd: 'builds' })

		await execFile('../ips/flips-linux', [
				'--create',
				'--ips',
				'../ips/smborg.nes',
				practice_name + '.nes',
				practice_name + '.ips'
			],
			{ cwd: 'builds' })

		res.setHeader('Content-Disposition', 'attachment; filename=smbex.ips');
		res.setHeader('Content-Transfer-Encoding', 'binary');
		res.setHeader('Content-Type', 'application/octet-stream');
		res.sendFile(path.join(__dirname, 'builds/', practice_name + '.ips'))

	} catch(e) {
		console.log(e)
		res.status(500).send('Build exploded somehow <.<')
	}
}

app.get('/', function(req, res) {
	//
	// To get an idea if anyone is using it...
	//
	const ip = req.connection.remoteAddress || '<no idea>'
	console.log('Access root: ' + ip)

	res.render('practice')
})

app.get('/practice', function(req, res) {
	//
	// To get an idea if anyone is using it...
	//
	const ip = req.connection.remoteAddress || '<no idea>'
	console.log('Access /practice: ' + ip)

	res.render('practice')
})

app.get('/build', async (req, res) => {
	console.log('Got build request!')
	await write_splits(res,
		parseInt(req.query.load),
		parseInt(req.query.reset),
		parseInt(req.query.music),
		parseInt(req.query.sfx),
		[
			req.query.w1,
			req.query.w2,
			req.query.w3,
			req.query.w4,
			req.query.w5,
			req.query.w6,
			req.query.w7,
			req.query.w8
		]
	)
})

app.listen(port, function() {
	console.log('SMB Practice ROM - Buildserver on port: ' + port)
})



