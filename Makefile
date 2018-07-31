ASM = python badassm/badassm.py
LINK = python idiotlink.py
AFLAGS = --use-linker
RM = rm -f
NESFILE = smbex.nes

all: $(NESFILE)

fpg_data.asm: genfpg.py fpg_data.json fpg_data_inline.asm
	python genfpg.py fpg_data.json > fpg_data.asm
main.bin: main.asm
	$(ASM) $^ $(AFLAGS)
vanilla.bin: vanilla.asm
	$(ASM) $^ $(AFLAGS)
fpg.bin: fpg.asm
	$(ASM) $^ $(AFLAGS)
fpg_data.bin: fpg_data.asm
	$(ASM) $^ $(AFLAGS)
practice.bin: practice.asm
	$(ASM) $^ $(AFLAGS)
sound.bin: sound.asm
	$(ASM) $^ $(AFLAGS)
loader.bin: loader.asm
	$(ASM) $^ $(AFLAGS)
smlsound.bin: smlsound.asm
	$(ASM) $^ $(AFLAGS)

$(NESFILE): sound.bin practice.bin vanilla.bin loader.bin smlsound.bin main.bin fpg.bin fpg_data.bin
	$(LINK) $(NESFILE) vanilla sound practice fpg fpg_data smlsound loader main

clean:
	$(RM) $(NESFILE) fpg_data.asm *.deb *.map *.bin *.und

