ASM = python badassm/badassm.py
LINK = python idiotlink.py
AFLAGS = --use-linker
RM = rm -f
NESFILE = smbex.nes

all: $(NESFILE)

main.bin: main.asm
	$(ASM) $^ $(AFLAGS)
vanilla.bin: vanilla.asm
	$(ASM) $^ $(AFLAGS)
practice.bin: practice.asm
	$(ASM) $^ $(AFLAGS)
sound.bin: sound.asm
	$(ASM) $^ $(AFLAGS)

$(NESFILE): sound.bin practice.bin vanilla.bin main.bin
	$(LINK) $(NESFILE) sound practice vanilla main

clean:
	$(RM) $(NESFILE) *.deb *.map *.bin *.und

