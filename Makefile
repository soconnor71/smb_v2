ASM = python badassm/badassm.py
LINK = python idiotlink.py
AFLAGS = --use-linker
RM = rm -f
NESFILE = smbex.nes

all: $(NESFILE)

main.bin: main.asm
	$(ASM) $^ $(AFLAGS)
smb-todo.bin: smb-todo.asm
	$(ASM) $^ $(AFLAGS)
practice.bin: practice.asm
	$(ASM) $^ $(AFLAGS)
sound.bin: sound.asm
	$(ASM) $^ $(AFLAGS)

$(NESFILE): sound.bin practice.bin smb-todo.bin main.bin
	$(LINK) $(NESFILE) vanilla.chr sound practice smb-todo main

clean:
	$(RM) $(NESFILE) *.deb *.map *.bin *.und

