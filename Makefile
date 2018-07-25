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
dummy.bin: dummy.asm
	$(ASM) $^ $(AFLAGS)
sound.bin: sound.asm
	$(ASM) $^ $(AFLAGS)

$(NESFILE): sound.bin dummy.bin smb-todo.bin main.bin
	$(LINK) $(NESFILE) vanilla.chr sound dummy smb-todo main

clean:
	$(RM) *.map *.bin *.und

