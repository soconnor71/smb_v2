ASM = python badassm/badassm.py
LINK = python idiotlink.py
AFLAGS = --use-linker
RM = rm -f
NESFILE = smb.nes

all: $(NESFILE)

main.bin: main.asm
	$(ASM) $^ $(AFLAGS)
smb-todo.bin: smb-todo.asm
	$(ASM) $^ $(AFLAGS)
sound.bin: sound.asm
	$(ASM) $^ $(AFLAGS)
title.bin: title.asm
	$(ASM) $^ $(AFLAGS)

$(NESFILE): sound.bin title.bin main.bin smb-todo.bin
	$(LINK) $(NESFILE) vanilla.chr sound title main smb-todo

clean:
	$(RM) *.map *.bin *.und

