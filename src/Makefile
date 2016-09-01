

F=graext

$(F).o: $(F).asm
	acme -v3 -l $(F).l -r $(F).r $(F).asm 
	

$(F).hex: $(F).o
	od -x $(F).o > $(F).hex

g-ext18.hex: g-ext18.prg
	od -x g-ext18.prg > g-ext18.hex

diff: g-ext18.hex $(F).hex
	diff g-ext18.hex $(F).hex

tag:
	@-if [ -z "$(TAG)" ]; then \
		echo "Usage:" ;\
		echo "$(MAKE) tag TAG=<tag-text>" ;\
		exit 0; \
	else \
		mkdir -p $(TAG); \
		cp -p Makefile $(F).asm $(F).o $(F).r $(F).l $(TAG) ;\
	fi

clean:
	rm $(F).o

x.d64 i: $(F).o gliding-lines.prg
	cp -p $(F).o ge.prg
	FILE2DISK x.d64

gliding-lines.prg: ../dist-disk/gliding-lines.prg
	cp -p $< .
