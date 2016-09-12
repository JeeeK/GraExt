

F=graext
FRUN=ge-run


.SUFFIXES: .prg .bas

%.bas:
	cp -p ../dist-disk/$@ .

%.prg: %.bas
	rm -f $@
	bas2prg $<


all: $(F).o $(FRUN).prg


$(F).o: $(F).asm $(F)-core.asm
	acme -v3 -l $(F).l -r $(F).r $(F).asm 
	

$(FRUN).prg: $(FRUN).asm $(F)-core.asm
	acme -v3 -l $(FRUN).l -r $(FRUN).r $(FRUN).asm 
	

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
		cp -p Makefile graext-core.asm $(F).asm $(F).o $(F).r $(F).l $(TAG) ;\
		cp -p $(FRUN).asm $(FRUN).prg $(FRUN).r $(FRUN).l $(TAG) ;\
	fi

clean:
	rm $(F).o

x.d64 i: $(F).o $(FRUN).prg gliding-lines.prg debug.prg demo1.prg gliding-lines.prg
	cp -p $(F).o ge.prg
	FILE2DISK x.d64

gliding-lines-init: ../dist-disk/gliding-lines.prg
	cp -p $< .


