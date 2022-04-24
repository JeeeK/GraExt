#
# GraExt Makefile
#
# J. E. Klasek j at klasek at
# 1985 - 2022
#

F=graext
FRUN=ge-run
FLTC=graext-c000-ltc

DIST_FILES=$(F).o $(F)+.o $(FRUN).prg $(FRUN)+.prg ge.prg ge+.prg g-ext18.prg debug.prg demo-v.prg demo-h-v.prg demo1.prg demo2.prg demo-star.prg gliding-lines.prg fill-demo.prg fill-demo2.prg fill-demo3.prg fill-butterfly.prg kreise.prg spirale.prg demo-test-1.prg butterfly-test.prg fill-all.prg

.SUFFIXES: .prg .bas

%.bas:
	cp -p ../dist-disk/$@ .

%.prg: %.bas
	rm -f $@
	../tools/bas2prg $<


all: $(F).o $(FRUN).prg $(F)+.o $(FRUN)+.prg $(FLTC)


# Binary at $c000, in PRG format
$(F).o: $(F).asm $(F)-core.asm
	acme -v3 -Dopt_space=1 -l $(F).l -r $(F).r $(F).asm 
	rm -f graext-c000 ; ln $(F).o graext-c000

# Fast/speed variant
$(F)+.o: $(F).asm $(F)-core.asm
	acme -v3 -l $(F)+.l -r $(F)+.r -o $(F)+.o -f cbm $(F).asm 
	rm -f graext-c000+ ; ln $(F)+.o graext-c000+


# Binary for LTC hardware, im raw format, always space optimized
$(FLTC): $(F).asm $(F)-core.asm
	acme -v3 -Dltc=1 -Dopt_space=1 -l $(FLTC).l -r $(FLTC).r -o $(FLTC) $(F).asm 

# Program as PRG installer
$(FRUN).prg: $(FRUN).asm $(F)-core.asm
	acme -v3 -Dopt_space=1 -l $(FRUN).l -r $(FRUN).r $(FRUN).asm 

# Fast/speed variante
$(FRUN)+.prg: $(FRUN).asm $(F)-core.asm
	acme -v3 -l $(FRUN)+.l -r $(FRUN)+.r -o $(FRUN)+.prg -f cbm $(FRUN).asm 


$(F).hex: $(F).o
	od -x $(F).o > $(F).hex

g-ext18.hex: g-ext18.prg
	od -x g-ext18.prg > g-ext18.hex

diff: g-ext18.hex $(F).hex
	diff g-ext18.hex $(F).hex

tag: x.d64
	@-if [ -z "$(TAG)" ]; then \
		echo "Usage:" ;\
		echo "$(MAKE) tag TAG=<tag-text>" ;\
		exit 0; \
	else \
		mkdir -p $(TAG); \
		cp -p Makefile graext-core.asm $(F).asm $(F).o $(F).r $(F).l $(TAG) ;\
		cp -p $(FRUN).asm $(FRUN).prg $(FRUN).r $(FRUN).l $(TAG) ;\
		cp -p $(FLTC) $(FLTC).r $(FLTC).l $(TAG) ;\
		cp -p x.d64 demo.d64 ; zip -m $(TAG)/demo.zip demo.d64 ;\
	fi

clean:
	rm $(F).o

x.d64 i: $(DIST_FILES)
	../tools/FILE2DISK x.d64

ge.prg: $(F).o
	cp -p $(F).o ge.prg

ge+.prg: $(F)+.o
	cp -p $(F)+.o ge+.prg

gra-ext.d64: $(DIST_FILES)
	-mkdir tmp ;\
	cp -p $(DIST_FILES) tmp/. ;\
	cd tmp ;\
	../../tools/FILE2DISK gra-ext.d64 ;\
	mv -f gra-ext.d64 .. ;\
	cd .. ;\
	rm -rf tmp


gliding-lines-init: ../dist-disk/gliding-lines.prg
	cp -p $< .


