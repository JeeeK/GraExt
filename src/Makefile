

F=graext

$(F).o: $(F).asm
	acme -v3 -l $(F).l -r $(F).r $(F).asm 
	

$(F).hex: $(F).o
	od -x $(F).o > $(F).hex

g-ext18.hex: g-ext18.prg
	od -x g-ext18.prg > g-ext18.hex

diff: g-ext18.hex $(F).hex
	diff g-ext18.hex $(F).hex

clean:
	rm $(F).o


