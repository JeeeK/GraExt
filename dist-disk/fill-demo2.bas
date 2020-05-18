5 rem fill demo 2: crossing lines filling
6 rem 2019-10-11 johann # klasek at
7 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
8 ifpeek(49153)+peek(49154)=21thensys49152
10 i=rnd(-ti)
15 &g 1,13,0: poke 53280,11: &s 1: &p 160,100
20 for i=1 to 250 : rem draw random frames
30 x=rnd(1)*280+20
35 y=rnd(1)*160+20
40 &t x,y
50 next
60 &f 0,0 : rem flood fill everything around them
70 for i=1 to 2000: next : rem wait
80 &s0: &f 0,0 : rem vanishing it with fill
85 for i=1 to 500: next : rem short wait
90 goto 15
