5 rem fill demo: unfilled box cloud filling
6 rem 2019-10-31 johann # klasek at
7 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
8 ifpeek(49153)+peek(49154)=21thensys49152
10 &g 1,13,0: poke 53280,5
15 i=rnd(-ti)
20 &s 1 : for i=1 to 70 : rem draw random frames
30 x=rnd(1)*270+10
35 y=rnd(1)*160+10
40 &b x,y,30,20
50 next
60 &f 0,0 : rem flood fill everything around them
70 for i=1 to 2000: next : rem wait
80 &s0: &f 0,0 : rem vanishing it with fill
85 for i=1 to 500: next : rem short wait
90 goto 20
