5 rem fill demo: frame cloud filling
6 rem 2016-08-12 johann # klasek at
7 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
8 ifpeek(49153)+peek(49154)=21thensys49152
10 &g 1,13,0: poke 53280,5: &s 1
15 i=rnd(-ti)
20 for i=1 to 70 : rem draw random frames
30 x=rnd(1)*270+10
35 y=rnd(1)*160+10
40 gosub 100
50 next
60 &f 0,0 : rem flood fill everything around them
70 for i=1 to 2000: next : rem wait
80 &s0: &f 0,0 : rem vanishing it with fill
85 for i=1 to 500: next : rem short wait
90 run
100 &h x,y,30,20
101 &s 0
102 &h x+1,y+1,28,18
103 &s 1
104 return
