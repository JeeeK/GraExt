1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
2 ifpeek(49153)+peek(49154)=21thensys49152
6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):goto10
7 t=fnt(peek(d+11))*10000+fnt(peek(d+10))*100+fnt(peek(d+9))+fnt(peek(d+8))/10:return
9 d=56576:poked+14,128orpeek(d+14):poked+15,127andpeek(d+15):poked+11,0:poked+10,0:poked+9,0:poked+8,0:return
10 dimx
20 &g1,5,0 : rem grafik ein
21 &s4 : rem fehlermodus: abbruch
22 &s2 : rem grafikmodus: toggle
50 &l0,0,319,199
51 &l0,199,319,0
100 ti$="000000":gosub9
110 forx=0to319
120 &hx,0,0,199
130 next
140 tt=ti:gosub7:printtt/60,t
199 rem
200 ti$="000000":gosub9
210 forx=0to319
220 &vx,0,199
230 next
240 tt=ti:gosub7:printtt/60,t
299 rem
300 ti$="000000":gosub9
310 &h0,0,319,199
340 tt=ti:gosub7:printtt/60,t
999 wait198,1:&g0
