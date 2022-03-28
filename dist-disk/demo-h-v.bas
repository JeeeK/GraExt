1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
2 ifpeek(49153)+peek(49154)=21thensys49152
6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):d=56590:goto10
7 t=fnt(peek(d-3))*1e4+fnt(peek(d-4))*100+fnt(peek(d-5))+fnt(peek(d-6))/10:return
9 poked,(127andpeek(d))+128*peek(678):poked-3,0:poked-4,0:poked-5,0:poked-6,0:return
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
