    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
    6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):goto10
    7 t=fnt(peek(d+11))*10000+fnt(peek(d+10))*100+fnt(peek(d+9))+fnt(peek(d+8))/10:return
    9 d=56576:poked+14,128orpeek(d+14):poked+15,127andpeek(d+15):poked+11,0:poked+10,0:poked+9,0:poked+8,0:return
   10 dimi,n
   50 &g1,13,0:&s2
  100 ti$="000000":gosub9
  160 forn=1to10
  200 fori=0to319
  220 &vi,0,199:next
  250 next:tt=ti
 9997 gosub7:printtt/60,t
 9998 &g0
