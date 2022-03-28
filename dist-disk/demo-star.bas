    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
    6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):d=56590:goto10
    7 t=fnt(peek(d-3))*1e4+fnt(peek(d-4))*100+fnt(peek(d-5))+fnt(peek(d-6))/10:return
    9 poked,(127andpeek(d))+128*peek(678):poked-3,0:poked-4,0:poked-5,0:poked-6,0:return
   10 i=10
   90 ti$="000000":gosub9
  100 &g1,5,0:&s1
  110 forx=0to149 step10+i
  120 fory=189 to 0 step-10-i
  130 &lx,y,310-x,190-y
  140 nexty,x
  190 tt=ti:gosub7:printtt/60,t
  999 wait198,1:&g0
