    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
    6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):d=56590:goto10
    7 t=fnt(peek(d-3))*1e4+fnt(peek(d-4))*100+fnt(peek(d-5))+fnt(peek(d-6))/10:return
    9 poked,(127andpeek(d))+128*peek(678):poked-3,0:poked-4,0:poked-5,0:poked-6,0:return
   10 sys(57812)"butterfly-test",8,0:poke781,0:poke782,224:sys65493
   15 gosub9
   20 &g2,0,5
  100 ti$="000000"
  150 &f160,100
  200 tt=ti:gosub7:print tt,t
 9999 wait198,1:&g0
