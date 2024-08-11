    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
    6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):d=56590:goto10
    7 t=fnt(peek(d-3))*1e4+fnt(peek(d-4))*100+fnt(peek(d-5))+fnt(peek(d-6))/10:return
    9 poked,(127andpeek(d))+128*peek(678):poked-3,0:poked-4,0:poked-5,0:poked-6,0:return
   10 printchr$(147):&g1,5,0:&s1
   11 goto 100
   20 r=sin(5*k*t)
   21 x2=80.5-57*r*sin(k*t):y2=96.5-95*r*cos(k*t):return
  100 &c17,1,"blume"
  101 ti$="000000":gosub9 : rem timer start
  110 k=~/72:t=0:gosub20
  120 fort=1to72
  130 x1=x2:y1=y2:gosub20
  140 &lx1,y1,x2,y2
  150 next
 9998 tt=ti:gosub7:&c0,24,str$(tt/60)+str$(t) : rem timer end
 9999 wait198,1:&g0
