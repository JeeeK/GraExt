    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
    4 dimx,y,w,r,h
    5 f=.5:t=10:s=160:o=100:s1=159:o1=99
    6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):d=56590:goto10
    7 t=fnt(peek(d-3))*1e4+fnt(peek(d-4))*100+fnt(peek(d-5))+fnt(peek(d-6))/10:return
    9 poked,(127andpeek(d))+128*peek(678):poked-3,0:poked-4,0:poked-5,0:poked-6,0:return
   10 &g1,13,0:&s1
  100 ti$="000000":gosub9
  110 forx=1tos:fory=-otoo1
  120 w=atn(y/x):r=sqr(x*x+y*y)
  130 h=f+f*(sin(w+w+log(r)*t))
  140 ifh<rnd(1)then160
  150 &ps-x,o+y:&ps1+x,o1-y
  160 next:next
 9996 tt=ti:gosub7
 9997 poke198,0:wait198,1
 9998 &g0
 9999 printtt/60,t


