    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
    6 dimt,d,x:deffnt(x)=int(x/16)*10+(xand15):goto10
    7 t=fnt(peek(d+11))*10000+fnt(peek(d+10))*100+fnt(peek(d+9))+fnt(peek(d+8))/10:return
    9 d=56576:poked+14,128orpeek(d+14):poked+15,127andpeek(d+15):poked+11,0:poked+10,0:poked+9,0:poked+8,0:return

   10 l=4*8
   20 y=200-l
   50 &g1,5,0
   60 &c17,1,"demo2"
   70 &s2
  100 ti$="000000":gosub9
  110 fori=0to319
  120 ifi<ythen:&l0,i+l,319,199-i
  150 &li,l,319-i,199:next 
  200 tt=ti:gosub7:&c1,3,str$(tt/60)+str$(t)
 9999 wait198,1:&g0
