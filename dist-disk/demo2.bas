    1 ifpeek(49153)+peek(49154)=21thensys49152
   10 l=4*8
   20 y=200-l
   50 &g1,5,0
   60 &c17,1,"demo2"
   70 &s2
  100 t=ti
  110 fori=0to319
  120 ifi<ythen:&l0,i+l,319,199-i
  150 &li,l,319-i,199:next 
  200 &c1,3,str$((ti-t)/60)
 9999 wait198,1:&g0