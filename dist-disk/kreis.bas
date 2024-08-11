    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
  100 &g1,5,0
  105 forr=9to89step10
  110 mx=r+60:my=r+1:gosub1000
  115 next
  120 fora=0to1:geta$:a=len(a$):next
  130 &g0:end
  140 :
 1000 x=r:d2=1-x
 1010 fory=0tor:ify>xthenreturn
 1010 &p mx+x,my+y
 1010 &p mx+x,my-y
 1010 &p mx-x,my+y
 1010 &p mx-x,my-y
 1010 &p mx+y,my+x
 1010 &p mx+y,my-x
 1010 &p mx-y,my+x
 1010 &p mx-y,my-x
 1010 ifd2>0thenx=x-1:d2=d2-x-x
 1010 d2=d2+y+y+3
 1010 next:return
