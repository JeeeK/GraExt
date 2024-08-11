    1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
    2 ifpeek(49153)+peek(49154)=21thensys49152
  100 &g1,5,0
  105 forr=9to89step10
  110 mx=r+60:my=r+1:e=1:f=0.7:gosub1000
  115 next
  120 fora=0to1:geta$:a=len(a$):next
  130 &g0:end
  140 :
 1000 x=r:d2=-x
 1010 xe=x*e:ye=0:xf=x*f:yf=0
 1020 fory=0tor:ify>xthenreturn
 1030 &p mx+xf,my+ye
 1040 &p mx+xf,my-ye
 1050 &p mx-xf,my+ye
 1060 &p mx-xf,my-ye
 1070 &p mx+yf,my+xe
 1080 &p mx+yf,my-xe
 1090 &p mx-yf,my+xe
 1100 &p mx-yf,my-xe
 1210 ifd2>0thenx=x-1:d2=d2-x-x:xe=xe-e:xf=xf-f
 1220 d2=d2+y+y+3:ye=ye+e:yf=yf+f
 1230 next:return
