    1 rem gra-ext gliding lines
    2 rem 2015-09-15 johann+c64@klasek.at
   10 deffnr(x)=int(rnd(1)*x)
   11 sys49152
   50 r=rnd(-ti)
  100 &g 1,13,0:poke53280,15
  105 &s 2
  150 z=0:ym=199:xm=319
  160 l=40:s=9
  170 dimx0(l),x1(l),y0(l),y1(l)
  175 h0=fnr(2*s)-s
  176 h1=fnr(2*s)-s
  177 v0=fnr(2*s)-s
  178 v1=fnr(2*s)-s
  180 x0(z)=fnr(320)
  181 x1(z)=fnr(320)
  182 y0(z)=fnr(200)
  183 y1(z)=fnr(200)
  190 i=z:f=z:p=-l+2
  200 forj=-1toz
  210 if p<z then 250
  230 &s0
  240 &l x0(p),y0(p),x1(p),y1(p)
  245 &s1:goto 255
  250 f=f+1:rem active lines
  255 n=i+1:ifn=lthenn=z
  256 p=p+1:ifp=lthenp=z
  260 x0(n)=x0(i)+h0:ifx0(n)>xmorx0(n)<zthenx0(n)=x0(i):h0=-h0
  270 y0(n)=y0(i)+v0:ify0(n)>ymory0(n)<ztheny0(n)=y0(i):v0=-v0
  280 x1(n)=x1(i)+h1:ifx1(n)>xmorx1(n)<zthenx1(n)=x1(i):h1=-h1
  290 y1(n)=y1(i)+v1:ify1(n)>ymory1(n)<ztheny1(n)=y1(i):v1=-v1
  300 i=n
  310 &l x0(i),y0(i),x1(i),y1(i)
  900 geta$:j=(a$="")
  910 next
  920 &g0
