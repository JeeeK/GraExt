1 ifpeek(49153)+peek(49154)<>21thenload"ge",8,1
2 ifpeek(49153)+peek(49154)=21thensys49152
10 &g1,5,0 : rem grafik ein
20 &s4 : rem fehlermodus: abbruch
21 &s2 : rem grafikmodus: toggle
50 &l0,0,319,199
51 &l0,199,319,0
100 t=ti
110 forx=0to319
120 &hx,0,0,199
130 next
140 print (ti-t)/60
199 :
200 t=ti 
210 forx=0to319
220 &vx,0,199
230 next
240 print (ti-t)/60
299 :
300 t=ti
310 &h0,0,319,199
320 print (ti-t)/60
999 wait 198,1
