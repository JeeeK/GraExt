
; ******** Source: ge-run.asm
     1                          !to "ge-run.prg",cbm	
     2                          
     3                          ;  **** gra-ext loader ****
     4                          ;
     5                          ; 2016-05-18 johann e. klasek, johann at klasek at
     6                          ;
     7                          ; revisions:
     8                          ;	2016-05-18 v 1.21
     9                          ;
    10                          ;
    11                          ; Usage: RUN
    12                          ;
    13                          
    14                          ; loader for BASIC
    15                          
    16                          *= $0801
    17                          basic_start
    18                          ;       2013 sys2061
    19  0801 0b08dd079e         	!by <EOP,>EOP,<(2013),>(2013),$9E
    20  0806 32303631           	!tx "2061"
    21  080a 00                 	!by 0 		; End of Line
    22  080b 0000               EOP	!by 0, 0	; Basic-Programmende
    23                          
    24                          loader
    25                          !if loader != 2061 {
    26                          	!error "Loader-Adresse stimmt nicht mit SYS-Adresse überein!"
    27                          }
    28                          
    29  080d a2a2               	ldx #<graext_end	; setup basic
    30  080f a00e               	ldy #>graext_end
    31  0811 18                 	clc			; set if C=0
    32  0812 209cff             	jsr $ff9c		; KERNAL: system RAM bottom
    33  0815 862b               	stx $2b			; BASIC text start
    34  0817 842c               	sty $2c
    35  0819 2016e4             	jsr $e416		; setup BASIC text start
    36  081c 202908             	jsr init		; init extension (place hook)
    37  081f a984               	lda #<author		; message ...
    38  0821 a008               	ldy #>author
    39  0823 2041e4             	jsr $e441		; output string and perform BASIC NEW (set remaining pointers)
    40  0826 4c86e3             	jmp $e386		; BASIC warm start
    41                          

; ******** Source: graext-core.asm
     1                          ;  **** gra-ext core ****
     2                          ;
     3                          ; 2015-10-05 johann e. klasek, johann at klasek at
     4                          ;
     5                          ; revisions:
     6                          ;	2016-05-16 v 1.21
     7                          ;	2016-02-23 v 1.20
     8                          ;	2016-01-15 v 1.19
     9                          ;	1992-12-28 v 1.18
    10                          ;	1986-03-24 v 1.17
    11                          ;	1985       v 0.00 - 1.16
    12                          ;
    13                          ; the original source has been lost.
    14                          ; development has based on the implemention
    15                          ; done on a forth-64 written with its forth assembler.
    16                          ; the code has been pulled out from there and enriched
    17                          ; with some glue code to get a basic extension.
    18                          
    19                          ; command dispatcher style JMP/RTS
    20                          ;	(if defined)
    21                          ;command_rts_style=1
    22                          
    23                          ; error handling 
    24                          ;	(if defined)
    25                          ;no_error=1
    26                          
    27                          ; basic interpreter registers, addresses and entry points
    28                          
    29                          str     = $22		; string address
    30                          ijmp    = $55		; address of JMP (addr)
    31                          chrget  = $73		; basic charget routine
    32                          facintl = $65		; integer result from b_fac2int
    33                          facinth = $64
    34                          facexp  = $61		; fac exponent, after b_getval
    35                          
    36                          z_reverseflag = $C7	; character routine
    37                          z_lastkey = $D7		; original use case, unused here
    38                          z_tmp = z_lastkey	; temporary reused for character routine
    39                          
    40                          v_bascmd = $0308
    41                          
    42                          basic_rom = $A000	; start of BASIC ROM
    43                          
    44                          b_interpreter =$A7AE	; interpreter loop
    45                          b_execstatement =$A7E7	; process statement
    46                          b_getcomma = $AEFD	; read comma from basic text
    47                          b_illquant = $B248	; error "illegal quantity"
    48                          b_get8bit = $B79E	; read 8 bit numeric value from
    49                          			; basic text
    50                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    51                          			; from basic text
    52                          b_getval = $AD8A	; read numeric value from basic text
    53                          b_getexpr = $AD9E	; read expression from basic text
    54                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    55                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    56                          b_fac2int = $BC9B	; convert FAC to integer
    57                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    58                          
    59                          ; hardware registers and values
    60                          
    61                          prozport = $01		; processor port
    62                          memrom = %00110111	; basic+kernal rom
    63                          membas = %00110110	; basic ram+kernal rom
    64                          memram = %00110101	; basic+kernal ram
    65                          
    66                          vic_cr	= $D011		; VIC control register
    67                          vic_mcr	= $D018		; VIC memory control register
    68                          cia_pra	= $DD00		; CIA 2 port register A
    69                          
    70                          cram	= $CC00		; start of color ram
    71                          
    72                          gram	= $e000		; start of graphic bitmap ram
    73                          gramp	= gram >> 8	; start page of bitmap
    74                          
    75                          ; constants 
    76                          
    77                          xmax	= 320		; max x dimension
    78                          ymax	= 200		; max y dimension
    79                          
    80                          ; zeropage variables
    81                          
    82                          x	= $9B		; start coordinate x, low+high
    83                          xl	= x
    84                          xh	= x+1
    85                          y	= $AA		; start coordinate y
    86                          
    87                          xendl	= $9E		; end coordinate x, low+high
    88                          xendh	= $9F
    89                          yend	= $93		; end coordinate y
    90                          
    91                          kl	= $95		; gradient for lines, low+high
    92                          kh	= kl+1
    93                          
    94                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
    95                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
    96                          
    97                          dxl	= $AB		; x delta, low+high
    98                          dxh	= $A7
    99                          
   100                          dy	= $A9		; y delta
   101                          
   102                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   103                          
   104                          cl	= $A3		; dot count, low+high
   105                          ch	= $A4
   106                          
   107                          gaddr	= $A5		; graphic address
   108                          
   109                          gpos	= $FB		; in graphic position
   110                          
   111                          gcol	= $FD		; graphic color, in "graphic on" context only
   112                          
   113                          ;
   114                          ; initialize extension
   115                          
   116                          init
   117  0829 a934                       LDA #<(parse)	; basic interpreter parser hook
   118  082b 8d0803                     STA v_bascmd
   119  082e a908                       LDA #>(parse)
   120  0830 8d0903                     STA v_bascmd+1
   121  0833 60                         RTS
   122                          
   123                          
   124                          ;-----------------------------------------------------------------
   125                          
   126                          ; start parsing an extension command ...
   127                          
   128                          parse
   129  0834 207300                     JSR chrget			; next char.
   130  0837 08                 	PHP
   131  0838 c926                       CMP #'&'			; command prefix
   132  083a f004                       BEQ newcmd
   133  083c 28                         PLP
   134  083d 4ce7a7                     JMP b_execstatement
   135                          newcmd
   136  0840 28                 	PLP
   137  0841 207300                     JSR chrget			; command character
   138                          
   139  0844 a00b                       LDY #(cmdsend-cmds)		; map character to
   140                          					; command address ...
   141                          checknextcmd
   142  0846 88                         DEY
   143  0847 f019               	BEQ parse_exit
   144  0849 d96508                     CMP cmds,Y
   145  084c d0f8                       BNE checknextcmd		; try next
   146  084e 88                         DEY				; found
   147  084f 98                         TYA
   148  0850 0a                         ASL				; *2
   149  0851 a8                         TAY
   150                          !ifndef command_rts_tyle {
   151                          	!set co=0			; command offset in jump table
   152  0852 b97108                     LDA cmdaddr+1,Y                 ; high byte from table
   153  0855 8556                       STA ijmp+1
   154  0857 b97008                     LDA cmdaddr,Y                   ; low byte from table
   155  085a 8555                       STA ijmp
   156  085c 207300                     JSR chrget			; read next byte in basic text
   157  085f 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   158                          } else {
   159                          	!set co=1			; command offset in jump table
   160                          	LDA #>(b_interpreter-1)		; return to interpreter
   161                          	PHA
   162                          	LDA #<(b_interpreter-1)
   163                          	PHA				
   164                                  LDA cmdaddr+1,Y			; command address (RTS style)
   165                                  PHA				; high byte on stack
   166                                  LDA cmdaddr,Y			; command address (RTS style)
   167                                  PHA				; low byte on stack
   168                                  JMP chrget			; read next byte in basic text 
   169                          					; and RTS to command
   170                          }
   171                          parse_exit
   172  0862 4caea7                     JMP b_interpreter		; continue parsing
   173                          
   174                          ;-----------------------------------------------------------------
   175                          
   176                          ; the most commonly used command placed at the end ...
   177                          
   178  0865 204743534d525456...cmds	!text " GCSMRTVHLP"		; first char. is a dummy
   179                          cmdsend
   180                          
   181                          cmdaddr
   182  0870 ee08dc0dfa0ced0c...        !word graphic-co,char-co,setmode-co,move-co,relto-co
   183  087a 8d0eaa0bf90a150c...        !word to-co,vline-co,hline-co,line-co,plot-co
   184                          
   185  0884 934752412d455854...author	!text 147,"GRA-EXT V1.21 1986,2016 JOHANN@KLASEK.AT",0
   186                          
   187                          bitmask
   188  08ae 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   189                          nbitmask
   190  08b6 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   191                          ytabl
   192  08be 004080c0           	!byte $00,$40,$80,$c0
   193                          ytabh
   194  08c2 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   195  08c6 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   196  08ca eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   197  08ce eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   198  08d2 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   199  08d6 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   200  08da fe                 	!byte gramp+$1e
   201                          
   202  08db 3a                 savexl	!byte $3a
   203  08dc 01                 savexh	!byte $01
   204  08dd 71                 savey	!byte $71
   205                          
   206                          ; for horiz. line
   207                          
   208  08de ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   209                          
   210  08e6 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   211                          
   212                          
   213                          ;-----------------------------------------------------------------
   214                          
   215                          graphic
   216  08ee 209eb7                     JSR b_get8bit
   217  08f1 e000                       CPX #$00
   218  08f3 d013                       BNE graphic_on
   219                          gra0			; &G 0
   220  08f5 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   221  08f7 8d00dd                     STA cia_pra
   222  08fa a915                       LDA #((1 <<4) + (2 <<1) + 1)
   223                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   224                          			; char addr $1000/4096 = char. ROM
   225  08fc 8d18d0                     STA vic_mcr	; VIC memory control
   226  08ff ad11d0                     LDA vic_cr	; VIC control register
   227  0902 29df                       AND #%11011111	; Hires mode off
   228  0904 8d11d0                     STA vic_cr
   229  0907 60                         RTS
   230                          graphic_on
   231  0908 e001                       CPX #$01
   232  090a d01f                       BNE gra2
   233                          gra1			; &G 1
   234  090c a000                       LDY #$00
   235  090e a220                       LDX #$20	; Pages (8 KByte)
   236  0910 a9e0                       LDA #>gram
   237  0912 85fc                       STA gpos+1
   238  0914 84fb                       STY gpos
   239  0916 a900                       LDA #$00
   240                          gra_clear
   241  0918 91fb                       STA (gpos),Y	; Loop unroll
   242  091a c8                         INY
   243  091b 91fb                       STA (gpos),Y
   244  091d c8                         INY
   245  091e 91fb                       STA (gpos),Y
   246  0920 c8                         INY
   247  0921 91fb                       STA (gpos),Y
   248  0923 c8                         INY
   249  0924 d0f2                       BNE gra_clear
   250  0926 e6fc                       INC gpos+1
   251  0928 ca                         DEX
   252  0929 d0ed                       BNE gra_clear
   253                          gra2
   254  092b 20f1b7                     JSR b_getcomma8bit
   255  092e 8a                         TXA		; foreground color
   256  092f 0a                         ASL		; upper nibble
   257  0930 0a                         ASL
   258  0931 0a                         ASL
   259  0932 0a                         ASL
   260  0933 85fd                       STA gcol
   261  0935 20f1b7                     JSR b_getcomma8bit
   262  0938 8a                         TXA		; background color
   263  0939 290f                       AND #$0F
   264  093b 05fd                       ORA gcol
   265  093d a000                       LDY #$00
   266                          cram_loop
   267  093f 9900cc                     STA cram,Y
   268  0942 9900cd                     STA cram+$100,Y
   269  0945 9900ce                     STA cram+$200,Y
   270  0948 99e8ce                     STA cram+$300-24,Y
   271  094b c8                         INY
   272  094c d0f1                       BNE cram_loop
   273  094e a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   274  0950 8d00dd                     STA cia_pra
   275  0953 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   276  0955 8d18d0                     STA vic_mcr	; VIC memory control
   277  0958 ad11d0                     LDA vic_cr	; VIC control register
   278  095b 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   279  095d 8d11d0                     STA vic_cr
   280  0960 60                         RTS
   281                          
   282                          
   283                          ;-----------------------------------------------------------------
   284                          
   285                          gexit
   286  0961 a501                       LDA prozport
   287  0963 0902                       ORA #%00000010	; kernal ROM enable
   288  0965 8501                       STA prozport
   289  0967 58                         CLI		; allow interrupts
   290  0968 60                         RTS
   291                          
   292                          ;-----------------------------------------------------------------
   293                          
   294                          ginit
   295  0969 a501                       LDA prozport
   296  096b 29fd                       AND #%11111101	; Kernal ROM disable
   297  096d 78                         SEI		; disable interrupts
   298  096e 8501                       STA prozport
   299  0970 60                         RTS
   300                          
   301                          ;-----------------------------------------------------------------
   302                          
   303                          gchange
   304  0971 b1a5                       LDA (gaddr),Y
   305                          gchange_op
   306  0973 1dae08                     ORA bitmask,X
   307  0976 91a5                       STA (gaddr),Y
   308  0978 60                         RTS
   309                          
   310                          ;-----------------------------------------------------------------
   311                          
   312                          gmask
   313                          gmask_flip
   314  0979 4900                       EOR #$00
   315                          gmask_op
   316  097b 11a5                       ORA (gaddr),Y
   317  097d 91a5                       STA (gaddr),Y
   318  097f 60                         RTS
   319                          
   320                          ;-----------------------------------------------------------------
   321                          
   322                          position
   323  0980 a5aa                       LDA y
   324  0982 4a                         LSR
   325  0983 4a                         LSR
   326  0984 4a                         LSR		; y/8
   327  0985 a8                         TAY
   328  0986 2903                       AND #%00000011	; (y/8) mod 4
   329  0988 aa                         TAX
   330  0989 a59b                       LDA xl		; x low
   331  098b 29f8                       AND #%11111000	; clear bit 2-0
   332  098d 18                         CLC
   333  098e 7dbe08                     ADC ytabl,X	; addr low: y base + x part
   334  0991 85a5                       STA gaddr
   335  0993 a59c                       LDA xh		; addr high: x part
   336  0995 79c208                     ADC ytabh,Y	; 	+ y base
   337  0998 85a6                       STA gaddr+1
   338  099a a5aa                       LDA y		; vertical offset
   339  099c 2907                       AND #%00000111	; y mod 8
   340  099e a8                         TAY
   341  099f a59b                       LDA xl
   342  09a1 2907                       AND #%00000111	; x mod 8
   343  09a3 aa                         TAX		; horizonal offset
   344  09a4 60                         RTS		; (bitmask)
   345                          
   346                          
   347                          ;-----------------------------------------------------------------
   348                          
   349                          ; line y up, x right, dx < dy (case 1)
   350                          
   351                          line_up_steep
   352  09a5 208009                     JSR position	; x,y
   353                          loop_yup_xright
   354  09a8 207109                     JSR gchange	; pixel
   355                          
   356  09ab 18                         CLC		; k += dx
   357  09ac a595                       LDA kl
   358  09ae 65ab                       ADC dxl		; dxh is 0, because dx < dy
   359  09b0 8595                       STA kl
   360  09b2 b004                       BCS ++		; k > 255
   361                          
   362  09b4 c5a9                       CMP dy
   363  09b6 9015                       BCC +		; k >= dy ->
   364                          
   365  09b8 e5a9               ++	SBC dy		; k -= dy
   366  09ba 8595                       STA kl
   367                          
   368  09bc e8                         INX		; x++
   369  09bd e008                       CPX #8
   370  09bf d00c                       BNE +
   371                          	; C=1
   372  09c1 a200                       LDX #0		; x overflow, wrap around
   373  09c3 a5a5                       LDA gaddr	; x+8: gaddr += 8
   374  09c5 6907                       ADC #8-1	; C already set by CPX
   375  09c7 85a5                       STA gaddr
   376  09c9 9002                       BCC +
   377  09cb e6a6                       INC gaddr+1
   378                          
   379  09cd 88                 +	DEY		; y--
   380  09ce 100f                       BPL +++
   381  09d0 38                         SEC		; y overflow
   382  09d1 a5a5                       LDA gaddr
   383  09d3 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   384  09d5 85a5                       STA gaddr
   385  09d7 a5a6                       LDA gaddr+1
   386  09d9 e901               	SBC #1
   387  09db 85a6                       STA gaddr+1
   388  09dd a007                       LDY #7		; wrap around
   389                          
   390  09df c6a3               +++	DEC cl		; until c=0
   391  09e1 d0c5                       BNE loop_yup_xright
   392  09e3 4c6109                     JMP gexit
   393                          
   394                          
   395                          ;-----------------------------------------------------------------
   396                          
   397                          ; line x right, y up, dx > dy (case 2)
   398                          
   399                          line_up_flat
   400  09e6 208009                     JSR position	; x,y
   401  09e9 a5a3               	LDA cl		; counter adjustment for
   402  09eb f002               	BEQ +		; dec-dec-counting
   403  09ed e6a4               	INC ch
   404                          +
   405                          loop_xright_yup
   406  09ef 207109                     JSR gchange	; pixel
   407                          
   408  09f2 18                         CLC		; k += dy
   409  09f3 a595                       LDA kl
   410  09f5 65a9                       ADC dy
   411  09f7 8595                       STA kl
   412  09f9 9002                       BCC ++
   413  09fb e696                       INC kh
   414                          
   415  09fd c5ab               ++	CMP dxl		; k > dx?
   416  09ff a596                       LDA kh
   417  0a01 e5a7                       SBC dxh
   418  0a03 901a                       BCC +
   419                          
   420  0a05 8596                       STA kh		; k -= dx
   421  0a07 a595                       LDA kl
   422  0a09 e5ab                       SBC dxl
   423  0a0b 8595                       STA kl
   424                          
   425  0a0d 88                         DEY		; y--
   426  0a0e 100f                       BPL +
   427  0a10 38                 	SEC		; C=1 not always true (SBC above)
   428  0a11 a5a5                       LDA gaddr	; y overflow
   429  0a13 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   430  0a15 85a5                       STA gaddr
   431  0a17 a5a6                       LDA gaddr+1
   432  0a19 e901               	SBC #1
   433  0a1b 85a6                       STA gaddr+1
   434  0a1d a007               	LDY #7		; wrap around
   435                          
   436  0a1f e8                 +	INX		; x++
   437  0a20 e008                       CPX #8		; x overflow?
   438  0a22 d00c                       BNE ++
   439                          	; C=1
   440  0a24 a200                       LDX #0		; wrap around
   441  0a26 a5a5                       LDA gaddr	; x+8: gaddr += 8
   442  0a28 6907                       ADC #8-1	; C already set by CPX
   443  0a2a 85a5                       STA gaddr
   444  0a2c 9002                       BCC ++
   445  0a2e e6a6                       INC gaddr+1
   446                          ++
   447  0a30 c6a3               	DEC cl		; c--
   448  0a32 d0bb                       BNE loop_xright_yup
   449  0a34 c6a4                       DEC ch		; adjusted high which allows this
   450  0a36 d0b7                       BNE loop_xright_yup
   451                          
   452  0a38 4c6109                     JMP gexit
   453                          
   454                          
   455                          
   456                          ;-----------------------------------------------------------------
   457                          
   458                          ; line x right, y down, dx > dy (case 3)
   459                          
   460                          line_down_flat
   461  0a3b 208009                     JSR position	; x,y
   462  0a3e a5a3               	LDA cl		; counter adjustment for
   463  0a40 f002               	BEQ +		; dec-dec-counting
   464  0a42 e6a4               	INC ch
   465                          +
   466                          loop_xright_ydown
   467  0a44 207109                     JSR gchange	; pixel
   468                          
   469  0a47 18                         CLC		; k += dy
   470  0a48 a595                       LDA kl
   471  0a4a 65a9                       ADC dy
   472  0a4c 8595                       STA kl
   473  0a4e 9002                       BCC ++
   474  0a50 e696                       INC kh
   475                          
   476  0a52 c5ab               ++	CMP dxl		; k > dx
   477  0a54 a596                       LDA kh
   478  0a56 e5a7                       SBC dxh		; k -= dx
   479  0a58 901b                       BCC +
   480                          
   481  0a5a 8596                       STA kh
   482  0a5c a595                       LDA kl
   483  0a5e e5ab                       SBC dxl
   484  0a60 8595                       STA kl
   485                          
   486  0a62 c8                         INY		; y++
   487  0a63 c008                       CPY #8
   488  0a65 d00e                       BNE +
   489                          	; C=1
   490  0a67 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   491  0a69 693f                       ADC #$40-1	; C already set by CPY
   492  0a6b 85a5                       STA gaddr
   493  0a6d a5a6                       LDA gaddr+1
   494  0a6f 6901               	ADC #1
   495  0a71 85a6                       STA gaddr+1
   496  0a73 a000                       LDY #0		; wrap around
   497                          
   498  0a75 e8                 +	INX		; x++
   499  0a76 e008                       CPX #8		; x overflow ?
   500  0a78 d00c                       BNE +++
   501                          	; C=1
   502  0a7a a200                       LDX #$00	; wrap around
   503  0a7c a5a5                       LDA gaddr	; gaddr += 8
   504  0a7e 6907                       ADC #$08-1	; C always set by CPX
   505  0a80 85a5                       STA gaddr
   506  0a82 9002                       BCC +++
   507  0a84 e6a6                       INC gaddr+1
   508                          +++
   509  0a86 c6a3               	DEC cl		; c--
   510  0a88 d0ba                       BNE loop_xright_ydown
   511  0a8a c6a4                       DEC ch		; adjusted high which allows this
   512  0a8c d0b6                       BNE loop_xright_ydown
   513                          
   514  0a8e 4c6109                     JMP gexit
   515                          
   516                          
   517                          ;-----------------------------------------------------------------
   518                          
   519                          ; line y down, x right, dx < dy (case 4)
   520                          
   521                          line_down_steep
   522  0a91 208009                     JSR position	; x,y
   523                          loop_ydown_xright
   524  0a94 207109                     JSR gchange	; pixel
   525                          
   526  0a97 18                         CLC		; k += dx
   527  0a98 a595                       LDA kl
   528  0a9a 65ab                       ADC dxl		; dxh is 0, because dx < dy
   529  0a9c 8595                       STA kl
   530  0a9e b004                       BCS ++
   531  0aa0 c5a9                       CMP dy		; k > dy?
   532  0aa2 9015                       BCC +
   533  0aa4 e5a9               ++	SBC dy		; k -= dy
   534  0aa6 8595                       STA kl
   535                          
   536  0aa8 e8                         INX		; x++
   537  0aa9 e008                       CPX #8
   538  0aab d00c                       BNE +		; x overflow?
   539  0aad a200                       LDX #0		; wrap around
   540  0aaf a5a5                       LDA gaddr	; x+9: gaddr += 8
   541  0ab1 6907                       ADC #8-1	; C already set by CPX
   542  0ab3 85a5                       STA gaddr
   543  0ab5 9002                       BCC +
   544  0ab7 e6a6                       INC gaddr+1
   545                          
   546  0ab9 c8                 +	INY		; y++
   547  0aba c008                       CPY #8		; y overflow?
   548  0abc d00e                       BNE +++
   549  0abe a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   550  0ac0 693f                       ADC #$40-1	; C already set by CPY
   551  0ac2 85a5                       STA gaddr
   552  0ac4 a5a6                       LDA gaddr+1
   553  0ac6 6901               	ADC #1
   554  0ac8 85a6                       STA gaddr+1
   555  0aca a000                       LDY #0		; wrap around
   556                          
   557  0acc c6a3               +++	DEC cl		; c--
   558                          			; until c=0
   559  0ace d0c4                       BNE loop_ydown_xright
   560  0ad0 4c6109                     JMP gexit
   561                          
   562                          
   563                          ;-----------------------------------------------------------------
   564                          
   565                          getcommaxy
   566  0ad3 20fdae                     JSR b_getcomma	; check ","
   567                          getxy
   568  0ad6 208aad                     JSR b_getval	; get X coord. value
   569  0ad9 20f7b7                     JSR b_convint
   570  0adc c901                       CMP #>xmax
   571  0ade 9009               	BCC gcxy_xok
   572  0ae0 f003                       BEQ +		; X = $1xx
   573                          error_iq
   574                          !ifdef no_error {
   575                          	RTS
   576                          } else {
   577  0ae2 4c48b2                     JMP b_illquant
   578                          }
   579  0ae5 c040               +	CPY #<xmax	; check X low
   580  0ae7 b0f9                       BCS error_iq	; X to big
   581                          gcxy_xok
   582  0ae9 84fb                       STY gpos	; temporary save X coord.
   583  0aeb 85fc                       STA gpos+1
   584                          
   585  0aed 20f1b7                     JSR b_getcomma8bit
   586                          			; get Y coord. value
   587  0af0 e0c8                       CPX #ymax
   588  0af2 b0ee                       BCS error_iq	; Y to big
   589                          
   590  0af4 a4fb                       LDY gpos	; restory X coord.
   591  0af6 a5fc                       LDA gpos+1
   592  0af8 60                         RTS
   593                          
   594                          
   595                          ;-----------------------------------------------------------------
   596                          
   597                          hline
   598  0af9 20d60a                     JSR getxy	; get startpoint
   599  0afc 86aa                       STX y
   600  0afe 8edd08                     STX savey	; save as cursor, too
   601  0b01 859c                       STA xh
   602  0b03 849b                       STY xl
   603  0b05 20fdae                     JSR b_getcomma	; get length
   604  0b08 208aad                     JSR b_getval
   605  0b0b 20f7b7                     JSR b_convint
   606                          
   607  0b0e c901                       CMP #>xmax
   608  0b10 9006                       BCC +		; X < 256
   609  0b12 d0ce                       BNE error_iq
   610  0b14 c040                       CPY #<xmax
   611  0b16 b0ca                       BCS error_iq
   612                          +
   613                          			; calculate end point
   614  0b18 aa                         TAX		; save length high byte
   615  0b19 98                         TYA		; length low byte
   616  0b1a 18                         CLC
   617  0b1b 659b                       ADC xl		; low xend = x+length
   618  0b1d 859e                       STA xendl
   619  0b1f a8                 	TAY
   620  0b20 8a                         TXA		; high
   621  0b21 659c                       ADC xh		; high xend = x+length
   622  0b23 859f                       STA xendh
   623  0b25 aa                 	TAX
   624                          
   625  0b26 c901               	CMP #>xmax	; endpoint outside?
   626  0b28 9005               	BCC +
   627  0b2a 98                 	TYA
   628  0b2b e940               	SBC #<xmax
   629  0b2d b0b3               	BCS error_iq
   630                          +
   631  0b2f 8edc08                     STX savexh
   632  0b32 8cdb08                     STY savexl	; also save as cursor
   633                          
   634  0b35 206909                     JSR ginit	; map in graphic memory
   635                          
   636                          hline_start
   637  0b38 a59e                       LDA xendl
   638  0b3a c59b                       CMP xl
   639  0b3c a59f                       LDA xendh
   640  0b3e e59c                       SBC xh
   641  0b40 b013                       BCS hl_noxswap	; xend < x ->
   642                          
   643  0b42 a69e                       LDX xendl	; swap x, xend
   644  0b44 a59b                       LDA xl
   645  0b46 869b                       STX xl
   646  0b48 859e                       STA xendl
   647                          
   648  0b4a a69f                       LDX xendh
   649  0b4c a49c                       LDY xh
   650  0b4e 849f                       STY xendh
   651  0b50 869c                       STX xh
   652  0b52 4c640b                     JMP hl_start	; x != xend
   653                          
   654                          hl_noxswap
   655  0b55 a59e                       LDA xendl
   656  0b57 c59b                       CMP xl
   657  0b59 d009                       BNE hl_start
   658  0b5b a59f                       LDA xendh
   659  0b5d c59c                       CMP xh
   660  0b5f d003                       BNE hl_start	; x = xend ->
   661  0b61 4cd80c             	JMP plot_start	; single point
   662                          ;	JMP gexit	; no point
   663                          
   664                          hl_start
   665  0b64 208009                     JSR position	; graphic position x,y
   666  0b67 bdde08                     LDA maskleft,X
   667  0b6a 48                         PHA		; save left end mask
   668  0b6b a59e                       LDA xendl
   669  0b6d 2907                       AND #%00000111
   670  0b6f 8596                       STA tmp2	; xend mod 8, mask index
   671  0b71 a59b                       LDA xl
   672  0b73 29f8                       AND #%11111000	; (xl div 8)*8
   673  0b75 8595                       STA tmp1
   674  0b77 a59e                       LDA xendl	; xend unmasked
   675  0b79 38                         SEC
   676  0b7a e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   677  0b7c 8595                       STA tmp1
   678  0b7e a59f                       LDA xendh
   679  0b80 e59c                       SBC xh
   680  0b82 4a                         LSR		; / 8 ->  0-39
   681  0b83 a595                       LDA tmp1	; only 1 highest bit
   682  0b85 6a                         ROR		; and 3 lower bits
   683  0b86 4a                         LSR
   684  0b87 4a                         LSR
   685  0b88 aa                         TAX		; 8-pixel-blocks count
   686  0b89 68                         PLA		; left end x mask
   687                          
   688                          hl_nextblock
   689  0b8a ca                         DEX
   690                          hl_islastblock
   691  0b8b 3012                       BMI hl_lastblock
   692                          			; leave loop if X<0
   693  0b8d 207909                     JSR gmask	; first with left end mask
   694  0b90 18                         CLC		; gaddr += 8
   695  0b91 a5a5                       LDA gaddr
   696  0b93 6908                       ADC #8
   697  0b95 85a5                       STA gaddr
   698  0b97 9002                       BCC +
   699  0b99 e6a6                       INC gaddr+1
   700  0b9b a9ff               +	LDA #$FF	; following with full 8-pixel mask
   701  0b9d d0eb               	BNE hl_nextblock	; always
   702                          
   703                          hl_lastblock
   704  0b9f a696                       LDX tmp2	; xend mask index
   705  0ba1 3de608                     AND maskright,X ; mask right end
   706  0ba4 207909                     JSR gmask	; modify
   707  0ba7 4c6109                     JMP gexit	; leave
   708                          
   709                          
   710                          ;-----------------------------------------------------------------
   711                          
   712                          vline
   713  0baa 20d60a                     JSR getxy	; get startpoint
   714  0bad 859c                       STA xh
   715  0baf 8ddc08                     STA savexh	; save as cursor too
   716  0bb2 849b                       STY xl
   717  0bb4 8cdb08                     STY savexl
   718  0bb7 86aa                       STX y
   719                          
   720  0bb9 20f1b7                     JSR b_getcomma8bit
   721                          			; get length
   722  0bbc 18                         CLC		; calculate end point
   723  0bbd 8a                         TXA		; length
   724                          ; DON'T-CHANGE: how long to go vertically (needed later)
   725                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   726                          ;	STA tmp1
   727  0bbe 65aa                       ADC y		; length + y
   728  0bc0 c9c8                       CMP #ymax
   729  0bc2 9003                       BCC +
   730                          vline_iq
   731                          !ifdef no_error {
   732                          	RTS
   733                          } else {
   734  0bc4 4c48b2                     JMP b_illquant
   735                          }
   736  0bc7 8593               +	STA yend	; endpoint
   737  0bc9 c9c8               	CMP #ymax	; outside?
   738  0bcb b0f7               	BCS vline_iq
   739                          
   740  0bcd 8ddd08             	STA savey	; set cursor y position
   741                          
   742  0bd0 206909                     JSR ginit	; map in graphic memory
   743                          
   744                          vline_start
   745  0bd3 a593                       LDA yend
   746  0bd5 c5aa                       CMP y
   747  0bd7 b00a                       BCS vl_noyswap	; yend < y ->
   748  0bd9 a5aa                       LDA y		; swap y, yend
   749  0bdb a693                       LDX yend
   750  0bdd 8593                       STA yend
   751  0bdf 86aa                       STX y
   752  0be1 f005               	BEQ vl_start	; always (with next branch)
   753                          	; fall through if yend is
   754                          vl_noyswap
   755  0be3 d003                       BNE vl_start	; y = yend ->
   756  0be5 4cd80c             	JMP plot_start	; single point
   757                          ;	JMP gexit	; no point
   758                          
   759                          vl_start
   760  0be8 208009                     JSR position	; graphic position x,y
   761  0beb bdae08                     LDA bitmask,X
   762  0bee 8596                       STA tmp2	; save mask
   763                          ; DON'T-CHANGE: replace ...
   764  0bf0 38                         SEC
   765  0bf1 a593                       LDA yend
   766  0bf3 e5aa                       SBC y		; vertical length
   767  0bf5 aa                         TAX
   768                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   769                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   770                          ;	LDX tmp1
   771  0bf6 e8                         INX		; +1 (exit on 0)
   772                          vl_nextline
   773  0bf7 a596                       LDA tmp2
   774  0bf9 207909                     JSR gmask	; modify 
   775  0bfc c8                         INY		; go down
   776  0bfd c008                       CPY #8		; 8-line wrap
   777  0bff d00e                       BNE +
   778  0c01 a5a5                       LDA gaddr	; gaddr += 320
   779  0c03 693f               	ADC #$40-1	; compensate for C = 1
   780  0c05 85a5                       STA gaddr
   781  0c07 a5a6                       LDA gaddr+1
   782  0c09 6901                       ADC #$01
   783  0c0b 85a6                       STA gaddr+1
   784  0c0d a000                       LDY #0		; wrap y offset
   785  0c0f ca                 +	DEX		; all vertical positions done?
   786  0c10 d0e5                       BNE vl_nextline
   787  0c12 4c6109                     JMP gexit	; leave
   788                          
   789                          
   790                          ;-----------------------------------------------------------------
   791                          
   792                          line
   793  0c15 20d60a                     JSR getxy	; get startpoint
   794  0c18 849b                       STY xl 
   795  0c1a 859c                       STA xh
   796  0c1c 86aa                       STX y
   797                          
   798  0c1e 20d30a                     JSR getcommaxy	; get endpoint
   799                          line_start
   800  0c21 8cdb08                     STY savexl	; save as cursor position too
   801  0c24 849e                       STY xendl
   802  0c26 8ddc08                     STA savexh
   803  0c29 859f                       STA xendh
   804  0c2b 8edd08                     STX savey
   805  0c2e 8693                       STX yend
   806                          
   807  0c30 206909                     JSR ginit	; map in graphic memory
   808                          
   809  0c33 a000                       LDY #$00	; initialize to 0
   810  0c35 84a8                       STY ydir
   811  0c37 8495                       STY kl
   812  0c39 8496                       STY kh
   813                          
   814  0c3b 38                         SEC
   815  0c3c a59e                       LDA xendl	; calculate dx
   816  0c3e e59b                       SBC xl
   817  0c40 85ab                       STA dxl
   818  0c42 a59f                       LDA xendh
   819  0c44 e59c                       SBC xh
   820  0c46 85a7                       STA dxh
   821                          
   822  0c48 b025                       BCS li_xend_right
   823                          	; dx != 0
   824  0c4a 98                         TYA		; negate dx
   825  0c4b 38                         SEC		; dx = 0 - dx
   826  0c4c e5ab                       SBC dxl
   827  0c4e 85ab                       STA dxl
   828  0c50 98                         TYA
   829  0c51 e5a7                       SBC dxh
   830  0c53 85a7                       STA dxh
   831                          			; C=0 always, needed later
   832  0c55 a69b                       LDX xl		; swap x low
   833  0c57 a49e                       LDY xendl
   834  0c59 869e                       STX xendl
   835  0c5b 849b                       STY xl
   836                          
   837  0c5d a69c                       LDX xh		; swap x high
   838  0c5f a49f                       LDY xendh
   839  0c61 869f                       STX xendh
   840  0c63 849c                       STY xh
   841                          
   842  0c65 a6aa                       LDX y		; swap y
   843  0c67 a493                       LDY yend
   844  0c69 8693                       STX yend
   845  0c6b 84aa                       STY y
   846                          
   847  0c6d 9009                       BCC li_x_different
   848                          			; C=0 always (from negation before)
   849                          
   850                          li_xend_right
   851  0c6f a5ab                       LDA dxl		; dx = 0?
   852  0c71 05a7                       ORA dxh
   853  0c73 d003                       BNE li_x_different
   854  0c75 4cd30b                     JMP vline_start	; vertical line case
   855                          
   856                          li_x_different
   857  0c78 38                         SEC		; calculate dy
   858  0c79 a593                       LDA yend
   859  0c7b e5aa                       SBC y
   860  0c7d b006                       BCS li_y_right
   861  0c7f 49ff                       EOR #$FF	; negate dy (two's complement)
   862  0c81 6901                       ADC #$01	; C=0
   863  0c83 85a8                       STA ydir	; flag y goes up
   864                          
   865                          li_y_right
   866  0c85 85a9                       STA dy
   867  0c87 d003                       BNE +
   868  0c89 4c380b                     JMP hline_start	; horizontal line case
   869                          +
   870                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   871                          
   872  0c8c a5a7                       LDA dxh		; dx > dy
   873  0c8e d017                       BNE line_flat	; yes -> flat
   874  0c90 a5a9                       LDA dy		; no -> steep
   875  0c92 aa                         TAX
   876  0c93 c5ab                       CMP dxl
   877  0c95 9010                       BCC line_flat
   878                          
   879                          line_steep
   880  0c97 e8                         INX	
   881  0c98 86a3                       STX cl		; c = dy+1
   882  0c9a 4a                         LSR		; k = dy/2
   883  0c9b 8595                       STA kl
   884  0c9d a5a8                       LDA ydir
   885  0c9f d003                       BNE +
   886  0ca1 4c910a                     JMP line_down_steep	; y down, steep
   887  0ca4 4ca509             +	JMP line_up_steep	; y up, steep
   888                          
   889                          line_flat
   890  0ca7 a5a7                       LDA dxh
   891  0ca9 a8                         TAY
   892  0caa a6ab                       LDX dxl
   893  0cac e8                         INX
   894  0cad d001                       BNE +
   895  0caf c8                         INY
   896  0cb0 86a3               +	STX cl		; c = dx+1
   897  0cb2 84a4                       STY ch
   898                          
   899  0cb4 4a                         LSR		; k = dx/2
   900  0cb5 8596                       STA kh
   901  0cb7 a5ab                       LDA dxl
   902  0cb9 6a                         ROR		; dx/2
   903  0cba 8595                       STA kl
   904  0cbc a5a8                       LDA ydir	
   905  0cbe d003                       BNE +
   906  0cc0 4c3b0a                     JMP line_down_flat	; y down, flat
   907  0cc3 4ce609             +	JMP line_up_flat	; y up, flat
   908                          
   909                          ;-----------------------------------------------------------------
   910                          
   911                          plot
   912  0cc6 20d60a                     JSR getxy	; get parameter
   913  0cc9 859c                       STA xh		; save x/y
   914  0ccb 849b                       STY xl
   915  0ccd 86aa                       STX y
   916  0ccf 8ddc08                     STA savexh	; and store as cursor
   917  0cd2 8cdb08                     STY savexl
   918  0cd5 8edd08                     STX savey
   919                          
   920                          plot_start
   921  0cd8 208009                     JSR position	; calculate graphical address
   922                          
   923  0cdb a501                       LDA prozport
   924  0cdd 29fd                       AND #%11111101	; Kernal ROM disable
   925  0cdf 78                         SEI			
   926  0ce0 8501                       STA prozport
   927                          
   928  0ce2 207109                     JSR gchange	; change graphical data
   929                          
   930  0ce5 a501                       LDA prozport
   931  0ce7 0902                       ORA #%00000010	; kernal ROM enable
   932  0ce9 8501                       STA prozport
   933  0ceb 58                         CLI
   934  0cec 60                         RTS
   935                          
   936                          ;-----------------------------------------------------------------
   937                          
   938                          move
   939  0ced 20d60a                     JSR getxy	; get parameter
   940  0cf0 8ddc08                     STA savexh	; just save as cursor
   941  0cf3 8cdb08                     STY savexl
   942  0cf6 8edd08                     STX savey
   943  0cf9 60                         RTS
   944                          
   945                          
   946                          ;-----------------------------------------------------------------
   947                          
   948                          setmode
   949  0cfa 209eb7                     JSR b_get8bit
   950  0cfd e003                       CPX #$03
   951  0cff 9003                       BCC +
   952  0d01 4c48b2                     JMP b_illquant
   953  0d04 e001               +	CPX #$01
   954  0d06 b01a                       BCS set_or_toggle
   955                          
   956                          modereset
   957  0d08 a908                       LDA #>(nbitmask)
   958  0d0a 8d7509                     STA gchange_op+2
   959  0d0d a9b6                       LDA #<(nbitmask)
   960  0d0f 8d7409                     STA gchange_op+1
   961  0d12 a93d                       LDA #$3D		; AND abs,X
   962  0d14 8d7309                     STA gchange_op
   963  0d17 a931                       LDA #$31		; AND (zp),Y
   964  0d19 8d7b09                     STA gmask_op
   965  0d1c a9ff                       LDA #$FF		; EOR $#FF, invertieren
   966  0d1e 8d7a09                     STA gmask_flip+1
   967  0d21 60                         RTS
   968                          
   969                          set_or_toggle
   970  0d22 d01a                       BNE modetoggle
   971                          modeset
   972  0d24 a908                       LDA #>(bitmask)
   973  0d26 8d7509                     STA gchange_op+2
   974  0d29 a9ae                       LDA #<(bitmask)
   975  0d2b 8d7409                     STA gchange_op+1
   976  0d2e a91d                       LDA #$1D		; OR abs,X
   977  0d30 8d7309                     STA gchange_op
   978  0d33 a911                       LDA #$11		; OR (zp),Y
   979  0d35 8d7b09                     STA gmask_op
   980  0d38 a900                       LDA #$00		; EOR #$00, nicht invertieren
   981  0d3a 8d7a09                     STA gmask_flip+1
   982  0d3d 60                         RTS
   983                          
   984                          modetoggle
   985  0d3e a908                       LDA #>(bitmask)
   986  0d40 8d7509                     STA gchange_op+2
   987  0d43 a9ae                       LDA #<(bitmask)
   988  0d45 8d7409                     STA gchange_op+1
   989  0d48 a95d                       LDA #$5D		; EOR abs,X
   990  0d4a 8d7309                     STA gchange_op
   991  0d4d a951                       LDA #$51		; EOR (zp),Y
   992  0d4f 8d7b09                     STA gmask_op
   993  0d52 a900                       LDA #$00		; EOR #$00, nicht invertieren
   994  0d54 8d7a09                     STA gmask_flip+1
   995  0d57 60                         RTS
   996                          
   997                          
   998                          ;-----------------------------------------------------------------
   999                          
  1000                          ; get pixel (check if pixel set)
  1001                          ; not used
  1002                          
  1003                          get
  1004  0d58 20d30a                     JSR getcommaxy
  1005  0d5b 859c                       STA xh
  1006  0d5d 849b                       STY xl
  1007  0d5f 86aa                       STX y
  1008                          
  1009  0d61 208009                     JSR position
  1010                          
  1011  0d64 a501                       LDA prozport
  1012  0d66 29fd               	AND #%11111101	; Kernal ROM disable
  1013  0d68 78                         SEI
  1014  0d69 8501                       STA prozport
  1015                          
  1016  0d6b b1a5                       LDA (gaddr),Y
  1017  0d6d 3dae08                     AND bitmask,X
  1018  0d70 a8                         TAY
  1019  0d71 a501                       LDA prozport
  1020  0d73 0902               	ORA #%00000010	; kernal ROM enable
  1021  0d75 8501                       STA prozport
  1022  0d77 58                         CLI
  1023  0d78 4ca2b3                     JMP b_byte2fac
  1024                          
  1025                          
  1026                          ;-----------------------------------------------------------------
  1027                          
  1028                          relto
  1029  0d7b 208aad                     JSR b_getval	; get X offset (+/-)
  1030  0d7e a561               	LDA facexp	; FAC exponent
  1031  0d80 c990               	CMP #$90	; more than 16 bit
  1032  0d82 b031               	BCS relto_error	; illegal quantity
  1033  0d84 209bbc                     JSR b_fac2int	; to signed integer
  1034                          
  1035  0d87 18                         CLC
  1036  0d88 a565                       LDA facintl
  1037  0d8a 6ddb08                     ADC savexl
  1038  0d8d 859e                       STA xendl
  1039  0d8f a564                       LDA facinth
  1040  0d91 6ddc08                     ADC savexh
  1041  0d94 859f                       STA xendh	; xend = savex+facint
  1042                          
  1043  0d96 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1044  0d99 208aad                     JSR b_getval
  1045  0d9c a561                       LDA facexp	; FAC exponent
  1046  0d9e c990                       CMP #$90	; more than 16 bit
  1047  0da0 b013                       BCS relto_error	; illegal quantity
  1048  0da2 209bbc                     JSR b_fac2int	; to signed integer
  1049  0da5 18                         CLC
  1050  0da6 a565                       LDA facintl
  1051  0da8 6ddd08                     ADC savey
  1052  0dab 8593                       STA yend	; yend = savey+facint
  1053                          
  1054  0dad a59f                       LDA xendh	; check end coord. x
  1055  0daf c901                       CMP #>xmax
  1056  0db1 900b                       BCC rt_xok
  1057  0db3 f003                       BEQ +
  1058                          relto_error
  1059                          !ifdef no_error {
  1060                          	RTS
  1061                          } else {
  1062  0db5 4c48b2                     JMP b_illquant
  1063                          }
  1064  0db8 a59e               +	LDA xendl
  1065  0dba c940                       CMP #<xmax
  1066  0dbc b0f7                       BCS relto_error
  1067                          rt_xok
  1068  0dbe a593                       LDA yend	; check end coord. y
  1069  0dc0 c9c8                       CMP #ymax
  1070  0dc2 b0f1                       BCS relto_error
  1071                          
  1072  0dc4 addb08                     LDA savexl
  1073  0dc7 859b                       STA xl
  1074  0dc9 addc08                     LDA savexh
  1075  0dcc 859c                       STA xh
  1076  0dce addd08                     LDA savey
  1077  0dd1 85aa                       STA y
  1078  0dd3 a49e                       LDY xendl
  1079  0dd5 a59f                       LDA xendh
  1080  0dd7 a693                       LDX yend	; xend/yend = cursor + x/y
  1081                          
  1082  0dd9 4c210c                     JMP line_start	; draw line x/y to xend/yend
  1083                          
  1084                          
  1085                          ;-----------------------------------------------------------------
  1086                          
  1087                          char
  1088  0ddc 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1089  0ddf e028                       CPX #40	
  1090  0de1 9003                       BCC +
  1091                          char_error
  1092  0de3 4c48b2                     JMP b_illquant
  1093  0de6 86fb               +	STX gpos	; save x coord.
  1094  0de8 20f1b7                     JSR b_getcomma8bit
  1095                          			; get char. position y 0-24
  1096  0deb e019                       CPX #25
  1097  0ded b0f4                       BCS char_error
  1098  0def 86fc                       STX gpos+1	; save y coord.
  1099                          
  1100  0df1 20fdae                     JSR b_getcomma	; get string
  1101  0df4 209ead                     JSR b_getexpr
  1102  0df7 20a3b6                     JSR b_stringval ; string address in str
  1103  0dfa 48                         PHA		; string length
  1104  0dfb a6fc                       LDX gpos+1	; y coord. for char. position
  1105  0dfd 8a                         TXA
  1106  0dfe 2903                       AND #$03	; mask 2 bits
  1107  0e00 a8                         TAY		; table index
  1108  0e01 a900                       LDA #$00
  1109  0e03 85fc                       STA gpos+1	; x high
  1110  0e05 a5fb                       LDA gpos	; saved x: multiply by 8
  1111  0e07 0a                         ASL
  1112  0e08 0a                         ASL
  1113  0e09 0a                         ASL
  1114  0e0a 26fc                       ROL gpos+1	; overflow to high byte
  1115  0e0c 79be08                     ADC ytabl,Y
  1116  0e0f 85a5                       STA gaddr
  1117  0e11 a5fc                       LDA gpos+1	; x high
  1118  0e13 7dc208                     ADC ytabh,X
  1119  0e16 85a6                       STA gaddr+1
  1120  0e18 68                         PLA		; string length
  1121  0e19 a000                       LDY #$00	; string index
  1122  0e1b aa                         TAX		; length
  1123  0e1c e8                         INX		; prepare as counter
  1124                          char_loop
  1125  0e1d ca                         DEX
  1126  0e1e f008                       BEQ char_exit
  1127  0e20 b122                       LDA (str),Y	; read string
  1128  0e22 20290e                     JSR char_display
  1129  0e25 c8                         INY
  1130  0e26 d0f5                       BNE char_loop
  1131                          char_exit
  1132  0e28 60                         RTS
  1133                          
  1134                          char_display
  1135  0e29 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1136  0e2b 8a                         TXA		; save register X+Y
  1137  0e2c 48                         PHA
  1138  0e2d 98                         TYA
  1139  0e2e 48                         PHA
  1140  0e2f a5d7                       LDA z_tmp	; get saved character
  1141  0e31 1049                       BPL char_normal
  1142                          
  1143                          char_inverse
  1144  0e33 297f                       AND #%01111111	; mask bit 7
  1145  0e35 c97f                       CMP #%01111111	; was 255? (pi)
  1146  0e37 d002                       BNE +
  1147  0e39 a95e                       LDA #$5E	; screen code for pi
  1148  0e3b c920               +	CMP #$20	; control character?
  1149  0e3d 9038                       BCC char_disp_leave
  1150                          			; yes, skip
  1151  0e3f 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1152                          			; $C0-$FF -> $40-$7F
  1153                          			; OPT: BNE char_hires
  1154                          			; OPT: char_normal
  1155                          char_hires
  1156  0e41 a6c7                       LDX z_reverseflag
  1157  0e43 f002                       BEQ +
  1158  0e45 0980                       ORA #%10000000	; invert char.
  1159  0e47 48                 +	PHA		; save char. for later
  1160  0e48 78                         SEI
  1161  0e49 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1162  0e4b 8501                       STA prozport	; char. rom base = $D000
  1163  0e4d a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1164  0e4f 85fc                       STA gpos+1	; 
  1165  0e51 68                         PLA		; char. code
  1166  0e52 0a                         ASL		; *8
  1167  0e53 26fc                       ROL gpos+1
  1168  0e55 0a                         ASL
  1169  0e56 26fc                       ROL gpos+1
  1170  0e58 0a                         ASL
  1171  0e59 26fc                       ROL gpos+1
  1172  0e5b 85fb                       STA gpos	; addr. in char. rom for char.
  1173                          
  1174  0e5d a007                       LDY #$07	; 8 hires lines
  1175                          char_line
  1176  0e5f b1fb                       LDA (gpos),Y	; read character line
  1177  0e61 207909                     JSR gmask	; write to hires screen
  1178  0e64 88                         DEY
  1179  0e65 10f8                       BPL char_line
  1180                          
  1181  0e67 a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1182  0e69 8501                       STA prozport
  1183  0e6b 58                         CLI
  1184                          
  1185  0e6c 18                         CLC		; step char position to left
  1186  0e6d a5a5                       LDA gaddr	; ( +8 )
  1187  0e6f 6908                       ADC #$08
  1188  0e71 85a5                       STA gaddr
  1189  0e73 9002                       BCC +
  1190  0e75 e6a6                       INC gaddr+1
  1191                          +
  1192                          char_disp_leave
  1193  0e77 68                 	PLA		; pass written character back
  1194  0e78 a8                         TAY		; restore saved registers
  1195  0e79 68                         PLA
  1196  0e7a aa                         TAX
  1197  0e7b 60                         RTS
  1198                          
  1199                          char_normal
  1200  0e7c c920                       CMP #$20	; control character?
  1201  0e7e 90f7                       BCC char_disp_leave
  1202  0e80 c960                       CMP #$60
  1203  0e82 9004                       BCC +
  1204  0e84 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1205  0e86 d002                       BNE ++
  1206  0e88 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1207  0e8a 4c410e             ++	JMP char_hires	; 		OPT: Bxx
  1208                          
  1209                          
  1210                          ;-----------------------------------------------------------------
  1211                          
  1212                          to
  1213  0e8d addb08                     LDA savexl
  1214  0e90 859b                       STA xl
  1215  0e92 addc08                     LDA savexh
  1216  0e95 859c                       STA xh
  1217  0e97 addd08                     LDA savey
  1218  0e9a 85aa                       STA y
  1219  0e9c 20d60a                     JSR getxy
  1220  0e9f 4c210c                     JMP line_start
  1221                          
  1222                          graext_end
  1223                          

; ******** Source: ge-run.asm
    43                          
    44                          
