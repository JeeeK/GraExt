
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
    29  080d a26a               	ldx #<graext_end	; setup basic
    30  080f a00f               	ldy #>graext_end
    31  0811 18                 	clc			; set if C=0
    32  0812 209cff             	jsr $ff9c		; KERNAL: system RAM bottom
    33  0815 862b               	stx $2b			; BASIC text start
    34  0817 842c               	sty $2c
    35  0819 2016e4             	jsr $e416		; setup BASIC text start
    36  081c 202908             	jsr init		; init extension (place hook)
    37  081f a9d7               	lda #<author		; message ...
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
     6                          ;	2016-05-29 v 1.23
     7                          ;	2016-05-20 v 1.22
     8                          ;	2016-05-16 v 1.21
     9                          ;	2016-02-23 v 1.20
    10                          ;	2016-01-15 v 1.19
    11                          ;	1992-12-28 v 1.18
    12                          ;	1986-03-24 v 1.17
    13                          ;	1985       v 0.00 - 1.16
    14                          ;
    15                          ; the original source has been lost.
    16                          ; development has based on the implemention
    17                          ; done on a forth-64 written with its forth assembler.
    18                          ; the code has been pulled out from there and enriched
    19                          ; with some glue code to get a basic extension.
    20                          
    21                          ; command dispatcher style JMP/RTS
    22                          ;	(if defined)
    23                          ;command_rts_style=1
    24                          
    25                          ; error handling 
    26                          ;	(if defined)
    27                          ;no_error=1
    28                          
    29                          ; basic interpreter registers, addresses and entry points
    30                          
    31                          str     = $22		; string address
    32                          bassta	= $2b		; basic start pointer
    33                          basend	= $2d		; basic end pointer
    34                          ijmp    = $55		; address of JMP (addr)
    35                          chrget  = $73		; basic charget routine
    36                          facintl = $65		; integer result from b_fac2int
    37                          facinth = $64
    38                          facexp  = $61		; fac exponent, after b_getval
    39                          
    40                          z_reverseflag = $C7	; character routine
    41                          z_lastkey = $D7		; original use case, unused here
    42                          z_tmp = z_lastkey	; temporary reused for character routine
    43                          
    44                          v_baserr = $0300	; vector error routine
    45                          v_basstp = $0328	; vector error routine
    46                          v_bascmd = $0308	; vector interpreter parsing
    47                          v_basexp = $030a	; vector evaluate expression
    48                          
    49                          basic_rom = $A000	; start of BASIC ROM
    50                          
    51                          b_clr = $A660		; CLR command
    52                          b_interpreter = $A7AE	; interpreter loop
    53                          b_execstatement = $A7E7	; process statement
    54                          b_getcomma = $AEFD	; read comma from basic text
    55                          b_illquant = $B248	; error "illegal quantity"
    56                          b_syntaxerror = $AF08	; error "syntax"
    57                          b_get8bit = $B79E	; read 8 bit numeric value from
    58                          			; basic text
    59                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    60                          			; from basic text
    61                          b_getval = $AD8A	; read numeric value from basic text
    62                          b_getexpr = $AD9E	; read expression from basic text
    63                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    64                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    65                          b_fac2int = $BC9B	; convert FAC to integer
    66                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    67                          b_rechain = $A533	; rechain basic lines
    68                          
    69                          ; hardware registers and values
    70                          
    71                          prozport = $01		; processor port
    72                          memrom = %00110111	; basic+kernal rom
    73                          membas = %00110110	; basic ram+kernal rom
    74                          memram = %00110101	; basic+kernal ram
    75                          
    76                          vic_cr	= $D011		; VIC control register
    77                          vic_mcr	= $D018		; VIC memory control register
    78                          cia_pra	= $DD00		; CIA 2 port register A
    79                          
    80                          cram	= $CC00		; start of color ram
    81                          
    82                          gram	= $e000		; start of graphic bitmap ram
    83                          gramp	= gram >> 8	; start page of bitmap
    84                          
    85                          ; constants 
    86                          
    87                          xmax	= 320		; max x dimension
    88                          ymax	= 200		; max y dimension
    89                          
    90                          ; zeropage variables
    91                          
    92                          x	= $9B		; start coordinate x, low+high
    93                          xl	= x
    94                          xh	= x+1
    95                          y	= $AA		; start coordinate y
    96                          
    97                          xendl	= $9E		; end coordinate x, low+high
    98                          xendh	= $9F
    99                          yend	= $93		; end coordinate y
   100                          
   101                          kl	= $95		; gradient for lines, low+high
   102                          kh	= kl+1
   103                          
   104                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
   105                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
   106                          
   107                          dxl	= $AB		; x delta, low+high
   108                          dxh	= $A7
   109                          
   110                          dy	= $A9		; y delta
   111                          
   112                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   113                          
   114                          cl	= $A3		; dot count, low+high
   115                          ch	= $A4
   116                          
   117                          gaddr	= $A5		; graphic address
   118                          
   119                          gpos	= $FB		; in graphic position
   120                          
   121                          gcol	= $FD		; graphic color, in "graphic on" context only
   122                          
   123                          
   124                          ; static ram areas
   125                          
   126                          savexl	= $0334		; the graphic cursor: x low 
   127                          savexh	= savexl+1	; the graphic cursor: x high
   128                          savey	= savexh+1	; the graphic cursor: y
   129                          savemo	= savey+1	; the graphic mode
   130                          saveverr = savemo+1	; original v_baserr
   131                          savevstp = saveverr+2	; original v_basstp
   132                          
   133                          gramcode = $03ed	; real place for gchange and gmask routines,
   134                          			; they take 15 bytes
   135                          
   136                          ;
   137                          ; initialize extension
   138                          
   139                          init
   140  0829 a981                       LDA #<(parse)	; basic interpreter parser hook
   141  082b 8d0803                     STA v_bascmd
   142  082e a908                       LDA #>(parse)
   143  0830 8d0903                     STA v_bascmd+1
   144                          
   145  0833 ad2803                     LDA v_basstp
   146  0836 8d3a03             	STA savevstp
   147  0839 a975                       LDA #<(stop)	; basic interpreter stop hook
   148  083b 8d2803                     STA v_basstp
   149  083e ad2903                     LDA v_basstp+1
   150  0841 8d3b03             	STA savevstp+1
   151  0844 a908                       LDA #>(stop)
   152  0846 8d2903                     STA v_basstp+1
   153                          
   154  0849 ad0003                     LDA v_baserr
   155  084c 8d3803             	STA saveverr
   156  084f a96f                       LDA #<(error)	; basic interpreter error hook
   157  0851 8d0003                     STA v_baserr
   158  0854 ad0103                     LDA v_baserr+1
   159  0857 8d3903             	STA saveverr+1
   160  085a a908                       LDA #>(error)
   161  085c 8d0103                     STA v_baserr+1
   162                          
   163  085f a200               	LDX #0		; set graphic cursor to (0,0)
   164  0861 8e3403             	STX savexl
   165  0864 8e3503             	STX savexh
   166  0867 8e3603             	STX savey
   167  086a e8                 	INX
   168  086b 8e3703             	STX savemo	; set mode 1
   169  086e 60                         RTS
   170                          
   171                          error	
   172                          	; reg A may destroyed
   173  086f 204509             	JSR gra_off		; uses only reg A
   174  0872 6c3803             	JMP (saveverr)		; to original vector
   175                          
   176                          stop	
   177                          	; reg A may destroyed
   178  0875 a591               	LDA $91			; Scan code
   179  0877 c97f               	CMP #$7F		; STOP key?
   180  0879 d003               	BNE nostop
   181  087b 204509             	JSR gra_off		; uses only reg A
   182                          nostop
   183  087e 6c3a03             	JMP (savevstp)		; to original vector
   184                          
   185                          ;-----------------------------------------------------------------
   186                          
   187                          ; start parsing an extension command ...
   188                          
   189                          parse
   190  0881 207300                     JSR chrget			; next char.
   191  0884 08                 	PHP
   192  0885 c926                       CMP #'&'			; command prefix
   193  0887 f004                       BEQ newcmd
   194  0889 28                         PLP
   195  088a 4ce7a7                     JMP b_execstatement
   196                          newcmd
   197  088d 28                 	PLP
   198  088e 207300                     JSR chrget			; command character
   199                          
   200  0891 a00c                       LDY #(cmdsend-cmds)		; map character to
   201                          					; command address ...
   202                          checknextcmd
   203  0893 88                         DEY
   204  0894 f01c               	BEQ parse_error
   205  0896 d9b508                     CMP cmds,Y
   206  0899 d0f8                       BNE checknextcmd		; try next
   207  089b 88                         DEY				; found
   208  089c 98                         TYA
   209  089d 0a                         ASL				; *2
   210  089e a8                         TAY
   211                          !ifndef command_rts_tyle {
   212                          	!set co=0			; command offset in jump table
   213  089f b9c208                     LDA cmdaddr+1,Y                 ; high byte from table
   214  08a2 8556                       STA ijmp+1
   215  08a4 b9c108                     LDA cmdaddr,Y                   ; low byte from table
   216  08a7 8555                       STA ijmp
   217  08a9 207300                     JSR chrget			; read next byte in basic text
   218  08ac 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   219  08af 4caea7                     JMP b_interpreter		; continue parsing
   220                          } else {
   221                          	!set co=1			; command offset in jump table
   222                          	LDA #>(b_interpreter-1)		; return to interpreter
   223                          	PHA
   224                          	LDA #<(b_interpreter-1)
   225                          	PHA				
   226                                  LDA cmdaddr+1,Y			; command address (RTS style)
   227                                  PHA				; high byte on stack
   228                                  LDA cmdaddr,Y			; command address (RTS style)
   229                                  PHA				; low byte on stack
   230                                  JMP chrget			; read next byte in basic text 
   231                          					; and RTS to command
   232                          }
   233                          parse_error
   234  08b2 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   235                          
   236                          ;-----------------------------------------------------------------
   237                          
   238                          ; the most commonly used command placed at the end ...
   239                          
   240  08b5 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   241                          cmdsend
   242                          
   243                          cmdaddr
   244  08c1 490f3e09830e860d...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   245  08cd 340f230c720b8e0c...        !word to-co,vline-co,hline-co,line-co,plot-co
   246                          
   247  08d7 934752412d455854...author	!text 147,"GRA-EXT V1.23 1986,2016 JOHANN@KLASEK.AT",0
   248                          
   249                          bitmask
   250  0901 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   251                          nbitmask
   252  0909 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   253                          ytabl
   254  0911 004080c0           	!byte $00,$40,$80,$c0
   255                          ytabh
   256  0915 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   257  0919 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   258  091d eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   259  0921 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   260  0925 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   261  0929 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   262  092d fe                 	!byte gramp+$1e
   263                          
   264                          ; for horiz. line
   265                          
   266  092e ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   267                          
   268  0936 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   269                          
   270                          
   271                          ;-----------------------------------------------------------------
   272                          
   273                          graphic
   274  093e 209eb7                     JSR b_get8bit
   275  0941 e000                       CPX #$00
   276  0943 d013                       BNE gra_other
   277                          gra0			; &G 0
   278                          gra_off
   279  0945 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   280  0947 8d00dd                     STA cia_pra
   281  094a a915                       LDA #((1 <<4) + (2 <<1) + 1)
   282                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   283                          			; char addr $1000/4096 = char. ROM
   284  094c 8d18d0                     STA vic_mcr	; VIC memory control
   285  094f ad11d0                     LDA vic_cr	; VIC control register
   286  0952 29df                       AND #%11011111	; Hires mode off
   287  0954 8d11d0                     STA vic_cr
   288  0957 60                         RTS
   289                          
   290                          gra_other
   291  0958 e001                       CPX #$01
   292  095a f00f               	BEQ gra1
   293  095c e002               	CPX #$02
   294  095e f00e                       BEQ gra2
   295  0960 e004               	CPX #$04
   296  0962 f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   297  0964 e003               	CPX #$03	; &G 3 (graphic on)
   298  0966 f029               	BEQ gra_on
   299  0968 4c48b2                     JMP b_illquant	; parameter illegal
   300                          	
   301                          gra1			; &G 1
   302  096b 20a709             	JSR gra_clear
   303                          
   304                          gra2
   305  096e 20f1b7                     JSR b_getcomma8bit
   306  0971 8a                         TXA		; foreground color
   307  0972 0a                         ASL		; upper nibble
   308  0973 0a                         ASL
   309  0974 0a                         ASL
   310  0975 0a                         ASL
   311  0976 85fd                       STA gcol
   312  0978 20f1b7                     JSR b_getcomma8bit
   313  097b 8a                         TXA		; background color
   314  097c 290f                       AND #$0F
   315  097e 05fd                       ORA gcol
   316  0980 a000                       LDY #$00
   317                          cram_loop
   318  0982 9900cc                     STA cram,Y	; fill color RAM
   319  0985 9900cd                     STA cram+$100,Y
   320  0988 9900ce                     STA cram+$200,Y
   321  098b 99e8ce                     STA cram+$300-24,Y
   322  098e c8                         INY
   323  098f d0f1                       BNE cram_loop
   324                          
   325                          gra_on
   326  0991 20c609             	JSR gra_setupcode
   327                          
   328  0994 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   329  0996 8d00dd                     STA cia_pra
   330  0999 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   331  099b 8d18d0                     STA vic_mcr	; VIC memory control
   332  099e ad11d0                     LDA vic_cr	; VIC control register
   333  09a1 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   334  09a3 8d11d0                     STA vic_cr
   335  09a6 60                         RTS
   336                          
   337                          gra_clear
   338  09a7 a220                       LDX #$20	; Pages (8 KByte)
   339  09a9 a9e0                       LDA #>gram
   340  09ab 85fc                       STA gpos+1
   341  09ad a000                       LDY #$00
   342  09af 84fb                       STY gpos
   343  09b1 98                         TYA
   344                          gra_fill
   345  09b2 91fb                       STA (gpos),Y	; Loop unroll
   346  09b4 c8                         INY
   347  09b5 91fb                       STA (gpos),Y
   348  09b7 c8                         INY
   349  09b8 91fb                       STA (gpos),Y
   350  09ba c8                         INY
   351  09bb 91fb                       STA (gpos),Y
   352  09bd c8                         INY
   353  09be d0f2                       BNE gra_fill
   354  09c0 e6fc                       INC gpos+1
   355  09c2 ca                         DEX
   356  09c3 d0ed                       BNE gra_fill
   357  09c5 60                 	RTS
   358                          
   359                          gra_setupcode
   360  09c6 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   361                          gra_copycode
   362  09c8 bde909             	LDA gromcode-1,X
   363  09cb 9dec03             	STA gramcode-1,X
   364  09ce ca                 	DEX
   365  09cf d0f7               	BNE gra_copycode
   366  09d1 ad3703             	LDA savemo
   367  09d4 290f               	AND #$0F
   368  09d6 aa                 	TAX
   369  09d7 4cab0d             	JMP setmode_enter	; re-apply mode to routines
   370                          				; implicit RTS
   371                          
   372                          ;-----------------------------------------------------------------
   373                          
   374                          gexit
   375  09da a501                       LDA prozport
   376  09dc 0902                       ORA #%00000010	; kernal ROM enable
   377  09de 8501                       STA prozport
   378  09e0 58                         CLI		; allow interrupts
   379  09e1 60                         RTS
   380                          
   381                          ;-----------------------------------------------------------------
   382                          
   383                          ginit
   384  09e2 a501                       LDA prozport
   385  09e4 29fd                       AND #%11111101	; Kernal ROM disable
   386  09e6 78                         SEI		; disable interrupts
   387  09e7 8501                       STA prozport
   388  09e9 60                         RTS
   389                          
   390                          ;-----------------------------------------------------------------
   391                          
   392                          ; These are selfmodified routines, which has to placed into RAM
   393                          ; (on every graphic "on")
   394                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   395                          
   396                          gromcode
   397                          
   398                          !pseudopc gramcode {
   399                          
   400                          ; change a graphic location
   401                          
   402                          gchange
   403  09ea b1a5                       LDA (gaddr),Y
   404                          gchange_op
   405  09ec 1d0109                     ORA bitmask,X
   406  09ef 91a5                       STA (gaddr),Y
   407  09f1 60                         RTS
   408                          
   409                          ; mask a graphic location 
   410                          
   411                          gmask
   412                          gmask_flip
   413  09f2 4900                       EOR #$00
   414                          gmask_op
   415  09f4 11a5                       ORA (gaddr),Y
   416  09f6 91a5                       STA (gaddr),Y
   417  09f8 60                         RTS
   418                          
   419                          }
   420                          
   421                          gromcode_end
   422                          
   423                          ;-----------------------------------------------------------------
   424                          
   425                          position
   426  09f9 a5aa                       LDA y
   427  09fb 4a                         LSR
   428  09fc 4a                         LSR
   429  09fd 4a                         LSR		; y/8
   430  09fe a8                         TAY
   431  09ff 2903                       AND #%00000011	; (y/8) mod 4
   432  0a01 aa                         TAX
   433  0a02 a59b                       LDA xl		; x low
   434  0a04 29f8                       AND #%11111000	; clear bit 2-0
   435  0a06 18                         CLC
   436  0a07 7d1109                     ADC ytabl,X	; addr low: y base + x part
   437  0a0a 85a5                       STA gaddr
   438  0a0c a59c                       LDA xh		; addr high: x part
   439  0a0e 791509                     ADC ytabh,Y	; 	+ y base
   440  0a11 85a6                       STA gaddr+1
   441  0a13 a5aa                       LDA y		; vertical offset
   442  0a15 2907                       AND #%00000111	; y mod 8
   443  0a17 a8                         TAY
   444  0a18 a59b                       LDA xl
   445  0a1a 2907                       AND #%00000111	; x mod 8
   446  0a1c aa                         TAX		; horizonal offset
   447  0a1d 60                         RTS		; (bitmask)
   448                          
   449                          
   450                          ;-----------------------------------------------------------------
   451                          
   452                          ; line y up, x right, dx < dy (case 1)
   453                          
   454                          line_up_steep
   455  0a1e 20f909                     JSR position	; x,y
   456                          loop_yup_xright
   457  0a21 20ed03                     JSR gchange	; pixel
   458                          
   459  0a24 18                         CLC		; k += dx
   460  0a25 a595                       LDA kl
   461  0a27 65ab                       ADC dxl		; dxh is 0, because dx < dy
   462  0a29 8595                       STA kl
   463  0a2b b004                       BCS ++		; k > 255
   464                          
   465  0a2d c5a9                       CMP dy
   466  0a2f 9015                       BCC +		; k >= dy ->
   467                          
   468  0a31 e5a9               ++	SBC dy		; k -= dy
   469  0a33 8595                       STA kl
   470                          
   471  0a35 e8                         INX		; x++
   472  0a36 e008                       CPX #8
   473  0a38 d00c                       BNE +
   474                          	; C=1
   475  0a3a a200                       LDX #0		; x overflow, wrap around
   476  0a3c a5a5                       LDA gaddr	; x+8: gaddr += 8
   477  0a3e 6907                       ADC #8-1	; C already set by CPX
   478  0a40 85a5                       STA gaddr
   479  0a42 9002                       BCC +
   480  0a44 e6a6                       INC gaddr+1
   481                          
   482  0a46 88                 +	DEY		; y--
   483  0a47 100f                       BPL +++
   484  0a49 38                         SEC		; y overflow
   485  0a4a a5a5                       LDA gaddr
   486  0a4c e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   487  0a4e 85a5                       STA gaddr
   488  0a50 a5a6                       LDA gaddr+1
   489  0a52 e901               	SBC #1
   490  0a54 85a6                       STA gaddr+1
   491  0a56 a007                       LDY #7		; wrap around
   492                          
   493  0a58 c6a3               +++	DEC cl		; until c=0
   494  0a5a d0c5                       BNE loop_yup_xright
   495  0a5c 4cda09                     JMP gexit
   496                          
   497                          
   498                          ;-----------------------------------------------------------------
   499                          
   500                          ; line x right, y up, dx > dy (case 2)
   501                          
   502                          line_up_flat
   503  0a5f 20f909                     JSR position	; x,y
   504  0a62 a5a3               	LDA cl		; counter adjustment for
   505  0a64 f002               	BEQ +		; dec-dec-counting
   506  0a66 e6a4               	INC ch
   507                          +
   508                          loop_xright_yup
   509  0a68 20ed03                     JSR gchange	; pixel
   510                          
   511  0a6b 18                         CLC		; k += dy
   512  0a6c a595                       LDA kl
   513  0a6e 65a9                       ADC dy
   514  0a70 8595                       STA kl
   515  0a72 9002                       BCC ++
   516  0a74 e696                       INC kh
   517                          
   518  0a76 c5ab               ++	CMP dxl		; k > dx?
   519  0a78 a596                       LDA kh
   520  0a7a e5a7                       SBC dxh
   521  0a7c 901a                       BCC +
   522                          
   523  0a7e 8596                       STA kh		; k -= dx
   524  0a80 a595                       LDA kl
   525  0a82 e5ab                       SBC dxl
   526  0a84 8595                       STA kl
   527                          
   528  0a86 88                         DEY		; y--
   529  0a87 100f                       BPL +
   530  0a89 38                 	SEC		; C=1 not always true (SBC above)
   531  0a8a a5a5                       LDA gaddr	; y overflow
   532  0a8c e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   533  0a8e 85a5                       STA gaddr
   534  0a90 a5a6                       LDA gaddr+1
   535  0a92 e901               	SBC #1
   536  0a94 85a6                       STA gaddr+1
   537  0a96 a007               	LDY #7		; wrap around
   538                          
   539  0a98 e8                 +	INX		; x++
   540  0a99 e008                       CPX #8		; x overflow?
   541  0a9b d00c                       BNE ++
   542                          	; C=1
   543  0a9d a200                       LDX #0		; wrap around
   544  0a9f a5a5                       LDA gaddr	; x+8: gaddr += 8
   545  0aa1 6907                       ADC #8-1	; C already set by CPX
   546  0aa3 85a5                       STA gaddr
   547  0aa5 9002                       BCC ++
   548  0aa7 e6a6                       INC gaddr+1
   549                          ++
   550  0aa9 c6a3               	DEC cl		; c--
   551  0aab d0bb                       BNE loop_xright_yup
   552  0aad c6a4                       DEC ch		; adjusted high which allows this
   553  0aaf d0b7                       BNE loop_xright_yup
   554                          
   555  0ab1 4cda09                     JMP gexit
   556                          
   557                          
   558                          
   559                          ;-----------------------------------------------------------------
   560                          
   561                          ; line x right, y down, dx > dy (case 3)
   562                          
   563                          line_down_flat
   564  0ab4 20f909                     JSR position	; x,y
   565  0ab7 a5a3               	LDA cl		; counter adjustment for
   566  0ab9 f002               	BEQ +		; dec-dec-counting
   567  0abb e6a4               	INC ch
   568                          +
   569                          loop_xright_ydown
   570  0abd 20ed03                     JSR gchange	; pixel
   571                          
   572  0ac0 18                         CLC		; k += dy
   573  0ac1 a595                       LDA kl
   574  0ac3 65a9                       ADC dy
   575  0ac5 8595                       STA kl
   576  0ac7 9002                       BCC ++
   577  0ac9 e696                       INC kh
   578                          
   579  0acb c5ab               ++	CMP dxl		; k > dx
   580  0acd a596                       LDA kh
   581  0acf e5a7                       SBC dxh		; k -= dx
   582  0ad1 901b                       BCC +
   583                          
   584  0ad3 8596                       STA kh
   585  0ad5 a595                       LDA kl
   586  0ad7 e5ab                       SBC dxl
   587  0ad9 8595                       STA kl
   588                          
   589  0adb c8                         INY		; y++
   590  0adc c008                       CPY #8
   591  0ade d00e                       BNE +
   592                          	; C=1
   593  0ae0 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   594  0ae2 693f                       ADC #$40-1	; C already set by CPY
   595  0ae4 85a5                       STA gaddr
   596  0ae6 a5a6                       LDA gaddr+1
   597  0ae8 6901               	ADC #1
   598  0aea 85a6                       STA gaddr+1
   599  0aec a000                       LDY #0		; wrap around
   600                          
   601  0aee e8                 +	INX		; x++
   602  0aef e008                       CPX #8		; x overflow ?
   603  0af1 d00c                       BNE +++
   604                          	; C=1
   605  0af3 a200                       LDX #$00	; wrap around
   606  0af5 a5a5                       LDA gaddr	; gaddr += 8
   607  0af7 6907                       ADC #$08-1	; C always set by CPX
   608  0af9 85a5                       STA gaddr
   609  0afb 9002                       BCC +++
   610  0afd e6a6                       INC gaddr+1
   611                          +++
   612  0aff c6a3               	DEC cl		; c--
   613  0b01 d0ba                       BNE loop_xright_ydown
   614  0b03 c6a4                       DEC ch		; adjusted high which allows this
   615  0b05 d0b6                       BNE loop_xright_ydown
   616                          
   617  0b07 4cda09                     JMP gexit
   618                          
   619                          
   620                          ;-----------------------------------------------------------------
   621                          
   622                          ; line y down, x right, dx < dy (case 4)
   623                          
   624                          line_down_steep
   625  0b0a 20f909                     JSR position	; x,y
   626                          loop_ydown_xright
   627  0b0d 20ed03                     JSR gchange	; pixel
   628                          
   629  0b10 18                         CLC		; k += dx
   630  0b11 a595                       LDA kl
   631  0b13 65ab                       ADC dxl		; dxh is 0, because dx < dy
   632  0b15 8595                       STA kl
   633  0b17 b004                       BCS ++
   634  0b19 c5a9                       CMP dy		; k > dy?
   635  0b1b 9015                       BCC +
   636  0b1d e5a9               ++	SBC dy		; k -= dy
   637  0b1f 8595                       STA kl
   638                          
   639  0b21 e8                         INX		; x++
   640  0b22 e008                       CPX #8
   641  0b24 d00c                       BNE +		; x overflow?
   642  0b26 a200                       LDX #0		; wrap around
   643  0b28 a5a5                       LDA gaddr	; x+9: gaddr += 8
   644  0b2a 6907                       ADC #8-1	; C already set by CPX
   645  0b2c 85a5                       STA gaddr
   646  0b2e 9002                       BCC +
   647  0b30 e6a6                       INC gaddr+1
   648                          
   649  0b32 c8                 +	INY		; y++
   650  0b33 c008                       CPY #8		; y overflow?
   651  0b35 d00e                       BNE +++
   652  0b37 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   653  0b39 693f                       ADC #$40-1	; C already set by CPY
   654  0b3b 85a5                       STA gaddr
   655  0b3d a5a6                       LDA gaddr+1
   656  0b3f 6901               	ADC #1
   657  0b41 85a6                       STA gaddr+1
   658  0b43 a000                       LDY #0		; wrap around
   659                          
   660  0b45 c6a3               +++	DEC cl		; c--
   661                          			; until c=0
   662  0b47 d0c4                       BNE loop_ydown_xright
   663  0b49 4cda09                     JMP gexit
   664                          
   665                          
   666                          ;-----------------------------------------------------------------
   667                          
   668                          getcommaxy
   669  0b4c 20fdae                     JSR b_getcomma	; check ","
   670                          getxy
   671  0b4f 208aad                     JSR b_getval	; get X coord. value
   672  0b52 20f7b7                     JSR b_convint
   673  0b55 c901                       CMP #>xmax
   674  0b57 9009               	BCC gcxy_xok
   675  0b59 f003                       BEQ +		; X = $1xx
   676                          error_iq
   677  0b5b 20730d                     JSR range_error
   678  0b5e c040               +	CPY #<xmax	; check X low
   679  0b60 b0f9                       BCS error_iq	; X to big
   680                          gcxy_xok
   681  0b62 84fb                       STY gpos	; temporary save X coord.
   682  0b64 85fc                       STA gpos+1
   683                          
   684  0b66 20f1b7                     JSR b_getcomma8bit
   685                          			; get Y coord. value
   686  0b69 e0c8                       CPX #ymax
   687  0b6b b0ee                       BCS error_iq	; Y to big
   688                          
   689  0b6d a4fb                       LDY gpos	; restory X coord.
   690  0b6f a5fc                       LDA gpos+1
   691  0b71 60                         RTS
   692                          
   693                          
   694                          ;-----------------------------------------------------------------
   695                          
   696                          hline
   697  0b72 204f0b                     JSR getxy	; get startpoint
   698  0b75 86aa                       STX y
   699  0b77 8e3603                     STX savey	; save as cursor, too
   700  0b7a 859c                       STA xh
   701  0b7c 849b                       STY xl
   702  0b7e 20fdae                     JSR b_getcomma	; get length
   703  0b81 208aad                     JSR b_getval
   704  0b84 20f7b7                     JSR b_convint
   705                          
   706  0b87 c901                       CMP #>xmax
   707  0b89 9006                       BCC +		; X < 256
   708  0b8b d0ce                       BNE error_iq
   709  0b8d c040                       CPY #<xmax
   710  0b8f b0ca                       BCS error_iq
   711                          +
   712                          			; calculate end point
   713  0b91 aa                         TAX		; save length high byte
   714  0b92 98                         TYA		; length low byte
   715  0b93 18                         CLC
   716  0b94 659b                       ADC xl		; low xend = x+length
   717  0b96 859e                       STA xendl
   718  0b98 a8                 	TAY
   719  0b99 8a                         TXA		; high
   720  0b9a 659c                       ADC xh		; high xend = x+length
   721  0b9c 859f                       STA xendh
   722  0b9e aa                 	TAX
   723                          
   724  0b9f c901               	CMP #>xmax	; endpoint outside?
   725  0ba1 9005               	BCC +
   726  0ba3 98                 	TYA
   727  0ba4 e940               	SBC #<xmax
   728  0ba6 b0b3               	BCS error_iq
   729                          +
   730  0ba8 8e3503                     STX savexh
   731  0bab 8c3403                     STY savexl	; also save as cursor
   732                          
   733  0bae 20e209                     JSR ginit	; map in graphic memory
   734                          
   735                          hline_start
   736  0bb1 a59e                       LDA xendl
   737  0bb3 c59b                       CMP xl
   738  0bb5 a59f                       LDA xendh
   739  0bb7 e59c                       SBC xh
   740  0bb9 b013                       BCS hl_noxswap	; xend < x ->
   741                          
   742  0bbb a69e                       LDX xendl	; swap x, xend
   743  0bbd a59b                       LDA xl
   744  0bbf 869b                       STX xl
   745  0bc1 859e                       STA xendl
   746                          
   747  0bc3 a69f                       LDX xendh
   748  0bc5 a49c                       LDY xh
   749  0bc7 849f                       STY xendh
   750  0bc9 869c                       STX xh
   751  0bcb 4cdd0b                     JMP hl_start	; x != xend
   752                          
   753                          hl_noxswap
   754  0bce a59e                       LDA xendl
   755  0bd0 c59b                       CMP xl
   756  0bd2 d009                       BNE hl_start
   757  0bd4 a59f                       LDA xendh
   758  0bd6 c59c                       CMP xh
   759  0bd8 d003                       BNE hl_start	; x = xend ->
   760  0bda 4c510d             	JMP plot_start	; single point
   761                          ;	JMP gexit	; no point
   762                          
   763                          hl_start
   764  0bdd 20f909                     JSR position	; graphic position x,y
   765  0be0 bd2e09                     LDA maskleft,X
   766  0be3 48                         PHA		; save left end mask
   767  0be4 a59e                       LDA xendl
   768  0be6 2907                       AND #%00000111
   769  0be8 8596                       STA tmp2	; xend mod 8, mask index
   770  0bea a59b                       LDA xl
   771  0bec 29f8                       AND #%11111000	; (xl div 8)*8
   772  0bee 8595                       STA tmp1
   773  0bf0 a59e                       LDA xendl	; xend unmasked
   774  0bf2 38                         SEC
   775  0bf3 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   776  0bf5 8595                       STA tmp1
   777  0bf7 a59f                       LDA xendh
   778  0bf9 e59c                       SBC xh
   779  0bfb 4a                         LSR		; / 8 ->  0-39
   780  0bfc a595                       LDA tmp1	; only 1 highest bit
   781  0bfe 6a                         ROR		; and 3 lower bits
   782  0bff 4a                         LSR
   783  0c00 4a                         LSR
   784  0c01 aa                         TAX		; 8-pixel-blocks count
   785  0c02 68                         PLA		; left end x mask
   786                          
   787                          hl_nextblock
   788  0c03 ca                         DEX
   789                          hl_islastblock
   790  0c04 3012                       BMI hl_lastblock
   791                          			; leave loop if X<0
   792  0c06 20f503                     JSR gmask	; first with left end mask
   793  0c09 18                         CLC		; gaddr += 8
   794  0c0a a5a5                       LDA gaddr
   795  0c0c 6908                       ADC #8
   796  0c0e 85a5                       STA gaddr
   797  0c10 9002                       BCC +
   798  0c12 e6a6                       INC gaddr+1
   799  0c14 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   800  0c16 d0eb               	BNE hl_nextblock	; always
   801                          
   802                          hl_lastblock
   803  0c18 a696                       LDX tmp2	; xend mask index
   804  0c1a 3d3609                     AND maskright,X ; mask right end
   805  0c1d 20f503                     JSR gmask	; modify
   806  0c20 4cda09                     JMP gexit	; leave
   807                          
   808                          
   809                          ;-----------------------------------------------------------------
   810                          
   811                          vline
   812  0c23 204f0b                     JSR getxy	; get startpoint
   813  0c26 859c                       STA xh
   814  0c28 8d3503                     STA savexh	; save as cursor too
   815  0c2b 849b                       STY xl
   816  0c2d 8c3403                     STY savexl
   817  0c30 86aa                       STX y
   818                          
   819  0c32 20f1b7                     JSR b_getcomma8bit
   820                          			; get length
   821  0c35 18                         CLC		; calculate end point
   822  0c36 8a                         TXA		; length
   823                          ; DON'T-CHANGE: how long to go vertically (needed later)
   824                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   825                          ;	STA tmp1
   826  0c37 65aa                       ADC y		; length + y
   827  0c39 c9c8                       CMP #ymax
   828  0c3b 9003                       BCC +
   829                          vline_iq
   830  0c3d 20730d                     JSR range_error
   831  0c40 8593               +	STA yend	; endpoint
   832  0c42 c9c8               	CMP #ymax	; outside?
   833  0c44 b0f7               	BCS vline_iq
   834                          
   835  0c46 8d3603             	STA savey	; set cursor y position
   836                          
   837  0c49 20e209                     JSR ginit	; map in graphic memory
   838                          
   839                          vline_start
   840  0c4c a593                       LDA yend
   841  0c4e c5aa                       CMP y
   842  0c50 b00a                       BCS vl_noyswap	; yend < y ->
   843  0c52 a5aa                       LDA y		; swap y, yend
   844  0c54 a693                       LDX yend
   845  0c56 8593                       STA yend
   846  0c58 86aa                       STX y
   847  0c5a f005               	BEQ vl_start	; always (with next branch)
   848                          	; fall through if yend is
   849                          vl_noyswap
   850  0c5c d003                       BNE vl_start	; y = yend ->
   851  0c5e 4c510d             	JMP plot_start	; single point
   852                          ;	JMP gexit	; no point
   853                          
   854                          vl_start
   855  0c61 20f909                     JSR position	; graphic position x,y
   856  0c64 bd0109                     LDA bitmask,X
   857  0c67 8596                       STA tmp2	; save mask
   858                          ; DON'T-CHANGE: replace ...
   859  0c69 38                         SEC
   860  0c6a a593                       LDA yend
   861  0c6c e5aa                       SBC y		; vertical length
   862  0c6e aa                         TAX
   863                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   864                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   865                          ;	LDX tmp1
   866  0c6f e8                         INX		; +1 (exit on 0)
   867                          vl_nextline
   868  0c70 a596                       LDA tmp2
   869  0c72 20f503                     JSR gmask	; modify 
   870  0c75 c8                         INY		; go down
   871  0c76 c008                       CPY #8		; 8-line wrap
   872  0c78 d00e                       BNE +
   873  0c7a a5a5                       LDA gaddr	; gaddr += 320
   874  0c7c 693f               	ADC #$40-1	; compensate for C = 1
   875  0c7e 85a5                       STA gaddr
   876  0c80 a5a6                       LDA gaddr+1
   877  0c82 6901                       ADC #$01
   878  0c84 85a6                       STA gaddr+1
   879  0c86 a000                       LDY #0		; wrap y offset
   880  0c88 ca                 +	DEX		; all vertical positions done?
   881  0c89 d0e5                       BNE vl_nextline
   882  0c8b 4cda09                     JMP gexit	; leave
   883                          
   884                          
   885                          ;-----------------------------------------------------------------
   886                          
   887                          line
   888  0c8e 204f0b                     JSR getxy	; get startpoint
   889  0c91 849b                       STY xl 
   890  0c93 859c                       STA xh
   891  0c95 86aa                       STX y
   892                          
   893  0c97 204c0b                     JSR getcommaxy	; get endpoint
   894                          line_start
   895  0c9a 8c3403                     STY savexl	; save as cursor position too
   896  0c9d 849e                       STY xendl
   897  0c9f 8d3503                     STA savexh
   898  0ca2 859f                       STA xendh
   899  0ca4 8e3603                     STX savey
   900  0ca7 8693                       STX yend
   901                          
   902  0ca9 20e209                     JSR ginit	; map in graphic memory
   903                          
   904  0cac a000                       LDY #$00	; initialize to 0
   905  0cae 84a8                       STY ydir
   906  0cb0 8495                       STY kl
   907  0cb2 8496                       STY kh
   908                          
   909  0cb4 38                         SEC
   910  0cb5 a59e                       LDA xendl	; calculate dx
   911  0cb7 e59b                       SBC xl
   912  0cb9 85ab                       STA dxl
   913  0cbb a59f                       LDA xendh
   914  0cbd e59c                       SBC xh
   915  0cbf 85a7                       STA dxh
   916                          
   917  0cc1 b025                       BCS li_xend_right
   918                          	; dx != 0
   919  0cc3 98                         TYA		; negate dx
   920  0cc4 38                         SEC		; dx = 0 - dx
   921  0cc5 e5ab                       SBC dxl
   922  0cc7 85ab                       STA dxl
   923  0cc9 98                         TYA
   924  0cca e5a7                       SBC dxh
   925  0ccc 85a7                       STA dxh
   926                          			; C=0 always, needed later
   927  0cce a69b                       LDX xl		; swap x low
   928  0cd0 a49e                       LDY xendl
   929  0cd2 869e                       STX xendl
   930  0cd4 849b                       STY xl
   931                          
   932  0cd6 a69c                       LDX xh		; swap x high
   933  0cd8 a49f                       LDY xendh
   934  0cda 869f                       STX xendh
   935  0cdc 849c                       STY xh
   936                          
   937  0cde a6aa                       LDX y		; swap y
   938  0ce0 a493                       LDY yend
   939  0ce2 8693                       STX yend
   940  0ce4 84aa                       STY y
   941                          
   942  0ce6 9009                       BCC li_x_different
   943                          			; C=0 always (from negation before)
   944                          
   945                          li_xend_right
   946  0ce8 a5ab                       LDA dxl		; dx = 0?
   947  0cea 05a7                       ORA dxh
   948  0cec d003                       BNE li_x_different
   949  0cee 4c4c0c                     JMP vline_start	; vertical line case
   950                          
   951                          li_x_different
   952  0cf1 38                         SEC		; calculate dy
   953  0cf2 a593                       LDA yend
   954  0cf4 e5aa                       SBC y
   955  0cf6 b006                       BCS li_y_right
   956  0cf8 49ff                       EOR #$FF	; negate dy (two's complement)
   957  0cfa 6901                       ADC #$01	; C=0
   958  0cfc 85a8                       STA ydir	; flag y goes up
   959                          
   960                          li_y_right
   961  0cfe 85a9                       STA dy
   962  0d00 d003                       BNE +
   963  0d02 4cb10b                     JMP hline_start	; horizontal line case
   964                          +
   965                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   966                          
   967  0d05 a5a7                       LDA dxh		; dx > dy
   968  0d07 d017                       BNE line_flat	; yes -> flat
   969  0d09 a5a9                       LDA dy		; no -> steep
   970  0d0b aa                         TAX
   971  0d0c c5ab                       CMP dxl
   972  0d0e 9010                       BCC line_flat
   973                          
   974                          line_steep
   975  0d10 e8                         INX	
   976  0d11 86a3                       STX cl		; c = dy+1
   977  0d13 4a                         LSR		; k = dy/2
   978  0d14 8595                       STA kl
   979  0d16 a5a8                       LDA ydir
   980  0d18 d003                       BNE +
   981  0d1a 4c0a0b                     JMP line_down_steep	; y down, steep
   982  0d1d 4c1e0a             +	JMP line_up_steep	; y up, steep
   983                          
   984                          line_flat
   985  0d20 a5a7                       LDA dxh
   986  0d22 a8                         TAY
   987  0d23 a6ab                       LDX dxl
   988  0d25 e8                         INX
   989  0d26 d001                       BNE +
   990  0d28 c8                         INY
   991  0d29 86a3               +	STX cl		; c = dx+1
   992  0d2b 84a4                       STY ch
   993                          
   994  0d2d 4a                         LSR		; k = dx/2
   995  0d2e 8596                       STA kh
   996  0d30 a5ab                       LDA dxl
   997  0d32 6a                         ROR		; dx/2
   998  0d33 8595                       STA kl
   999  0d35 a5a8                       LDA ydir	
  1000  0d37 d003                       BNE +
  1001  0d39 4cb40a                     JMP line_down_flat	; y down, flat
  1002  0d3c 4c5f0a             +	JMP line_up_flat	; y up, flat
  1003                          
  1004                          ;-----------------------------------------------------------------
  1005                          
  1006                          plot
  1007  0d3f 204f0b                     JSR getxy	; get parameter
  1008  0d42 859c                       STA xh		; save x/y
  1009  0d44 849b                       STY xl
  1010  0d46 86aa                       STX y
  1011  0d48 8d3503                     STA savexh	; and store as cursor
  1012  0d4b 8c3403                     STY savexl
  1013  0d4e 8e3603                     STX savey
  1014                          
  1015                          plot_start
  1016  0d51 20f909                     JSR position	; calculate graphical address
  1017                          
  1018  0d54 a501                       LDA prozport
  1019  0d56 29fd                       AND #%11111101	; Kernal ROM disable
  1020  0d58 78                         SEI			
  1021  0d59 8501                       STA prozport
  1022                          
  1023  0d5b 20ed03                     JSR gchange	; change graphical data
  1024                          
  1025  0d5e a501                       LDA prozport
  1026  0d60 0902                       ORA #%00000010	; kernal ROM enable
  1027  0d62 8501                       STA prozport
  1028  0d64 58                         CLI
  1029  0d65 60                         RTS
  1030                          
  1031                          ;-----------------------------------------------------------------
  1032                          
  1033                          move
  1034  0d66 204f0b                     JSR getxy	; get parameter
  1035  0d69 8d3503                     STA savexh	; just save as cursor
  1036  0d6c 8c3403                     STY savexl
  1037  0d6f 8e3603                     STX savey
  1038  0d72 60                         RTS
  1039                          
  1040                          
  1041                          ;-----------------------------------------------------------------
  1042                          
  1043                          range_error
  1044  0d73 ad3703             	LDA savemo
  1045  0d76 29f0               	AND #$F0
  1046  0d78 d003               	BNE +
  1047  0d7a 68                 	PLA			; cleanup JSR
  1048  0d7b 68                 	PLA
  1049  0d7c 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1050                          				; error mode 2: cut value: control back
  1051                          				; to handle value correction
  1052  0d7d 2920               +	AND #$20
  1053  0d7f d0fb               	BNE -
  1054  0d81 68                 	PLA			; cleanup JSR
  1055  0d82 68                 	PLA
  1056                          setmode_error
  1057  0d83 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1058                          
  1059                          ;-----------------------------------------------------------------
  1060                          
  1061                          setmode
  1062  0d86 209eb7                     JSR b_get8bit
  1063  0d89 e003                       CPX #3
  1064  0d8b 9012                       BCC +			; less then 3, modification mode
  1065  0d8d e006               	CPX #6
  1066  0d8f b0f2               	BCS setmode_error	; out of range
  1067                          				; error mode
  1068  0d91 690d               	ADC #13			; C=0, therefore -3
  1069                          				; 3-5 -> 16-18
  1070                          				; put A's bit 4-7 into savemo
  1071  0d93 4d3703             	EOR savemo		; ********
  1072  0d96 29f0               	AND #$F0		; ****0000
  1073  0d98 4d3703             	EOR savemo		; AAAAmmmm
  1074  0d9b 8d3703             	STA savemo		; 
  1075  0d9e 60                 	RTS
  1076                          
  1077  0d9f 8a                 +	TXA
  1078  0da0 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1079  0da3 290f               	AND #$0F
  1080  0da5 4d3703             	EOR savemo
  1081  0da8 8d3703             	STA savemo
  1082                          setmode_enter
  1083  0dab e001               	CPX #$01
  1084  0dad b01a                       BCS set_or_toggle
  1085                          
  1086                          modereset
  1087  0daf a909                       LDA #>(nbitmask)
  1088  0db1 8df103                     STA gchange_op+2
  1089  0db4 a909                       LDA #<(nbitmask)
  1090  0db6 8df003                     STA gchange_op+1
  1091  0db9 a93d                       LDA #$3D		; AND abs,X
  1092  0dbb 8def03                     STA gchange_op
  1093  0dbe a931                       LDA #$31		; AND (zp),Y
  1094  0dc0 8df703                     STA gmask_op
  1095  0dc3 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1096  0dc5 8df603                     STA gmask_flip+1
  1097  0dc8 60                         RTS
  1098                          
  1099                          set_or_toggle
  1100  0dc9 d01a                       BNE modetoggle
  1101                          modeset
  1102  0dcb a909                       LDA #>(bitmask)
  1103  0dcd 8df103                     STA gchange_op+2
  1104  0dd0 a901                       LDA #<(bitmask)
  1105  0dd2 8df003                     STA gchange_op+1
  1106  0dd5 a91d                       LDA #$1D		; OR abs,X
  1107  0dd7 8def03                     STA gchange_op
  1108  0dda a911                       LDA #$11		; OR (zp),Y
  1109  0ddc 8df703                     STA gmask_op
  1110  0ddf a900                       LDA #$00		; EOR #$00, nicht invertieren
  1111  0de1 8df603                     STA gmask_flip+1
  1112  0de4 60                         RTS
  1113                          
  1114                          modetoggle
  1115  0de5 a909                       LDA #>(bitmask)
  1116  0de7 8df103                     STA gchange_op+2
  1117  0dea a901                       LDA #<(bitmask)
  1118  0dec 8df003                     STA gchange_op+1
  1119  0def a95d                       LDA #$5D		; EOR abs,X
  1120  0df1 8def03                     STA gchange_op
  1121  0df4 a951                       LDA #$51		; EOR (zp),Y
  1122  0df6 8df703                     STA gmask_op
  1123  0df9 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1124  0dfb 8df603                     STA gmask_flip+1
  1125  0dfe 60                         RTS
  1126                          
  1127                          
  1128                          ;-----------------------------------------------------------------
  1129                          
  1130                          ; get pixel (check if pixel set)
  1131                          ; not used
  1132                          
  1133                          get
  1134  0dff 204c0b                     JSR getcommaxy
  1135  0e02 859c                       STA xh
  1136  0e04 849b                       STY xl
  1137  0e06 86aa                       STX y
  1138                          
  1139  0e08 20f909                     JSR position
  1140                          
  1141  0e0b a501                       LDA prozport
  1142  0e0d 29fd               	AND #%11111101	; Kernal ROM disable
  1143  0e0f 78                         SEI
  1144  0e10 8501                       STA prozport
  1145                          
  1146  0e12 b1a5                       LDA (gaddr),Y
  1147  0e14 3d0109                     AND bitmask,X
  1148  0e17 a8                         TAY
  1149  0e18 a501                       LDA prozport
  1150  0e1a 0902               	ORA #%00000010	; kernal ROM enable
  1151  0e1c 8501                       STA prozport
  1152  0e1e 58                         CLI
  1153  0e1f 4ca2b3                     JMP b_byte2fac
  1154                          
  1155                          
  1156                          ;-----------------------------------------------------------------
  1157                          
  1158                          relto
  1159  0e22 208aad                     JSR b_getval	; get X offset (+/-)
  1160  0e25 a561               	LDA facexp	; FAC exponent
  1161  0e27 c990               	CMP #$90	; more than 16 bit
  1162  0e29 b031               	BCS relto_error	; illegal quantity
  1163  0e2b 209bbc                     JSR b_fac2int	; to signed integer
  1164                          
  1165  0e2e 18                         CLC
  1166  0e2f a565                       LDA facintl
  1167  0e31 6d3403                     ADC savexl
  1168  0e34 859e                       STA xendl
  1169  0e36 a564                       LDA facinth
  1170  0e38 6d3503                     ADC savexh
  1171  0e3b 859f                       STA xendh	; xend = savex+facint
  1172                          
  1173  0e3d 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1174  0e40 208aad                     JSR b_getval
  1175  0e43 a561                       LDA facexp	; FAC exponent
  1176  0e45 c990                       CMP #$90	; more than 16 bit
  1177  0e47 b013                       BCS relto_error	; illegal quantity
  1178  0e49 209bbc                     JSR b_fac2int	; to signed integer
  1179  0e4c 18                         CLC
  1180  0e4d a565                       LDA facintl
  1181  0e4f 6d3603                     ADC savey
  1182  0e52 8593                       STA yend	; yend = savey+facint
  1183                          
  1184  0e54 a59f                       LDA xendh	; check end coord. x
  1185  0e56 c901                       CMP #>xmax
  1186  0e58 900b                       BCC rt_xok
  1187  0e5a f003                       BEQ +
  1188                          relto_error
  1189  0e5c 20730d                     JSR range_error
  1190  0e5f a59e               +	LDA xendl
  1191  0e61 c940                       CMP #<xmax
  1192  0e63 b0f7                       BCS relto_error
  1193                          rt_xok
  1194  0e65 a593                       LDA yend	; check end coord. y
  1195  0e67 c9c8                       CMP #ymax
  1196  0e69 b0f1                       BCS relto_error
  1197                          
  1198  0e6b ad3403                     LDA savexl
  1199  0e6e 859b                       STA xl
  1200  0e70 ad3503                     LDA savexh
  1201  0e73 859c                       STA xh
  1202  0e75 ad3603                     LDA savey
  1203  0e78 85aa                       STA y
  1204  0e7a a49e                       LDY xendl
  1205  0e7c a59f                       LDA xendh
  1206  0e7e a693                       LDX yend	; xend/yend = cursor + x/y
  1207                          
  1208  0e80 4c9a0c                     JMP line_start	; draw line x/y to xend/yend
  1209                          
  1210                          
  1211                          ;-----------------------------------------------------------------
  1212                          
  1213                          char
  1214  0e83 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1215  0e86 e028                       CPX #40	
  1216  0e88 9003                       BCC +
  1217                          char_error
  1218  0e8a 4c48b2                     JMP b_illquant
  1219  0e8d 86fb               +	STX gpos	; save x coord.
  1220  0e8f 20f1b7                     JSR b_getcomma8bit
  1221                          			; get char. position y 0-24
  1222  0e92 e019                       CPX #25
  1223  0e94 b0f4                       BCS char_error
  1224  0e96 86fc                       STX gpos+1	; save y coord.
  1225                          
  1226  0e98 20fdae                     JSR b_getcomma	; get string
  1227  0e9b 209ead                     JSR b_getexpr
  1228  0e9e 20a3b6                     JSR b_stringval ; string address in str
  1229  0ea1 48                         PHA		; string length
  1230  0ea2 a6fc                       LDX gpos+1	; y coord. for char. position
  1231  0ea4 8a                         TXA
  1232  0ea5 2903                       AND #$03	; mask 2 bits
  1233  0ea7 a8                         TAY		; table index
  1234  0ea8 a900                       LDA #$00
  1235  0eaa 85fc                       STA gpos+1	; x high
  1236  0eac a5fb                       LDA gpos	; saved x: multiply by 8
  1237  0eae 0a                         ASL
  1238  0eaf 0a                         ASL
  1239  0eb0 0a                         ASL
  1240  0eb1 26fc                       ROL gpos+1	; overflow to high byte
  1241  0eb3 791109                     ADC ytabl,Y
  1242  0eb6 85a5                       STA gaddr
  1243  0eb8 a5fc                       LDA gpos+1	; x high
  1244  0eba 7d1509                     ADC ytabh,X
  1245  0ebd 85a6                       STA gaddr+1
  1246  0ebf 68                         PLA		; string length
  1247  0ec0 a000                       LDY #$00	; string index
  1248  0ec2 aa                         TAX		; length
  1249  0ec3 e8                         INX		; prepare as counter
  1250                          char_loop
  1251  0ec4 ca                         DEX
  1252  0ec5 f008                       BEQ char_exit
  1253  0ec7 b122                       LDA (str),Y	; read string
  1254  0ec9 20d00e                     JSR char_display
  1255  0ecc c8                         INY
  1256  0ecd d0f5                       BNE char_loop
  1257                          char_exit
  1258  0ecf 60                         RTS
  1259                          
  1260                          char_display
  1261  0ed0 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1262  0ed2 8a                         TXA		; save register X+Y
  1263  0ed3 48                         PHA
  1264  0ed4 98                         TYA
  1265  0ed5 48                         PHA
  1266  0ed6 a5d7                       LDA z_tmp	; get saved character
  1267  0ed8 1049                       BPL char_normal
  1268                          
  1269                          char_inverse
  1270  0eda 297f                       AND #%01111111	; mask bit 7
  1271  0edc c97f                       CMP #%01111111	; was 255? (pi)
  1272  0ede d002                       BNE +
  1273  0ee0 a95e                       LDA #$5E	; screen code for pi
  1274  0ee2 c920               +	CMP #$20	; control character?
  1275  0ee4 9038                       BCC char_disp_leave
  1276                          			; yes, skip
  1277  0ee6 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1278                          			; $C0-$FF -> $40-$7F
  1279                          			; OPT: BNE char_hires
  1280                          			; OPT: char_normal
  1281                          char_hires
  1282  0ee8 a6c7                       LDX z_reverseflag
  1283  0eea f002                       BEQ +
  1284  0eec 0980                       ORA #%10000000	; invert char.
  1285  0eee 48                 +	PHA		; save char. for later
  1286  0eef 78                         SEI
  1287  0ef0 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1288  0ef2 8501                       STA prozport	; char. rom base = $D000
  1289  0ef4 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1290  0ef6 85fc                       STA gpos+1	; 
  1291  0ef8 68                         PLA		; char. code
  1292  0ef9 0a                         ASL		; *8
  1293  0efa 26fc                       ROL gpos+1
  1294  0efc 0a                         ASL
  1295  0efd 26fc                       ROL gpos+1
  1296  0eff 0a                         ASL
  1297  0f00 26fc                       ROL gpos+1
  1298  0f02 85fb                       STA gpos	; addr. in char. rom for char.
  1299                          
  1300  0f04 a007                       LDY #$07	; 8 hires lines
  1301                          char_line
  1302  0f06 b1fb                       LDA (gpos),Y	; read character line
  1303  0f08 20f503                     JSR gmask	; write to hires screen
  1304  0f0b 88                         DEY
  1305  0f0c 10f8                       BPL char_line
  1306                          
  1307  0f0e a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1308  0f10 8501                       STA prozport
  1309  0f12 58                         CLI
  1310                          
  1311  0f13 18                         CLC		; step char position to left
  1312  0f14 a5a5                       LDA gaddr	; ( +8 )
  1313  0f16 6908                       ADC #$08
  1314  0f18 85a5                       STA gaddr
  1315  0f1a 9002                       BCC +
  1316  0f1c e6a6                       INC gaddr+1
  1317                          +
  1318                          char_disp_leave
  1319  0f1e 68                 	PLA		; pass written character back
  1320  0f1f a8                         TAY		; restore saved registers
  1321  0f20 68                         PLA
  1322  0f21 aa                         TAX
  1323  0f22 60                         RTS
  1324                          
  1325                          char_normal
  1326  0f23 c920                       CMP #$20	; control character?
  1327  0f25 90f7                       BCC char_disp_leave
  1328  0f27 c960                       CMP #$60
  1329  0f29 9004                       BCC +
  1330  0f2b 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1331  0f2d d002                       BNE ++
  1332  0f2f 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1333  0f31 4ce80e             ++	JMP char_hires	; 		OPT: Bxx
  1334                          
  1335                          
  1336                          ;-----------------------------------------------------------------
  1337                          
  1338                          to
  1339  0f34 ad3403                     LDA savexl
  1340  0f37 859b                       STA xl
  1341  0f39 ad3503                     LDA savexh
  1342  0f3c 859c                       STA xh
  1343  0f3e ad3603                     LDA savey
  1344  0f41 85aa                       STA y
  1345  0f43 204f0b                     JSR getxy
  1346  0f46 4c9a0c                     JMP line_start
  1347                          
  1348                          ;-----------------------------------------------------------------
  1349                          
  1350                          unnew
  1351                          
  1352  0f49 a52b               	lda bassta
  1353  0f4b 8522               	sta str
  1354  0f4d a52c               	lda bassta+1
  1355  0f4f 8523               	sta str+1
  1356  0f51 a001               	ldy #1
  1357  0f53 98                 	tya
  1358  0f54 9122               	sta (str),y		; != 0
  1359                          
  1360  0f56 2033a5             	jsr b_rechain		; starting from bassta
  1361                          				; result in (str)
  1362  0f59 18                 	clc			; str+1 -> new basic end
  1363  0f5a a423               	ldy str+1
  1364  0f5c a522               	lda str
  1365  0f5e 6902               	adc #2
  1366  0f60 852d               	sta basend
  1367  0f62 9001               	bcc +
  1368  0f64 c8                 	iny
  1369  0f65 842e               +	sty basend+1
  1370  0f67 4c60a6             	jmp b_clr		; perform CLR
  1371                          
  1372                          ;-----------------------------------------------------------------
  1373                          graext_end

; ******** Source: ge-run.asm
    43                          
    44                          
