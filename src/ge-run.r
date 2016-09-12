
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
    29  080d a26d               	ldx #<graext_end	; setup basic
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
     6                          ;	2016-06-16 v 1.24
     7                          ;	2016-05-29 v 1.23
     8                          ;	2016-05-20 v 1.22
     9                          ;	2016-05-16 v 1.21
    10                          ;	2016-02-23 v 1.20
    11                          ;	2016-01-15 v 1.19
    12                          ;	1992-12-28 v 1.18
    13                          ;	1986-03-24 v 1.17
    14                          ;	1985       v 0.00 - 1.16
    15                          ;
    16                          ; the original source has been lost.
    17                          ; development has based on the implemention
    18                          ; done on a forth-64 written with its forth assembler.
    19                          ; the code has been pulled out from there and enriched
    20                          ; with some glue code to get a basic extension.
    21                          
    22                          ; command dispatcher style JMP/RTS
    23                          ;	(if defined)
    24                          ;command_rts_style=1
    25                          
    26                          ; error handling 
    27                          ;	(if defined)
    28                          ;no_error=1
    29                          
    30                          ; basic interpreter registers, addresses and entry points
    31                          
    32                          str     = $22		; string address
    33                          bassta	= $2b		; basic start pointer
    34                          basend	= $2d		; basic end pointer
    35                          ijmp    = $55		; address of JMP (addr)
    36                          chrget  = $73		; basic charget routine
    37                          facintl = $65		; integer result from b_fac2int
    38                          facinth = $64
    39                          facexp  = $61		; fac exponent, after b_getval
    40                          
    41                          z_reverseflag = $C7	; character routine
    42                          z_lastkey = $D7		; original use case, unused here
    43                          z_tmp = z_lastkey	; temporary reused for character routine
    44                          
    45                          v_baserr = $0300	; vector error routine
    46                          v_basstp = $0328	; vector error routine
    47                          v_bascmd = $0308	; vector interpreter parsing
    48                          v_basexp = $030a	; vector evaluate expression
    49                          
    50                          basic_rom = $A000	; start of BASIC ROM
    51                          
    52                          b_clr = $A660		; CLR command
    53                          b_interpreter = $A7AE	; interpreter loop
    54                          b_execstatement = $A7E7	; process statement
    55                          b_getcomma = $AEFD	; read comma from basic text
    56                          b_illquant = $B248	; error "illegal quantity"
    57                          b_syntaxerror = $AF08	; error "syntax"
    58                          b_get8bit = $B79E	; read 8 bit numeric value from
    59                          			; basic text
    60                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    61                          			; from basic text
    62                          b_getval = $AD8A	; read numeric value from basic text
    63                          b_getexpr = $AD9E	; read expression from basic text
    64                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    65                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    66                          b_fac2int = $BC9B	; convert FAC to integer
    67                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    68                          b_rechain = $A533	; rechain basic lines
    69                          
    70                          ; hardware registers and values
    71                          
    72                          prozport = $01		; processor port
    73                          memrom = %00110111	; basic+kernal rom
    74                          membas = %00110110	; basic ram+kernal rom
    75                          memram = %00110101	; basic+kernal ram
    76                          
    77                          vic_cr	= $D011		; VIC control register
    78                          vic_mcr	= $D018		; VIC memory control register
    79                          cia_pra	= $DD00		; CIA 2 port register A
    80                          
    81                          cram	= $CC00		; start of color ram
    82                          
    83                          gram	= $e000		; start of graphic bitmap ram
    84                          gramp	= gram >> 8	; start page of bitmap
    85                          
    86                          ; constants 
    87                          
    88                          xmax	= 320		; max x dimension
    89                          ymax	= 200		; max y dimension
    90                          
    91                          ; zeropage variables
    92                          
    93                          x	= $9B		; start coordinate x, low+high
    94                          xl	= x
    95                          xh	= x+1
    96                          y	= $AA		; start coordinate y
    97                          
    98                          xendl	= $9E		; end coordinate x, low+high
    99                          xendh	= $9F
   100                          yend	= $93		; end coordinate y
   101                          
   102                          kl	= $95		; gradient for lines, low+high
   103                          kh	= kl+1
   104                          
   105                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
   106                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
   107                          
   108                          dxl	= $AB		; x delta, low+high
   109                          dxh	= $A7
   110                          
   111                          dy	= $A9		; y delta
   112                          
   113                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   114                          
   115                          cl	= $A3		; dot count, low+high
   116                          ch	= $A4
   117                          
   118                          gaddr	= $A5		; graphic address
   119                          
   120                          gpos	= $FB		; in graphic position
   121                          
   122                          gcol	= $FD		; graphic color, in "graphic on" context only
   123                          
   124                          
   125                          ; static ram areas
   126                          
   127                          savexl	= $0334		; the graphic cursor: x low 
   128                          savexh	= savexl+1	; the graphic cursor: x high
   129                          savey	= savexh+1	; the graphic cursor: y
   130                          savemo	= savey+1	; the graphic mode
   131                          saveverr = savemo+1	; original v_baserr
   132                          savevstp = saveverr+2	; original v_basstp
   133                          
   134                          gramcode = $03ed	; real place for gchange and gmask routines,
   135                          			; they take 15 bytes
   136                          
   137                          ;
   138                          ; initialize extension
   139                          
   140                          init
   141  0829 a981                       LDA #<(parse)	; basic interpreter parser hook
   142  082b 8d0803                     STA v_bascmd
   143  082e a908                       LDA #>(parse)
   144  0830 8d0903                     STA v_bascmd+1
   145                          
   146  0833 ad2803                     LDA v_basstp
   147  0836 8d3a03             	STA savevstp
   148  0839 a975                       LDA #<(stop)	; basic interpreter stop hook
   149  083b 8d2803                     STA v_basstp
   150  083e ad2903                     LDA v_basstp+1
   151  0841 8d3b03             	STA savevstp+1
   152  0844 a908                       LDA #>(stop)
   153  0846 8d2903                     STA v_basstp+1
   154                          
   155  0849 ad0003                     LDA v_baserr
   156  084c 8d3803             	STA saveverr
   157  084f a96f                       LDA #<(error)	; basic interpreter error hook
   158  0851 8d0003                     STA v_baserr
   159  0854 ad0103                     LDA v_baserr+1
   160  0857 8d3903             	STA saveverr+1
   161  085a a908                       LDA #>(error)
   162  085c 8d0103                     STA v_baserr+1
   163                          
   164  085f a200               	LDX #0		; set graphic cursor to (0,0)
   165  0861 8e3403             	STX savexl
   166  0864 8e3503             	STX savexh
   167  0867 8e3603             	STX savey
   168  086a e8                 	INX
   169  086b 8e3703             	STX savemo	; set mode 1
   170  086e 60                         RTS
   171                          
   172                          error	
   173                          	; reg A may destroyed
   174  086f 204509             	JSR gra_off		; uses only reg A
   175  0872 6c3803             	JMP (saveverr)		; to original vector
   176                          
   177                          stop	
   178                          	; reg A may destroyed
   179  0875 a591               	LDA $91			; Scan code
   180  0877 c97f               	CMP #$7F		; STOP key?
   181  0879 d003               	BNE nostop
   182  087b 204509             	JSR gra_off		; uses only reg A
   183                          nostop
   184  087e 6c3a03             	JMP (savevstp)		; to original vector
   185                          
   186                          ;-----------------------------------------------------------------
   187                          
   188                          ; start parsing an extension command ...
   189                          
   190                          parse
   191  0881 207300                     JSR chrget			; next char.
   192  0884 08                 	PHP
   193  0885 c926                       CMP #'&'			; command prefix
   194  0887 f004                       BEQ newcmd
   195  0889 28                         PLP
   196  088a 4ce7a7                     JMP b_execstatement
   197                          newcmd
   198  088d 28                 	PLP
   199  088e 207300                     JSR chrget			; command character
   200                          
   201  0891 a00c                       LDY #(cmdsend-cmds)		; map character to
   202                          					; command address ...
   203                          checknextcmd
   204  0893 88                         DEY
   205  0894 f01c               	BEQ parse_error
   206  0896 d9b508                     CMP cmds,Y
   207  0899 d0f8                       BNE checknextcmd		; try next
   208  089b 88                         DEY				; found
   209  089c 98                         TYA
   210  089d 0a                         ASL				; *2
   211  089e a8                         TAY
   212                          !ifndef command_rts_tyle {
   213                          	!set co=0			; command offset in jump table
   214  089f b9c208                     LDA cmdaddr+1,Y                 ; high byte from table
   215  08a2 8556                       STA ijmp+1
   216  08a4 b9c108                     LDA cmdaddr,Y                   ; low byte from table
   217  08a7 8555                       STA ijmp
   218  08a9 207300                     JSR chrget			; read next byte in basic text
   219  08ac 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   220  08af 4caea7                     JMP b_interpreter		; continue parsing
   221                          } else {
   222                          	!set co=1			; command offset in jump table
   223                          	LDA #>(b_interpreter-1)		; return to interpreter
   224                          	PHA
   225                          	LDA #<(b_interpreter-1)
   226                          	PHA				
   227                                  LDA cmdaddr+1,Y			; command address (RTS style)
   228                                  PHA				; high byte on stack
   229                                  LDA cmdaddr,Y			; command address (RTS style)
   230                                  PHA				; low byte on stack
   231                                  JMP chrget			; read next byte in basic text 
   232                          					; and RTS to command
   233                          }
   234                          parse_error
   235  08b2 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   236                          
   237                          ;-----------------------------------------------------------------
   238                          
   239                          ; the most commonly used command placed at the end ...
   240                          
   241  08b5 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   242                          cmdsend
   243                          
   244                          cmdaddr
   245  08c1 4c0f3e09830e860d...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   246  08cd 370f230c720b8e0c...        !word to-co,vline-co,hline-co,line-co,plot-co
   247                          
   248  08d7 934752412d455854...author	!text 147,"GRA-EXT V1.24 1986,2016 JOHANN@KLASEK.AT",0
   249                          
   250                          bitmask
   251  0901 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   252                          nbitmask
   253  0909 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   254                          ytabl
   255  0911 004080c0           	!byte $00,$40,$80,$c0
   256                          ytabh
   257  0915 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   258  0919 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   259  091d eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   260  0921 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   261  0925 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   262  0929 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   263  092d fe                 	!byte gramp+$1e
   264                          
   265                          ; for horiz. line
   266                          
   267  092e ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   268                          
   269  0936 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   270                          
   271                          
   272                          ;-----------------------------------------------------------------
   273                          
   274                          graphic
   275  093e 209eb7                     JSR b_get8bit
   276  0941 e000                       CPX #$00
   277  0943 d013                       BNE gra_other
   278                          gra0			; &G 0
   279                          gra_off
   280  0945 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   281  0947 8d00dd                     STA cia_pra
   282  094a a915                       LDA #((1 <<4) + (2 <<1) + 1)
   283                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   284                          			; char addr $1000/4096 = char. ROM
   285  094c 8d18d0                     STA vic_mcr	; VIC memory control
   286  094f ad11d0                     LDA vic_cr	; VIC control register
   287  0952 29df                       AND #%11011111	; Hires mode off
   288  0954 8d11d0                     STA vic_cr
   289  0957 60                         RTS
   290                          
   291                          gra_other
   292  0958 e001                       CPX #$01
   293  095a f00f               	BEQ gra1
   294  095c e002               	CPX #$02
   295  095e f00e                       BEQ gra2
   296  0960 e004               	CPX #$04
   297  0962 f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   298  0964 e003               	CPX #$03	; &G 3 (graphic on)
   299  0966 f029               	BEQ gra_on
   300  0968 4c48b2                     JMP b_illquant	; parameter illegal
   301                          	
   302                          gra1			; &G 1
   303  096b 20a709             	JSR gra_clear
   304                          
   305                          gra2
   306  096e 20f1b7                     JSR b_getcomma8bit
   307  0971 8a                         TXA		; foreground color
   308  0972 0a                         ASL		; upper nibble
   309  0973 0a                         ASL
   310  0974 0a                         ASL
   311  0975 0a                         ASL
   312  0976 85fd                       STA gcol
   313  0978 20f1b7                     JSR b_getcomma8bit
   314  097b 8a                         TXA		; background color
   315  097c 290f                       AND #$0F
   316  097e 05fd                       ORA gcol
   317  0980 a000                       LDY #$00
   318                          cram_loop
   319  0982 9900cc                     STA cram,Y	; fill color RAM
   320  0985 9900cd                     STA cram+$100,Y
   321  0988 9900ce                     STA cram+$200,Y
   322  098b 99e8ce                     STA cram+$300-24,Y
   323  098e c8                         INY
   324  098f d0f1                       BNE cram_loop
   325                          
   326                          gra_on
   327  0991 20c609             	JSR gra_setupcode
   328                          
   329  0994 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   330  0996 8d00dd                     STA cia_pra
   331  0999 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   332  099b 8d18d0                     STA vic_mcr	; VIC memory control
   333  099e ad11d0                     LDA vic_cr	; VIC control register
   334  09a1 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   335  09a3 8d11d0                     STA vic_cr
   336  09a6 60                         RTS
   337                          
   338                          gra_clear
   339  09a7 a220                       LDX #$20	; Pages (8 KByte)
   340  09a9 a9e0                       LDA #>gram
   341  09ab 85fc                       STA gpos+1
   342  09ad a000                       LDY #$00
   343  09af 84fb                       STY gpos
   344  09b1 98                         TYA
   345                          gra_fill
   346  09b2 91fb                       STA (gpos),Y	; Loop unroll
   347  09b4 c8                         INY
   348  09b5 91fb                       STA (gpos),Y
   349  09b7 c8                         INY
   350  09b8 91fb                       STA (gpos),Y
   351  09ba c8                         INY
   352  09bb 91fb                       STA (gpos),Y
   353  09bd c8                         INY
   354  09be d0f2                       BNE gra_fill
   355  09c0 e6fc                       INC gpos+1
   356  09c2 ca                         DEX
   357  09c3 d0ed                       BNE gra_fill
   358  09c5 60                 	RTS
   359                          
   360                          gra_setupcode
   361  09c6 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   362                          gra_copycode
   363  09c8 bde909             	LDA gromcode-1,X
   364  09cb 9dec03             	STA gramcode-1,X
   365  09ce ca                 	DEX
   366  09cf d0f7               	BNE gra_copycode
   367  09d1 ad3703             	LDA savemo
   368  09d4 290f               	AND #$0F
   369  09d6 aa                 	TAX
   370  09d7 4cab0d             	JMP setmode_enter	; re-apply mode to routines
   371                          				; implicit RTS
   372                          
   373                          ;-----------------------------------------------------------------
   374                          
   375                          gexit
   376  09da a501                       LDA prozport
   377  09dc 0902                       ORA #%00000010	; kernal ROM enable
   378  09de 8501                       STA prozport
   379  09e0 58                         CLI		; allow interrupts
   380  09e1 60                         RTS
   381                          
   382                          ;-----------------------------------------------------------------
   383                          
   384                          ginit
   385  09e2 a501                       LDA prozport
   386  09e4 29fd                       AND #%11111101	; Kernal ROM disable
   387  09e6 78                         SEI		; disable interrupts
   388  09e7 8501                       STA prozport
   389  09e9 60                         RTS
   390                          
   391                          ;-----------------------------------------------------------------
   392                          
   393                          ; These are selfmodified routines, which has to placed into RAM
   394                          ; (on every graphic "on")
   395                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   396                          
   397                          gromcode
   398                          
   399                          !pseudopc gramcode {
   400                          
   401                          ; change a graphic location
   402                          
   403                          gchange
   404  09ea b1a5                       LDA (gaddr),Y
   405                          gchange_op
   406  09ec 1d0109                     ORA bitmask,X
   407  09ef 91a5                       STA (gaddr),Y
   408  09f1 60                         RTS
   409                          
   410                          ; mask a graphic location 
   411                          
   412                          gmask
   413                          gmask_flip
   414  09f2 4900                       EOR #$00
   415                          gmask_op
   416  09f4 11a5                       ORA (gaddr),Y
   417  09f6 91a5                       STA (gaddr),Y
   418  09f8 60                         RTS
   419                          
   420                          }
   421                          
   422                          gromcode_end
   423                          
   424                          ;-----------------------------------------------------------------
   425                          
   426                          position
   427  09f9 a5aa                       LDA y
   428  09fb 4a                         LSR
   429  09fc 4a                         LSR
   430  09fd 4a                         LSR		; y/8
   431  09fe a8                         TAY
   432  09ff 2903                       AND #%00000011	; (y/8) mod 4
   433  0a01 aa                         TAX
   434  0a02 a59b                       LDA xl		; x low
   435  0a04 29f8                       AND #%11111000	; clear bit 2-0
   436  0a06 18                         CLC
   437  0a07 7d1109                     ADC ytabl,X	; addr low: y base + x part
   438  0a0a 85a5                       STA gaddr
   439  0a0c a59c                       LDA xh		; addr high: x part
   440  0a0e 791509                     ADC ytabh,Y	; 	+ y base
   441  0a11 85a6                       STA gaddr+1
   442  0a13 a5aa                       LDA y		; vertical offset
   443  0a15 2907                       AND #%00000111	; y mod 8
   444  0a17 a8                         TAY
   445  0a18 a59b                       LDA xl
   446  0a1a 2907                       AND #%00000111	; x mod 8
   447  0a1c aa                         TAX		; horizonal offset
   448  0a1d 60                         RTS		; (bitmask)
   449                          
   450                          
   451                          ;-----------------------------------------------------------------
   452                          
   453                          ; line y up, x right, dx < dy (case 1)
   454                          
   455                          line_up_steep
   456  0a1e 20f909                     JSR position	; x,y
   457                          loop_yup_xright
   458  0a21 20ed03                     JSR gchange	; pixel
   459                          
   460  0a24 18                         CLC		; k += dx
   461  0a25 a595                       LDA kl
   462  0a27 65ab                       ADC dxl		; dxh is 0, because dx < dy
   463  0a29 8595                       STA kl
   464  0a2b b004                       BCS ++		; k > 255
   465                          
   466  0a2d c5a9                       CMP dy
   467  0a2f 9015                       BCC +		; k >= dy ->
   468                          
   469  0a31 e5a9               ++	SBC dy		; k -= dy
   470  0a33 8595                       STA kl
   471                          
   472  0a35 e8                         INX		; x++
   473  0a36 e008                       CPX #8
   474  0a38 d00c                       BNE +
   475                          	; C=1
   476  0a3a a200                       LDX #0		; x overflow, wrap around
   477  0a3c a5a5                       LDA gaddr	; x+8: gaddr += 8
   478  0a3e 6907                       ADC #8-1	; C already set by CPX
   479  0a40 85a5                       STA gaddr
   480  0a42 9002                       BCC +
   481  0a44 e6a6                       INC gaddr+1
   482                          
   483  0a46 88                 +	DEY		; y--
   484  0a47 100f                       BPL +++
   485  0a49 38                         SEC		; y overflow
   486  0a4a a5a5                       LDA gaddr
   487  0a4c e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   488  0a4e 85a5                       STA gaddr
   489  0a50 a5a6                       LDA gaddr+1
   490  0a52 e901               	SBC #1
   491  0a54 85a6                       STA gaddr+1
   492  0a56 a007                       LDY #7		; wrap around
   493                          
   494  0a58 c6a3               +++	DEC cl		; until c=0
   495  0a5a d0c5                       BNE loop_yup_xright
   496  0a5c 4cda09                     JMP gexit
   497                          
   498                          
   499                          ;-----------------------------------------------------------------
   500                          
   501                          ; line x right, y up, dx > dy (case 2)
   502                          
   503                          line_up_flat
   504  0a5f 20f909                     JSR position	; x,y
   505  0a62 a5a3               	LDA cl		; counter adjustment for
   506  0a64 f002               	BEQ +		; dec-dec-counting
   507  0a66 e6a4               	INC ch
   508                          +
   509                          loop_xright_yup
   510  0a68 20ed03                     JSR gchange	; pixel
   511                          
   512  0a6b 18                         CLC		; k += dy
   513  0a6c a595                       LDA kl
   514  0a6e 65a9                       ADC dy
   515  0a70 8595                       STA kl
   516  0a72 9002                       BCC ++
   517  0a74 e696                       INC kh
   518                          
   519  0a76 c5ab               ++	CMP dxl		; k > dx?
   520  0a78 a596                       LDA kh
   521  0a7a e5a7                       SBC dxh
   522  0a7c 901a                       BCC +
   523                          
   524  0a7e 8596                       STA kh		; k -= dx
   525  0a80 a595                       LDA kl
   526  0a82 e5ab                       SBC dxl
   527  0a84 8595                       STA kl
   528                          
   529  0a86 88                         DEY		; y--
   530  0a87 100f                       BPL +
   531  0a89 38                 	SEC		; C=1 not always true (SBC above)
   532  0a8a a5a5                       LDA gaddr	; y overflow
   533  0a8c e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   534  0a8e 85a5                       STA gaddr
   535  0a90 a5a6                       LDA gaddr+1
   536  0a92 e901               	SBC #1
   537  0a94 85a6                       STA gaddr+1
   538  0a96 a007               	LDY #7		; wrap around
   539                          
   540  0a98 e8                 +	INX		; x++
   541  0a99 e008                       CPX #8		; x overflow?
   542  0a9b d00c                       BNE ++
   543                          	; C=1
   544  0a9d a200                       LDX #0		; wrap around
   545  0a9f a5a5                       LDA gaddr	; x+8: gaddr += 8
   546  0aa1 6907                       ADC #8-1	; C already set by CPX
   547  0aa3 85a5                       STA gaddr
   548  0aa5 9002                       BCC ++
   549  0aa7 e6a6                       INC gaddr+1
   550                          ++
   551  0aa9 c6a3               	DEC cl		; c--
   552  0aab d0bb                       BNE loop_xright_yup
   553  0aad c6a4                       DEC ch		; adjusted high which allows this
   554  0aaf d0b7                       BNE loop_xright_yup
   555                          
   556  0ab1 4cda09                     JMP gexit
   557                          
   558                          
   559                          
   560                          ;-----------------------------------------------------------------
   561                          
   562                          ; line x right, y down, dx > dy (case 3)
   563                          
   564                          line_down_flat
   565  0ab4 20f909                     JSR position	; x,y
   566  0ab7 a5a3               	LDA cl		; counter adjustment for
   567  0ab9 f002               	BEQ +		; dec-dec-counting
   568  0abb e6a4               	INC ch
   569                          +
   570                          loop_xright_ydown
   571  0abd 20ed03                     JSR gchange	; pixel
   572                          
   573  0ac0 18                         CLC		; k += dy
   574  0ac1 a595                       LDA kl
   575  0ac3 65a9                       ADC dy
   576  0ac5 8595                       STA kl
   577  0ac7 9002                       BCC ++
   578  0ac9 e696                       INC kh
   579                          
   580  0acb c5ab               ++	CMP dxl		; k > dx
   581  0acd a596                       LDA kh
   582  0acf e5a7                       SBC dxh		; k -= dx
   583  0ad1 901b                       BCC +
   584                          
   585  0ad3 8596                       STA kh
   586  0ad5 a595                       LDA kl
   587  0ad7 e5ab                       SBC dxl
   588  0ad9 8595                       STA kl
   589                          
   590  0adb c8                         INY		; y++
   591  0adc c008                       CPY #8
   592  0ade d00e                       BNE +
   593                          	; C=1
   594  0ae0 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   595  0ae2 693f                       ADC #$40-1	; C already set by CPY
   596  0ae4 85a5                       STA gaddr
   597  0ae6 a5a6                       LDA gaddr+1
   598  0ae8 6901               	ADC #1
   599  0aea 85a6                       STA gaddr+1
   600  0aec a000                       LDY #0		; wrap around
   601                          
   602  0aee e8                 +	INX		; x++
   603  0aef e008                       CPX #8		; x overflow ?
   604  0af1 d00c                       BNE +++
   605                          	; C=1
   606  0af3 a200                       LDX #$00	; wrap around
   607  0af5 a5a5                       LDA gaddr	; gaddr += 8
   608  0af7 6907                       ADC #$08-1	; C always set by CPX
   609  0af9 85a5                       STA gaddr
   610  0afb 9002                       BCC +++
   611  0afd e6a6                       INC gaddr+1
   612                          +++
   613  0aff c6a3               	DEC cl		; c--
   614  0b01 d0ba                       BNE loop_xright_ydown
   615  0b03 c6a4                       DEC ch		; adjusted high which allows this
   616  0b05 d0b6                       BNE loop_xright_ydown
   617                          
   618  0b07 4cda09                     JMP gexit
   619                          
   620                          
   621                          ;-----------------------------------------------------------------
   622                          
   623                          ; line y down, x right, dx < dy (case 4)
   624                          
   625                          line_down_steep
   626  0b0a 20f909                     JSR position	; x,y
   627                          loop_ydown_xright
   628  0b0d 20ed03                     JSR gchange	; pixel
   629                          
   630  0b10 18                         CLC		; k += dx
   631  0b11 a595                       LDA kl
   632  0b13 65ab                       ADC dxl		; dxh is 0, because dx < dy
   633  0b15 8595                       STA kl
   634  0b17 b004                       BCS ++
   635  0b19 c5a9                       CMP dy		; k > dy?
   636  0b1b 9015                       BCC +
   637  0b1d e5a9               ++	SBC dy		; k -= dy
   638  0b1f 8595                       STA kl
   639                          
   640  0b21 e8                         INX		; x++
   641  0b22 e008                       CPX #8
   642  0b24 d00c                       BNE +		; x overflow?
   643  0b26 a200                       LDX #0		; wrap around
   644  0b28 a5a5                       LDA gaddr	; x+9: gaddr += 8
   645  0b2a 6907                       ADC #8-1	; C already set by CPX
   646  0b2c 85a5                       STA gaddr
   647  0b2e 9002                       BCC +
   648  0b30 e6a6                       INC gaddr+1
   649                          
   650  0b32 c8                 +	INY		; y++
   651  0b33 c008                       CPY #8		; y overflow?
   652  0b35 d00e                       BNE +++
   653  0b37 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   654  0b39 693f                       ADC #$40-1	; C already set by CPY
   655  0b3b 85a5                       STA gaddr
   656  0b3d a5a6                       LDA gaddr+1
   657  0b3f 6901               	ADC #1
   658  0b41 85a6                       STA gaddr+1
   659  0b43 a000                       LDY #0		; wrap around
   660                          
   661  0b45 c6a3               +++	DEC cl		; c--
   662                          			; until c=0
   663  0b47 d0c4                       BNE loop_ydown_xright
   664  0b49 4cda09                     JMP gexit
   665                          
   666                          
   667                          ;-----------------------------------------------------------------
   668                          
   669                          getcommaxy
   670  0b4c 20fdae                     JSR b_getcomma	; check ","
   671                          getxy
   672  0b4f 208aad                     JSR b_getval	; get X coord. value
   673  0b52 20f7b7                     JSR b_convint
   674  0b55 c901                       CMP #>xmax
   675  0b57 9009               	BCC gcxy_xok
   676  0b59 f003                       BEQ +		; X = $1xx
   677                          error_iq
   678  0b5b 20730d                     JSR range_error
   679  0b5e c040               +	CPY #<xmax	; check X low
   680  0b60 b0f9                       BCS error_iq	; X to big
   681                          gcxy_xok
   682  0b62 84fb                       STY gpos	; temporary save X coord.
   683  0b64 85fc                       STA gpos+1
   684                          
   685  0b66 20f1b7                     JSR b_getcomma8bit
   686                          			; get Y coord. value
   687  0b69 e0c8                       CPX #ymax
   688  0b6b b0ee                       BCS error_iq	; Y to big
   689                          
   690  0b6d a4fb                       LDY gpos	; restory X coord.
   691  0b6f a5fc                       LDA gpos+1
   692  0b71 60                         RTS
   693                          
   694                          
   695                          ;-----------------------------------------------------------------
   696                          
   697                          hline
   698  0b72 204f0b                     JSR getxy	; get startpoint
   699  0b75 86aa                       STX y
   700  0b77 8e3603                     STX savey	; save as cursor, too
   701  0b7a 859c                       STA xh
   702  0b7c 849b                       STY xl
   703  0b7e 20fdae                     JSR b_getcomma	; get length
   704  0b81 208aad                     JSR b_getval
   705  0b84 20f7b7                     JSR b_convint
   706                          
   707  0b87 c901                       CMP #>xmax
   708  0b89 9006                       BCC +		; X < 256
   709  0b8b d0ce                       BNE error_iq
   710  0b8d c040                       CPY #<xmax
   711  0b8f b0ca                       BCS error_iq
   712                          +
   713                          			; calculate end point
   714  0b91 aa                         TAX		; save length high byte
   715  0b92 98                         TYA		; length low byte
   716  0b93 18                         CLC
   717  0b94 659b                       ADC xl		; low xend = x+length
   718  0b96 859e                       STA xendl
   719  0b98 a8                 	TAY
   720  0b99 8a                         TXA		; high
   721  0b9a 659c                       ADC xh		; high xend = x+length
   722  0b9c 859f                       STA xendh
   723  0b9e aa                 	TAX
   724                          
   725  0b9f c901               	CMP #>xmax	; endpoint outside?
   726  0ba1 9005               	BCC +
   727  0ba3 98                 	TYA
   728  0ba4 e940               	SBC #<xmax
   729  0ba6 b0b3               	BCS error_iq
   730                          +
   731  0ba8 8e3503                     STX savexh
   732  0bab 8c3403                     STY savexl	; also save as cursor
   733                          
   734  0bae 20e209                     JSR ginit	; map in graphic memory
   735                          
   736                          hline_start
   737  0bb1 a59e                       LDA xendl
   738  0bb3 c59b                       CMP xl
   739  0bb5 a59f                       LDA xendh
   740  0bb7 e59c                       SBC xh
   741  0bb9 b013                       BCS hl_noxswap	; xend < x ->
   742                          
   743  0bbb a69e                       LDX xendl	; swap x, xend
   744  0bbd a59b                       LDA xl
   745  0bbf 869b                       STX xl
   746  0bc1 859e                       STA xendl
   747                          
   748  0bc3 a69f                       LDX xendh
   749  0bc5 a49c                       LDY xh
   750  0bc7 849f                       STY xendh
   751  0bc9 869c                       STX xh
   752  0bcb 4cdd0b                     JMP hl_start	; x != xend
   753                          
   754                          hl_noxswap
   755  0bce a59e                       LDA xendl
   756  0bd0 c59b                       CMP xl
   757  0bd2 d009                       BNE hl_start
   758  0bd4 a59f                       LDA xendh
   759  0bd6 c59c                       CMP xh
   760  0bd8 d003                       BNE hl_start	; x = xend ->
   761  0bda 4c510d             	JMP plot_start	; single point
   762                          ;	JMP gexit	; no point
   763                          
   764                          hl_start
   765  0bdd 20f909                     JSR position	; graphic position x,y
   766  0be0 bd2e09                     LDA maskleft,X
   767  0be3 48                         PHA		; save left end mask
   768  0be4 a59e                       LDA xendl
   769  0be6 2907                       AND #%00000111
   770  0be8 8596                       STA tmp2	; xend mod 8, mask index
   771  0bea a59b                       LDA xl
   772  0bec 29f8                       AND #%11111000	; (xl div 8)*8
   773  0bee 8595                       STA tmp1
   774  0bf0 a59e                       LDA xendl	; xend unmasked
   775  0bf2 38                         SEC
   776  0bf3 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   777  0bf5 8595                       STA tmp1
   778  0bf7 a59f                       LDA xendh
   779  0bf9 e59c                       SBC xh
   780  0bfb 4a                         LSR		; / 8 ->  0-39
   781  0bfc a595                       LDA tmp1	; only 1 highest bit
   782  0bfe 6a                         ROR		; and 3 lower bits
   783  0bff 4a                         LSR
   784  0c00 4a                         LSR
   785  0c01 aa                         TAX		; 8-pixel-blocks count
   786  0c02 68                         PLA		; left end x mask
   787                          
   788                          hl_nextblock
   789  0c03 ca                         DEX
   790                          hl_islastblock
   791  0c04 3012                       BMI hl_lastblock
   792                          			; leave loop if X<0
   793  0c06 20f503                     JSR gmask	; first with left end mask
   794  0c09 18                         CLC		; gaddr += 8
   795  0c0a a5a5                       LDA gaddr
   796  0c0c 6908                       ADC #8
   797  0c0e 85a5                       STA gaddr
   798  0c10 9002                       BCC +
   799  0c12 e6a6                       INC gaddr+1
   800  0c14 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   801  0c16 d0eb               	BNE hl_nextblock	; always
   802                          
   803                          hl_lastblock
   804  0c18 a696                       LDX tmp2	; xend mask index
   805  0c1a 3d3609                     AND maskright,X ; mask right end
   806  0c1d 20f503                     JSR gmask	; modify
   807  0c20 4cda09                     JMP gexit	; leave
   808                          
   809                          
   810                          ;-----------------------------------------------------------------
   811                          
   812                          vline
   813  0c23 204f0b                     JSR getxy	; get startpoint
   814  0c26 859c                       STA xh
   815  0c28 8d3503                     STA savexh	; save as cursor too
   816  0c2b 849b                       STY xl
   817  0c2d 8c3403                     STY savexl
   818  0c30 86aa                       STX y
   819                          
   820  0c32 20f1b7                     JSR b_getcomma8bit
   821                          			; get length
   822  0c35 18                         CLC		; calculate end point
   823  0c36 8a                         TXA		; length
   824                          ; DON'T-CHANGE: how long to go vertically (needed later)
   825                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   826                          ;	STA tmp1
   827  0c37 65aa                       ADC y		; length + y
   828  0c39 c9c8                       CMP #ymax
   829  0c3b 9003                       BCC +
   830                          vline_iq
   831  0c3d 20730d                     JSR range_error
   832  0c40 8593               +	STA yend	; endpoint
   833  0c42 c9c8               	CMP #ymax	; outside?
   834  0c44 b0f7               	BCS vline_iq
   835                          
   836  0c46 8d3603             	STA savey	; set cursor y position
   837                          
   838  0c49 20e209                     JSR ginit	; map in graphic memory
   839                          
   840                          vline_start
   841  0c4c a593                       LDA yend
   842  0c4e c5aa                       CMP y
   843  0c50 b00a                       BCS vl_noyswap	; yend < y ->
   844  0c52 a5aa                       LDA y		; swap y, yend
   845  0c54 a693                       LDX yend
   846  0c56 8593                       STA yend
   847  0c58 86aa                       STX y
   848  0c5a f005               	BEQ vl_start	; always (with next branch)
   849                          	; fall through if yend is
   850                          vl_noyswap
   851  0c5c d003                       BNE vl_start	; y = yend ->
   852  0c5e 4c510d             	JMP plot_start	; single point
   853                          ;	JMP gexit	; no point
   854                          
   855                          vl_start
   856  0c61 20f909                     JSR position	; graphic position x,y
   857  0c64 bd0109                     LDA bitmask,X
   858  0c67 8596                       STA tmp2	; save mask
   859                          ; DON'T-CHANGE: replace ...
   860  0c69 38                         SEC
   861  0c6a a593                       LDA yend
   862  0c6c e5aa                       SBC y		; vertical length
   863  0c6e aa                         TAX
   864                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   865                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   866                          ;	LDX tmp1
   867  0c6f e8                         INX		; +1 (exit on 0)
   868                          vl_nextline
   869  0c70 a596                       LDA tmp2
   870  0c72 20f503                     JSR gmask	; modify 
   871  0c75 c8                         INY		; go down
   872  0c76 c008                       CPY #8		; 8-line wrap
   873  0c78 d00e                       BNE +
   874  0c7a a5a5                       LDA gaddr	; gaddr += 320
   875  0c7c 693f               	ADC #$40-1	; compensate for C = 1
   876  0c7e 85a5                       STA gaddr
   877  0c80 a5a6                       LDA gaddr+1
   878  0c82 6901                       ADC #$01
   879  0c84 85a6                       STA gaddr+1
   880  0c86 a000                       LDY #0		; wrap y offset
   881  0c88 ca                 +	DEX		; all vertical positions done?
   882  0c89 d0e5                       BNE vl_nextline
   883  0c8b 4cda09                     JMP gexit	; leave
   884                          
   885                          
   886                          ;-----------------------------------------------------------------
   887                          
   888                          line
   889  0c8e 204f0b                     JSR getxy	; get startpoint
   890  0c91 849b                       STY xl 
   891  0c93 859c                       STA xh
   892  0c95 86aa                       STX y
   893                          
   894  0c97 204c0b                     JSR getcommaxy	; get endpoint
   895                          line_start
   896  0c9a 8c3403                     STY savexl	; save as cursor position too
   897  0c9d 849e                       STY xendl
   898  0c9f 8d3503                     STA savexh
   899  0ca2 859f                       STA xendh
   900  0ca4 8e3603                     STX savey
   901  0ca7 8693                       STX yend
   902                          
   903  0ca9 20e209                     JSR ginit	; map in graphic memory
   904                          
   905  0cac a000                       LDY #$00	; initialize to 0
   906  0cae 84a8                       STY ydir
   907  0cb0 8495                       STY kl
   908  0cb2 8496                       STY kh
   909                          
   910  0cb4 38                         SEC
   911  0cb5 a59e                       LDA xendl	; calculate dx
   912  0cb7 e59b                       SBC xl
   913  0cb9 85ab                       STA dxl
   914  0cbb a59f                       LDA xendh
   915  0cbd e59c                       SBC xh
   916  0cbf 85a7                       STA dxh
   917                          
   918  0cc1 b025                       BCS li_xend_right
   919                          	; dx != 0
   920  0cc3 98                         TYA		; negate dx
   921  0cc4 38                         SEC		; dx = 0 - dx
   922  0cc5 e5ab                       SBC dxl
   923  0cc7 85ab                       STA dxl
   924  0cc9 98                         TYA
   925  0cca e5a7                       SBC dxh
   926  0ccc 85a7                       STA dxh
   927                          			; C=0 always, needed later
   928  0cce a69b                       LDX xl		; swap x low
   929  0cd0 a49e                       LDY xendl
   930  0cd2 869e                       STX xendl
   931  0cd4 849b                       STY xl
   932                          
   933  0cd6 a69c                       LDX xh		; swap x high
   934  0cd8 a49f                       LDY xendh
   935  0cda 869f                       STX xendh
   936  0cdc 849c                       STY xh
   937                          
   938  0cde a6aa                       LDX y		; swap y
   939  0ce0 a493                       LDY yend
   940  0ce2 8693                       STX yend
   941  0ce4 84aa                       STY y
   942                          
   943  0ce6 9009                       BCC li_x_different
   944                          			; C=0 always (from negation before)
   945                          
   946                          li_xend_right
   947  0ce8 a5ab                       LDA dxl		; dx = 0?
   948  0cea 05a7                       ORA dxh
   949  0cec d003                       BNE li_x_different
   950  0cee 4c4c0c                     JMP vline_start	; vertical line case
   951                          
   952                          li_x_different
   953  0cf1 38                         SEC		; calculate dy
   954  0cf2 a593                       LDA yend
   955  0cf4 e5aa                       SBC y
   956  0cf6 b006                       BCS li_y_right
   957  0cf8 49ff                       EOR #$FF	; negate dy (two's complement)
   958  0cfa 6901                       ADC #$01	; C=0
   959  0cfc 85a8                       STA ydir	; flag y goes up
   960                          
   961                          li_y_right
   962  0cfe 85a9                       STA dy
   963  0d00 d003                       BNE +
   964  0d02 4cb10b                     JMP hline_start	; horizontal line case
   965                          +
   966                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   967                          
   968  0d05 a5a7                       LDA dxh		; dx > dy
   969  0d07 d017                       BNE line_flat	; yes -> flat
   970  0d09 a5a9                       LDA dy		; no -> steep
   971  0d0b aa                         TAX
   972  0d0c c5ab                       CMP dxl
   973  0d0e 9010                       BCC line_flat
   974                          
   975                          line_steep
   976  0d10 e8                         INX	
   977  0d11 86a3                       STX cl		; c = dy+1
   978  0d13 4a                         LSR		; k = dy/2
   979  0d14 8595                       STA kl
   980  0d16 a5a8                       LDA ydir
   981  0d18 d003                       BNE +
   982  0d1a 4c0a0b                     JMP line_down_steep	; y down, steep
   983  0d1d 4c1e0a             +	JMP line_up_steep	; y up, steep
   984                          
   985                          line_flat
   986  0d20 a5a7                       LDA dxh
   987  0d22 a8                         TAY
   988  0d23 a6ab                       LDX dxl
   989  0d25 e8                         INX
   990  0d26 d001                       BNE +
   991  0d28 c8                         INY
   992  0d29 86a3               +	STX cl		; c = dx+1
   993  0d2b 84a4                       STY ch
   994                          
   995  0d2d 4a                         LSR		; k = dx/2
   996  0d2e 8596                       STA kh
   997  0d30 a5ab                       LDA dxl
   998  0d32 6a                         ROR		; dx/2
   999  0d33 8595                       STA kl
  1000  0d35 a5a8                       LDA ydir	
  1001  0d37 d003                       BNE +
  1002  0d39 4cb40a                     JMP line_down_flat	; y down, flat
  1003  0d3c 4c5f0a             +	JMP line_up_flat	; y up, flat
  1004                          
  1005                          ;-----------------------------------------------------------------
  1006                          
  1007                          plot
  1008  0d3f 204f0b                     JSR getxy	; get parameter
  1009  0d42 859c                       STA xh		; save x/y
  1010  0d44 849b                       STY xl
  1011  0d46 86aa                       STX y
  1012  0d48 8d3503                     STA savexh	; and store as cursor
  1013  0d4b 8c3403                     STY savexl
  1014  0d4e 8e3603                     STX savey
  1015                          
  1016                          plot_start
  1017  0d51 20f909                     JSR position	; calculate graphical address
  1018                          
  1019  0d54 a501                       LDA prozport
  1020  0d56 29fd                       AND #%11111101	; Kernal ROM disable
  1021  0d58 78                         SEI			
  1022  0d59 8501                       STA prozport
  1023                          
  1024  0d5b 20ed03                     JSR gchange	; change graphical data
  1025                          
  1026  0d5e a501                       LDA prozport
  1027  0d60 0902                       ORA #%00000010	; kernal ROM enable
  1028  0d62 8501                       STA prozport
  1029  0d64 58                         CLI
  1030  0d65 60                         RTS
  1031                          
  1032                          ;-----------------------------------------------------------------
  1033                          
  1034                          move
  1035  0d66 204f0b                     JSR getxy	; get parameter
  1036  0d69 8d3503                     STA savexh	; just save as cursor
  1037  0d6c 8c3403                     STY savexl
  1038  0d6f 8e3603                     STX savey
  1039  0d72 60                         RTS
  1040                          
  1041                          
  1042                          ;-----------------------------------------------------------------
  1043                          
  1044                          range_error
  1045  0d73 ad3703             	LDA savemo
  1046  0d76 29f0               	AND #$F0
  1047  0d78 d003               	BNE +
  1048  0d7a 68                 	PLA			; cleanup JSR
  1049  0d7b 68                 	PLA
  1050  0d7c 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1051                          				; error mode 2: cut value: control back
  1052                          				; to handle value correction
  1053  0d7d 2920               +	AND #$20
  1054  0d7f d0fb               	BNE -
  1055  0d81 68                 	PLA			; cleanup JSR
  1056  0d82 68                 	PLA
  1057                          setmode_error
  1058  0d83 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1059                          
  1060                          ;-----------------------------------------------------------------
  1061                          
  1062                          setmode
  1063  0d86 209eb7                     JSR b_get8bit
  1064  0d89 e003                       CPX #3
  1065  0d8b 9012                       BCC +			; less then 3, modification mode
  1066  0d8d e006               	CPX #6
  1067  0d8f b0f2               	BCS setmode_error	; out of range
  1068                          				; error mode
  1069  0d91 690d               	ADC #13			; C=0, therefore -3
  1070                          				; 3-5 -> 16-18
  1071                          				; put A's bit 4-7 into savemo
  1072  0d93 4d3703             	EOR savemo		; ********
  1073  0d96 29f0               	AND #$F0		; ****0000
  1074  0d98 4d3703             	EOR savemo		; AAAAmmmm
  1075  0d9b 8d3703             	STA savemo		; 
  1076  0d9e 60                 	RTS
  1077                          
  1078  0d9f 8a                 +	TXA
  1079  0da0 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1080  0da3 290f               	AND #$0F
  1081  0da5 4d3703             	EOR savemo
  1082  0da8 8d3703             	STA savemo
  1083                          setmode_enter
  1084  0dab e001               	CPX #$01
  1085  0dad b01a                       BCS set_or_toggle
  1086                          
  1087                          modereset
  1088  0daf a909                       LDA #>(nbitmask)
  1089  0db1 8df103                     STA gchange_op+2
  1090  0db4 a909                       LDA #<(nbitmask)
  1091  0db6 8df003                     STA gchange_op+1
  1092  0db9 a93d                       LDA #$3D		; AND abs,X
  1093  0dbb 8def03                     STA gchange_op
  1094  0dbe a931                       LDA #$31		; AND (zp),Y
  1095  0dc0 8df703                     STA gmask_op
  1096  0dc3 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1097  0dc5 8df603                     STA gmask_flip+1
  1098  0dc8 60                         RTS
  1099                          
  1100                          set_or_toggle
  1101  0dc9 d01a                       BNE modetoggle
  1102                          modeset
  1103  0dcb a909                       LDA #>(bitmask)
  1104  0dcd 8df103                     STA gchange_op+2
  1105  0dd0 a901                       LDA #<(bitmask)
  1106  0dd2 8df003                     STA gchange_op+1
  1107  0dd5 a91d                       LDA #$1D		; OR abs,X
  1108  0dd7 8def03                     STA gchange_op
  1109  0dda a911                       LDA #$11		; OR (zp),Y
  1110  0ddc 8df703                     STA gmask_op
  1111  0ddf a900                       LDA #$00		; EOR #$00, nicht invertieren
  1112  0de1 8df603                     STA gmask_flip+1
  1113  0de4 60                         RTS
  1114                          
  1115                          modetoggle
  1116  0de5 a909                       LDA #>(bitmask)
  1117  0de7 8df103                     STA gchange_op+2
  1118  0dea a901                       LDA #<(bitmask)
  1119  0dec 8df003                     STA gchange_op+1
  1120  0def a95d                       LDA #$5D		; EOR abs,X
  1121  0df1 8def03                     STA gchange_op
  1122  0df4 a951                       LDA #$51		; EOR (zp),Y
  1123  0df6 8df703                     STA gmask_op
  1124  0df9 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1125  0dfb 8df603                     STA gmask_flip+1
  1126  0dfe 60                         RTS
  1127                          
  1128                          
  1129                          ;-----------------------------------------------------------------
  1130                          
  1131                          ; get pixel (check if pixel set)
  1132                          ; not used
  1133                          
  1134                          get
  1135  0dff 204c0b                     JSR getcommaxy
  1136  0e02 859c                       STA xh
  1137  0e04 849b                       STY xl
  1138  0e06 86aa                       STX y
  1139                          
  1140  0e08 20f909                     JSR position
  1141                          
  1142  0e0b a501                       LDA prozport
  1143  0e0d 29fd               	AND #%11111101	; Kernal ROM disable
  1144  0e0f 78                         SEI
  1145  0e10 8501                       STA prozport
  1146                          
  1147  0e12 b1a5                       LDA (gaddr),Y
  1148  0e14 3d0109                     AND bitmask,X
  1149  0e17 a8                         TAY
  1150  0e18 a501                       LDA prozport
  1151  0e1a 0902               	ORA #%00000010	; kernal ROM enable
  1152  0e1c 8501                       STA prozport
  1153  0e1e 58                         CLI
  1154  0e1f 4ca2b3                     JMP b_byte2fac
  1155                          
  1156                          
  1157                          ;-----------------------------------------------------------------
  1158                          
  1159                          relto
  1160  0e22 208aad                     JSR b_getval	; get X offset (+/-)
  1161  0e25 a561               	LDA facexp	; FAC exponent
  1162  0e27 c990               	CMP #$90	; more than 16 bit
  1163  0e29 b031               	BCS relto_error	; illegal quantity
  1164  0e2b 209bbc                     JSR b_fac2int	; to signed integer
  1165                          
  1166  0e2e 18                         CLC
  1167  0e2f a565                       LDA facintl
  1168  0e31 6d3403                     ADC savexl
  1169  0e34 859e                       STA xendl
  1170  0e36 a564                       LDA facinth
  1171  0e38 6d3503                     ADC savexh
  1172  0e3b 859f                       STA xendh	; xend = savex+facint
  1173                          
  1174  0e3d 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1175  0e40 208aad                     JSR b_getval
  1176  0e43 a561                       LDA facexp	; FAC exponent
  1177  0e45 c990                       CMP #$90	; more than 16 bit
  1178  0e47 b013                       BCS relto_error	; illegal quantity
  1179  0e49 209bbc                     JSR b_fac2int	; to signed integer
  1180  0e4c 18                         CLC
  1181  0e4d a565                       LDA facintl
  1182  0e4f 6d3603                     ADC savey
  1183  0e52 8593                       STA yend	; yend = savey+facint
  1184                          
  1185  0e54 a59f                       LDA xendh	; check end coord. x
  1186  0e56 c901                       CMP #>xmax
  1187  0e58 900b                       BCC rt_xok
  1188  0e5a f003                       BEQ +
  1189                          relto_error
  1190  0e5c 20730d                     JSR range_error
  1191  0e5f a59e               +	LDA xendl
  1192  0e61 c940                       CMP #<xmax
  1193  0e63 b0f7                       BCS relto_error
  1194                          rt_xok
  1195  0e65 a593                       LDA yend	; check end coord. y
  1196  0e67 c9c8                       CMP #ymax
  1197  0e69 b0f1                       BCS relto_error
  1198                          
  1199  0e6b ad3403                     LDA savexl
  1200  0e6e 859b                       STA xl
  1201  0e70 ad3503                     LDA savexh
  1202  0e73 859c                       STA xh
  1203  0e75 ad3603                     LDA savey
  1204  0e78 85aa                       STA y
  1205  0e7a a49e                       LDY xendl
  1206  0e7c a59f                       LDA xendh
  1207  0e7e a693                       LDX yend	; xend/yend = cursor + x/y
  1208                          
  1209  0e80 4c9a0c                     JMP line_start	; draw line x/y to xend/yend
  1210                          
  1211                          
  1212                          ;-----------------------------------------------------------------
  1213                          
  1214                          char
  1215  0e83 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1216  0e86 e028                       CPX #40	
  1217  0e88 9003                       BCC +
  1218                          char_error
  1219  0e8a 4c48b2                     JMP b_illquant
  1220  0e8d 86fb               +	STX gpos	; save x coord.
  1221  0e8f 20f1b7                     JSR b_getcomma8bit
  1222                          			; get char. position y 0-24
  1223  0e92 e019                       CPX #25
  1224  0e94 b0f4                       BCS char_error
  1225  0e96 86fc                       STX gpos+1	; save y coord.
  1226                          
  1227  0e98 20fdae                     JSR b_getcomma	; get string
  1228  0e9b 209ead                     JSR b_getexpr
  1229  0e9e 20a3b6                     JSR b_stringval ; string address in str
  1230  0ea1 48                         PHA		; string length
  1231  0ea2 a6fc                       LDX gpos+1	; y coord. for char. position
  1232  0ea4 8a                         TXA
  1233  0ea5 2903                       AND #$03	; mask 2 bits
  1234  0ea7 a8                         TAY		; table index
  1235  0ea8 a900                       LDA #$00
  1236  0eaa 85fc                       STA gpos+1	; x high
  1237  0eac a5fb                       LDA gpos	; saved x: multiply by 8
  1238  0eae 0a                         ASL
  1239  0eaf 0a                         ASL
  1240  0eb0 0a                         ASL
  1241  0eb1 26fc                       ROL gpos+1	; overflow to high byte
  1242  0eb3 791109                     ADC ytabl,Y
  1243  0eb6 85a5                       STA gaddr
  1244  0eb8 a5fc                       LDA gpos+1	; x high
  1245  0eba 7d1509                     ADC ytabh,X
  1246  0ebd 85a6                       STA gaddr+1
  1247  0ebf 68                         PLA		; string length
  1248  0ec0 a000                       LDY #$00	; string index
  1249  0ec2 aa                         TAX		; length
  1250  0ec3 e8                         INX		; prepare as counter
  1251                          char_loop
  1252  0ec4 ca                         DEX
  1253  0ec5 f008                       BEQ char_exit
  1254  0ec7 b122                       LDA (str),Y	; read string
  1255  0ec9 20d00e                     JSR char_display
  1256  0ecc c8                         INY
  1257  0ecd d0f5                       BNE char_loop
  1258                          char_exit
  1259  0ecf 60                         RTS
  1260                          
  1261                          char_display
  1262  0ed0 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1263  0ed2 8a                         TXA		; save register X+Y
  1264  0ed3 48                         PHA
  1265  0ed4 98                         TYA
  1266  0ed5 48                         PHA
  1267  0ed6 a5d7                       LDA z_tmp	; get saved character
  1268  0ed8 3012                       BMI char_inverse
  1269                          
  1270                          char_normal
  1271  0eda c920                       CMP #$20	; control character?
  1272  0edc 9054                       BCC char_disp_leave
  1273  0ede c960                       CMP #$60
  1274  0ee0 9004                       BCC +
  1275  0ee2 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1276  0ee4 d014                       BNE char_hires
  1277  0ee6 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1278  0ee8 d010               	BNE char_hires
  1279  0eea f00e               	BEQ char_hires
  1280                          
  1281                          char_inverse
  1282  0eec 297f                       AND #%01111111	; mask bit 7
  1283  0eee c97f                       CMP #%01111111	; was 255? (pi)
  1284  0ef0 d002                       BNE +
  1285  0ef2 a95e                       LDA #$5E	; screen code for pi
  1286  0ef4 c920               +	CMP #$20	; control character?
  1287  0ef6 903a                       BCC char_disp_leave
  1288                          			; yes, skip
  1289  0ef8 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1290                          			; $C0-$FF -> $40-$7F
  1291                          			; OPT: BNE char_hires
  1292                          			; OPT: char_normal
  1293                          char_hires
  1294  0efa a6c7                       LDX z_reverseflag
  1295  0efc f002                       BEQ +
  1296  0efe 0980                       ORA #%10000000	; invert char.
  1297  0f00 aa                 +	TAX		; save char. for later
  1298  0f01 a501                       LDA prozport	; save prozport state
  1299  0f03 48                 	PHA
  1300  0f04 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1301  0f06 78                         SEI
  1302  0f07 8501                       STA prozport	; char. rom base = $D000
  1303  0f09 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1304  0f0b 85fc                       STA gpos+1	; 
  1305  0f0d 8a                         TXA		; char. code
  1306  0f0e 0a                         ASL		; *8
  1307  0f0f 26fc                       ROL gpos+1
  1308  0f11 0a                         ASL
  1309  0f12 26fc                       ROL gpos+1
  1310  0f14 0a                         ASL
  1311  0f15 26fc                       ROL gpos+1
  1312  0f17 85fb                       STA gpos	; addr. in char. rom for char.
  1313                          
  1314  0f19 a007                       LDY #$07	; 8 hires lines
  1315                          char_line
  1316  0f1b b1fb                       LDA (gpos),Y	; read character line
  1317  0f1d 20f503                     JSR gmask	; write to hires screen
  1318  0f20 88                         DEY
  1319  0f21 10f8                       BPL char_line
  1320                          
  1321  0f23 68                 	PLA
  1322  0f24 8501                       STA prozport
  1323  0f26 58                         CLI
  1324                          
  1325  0f27 18                         CLC		; step char position to left
  1326  0f28 a5a5                       LDA gaddr	; ( +8 )
  1327  0f2a 6908                       ADC #$08
  1328  0f2c 85a5                       STA gaddr
  1329  0f2e 9002                       BCC +
  1330  0f30 e6a6                       INC gaddr+1
  1331                          +
  1332                          char_disp_leave
  1333  0f32 68                 	PLA		; pass written character back
  1334  0f33 a8                         TAY		; restore saved registers
  1335  0f34 68                         PLA
  1336  0f35 aa                         TAX
  1337  0f36 60                         RTS
  1338                          
  1339                          
  1340                          ;-----------------------------------------------------------------
  1341                          
  1342                          to
  1343  0f37 ad3403                     LDA savexl
  1344  0f3a 859b                       STA xl
  1345  0f3c ad3503                     LDA savexh
  1346  0f3f 859c                       STA xh
  1347  0f41 ad3603                     LDA savey
  1348  0f44 85aa                       STA y
  1349  0f46 204f0b                     JSR getxy
  1350  0f49 4c9a0c                     JMP line_start
  1351                          
  1352                          ;-----------------------------------------------------------------
  1353                          
  1354                          unnew
  1355                          
  1356  0f4c a52b               	lda bassta
  1357  0f4e 8522               	sta str
  1358  0f50 a52c               	lda bassta+1
  1359  0f52 8523               	sta str+1
  1360  0f54 a001               	ldy #1
  1361  0f56 98                 	tya
  1362  0f57 9122               	sta (str),y		; != 0
  1363                          
  1364  0f59 2033a5             	jsr b_rechain		; starting from bassta
  1365                          				; result in (str)
  1366  0f5c 18                 	clc			; str+1 -> new basic end
  1367  0f5d a423               	ldy str+1
  1368  0f5f a522               	lda str
  1369  0f61 6902               	adc #2
  1370  0f63 852d               	sta basend
  1371  0f65 9001               	bcc +
  1372  0f67 c8                 	iny
  1373  0f68 842e               +	sty basend+1
  1374  0f6a 4c60a6             	jmp b_clr		; perform CLR
  1375                          
  1376                          ;-----------------------------------------------------------------
  1377                          graext_end

; ******** Source: ge-run.asm
    43                          
    44                          
