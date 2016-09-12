
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
    29  080d a246               	ldx #<graext_end	; setup basic
    30  080f a00f               	ldy #>graext_end
    31  0811 18                 	clc			; set if C=0
    32  0812 209cff             	jsr $ff9c		; KERNAL: system RAM bottom
    33  0815 862b               	stx $2b			; BASIC text start
    34  0817 842c               	sty $2c
    35  0819 2016e4             	jsr $e416		; setup BASIC text start
    36  081c 202908             	jsr init		; init extension (place hook)
    37  081f a9d4               	lda #<author		; message ...
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
     6                          ;	2016-05-20 v 1.22
     7                          ;	2016-05-16 v 1.21
     8                          ;	2016-02-23 v 1.20
     9                          ;	2016-01-15 v 1.19
    10                          ;	1992-12-28 v 1.18
    11                          ;	1986-03-24 v 1.17
    12                          ;	1985       v 0.00 - 1.16
    13                          ;
    14                          ; the original source has been lost.
    15                          ; development has based on the implemention
    16                          ; done on a forth-64 written with its forth assembler.
    17                          ; the code has been pulled out from there and enriched
    18                          ; with some glue code to get a basic extension.
    19                          
    20                          ; command dispatcher style JMP/RTS
    21                          ;	(if defined)
    22                          ;command_rts_style=1
    23                          
    24                          ; error handling 
    25                          ;	(if defined)
    26                          ;no_error=1
    27                          
    28                          ; basic interpreter registers, addresses and entry points
    29                          
    30                          str     = $22		; string address
    31                          ijmp    = $55		; address of JMP (addr)
    32                          chrget  = $73		; basic charget routine
    33                          facintl = $65		; integer result from b_fac2int
    34                          facinth = $64
    35                          facexp  = $61		; fac exponent, after b_getval
    36                          
    37                          z_reverseflag = $C7	; character routine
    38                          z_lastkey = $D7		; original use case, unused here
    39                          z_tmp = z_lastkey	; temporary reused for character routine
    40                          
    41                          v_baserr = $0300	; vector error routine
    42                          v_basstp = $0328	; vector error routine
    43                          v_bascmd = $0308	; vector interpreter parsing
    44                          v_basexp = $030a	; vector evaluate expression
    45                          
    46                          basic_rom = $A000	; start of BASIC ROM
    47                          
    48                          b_interpreter =$A7AE	; interpreter loop
    49                          b_execstatement =$A7E7	; process statement
    50                          b_getcomma = $AEFD	; read comma from basic text
    51                          b_illquant = $B248	; error "illegal quantity"
    52                          b_syntaxerror = $AF08	; error "syntax"
    53                          b_get8bit = $B79E	; read 8 bit numeric value from
    54                          			; basic text
    55                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    56                          			; from basic text
    57                          b_getval = $AD8A	; read numeric value from basic text
    58                          b_getexpr = $AD9E	; read expression from basic text
    59                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    60                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    61                          b_fac2int = $BC9B	; convert FAC to integer
    62                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    63                          
    64                          ; hardware registers and values
    65                          
    66                          prozport = $01		; processor port
    67                          memrom = %00110111	; basic+kernal rom
    68                          membas = %00110110	; basic ram+kernal rom
    69                          memram = %00110101	; basic+kernal ram
    70                          
    71                          vic_cr	= $D011		; VIC control register
    72                          vic_mcr	= $D018		; VIC memory control register
    73                          cia_pra	= $DD00		; CIA 2 port register A
    74                          
    75                          cram	= $CC00		; start of color ram
    76                          
    77                          gram	= $e000		; start of graphic bitmap ram
    78                          gramp	= gram >> 8	; start page of bitmap
    79                          
    80                          ; constants 
    81                          
    82                          xmax	= 320		; max x dimension
    83                          ymax	= 200		; max y dimension
    84                          
    85                          ; zeropage variables
    86                          
    87                          x	= $9B		; start coordinate x, low+high
    88                          xl	= x
    89                          xh	= x+1
    90                          y	= $AA		; start coordinate y
    91                          
    92                          xendl	= $9E		; end coordinate x, low+high
    93                          xendh	= $9F
    94                          yend	= $93		; end coordinate y
    95                          
    96                          kl	= $95		; gradient for lines, low+high
    97                          kh	= kl+1
    98                          
    99                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
   100                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
   101                          
   102                          dxl	= $AB		; x delta, low+high
   103                          dxh	= $A7
   104                          
   105                          dy	= $A9		; y delta
   106                          
   107                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   108                          
   109                          cl	= $A3		; dot count, low+high
   110                          ch	= $A4
   111                          
   112                          gaddr	= $A5		; graphic address
   113                          
   114                          gpos	= $FB		; in graphic position
   115                          
   116                          gcol	= $FD		; graphic color, in "graphic on" context only
   117                          
   118                          
   119                          ; static ram areas
   120                          
   121                          savexl	= $0334		; the graphic cursor: x low 
   122                          savexh	= savexl+1	; the graphic cursor: x high
   123                          savey	= savexh+1	; the graphic cursor: y
   124                          savemo	= savey+1	; the graphic mode
   125                          saveverr = savemo+1	; original v_baserr
   126                          savevstp = saveverr+2	; original v_basstp
   127                          
   128                          gramcode = $03ed	; real place for gchange and gmask routines,
   129                          			; they take 15 bytes
   130                          
   131                          ;
   132                          ; initialize extension
   133                          
   134                          init
   135  0829 a981                       LDA #<(parse)	; basic interpreter parser hook
   136  082b 8d0803                     STA v_bascmd
   137  082e a908                       LDA #>(parse)
   138  0830 8d0903                     STA v_bascmd+1
   139                          
   140  0833 ad2803                     LDA v_basstp
   141  0836 8d3a03             	STA savevstp
   142  0839 a975                       LDA #<(stop)	; basic interpreter stop hook
   143  083b 8d2803                     STA v_basstp
   144  083e ad2903                     LDA v_basstp+1
   145  0841 8d3b03             	STA savevstp+1
   146  0844 a908                       LDA #>(stop)
   147  0846 8d2903                     STA v_basstp+1
   148                          
   149  0849 ad0003                     LDA v_baserr
   150  084c 8d3803             	STA saveverr
   151  084f a96f                       LDA #<(error)	; basic interpreter error hook
   152  0851 8d0003                     STA v_baserr
   153  0854 ad0103                     LDA v_baserr+1
   154  0857 8d3903             	STA saveverr+1
   155  085a a908                       LDA #>(error)
   156  085c 8d0103                     STA v_baserr+1
   157                          
   158  085f a200               	LDX #0		; set graphic cursor to (0,0)
   159  0861 8e3403             	STX savexl
   160  0864 8e3503             	STX savexh
   161  0867 8e3603             	STX savey
   162  086a e8                 	INX
   163  086b 8e3703             	STX savemo	; set mode 1
   164  086e 60                         RTS
   165                          
   166                          error	
   167                          	; reg A may destroyed
   168  086f 204209             	JSR gra_off		; uses only reg A
   169  0872 6c3803             	JMP (saveverr)		; to original vector
   170                          
   171                          stop	
   172                          	; reg A may destroyed
   173  0875 a591               	LDA $91			; Scan code
   174  0877 c97f               	CMP #$7F		; STOP key?
   175  0879 d003               	BNE nostop
   176  087b 204209             	JSR gra_off		; uses only reg A
   177                          nostop
   178  087e 6c3a03             	JMP (savevstp)		; to original vector
   179                          
   180                          ;-----------------------------------------------------------------
   181                          
   182                          ; start parsing an extension command ...
   183                          
   184                          parse
   185  0881 207300                     JSR chrget			; next char.
   186  0884 08                 	PHP
   187  0885 c926                       CMP #'&'			; command prefix
   188  0887 f004                       BEQ newcmd
   189  0889 28                         PLP
   190  088a 4ce7a7                     JMP b_execstatement
   191                          newcmd
   192  088d 28                 	PLP
   193  088e 207300                     JSR chrget			; command character
   194                          
   195  0891 a00b                       LDY #(cmdsend-cmds)		; map character to
   196                          					; command address ...
   197                          checknextcmd
   198  0893 88                         DEY
   199  0894 f01c               	BEQ parse_error
   200  0896 d9b508                     CMP cmds,Y
   201  0899 d0f8                       BNE checknextcmd		; try next
   202  089b 88                         DEY				; found
   203  089c 98                         TYA
   204  089d 0a                         ASL				; *2
   205  089e a8                         TAY
   206                          !ifndef command_rts_tyle {
   207                          	!set co=0			; command offset in jump table
   208  089f b9c108                     LDA cmdaddr+1,Y                 ; high byte from table
   209  08a2 8556                       STA ijmp+1
   210  08a4 b9c008                     LDA cmdaddr,Y                   ; low byte from table
   211  08a7 8555                       STA ijmp
   212  08a9 207300                     JSR chrget			; read next byte in basic text
   213  08ac 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   214  08af 4caea7                     JMP b_interpreter		; continue parsing
   215                          } else {
   216                          	!set co=1			; command offset in jump table
   217                          	LDA #>(b_interpreter-1)		; return to interpreter
   218                          	PHA
   219                          	LDA #<(b_interpreter-1)
   220                          	PHA				
   221                                  LDA cmdaddr+1,Y			; command address (RTS style)
   222                                  PHA				; high byte on stack
   223                                  LDA cmdaddr,Y			; command address (RTS style)
   224                                  PHA				; low byte on stack
   225                                  JMP chrget			; read next byte in basic text 
   226                          					; and RTS to command
   227                          }
   228                          parse_error
   229  08b2 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   230                          
   231                          ;-----------------------------------------------------------------
   232                          
   233                          ; the most commonly used command placed at the end ...
   234                          
   235  08b5 204743534d525456...cmds	!text " GCSMRTVHLP"		; first char. is a dummy
   236                          cmdsend
   237                          
   238                          cmdaddr
   239  08c0 3b09800e830d630d...        !word graphic-co,char-co,setmode-co,move-co,relto-co
   240  08ca 310f200c6f0b8b0c...        !word to-co,vline-co,hline-co,line-co,plot-co
   241                          
   242  08d4 934752412d455854...author	!text 147,"GRA-EXT V1.22 1986,2016 JOHANN@KLASEK.AT",0
   243                          
   244                          bitmask
   245  08fe 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   246                          nbitmask
   247  0906 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   248                          ytabl
   249  090e 004080c0           	!byte $00,$40,$80,$c0
   250                          ytabh
   251  0912 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   252  0916 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   253  091a eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   254  091e eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   255  0922 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   256  0926 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   257  092a fe                 	!byte gramp+$1e
   258                          
   259                          ; for horiz. line
   260                          
   261  092b ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   262                          
   263  0933 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   264                          
   265                          
   266                          ;-----------------------------------------------------------------
   267                          
   268                          graphic
   269  093b 209eb7                     JSR b_get8bit
   270  093e e000                       CPX #$00
   271  0940 d013                       BNE gra_other
   272                          gra0			; &G 0
   273                          gra_off
   274  0942 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   275  0944 8d00dd                     STA cia_pra
   276  0947 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   277                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   278                          			; char addr $1000/4096 = char. ROM
   279  0949 8d18d0                     STA vic_mcr	; VIC memory control
   280  094c ad11d0                     LDA vic_cr	; VIC control register
   281  094f 29df                       AND #%11011111	; Hires mode off
   282  0951 8d11d0                     STA vic_cr
   283  0954 60                         RTS
   284                          
   285                          gra_other
   286  0955 e001                       CPX #$01
   287  0957 f00f               	BEQ gra1
   288  0959 e002               	CPX #$02
   289  095b f00e                       BEQ gra2
   290  095d e004               	CPX #$04
   291  095f f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   292  0961 e003               	CPX #$03	; &G 3 (graphic on)
   293  0963 f029               	BEQ gra_on
   294  0965 4c48b2                     JMP b_illquant	; parameter illegal
   295                          	
   296                          gra1			; &G 1
   297  0968 20a409             	JSR gra_clear
   298                          
   299                          gra2
   300  096b 20f1b7                     JSR b_getcomma8bit
   301  096e 8a                         TXA		; foreground color
   302  096f 0a                         ASL		; upper nibble
   303  0970 0a                         ASL
   304  0971 0a                         ASL
   305  0972 0a                         ASL
   306  0973 85fd                       STA gcol
   307  0975 20f1b7                     JSR b_getcomma8bit
   308  0978 8a                         TXA		; background color
   309  0979 290f                       AND #$0F
   310  097b 05fd                       ORA gcol
   311  097d a000                       LDY #$00
   312                          cram_loop
   313  097f 9900cc                     STA cram,Y	; fill color RAM
   314  0982 9900cd                     STA cram+$100,Y
   315  0985 9900ce                     STA cram+$200,Y
   316  0988 99e8ce                     STA cram+$300-24,Y
   317  098b c8                         INY
   318  098c d0f1                       BNE cram_loop
   319                          
   320                          gra_on
   321  098e 20c309             	JSR gra_setupcode
   322                          
   323  0991 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   324  0993 8d00dd                     STA cia_pra
   325  0996 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   326  0998 8d18d0                     STA vic_mcr	; VIC memory control
   327  099b ad11d0                     LDA vic_cr	; VIC control register
   328  099e 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   329  09a0 8d11d0                     STA vic_cr
   330  09a3 60                         RTS
   331                          
   332                          gra_clear
   333  09a4 a220                       LDX #$20	; Pages (8 KByte)
   334  09a6 a9e0                       LDA #>gram
   335  09a8 85fc                       STA gpos+1
   336  09aa a000                       LDY #$00
   337  09ac 84fb                       STY gpos
   338  09ae 98                         TYA
   339                          gra_fill
   340  09af 91fb                       STA (gpos),Y	; Loop unroll
   341  09b1 c8                         INY
   342  09b2 91fb                       STA (gpos),Y
   343  09b4 c8                         INY
   344  09b5 91fb                       STA (gpos),Y
   345  09b7 c8                         INY
   346  09b8 91fb                       STA (gpos),Y
   347  09ba c8                         INY
   348  09bb d0f2                       BNE gra_fill
   349  09bd e6fc                       INC gpos+1
   350  09bf ca                         DEX
   351  09c0 d0ed                       BNE gra_fill
   352  09c2 60                 	RTS
   353                          
   354                          gra_setupcode
   355  09c3 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   356                          gra_copycode
   357  09c5 bde609             	LDA gromcode-1,X
   358  09c8 9dec03             	STA gramcode-1,X
   359  09cb ca                 	DEX
   360  09cc d0f7               	BNE gra_copycode
   361  09ce ad3703             	LDA savemo
   362  09d1 290f               	AND #$0F
   363  09d3 aa                 	TAX
   364  09d4 4ca80d             	JMP setmode_enter	; re-apply mode to routines
   365                          				; implicit RTS
   366                          
   367                          ;-----------------------------------------------------------------
   368                          
   369                          gexit
   370  09d7 a501                       LDA prozport
   371  09d9 0902                       ORA #%00000010	; kernal ROM enable
   372  09db 8501                       STA prozport
   373  09dd 58                         CLI		; allow interrupts
   374  09de 60                         RTS
   375                          
   376                          ;-----------------------------------------------------------------
   377                          
   378                          ginit
   379  09df a501                       LDA prozport
   380  09e1 29fd                       AND #%11111101	; Kernal ROM disable
   381  09e3 78                         SEI		; disable interrupts
   382  09e4 8501                       STA prozport
   383  09e6 60                         RTS
   384                          
   385                          ;-----------------------------------------------------------------
   386                          
   387                          ; These are selfmodified routines, which has to placed into RAM
   388                          ; (on every graphic "on")
   389                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   390                          
   391                          gromcode
   392                          
   393                          !pseudopc gramcode {
   394                          
   395                          ; change a graphic location
   396                          
   397                          gchange
   398  09e7 b1a5                       LDA (gaddr),Y
   399                          gchange_op
   400  09e9 1dfe08                     ORA bitmask,X
   401  09ec 91a5                       STA (gaddr),Y
   402  09ee 60                         RTS
   403                          
   404                          ; mask a graphic location 
   405                          
   406                          gmask
   407                          gmask_flip
   408  09ef 4900                       EOR #$00
   409                          gmask_op
   410  09f1 11a5                       ORA (gaddr),Y
   411  09f3 91a5                       STA (gaddr),Y
   412  09f5 60                         RTS
   413                          
   414                          }
   415                          
   416                          gromcode_end
   417                          
   418                          ;-----------------------------------------------------------------
   419                          
   420                          position
   421  09f6 a5aa                       LDA y
   422  09f8 4a                         LSR
   423  09f9 4a                         LSR
   424  09fa 4a                         LSR		; y/8
   425  09fb a8                         TAY
   426  09fc 2903                       AND #%00000011	; (y/8) mod 4
   427  09fe aa                         TAX
   428  09ff a59b                       LDA xl		; x low
   429  0a01 29f8                       AND #%11111000	; clear bit 2-0
   430  0a03 18                         CLC
   431  0a04 7d0e09                     ADC ytabl,X	; addr low: y base + x part
   432  0a07 85a5                       STA gaddr
   433  0a09 a59c                       LDA xh		; addr high: x part
   434  0a0b 791209                     ADC ytabh,Y	; 	+ y base
   435  0a0e 85a6                       STA gaddr+1
   436  0a10 a5aa                       LDA y		; vertical offset
   437  0a12 2907                       AND #%00000111	; y mod 8
   438  0a14 a8                         TAY
   439  0a15 a59b                       LDA xl
   440  0a17 2907                       AND #%00000111	; x mod 8
   441  0a19 aa                         TAX		; horizonal offset
   442  0a1a 60                         RTS		; (bitmask)
   443                          
   444                          
   445                          ;-----------------------------------------------------------------
   446                          
   447                          ; line y up, x right, dx < dy (case 1)
   448                          
   449                          line_up_steep
   450  0a1b 20f609                     JSR position	; x,y
   451                          loop_yup_xright
   452  0a1e 20ed03                     JSR gchange	; pixel
   453                          
   454  0a21 18                         CLC		; k += dx
   455  0a22 a595                       LDA kl
   456  0a24 65ab                       ADC dxl		; dxh is 0, because dx < dy
   457  0a26 8595                       STA kl
   458  0a28 b004                       BCS ++		; k > 255
   459                          
   460  0a2a c5a9                       CMP dy
   461  0a2c 9015                       BCC +		; k >= dy ->
   462                          
   463  0a2e e5a9               ++	SBC dy		; k -= dy
   464  0a30 8595                       STA kl
   465                          
   466  0a32 e8                         INX		; x++
   467  0a33 e008                       CPX #8
   468  0a35 d00c                       BNE +
   469                          	; C=1
   470  0a37 a200                       LDX #0		; x overflow, wrap around
   471  0a39 a5a5                       LDA gaddr	; x+8: gaddr += 8
   472  0a3b 6907                       ADC #8-1	; C already set by CPX
   473  0a3d 85a5                       STA gaddr
   474  0a3f 9002                       BCC +
   475  0a41 e6a6                       INC gaddr+1
   476                          
   477  0a43 88                 +	DEY		; y--
   478  0a44 100f                       BPL +++
   479  0a46 38                         SEC		; y overflow
   480  0a47 a5a5                       LDA gaddr
   481  0a49 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   482  0a4b 85a5                       STA gaddr
   483  0a4d a5a6                       LDA gaddr+1
   484  0a4f e901               	SBC #1
   485  0a51 85a6                       STA gaddr+1
   486  0a53 a007                       LDY #7		; wrap around
   487                          
   488  0a55 c6a3               +++	DEC cl		; until c=0
   489  0a57 d0c5                       BNE loop_yup_xright
   490  0a59 4cd709                     JMP gexit
   491                          
   492                          
   493                          ;-----------------------------------------------------------------
   494                          
   495                          ; line x right, y up, dx > dy (case 2)
   496                          
   497                          line_up_flat
   498  0a5c 20f609                     JSR position	; x,y
   499  0a5f a5a3               	LDA cl		; counter adjustment for
   500  0a61 f002               	BEQ +		; dec-dec-counting
   501  0a63 e6a4               	INC ch
   502                          +
   503                          loop_xright_yup
   504  0a65 20ed03                     JSR gchange	; pixel
   505                          
   506  0a68 18                         CLC		; k += dy
   507  0a69 a595                       LDA kl
   508  0a6b 65a9                       ADC dy
   509  0a6d 8595                       STA kl
   510  0a6f 9002                       BCC ++
   511  0a71 e696                       INC kh
   512                          
   513  0a73 c5ab               ++	CMP dxl		; k > dx?
   514  0a75 a596                       LDA kh
   515  0a77 e5a7                       SBC dxh
   516  0a79 901a                       BCC +
   517                          
   518  0a7b 8596                       STA kh		; k -= dx
   519  0a7d a595                       LDA kl
   520  0a7f e5ab                       SBC dxl
   521  0a81 8595                       STA kl
   522                          
   523  0a83 88                         DEY		; y--
   524  0a84 100f                       BPL +
   525  0a86 38                 	SEC		; C=1 not always true (SBC above)
   526  0a87 a5a5                       LDA gaddr	; y overflow
   527  0a89 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   528  0a8b 85a5                       STA gaddr
   529  0a8d a5a6                       LDA gaddr+1
   530  0a8f e901               	SBC #1
   531  0a91 85a6                       STA gaddr+1
   532  0a93 a007               	LDY #7		; wrap around
   533                          
   534  0a95 e8                 +	INX		; x++
   535  0a96 e008                       CPX #8		; x overflow?
   536  0a98 d00c                       BNE ++
   537                          	; C=1
   538  0a9a a200                       LDX #0		; wrap around
   539  0a9c a5a5                       LDA gaddr	; x+8: gaddr += 8
   540  0a9e 6907                       ADC #8-1	; C already set by CPX
   541  0aa0 85a5                       STA gaddr
   542  0aa2 9002                       BCC ++
   543  0aa4 e6a6                       INC gaddr+1
   544                          ++
   545  0aa6 c6a3               	DEC cl		; c--
   546  0aa8 d0bb                       BNE loop_xright_yup
   547  0aaa c6a4                       DEC ch		; adjusted high which allows this
   548  0aac d0b7                       BNE loop_xright_yup
   549                          
   550  0aae 4cd709                     JMP gexit
   551                          
   552                          
   553                          
   554                          ;-----------------------------------------------------------------
   555                          
   556                          ; line x right, y down, dx > dy (case 3)
   557                          
   558                          line_down_flat
   559  0ab1 20f609                     JSR position	; x,y
   560  0ab4 a5a3               	LDA cl		; counter adjustment for
   561  0ab6 f002               	BEQ +		; dec-dec-counting
   562  0ab8 e6a4               	INC ch
   563                          +
   564                          loop_xright_ydown
   565  0aba 20ed03                     JSR gchange	; pixel
   566                          
   567  0abd 18                         CLC		; k += dy
   568  0abe a595                       LDA kl
   569  0ac0 65a9                       ADC dy
   570  0ac2 8595                       STA kl
   571  0ac4 9002                       BCC ++
   572  0ac6 e696                       INC kh
   573                          
   574  0ac8 c5ab               ++	CMP dxl		; k > dx
   575  0aca a596                       LDA kh
   576  0acc e5a7                       SBC dxh		; k -= dx
   577  0ace 901b                       BCC +
   578                          
   579  0ad0 8596                       STA kh
   580  0ad2 a595                       LDA kl
   581  0ad4 e5ab                       SBC dxl
   582  0ad6 8595                       STA kl
   583                          
   584  0ad8 c8                         INY		; y++
   585  0ad9 c008                       CPY #8
   586  0adb d00e                       BNE +
   587                          	; C=1
   588  0add a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   589  0adf 693f                       ADC #$40-1	; C already set by CPY
   590  0ae1 85a5                       STA gaddr
   591  0ae3 a5a6                       LDA gaddr+1
   592  0ae5 6901               	ADC #1
   593  0ae7 85a6                       STA gaddr+1
   594  0ae9 a000                       LDY #0		; wrap around
   595                          
   596  0aeb e8                 +	INX		; x++
   597  0aec e008                       CPX #8		; x overflow ?
   598  0aee d00c                       BNE +++
   599                          	; C=1
   600  0af0 a200                       LDX #$00	; wrap around
   601  0af2 a5a5                       LDA gaddr	; gaddr += 8
   602  0af4 6907                       ADC #$08-1	; C always set by CPX
   603  0af6 85a5                       STA gaddr
   604  0af8 9002                       BCC +++
   605  0afa e6a6                       INC gaddr+1
   606                          +++
   607  0afc c6a3               	DEC cl		; c--
   608  0afe d0ba                       BNE loop_xright_ydown
   609  0b00 c6a4                       DEC ch		; adjusted high which allows this
   610  0b02 d0b6                       BNE loop_xright_ydown
   611                          
   612  0b04 4cd709                     JMP gexit
   613                          
   614                          
   615                          ;-----------------------------------------------------------------
   616                          
   617                          ; line y down, x right, dx < dy (case 4)
   618                          
   619                          line_down_steep
   620  0b07 20f609                     JSR position	; x,y
   621                          loop_ydown_xright
   622  0b0a 20ed03                     JSR gchange	; pixel
   623                          
   624  0b0d 18                         CLC		; k += dx
   625  0b0e a595                       LDA kl
   626  0b10 65ab                       ADC dxl		; dxh is 0, because dx < dy
   627  0b12 8595                       STA kl
   628  0b14 b004                       BCS ++
   629  0b16 c5a9                       CMP dy		; k > dy?
   630  0b18 9015                       BCC +
   631  0b1a e5a9               ++	SBC dy		; k -= dy
   632  0b1c 8595                       STA kl
   633                          
   634  0b1e e8                         INX		; x++
   635  0b1f e008                       CPX #8
   636  0b21 d00c                       BNE +		; x overflow?
   637  0b23 a200                       LDX #0		; wrap around
   638  0b25 a5a5                       LDA gaddr	; x+9: gaddr += 8
   639  0b27 6907                       ADC #8-1	; C already set by CPX
   640  0b29 85a5                       STA gaddr
   641  0b2b 9002                       BCC +
   642  0b2d e6a6                       INC gaddr+1
   643                          
   644  0b2f c8                 +	INY		; y++
   645  0b30 c008                       CPY #8		; y overflow?
   646  0b32 d00e                       BNE +++
   647  0b34 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   648  0b36 693f                       ADC #$40-1	; C already set by CPY
   649  0b38 85a5                       STA gaddr
   650  0b3a a5a6                       LDA gaddr+1
   651  0b3c 6901               	ADC #1
   652  0b3e 85a6                       STA gaddr+1
   653  0b40 a000                       LDY #0		; wrap around
   654                          
   655  0b42 c6a3               +++	DEC cl		; c--
   656                          			; until c=0
   657  0b44 d0c4                       BNE loop_ydown_xright
   658  0b46 4cd709                     JMP gexit
   659                          
   660                          
   661                          ;-----------------------------------------------------------------
   662                          
   663                          getcommaxy
   664  0b49 20fdae                     JSR b_getcomma	; check ","
   665                          getxy
   666  0b4c 208aad                     JSR b_getval	; get X coord. value
   667  0b4f 20f7b7                     JSR b_convint
   668  0b52 c901                       CMP #>xmax
   669  0b54 9009               	BCC gcxy_xok
   670  0b56 f003                       BEQ +		; X = $1xx
   671                          error_iq
   672  0b58 20700d                     JSR range_error
   673  0b5b c040               +	CPY #<xmax	; check X low
   674  0b5d b0f9                       BCS error_iq	; X to big
   675                          gcxy_xok
   676  0b5f 84fb                       STY gpos	; temporary save X coord.
   677  0b61 85fc                       STA gpos+1
   678                          
   679  0b63 20f1b7                     JSR b_getcomma8bit
   680                          			; get Y coord. value
   681  0b66 e0c8                       CPX #ymax
   682  0b68 b0ee                       BCS error_iq	; Y to big
   683                          
   684  0b6a a4fb                       LDY gpos	; restory X coord.
   685  0b6c a5fc                       LDA gpos+1
   686  0b6e 60                         RTS
   687                          
   688                          
   689                          ;-----------------------------------------------------------------
   690                          
   691                          hline
   692  0b6f 204c0b                     JSR getxy	; get startpoint
   693  0b72 86aa                       STX y
   694  0b74 8e3603                     STX savey	; save as cursor, too
   695  0b77 859c                       STA xh
   696  0b79 849b                       STY xl
   697  0b7b 20fdae                     JSR b_getcomma	; get length
   698  0b7e 208aad                     JSR b_getval
   699  0b81 20f7b7                     JSR b_convint
   700                          
   701  0b84 c901                       CMP #>xmax
   702  0b86 9006                       BCC +		; X < 256
   703  0b88 d0ce                       BNE error_iq
   704  0b8a c040                       CPY #<xmax
   705  0b8c b0ca                       BCS error_iq
   706                          +
   707                          			; calculate end point
   708  0b8e aa                         TAX		; save length high byte
   709  0b8f 98                         TYA		; length low byte
   710  0b90 18                         CLC
   711  0b91 659b                       ADC xl		; low xend = x+length
   712  0b93 859e                       STA xendl
   713  0b95 a8                 	TAY
   714  0b96 8a                         TXA		; high
   715  0b97 659c                       ADC xh		; high xend = x+length
   716  0b99 859f                       STA xendh
   717  0b9b aa                 	TAX
   718                          
   719  0b9c c901               	CMP #>xmax	; endpoint outside?
   720  0b9e 9005               	BCC +
   721  0ba0 98                 	TYA
   722  0ba1 e940               	SBC #<xmax
   723  0ba3 b0b3               	BCS error_iq
   724                          +
   725  0ba5 8e3503                     STX savexh
   726  0ba8 8c3403                     STY savexl	; also save as cursor
   727                          
   728  0bab 20df09                     JSR ginit	; map in graphic memory
   729                          
   730                          hline_start
   731  0bae a59e                       LDA xendl
   732  0bb0 c59b                       CMP xl
   733  0bb2 a59f                       LDA xendh
   734  0bb4 e59c                       SBC xh
   735  0bb6 b013                       BCS hl_noxswap	; xend < x ->
   736                          
   737  0bb8 a69e                       LDX xendl	; swap x, xend
   738  0bba a59b                       LDA xl
   739  0bbc 869b                       STX xl
   740  0bbe 859e                       STA xendl
   741                          
   742  0bc0 a69f                       LDX xendh
   743  0bc2 a49c                       LDY xh
   744  0bc4 849f                       STY xendh
   745  0bc6 869c                       STX xh
   746  0bc8 4cda0b                     JMP hl_start	; x != xend
   747                          
   748                          hl_noxswap
   749  0bcb a59e                       LDA xendl
   750  0bcd c59b                       CMP xl
   751  0bcf d009                       BNE hl_start
   752  0bd1 a59f                       LDA xendh
   753  0bd3 c59c                       CMP xh
   754  0bd5 d003                       BNE hl_start	; x = xend ->
   755  0bd7 4c4e0d             	JMP plot_start	; single point
   756                          ;	JMP gexit	; no point
   757                          
   758                          hl_start
   759  0bda 20f609                     JSR position	; graphic position x,y
   760  0bdd bd2b09                     LDA maskleft,X
   761  0be0 48                         PHA		; save left end mask
   762  0be1 a59e                       LDA xendl
   763  0be3 2907                       AND #%00000111
   764  0be5 8596                       STA tmp2	; xend mod 8, mask index
   765  0be7 a59b                       LDA xl
   766  0be9 29f8                       AND #%11111000	; (xl div 8)*8
   767  0beb 8595                       STA tmp1
   768  0bed a59e                       LDA xendl	; xend unmasked
   769  0bef 38                         SEC
   770  0bf0 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   771  0bf2 8595                       STA tmp1
   772  0bf4 a59f                       LDA xendh
   773  0bf6 e59c                       SBC xh
   774  0bf8 4a                         LSR		; / 8 ->  0-39
   775  0bf9 a595                       LDA tmp1	; only 1 highest bit
   776  0bfb 6a                         ROR		; and 3 lower bits
   777  0bfc 4a                         LSR
   778  0bfd 4a                         LSR
   779  0bfe aa                         TAX		; 8-pixel-blocks count
   780  0bff 68                         PLA		; left end x mask
   781                          
   782                          hl_nextblock
   783  0c00 ca                         DEX
   784                          hl_islastblock
   785  0c01 3012                       BMI hl_lastblock
   786                          			; leave loop if X<0
   787  0c03 20f503                     JSR gmask	; first with left end mask
   788  0c06 18                         CLC		; gaddr += 8
   789  0c07 a5a5                       LDA gaddr
   790  0c09 6908                       ADC #8
   791  0c0b 85a5                       STA gaddr
   792  0c0d 9002                       BCC +
   793  0c0f e6a6                       INC gaddr+1
   794  0c11 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   795  0c13 d0eb               	BNE hl_nextblock	; always
   796                          
   797                          hl_lastblock
   798  0c15 a696                       LDX tmp2	; xend mask index
   799  0c17 3d3309                     AND maskright,X ; mask right end
   800  0c1a 20f503                     JSR gmask	; modify
   801  0c1d 4cd709                     JMP gexit	; leave
   802                          
   803                          
   804                          ;-----------------------------------------------------------------
   805                          
   806                          vline
   807  0c20 204c0b                     JSR getxy	; get startpoint
   808  0c23 859c                       STA xh
   809  0c25 8d3503                     STA savexh	; save as cursor too
   810  0c28 849b                       STY xl
   811  0c2a 8c3403                     STY savexl
   812  0c2d 86aa                       STX y
   813                          
   814  0c2f 20f1b7                     JSR b_getcomma8bit
   815                          			; get length
   816  0c32 18                         CLC		; calculate end point
   817  0c33 8a                         TXA		; length
   818                          ; DON'T-CHANGE: how long to go vertically (needed later)
   819                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   820                          ;	STA tmp1
   821  0c34 65aa                       ADC y		; length + y
   822  0c36 c9c8                       CMP #ymax
   823  0c38 9003                       BCC +
   824                          vline_iq
   825  0c3a 20700d                     JSR range_error
   826  0c3d 8593               +	STA yend	; endpoint
   827  0c3f c9c8               	CMP #ymax	; outside?
   828  0c41 b0f7               	BCS vline_iq
   829                          
   830  0c43 8d3603             	STA savey	; set cursor y position
   831                          
   832  0c46 20df09                     JSR ginit	; map in graphic memory
   833                          
   834                          vline_start
   835  0c49 a593                       LDA yend
   836  0c4b c5aa                       CMP y
   837  0c4d b00a                       BCS vl_noyswap	; yend < y ->
   838  0c4f a5aa                       LDA y		; swap y, yend
   839  0c51 a693                       LDX yend
   840  0c53 8593                       STA yend
   841  0c55 86aa                       STX y
   842  0c57 f005               	BEQ vl_start	; always (with next branch)
   843                          	; fall through if yend is
   844                          vl_noyswap
   845  0c59 d003                       BNE vl_start	; y = yend ->
   846  0c5b 4c4e0d             	JMP plot_start	; single point
   847                          ;	JMP gexit	; no point
   848                          
   849                          vl_start
   850  0c5e 20f609                     JSR position	; graphic position x,y
   851  0c61 bdfe08                     LDA bitmask,X
   852  0c64 8596                       STA tmp2	; save mask
   853                          ; DON'T-CHANGE: replace ...
   854  0c66 38                         SEC
   855  0c67 a593                       LDA yend
   856  0c69 e5aa                       SBC y		; vertical length
   857  0c6b aa                         TAX
   858                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   859                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   860                          ;	LDX tmp1
   861  0c6c e8                         INX		; +1 (exit on 0)
   862                          vl_nextline
   863  0c6d a596                       LDA tmp2
   864  0c6f 20f503                     JSR gmask	; modify 
   865  0c72 c8                         INY		; go down
   866  0c73 c008                       CPY #8		; 8-line wrap
   867  0c75 d00e                       BNE +
   868  0c77 a5a5                       LDA gaddr	; gaddr += 320
   869  0c79 693f               	ADC #$40-1	; compensate for C = 1
   870  0c7b 85a5                       STA gaddr
   871  0c7d a5a6                       LDA gaddr+1
   872  0c7f 6901                       ADC #$01
   873  0c81 85a6                       STA gaddr+1
   874  0c83 a000                       LDY #0		; wrap y offset
   875  0c85 ca                 +	DEX		; all vertical positions done?
   876  0c86 d0e5                       BNE vl_nextline
   877  0c88 4cd709                     JMP gexit	; leave
   878                          
   879                          
   880                          ;-----------------------------------------------------------------
   881                          
   882                          line
   883  0c8b 204c0b                     JSR getxy	; get startpoint
   884  0c8e 849b                       STY xl 
   885  0c90 859c                       STA xh
   886  0c92 86aa                       STX y
   887                          
   888  0c94 20490b                     JSR getcommaxy	; get endpoint
   889                          line_start
   890  0c97 8c3403                     STY savexl	; save as cursor position too
   891  0c9a 849e                       STY xendl
   892  0c9c 8d3503                     STA savexh
   893  0c9f 859f                       STA xendh
   894  0ca1 8e3603                     STX savey
   895  0ca4 8693                       STX yend
   896                          
   897  0ca6 20df09                     JSR ginit	; map in graphic memory
   898                          
   899  0ca9 a000                       LDY #$00	; initialize to 0
   900  0cab 84a8                       STY ydir
   901  0cad 8495                       STY kl
   902  0caf 8496                       STY kh
   903                          
   904  0cb1 38                         SEC
   905  0cb2 a59e                       LDA xendl	; calculate dx
   906  0cb4 e59b                       SBC xl
   907  0cb6 85ab                       STA dxl
   908  0cb8 a59f                       LDA xendh
   909  0cba e59c                       SBC xh
   910  0cbc 85a7                       STA dxh
   911                          
   912  0cbe b025                       BCS li_xend_right
   913                          	; dx != 0
   914  0cc0 98                         TYA		; negate dx
   915  0cc1 38                         SEC		; dx = 0 - dx
   916  0cc2 e5ab                       SBC dxl
   917  0cc4 85ab                       STA dxl
   918  0cc6 98                         TYA
   919  0cc7 e5a7                       SBC dxh
   920  0cc9 85a7                       STA dxh
   921                          			; C=0 always, needed later
   922  0ccb a69b                       LDX xl		; swap x low
   923  0ccd a49e                       LDY xendl
   924  0ccf 869e                       STX xendl
   925  0cd1 849b                       STY xl
   926                          
   927  0cd3 a69c                       LDX xh		; swap x high
   928  0cd5 a49f                       LDY xendh
   929  0cd7 869f                       STX xendh
   930  0cd9 849c                       STY xh
   931                          
   932  0cdb a6aa                       LDX y		; swap y
   933  0cdd a493                       LDY yend
   934  0cdf 8693                       STX yend
   935  0ce1 84aa                       STY y
   936                          
   937  0ce3 9009                       BCC li_x_different
   938                          			; C=0 always (from negation before)
   939                          
   940                          li_xend_right
   941  0ce5 a5ab                       LDA dxl		; dx = 0?
   942  0ce7 05a7                       ORA dxh
   943  0ce9 d003                       BNE li_x_different
   944  0ceb 4c490c                     JMP vline_start	; vertical line case
   945                          
   946                          li_x_different
   947  0cee 38                         SEC		; calculate dy
   948  0cef a593                       LDA yend
   949  0cf1 e5aa                       SBC y
   950  0cf3 b006                       BCS li_y_right
   951  0cf5 49ff                       EOR #$FF	; negate dy (two's complement)
   952  0cf7 6901                       ADC #$01	; C=0
   953  0cf9 85a8                       STA ydir	; flag y goes up
   954                          
   955                          li_y_right
   956  0cfb 85a9                       STA dy
   957  0cfd d003                       BNE +
   958  0cff 4cae0b                     JMP hline_start	; horizontal line case
   959                          +
   960                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   961                          
   962  0d02 a5a7                       LDA dxh		; dx > dy
   963  0d04 d017                       BNE line_flat	; yes -> flat
   964  0d06 a5a9                       LDA dy		; no -> steep
   965  0d08 aa                         TAX
   966  0d09 c5ab                       CMP dxl
   967  0d0b 9010                       BCC line_flat
   968                          
   969                          line_steep
   970  0d0d e8                         INX	
   971  0d0e 86a3                       STX cl		; c = dy+1
   972  0d10 4a                         LSR		; k = dy/2
   973  0d11 8595                       STA kl
   974  0d13 a5a8                       LDA ydir
   975  0d15 d003                       BNE +
   976  0d17 4c070b                     JMP line_down_steep	; y down, steep
   977  0d1a 4c1b0a             +	JMP line_up_steep	; y up, steep
   978                          
   979                          line_flat
   980  0d1d a5a7                       LDA dxh
   981  0d1f a8                         TAY
   982  0d20 a6ab                       LDX dxl
   983  0d22 e8                         INX
   984  0d23 d001                       BNE +
   985  0d25 c8                         INY
   986  0d26 86a3               +	STX cl		; c = dx+1
   987  0d28 84a4                       STY ch
   988                          
   989  0d2a 4a                         LSR		; k = dx/2
   990  0d2b 8596                       STA kh
   991  0d2d a5ab                       LDA dxl
   992  0d2f 6a                         ROR		; dx/2
   993  0d30 8595                       STA kl
   994  0d32 a5a8                       LDA ydir	
   995  0d34 d003                       BNE +
   996  0d36 4cb10a                     JMP line_down_flat	; y down, flat
   997  0d39 4c5c0a             +	JMP line_up_flat	; y up, flat
   998                          
   999                          ;-----------------------------------------------------------------
  1000                          
  1001                          plot
  1002  0d3c 204c0b                     JSR getxy	; get parameter
  1003  0d3f 859c                       STA xh		; save x/y
  1004  0d41 849b                       STY xl
  1005  0d43 86aa                       STX y
  1006  0d45 8d3503                     STA savexh	; and store as cursor
  1007  0d48 8c3403                     STY savexl
  1008  0d4b 8e3603                     STX savey
  1009                          
  1010                          plot_start
  1011  0d4e 20f609                     JSR position	; calculate graphical address
  1012                          
  1013  0d51 a501                       LDA prozport
  1014  0d53 29fd                       AND #%11111101	; Kernal ROM disable
  1015  0d55 78                         SEI			
  1016  0d56 8501                       STA prozport
  1017                          
  1018  0d58 20ed03                     JSR gchange	; change graphical data
  1019                          
  1020  0d5b a501                       LDA prozport
  1021  0d5d 0902                       ORA #%00000010	; kernal ROM enable
  1022  0d5f 8501                       STA prozport
  1023  0d61 58                         CLI
  1024  0d62 60                         RTS
  1025                          
  1026                          ;-----------------------------------------------------------------
  1027                          
  1028                          move
  1029  0d63 204c0b                     JSR getxy	; get parameter
  1030  0d66 8d3503                     STA savexh	; just save as cursor
  1031  0d69 8c3403                     STY savexl
  1032  0d6c 8e3603                     STX savey
  1033  0d6f 60                         RTS
  1034                          
  1035                          
  1036                          ;-----------------------------------------------------------------
  1037                          
  1038                          range_error
  1039  0d70 ad3703             	LDA savemo
  1040  0d73 29f0               	AND #$F0
  1041  0d75 d003               	BNE +
  1042  0d77 68                 	PLA			; cleanup JSR
  1043  0d78 68                 	PLA
  1044  0d79 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1045                          				; error mode 2: cut value: control back
  1046                          				; to handle value correction
  1047  0d7a 2920               +	AND #$20
  1048  0d7c d0fb               	BNE -
  1049  0d7e 68                 	PLA			; cleanup JSR
  1050  0d7f 68                 	PLA
  1051                          setmode_error
  1052  0d80 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1053                          
  1054                          ;-----------------------------------------------------------------
  1055                          
  1056                          setmode
  1057  0d83 209eb7                     JSR b_get8bit
  1058  0d86 e003                       CPX #3
  1059  0d88 9012                       BCC +			; less then 3, modification mode
  1060  0d8a e006               	CPX #6
  1061  0d8c b0f2               	BCS setmode_error	; out of range
  1062                          				; error mode
  1063  0d8e 690d               	ADC #13			; C=0, therefore -3
  1064                          				; 3-5 -> 16-18
  1065                          				; put A's bit 4-7 into savemo
  1066  0d90 4d3703             	EOR savemo		; ********
  1067  0d93 29f0               	AND #$F0		; ****0000
  1068  0d95 4d3703             	EOR savemo		; AAAAmmmm
  1069  0d98 8d3703             	STA savemo		; 
  1070  0d9b 60                 	RTS
  1071                          
  1072  0d9c 8a                 +	TXA
  1073  0d9d 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1074  0da0 290f               	AND #$0F
  1075  0da2 4d3703             	EOR savemo
  1076  0da5 8d3703             	STA savemo
  1077                          setmode_enter
  1078  0da8 e001               	CPX #$01
  1079  0daa b01a                       BCS set_or_toggle
  1080                          
  1081                          modereset
  1082  0dac a909                       LDA #>(nbitmask)
  1083  0dae 8df103                     STA gchange_op+2
  1084  0db1 a906                       LDA #<(nbitmask)
  1085  0db3 8df003                     STA gchange_op+1
  1086  0db6 a93d                       LDA #$3D		; AND abs,X
  1087  0db8 8def03                     STA gchange_op
  1088  0dbb a931                       LDA #$31		; AND (zp),Y
  1089  0dbd 8df703                     STA gmask_op
  1090  0dc0 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1091  0dc2 8df603                     STA gmask_flip+1
  1092  0dc5 60                         RTS
  1093                          
  1094                          set_or_toggle
  1095  0dc6 d01a                       BNE modetoggle
  1096                          modeset
  1097  0dc8 a908                       LDA #>(bitmask)
  1098  0dca 8df103                     STA gchange_op+2
  1099  0dcd a9fe                       LDA #<(bitmask)
  1100  0dcf 8df003                     STA gchange_op+1
  1101  0dd2 a91d                       LDA #$1D		; OR abs,X
  1102  0dd4 8def03                     STA gchange_op
  1103  0dd7 a911                       LDA #$11		; OR (zp),Y
  1104  0dd9 8df703                     STA gmask_op
  1105  0ddc a900                       LDA #$00		; EOR #$00, nicht invertieren
  1106  0dde 8df603                     STA gmask_flip+1
  1107  0de1 60                         RTS
  1108                          
  1109                          modetoggle
  1110  0de2 a908                       LDA #>(bitmask)
  1111  0de4 8df103                     STA gchange_op+2
  1112  0de7 a9fe                       LDA #<(bitmask)
  1113  0de9 8df003                     STA gchange_op+1
  1114  0dec a95d                       LDA #$5D		; EOR abs,X
  1115  0dee 8def03                     STA gchange_op
  1116  0df1 a951                       LDA #$51		; EOR (zp),Y
  1117  0df3 8df703                     STA gmask_op
  1118  0df6 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1119  0df8 8df603                     STA gmask_flip+1
  1120  0dfb 60                         RTS
  1121                          
  1122                          
  1123                          ;-----------------------------------------------------------------
  1124                          
  1125                          ; get pixel (check if pixel set)
  1126                          ; not used
  1127                          
  1128                          get
  1129  0dfc 20490b                     JSR getcommaxy
  1130  0dff 859c                       STA xh
  1131  0e01 849b                       STY xl
  1132  0e03 86aa                       STX y
  1133                          
  1134  0e05 20f609                     JSR position
  1135                          
  1136  0e08 a501                       LDA prozport
  1137  0e0a 29fd               	AND #%11111101	; Kernal ROM disable
  1138  0e0c 78                         SEI
  1139  0e0d 8501                       STA prozport
  1140                          
  1141  0e0f b1a5                       LDA (gaddr),Y
  1142  0e11 3dfe08                     AND bitmask,X
  1143  0e14 a8                         TAY
  1144  0e15 a501                       LDA prozport
  1145  0e17 0902               	ORA #%00000010	; kernal ROM enable
  1146  0e19 8501                       STA prozport
  1147  0e1b 58                         CLI
  1148  0e1c 4ca2b3                     JMP b_byte2fac
  1149                          
  1150                          
  1151                          ;-----------------------------------------------------------------
  1152                          
  1153                          relto
  1154  0e1f 208aad                     JSR b_getval	; get X offset (+/-)
  1155  0e22 a561               	LDA facexp	; FAC exponent
  1156  0e24 c990               	CMP #$90	; more than 16 bit
  1157  0e26 b031               	BCS relto_error	; illegal quantity
  1158  0e28 209bbc                     JSR b_fac2int	; to signed integer
  1159                          
  1160  0e2b 18                         CLC
  1161  0e2c a565                       LDA facintl
  1162  0e2e 6d3403                     ADC savexl
  1163  0e31 859e                       STA xendl
  1164  0e33 a564                       LDA facinth
  1165  0e35 6d3503                     ADC savexh
  1166  0e38 859f                       STA xendh	; xend = savex+facint
  1167                          
  1168  0e3a 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1169  0e3d 208aad                     JSR b_getval
  1170  0e40 a561                       LDA facexp	; FAC exponent
  1171  0e42 c990                       CMP #$90	; more than 16 bit
  1172  0e44 b013                       BCS relto_error	; illegal quantity
  1173  0e46 209bbc                     JSR b_fac2int	; to signed integer
  1174  0e49 18                         CLC
  1175  0e4a a565                       LDA facintl
  1176  0e4c 6d3603                     ADC savey
  1177  0e4f 8593                       STA yend	; yend = savey+facint
  1178                          
  1179  0e51 a59f                       LDA xendh	; check end coord. x
  1180  0e53 c901                       CMP #>xmax
  1181  0e55 900b                       BCC rt_xok
  1182  0e57 f003                       BEQ +
  1183                          relto_error
  1184  0e59 20700d                     JSR range_error
  1185  0e5c a59e               +	LDA xendl
  1186  0e5e c940                       CMP #<xmax
  1187  0e60 b0f7                       BCS relto_error
  1188                          rt_xok
  1189  0e62 a593                       LDA yend	; check end coord. y
  1190  0e64 c9c8                       CMP #ymax
  1191  0e66 b0f1                       BCS relto_error
  1192                          
  1193  0e68 ad3403                     LDA savexl
  1194  0e6b 859b                       STA xl
  1195  0e6d ad3503                     LDA savexh
  1196  0e70 859c                       STA xh
  1197  0e72 ad3603                     LDA savey
  1198  0e75 85aa                       STA y
  1199  0e77 a49e                       LDY xendl
  1200  0e79 a59f                       LDA xendh
  1201  0e7b a693                       LDX yend	; xend/yend = cursor + x/y
  1202                          
  1203  0e7d 4c970c                     JMP line_start	; draw line x/y to xend/yend
  1204                          
  1205                          
  1206                          ;-----------------------------------------------------------------
  1207                          
  1208                          char
  1209  0e80 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1210  0e83 e028                       CPX #40	
  1211  0e85 9003                       BCC +
  1212                          char_error
  1213  0e87 4c48b2                     JMP b_illquant
  1214  0e8a 86fb               +	STX gpos	; save x coord.
  1215  0e8c 20f1b7                     JSR b_getcomma8bit
  1216                          			; get char. position y 0-24
  1217  0e8f e019                       CPX #25
  1218  0e91 b0f4                       BCS char_error
  1219  0e93 86fc                       STX gpos+1	; save y coord.
  1220                          
  1221  0e95 20fdae                     JSR b_getcomma	; get string
  1222  0e98 209ead                     JSR b_getexpr
  1223  0e9b 20a3b6                     JSR b_stringval ; string address in str
  1224  0e9e 48                         PHA		; string length
  1225  0e9f a6fc                       LDX gpos+1	; y coord. for char. position
  1226  0ea1 8a                         TXA
  1227  0ea2 2903                       AND #$03	; mask 2 bits
  1228  0ea4 a8                         TAY		; table index
  1229  0ea5 a900                       LDA #$00
  1230  0ea7 85fc                       STA gpos+1	; x high
  1231  0ea9 a5fb                       LDA gpos	; saved x: multiply by 8
  1232  0eab 0a                         ASL
  1233  0eac 0a                         ASL
  1234  0ead 0a                         ASL
  1235  0eae 26fc                       ROL gpos+1	; overflow to high byte
  1236  0eb0 790e09                     ADC ytabl,Y
  1237  0eb3 85a5                       STA gaddr
  1238  0eb5 a5fc                       LDA gpos+1	; x high
  1239  0eb7 7d1209                     ADC ytabh,X
  1240  0eba 85a6                       STA gaddr+1
  1241  0ebc 68                         PLA		; string length
  1242  0ebd a000                       LDY #$00	; string index
  1243  0ebf aa                         TAX		; length
  1244  0ec0 e8                         INX		; prepare as counter
  1245                          char_loop
  1246  0ec1 ca                         DEX
  1247  0ec2 f008                       BEQ char_exit
  1248  0ec4 b122                       LDA (str),Y	; read string
  1249  0ec6 20cd0e                     JSR char_display
  1250  0ec9 c8                         INY
  1251  0eca d0f5                       BNE char_loop
  1252                          char_exit
  1253  0ecc 60                         RTS
  1254                          
  1255                          char_display
  1256  0ecd 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1257  0ecf 8a                         TXA		; save register X+Y
  1258  0ed0 48                         PHA
  1259  0ed1 98                         TYA
  1260  0ed2 48                         PHA
  1261  0ed3 a5d7                       LDA z_tmp	; get saved character
  1262  0ed5 1049                       BPL char_normal
  1263                          
  1264                          char_inverse
  1265  0ed7 297f                       AND #%01111111	; mask bit 7
  1266  0ed9 c97f                       CMP #%01111111	; was 255? (pi)
  1267  0edb d002                       BNE +
  1268  0edd a95e                       LDA #$5E	; screen code for pi
  1269  0edf c920               +	CMP #$20	; control character?
  1270  0ee1 9038                       BCC char_disp_leave
  1271                          			; yes, skip
  1272  0ee3 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1273                          			; $C0-$FF -> $40-$7F
  1274                          			; OPT: BNE char_hires
  1275                          			; OPT: char_normal
  1276                          char_hires
  1277  0ee5 a6c7                       LDX z_reverseflag
  1278  0ee7 f002                       BEQ +
  1279  0ee9 0980                       ORA #%10000000	; invert char.
  1280  0eeb 48                 +	PHA		; save char. for later
  1281  0eec 78                         SEI
  1282  0eed a921                       LDA #$21	; char. rom, no basic and kernal rom
  1283  0eef 8501                       STA prozport	; char. rom base = $D000
  1284  0ef1 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1285  0ef3 85fc                       STA gpos+1	; 
  1286  0ef5 68                         PLA		; char. code
  1287  0ef6 0a                         ASL		; *8
  1288  0ef7 26fc                       ROL gpos+1
  1289  0ef9 0a                         ASL
  1290  0efa 26fc                       ROL gpos+1
  1291  0efc 0a                         ASL
  1292  0efd 26fc                       ROL gpos+1
  1293  0eff 85fb                       STA gpos	; addr. in char. rom for char.
  1294                          
  1295  0f01 a007                       LDY #$07	; 8 hires lines
  1296                          char_line
  1297  0f03 b1fb                       LDA (gpos),Y	; read character line
  1298  0f05 20f503                     JSR gmask	; write to hires screen
  1299  0f08 88                         DEY
  1300  0f09 10f8                       BPL char_line
  1301                          
  1302  0f0b a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1303  0f0d 8501                       STA prozport
  1304  0f0f 58                         CLI
  1305                          
  1306  0f10 18                         CLC		; step char position to left
  1307  0f11 a5a5                       LDA gaddr	; ( +8 )
  1308  0f13 6908                       ADC #$08
  1309  0f15 85a5                       STA gaddr
  1310  0f17 9002                       BCC +
  1311  0f19 e6a6                       INC gaddr+1
  1312                          +
  1313                          char_disp_leave
  1314  0f1b 68                 	PLA		; pass written character back
  1315  0f1c a8                         TAY		; restore saved registers
  1316  0f1d 68                         PLA
  1317  0f1e aa                         TAX
  1318  0f1f 60                         RTS
  1319                          
  1320                          char_normal
  1321  0f20 c920                       CMP #$20	; control character?
  1322  0f22 90f7                       BCC char_disp_leave
  1323  0f24 c960                       CMP #$60
  1324  0f26 9004                       BCC +
  1325  0f28 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1326  0f2a d002                       BNE ++
  1327  0f2c 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1328  0f2e 4ce50e             ++	JMP char_hires	; 		OPT: Bxx
  1329                          
  1330                          
  1331                          ;-----------------------------------------------------------------
  1332                          
  1333                          to
  1334  0f31 ad3403                     LDA savexl
  1335  0f34 859b                       STA xl
  1336  0f36 ad3503                     LDA savexh
  1337  0f39 859c                       STA xh
  1338  0f3b ad3603                     LDA savey
  1339  0f3e 85aa                       STA y
  1340  0f40 204c0b                     JSR getxy
  1341  0f43 4c970c                     JMP line_start
  1342                          
  1343                          ;-----------------------------------------------------------------
  1344                          graext_end

; ******** Source: ge-run.asm
    43                          
    44                          
