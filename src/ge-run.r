
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
    29  080d a2eb               	ldx #<graext_end	; setup basic
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
     5                          !macro version {
     6                          	!text "1.25" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2016-06-21 v 1.25
    10                          ;	2016-06-16 v 1.24
    11                          ;	2016-05-29 v 1.23
    12                          ;	2016-05-20 v 1.22
    13                          ;	2016-05-16 v 1.21
    14                          ;	2016-02-23 v 1.20
    15                          ;	2016-01-15 v 1.19
    16                          ;	1992-12-28 v 1.18
    17                          ;	1986-03-24 v 1.17
    18                          ;	1985       v 0.00 - 1.16
    19                          ;
    20                          ; the original source has been lost.
    21                          ; development has based on the implemention
    22                          ; done on a forth-64 written with its forth assembler.
    23                          ; the code has been pulled out from there and enriched
    24                          ; with some glue code to get a basic extension.
    25                          
    26                          ; command dispatcher style JMP/RTS
    27                          ;	(if defined)
    28                          ;command_rts_style=1
    29                          
    30                          ; error handling 
    31                          ;	(if defined)
    32                          ;no_error=1
    33                          
    34                          ; basic interpreter registers, addresses and entry points
    35                          
    36                          str     = $22		; string address
    37                          bassta	= $2b		; basic start pointer
    38                          basend	= $2d		; basic end pointer
    39                          ijmp    = $55		; address of JMP (addr)
    40                          chrget  = $73		; basic charget routine
    41                          facintl = $65		; integer result from b_fac2int
    42                          facinth = $64
    43                          facexp  = $61		; fac exponent, after b_getval
    44                          
    45                          z_reverseflag = $C7	; character routine
    46                          z_lastkey = $D7		; original use case, unused here
    47                          z_tmp = z_lastkey	; temporary reused for character routine
    48                          
    49                          v_baserr = $0300	; vector error routine
    50                          v_basstp = $0328	; vector error routine
    51                          v_bascmd = $0308	; vector interpreter parsing
    52                          v_basexp = $030a	; vector evaluate expression
    53                          
    54                          basic_rom = $A000	; start of BASIC ROM
    55                          
    56                          b_clr = $A660		; CLR command
    57                          b_interpreter = $A7AE	; interpreter loop
    58                          b_execstatement = $A7E7	; process statement
    59                          b_getcomma = $AEFD	; read comma from basic text
    60                          b_illquant = $B248	; error "illegal quantity"
    61                          b_syntaxerror = $AF08	; error "syntax"
    62                          b_get8bit = $B79E	; read 8 bit numeric value from
    63                          			; basic text
    64                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    65                          			; from basic text
    66                          b_getval = $AD8A	; read numeric value from basic text
    67                          b_getexpr = $AD9E	; read expression from basic text
    68                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    69                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    70                          b_fac2int = $BC9B	; convert FAC to integer
    71                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    72                          b_rechain = $A533	; rechain basic lines
    73                          
    74                          ; hardware registers and values
    75                          
    76                          prozport = $01		; processor port
    77                          memrom = %00110111	; basic+kernal rom
    78                          membas = %00110110	; basic ram+kernal rom
    79                          memram = %00110101	; basic+kernal ram
    80                          
    81                          vic_cr	= $D011		; VIC control register
    82                          vic_mcr	= $D018		; VIC memory control register
    83                          cia_pra	= $DD00		; CIA 2 port register A
    84                          
    85                          cram	= $CC00		; start of color ram
    86                          
    87                          gram	= $e000		; start of graphic bitmap ram
    88                          gramp	= gram >> 8	; start page of bitmap
    89                          
    90                          ; constants 
    91                          
    92                          xmax	= 320		; max x dimension
    93                          ymax	= 200		; max y dimension
    94                          
    95                          ; zeropage variables
    96                          
    97                          x	= $9B		; start coordinate x, low+high
    98                          xl	= x
    99                          xh	= x+1
   100                          y	= $AA		; start coordinate y
   101                          
   102                          xendl	= $9E		; end coordinate x, low+high
   103                          xendh	= $9F
   104                          yend	= $93		; end coordinate y
   105                          
   106                          kl	= $95		; gradient for lines, low+high
   107                          kh	= kl+1
   108                          
   109                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
   110                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
   111                          
   112                          dxl	= $AB		; x delta, low+high
   113                          dxh	= $A7
   114                          
   115                          dy	= $A9		; y delta
   116                          ysave	= dy		; y saved (hline context)
   117                          
   118                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   119                          ylimit	= ydir		; y limit in a 8x8 block (hline context)
   120                          
   121                          cl	= $A3		; dot count, low+high
   122                          ch	= $A4
   123                          ycount	= cl		; y count overall (hline context)
   124                          hcount	= ch		; horizontal blocks (hline context)
   125                          
   126                          gaddr	= $A5		; graphic address
   127                          
   128                          gpos	= $FB		; in graphic position
   129                          sgaddr	= gpos		; saved gaddr, hline context
   130                          
   131                          gcol	= $FD		; graphic color, in "graphic on" context only
   132                          xsave	= gcol		; X register save (hline context)
   133                          
   134                          
   135                          ; static ram areas
   136                          
   137                          savexl	= $0334		; the graphic cursor: x low 
   138                          savexh	= savexl+1	; the graphic cursor: x high
   139                          savey	= savexh+1	; the graphic cursor: y
   140                          savemo	= savey+1	; the graphic mode
   141                          saveverr = savemo+1	; original v_baserr
   142                          savevstp = saveverr+2	; original v_basstp
   143                          
   144                          			; real place for gchange and gmask routines,
   145                          !ifdef ltc {
   146                          gramcode = $03ed - 26	; 15 bytes + 4*6+2
   147                          } else {
   148                          gramcode = $03ed	; 15 bytes
   149                          }
   150                          
   151                          ; LTC64 specifics
   152                          
   153                          !ifdef ltc {
   154                          
   155                          !cpu 65816
   156                          
   157                          bank4+3 = $040000
   158                          rombank+3 = $010000     ; c't
   159                          
   160                          ; c't-Karte-Kontrollregister
   161                          
   162                          memconf = bank4 or 1
   163                          mc_off  = $80                   ; CPU 816 ausschalten
   164                          mc_slow = $40                   ; CPU 1 MHz
   165                          mc_epr  = $20                   ; EPROM in Bank0
   166                          mc_sim  = $10                   ; ROM-Simulation Bit
   167                          
   168                          }
   169                          
   170                          
   171                          
   172                          ;
   173                          ; initialize extension
   174                          
   175                          init
   176  0829 a981                       LDA #<(parse)	; basic interpreter parser hook
   177  082b 8d0803                     STA v_bascmd
   178  082e a908                       LDA #>(parse)
   179  0830 8d0903                     STA v_bascmd+1
   180                          
   181  0833 ad2803                     LDA v_basstp
   182  0836 8d3a03             	STA savevstp
   183  0839 a975                       LDA #<(stop)	; basic interpreter stop hook
   184  083b 8d2803                     STA v_basstp
   185  083e ad2903                     LDA v_basstp+1
   186  0841 8d3b03             	STA savevstp+1
   187  0844 a908                       LDA #>(stop)
   188  0846 8d2903                     STA v_basstp+1
   189                          
   190  0849 ad0003                     LDA v_baserr
   191  084c 8d3803             	STA saveverr
   192  084f a96f                       LDA #<(error)	; basic interpreter error hook
   193  0851 8d0003                     STA v_baserr
   194  0854 ad0103                     LDA v_baserr+1
   195  0857 8d3903             	STA saveverr+1
   196  085a a908                       LDA #>(error)
   197  085c 8d0103                     STA v_baserr+1
   198                          
   199  085f a200               	LDX #0		; set graphic cursor to (0,0)
   200  0861 8e3403             	STX savexl
   201  0864 8e3503             	STX savexh
   202  0867 8e3603             	STX savey
   203  086a e8                 	INX
   204  086b 8e3703             	STX savemo	; set mode 1
   205  086e 60                         RTS
   206                          
   207                          error	
   208                          	; reg A may destroyed
   209  086f 204509             	JSR gra_off		; uses only reg A
   210  0872 6c3803             	JMP (saveverr)		; to original vector
   211                          
   212                          stop	
   213                          	; reg A may destroyed
   214  0875 a591               	LDA $91			; Scan code
   215  0877 c97f               	CMP #$7F		; STOP key?
   216  0879 d003               	BNE nostop
   217  087b 204509             	JSR gra_off		; uses only reg A
   218                          nostop
   219  087e 6c3a03             	JMP (savevstp)		; to original vector
   220                          
   221                          ;-----------------------------------------------------------------
   222                          
   223                          ; start parsing an extension command ...
   224                          
   225                          parse
   226  0881 207300                     JSR chrget			; next char.
   227  0884 08                 	PHP
   228  0885 c926                       CMP #'&'			; command prefix
   229  0887 f004                       BEQ newcmd
   230  0889 28                         PLP
   231  088a 4ce7a7                     JMP b_execstatement
   232                          newcmd
   233  088d 28                 	PLP
   234  088e 207300                     JSR chrget			; command character
   235                          
   236  0891 a00c                       LDY #(cmdsend-cmds)		; map character to
   237                          					; command address ...
   238                          checknextcmd
   239  0893 88                         DEY
   240  0894 f01c               	BEQ parse_error
   241  0896 d9b508                     CMP cmds,Y
   242  0899 d0f8                       BNE checknextcmd		; try next
   243  089b 88                         DEY				; found
   244  089c 98                         TYA
   245  089d 0a                         ASL				; *2
   246  089e a8                         TAY
   247                          !ifndef command_rts_tyle {
   248                          	!set co=0			; command offset in jump table
   249  089f b9c208                     LDA cmdaddr+1,Y                 ; high byte from table
   250  08a2 8556                       STA ijmp+1
   251  08a4 b9c108                     LDA cmdaddr,Y                   ; low byte from table
   252  08a7 8555                       STA ijmp
   253  08a9 207300                     JSR chrget			; read next byte in basic text
   254  08ac 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   255  08af 4caea7                     JMP b_interpreter		; continue parsing
   256                          } else {
   257                          	!set co=1			; command offset in jump table
   258                          	LDA #>(b_interpreter-1)		; return to interpreter
   259                          	PHA
   260                          	LDA #<(b_interpreter-1)
   261                          	PHA				
   262                                  LDA cmdaddr+1,Y			; command address (RTS style)
   263                                  PHA				; high byte on stack
   264                                  LDA cmdaddr,Y			; command address (RTS style)
   265                                  PHA				; low byte on stack
   266                                  JMP chrget			; read next byte in basic text 
   267                          					; and RTS to command
   268                          }
   269                          parse_error
   270  08b2 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   271                          
   272                          ;-----------------------------------------------------------------
   273                          
   274                          ; the most commonly used command placed at the end ...
   275                          
   276  08b5 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   277                          cmdsend
   278                          
   279                          cmdaddr
   280  08c1 ca0f3e09010ffd0d...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   281  08cd b50f9b0c920b010d...        !word to-co,vline-co,hline-co,line-co,plot-co
   282                          
   283  08d7 934752412d455854...author	!text 147,"GRA-EXT V"
   284  08e1 312e3235           	+version
   285  08e5 20313938362c3230...	!text " 1986,2016 JOHANN@KLASEK.AT",0
   286                          
   287                          bitmask
   288  0901 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   289                          nbitmask
   290  0909 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   291                          ytabl
   292  0911 004080c0           	!byte $00,$40,$80,$c0
   293                          ytabh
   294  0915 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   295  0919 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   296  091d eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   297  0921 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   298  0925 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   299  0929 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   300  092d fe                 	!byte gramp+$1e
   301                          
   302                          ; for horiz. line
   303                          
   304  092e ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   305                          
   306  0936 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   307                          
   308                          
   309                          ;-----------------------------------------------------------------
   310                          
   311                          graphic
   312  093e 209eb7                     JSR b_get8bit
   313  0941 e000                       CPX #$00
   314  0943 d013                       BNE gra_other
   315                          gra0			; &G 0
   316                          gra_off
   317  0945 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   318  0947 8d00dd                     STA cia_pra
   319  094a a915                       LDA #((1 <<4) + (2 <<1) + 1)
   320                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   321                          			; char addr $1000/4096 = char. ROM
   322  094c 8d18d0                     STA vic_mcr	; VIC memory control
   323  094f ad11d0                     LDA vic_cr	; VIC control register
   324  0952 29df                       AND #%11011111	; Hires mode off
   325  0954 8d11d0                     STA vic_cr
   326  0957 60                         RTS
   327                          
   328                          gra_other
   329  0958 e001                       CPX #$01
   330  095a f00f               	BEQ gra1
   331  095c e002               	CPX #$02
   332  095e f00e                       BEQ gra2
   333  0960 e004               	CPX #$04
   334  0962 f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   335  0964 e003               	CPX #$03	; &G 3 (graphic on)
   336  0966 f029               	BEQ gra_on
   337  0968 4c48b2                     JMP b_illquant	; parameter illegal
   338                          	
   339                          gra1			; &G 1
   340  096b 20a709             	JSR gra_clear
   341                          
   342                          gra2
   343  096e 20f1b7                     JSR b_getcomma8bit
   344  0971 8a                         TXA		; foreground color
   345  0972 0a                         ASL		; upper nibble
   346  0973 0a                         ASL
   347  0974 0a                         ASL
   348  0975 0a                         ASL
   349  0976 85fd                       STA gcol
   350  0978 20f1b7                     JSR b_getcomma8bit
   351  097b 8a                         TXA		; background color
   352  097c 290f                       AND #$0F
   353  097e 05fd                       ORA gcol
   354  0980 a000                       LDY #$00
   355                          cram_loop
   356  0982 9900cc                     STA cram,Y	; fill color RAM
   357  0985 9900cd                     STA cram+$100,Y
   358  0988 9900ce                     STA cram+$200,Y
   359  098b 99e8ce                     STA cram+$300-24,Y
   360  098e c8                         INY
   361  098f d0f1                       BNE cram_loop
   362                          
   363                          gra_on
   364  0991 20c609             	JSR gra_setupcode
   365                          
   366  0994 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   367  0996 8d00dd                     STA cia_pra
   368  0999 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   369  099b 8d18d0                     STA vic_mcr	; VIC memory control
   370  099e ad11d0                     LDA vic_cr	; VIC control register
   371  09a1 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   372  09a3 8d11d0                     STA vic_cr
   373  09a6 60                         RTS
   374                          
   375                          gra_clear
   376  09a7 a220                       LDX #$20	; Pages (8 KByte)
   377  09a9 a9e0                       LDA #>gram
   378  09ab 85fc                       STA gpos+1
   379  09ad a000                       LDY #$00
   380  09af 84fb                       STY gpos
   381  09b1 98                         TYA
   382                          gra_fill
   383  09b2 91fb                       STA (gpos),Y	; Loop unroll
   384  09b4 c8                         INY
   385  09b5 91fb                       STA (gpos),Y
   386  09b7 c8                         INY
   387  09b8 91fb                       STA (gpos),Y
   388  09ba c8                         INY
   389  09bb 91fb                       STA (gpos),Y
   390  09bd c8                         INY
   391  09be d0f2                       BNE gra_fill
   392  09c0 e6fc                       INC gpos+1
   393  09c2 ca                         DEX
   394  09c3 d0ed                       BNE gra_fill
   395  09c5 60                 	RTS
   396                          
   397                          gra_setupcode
   398  09c6 a229               	LDX #(gromcode_end-gromcode) ; count of bytes
   399                          gra_copycode
   400  09c8 bde909             	LDA gromcode-1,X
   401  09cb 9dec03             	STA gramcode-1,X
   402  09ce ca                 	DEX
   403  09cf d0f7               	BNE gra_copycode
   404  09d1 ad3703             	LDA savemo
   405  09d4 290f               	AND #$0F
   406  09d6 aa                 	TAX
   407  09d7 4c230e             	JMP setmode_enter	; re-apply mode to routines
   408                          				; implicit RTS
   409                          
   410                          ;-----------------------------------------------------------------
   411                          
   412                          gexit
   413  09da a501                       LDA prozport
   414  09dc 0902                       ORA #%00000010	; kernal ROM enable
   415  09de 8501                       STA prozport
   416  09e0 58                         CLI		; allow interrupts
   417  09e1 60                         RTS
   418                          
   419                          ;-----------------------------------------------------------------
   420                          
   421                          ginit
   422  09e2 a501                       LDA prozport
   423  09e4 29fd                       AND #%11111101	; Kernal ROM disable
   424  09e6 78                         SEI		; disable interrupts
   425  09e7 8501                       STA prozport
   426  09e9 60                         RTS
   427                          			; on exit Z=0
   428                          
   429                          ;-----------------------------------------------------------------
   430                          
   431                          ; These are selfmodified routines, which has to placed into RAM
   432                          ; (on every graphic "on")
   433                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   434                          
   435                          gromcode
   436                          
   437                          !pseudopc gramcode {
   438                          
   439                          ; change a graphic location
   440                          
   441                          gchange
   442                          !ifdef ltc {
   443                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   444                          	STA memconf		; damit internes RAM gelesen werden kann!
   445                          } else {
   446  09ea eaeaeaeaeaea       	!fill 6, $ea
   447                          }
   448  09f0 b1a5                       LDA (gaddr),Y
   449                          gchange_op
   450  09f2 1d0109                     ORA bitmask,X
   451  09f5 91a5                       STA (gaddr),Y
   452                          !ifdef ltc {
   453                          	LDA #mc_sim		; vollständige ROM-Simulation
   454                          	STA memconf		; wieder schnelles RAM ab $C000
   455                          } else {
   456  09f7 eaeaeaeaeaea       	!fill 6, $ea
   457                          }
   458  09fd 60                         RTS
   459                          
   460                          ; mask a graphic location 
   461                          
   462                          gmask
   463                          !ifdef ltc {
   464                          	XBA
   465                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   466                          	STA memconf		; damit internes RAM gelesen werden kann!
   467                          	XBA
   468                          } else {
   469  09fe eaeaeaeaeaeaeaea   	!fill 8, $ea
   470                          }
   471                          gmask_flip
   472  0a06 4900                       EOR #$00
   473                          gmask_op
   474  0a08 11a5                       ORA (gaddr),Y
   475  0a0a 91a5                       STA (gaddr),Y
   476                          !ifdef ltc {
   477                          	LDA #mc_sim		; vollständige ROM-Simulation
   478                          	STA memconf		; wieder schnelles RAM ab $C000
   479                          } else {
   480  0a0c eaeaeaeaeaea       	!fill 6, $ea
   481                          }
   482  0a12 60                         RTS
   483                          
   484                          }
   485                          
   486                          gromcode_end
   487                          
   488                          ;-----------------------------------------------------------------
   489                          
   490                          position
   491  0a13 a5aa                       LDA y
   492  0a15 4a                         LSR
   493  0a16 4a                         LSR
   494  0a17 4a                         LSR		; y/8
   495  0a18 a8                         TAY
   496  0a19 2903                       AND #%00000011	; (y/8) mod 4
   497  0a1b aa                         TAX
   498  0a1c a59b                       LDA xl		; x low
   499  0a1e 29f8                       AND #%11111000	; clear bit 2-0
   500  0a20 18                         CLC
   501  0a21 7d1109                     ADC ytabl,X	; addr low: y base + x part
   502  0a24 85a5                       STA gaddr
   503  0a26 a59c                       LDA xh		; addr high: x part
   504  0a28 791509                     ADC ytabh,Y	; 	+ y base
   505  0a2b 85a6                       STA gaddr+1
   506  0a2d a5aa                       LDA y		; vertical offset
   507  0a2f 2907                       AND #%00000111	; y mod 8
   508  0a31 a8                         TAY
   509  0a32 a59b                       LDA xl
   510  0a34 2907                       AND #%00000111	; x mod 8
   511  0a36 aa                         TAX		; horizonal offset
   512  0a37 60                         RTS		; (bitmask)
   513                          
   514                          
   515                          ;-----------------------------------------------------------------
   516                          
   517                          ; line y up, x right, dx < dy (case 1)
   518                          
   519                          line_up_steep
   520  0a38 20130a                     JSR position	; x,y
   521                          loop_yup_xright
   522  0a3b 20ed03                     JSR gchange	; pixel
   523                          
   524  0a3e 18                         CLC		; k += dx
   525  0a3f a595                       LDA kl
   526  0a41 65ab                       ADC dxl		; dxh is 0, because dx < dy
   527  0a43 8595                       STA kl
   528  0a45 b004                       BCS ++		; k > 255
   529                          
   530  0a47 c5a9                       CMP dy
   531  0a49 9015                       BCC +		; k >= dy ->
   532                          
   533  0a4b e5a9               ++	SBC dy		; k -= dy
   534  0a4d 8595                       STA kl
   535                          
   536  0a4f e8                         INX		; x++
   537  0a50 e008                       CPX #8
   538  0a52 d00c                       BNE +
   539                          	; C=1
   540  0a54 a200                       LDX #0		; x overflow, wrap around
   541  0a56 a5a5                       LDA gaddr	; x+8: gaddr += 8
   542  0a58 6907                       ADC #8-1	; C already set by CPX
   543  0a5a 85a5                       STA gaddr
   544  0a5c 9002                       BCC +
   545  0a5e e6a6                       INC gaddr+1
   546                          
   547  0a60 88                 +	DEY		; y--
   548  0a61 100f                       BPL +++
   549  0a63 38                         SEC		; y overflow
   550  0a64 a5a5                       LDA gaddr
   551  0a66 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   552  0a68 85a5                       STA gaddr
   553  0a6a a5a6                       LDA gaddr+1
   554  0a6c e901               	SBC #1
   555  0a6e 85a6                       STA gaddr+1
   556  0a70 a007                       LDY #7		; wrap around
   557                          
   558  0a72 c6a3               +++	DEC cl		; until c=0
   559  0a74 d0c5                       BNE loop_yup_xright
   560  0a76 4cda09                     JMP gexit
   561                          
   562                          
   563                          ;-----------------------------------------------------------------
   564                          
   565                          ; line x right, y up, dx > dy (case 2)
   566                          
   567                          line_up_flat
   568  0a79 20130a                     JSR position	; x,y
   569  0a7c a5a3               	LDA cl		; counter adjustment for
   570  0a7e f002               	BEQ +		; dec-dec-counting
   571  0a80 e6a4               	INC ch
   572                          +
   573                          loop_xright_yup
   574  0a82 20ed03                     JSR gchange	; pixel
   575                          
   576  0a85 18                         CLC		; k += dy
   577  0a86 a595                       LDA kl
   578  0a88 65a9                       ADC dy
   579  0a8a 8595                       STA kl
   580  0a8c 9002                       BCC ++
   581  0a8e e696                       INC kh
   582                          
   583  0a90 c5ab               ++	CMP dxl		; k > dx?
   584  0a92 a596                       LDA kh
   585  0a94 e5a7                       SBC dxh
   586  0a96 901a                       BCC +
   587                          
   588  0a98 8596                       STA kh		; k -= dx
   589  0a9a a595                       LDA kl
   590  0a9c e5ab                       SBC dxl
   591  0a9e 8595                       STA kl
   592                          
   593  0aa0 88                         DEY		; y--
   594  0aa1 100f                       BPL +
   595  0aa3 38                 	SEC		; C=1 not always true (SBC above)
   596  0aa4 a5a5                       LDA gaddr	; y overflow
   597  0aa6 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   598  0aa8 85a5                       STA gaddr
   599  0aaa a5a6                       LDA gaddr+1
   600  0aac e901               	SBC #1
   601  0aae 85a6                       STA gaddr+1
   602  0ab0 a007               	LDY #7		; wrap around
   603                          
   604  0ab2 e8                 +	INX		; x++
   605  0ab3 e008                       CPX #8		; x overflow?
   606  0ab5 d00c                       BNE ++
   607                          	; C=1
   608  0ab7 a200                       LDX #0		; wrap around
   609  0ab9 a5a5                       LDA gaddr	; x+8: gaddr += 8
   610  0abb 6907                       ADC #8-1	; C already set by CPX
   611  0abd 85a5                       STA gaddr
   612  0abf 9002                       BCC ++
   613  0ac1 e6a6                       INC gaddr+1
   614                          ++
   615  0ac3 c6a3               	DEC cl		; c--
   616  0ac5 d0bb                       BNE loop_xright_yup
   617  0ac7 c6a4                       DEC ch		; adjusted high which allows this
   618  0ac9 d0b7                       BNE loop_xright_yup
   619                          
   620  0acb 4cda09                     JMP gexit
   621                          
   622                          
   623                          
   624                          ;-----------------------------------------------------------------
   625                          
   626                          ; line x right, y down, dx > dy (case 3)
   627                          
   628                          line_down_flat
   629  0ace 20130a                     JSR position	; x,y
   630  0ad1 a5a3               	LDA cl		; counter adjustment for
   631  0ad3 f002               	BEQ +		; dec-dec-counting
   632  0ad5 e6a4               	INC ch
   633                          +
   634                          loop_xright_ydown
   635  0ad7 20ed03                     JSR gchange	; pixel
   636                          
   637  0ada 18                         CLC		; k += dy
   638  0adb a595                       LDA kl
   639  0add 65a9                       ADC dy
   640  0adf 8595                       STA kl
   641  0ae1 9002                       BCC ++
   642  0ae3 e696                       INC kh
   643                          
   644  0ae5 c5ab               ++	CMP dxl		; k > dx
   645  0ae7 a596                       LDA kh
   646  0ae9 e5a7                       SBC dxh		; k -= dx
   647  0aeb 901b                       BCC +
   648                          
   649  0aed 8596                       STA kh
   650  0aef a595                       LDA kl
   651  0af1 e5ab                       SBC dxl
   652  0af3 8595                       STA kl
   653                          
   654  0af5 c8                         INY		; y++
   655  0af6 c008                       CPY #8
   656  0af8 d00e                       BNE +
   657                          	; C=1
   658  0afa a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   659  0afc 693f                       ADC #$40-1	; C already set by CPY
   660  0afe 85a5                       STA gaddr
   661  0b00 a5a6                       LDA gaddr+1
   662  0b02 6901               	ADC #1
   663  0b04 85a6                       STA gaddr+1
   664  0b06 a000                       LDY #0		; wrap around
   665                          
   666  0b08 e8                 +	INX		; x++
   667  0b09 e008                       CPX #8		; x overflow ?
   668  0b0b d00c                       BNE +++
   669                          	; C=1
   670  0b0d a200                       LDX #$00	; wrap around
   671  0b0f a5a5                       LDA gaddr	; gaddr += 8
   672  0b11 6907                       ADC #$08-1	; C always set by CPX
   673  0b13 85a5                       STA gaddr
   674  0b15 9002                       BCC +++
   675  0b17 e6a6                       INC gaddr+1
   676                          +++
   677  0b19 c6a3               	DEC cl		; c--
   678  0b1b d0ba                       BNE loop_xright_ydown
   679  0b1d c6a4                       DEC ch		; adjusted high which allows this
   680  0b1f d0b6                       BNE loop_xright_ydown
   681                          
   682  0b21 4cda09                     JMP gexit
   683                          
   684                          
   685                          ;-----------------------------------------------------------------
   686                          
   687                          ; line y down, x right, dx < dy (case 4)
   688                          
   689                          line_down_steep
   690  0b24 20130a                     JSR position	; x,y
   691                          loop_ydown_xright
   692  0b27 20ed03                     JSR gchange	; pixel
   693                          
   694  0b2a 18                         CLC		; k += dx
   695  0b2b a595                       LDA kl
   696  0b2d 65ab                       ADC dxl		; dxh is 0, because dx < dy
   697  0b2f 8595                       STA kl
   698  0b31 b004                       BCS ++
   699  0b33 c5a9                       CMP dy		; k > dy?
   700  0b35 9015                       BCC +
   701  0b37 e5a9               ++	SBC dy		; k -= dy
   702  0b39 8595                       STA kl
   703                          
   704  0b3b e8                         INX		; x++
   705  0b3c e008                       CPX #8
   706  0b3e d00c                       BNE +		; x overflow?
   707  0b40 a200                       LDX #0		; wrap around
   708  0b42 a5a5                       LDA gaddr	; x+9: gaddr += 8
   709  0b44 6907                       ADC #8-1	; C already set by CPX
   710  0b46 85a5                       STA gaddr
   711  0b48 9002                       BCC +
   712  0b4a e6a6                       INC gaddr+1
   713                          
   714  0b4c c8                 +	INY		; y++
   715  0b4d c008                       CPY #8		; y overflow?
   716  0b4f d00e                       BNE +++
   717  0b51 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   718  0b53 693f                       ADC #$40-1	; C already set by CPY
   719  0b55 85a5                       STA gaddr
   720  0b57 a5a6                       LDA gaddr+1
   721  0b59 6901               	ADC #1
   722  0b5b 85a6                       STA gaddr+1
   723  0b5d a000                       LDY #0		; wrap around
   724                          
   725  0b5f c6a3               +++	DEC cl		; c--
   726                          			; until c=0
   727  0b61 d0c4                       BNE loop_ydown_xright
   728  0b63 4cda09                     JMP gexit
   729                          
   730                          
   731                          ;-----------------------------------------------------------------
   732                          
   733                          getcommaxy
   734  0b66 20fdae                     JSR b_getcomma	; check ","
   735                          getxy
   736  0b69 208aad                     JSR b_getval	; get X coord. value
   737  0b6c 20f7b7                     JSR b_convint
   738  0b6f c901                       CMP #>xmax
   739  0b71 900c               	BCC gcxy_xok
   740  0b73 f003                       BEQ ++		; X = $1xx
   741  0b75 20ea0d                     JSR range_error
   742                          
   743  0b78 c040               ++	CPY #<xmax	; check X low
   744  0b7a 9003                       BCC +
   745  0b7c 20ea0d                     JSR range_error
   746                          +
   747                          gcxy_xok
   748  0b7f 84fb                       STY gpos	; temporary save X coord.
   749  0b81 85fc                       STA gpos+1
   750                          
   751  0b83 20f1b7                     JSR b_getcomma8bit
   752                          			; get Y coord. value
   753  0b86 e0c8                       CPX #ymax
   754  0b88 9003                       BCC +
   755  0b8a 20ea0d                     JSR range_error
   756                          +
   757  0b8d a4fb                       LDY gpos	; restory X coord.
   758  0b8f a5fc                       LDA gpos+1
   759  0b91 60                         RTS
   760                          
   761                          
   762                          ;-----------------------------------------------------------------
   763                          
   764                          hline
   765  0b92 20690b                     JSR getxy	; get startpoint
   766  0b95 86aa                       STX y
   767  0b97 8e3603                     STX savey	; save as cursor, too
   768  0b9a 859c                       STA xh
   769  0b9c 849b                       STY xl
   770  0b9e 20fdae                     JSR b_getcomma	; get length
   771  0ba1 208aad                     JSR b_getval
   772  0ba4 20f7b7                     JSR b_convint
   773                          			; calculate end point
   774  0ba7 aa                         TAX		; save length high byte
   775  0ba8 98                         TYA		; length low byte
   776  0ba9 18                         CLC
   777  0baa 659b                       ADC xl		; low xend = x+length
   778  0bac 859e                       STA xendl
   779  0bae a8                 	TAY
   780  0baf 8a                         TXA		; high
   781  0bb0 659c                       ADC xh		; high xend = x+length
   782  0bb2 859f                       STA xendh
   783  0bb4 aa                 	TAX
   784                          
   785  0bb5 c901               	CMP #>xmax	; endpoint outside?
   786  0bb7 900a               	BCC +
   787  0bb9 d005               	BNE ++		; >=$200
   788  0bbb 98                 	TYA
   789  0bbc e940               	SBC #<xmax
   790  0bbe 9003               	BCC +
   791  0bc0 20ea0d             ++	JSR range_error
   792                          +
   793  0bc3 8e3503                     STX savexh
   794  0bc6 8c3403                     STY savexl	; also save as cursor
   795                          
   796  0bc9 a900               	LDA #0
   797  0bcb 85a3               	STA ycount
   798  0bcd 207900             	JSR $0079
   799  0bd0 f012               	BEQ +
   800  0bd2 20f1b7             	JSR b_getcomma8bit
   801  0bd5 8a                 	TXA
   802  0bd6 85a3               	STA ycount
   803  0bd8 f00a               	BEQ +
   804  0bda 18                 	CLC
   805  0bdb 65aa               	ADC y		; end position for y coord.
   806  0bdd c9c8               	CMP #ymax
   807  0bdf 9003               	BCC ++
   808  0be1 20ea0d             	JSR range_error
   809                          ++
   810                          +
   811  0be4 20e209                     JSR ginit	; map in graphic memory
   812  0be7 d01a               	BNE hl_noxswap	; ginit left with Z=0
   813                          
   814                          hline_start
   815  0be9 a59e                       LDA xendl
   816  0beb c59b                       CMP xl
   817  0bed a59f                       LDA xendh
   818  0bef e59c                       SBC xh
   819  0bf1 b010                       BCS hl_noxswap	; xend < x ->
   820                          
   821  0bf3 a69e                       LDX xendl	; swap x, xend
   822  0bf5 a59b                       LDA xl
   823  0bf7 869b                       STX xl
   824  0bf9 859e                       STA xendl
   825                          
   826  0bfb a69f                       LDX xendh
   827  0bfd a49c                       LDY xh
   828  0bff 849f                       STY xendh
   829  0c01 869c                       STX xh
   830                          hl_noxswap
   831  0c03 e6a3               	INC ycount
   832                          hl_start
   833  0c05 20130a                     JSR position	; graphic position x,y
   834                          
   835  0c08 a5a5               	LDA gaddr	; save position for vertical
   836  0c0a 85fb               	STA sgaddr
   837  0c0c a5a6               	LDA gaddr+1
   838  0c0e 85fc               	STA sgaddr+1
   839  0c10 86fd               	STX xsave
   840  0c12 84a9               	STY ysave
   841                          
   842  0c14 a59e                       LDA xendl
   843  0c16 2907                       AND #%00000111
   844  0c18 8596                       STA tmp2	; xend mod 8, mask index
   845  0c1a a59b                       LDA xl
   846  0c1c 29f8                       AND #%11111000	; (xl div 8)*8
   847  0c1e 8595                       STA tmp1
   848  0c20 a59e                       LDA xendl	; xend unmasked
   849  0c22 38                         SEC
   850  0c23 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   851  0c25 8595                       STA tmp1
   852  0c27 a59f                       LDA xendh
   853  0c29 e59c                       SBC xh
   854  0c2b 4a                         LSR		; / 8 ->  0-39
   855  0c2c a595                       LDA tmp1	; only 1 highest bit
   856  0c2e 6a                         ROR		; and 3 lower bits
   857  0c2f 4a                         LSR
   858  0c30 4a                         LSR
   859                                  		; 8-pixel-blocks count
   860  0c31 85a4               	STA hcount	; save for vertical extension
   861                           
   862                          hl_vertloop
   863  0c33 98                 	TYA		; calculate max. Y in 8x8 block
   864  0c34 18                 	CLC
   865  0c35 65a3               	ADC ycount
   866  0c37 c908               	CMP #8
   867  0c39 9002               	BCC +
   868  0c3b a908               	LDA #8
   869  0c3d 85a8               +	STA ylimit
   870                          
   871  0c3f bd2e09                     LDA maskleft,X	; starting mask
   872  0c42 8595               	STA tmp1
   873  0c44 a6a4               	LDX hcount	; how many blocks
   874                          
   875                          hl_nextblock
   876  0c46 ca                         DEX
   877                          hl_islastblock
   878  0c47 301d                       BMI hl_lastblock
   879                          			; leave loop if X<0
   880  0c49 a4a9               	LDY ysave
   881  0c4b a595               -	LDA tmp1	; mask
   882  0c4d 200104             	JSR gmask	; first with left end mask
   883  0c50 c8                 	INY		; vertical down
   884  0c51 c4a8               	CPY ylimit	; in 8x8 box
   885  0c53 d0f6               	BNE -
   886                          
   887  0c55 18                         CLC		; gaddr += 8 (one block to right)
   888  0c56 a5a5                       LDA gaddr
   889  0c58 6908                       ADC #8
   890  0c5a 85a5                       STA gaddr
   891  0c5c 9002                       BCC +
   892  0c5e e6a6                       INC gaddr+1
   893                          
   894  0c60 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   895  0c62 8595               	STA tmp1
   896  0c64 d0e0               	BNE hl_nextblock	; always
   897                          
   898                          hl_lastblock
   899  0c66 a696                       LDX tmp2	; xend mask index
   900  0c68 3d3609                     AND maskright,X ; A has current maskt combine with mask right end
   901  0c6b 8595               	STA tmp1	; mask
   902  0c6d a4a9               	LDY ysave	; start position in 8x8 block
   903  0c6f a595               -	LDA tmp1	; mask
   904  0c71 200104             	JSR gmask	; modify
   905  0c74 c8                 	INY		; vertical down
   906  0c75 c6a3               	DEC ycount	; overall y counter
   907  0c77 c4a8               	CPY ylimit
   908  0c79 d0f4               	BNE -
   909                          
   910  0c7b a5a3               	LDA ycount	; finished
   911  0c7d d003               	BNE +
   912  0c7f 4cda09                     JMP gexit	; leave
   913                          
   914  0c82 18                 +	CLC
   915  0c83 a5fb               	LDA sgaddr
   916  0c85 6940               	ADC #$40	; next 8-pixel row
   917  0c87 85fb               	STA sgaddr	; + $140 (320)
   918  0c89 85a5               	STA gaddr
   919  0c8b a5fc               	LDA sgaddr+1
   920  0c8d 6901               	ADC #$01
   921  0c8f 85fc               	STA sgaddr+1
   922  0c91 85a6               	STA gaddr+1
   923  0c93 a6fd               	LDX xsave
   924  0c95 a000               	LDY #0
   925  0c97 84a9               	STY ysave
   926  0c99 f098               	BEQ hl_vertloop
   927                          ;-----------------------------------------------------------------
   928                          
   929                          vline
   930  0c9b 20690b                     JSR getxy	; get startpoint
   931  0c9e 859c                       STA xh
   932  0ca0 8d3503                     STA savexh	; save as cursor too
   933  0ca3 849b                       STY xl
   934  0ca5 8c3403                     STY savexl
   935  0ca8 86aa                       STX y
   936                          
   937  0caa 20f1b7                     JSR b_getcomma8bit
   938                          			; get length
   939  0cad 18                         CLC		; calculate end point
   940  0cae 8a                         TXA		; length
   941                          ; DON'T-CHANGE: how long to go vertically (needed later)
   942                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   943                          ;	STA tmp1
   944  0caf 65aa                       ADC y		; length + y
   945  0cb1 c9c8                       CMP #ymax	; outside?
   946  0cb3 9003                       BCC +
   947                          vline_iq
   948  0cb5 20ea0d                     JSR range_error
   949  0cb8 8593               +	STA yend	; endpoint
   950                          
   951  0cba 8d3603             	STA savey	; set cursor y position
   952  0cbd 20e209                     JSR ginit	; map in graphic memory
   953  0cc0 d012               	BNE vl_start	; ginit left with Z=0
   954                          
   955                          vline_start
   956  0cc2 a593                       LDA yend
   957  0cc4 c5aa                       CMP y
   958  0cc6 b00a                       BCS vl_noyswap	; yend < y ->
   959  0cc8 a5aa                       LDA y		; swap y, yend
   960  0cca a693                       LDX yend
   961  0ccc 8593                       STA yend
   962  0cce 86aa                       STX y
   963  0cd0 f002               	BEQ vl_start	; always (with next branch)
   964                          	; fall through if yend is
   965                          vl_noyswap
   966  0cd2 d000                       BNE vl_start	; yend > y
   967                          ;	JMP plot_start	; y = yend -> single point
   968                          ;	JMP gexit	; no point
   969                          
   970                          vl_start
   971  0cd4 20130a                     JSR position	; graphic position x,y
   972  0cd7 bd0109                     LDA bitmask,X
   973  0cda 8596                       STA tmp2	; save mask
   974                          ; DON'T-CHANGE: replace ...
   975  0cdc 38                         SEC
   976  0cdd a593                       LDA yend
   977  0cdf e5aa                       SBC y		; vertical length
   978  0ce1 aa                         TAX
   979                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   980                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   981                          ;	LDX tmp1
   982  0ce2 e8                         INX		; +1 (exit on 0)
   983                          vl_nextline
   984  0ce3 a596                       LDA tmp2
   985  0ce5 200104                     JSR gmask	; modify 
   986  0ce8 c8                         INY		; go down
   987  0ce9 c008                       CPY #8		; 8-line wrap
   988  0ceb d00e                       BNE +
   989  0ced a5a5                       LDA gaddr	; gaddr += 320
   990  0cef 693f               	ADC #$40-1	; compensate for C = 1
   991  0cf1 85a5                       STA gaddr
   992  0cf3 a5a6                       LDA gaddr+1
   993  0cf5 6901                       ADC #$01
   994  0cf7 85a6                       STA gaddr+1
   995  0cf9 a000                       LDY #0		; wrap y offset
   996  0cfb ca                 +	DEX		; all vertical positions done?
   997  0cfc d0e5                       BNE vl_nextline
   998  0cfe 4cda09                     JMP gexit	; leave
   999                          
  1000                          
  1001                          ;-----------------------------------------------------------------
  1002                          
  1003                          line
  1004  0d01 20690b                     JSR getxy	; get startpoint
  1005  0d04 849b                       STY xl 
  1006  0d06 859c                       STA xh
  1007  0d08 86aa                       STX y
  1008                          
  1009  0d0a 20660b                     JSR getcommaxy	; get endpoint
  1010                          line_start
  1011  0d0d 8c3403                     STY savexl	; save as cursor position too
  1012  0d10 849e                       STY xendl
  1013  0d12 8d3503                     STA savexh
  1014  0d15 859f                       STA xendh
  1015  0d17 8e3603                     STX savey
  1016  0d1a 8693                       STX yend
  1017                          
  1018  0d1c 20e209                     JSR ginit	; map in graphic memory
  1019                          
  1020  0d1f a000                       LDY #$00	; initialize to 0
  1021  0d21 84a8                       STY ydir
  1022  0d23 8495                       STY kl
  1023  0d25 8496                       STY kh
  1024                          
  1025  0d27 38                         SEC
  1026  0d28 a59e                       LDA xendl	; calculate dx
  1027  0d2a e59b                       SBC xl
  1028  0d2c 85ab                       STA dxl
  1029  0d2e a59f                       LDA xendh
  1030  0d30 e59c                       SBC xh
  1031  0d32 85a7                       STA dxh
  1032                          
  1033  0d34 b025                       BCS li_xend_right
  1034                          	; dx != 0
  1035  0d36 98                         TYA		; negate dx
  1036  0d37 38                         SEC		; dx = 0 - dx
  1037  0d38 e5ab                       SBC dxl
  1038  0d3a 85ab                       STA dxl
  1039  0d3c 98                         TYA
  1040  0d3d e5a7                       SBC dxh
  1041  0d3f 85a7                       STA dxh
  1042                          			; C=0 always, needed later
  1043  0d41 a69b                       LDX xl		; swap x low
  1044  0d43 a49e                       LDY xendl
  1045  0d45 869e                       STX xendl
  1046  0d47 849b                       STY xl
  1047                          
  1048  0d49 a69c                       LDX xh		; swap x high
  1049  0d4b a49f                       LDY xendh
  1050  0d4d 869f                       STX xendh
  1051  0d4f 849c                       STY xh
  1052                          
  1053  0d51 a6aa                       LDX y		; swap y
  1054  0d53 a493                       LDY yend
  1055  0d55 8693                       STX yend
  1056  0d57 84aa                       STY y
  1057                          
  1058  0d59 9009                       BCC li_x_different
  1059                          			; C=0 always (from negation before)
  1060                          
  1061                          li_xend_right
  1062  0d5b a5ab                       LDA dxl		; dx = 0?
  1063  0d5d 05a7                       ORA dxh
  1064  0d5f d003                       BNE li_x_different
  1065  0d61 4cc20c                     JMP vline_start	; vertical line case
  1066                          
  1067                          li_x_different
  1068  0d64 38                         SEC		; calculate dy
  1069  0d65 a593                       LDA yend
  1070  0d67 e5aa                       SBC y
  1071  0d69 b006                       BCS li_y_right
  1072  0d6b 49ff                       EOR #$FF	; negate dy (two's complement)
  1073  0d6d 6901                       ADC #$01	; C=0
  1074  0d6f 85a8                       STA ydir	; flag y goes up
  1075                          
  1076                          li_y_right
  1077  0d71 85a9                       STA dy
  1078  0d73 d007                       BNE +
  1079  0d75 a900               	LDA #0
  1080  0d77 85a3               	STA ycount
  1081  0d79 4ce90b                     JMP hline_start	; horizontal line case
  1082                          +
  1083                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1084                          
  1085  0d7c a5a7                       LDA dxh		; dx > dy
  1086  0d7e d017                       BNE line_flat	; yes -> flat
  1087  0d80 a5a9                       LDA dy		; no -> steep
  1088  0d82 aa                         TAX
  1089  0d83 c5ab                       CMP dxl
  1090  0d85 9010                       BCC line_flat
  1091                          
  1092                          line_steep
  1093  0d87 e8                         INX	
  1094  0d88 86a3                       STX cl		; c = dy+1
  1095  0d8a 4a                         LSR		; k = dy/2
  1096  0d8b 8595                       STA kl
  1097  0d8d a5a8                       LDA ydir
  1098  0d8f d003                       BNE +
  1099  0d91 4c240b                     JMP line_down_steep	; y down, steep
  1100  0d94 4c380a             +	JMP line_up_steep	; y up, steep
  1101                          
  1102                          line_flat
  1103  0d97 a5a7                       LDA dxh
  1104  0d99 a8                         TAY
  1105  0d9a a6ab                       LDX dxl
  1106  0d9c e8                         INX
  1107  0d9d d001                       BNE +
  1108  0d9f c8                         INY
  1109  0da0 86a3               +	STX cl		; c = dx+1
  1110  0da2 84a4                       STY ch
  1111                          
  1112  0da4 4a                         LSR		; k = dx/2
  1113  0da5 8596                       STA kh
  1114  0da7 a5ab                       LDA dxl
  1115  0da9 6a                         ROR		; dx/2
  1116  0daa 8595                       STA kl
  1117  0dac a5a8                       LDA ydir	
  1118  0dae d003                       BNE +
  1119  0db0 4cce0a                     JMP line_down_flat	; y down, flat
  1120  0db3 4c790a             +	JMP line_up_flat	; y up, flat
  1121                          
  1122                          ;-----------------------------------------------------------------
  1123                          
  1124                          plot
  1125  0db6 20690b                     JSR getxy	; get parameter
  1126  0db9 859c                       STA xh		; save x/y
  1127  0dbb 849b                       STY xl
  1128  0dbd 86aa                       STX y
  1129  0dbf 8d3503                     STA savexh	; and store as cursor
  1130  0dc2 8c3403                     STY savexl
  1131  0dc5 8e3603                     STX savey
  1132                          
  1133                          plot_start
  1134  0dc8 20130a                     JSR position	; calculate graphical address
  1135                          
  1136  0dcb a501                       LDA prozport
  1137  0dcd 29fd                       AND #%11111101	; Kernal ROM disable
  1138  0dcf 78                         SEI			
  1139  0dd0 8501                       STA prozport
  1140                          
  1141  0dd2 20ed03                     JSR gchange	; change graphical data
  1142                          
  1143  0dd5 a501                       LDA prozport
  1144  0dd7 0902                       ORA #%00000010	; kernal ROM enable
  1145  0dd9 8501                       STA prozport
  1146  0ddb 58                         CLI
  1147  0ddc 60                         RTS
  1148                          
  1149                          ;-----------------------------------------------------------------
  1150                          
  1151                          move
  1152  0ddd 20690b                     JSR getxy	; get parameter
  1153  0de0 8d3503                     STA savexh	; just save as cursor
  1154  0de3 8c3403                     STY savexl
  1155  0de6 8e3603                     STX savey
  1156  0de9 60                         RTS
  1157                          
  1158                          
  1159                          ;-----------------------------------------------------------------
  1160                          
  1161                          ; never touch X, Y
  1162                          range_error
  1163  0dea ad3703             	LDA savemo
  1164  0ded 29f0               	AND #$F0
  1165  0def d003               	BNE +
  1166  0df1 68                 	PLA			; cleanup JSR
  1167  0df2 68                 	PLA
  1168  0df3 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1169                          				; error mode 2: cut value: control back
  1170                          				; to handle value correction
  1171  0df4 2920               +	AND #$20
  1172  0df6 d0fb               	BNE -
  1173  0df8 68                 	PLA			; cleanup JSR
  1174  0df9 68                 	PLA
  1175                          setmode_error
  1176  0dfa 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1177                          
  1178                          ;-----------------------------------------------------------------
  1179                          
  1180                          setmode
  1181  0dfd 209eb7                     JSR b_get8bit
  1182  0e00 e003                       CPX #3
  1183  0e02 9013                       BCC +			; less then 3, modification mode
  1184  0e04 e006               	CPX #6
  1185  0e06 b0f2               	BCS setmode_error	; out of range
  1186                          				; error mode
  1187  0e08 8a                 	TXA
  1188  0e09 690d               	ADC #13			; C=0, therefore -3
  1189                          				; 3-5 -> 16-18
  1190                          				; put A's bit 4-7 into savemo
  1191  0e0b 4d3703             	EOR savemo		; ********
  1192  0e0e 29f0               	AND #$F0		; ****0000
  1193  0e10 4d3703             	EOR savemo		; AAAAmmmm
  1194  0e13 8d3703             	STA savemo		; 
  1195  0e16 60                 	RTS
  1196                          
  1197  0e17 8a                 +	TXA
  1198  0e18 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1199  0e1b 290f               	AND #$0F
  1200  0e1d 4d3703             	EOR savemo
  1201  0e20 8d3703             	STA savemo
  1202                          setmode_enter
  1203  0e23 e001               	CPX #$01
  1204  0e25 b01a                       BCS set_or_toggle
  1205                          
  1206                          modereset
  1207  0e27 a909                       LDA #>(nbitmask)
  1208  0e29 8df703                     STA gchange_op+2
  1209  0e2c a909                       LDA #<(nbitmask)
  1210  0e2e 8df603                     STA gchange_op+1
  1211  0e31 a93d                       LDA #$3D		; AND abs,X
  1212  0e33 8df503                     STA gchange_op
  1213  0e36 a931                       LDA #$31		; AND (zp),Y
  1214  0e38 8d0b04                     STA gmask_op
  1215  0e3b a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1216  0e3d 8d0a04                     STA gmask_flip+1
  1217  0e40 60                         RTS
  1218                          
  1219                          set_or_toggle
  1220  0e41 d01a                       BNE modetoggle
  1221                          modeset
  1222  0e43 a909                       LDA #>(bitmask)
  1223  0e45 8df703                     STA gchange_op+2
  1224  0e48 a901                       LDA #<(bitmask)
  1225  0e4a 8df603                     STA gchange_op+1
  1226  0e4d a91d                       LDA #$1D		; OR abs,X
  1227  0e4f 8df503                     STA gchange_op
  1228  0e52 a911                       LDA #$11		; OR (zp),Y
  1229  0e54 8d0b04                     STA gmask_op
  1230  0e57 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1231  0e59 8d0a04                     STA gmask_flip+1
  1232  0e5c 60                         RTS
  1233                          
  1234                          modetoggle
  1235  0e5d a909                       LDA #>(bitmask)
  1236  0e5f 8df703                     STA gchange_op+2
  1237  0e62 a901                       LDA #<(bitmask)
  1238  0e64 8df603                     STA gchange_op+1
  1239  0e67 a95d                       LDA #$5D		; EOR abs,X
  1240  0e69 8df503                     STA gchange_op
  1241  0e6c a951                       LDA #$51		; EOR (zp),Y
  1242  0e6e 8d0b04                     STA gmask_op
  1243  0e71 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1244  0e73 8d0a04                     STA gmask_flip+1
  1245  0e76 60                         RTS
  1246                          
  1247                          
  1248                          ;-----------------------------------------------------------------
  1249                          
  1250                          ; get pixel (check if pixel set)
  1251                          ; not used
  1252                          
  1253                          get
  1254  0e77 20660b                     JSR getcommaxy
  1255  0e7a 859c                       STA xh
  1256  0e7c 849b                       STY xl
  1257  0e7e 86aa                       STX y
  1258                          
  1259  0e80 20130a                     JSR position
  1260                          
  1261  0e83 a501                       LDA prozport
  1262  0e85 29fd               	AND #%11111101	; Kernal ROM disable
  1263  0e87 78                         SEI
  1264  0e88 8501                       STA prozport
  1265                          
  1266  0e8a b1a5                       LDA (gaddr),Y
  1267  0e8c 3d0109                     AND bitmask,X
  1268  0e8f a8                         TAY
  1269  0e90 a501                       LDA prozport
  1270  0e92 0902               	ORA #%00000010	; kernal ROM enable
  1271  0e94 8501                       STA prozport
  1272  0e96 58                         CLI
  1273  0e97 4ca2b3                     JMP b_byte2fac
  1274                          
  1275                          
  1276                          ;-----------------------------------------------------------------
  1277                          
  1278                          relto
  1279  0e9a 208aad                     JSR b_getval	; get X offset (+/-)
  1280  0e9d a561               	LDA facexp	; FAC exponent
  1281  0e9f c990               	CMP #$90	; more than 16 bit
  1282  0ea1 b031               	BCS relto_error	; illegal quantity
  1283  0ea3 209bbc                     JSR b_fac2int	; to signed integer
  1284                          
  1285  0ea6 18                         CLC
  1286  0ea7 a565                       LDA facintl
  1287  0ea9 6d3403                     ADC savexl
  1288  0eac 859e                       STA xendl
  1289  0eae a564                       LDA facinth
  1290  0eb0 6d3503                     ADC savexh
  1291  0eb3 859f                       STA xendh	; xend = savex+facint
  1292                          
  1293  0eb5 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1294  0eb8 208aad                     JSR b_getval
  1295  0ebb a561                       LDA facexp	; FAC exponent
  1296  0ebd c990                       CMP #$90	; more than 16 bit
  1297  0ebf b013                       BCS relto_error	; illegal quantity
  1298  0ec1 209bbc                     JSR b_fac2int	; to signed integer
  1299  0ec4 18                         CLC
  1300  0ec5 a565                       LDA facintl
  1301  0ec7 6d3603                     ADC savey
  1302  0eca 8593                       STA yend	; yend = savey+facint
  1303                          
  1304  0ecc a59f                       LDA xendh	; check end coord. x
  1305  0ece c901                       CMP #>xmax
  1306  0ed0 900e                       BCC rt_xok
  1307  0ed2 f003                       BEQ +
  1308                          relto_error
  1309  0ed4 20ea0d                     JSR range_error
  1310  0ed7 a59e               +	LDA xendl
  1311  0ed9 c940                       CMP #<xmax
  1312  0edb 9003                       BCC +
  1313  0edd 20ea0d                     JSR range_error
  1314                          +
  1315                          rt_xok
  1316  0ee0 a593                       LDA yend	; check end coord. y
  1317  0ee2 c9c8                       CMP #ymax
  1318  0ee4 9003                       BCC +
  1319  0ee6 20ea0d                     JSR range_error
  1320                          +
  1321  0ee9 ad3403                     LDA savexl
  1322  0eec 859b                       STA xl
  1323  0eee ad3503                     LDA savexh
  1324  0ef1 859c                       STA xh
  1325  0ef3 ad3603                     LDA savey
  1326  0ef6 85aa                       STA y
  1327  0ef8 a49e                       LDY xendl
  1328  0efa a59f                       LDA xendh
  1329  0efc a693                       LDX yend	; xend/yend = cursor + x/y
  1330                          
  1331  0efe 4c0d0d                     JMP line_start	; draw line x/y to xend/yend
  1332                          
  1333                          
  1334                          ;-----------------------------------------------------------------
  1335                          
  1336                          char
  1337  0f01 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1338  0f04 e028                       CPX #40	
  1339  0f06 9003                       BCC +
  1340                          char_error
  1341  0f08 4c48b2                     JMP b_illquant
  1342  0f0b 86fb               +	STX gpos	; save x coord.
  1343  0f0d 20f1b7                     JSR b_getcomma8bit
  1344                          			; get char. position y 0-24
  1345  0f10 e019                       CPX #25
  1346  0f12 b0f4                       BCS char_error
  1347  0f14 86fc                       STX gpos+1	; save y coord.
  1348                          
  1349  0f16 20fdae                     JSR b_getcomma	; get string
  1350  0f19 209ead                     JSR b_getexpr
  1351  0f1c 20a3b6                     JSR b_stringval ; string address in str
  1352  0f1f 48                         PHA		; string length
  1353  0f20 a6fc                       LDX gpos+1	; y coord. for char. position
  1354  0f22 8a                         TXA
  1355  0f23 2903                       AND #$03	; mask 2 bits
  1356  0f25 a8                         TAY		; table index
  1357  0f26 a900                       LDA #$00
  1358  0f28 85fc                       STA gpos+1	; x high
  1359  0f2a a5fb                       LDA gpos	; saved x: multiply by 8
  1360  0f2c 0a                         ASL
  1361  0f2d 0a                         ASL
  1362  0f2e 0a                         ASL
  1363  0f2f 26fc                       ROL gpos+1	; overflow to high byte
  1364  0f31 791109                     ADC ytabl,Y
  1365  0f34 85a5                       STA gaddr
  1366  0f36 a5fc                       LDA gpos+1	; x high
  1367  0f38 7d1509                     ADC ytabh,X
  1368  0f3b 85a6                       STA gaddr+1
  1369  0f3d 68                         PLA		; string length
  1370  0f3e a000                       LDY #$00	; string index
  1371  0f40 aa                         TAX		; length
  1372  0f41 e8                         INX		; prepare as counter
  1373                          char_loop
  1374  0f42 ca                         DEX
  1375  0f43 f008                       BEQ char_exit
  1376  0f45 b122                       LDA (str),Y	; read string
  1377  0f47 204e0f                     JSR char_display
  1378  0f4a c8                         INY
  1379  0f4b d0f5                       BNE char_loop
  1380                          char_exit
  1381  0f4d 60                         RTS
  1382                          
  1383                          char_display
  1384  0f4e 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1385  0f50 8a                         TXA		; save register X+Y
  1386  0f51 48                         PHA
  1387  0f52 98                         TYA
  1388  0f53 48                         PHA
  1389  0f54 a5d7                       LDA z_tmp	; get saved character
  1390  0f56 3012                       BMI char_inverse
  1391                          
  1392                          char_normal
  1393  0f58 c920                       CMP #$20	; control character?
  1394  0f5a 9054                       BCC char_disp_leave
  1395  0f5c c960                       CMP #$60
  1396  0f5e 9004                       BCC +
  1397  0f60 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1398  0f62 d014                       BNE char_hires
  1399  0f64 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1400  0f66 d010               	BNE char_hires
  1401  0f68 f00e               	BEQ char_hires
  1402                          
  1403                          char_inverse
  1404  0f6a 297f                       AND #%01111111	; mask bit 7
  1405  0f6c c97f                       CMP #%01111111	; was 255? (pi)
  1406  0f6e d002                       BNE +
  1407  0f70 a95e                       LDA #$5E	; screen code for pi
  1408  0f72 c920               +	CMP #$20	; control character?
  1409  0f74 903a                       BCC char_disp_leave
  1410                          			; yes, skip
  1411  0f76 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1412                          			; $C0-$FF -> $40-$7F
  1413                          			; OPT: BNE char_hires
  1414                          			; OPT: char_normal
  1415                          char_hires
  1416  0f78 a6c7                       LDX z_reverseflag
  1417  0f7a f002                       BEQ +
  1418  0f7c 0980                       ORA #%10000000	; invert char.
  1419  0f7e aa                 +	TAX		; save char. for later
  1420  0f7f a501                       LDA prozport	; save prozport state
  1421  0f81 48                 	PHA
  1422  0f82 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1423  0f84 78                         SEI
  1424  0f85 8501                       STA prozport	; char. rom base = $D000
  1425  0f87 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1426  0f89 85fc                       STA gpos+1	; 
  1427  0f8b 8a                         TXA		; char. code
  1428  0f8c 0a                         ASL		; *8
  1429  0f8d 26fc                       ROL gpos+1
  1430  0f8f 0a                         ASL
  1431  0f90 26fc                       ROL gpos+1
  1432  0f92 0a                         ASL
  1433  0f93 26fc                       ROL gpos+1
  1434  0f95 85fb                       STA gpos	; addr. in char. rom for char.
  1435                          
  1436  0f97 a007                       LDY #$07	; 8 hires lines
  1437                          char_line
  1438  0f99 b1fb                       LDA (gpos),Y	; read character line
  1439  0f9b 200104                     JSR gmask	; write to hires screen
  1440  0f9e 88                         DEY
  1441  0f9f 10f8                       BPL char_line
  1442                          
  1443  0fa1 68                 	PLA
  1444  0fa2 8501                       STA prozport
  1445  0fa4 58                         CLI
  1446                          
  1447  0fa5 18                         CLC		; step char position to left
  1448  0fa6 a5a5                       LDA gaddr	; ( +8 )
  1449  0fa8 6908                       ADC #$08
  1450  0faa 85a5                       STA gaddr
  1451  0fac 9002                       BCC +
  1452  0fae e6a6                       INC gaddr+1
  1453                          +
  1454                          char_disp_leave
  1455  0fb0 68                 	PLA		; pass written character back
  1456  0fb1 a8                         TAY		; restore saved registers
  1457  0fb2 68                         PLA
  1458  0fb3 aa                         TAX
  1459  0fb4 60                         RTS
  1460                          
  1461                          
  1462                          ;-----------------------------------------------------------------
  1463                          
  1464                          to
  1465  0fb5 ad3403                     LDA savexl
  1466  0fb8 859b                       STA xl
  1467  0fba ad3503                     LDA savexh
  1468  0fbd 859c                       STA xh
  1469  0fbf ad3603                     LDA savey
  1470  0fc2 85aa                       STA y
  1471  0fc4 20690b                     JSR getxy
  1472  0fc7 4c0d0d                     JMP line_start
  1473                          
  1474                          ;-----------------------------------------------------------------
  1475                          
  1476                          unnew
  1477                          
  1478  0fca a52b               	lda bassta
  1479  0fcc 8522               	sta str
  1480  0fce a52c               	lda bassta+1
  1481  0fd0 8523               	sta str+1
  1482  0fd2 a001               	ldy #1
  1483  0fd4 98                 	tya
  1484  0fd5 9122               	sta (str),y		; != 0
  1485                          
  1486  0fd7 2033a5             	jsr b_rechain		; starting from bassta
  1487                          				; result in (str)
  1488  0fda 18                 	clc			; str+1 -> new basic end
  1489  0fdb a423               	ldy str+1
  1490  0fdd a522               	lda str
  1491  0fdf 6902               	adc #2
  1492  0fe1 852d               	sta basend
  1493  0fe3 9001               	bcc +
  1494  0fe5 c8                 	iny
  1495  0fe6 842e               +	sty basend+1
  1496  0fe8 4c60a6             	jmp b_clr		; perform CLR
  1497                          
  1498                          ;-----------------------------------------------------------------
  1499                          graext_end

; ******** Source: ge-run.asm
    43                          
    44                          
