
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
    29  080d a23f               	ldx #<graext_end	; setup basic
    30  080f a010               	ldy #>graext_end
    31  0811 18                 	clc			; set if C=0
    32  0812 209cff             	jsr $ff9c		; KERNAL: system RAM bottom
    33  0815 862b               	stx $2b			; BASIC text start
    34  0817 842c               	sty $2c
    35  0819 2016e4             	jsr $e416		; setup BASIC text start
    36  081c 202908             	jsr init		; init extension (place hook)
    37  081f a920               	lda #<author		; message ...
    38  0821 a009               	ldy #>author
    39  0823 2041e4             	jsr $e441		; output string and perform BASIC NEW (set remaining pointers)
    40  0826 4c86e3             	jmp $e386		; BASIC warm start
    41                          

; ******** Source: graext-core.asm
     1                          ;  **** gra-ext core ****
     2                          ;
     3                          ; 2015-10-05 johann e. klasek, johann at klasek at
     4                          ;
     5                          !macro version {
     6                          	!text "1.27" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2016-07-13 v 1.27
    10                          ;	2016-06-21 v 1.25
    11                          ;	2016-06-16 v 1.24
    12                          ;	2016-05-29 v 1.23
    13                          ;	2016-05-20 v 1.22
    14                          ;	2016-05-16 v 1.21
    15                          ;	2016-02-23 v 1.20
    16                          ;	2016-01-15 v 1.19
    17                          ;	1992-12-28 v 1.18
    18                          ;	1986-03-24 v 1.17
    19                          ;	1985       v 0.00 - 1.16
    20                          ;
    21                          ; the original source has been lost.
    22                          ; development has based on the implemention
    23                          ; done on a forth-64 written with its forth assembler.
    24                          ; the code has been pulled out from there and enriched
    25                          ; with some glue code to get a basic extension.
    26                          
    27                          ; command dispatcher style JMP/RTS
    28                          ;	(if defined)
    29                          ;command_rts_style=1
    30                          
    31                          ; error handling 
    32                          ;	(if defined)
    33                          ;no_error=1
    34                          
    35                          ; basic interpreter registers, addresses and entry points
    36                          
    37                          type	= $0d
    38                          str     = $22		; string address
    39                          bassta	= $2b		; basic start pointer
    40                          basend	= $2d		; basic end pointer
    41                          ijmp    = $55		; address of JMP (addr)
    42                          chrget  = $73		; basic charget routine
    43                          chrgot  = $79		; basic last char got (charget routine)
    44                          txtptr	= $7A		; basic text pointer
    45                          facintl = $65		; integer result from b_fac2int
    46                          facinth = $64
    47                          facexp  = $61		; fac exponent, after b_getval
    48                          
    49                          z_reverseflag = $C7	; character routine
    50                          z_lastkey = $D7		; original use case, unused here
    51                          z_tmp = z_lastkey	; temporary reused for character routine
    52                          
    53                          v_baserr = $0300	; vector error routine
    54                          v_basstp = $0328	; vector error routine
    55                          v_bascmd = $0308	; vector interpreter parsing
    56                          v_basexp = $030a	; vector evaluate expression
    57                          
    58                          basic_rom = $A000	; start of BASIC ROM
    59                          
    60                          b_clr = $A660		; CLR command
    61                          b_interpreter = $A7AE	; interpreter loop
    62                          b_execstatement = $A7E7	; process statement
    63                          b_execexpr =$AE92	; process expression
    64                          b_getcomma = $AEFD	; read comma from basic text
    65                          b_illquant = $B248	; error "illegal quantity"
    66                          b_syntaxerror = $AF08	; error "syntax"
    67                          b_get8bit = $B79E	; read 8 bit numeric value from
    68                          			; basic text
    69                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    70                          			; from basic text
    71                          b_getval = $AD8A	; read numeric value from basic text
    72                          b_getexpr = $AD9E	; read expression from basic text
    73                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    74                          b_word2fac =$B391	; convert Y/Y to FAC (signed 16 bit)
    75                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    76                          b_fac2int = $BC9B	; convert FAC to integer
    77                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    78                          b_rechain = $A533	; rechain basic lines
    79                          b_str2fac = $BCF3	; convert string in FAC (expression handling)
    80                          b_chkparl = $AEFA 	; check '('
    81                          b_chkparr = $AEF7 	; check ')'
    82                          
    83                          ; hardware registers and values
    84                          
    85                          prozport = $01		; processor port
    86                          memrom = %00110111	; basic+kernal rom
    87                          membas = %00110110	; basic ram+kernal rom
    88                          memram = %00110101	; basic+kernal ram
    89                          
    90                          vic_cr	= $D011		; VIC control register
    91                          vic_mcr	= $D018		; VIC memory control register
    92                          cia_pra	= $DD00		; CIA 2 port register A
    93                          
    94                          cram	= $CC00		; start of color ram
    95                          
    96                          gram	= $e000		; start of graphic bitmap ram
    97                          gramp	= gram >> 8	; start page of bitmap
    98                          
    99                          ; constants 
   100                          
   101                          xmax	= 320		; max x dimension
   102                          ymax	= 200		; max y dimension
   103                          
   104                          ; zeropage variables
   105                          
   106                          x	= $9B		; start coordinate x, low+high
   107                          xl	= x
   108                          xh	= x+1
   109                          y	= $AA		; start coordinate y
   110                          
   111                          xendl	= $9E		; end coordinate x, low+high
   112                          xendh	= $9F
   113                          yend	= $93		; end coordinate y
   114                          
   115                          kl	= $95		; gradient for lines, low+high
   116                          kh	= kl+1
   117                          
   118                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
   119                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
   120                          
   121                          dxl	= $AB		; x delta, low+high
   122                          dxh	= $A7
   123                          
   124                          dy	= $A9		; y delta
   125                          ysave	= dy		; y saved (hline context)
   126                          
   127                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   128                          ylimit	= ydir		; y limit in a 8x8 block (hline context)
   129                          
   130                          cl	= $A3		; dot count, low+high
   131                          ch	= $A4
   132                          ycount	= cl		; y count overall (hline context)
   133                          hcount	= ch		; horizontal blocks (hline context)
   134                          
   135                          gaddr	= $A5		; graphic address
   136                          
   137                          gpos	= $FB		; in graphic position
   138                          sgaddr	= gpos		; saved gaddr, hline context
   139                          
   140                          gcol	= $FD		; graphic color, in "graphic on" context only
   141                          xsave	= gcol		; X register save (hline context)
   142                          
   143                          
   144                          ; static ram areas
   145                          
   146                          saveverr = $0334	; original v_baserr
   147                          savevstp = saveverr+2	; original v_basstp
   148                          savevexp = savevstp+2	; original v_basexp
   149                          savexl	= savevexp+2	; the graphic cursor: x low 
   150                          savexh	= savexl+1	; the graphic cursor: x high
   151                          savey	= savexh+1	; the graphic cursor: y
   152                          savemo	= savey+1	; the graphic mode
   153                          saveend = savemo+1	; byte after save area
   154                          
   155                          			; real place for gchange and gmask routines,
   156                          !ifdef ltc {
   157                          gramcode = $03ed - 26	; 15 bytes + 4*6+2
   158                          } else {
   159                          gramcode = $03ed	; 15 bytes
   160                          }
   161                          
   162                          ; LTC64 specifics
   163                          
   164                          !ifdef ltc {
   165                          
   166                          !cpu 65816
   167                          
   168                          bank4+3 = $040000
   169                          rombank+3 = $010000     ; c't
   170                          
   171                          ; c't-Karte-Kontrollregister
   172                          
   173                          memconf = bank4 or 1
   174                          mc_off  = $80                   ; CPU 816 ausschalten
   175                          mc_slow = $40                   ; CPU 1 MHz
   176                          mc_epr  = $20                   ; EPROM in Bank0
   177                          mc_sim  = $10                   ; ROM-Simulation Bit
   178                          
   179                          }
   180                          
   181                          
   182                          
   183                          ;
   184                          ; initialize extension
   185                          
   186                          init
   187  0829 a997                       LDA #<(parse)	; basic interpreter parser hook
   188  082b 8d0803                     STA v_bascmd	; for commands
   189  082e a908                       LDA #>(parse)
   190  0830 8d0903                     STA v_bascmd+1
   191                          
   192  0833 ad0a03                     LDA v_basexp	; basic interpreter parser hook
   193  0836 8d3803             	STA savevexp	; for expressions
   194  0839 a9cb                       LDA #<(express) ; with save of old pointer
   195  083b 8d0a03                     STA v_basexp
   196  083e ad0b03                     LDA v_basexp+1
   197  0841 8d3903             	STA savevexp+1
   198  0844 a908                       LDA #>(express)
   199  0846 8d0b03                     STA v_basexp+1
   200                          
   201  0849 ad2803                     LDA v_basstp
   202  084c 8d3603             	STA savevstp
   203  084f a98b                       LDA #<(stop)	; basic interpreter stop hook
   204  0851 8d2803                     STA v_basstp
   205  0854 ad2903                     LDA v_basstp+1
   206  0857 8d3703             	STA savevstp+1
   207  085a a908                       LDA #>(stop)
   208  085c 8d2903                     STA v_basstp+1
   209                          
   210  085f ad0003                     LDA v_baserr
   211  0862 8d3403             	STA saveverr
   212  0865 a985                       LDA #<(error)	; basic interpreter error hook
   213  0867 8d0003                     STA v_baserr
   214  086a ad0103                     LDA v_baserr+1
   215  086d 8d3503             	STA saveverr+1
   216  0870 a908                       LDA #>(error)
   217  0872 8d0103                     STA v_baserr+1
   218                          
   219  0875 a200               	LDX #0		; set graphic cursor to (0,0)
   220  0877 8e3a03             	STX savexl
   221  087a 8e3b03             	STX savexh
   222  087d 8e3c03             	STX savey
   223  0880 e8                 	INX
   224  0881 8e3d03             	STX savemo	; set mode 1
   225  0884 60                         RTS
   226                          
   227                          error	
   228                          	; reg A may destroyed
   229  0885 208e09             	JSR gra_off		; uses only reg A
   230  0888 6c3403             	JMP (saveverr)		; to original vector
   231                          
   232                          stop	
   233                          	; reg A may destroyed
   234  088b a591               	LDA $91			; Scan code
   235  088d c97f               	CMP #$7F		; STOP key?
   236  088f d003               	BNE nostop
   237  0891 208e09             	JSR gra_off		; uses only reg A
   238                          nostop
   239  0894 6c3603             	JMP (savevstp)		; to original vector
   240                          
   241                          ;-----------------------------------------------------------------
   242                          
   243                          ; start parsing an extension command ...
   244                          
   245                          parse
   246  0897 207300                     JSR chrget			; next char.
   247  089a 08                 	PHP
   248  089b c926                       CMP #'&'			; command prefix
   249  089d f004                       BEQ newcmd
   250  089f 28                         PLP
   251  08a0 4ce7a7                     JMP b_execstatement
   252                          newcmd
   253  08a3 28                 	PLP
   254  08a4 207300                     JSR chrget			; command character
   255                          
   256  08a7 a00c                       LDY #(cmdsend-cmds)		; map character to
   257                          					; command address ...
   258                          checknextcmd
   259  08a9 88                         DEY
   260  08aa f01c               	BEQ parse_error
   261  08ac d9fe08                     CMP cmds,Y
   262  08af d0f8                       BNE checknextcmd		; try next
   263  08b1 88                         DEY				; found
   264  08b2 98                         TYA
   265  08b3 0a                         ASL				; *2
   266  08b4 a8                         TAY
   267                          !ifndef command_rts_tyle {
   268                          	!set co=0			; command offset in jump table
   269  08b5 b90b09                     LDA cmdaddr+1,Y                 ; high byte from table
   270  08b8 8556                       STA ijmp+1
   271  08ba b90a09                     LDA cmdaddr,Y                   ; low byte from table
   272  08bd 8555                       STA ijmp
   273  08bf 207300                     JSR chrget			; read next byte in basic text
   274  08c2 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   275  08c5 4caea7                     JMP b_interpreter		; continue parsing
   276                          } else {
   277                          	!set co=1			; command offset in jump table
   278                          	LDA #>(b_interpreter-1)		; return to interpreter
   279                          	PHA
   280                          	LDA #<(b_interpreter-1)
   281                          	PHA				
   282                                  LDA cmdaddr+1,Y			; command address (RTS style)
   283                                  PHA				; high byte on stack
   284                                  LDA cmdaddr,Y			; command address (RTS style)
   285                                  PHA				; low byte on stack
   286                                  JMP chrget			; read next byte in basic text 
   287                          					; and RTS to command
   288                          }
   289                          parse_error
   290  08c8 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   291                          
   292                          ;-----------------------------------------------------------------
   293                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   294                          express
   295  08cb a900               	LDA #0
   296  08cd 850d               	STA type	
   297  08cf 207300             	JSR chrget
   298  08d2 b003               	BCS exp_nonumber
   299  08d4 4cf3bc             	JMP b_str2fac
   300                          exp_nonumber
   301  08d7 c926                       CMP #'&'			; command prefix
   302  08d9 f00b                       BEQ newfunc
   303  08db a57a               	LDA txtptr			; undo chrget
   304  08dd d002               	BNE +
   305  08df c67b               	DEC txtptr+1
   306  08e1 c67a               +	dec txtptr
   307  08e3 6c3803             	JMP (savevexp)			; original routine	
   308                          ;	JMP b_execexpr
   309                          newfunc
   310  08e6 207300             	jsr chrget
   311  08e9 c95a               	CMP #'Z'
   312  08eb d003               	BNE +
   313  08ed 4cba0e             	JMP get
   314  08f0 c958               +	CMP #'X'
   315  08f2 d003               	BNE +
   316  08f4 4ca50e             	JMP getposx
   317  08f7 c959               +	CMP #'Y'
   318  08f9 d0cd               	BNE parse_error
   319  08fb 4cb10e             	JMP getposy
   320                          
   321                          ;-----------------------------------------------------------------
   322                          
   323                          ; the most commonly used command placed at the end ...
   324                          
   325  08fe 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   326                          cmdsend
   327                          
   328                          cmdaddr
   329  090a 1e108709550f2b0e...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   330  0916 0910cc0cc10b2f0d...        !word to-co,vline-co,hline-co,line-co,plot-co
   331                          
   332  0920 934752412d455854...author	!text 147,"GRA-EXT V"
   333  092a 312e3237           	+version
   334  092e 20313938362c3230...	!text " 1986,2016 JOHANN@KLASEK.AT",0
   335                          
   336                          bitmask
   337  094a 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   338                          nbitmask
   339  0952 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   340                          ytabl
   341  095a 004080c0           	!byte $00,$40,$80,$c0
   342                          ytabh
   343  095e e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   344  0962 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   345  0966 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   346  096a eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   347  096e f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   348  0972 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   349  0976 fe                 	!byte gramp+$1e
   350                          
   351                          ; for horiz. line
   352                          
   353  0977 ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   354                          
   355  097f 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   356                          
   357                          
   358                          ;-----------------------------------------------------------------
   359                          
   360                          graphic
   361  0987 209eb7                     JSR b_get8bit
   362  098a e000                       CPX #$00
   363  098c d013                       BNE gra_other
   364                          gra0			; &G 0
   365                          gra_off
   366  098e a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   367  0990 8d00dd                     STA cia_pra
   368  0993 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   369                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   370                          			; char addr $1000/4096 = char. ROM
   371  0995 8d18d0                     STA vic_mcr	; VIC memory control
   372  0998 ad11d0                     LDA vic_cr	; VIC control register
   373  099b 29df                       AND #%11011111	; Hires mode off
   374  099d 8d11d0                     STA vic_cr
   375  09a0 60                         RTS
   376                          
   377                          gra_other
   378  09a1 e001                       CPX #$01
   379  09a3 f00f               	BEQ gra1
   380  09a5 e002               	CPX #$02
   381  09a7 f00e                       BEQ gra2
   382  09a9 e004               	CPX #$04
   383  09ab f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   384  09ad e003               	CPX #$03	; &G 3 (graphic on)
   385  09af f029               	BEQ gra_on
   386  09b1 4c48b2                     JMP b_illquant	; parameter illegal
   387                          	
   388                          gra1			; &G 1
   389  09b4 20f009             	JSR gra_clear
   390                          
   391                          gra2
   392  09b7 20f1b7                     JSR b_getcomma8bit
   393  09ba 8a                         TXA		; foreground color
   394  09bb 0a                         ASL		; upper nibble
   395  09bc 0a                         ASL
   396  09bd 0a                         ASL
   397  09be 0a                         ASL
   398  09bf 85fd                       STA gcol
   399  09c1 20f1b7                     JSR b_getcomma8bit
   400  09c4 8a                         TXA		; background color
   401  09c5 290f                       AND #$0F
   402  09c7 05fd                       ORA gcol
   403  09c9 a000                       LDY #$00
   404                          cram_loop
   405  09cb 9900cc                     STA cram,Y	; fill color RAM
   406  09ce 9900cd                     STA cram+$100,Y
   407  09d1 9900ce                     STA cram+$200,Y
   408  09d4 99e8ce                     STA cram+$300-24,Y
   409  09d7 c8                         INY
   410  09d8 d0f1                       BNE cram_loop
   411                          
   412                          gra_on
   413  09da 200f0a             	JSR gra_setupcode
   414                          
   415  09dd a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   416  09df 8d00dd                     STA cia_pra
   417  09e2 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   418  09e4 8d18d0                     STA vic_mcr	; VIC memory control
   419  09e7 ad11d0                     LDA vic_cr	; VIC control register
   420  09ea 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   421  09ec 8d11d0                     STA vic_cr
   422  09ef 60                         RTS
   423                          
   424                          gra_clear
   425  09f0 a220                       LDX #$20	; Pages (8 KByte)
   426  09f2 a9e0                       LDA #>gram
   427  09f4 85fc                       STA gpos+1
   428  09f6 a000                       LDY #$00
   429  09f8 84fb                       STY gpos
   430  09fa 98                         TYA
   431                          gra_fill
   432  09fb 91fb                       STA (gpos),Y	; Loop unroll
   433  09fd c8                         INY
   434  09fe 91fb                       STA (gpos),Y
   435  0a00 c8                         INY
   436  0a01 91fb                       STA (gpos),Y
   437  0a03 c8                         INY
   438  0a04 91fb                       STA (gpos),Y
   439  0a06 c8                         INY
   440  0a07 d0f2                       BNE gra_fill
   441  0a09 e6fc                       INC gpos+1
   442  0a0b ca                         DEX
   443  0a0c d0ed                       BNE gra_fill
   444  0a0e 60                 	RTS
   445                          
   446                          gra_setupcode
   447  0a0f a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   448                          gra_copycode
   449  0a11 bd320a             	LDA gromcode-1,X
   450  0a14 9dec03             	STA gramcode-1,X
   451  0a17 ca                 	DEX
   452  0a18 d0f7               	BNE gra_copycode
   453  0a1a ad3d03             	LDA savemo
   454  0a1d 290f               	AND #$0F
   455  0a1f aa                 	TAX
   456  0a20 4c510e             	JMP setmode_enter	; re-apply mode to routines
   457                          				; implicit RTS
   458                          
   459                          ;-----------------------------------------------------------------
   460                          
   461                          gexit
   462  0a23 a501                       LDA prozport
   463  0a25 0902                       ORA #%00000010	; kernal ROM enable
   464  0a27 8501                       STA prozport
   465  0a29 58                         CLI		; allow interrupts
   466  0a2a 60                         RTS
   467                          
   468                          ;-----------------------------------------------------------------
   469                          
   470                          ginit
   471  0a2b a501                       LDA prozport
   472  0a2d 29fd                       AND #%11111101	; Kernal ROM disable
   473  0a2f 78                         SEI		; disable interrupts
   474  0a30 8501                       STA prozport
   475  0a32 60                         RTS
   476                          			; on exit Z=0
   477                          
   478                          ;-----------------------------------------------------------------
   479                          
   480                          ; These are selfmodified routines, which has to placed into RAM
   481                          ; (on every graphic "on")
   482                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   483                          
   484                          gromcode
   485                          
   486                          !pseudopc gramcode {
   487                          
   488                          ; change a graphic location
   489                          
   490                          gchange
   491                          !ifdef ltc {
   492                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   493                          	STA memconf		; damit internes RAM gelesen werden kann!
   494                          }
   495  0a33 b1a5                       LDA (gaddr),Y
   496                          gchange_op
   497  0a35 1d4a09                     ORA bitmask,X
   498  0a38 91a5                       STA (gaddr),Y
   499                          !ifdef ltc {
   500                          	LDA #mc_sim		; vollständige ROM-Simulation
   501                          	STA memconf		; wieder schnelles RAM ab $C000
   502                          }
   503  0a3a 60                         RTS
   504                          
   505                          ; mask a graphic location 
   506                          
   507                          gmask
   508                          !ifdef ltc {
   509                          	XBA
   510                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   511                          	STA memconf		; damit internes RAM gelesen werden kann!
   512                          	XBA
   513                          }
   514                          gmask_flip
   515  0a3b 4900                       EOR #$00
   516                          gmask_op
   517  0a3d 11a5                       ORA (gaddr),Y
   518  0a3f 91a5                       STA (gaddr),Y
   519                          !ifdef ltc {
   520                          	LDA #mc_sim		; vollständige ROM-Simulation
   521                          	STA memconf		; wieder schnelles RAM ab $C000
   522                          }
   523  0a41 60                         RTS
   524                          
   525                          }
   526                          
   527                          gromcode_end
   528                          
   529                          ;-----------------------------------------------------------------
   530                          
   531                          position
   532  0a42 a5aa                       LDA y
   533  0a44 4a                         LSR
   534  0a45 4a                         LSR
   535  0a46 4a                         LSR		; y/8
   536  0a47 a8                         TAY
   537  0a48 2903                       AND #%00000011	; (y/8) mod 4
   538  0a4a aa                         TAX
   539  0a4b a59b                       LDA xl		; x low
   540  0a4d 29f8                       AND #%11111000	; clear bit 2-0
   541  0a4f 18                         CLC
   542  0a50 7d5a09                     ADC ytabl,X	; addr low: y base + x part
   543  0a53 85a5                       STA gaddr
   544  0a55 a59c                       LDA xh		; addr high: x part
   545  0a57 795e09                     ADC ytabh,Y	; 	+ y base
   546  0a5a 85a6                       STA gaddr+1
   547  0a5c a5aa                       LDA y		; vertical offset
   548  0a5e 2907                       AND #%00000111	; y mod 8
   549  0a60 a8                         TAY
   550  0a61 a59b                       LDA xl
   551  0a63 2907                       AND #%00000111	; x mod 8
   552  0a65 aa                         TAX		; horizonal offset
   553  0a66 60                         RTS		; (bitmask)
   554                          
   555                          
   556                          ;-----------------------------------------------------------------
   557                          
   558                          ; line y up, x right, dx < dy (case 1)
   559                          
   560                          line_up_steep
   561  0a67 20420a                     JSR position	; x,y
   562                          loop_yup_xright
   563  0a6a 20ed03                     JSR gchange	; pixel
   564                          
   565  0a6d 18                         CLC		; k += dx
   566  0a6e a595                       LDA kl
   567  0a70 65ab                       ADC dxl		; dxh is 0, because dx < dy
   568  0a72 8595                       STA kl
   569  0a74 b004                       BCS ++		; k > 255
   570                          
   571  0a76 c5a9                       CMP dy
   572  0a78 9015                       BCC +		; k >= dy ->
   573                          
   574  0a7a e5a9               ++	SBC dy		; k -= dy
   575  0a7c 8595                       STA kl
   576                          
   577  0a7e e8                         INX		; x++
   578  0a7f e008                       CPX #8
   579  0a81 d00c                       BNE +
   580                          	; C=1
   581  0a83 a200                       LDX #0		; x overflow, wrap around
   582  0a85 a5a5                       LDA gaddr	; x+8: gaddr += 8
   583  0a87 6907                       ADC #8-1	; C already set by CPX
   584  0a89 85a5                       STA gaddr
   585  0a8b 9002                       BCC +
   586  0a8d e6a6                       INC gaddr+1
   587                          
   588  0a8f 88                 +	DEY		; y--
   589  0a90 100f                       BPL +++
   590  0a92 38                         SEC		; y overflow
   591  0a93 a5a5                       LDA gaddr
   592  0a95 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   593  0a97 85a5                       STA gaddr
   594  0a99 a5a6                       LDA gaddr+1
   595  0a9b e901               	SBC #1
   596  0a9d 85a6                       STA gaddr+1
   597  0a9f a007                       LDY #7		; wrap around
   598                          
   599  0aa1 c6a3               +++	DEC cl		; until c=0
   600  0aa3 d0c5                       BNE loop_yup_xright
   601  0aa5 4c230a                     JMP gexit
   602                          
   603                          
   604                          ;-----------------------------------------------------------------
   605                          
   606                          ; line x right, y up, dx > dy (case 2)
   607                          
   608                          line_up_flat
   609  0aa8 20420a                     JSR position	; x,y
   610  0aab a5a3               	LDA cl		; counter adjustment for
   611  0aad f002               	BEQ +		; dec-dec-counting
   612  0aaf e6a4               	INC ch
   613                          +
   614                          loop_xright_yup
   615  0ab1 20ed03                     JSR gchange	; pixel
   616                          
   617  0ab4 18                         CLC		; k += dy
   618  0ab5 a595                       LDA kl
   619  0ab7 65a9                       ADC dy
   620  0ab9 8595                       STA kl
   621  0abb 9002                       BCC ++
   622  0abd e696                       INC kh
   623                          
   624  0abf c5ab               ++	CMP dxl		; k > dx?
   625  0ac1 a596                       LDA kh
   626  0ac3 e5a7                       SBC dxh
   627  0ac5 901a                       BCC +
   628                          
   629  0ac7 8596                       STA kh		; k -= dx
   630  0ac9 a595                       LDA kl
   631  0acb e5ab                       SBC dxl
   632  0acd 8595                       STA kl
   633                          
   634  0acf 88                         DEY		; y--
   635  0ad0 100f                       BPL +
   636  0ad2 38                 	SEC		; C=1 not always true (SBC above)
   637  0ad3 a5a5                       LDA gaddr	; y overflow
   638  0ad5 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   639  0ad7 85a5                       STA gaddr
   640  0ad9 a5a6                       LDA gaddr+1
   641  0adb e901               	SBC #1
   642  0add 85a6                       STA gaddr+1
   643  0adf a007               	LDY #7		; wrap around
   644                          
   645  0ae1 e8                 +	INX		; x++
   646  0ae2 e008                       CPX #8		; x overflow?
   647  0ae4 d00c                       BNE ++
   648                          	; C=1
   649  0ae6 a200                       LDX #0		; wrap around
   650  0ae8 a5a5                       LDA gaddr	; x+8: gaddr += 8
   651  0aea 6907                       ADC #8-1	; C already set by CPX
   652  0aec 85a5                       STA gaddr
   653  0aee 9002                       BCC ++
   654  0af0 e6a6                       INC gaddr+1
   655                          ++
   656  0af2 c6a3               	DEC cl		; c--
   657  0af4 d0bb                       BNE loop_xright_yup
   658  0af6 c6a4                       DEC ch		; adjusted high which allows this
   659  0af8 d0b7                       BNE loop_xright_yup
   660                          
   661  0afa 4c230a                     JMP gexit
   662                          
   663                          
   664                          
   665                          ;-----------------------------------------------------------------
   666                          
   667                          ; line x right, y down, dx > dy (case 3)
   668                          
   669                          line_down_flat
   670  0afd 20420a                     JSR position	; x,y
   671  0b00 a5a3               	LDA cl		; counter adjustment for
   672  0b02 f002               	BEQ +		; dec-dec-counting
   673  0b04 e6a4               	INC ch
   674                          +
   675                          loop_xright_ydown
   676  0b06 20ed03                     JSR gchange	; pixel
   677                          
   678  0b09 18                         CLC		; k += dy
   679  0b0a a595                       LDA kl
   680  0b0c 65a9                       ADC dy
   681  0b0e 8595                       STA kl
   682  0b10 9002                       BCC ++
   683  0b12 e696                       INC kh
   684                          
   685  0b14 c5ab               ++	CMP dxl		; k > dx
   686  0b16 a596                       LDA kh
   687  0b18 e5a7                       SBC dxh		; k -= dx
   688  0b1a 901b                       BCC +
   689                          
   690  0b1c 8596                       STA kh
   691  0b1e a595                       LDA kl
   692  0b20 e5ab                       SBC dxl
   693  0b22 8595                       STA kl
   694                          
   695  0b24 c8                         INY		; y++
   696  0b25 c008                       CPY #8
   697  0b27 d00e                       BNE +
   698                          	; C=1
   699  0b29 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   700  0b2b 693f                       ADC #$40-1	; C already set by CPY
   701  0b2d 85a5                       STA gaddr
   702  0b2f a5a6                       LDA gaddr+1
   703  0b31 6901               	ADC #1
   704  0b33 85a6                       STA gaddr+1
   705  0b35 a000                       LDY #0		; wrap around
   706                          
   707  0b37 e8                 +	INX		; x++
   708  0b38 e008                       CPX #8		; x overflow ?
   709  0b3a d00c                       BNE +++
   710                          	; C=1
   711  0b3c a200                       LDX #$00	; wrap around
   712  0b3e a5a5                       LDA gaddr	; gaddr += 8
   713  0b40 6907                       ADC #$08-1	; C always set by CPX
   714  0b42 85a5                       STA gaddr
   715  0b44 9002                       BCC +++
   716  0b46 e6a6                       INC gaddr+1
   717                          +++
   718  0b48 c6a3               	DEC cl		; c--
   719  0b4a d0ba                       BNE loop_xright_ydown
   720  0b4c c6a4                       DEC ch		; adjusted high which allows this
   721  0b4e d0b6                       BNE loop_xright_ydown
   722                          
   723  0b50 4c230a                     JMP gexit
   724                          
   725                          
   726                          ;-----------------------------------------------------------------
   727                          
   728                          ; line y down, x right, dx < dy (case 4)
   729                          
   730                          line_down_steep
   731  0b53 20420a                     JSR position	; x,y
   732                          loop_ydown_xright
   733  0b56 20ed03                     JSR gchange	; pixel
   734                          
   735  0b59 18                         CLC		; k += dx
   736  0b5a a595                       LDA kl
   737  0b5c 65ab                       ADC dxl		; dxh is 0, because dx < dy
   738  0b5e 8595                       STA kl
   739  0b60 b004                       BCS ++
   740  0b62 c5a9                       CMP dy		; k > dy?
   741  0b64 9015                       BCC +
   742  0b66 e5a9               ++	SBC dy		; k -= dy
   743  0b68 8595                       STA kl
   744                          
   745  0b6a e8                         INX		; x++
   746  0b6b e008                       CPX #8
   747  0b6d d00c                       BNE +		; x overflow?
   748  0b6f a200                       LDX #0		; wrap around
   749  0b71 a5a5                       LDA gaddr	; x+9: gaddr += 8
   750  0b73 6907                       ADC #8-1	; C already set by CPX
   751  0b75 85a5                       STA gaddr
   752  0b77 9002                       BCC +
   753  0b79 e6a6                       INC gaddr+1
   754                          
   755  0b7b c8                 +	INY		; y++
   756  0b7c c008                       CPY #8		; y overflow?
   757  0b7e d00e                       BNE +++
   758  0b80 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   759  0b82 693f                       ADC #$40-1	; C already set by CPY
   760  0b84 85a5                       STA gaddr
   761  0b86 a5a6                       LDA gaddr+1
   762  0b88 6901               	ADC #1
   763  0b8a 85a6                       STA gaddr+1
   764  0b8c a000                       LDY #0		; wrap around
   765                          
   766  0b8e c6a3               +++	DEC cl		; c--
   767                          			; until c=0
   768  0b90 d0c4                       BNE loop_ydown_xright
   769  0b92 4c230a                     JMP gexit
   770                          
   771                          
   772                          ;-----------------------------------------------------------------
   773                          
   774                          getcommaxy
   775  0b95 20fdae                     JSR b_getcomma	; check ","
   776                          getxy
   777  0b98 208aad                     JSR b_getval	; get X coord. value
   778  0b9b 20f7b7                     JSR b_convint
   779  0b9e c901                       CMP #>xmax
   780  0ba0 900c               	BCC gcxy_xok
   781  0ba2 f003                       BEQ ++		; X = $1xx
   782  0ba4 20180e                     JSR range_error
   783                          
   784  0ba7 c040               ++	CPY #<xmax	; check X low
   785  0ba9 9003                       BCC +
   786  0bab 20180e                     JSR range_error
   787                          +
   788                          gcxy_xok
   789  0bae 84fb                       STY gpos	; temporary save X coord.
   790  0bb0 85fc                       STA gpos+1
   791                          
   792  0bb2 20f1b7                     JSR b_getcomma8bit
   793                          			; get Y coord. value
   794  0bb5 e0c8                       CPX #ymax
   795  0bb7 9003                       BCC +
   796  0bb9 20180e                     JSR range_error
   797                          +
   798  0bbc a4fb                       LDY gpos	; restory X coord.
   799  0bbe a5fc                       LDA gpos+1
   800  0bc0 60                         RTS
   801                          
   802                          
   803                          ;-----------------------------------------------------------------
   804                          
   805                          hline
   806  0bc1 20980b                     JSR getxy	; get startpoint
   807  0bc4 86aa                       STX y
   808  0bc6 8e3c03                     STX savey	; save as cursor, too
   809  0bc9 859c                       STA xh
   810  0bcb 849b                       STY xl
   811  0bcd 20fdae                     JSR b_getcomma	; get length
   812  0bd0 208aad                     JSR b_getval
   813  0bd3 20f7b7                     JSR b_convint
   814                          			; calculate end point
   815  0bd6 aa                         TAX		; save length high byte
   816  0bd7 98                         TYA		; length low byte
   817  0bd8 18                         CLC
   818  0bd9 659b                       ADC xl		; low xend = x+length
   819  0bdb 859e                       STA xendl
   820  0bdd a8                 	TAY
   821  0bde 8a                         TXA		; high
   822  0bdf 659c                       ADC xh		; high xend = x+length
   823  0be1 859f                       STA xendh
   824  0be3 aa                 	TAX
   825                          
   826  0be4 c901               	CMP #>xmax	; endpoint outside?
   827  0be6 900a               	BCC +
   828  0be8 d005               	BNE ++		; >=$200
   829  0bea 98                 	TYA
   830  0beb e940               	SBC #<xmax
   831  0bed 9003               	BCC +
   832  0bef 20180e             ++	JSR range_error
   833                          +
   834  0bf2 8e3b03                     STX savexh
   835  0bf5 8c3a03                     STY savexl	; also save as cursor
   836                          
   837  0bf8 a900               	LDA #0		; default thickness 0 (means 1 pixel)
   838  0bfa 85a3               	STA ycount
   839  0bfc 207900             	JSR $0079	; chargot
   840  0bff f014               	BEQ +		; command end? no optional param.
   841  0c01 20f1b7             	JSR b_getcomma8bit
   842  0c04 8a                 	TXA		; optional 8-bit parameter
   843  0c05 85a3               	STA ycount	; hline thickness
   844  0c07 f00c               	BEQ +
   845  0c09 18                 	CLC
   846  0c0a 65aa               	ADC y		; end position for y coord.
   847  0c0c b004               	BCS +++
   848  0c0e c9c8               	CMP #ymax
   849  0c10 9003               	BCC ++
   850  0c12 20180e             +++	JSR range_error
   851                          ++
   852                          +
   853  0c15 202b0a                     JSR ginit	; map in graphic memory
   854  0c18 d01a               	BNE hl_noxswap	; ginit left with Z=0
   855                          
   856                          hline_start
   857  0c1a a59e                       LDA xendl
   858  0c1c c59b                       CMP xl
   859  0c1e a59f                       LDA xendh
   860  0c20 e59c                       SBC xh
   861  0c22 b010                       BCS hl_noxswap	; xend < x ->
   862                          
   863  0c24 a69e                       LDX xendl	; swap x, xend
   864  0c26 a59b                       LDA xl
   865  0c28 869b                       STX xl
   866  0c2a 859e                       STA xendl
   867                          
   868  0c2c a69f                       LDX xendh
   869  0c2e a49c                       LDY xh
   870  0c30 849f                       STY xendh
   871  0c32 869c                       STX xh
   872                          hl_noxswap
   873  0c34 e6a3               	INC ycount	; count to 0
   874                          hl_start
   875  0c36 20420a                     JSR position	; graphic position x,y
   876                          
   877  0c39 a5a5               	LDA gaddr	; save position for vertical
   878  0c3b 85fb               	STA sgaddr
   879  0c3d a5a6               	LDA gaddr+1
   880  0c3f 85fc               	STA sgaddr+1
   881  0c41 86fd               	STX xsave
   882  0c43 84a9               	STY ysave
   883                          
   884  0c45 a59e                       LDA xendl
   885  0c47 2907                       AND #%00000111
   886  0c49 8596                       STA tmp2	; xend mod 8, mask index
   887  0c4b a59b                       LDA xl
   888  0c4d 29f8                       AND #%11111000	; (xl div 8)*8
   889  0c4f 8595                       STA tmp1
   890  0c51 a59e                       LDA xendl	; xend unmasked
   891  0c53 38                         SEC
   892  0c54 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   893  0c56 8595                       STA tmp1
   894  0c58 a59f                       LDA xendh
   895  0c5a e59c                       SBC xh
   896  0c5c 4a                         LSR		; / 8 ->  0-39
   897  0c5d a595                       LDA tmp1	; only 1 highest bit
   898  0c5f 6a                         ROR		; and 3 lower bits
   899  0c60 4a                         LSR
   900  0c61 4a                         LSR
   901                                  		; 8-pixel-blocks count
   902  0c62 85a4               	STA hcount	; save for vertical extension
   903                           
   904                          hl_vertloop
   905  0c64 98                 	TYA		; calculate max. Y in 8x8 block
   906  0c65 18                 	CLC
   907  0c66 65a3               	ADC ycount
   908  0c68 c908               	CMP #8
   909  0c6a 9002               	BCC +
   910  0c6c a908               	LDA #8
   911  0c6e 85a8               +	STA ylimit
   912                          
   913  0c70 bd7709                     LDA maskleft,X	; starting mask
   914  0c73 8595               	STA tmp1
   915  0c75 a6a4               	LDX hcount	; how many blocks
   916                          
   917                          hl_nextblock
   918  0c77 ca                         DEX
   919                          hl_islastblock
   920  0c78 301d                       BMI hl_lastblock
   921                          			; leave loop if X<0
   922  0c7a a4a9               	LDY ysave
   923  0c7c a595               -	LDA tmp1	; mask
   924  0c7e 20f503             	JSR gmask	; first with left end mask
   925  0c81 c8                 	INY		; vertical down
   926  0c82 c4a8               	CPY ylimit	; in 8x8 box
   927  0c84 d0f6               	BNE -
   928                          
   929  0c86 18                         CLC		; gaddr += 8 (one block to right)
   930  0c87 a5a5                       LDA gaddr
   931  0c89 6908                       ADC #8
   932  0c8b 85a5                       STA gaddr
   933  0c8d 9002                       BCC +
   934  0c8f e6a6                       INC gaddr+1
   935                          
   936  0c91 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   937  0c93 8595               	STA tmp1
   938  0c95 d0e0               	BNE hl_nextblock	; always
   939                          
   940                          hl_lastblock
   941  0c97 a696                       LDX tmp2	; xend mask index
   942  0c99 3d7f09                     AND maskright,X ; A has current maskt combine with mask right end
   943  0c9c 8595               	STA tmp1	; mask
   944  0c9e a4a9               	LDY ysave	; start position in 8x8 block
   945  0ca0 a595               -	LDA tmp1	; mask
   946  0ca2 20f503             	JSR gmask	; modify
   947  0ca5 c8                 	INY		; vertical down
   948  0ca6 c6a3               	DEC ycount	; overall y counter
   949  0ca8 c4a8               	CPY ylimit
   950  0caa d0f4               	BNE -
   951                          
   952  0cac a5a3               	LDA ycount	; finished
   953  0cae d003               	BNE +		; roll-over into 8x8 block below
   954  0cb0 4c230a                     JMP gexit	; leave
   955                          
   956  0cb3 18                 +	CLC
   957  0cb4 a5fb               	LDA sgaddr
   958  0cb6 6940               	ADC #$40	; next 8-pixel row below
   959  0cb8 85fb               	STA sgaddr	; + $140 (320)
   960  0cba 85a5               	STA gaddr
   961  0cbc a5fc               	LDA sgaddr+1
   962  0cbe 6901               	ADC #$01
   963  0cc0 85fc               	STA sgaddr+1
   964  0cc2 85a6               	STA gaddr+1
   965  0cc4 a6fd               	LDX xsave	; initial mask index
   966  0cc6 a000               	LDY #0		; start on top of 8x8
   967  0cc8 84a9               	STY ysave
   968  0cca f098               	BEQ hl_vertloop
   969                          ;-----------------------------------------------------------------
   970                          
   971                          vline
   972  0ccc 20980b                     JSR getxy	; get startpoint
   973  0ccf 859c                       STA xh
   974  0cd1 8d3b03                     STA savexh	; save as cursor too
   975  0cd4 849b                       STY xl
   976  0cd6 8c3a03                     STY savexl
   977  0cd9 8693                       STX yend	; inital point is endpoint
   978                          
   979  0cdb 20f1b7                     JSR b_getcomma8bit
   980                          			; get length
   981  0cde 18                         CLC		; calculate end point
   982  0cdf 8a                         TXA		; length
   983                          ; DON'T-CHANGE: how long to go vertically (needed later)
   984                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   985                          ;	STA tmp1
   986  0ce0 6593                       ADC yend	; length + initial point is startpoint
   987  0ce2 b004               	BCS vline_iq	; > 255
   988  0ce4 c9c8                       CMP #ymax	; outside?
   989  0ce6 9003                       BCC +
   990                          vline_iq
   991  0ce8 20180e                     JSR range_error
   992  0ceb 85aa               +	STA y		; startpoint
   993                          
   994  0ced 8d3c03             	STA savey	; set cursor y position
   995  0cf0 202b0a                     JSR ginit	; map in graphic memory
   996  0cf3 d00e               	BNE vl_start	; ginit left with Z=0
   997                          			; skip following, because y, yend are already ordered
   998                          
   999                          vline_start		; entry point from line command (only)
  1000  0cf5 a5aa                       LDA y
  1001  0cf7 c593                       CMP yend
  1002  0cf9 b008                       BCS vl_noyswap	; yend > y ->
  1003  0cfb a5aa                       LDA y		; swap y, yend
  1004  0cfd a693                       LDX yend
  1005  0cff 8593                       STA yend
  1006  0d01 86aa                       STX y
  1007                          vl_noyswap
  1008                          			; startpoint is below the endpoint
  1009                          
  1010                          vl_start
  1011  0d03 20420a                     JSR position	; graphic position x,y
  1012  0d06 bd4a09                     LDA bitmask,X
  1013  0d09 8596                       STA tmp2	; save mask
  1014                          ; DON'T-CHANGE: replace ...
  1015  0d0b 38                         SEC
  1016  0d0c a5aa                       LDA y		; startpoint is greater!
  1017  0d0e e593                       SBC yend	; vertical length
  1018  0d10 aa                         TAX
  1019                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1020                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1021                          ;	LDX tmp1
  1022  0d11 e8                         INX		; +1 (exit on 0)
  1023  0d12 38                 	SEC		; for subtraction, never changed!
  1024                          vl_nextline
  1025  0d13 a596                       LDA tmp2
  1026  0d15 20f503                     JSR gmask	; modify 
  1027  0d18 88                         DEY		; go up
  1028  0d19 100e                       BPL +
  1029  0d1b a5a5                       LDA gaddr	; C=1
  1030  0d1d e940               	SBC #$40	; gaddr -= 320
  1031  0d1f 85a5                       STA gaddr
  1032  0d21 a5a6                       LDA gaddr+1
  1033  0d23 e901                       SBC #$01
  1034  0d25 85a6                       STA gaddr+1
  1035  0d27 a007                       LDY #7		; wrap y offset
  1036  0d29 ca                 +	DEX		; all vertical positions done?
  1037  0d2a d0e7                       BNE vl_nextline
  1038  0d2c 4c230a                     JMP gexit	; leave
  1039                          
  1040                          
  1041                          ;-----------------------------------------------------------------
  1042                          
  1043                          line
  1044  0d2f 20980b                     JSR getxy	; get startpoint
  1045  0d32 849b                       STY xl 
  1046  0d34 859c                       STA xh
  1047  0d36 86aa                       STX y
  1048                          
  1049  0d38 20950b                     JSR getcommaxy	; get endpoint
  1050                          line_start
  1051  0d3b 8c3a03                     STY savexl	; save as cursor position too
  1052  0d3e 849e                       STY xendl
  1053  0d40 8d3b03                     STA savexh
  1054  0d43 859f                       STA xendh
  1055  0d45 8e3c03                     STX savey
  1056  0d48 8693                       STX yend
  1057                          
  1058  0d4a 202b0a                     JSR ginit	; map in graphic memory
  1059                          
  1060  0d4d a000                       LDY #$00	; initialize to 0
  1061  0d4f 84a8                       STY ydir
  1062  0d51 8495                       STY kl
  1063  0d53 8496                       STY kh
  1064                          
  1065  0d55 38                         SEC
  1066  0d56 a59e                       LDA xendl	; calculate dx
  1067  0d58 e59b                       SBC xl
  1068  0d5a 85ab                       STA dxl
  1069  0d5c a59f                       LDA xendh
  1070  0d5e e59c                       SBC xh
  1071  0d60 85a7                       STA dxh
  1072                          
  1073  0d62 b025                       BCS li_xend_right
  1074                          	; dx != 0
  1075  0d64 98                         TYA		; negate dx
  1076  0d65 38                         SEC		; dx = 0 - dx
  1077  0d66 e5ab                       SBC dxl
  1078  0d68 85ab                       STA dxl
  1079  0d6a 98                         TYA
  1080  0d6b e5a7                       SBC dxh
  1081  0d6d 85a7                       STA dxh
  1082                          			; C=0 always, needed later
  1083  0d6f a69b                       LDX xl		; swap x low
  1084  0d71 a49e                       LDY xendl
  1085  0d73 869e                       STX xendl
  1086  0d75 849b                       STY xl
  1087                          
  1088  0d77 a69c                       LDX xh		; swap x high
  1089  0d79 a49f                       LDY xendh
  1090  0d7b 869f                       STX xendh
  1091  0d7d 849c                       STY xh
  1092                          
  1093  0d7f a6aa                       LDX y		; swap y
  1094  0d81 a493                       LDY yend
  1095  0d83 8693                       STX yend
  1096  0d85 84aa                       STY y
  1097                          
  1098  0d87 9009                       BCC li_x_different
  1099                          			; C=0 always (from negation before)
  1100                          
  1101                          li_xend_right
  1102  0d89 a5ab                       LDA dxl		; dx = 0?
  1103  0d8b 05a7                       ORA dxh
  1104  0d8d d003                       BNE li_x_different
  1105  0d8f 4cf50c                     JMP vline_start	; vertical line case
  1106                          
  1107                          li_x_different
  1108  0d92 38                         SEC		; calculate dy
  1109  0d93 a593                       LDA yend
  1110  0d95 e5aa                       SBC y
  1111  0d97 b006                       BCS li_y_right
  1112  0d99 49ff                       EOR #$FF	; negate dy (two's complement)
  1113  0d9b 6901                       ADC #$01	; C=0
  1114  0d9d 85a8                       STA ydir	; flag y goes up
  1115                          
  1116                          li_y_right
  1117  0d9f 85a9                       STA dy
  1118  0da1 d007                       BNE +
  1119  0da3 a900               	LDA #0
  1120  0da5 85a3               	STA ycount
  1121  0da7 4c1a0c                     JMP hline_start	; horizontal line case
  1122                          +
  1123                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1124                          
  1125  0daa a5a7                       LDA dxh		; dx > dy
  1126  0dac d017                       BNE line_flat	; yes -> flat
  1127  0dae a5a9                       LDA dy		; no -> steep
  1128  0db0 aa                         TAX
  1129  0db1 c5ab                       CMP dxl
  1130  0db3 9010                       BCC line_flat
  1131                          
  1132                          line_steep
  1133  0db5 e8                         INX	
  1134  0db6 86a3                       STX cl		; c = dy+1
  1135  0db8 4a                         LSR		; k = dy/2
  1136  0db9 8595                       STA kl
  1137  0dbb a5a8                       LDA ydir
  1138  0dbd d003                       BNE +
  1139  0dbf 4c530b                     JMP line_down_steep	; y down, steep
  1140  0dc2 4c670a             +	JMP line_up_steep	; y up, steep
  1141                          
  1142                          line_flat
  1143  0dc5 a5a7                       LDA dxh
  1144  0dc7 a8                         TAY
  1145  0dc8 a6ab                       LDX dxl
  1146  0dca e8                         INX
  1147  0dcb d001                       BNE +
  1148  0dcd c8                         INY
  1149  0dce 86a3               +	STX cl		; c = dx+1
  1150  0dd0 84a4                       STY ch
  1151                          
  1152  0dd2 4a                         LSR		; k = dx/2
  1153  0dd3 8596                       STA kh
  1154  0dd5 a5ab                       LDA dxl
  1155  0dd7 6a                         ROR		; dx/2
  1156  0dd8 8595                       STA kl
  1157  0dda a5a8                       LDA ydir	
  1158  0ddc d003                       BNE +
  1159  0dde 4cfd0a                     JMP line_down_flat	; y down, flat
  1160  0de1 4ca80a             +	JMP line_up_flat	; y up, flat
  1161                          
  1162                          ;-----------------------------------------------------------------
  1163                          
  1164                          plot
  1165  0de4 20980b                     JSR getxy	; get parameter
  1166  0de7 859c                       STA xh		; save x/y
  1167  0de9 849b                       STY xl
  1168  0deb 86aa                       STX y
  1169  0ded 8d3b03                     STA savexh	; and store as cursor
  1170  0df0 8c3a03                     STY savexl
  1171  0df3 8e3c03                     STX savey
  1172                          
  1173                          plot_start
  1174  0df6 20420a                     JSR position	; calculate graphical address
  1175                          
  1176  0df9 a501                       LDA prozport
  1177  0dfb 29fd                       AND #%11111101	; Kernal ROM disable
  1178  0dfd 78                         SEI			
  1179  0dfe 8501                       STA prozport
  1180                          
  1181  0e00 20ed03                     JSR gchange	; change graphical data
  1182                          
  1183  0e03 a501                       LDA prozport
  1184  0e05 0902                       ORA #%00000010	; kernal ROM enable
  1185  0e07 8501                       STA prozport
  1186  0e09 58                         CLI
  1187  0e0a 60                         RTS
  1188                          
  1189                          ;-----------------------------------------------------------------
  1190                          
  1191                          move
  1192  0e0b 20980b                     JSR getxy	; get parameter
  1193  0e0e 8d3b03                     STA savexh	; just save as cursor
  1194  0e11 8c3a03                     STY savexl
  1195  0e14 8e3c03                     STX savey
  1196  0e17 60                         RTS
  1197                          
  1198                          
  1199                          ;-----------------------------------------------------------------
  1200                          
  1201                          ; never touch X, Y
  1202                          range_error
  1203  0e18 ad3d03             	LDA savemo
  1204  0e1b 29f0               	AND #$F0
  1205  0e1d d003               	BNE +
  1206  0e1f 68                 	PLA			; cleanup JSR
  1207  0e20 68                 	PLA
  1208  0e21 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1209                          				; error mode 2: cut value: control back
  1210                          				; to handle value correction
  1211  0e22 2920               +	AND #$20
  1212  0e24 d0fb               	BNE -
  1213  0e26 68                 	PLA			; cleanup JSR
  1214  0e27 68                 	PLA
  1215                          setmode_error
  1216  0e28 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1217                          
  1218                          ;-----------------------------------------------------------------
  1219                          
  1220                          setmode
  1221  0e2b 209eb7                     JSR b_get8bit
  1222  0e2e e003                       CPX #3
  1223  0e30 9013                       BCC +			; less then 3, modification mode
  1224  0e32 e006               	CPX #6
  1225  0e34 b0f2               	BCS setmode_error	; out of range
  1226                          				; error mode
  1227  0e36 8a                 	TXA
  1228  0e37 690d               	ADC #13			; C=0, therefore -3
  1229                          				; 3-5 -> 16-18
  1230                          				; put A's bit 4-7 into savemo
  1231  0e39 4d3d03             	EOR savemo		; ********
  1232  0e3c 29f0               	AND #$F0		; ****0000
  1233  0e3e 4d3d03             	EOR savemo		; AAAAmmmm
  1234  0e41 8d3d03             	STA savemo		; 
  1235  0e44 60                 	RTS
  1236                          
  1237  0e45 8a                 +	TXA
  1238  0e46 4d3d03             	EOR savemo		; put A's bit 0-3 into savemo
  1239  0e49 290f               	AND #$0F
  1240  0e4b 4d3d03             	EOR savemo
  1241  0e4e 8d3d03             	STA savemo
  1242                          setmode_enter
  1243  0e51 e001               	CPX #$01
  1244  0e53 b01a                       BCS set_or_toggle
  1245                          
  1246                          modereset
  1247  0e55 a909                       LDA #>(nbitmask)
  1248  0e57 8df103                     STA gchange_op+2
  1249  0e5a a952                       LDA #<(nbitmask)
  1250  0e5c 8df003                     STA gchange_op+1
  1251  0e5f a93d                       LDA #$3D		; AND abs,X
  1252  0e61 8def03                     STA gchange_op
  1253  0e64 a931                       LDA #$31		; AND (zp),Y
  1254  0e66 8df703                     STA gmask_op
  1255  0e69 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1256  0e6b 8df603                     STA gmask_flip+1
  1257  0e6e 60                         RTS
  1258                          
  1259                          set_or_toggle
  1260  0e6f d01a                       BNE modetoggle
  1261                          modeset
  1262  0e71 a909                       LDA #>(bitmask)
  1263  0e73 8df103                     STA gchange_op+2
  1264  0e76 a94a                       LDA #<(bitmask)
  1265  0e78 8df003                     STA gchange_op+1
  1266  0e7b a91d                       LDA #$1D		; OR abs,X
  1267  0e7d 8def03                     STA gchange_op
  1268  0e80 a911                       LDA #$11		; OR (zp),Y
  1269  0e82 8df703                     STA gmask_op
  1270  0e85 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1271  0e87 8df603                     STA gmask_flip+1
  1272  0e8a 60                         RTS
  1273                          
  1274                          modetoggle
  1275  0e8b a909                       LDA #>(bitmask)
  1276  0e8d 8df103                     STA gchange_op+2
  1277  0e90 a94a                       LDA #<(bitmask)
  1278  0e92 8df003                     STA gchange_op+1
  1279  0e95 a95d                       LDA #$5D		; EOR abs,X
  1280  0e97 8def03                     STA gchange_op
  1281  0e9a a951                       LDA #$51		; EOR (zp),Y
  1282  0e9c 8df703                     STA gmask_op
  1283  0e9f a900                       LDA #$00		; EOR #$00, nicht invertieren
  1284  0ea1 8df603                     STA gmask_flip+1
  1285  0ea4 60                         RTS
  1286                          
  1287                          
  1288                          ;-----------------------------------------------------------------
  1289                          ; get current x cursor position
  1290                          
  1291                          getposx
  1292  0ea5 ac3a03             	LDY savexl
  1293  0ea8 ad3b03             	LDA savexh
  1294  0eab 2091b3             	JSR b_word2fac
  1295  0eae 4c7300             	JMP chrget	; last position of expression (function name)
  1296                          
  1297                          ;-----------------------------------------------------------------
  1298                          ; get current y cursor position
  1299                          
  1300                          getposy
  1301  0eb1 ac3c03             	LDY savey
  1302  0eb4 20a2b3             	JSR b_byte2fac
  1303  0eb7 4c7300             	JMP chrget	; last position of expression (function name)
  1304                          
  1305                          ;-----------------------------------------------------------------
  1306                          
  1307                          ; get pixel (check if pixel set)
  1308                          ; not used
  1309                          
  1310                          get
  1311  0eba 207300             	JSR chrget	; advance past function name
  1312  0ebd 20faae             	JSR b_chkparl	; "("?
  1313  0ec0 20980b                     JSR getxy	; get X,Y values
  1314  0ec3 859c                       STA xh
  1315  0ec5 849b                       STY xl
  1316  0ec7 86aa                       STX y
  1317  0ec9 207900             	JSR chrgot
  1318  0ecc 20f7ae             	JSR b_chkparr	; ")"?
  1319                          	
  1320                          
  1321  0ecf 20420a                     JSR position	; calculate graphic address/position
  1322                          
  1323  0ed2 a501                       LDA prozport
  1324  0ed4 29fd               	AND #%11111101	; Kernal ROM disable
  1325  0ed6 78                         SEI
  1326  0ed7 8501                       STA prozport
  1327                          
  1328  0ed9 b1a5                       LDA (gaddr),Y
  1329  0edb 3d4a09                     AND bitmask,X	; mask position
  1330  0ede a8                         TAY
  1331  0edf a501                       LDA prozport
  1332  0ee1 0902               	ORA #%00000010	; kernal ROM enable
  1333  0ee3 8501                       STA prozport
  1334  0ee5 58                         CLI
  1335  0ee6 98                 	TYA
  1336  0ee7 f002               	BEQ +
  1337  0ee9 a001               	LDY #1		; <> 0 -> alway return 1
  1338  0eeb 4ca2b3             +	JMP b_byte2fac	; still on expr.'s last character
  1339                          
  1340                          ;-----------------------------------------------------------------
  1341                          
  1342                          relto
  1343  0eee 208aad                     JSR b_getval	; get X offset (+/-)
  1344  0ef1 a561               	LDA facexp	; FAC exponent
  1345  0ef3 c990               	CMP #$90	; more than 16 bit
  1346  0ef5 b031               	BCS relto_error	; illegal quantity
  1347  0ef7 209bbc                     JSR b_fac2int	; to signed integer
  1348                          
  1349  0efa 18                         CLC
  1350  0efb a565                       LDA facintl
  1351  0efd 6d3a03                     ADC savexl
  1352  0f00 859e                       STA xendl
  1353  0f02 a564                       LDA facinth
  1354  0f04 6d3b03                     ADC savexh
  1355  0f07 859f                       STA xendh	; xend = savex+facint
  1356                          
  1357  0f09 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1358  0f0c 208aad                     JSR b_getval
  1359  0f0f a561                       LDA facexp	; FAC exponent
  1360  0f11 c990                       CMP #$90	; more than 16 bit
  1361  0f13 b013                       BCS relto_error	; illegal quantity
  1362  0f15 209bbc                     JSR b_fac2int	; to signed integer
  1363  0f18 18                         CLC
  1364  0f19 a565                       LDA facintl
  1365  0f1b 6d3c03                     ADC savey
  1366  0f1e 8593                       STA yend	; yend = savey+facint
  1367                          
  1368  0f20 a59f                       LDA xendh	; check end coord. x
  1369  0f22 c901                       CMP #>xmax
  1370  0f24 900e                       BCC rt_xok
  1371  0f26 f003                       BEQ +
  1372                          relto_error
  1373  0f28 20180e                     JSR range_error
  1374  0f2b a59e               +	LDA xendl
  1375  0f2d c940                       CMP #<xmax
  1376  0f2f 9003                       BCC +
  1377  0f31 20180e                     JSR range_error
  1378                          +
  1379                          rt_xok
  1380  0f34 a593                       LDA yend	; check end coord. y
  1381  0f36 c9c8                       CMP #ymax
  1382  0f38 9003                       BCC +
  1383  0f3a 20180e                     JSR range_error
  1384                          +
  1385  0f3d ad3a03                     LDA savexl
  1386  0f40 859b                       STA xl
  1387  0f42 ad3b03                     LDA savexh
  1388  0f45 859c                       STA xh
  1389  0f47 ad3c03                     LDA savey
  1390  0f4a 85aa                       STA y
  1391  0f4c a49e                       LDY xendl
  1392  0f4e a59f                       LDA xendh
  1393  0f50 a693                       LDX yend	; xend/yend = cursor + x/y
  1394                          
  1395  0f52 4c3b0d                     JMP line_start	; draw line x/y to xend/yend
  1396                          
  1397                          
  1398                          ;-----------------------------------------------------------------
  1399                          
  1400                          char
  1401  0f55 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1402  0f58 e028                       CPX #40	
  1403  0f5a 9003                       BCC +
  1404                          char_error
  1405  0f5c 4c48b2                     JMP b_illquant
  1406  0f5f 86fb               +	STX gpos	; save x coord.
  1407  0f61 20f1b7                     JSR b_getcomma8bit
  1408                          			; get char. position y 0-24
  1409  0f64 e019                       CPX #25
  1410  0f66 b0f4                       BCS char_error
  1411  0f68 86fc                       STX gpos+1	; save y coord.
  1412                          
  1413  0f6a 20fdae                     JSR b_getcomma	; get string
  1414  0f6d 209ead                     JSR b_getexpr
  1415  0f70 20a3b6                     JSR b_stringval ; string address in str
  1416  0f73 48                         PHA		; string length
  1417  0f74 a6fc                       LDX gpos+1	; y coord. for char. position
  1418  0f76 8a                         TXA
  1419  0f77 2903                       AND #$03	; mask 2 bits
  1420  0f79 a8                         TAY		; table index
  1421  0f7a a900                       LDA #$00
  1422  0f7c 85fc                       STA gpos+1	; x high
  1423  0f7e a5fb                       LDA gpos	; saved x: multiply by 8
  1424  0f80 0a                         ASL
  1425  0f81 0a                         ASL
  1426  0f82 0a                         ASL
  1427  0f83 26fc                       ROL gpos+1	; overflow to high byte
  1428  0f85 795a09                     ADC ytabl,Y
  1429  0f88 85a5                       STA gaddr
  1430  0f8a a5fc                       LDA gpos+1	; x high
  1431  0f8c 7d5e09                     ADC ytabh,X
  1432  0f8f 85a6                       STA gaddr+1
  1433  0f91 68                         PLA		; string length
  1434  0f92 a000                       LDY #$00	; string index
  1435  0f94 aa                         TAX		; length
  1436  0f95 e8                         INX		; prepare as counter
  1437                          char_loop
  1438  0f96 ca                         DEX
  1439  0f97 f008                       BEQ char_exit
  1440  0f99 b122                       LDA (str),Y	; read string
  1441  0f9b 20a20f                     JSR char_display
  1442  0f9e c8                         INY
  1443  0f9f d0f5                       BNE char_loop
  1444                          char_exit
  1445  0fa1 60                         RTS
  1446                          
  1447                          char_display
  1448  0fa2 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1449  0fa4 8a                         TXA		; save register X+Y
  1450  0fa5 48                         PHA
  1451  0fa6 98                         TYA
  1452  0fa7 48                         PHA
  1453  0fa8 a5d7                       LDA z_tmp	; get saved character
  1454  0faa 3012                       BMI char_inverse
  1455                          
  1456                          char_normal
  1457  0fac c920                       CMP #$20	; control character?
  1458  0fae 9054                       BCC char_disp_leave
  1459  0fb0 c960                       CMP #$60
  1460  0fb2 9004                       BCC +
  1461  0fb4 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1462  0fb6 d014                       BNE char_hires
  1463  0fb8 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1464  0fba d010               	BNE char_hires
  1465  0fbc f00e               	BEQ char_hires
  1466                          
  1467                          char_inverse
  1468  0fbe 297f                       AND #%01111111	; mask bit 7
  1469  0fc0 c97f                       CMP #%01111111	; was 255? (pi)
  1470  0fc2 d002                       BNE +
  1471  0fc4 a95e                       LDA #$5E	; screen code for pi
  1472  0fc6 c920               +	CMP #$20	; control character?
  1473  0fc8 903a                       BCC char_disp_leave
  1474                          			; yes, skip
  1475  0fca 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1476                          			; $C0-$FF -> $40-$7F
  1477                          			; OPT: BNE char_hires
  1478                          			; OPT: char_normal
  1479                          char_hires
  1480  0fcc a6c7                       LDX z_reverseflag
  1481  0fce f002                       BEQ +
  1482  0fd0 0980                       ORA #%10000000	; invert char.
  1483  0fd2 aa                 +	TAX		; save char. for later
  1484  0fd3 a501                       LDA prozport	; save prozport state
  1485  0fd5 48                 	PHA
  1486  0fd6 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1487  0fd8 78                         SEI
  1488  0fd9 8501                       STA prozport	; char. rom base = $D000
  1489  0fdb a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1490  0fdd 85fc                       STA gpos+1	; 
  1491  0fdf 8a                         TXA		; char. code
  1492  0fe0 0a                         ASL		; *8
  1493  0fe1 26fc                       ROL gpos+1
  1494  0fe3 0a                         ASL
  1495  0fe4 26fc                       ROL gpos+1
  1496  0fe6 0a                         ASL
  1497  0fe7 26fc                       ROL gpos+1
  1498  0fe9 85fb                       STA gpos	; addr. in char. rom for char.
  1499                          
  1500  0feb a007                       LDY #$07	; 8 hires lines
  1501                          char_line
  1502  0fed b1fb                       LDA (gpos),Y	; read character line
  1503  0fef 20f503                     JSR gmask	; write to hires screen
  1504  0ff2 88                         DEY
  1505  0ff3 10f8                       BPL char_line
  1506                          
  1507  0ff5 68                 	PLA
  1508  0ff6 8501                       STA prozport
  1509  0ff8 58                         CLI
  1510                          
  1511  0ff9 18                         CLC		; step char position to left
  1512  0ffa a5a5                       LDA gaddr	; ( +8 )
  1513  0ffc 6908                       ADC #$08
  1514  0ffe 85a5                       STA gaddr
  1515  1000 9002                       BCC +
  1516  1002 e6a6                       INC gaddr+1
  1517                          +
  1518                          char_disp_leave
  1519  1004 68                 	PLA		; pass written character back
  1520  1005 a8                         TAY		; restore saved registers
  1521  1006 68                         PLA
  1522  1007 aa                         TAX
  1523  1008 60                         RTS
  1524                          
  1525                          
  1526                          ;-----------------------------------------------------------------
  1527                          
  1528                          to
  1529  1009 ad3a03                     LDA savexl
  1530  100c 859b                       STA xl
  1531  100e ad3b03                     LDA savexh
  1532  1011 859c                       STA xh
  1533  1013 ad3c03                     LDA savey
  1534  1016 85aa                       STA y
  1535  1018 20980b                     JSR getxy
  1536  101b 4c3b0d                     JMP line_start
  1537                          
  1538                          ;-----------------------------------------------------------------
  1539                          
  1540                          unnew
  1541                          
  1542  101e a52b               	lda bassta
  1543  1020 8522               	sta str
  1544  1022 a52c               	lda bassta+1
  1545  1024 8523               	sta str+1
  1546  1026 a001               	ldy #1
  1547  1028 98                 	tya
  1548  1029 9122               	sta (str),y		; != 0
  1549                          
  1550  102b 2033a5             	jsr b_rechain		; starting from bassta
  1551                          				; result in (str)
  1552  102e 18                 	clc			; str+1 -> new basic end
  1553  102f a423               	ldy str+1
  1554  1031 a522               	lda str
  1555  1033 6902               	adc #2
  1556  1035 852d               	sta basend
  1557  1037 9001               	bcc +
  1558  1039 c8                 	iny
  1559  103a 842e               +	sty basend+1
  1560  103c 4c60a6             	jmp b_clr		; perform CLR
  1561                          
  1562                          ;-----------------------------------------------------------------
  1563                          graext_end

; ******** Source: ge-run.asm
    43                          
    44                          
