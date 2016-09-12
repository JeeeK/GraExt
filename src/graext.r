
; ******** Source: graext.asm
     1                          !to "graext.o",cbm	
     2                          
     3                          ;  **** gra-ext module ****
     4                          ;
     5                          ; 2016-05-18 johann e. klasek, johann at klasek at
     6                          ;
     7                          ; revisions:
     8                          ;	2016-05-18 v 1.21
     9                          ;
    10                          ;
    11                          ; Usage: SYS49152
    12                          ;
    13                          
    14                          *= $c000
    15                          
    16  c000 2cffff             	bit $FFFF
    17                          *=*-2				; back 2 byte (reset bit-argument)
    18                          magic
    19  c001 0a0b               	!scr "jk"		; magic
    20                          
    21  c003 a92a                       lda #<(parse)		; check if basic interpreter parser hook
    22  c005 cd0803                     cmp v_bascmd		; does already exist
    23  c008 d008               	bne start
    24  c00a a9c0                       lda #>(parse)
    25  c00c cd0903                     cmp v_bascmd+1
    26  c00f d001                       bne start
    27  c011 60                 	rts			; hook already in place, no start message
    28                          
    29                          start
    30  c012 201fc0                     jsr init                ; init extension (place hook)
    31  c015 a97a                       lda #<author            ; message ...
    32  c017 a0c0                       ldy #>author
    33  c019 201eab                     jsr $ab1e               ; output string 
    34  c01c 4c86e3                     jmp $e386               ; BASIC warm start
    35                          

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
   117  c01f a92a                       LDA #<(parse)	; basic interpreter parser hook
   118  c021 8d0803                     STA v_bascmd
   119  c024 a9c0                       LDA #>(parse)
   120  c026 8d0903                     STA v_bascmd+1
   121  c029 60                         RTS
   122                          
   123                          
   124                          ;-----------------------------------------------------------------
   125                          
   126                          ; start parsing an extension command ...
   127                          
   128                          parse
   129  c02a 207300                     JSR chrget			; next char.
   130  c02d 08                 	PHP
   131  c02e c926                       CMP #'&'			; command prefix
   132  c030 f004                       BEQ newcmd
   133  c032 28                         PLP
   134  c033 4ce7a7                     JMP b_execstatement
   135                          newcmd
   136  c036 28                 	PLP
   137  c037 207300                     JSR chrget			; command character
   138                          
   139  c03a a00b                       LDY #(cmdsend-cmds)		; map character to
   140                          					; command address ...
   141                          checknextcmd
   142  c03c 88                         DEY
   143  c03d f019               	BEQ parse_exit
   144  c03f d95bc0                     CMP cmds,Y
   145  c042 d0f8                       BNE checknextcmd		; try next
   146  c044 88                         DEY				; found
   147  c045 98                         TYA
   148  c046 0a                         ASL				; *2
   149  c047 a8                         TAY
   150                          !ifndef command_rts_tyle {
   151                          	!set co=0			; command offset in jump table
   152  c048 b967c0                     LDA cmdaddr+1,Y                 ; high byte from table
   153  c04b 8556                       STA ijmp+1
   154  c04d b966c0                     LDA cmdaddr,Y                   ; low byte from table
   155  c050 8555                       STA ijmp
   156  c052 207300                     JSR chrget			; read next byte in basic text
   157  c055 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
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
   172  c058 4caea7                     JMP b_interpreter		; continue parsing
   173                          
   174                          ;-----------------------------------------------------------------
   175                          
   176                          ; the most commonly used command placed at the end ...
   177                          
   178  c05b 204743534d525456...cmds	!text " GCSMRTVHLP"		; first char. is a dummy
   179                          cmdsend
   180                          
   181                          cmdaddr
   182  c066 e4c0d2c5f0c4e3c4...        !word graphic-co,char-co,setmode-co,move-co,relto-co
   183  c070 83c6a0c3efc20bc4...        !word to-co,vline-co,hline-co,line-co,plot-co
   184                          
   185  c07a 934752412d455854...author	!text 147,"GRA-EXT V1.21 1986,2016 JOHANN@KLASEK.AT",0
   186                          
   187                          bitmask
   188  c0a4 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   189                          nbitmask
   190  c0ac 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   191                          ytabl
   192  c0b4 004080c0           	!byte $00,$40,$80,$c0
   193                          ytabh
   194  c0b8 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   195  c0bc e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   196  c0c0 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   197  c0c4 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   198  c0c8 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   199  c0cc f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   200  c0d0 fe                 	!byte gramp+$1e
   201                          
   202  c0d1 3a                 savexl	!byte $3a
   203  c0d2 01                 savexh	!byte $01
   204  c0d3 71                 savey	!byte $71
   205                          
   206                          ; for horiz. line
   207                          
   208  c0d4 ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   209                          
   210  c0dc 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   211                          
   212                          
   213                          ;-----------------------------------------------------------------
   214                          
   215                          graphic
   216  c0e4 209eb7                     JSR b_get8bit
   217  c0e7 e000                       CPX #$00
   218  c0e9 d013                       BNE graphic_on
   219                          gra0			; &G 0
   220  c0eb a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   221  c0ed 8d00dd                     STA cia_pra
   222  c0f0 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   223                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   224                          			; char addr $1000/4096 = char. ROM
   225  c0f2 8d18d0                     STA vic_mcr	; VIC memory control
   226  c0f5 ad11d0                     LDA vic_cr	; VIC control register
   227  c0f8 29df                       AND #%11011111	; Hires mode off
   228  c0fa 8d11d0                     STA vic_cr
   229  c0fd 60                         RTS
   230                          graphic_on
   231  c0fe e001                       CPX #$01
   232  c100 d01f                       BNE gra2
   233                          gra1			; &G 1
   234  c102 a000                       LDY #$00
   235  c104 a220                       LDX #$20	; Pages (8 KByte)
   236  c106 a9e0                       LDA #>gram
   237  c108 85fc                       STA gpos+1
   238  c10a 84fb                       STY gpos
   239  c10c a900                       LDA #$00
   240                          gra_clear
   241  c10e 91fb                       STA (gpos),Y	; Loop unroll
   242  c110 c8                         INY
   243  c111 91fb                       STA (gpos),Y
   244  c113 c8                         INY
   245  c114 91fb                       STA (gpos),Y
   246  c116 c8                         INY
   247  c117 91fb                       STA (gpos),Y
   248  c119 c8                         INY
   249  c11a d0f2                       BNE gra_clear
   250  c11c e6fc                       INC gpos+1
   251  c11e ca                         DEX
   252  c11f d0ed                       BNE gra_clear
   253                          gra2
   254  c121 20f1b7                     JSR b_getcomma8bit
   255  c124 8a                         TXA		; foreground color
   256  c125 0a                         ASL		; upper nibble
   257  c126 0a                         ASL
   258  c127 0a                         ASL
   259  c128 0a                         ASL
   260  c129 85fd                       STA gcol
   261  c12b 20f1b7                     JSR b_getcomma8bit
   262  c12e 8a                         TXA		; background color
   263  c12f 290f                       AND #$0F
   264  c131 05fd                       ORA gcol
   265  c133 a000                       LDY #$00
   266                          cram_loop
   267  c135 9900cc                     STA cram,Y
   268  c138 9900cd                     STA cram+$100,Y
   269  c13b 9900ce                     STA cram+$200,Y
   270  c13e 99e8ce                     STA cram+$300-24,Y
   271  c141 c8                         INY
   272  c142 d0f1                       BNE cram_loop
   273  c144 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   274  c146 8d00dd                     STA cia_pra
   275  c149 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   276  c14b 8d18d0                     STA vic_mcr	; VIC memory control
   277  c14e ad11d0                     LDA vic_cr	; VIC control register
   278  c151 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   279  c153 8d11d0                     STA vic_cr
   280  c156 60                         RTS
   281                          
   282                          
   283                          ;-----------------------------------------------------------------
   284                          
   285                          gexit
   286  c157 a501                       LDA prozport
   287  c159 0902                       ORA #%00000010	; kernal ROM enable
   288  c15b 8501                       STA prozport
   289  c15d 58                         CLI		; allow interrupts
   290  c15e 60                         RTS
   291                          
   292                          ;-----------------------------------------------------------------
   293                          
   294                          ginit
   295  c15f a501                       LDA prozport
   296  c161 29fd                       AND #%11111101	; Kernal ROM disable
   297  c163 78                         SEI		; disable interrupts
   298  c164 8501                       STA prozport
   299  c166 60                         RTS
   300                          
   301                          ;-----------------------------------------------------------------
   302                          
   303                          gchange
   304  c167 b1a5                       LDA (gaddr),Y
   305                          gchange_op
   306  c169 1da4c0                     ORA bitmask,X
   307  c16c 91a5                       STA (gaddr),Y
   308  c16e 60                         RTS
   309                          
   310                          ;-----------------------------------------------------------------
   311                          
   312                          gmask
   313                          gmask_flip
   314  c16f 4900                       EOR #$00
   315                          gmask_op
   316  c171 11a5                       ORA (gaddr),Y
   317  c173 91a5                       STA (gaddr),Y
   318  c175 60                         RTS
   319                          
   320                          ;-----------------------------------------------------------------
   321                          
   322                          position
   323  c176 a5aa                       LDA y
   324  c178 4a                         LSR
   325  c179 4a                         LSR
   326  c17a 4a                         LSR		; y/8
   327  c17b a8                         TAY
   328  c17c 2903                       AND #%00000011	; (y/8) mod 4
   329  c17e aa                         TAX
   330  c17f a59b                       LDA xl		; x low
   331  c181 29f8                       AND #%11111000	; clear bit 2-0
   332  c183 18                         CLC
   333  c184 7db4c0                     ADC ytabl,X	; addr low: y base + x part
   334  c187 85a5                       STA gaddr
   335  c189 a59c                       LDA xh		; addr high: x part
   336  c18b 79b8c0                     ADC ytabh,Y	; 	+ y base
   337  c18e 85a6                       STA gaddr+1
   338  c190 a5aa                       LDA y		; vertical offset
   339  c192 2907                       AND #%00000111	; y mod 8
   340  c194 a8                         TAY
   341  c195 a59b                       LDA xl
   342  c197 2907                       AND #%00000111	; x mod 8
   343  c199 aa                         TAX		; horizonal offset
   344  c19a 60                         RTS		; (bitmask)
   345                          
   346                          
   347                          ;-----------------------------------------------------------------
   348                          
   349                          ; line y up, x right, dx < dy (case 1)
   350                          
   351                          line_up_steep
   352  c19b 2076c1                     JSR position	; x,y
   353                          loop_yup_xright
   354  c19e 2067c1                     JSR gchange	; pixel
   355                          
   356  c1a1 18                         CLC		; k += dx
   357  c1a2 a595                       LDA kl
   358  c1a4 65ab                       ADC dxl		; dxh is 0, because dx < dy
   359  c1a6 8595                       STA kl
   360  c1a8 b004                       BCS ++		; k > 255
   361                          
   362  c1aa c5a9                       CMP dy
   363  c1ac 9015                       BCC +		; k >= dy ->
   364                          
   365  c1ae e5a9               ++	SBC dy		; k -= dy
   366  c1b0 8595                       STA kl
   367                          
   368  c1b2 e8                         INX		; x++
   369  c1b3 e008                       CPX #8
   370  c1b5 d00c                       BNE +
   371                          	; C=1
   372  c1b7 a200                       LDX #0		; x overflow, wrap around
   373  c1b9 a5a5                       LDA gaddr	; x+8: gaddr += 8
   374  c1bb 6907                       ADC #8-1	; C already set by CPX
   375  c1bd 85a5                       STA gaddr
   376  c1bf 9002                       BCC +
   377  c1c1 e6a6                       INC gaddr+1
   378                          
   379  c1c3 88                 +	DEY		; y--
   380  c1c4 100f                       BPL +++
   381  c1c6 38                         SEC		; y overflow
   382  c1c7 a5a5                       LDA gaddr
   383  c1c9 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   384  c1cb 85a5                       STA gaddr
   385  c1cd a5a6                       LDA gaddr+1
   386  c1cf e901               	SBC #1
   387  c1d1 85a6                       STA gaddr+1
   388  c1d3 a007                       LDY #7		; wrap around
   389                          
   390  c1d5 c6a3               +++	DEC cl		; until c=0
   391  c1d7 d0c5                       BNE loop_yup_xright
   392  c1d9 4c57c1                     JMP gexit
   393                          
   394                          
   395                          ;-----------------------------------------------------------------
   396                          
   397                          ; line x right, y up, dx > dy (case 2)
   398                          
   399                          line_up_flat
   400  c1dc 2076c1                     JSR position	; x,y
   401  c1df a5a3               	LDA cl		; counter adjustment for
   402  c1e1 f002               	BEQ +		; dec-dec-counting
   403  c1e3 e6a4               	INC ch
   404                          +
   405                          loop_xright_yup
   406  c1e5 2067c1                     JSR gchange	; pixel
   407                          
   408  c1e8 18                         CLC		; k += dy
   409  c1e9 a595                       LDA kl
   410  c1eb 65a9                       ADC dy
   411  c1ed 8595                       STA kl
   412  c1ef 9002                       BCC ++
   413  c1f1 e696                       INC kh
   414                          
   415  c1f3 c5ab               ++	CMP dxl		; k > dx?
   416  c1f5 a596                       LDA kh
   417  c1f7 e5a7                       SBC dxh
   418  c1f9 901a                       BCC +
   419                          
   420  c1fb 8596                       STA kh		; k -= dx
   421  c1fd a595                       LDA kl
   422  c1ff e5ab                       SBC dxl
   423  c201 8595                       STA kl
   424                          
   425  c203 88                         DEY		; y--
   426  c204 100f                       BPL +
   427  c206 38                 	SEC		; C=1 not always true (SBC above)
   428  c207 a5a5                       LDA gaddr	; y overflow
   429  c209 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   430  c20b 85a5                       STA gaddr
   431  c20d a5a6                       LDA gaddr+1
   432  c20f e901               	SBC #1
   433  c211 85a6                       STA gaddr+1
   434  c213 a007               	LDY #7		; wrap around
   435                          
   436  c215 e8                 +	INX		; x++
   437  c216 e008                       CPX #8		; x overflow?
   438  c218 d00c                       BNE ++
   439                          	; C=1
   440  c21a a200                       LDX #0		; wrap around
   441  c21c a5a5                       LDA gaddr	; x+8: gaddr += 8
   442  c21e 6907                       ADC #8-1	; C already set by CPX
   443  c220 85a5                       STA gaddr
   444  c222 9002                       BCC ++
   445  c224 e6a6                       INC gaddr+1
   446                          ++
   447  c226 c6a3               	DEC cl		; c--
   448  c228 d0bb                       BNE loop_xright_yup
   449  c22a c6a4                       DEC ch		; adjusted high which allows this
   450  c22c d0b7                       BNE loop_xright_yup
   451                          
   452  c22e 4c57c1                     JMP gexit
   453                          
   454                          
   455                          
   456                          ;-----------------------------------------------------------------
   457                          
   458                          ; line x right, y down, dx > dy (case 3)
   459                          
   460                          line_down_flat
   461  c231 2076c1                     JSR position	; x,y
   462  c234 a5a3               	LDA cl		; counter adjustment for
   463  c236 f002               	BEQ +		; dec-dec-counting
   464  c238 e6a4               	INC ch
   465                          +
   466                          loop_xright_ydown
   467  c23a 2067c1                     JSR gchange	; pixel
   468                          
   469  c23d 18                         CLC		; k += dy
   470  c23e a595                       LDA kl
   471  c240 65a9                       ADC dy
   472  c242 8595                       STA kl
   473  c244 9002                       BCC ++
   474  c246 e696                       INC kh
   475                          
   476  c248 c5ab               ++	CMP dxl		; k > dx
   477  c24a a596                       LDA kh
   478  c24c e5a7                       SBC dxh		; k -= dx
   479  c24e 901b                       BCC +
   480                          
   481  c250 8596                       STA kh
   482  c252 a595                       LDA kl
   483  c254 e5ab                       SBC dxl
   484  c256 8595                       STA kl
   485                          
   486  c258 c8                         INY		; y++
   487  c259 c008                       CPY #8
   488  c25b d00e                       BNE +
   489                          	; C=1
   490  c25d a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   491  c25f 693f                       ADC #$40-1	; C already set by CPY
   492  c261 85a5                       STA gaddr
   493  c263 a5a6                       LDA gaddr+1
   494  c265 6901               	ADC #1
   495  c267 85a6                       STA gaddr+1
   496  c269 a000                       LDY #0		; wrap around
   497                          
   498  c26b e8                 +	INX		; x++
   499  c26c e008                       CPX #8		; x overflow ?
   500  c26e d00c                       BNE +++
   501                          	; C=1
   502  c270 a200                       LDX #$00	; wrap around
   503  c272 a5a5                       LDA gaddr	; gaddr += 8
   504  c274 6907                       ADC #$08-1	; C always set by CPX
   505  c276 85a5                       STA gaddr
   506  c278 9002                       BCC +++
   507  c27a e6a6                       INC gaddr+1
   508                          +++
   509  c27c c6a3               	DEC cl		; c--
   510  c27e d0ba                       BNE loop_xright_ydown
   511  c280 c6a4                       DEC ch		; adjusted high which allows this
   512  c282 d0b6                       BNE loop_xright_ydown
   513                          
   514  c284 4c57c1                     JMP gexit
   515                          
   516                          
   517                          ;-----------------------------------------------------------------
   518                          
   519                          ; line y down, x right, dx < dy (case 4)
   520                          
   521                          line_down_steep
   522  c287 2076c1                     JSR position	; x,y
   523                          loop_ydown_xright
   524  c28a 2067c1                     JSR gchange	; pixel
   525                          
   526  c28d 18                         CLC		; k += dx
   527  c28e a595                       LDA kl
   528  c290 65ab                       ADC dxl		; dxh is 0, because dx < dy
   529  c292 8595                       STA kl
   530  c294 b004                       BCS ++
   531  c296 c5a9                       CMP dy		; k > dy?
   532  c298 9015                       BCC +
   533  c29a e5a9               ++	SBC dy		; k -= dy
   534  c29c 8595                       STA kl
   535                          
   536  c29e e8                         INX		; x++
   537  c29f e008                       CPX #8
   538  c2a1 d00c                       BNE +		; x overflow?
   539  c2a3 a200                       LDX #0		; wrap around
   540  c2a5 a5a5                       LDA gaddr	; x+9: gaddr += 8
   541  c2a7 6907                       ADC #8-1	; C already set by CPX
   542  c2a9 85a5                       STA gaddr
   543  c2ab 9002                       BCC +
   544  c2ad e6a6                       INC gaddr+1
   545                          
   546  c2af c8                 +	INY		; y++
   547  c2b0 c008                       CPY #8		; y overflow?
   548  c2b2 d00e                       BNE +++
   549  c2b4 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   550  c2b6 693f                       ADC #$40-1	; C already set by CPY
   551  c2b8 85a5                       STA gaddr
   552  c2ba a5a6                       LDA gaddr+1
   553  c2bc 6901               	ADC #1
   554  c2be 85a6                       STA gaddr+1
   555  c2c0 a000                       LDY #0		; wrap around
   556                          
   557  c2c2 c6a3               +++	DEC cl		; c--
   558                          			; until c=0
   559  c2c4 d0c4                       BNE loop_ydown_xright
   560  c2c6 4c57c1                     JMP gexit
   561                          
   562                          
   563                          ;-----------------------------------------------------------------
   564                          
   565                          getcommaxy
   566  c2c9 20fdae                     JSR b_getcomma	; check ","
   567                          getxy
   568  c2cc 208aad                     JSR b_getval	; get X coord. value
   569  c2cf 20f7b7                     JSR b_convint
   570  c2d2 c901                       CMP #>xmax
   571  c2d4 9009               	BCC gcxy_xok
   572  c2d6 f003                       BEQ +		; X = $1xx
   573                          error_iq
   574                          !ifdef no_error {
   575                          	RTS
   576                          } else {
   577  c2d8 4c48b2                     JMP b_illquant
   578                          }
   579  c2db c040               +	CPY #<xmax	; check X low
   580  c2dd b0f9                       BCS error_iq	; X to big
   581                          gcxy_xok
   582  c2df 84fb                       STY gpos	; temporary save X coord.
   583  c2e1 85fc                       STA gpos+1
   584                          
   585  c2e3 20f1b7                     JSR b_getcomma8bit
   586                          			; get Y coord. value
   587  c2e6 e0c8                       CPX #ymax
   588  c2e8 b0ee                       BCS error_iq	; Y to big
   589                          
   590  c2ea a4fb                       LDY gpos	; restory X coord.
   591  c2ec a5fc                       LDA gpos+1
   592  c2ee 60                         RTS
   593                          
   594                          
   595                          ;-----------------------------------------------------------------
   596                          
   597                          hline
   598  c2ef 20ccc2                     JSR getxy	; get startpoint
   599  c2f2 86aa                       STX y
   600  c2f4 8ed3c0                     STX savey	; save as cursor, too
   601  c2f7 859c                       STA xh
   602  c2f9 849b                       STY xl
   603  c2fb 20fdae                     JSR b_getcomma	; get length
   604  c2fe 208aad                     JSR b_getval
   605  c301 20f7b7                     JSR b_convint
   606                          
   607  c304 c901                       CMP #>xmax
   608  c306 9006                       BCC +		; X < 256
   609  c308 d0ce                       BNE error_iq
   610  c30a c040                       CPY #<xmax
   611  c30c b0ca                       BCS error_iq
   612                          +
   613                          			; calculate end point
   614  c30e aa                         TAX		; save length high byte
   615  c30f 98                         TYA		; length low byte
   616  c310 18                         CLC
   617  c311 659b                       ADC xl		; low xend = x+length
   618  c313 859e                       STA xendl
   619  c315 a8                 	TAY
   620  c316 8a                         TXA		; high
   621  c317 659c                       ADC xh		; high xend = x+length
   622  c319 859f                       STA xendh
   623  c31b aa                 	TAX
   624                          
   625  c31c c901               	CMP #>xmax	; endpoint outside?
   626  c31e 9005               	BCC +
   627  c320 98                 	TYA
   628  c321 e940               	SBC #<xmax
   629  c323 b0b3               	BCS error_iq
   630                          +
   631  c325 8ed2c0                     STX savexh
   632  c328 8cd1c0                     STY savexl	; also save as cursor
   633                          
   634  c32b 205fc1                     JSR ginit	; map in graphic memory
   635                          
   636                          hline_start
   637  c32e a59e                       LDA xendl
   638  c330 c59b                       CMP xl
   639  c332 a59f                       LDA xendh
   640  c334 e59c                       SBC xh
   641  c336 b013                       BCS hl_noxswap	; xend < x ->
   642                          
   643  c338 a69e                       LDX xendl	; swap x, xend
   644  c33a a59b                       LDA xl
   645  c33c 869b                       STX xl
   646  c33e 859e                       STA xendl
   647                          
   648  c340 a69f                       LDX xendh
   649  c342 a49c                       LDY xh
   650  c344 849f                       STY xendh
   651  c346 869c                       STX xh
   652  c348 4c5ac3                     JMP hl_start	; x != xend
   653                          
   654                          hl_noxswap
   655  c34b a59e                       LDA xendl
   656  c34d c59b                       CMP xl
   657  c34f d009                       BNE hl_start
   658  c351 a59f                       LDA xendh
   659  c353 c59c                       CMP xh
   660  c355 d003                       BNE hl_start	; x = xend ->
   661  c357 4ccec4             	JMP plot_start	; single point
   662                          ;	JMP gexit	; no point
   663                          
   664                          hl_start
   665  c35a 2076c1                     JSR position	; graphic position x,y
   666  c35d bdd4c0                     LDA maskleft,X
   667  c360 48                         PHA		; save left end mask
   668  c361 a59e                       LDA xendl
   669  c363 2907                       AND #%00000111
   670  c365 8596                       STA tmp2	; xend mod 8, mask index
   671  c367 a59b                       LDA xl
   672  c369 29f8                       AND #%11111000	; (xl div 8)*8
   673  c36b 8595                       STA tmp1
   674  c36d a59e                       LDA xendl	; xend unmasked
   675  c36f 38                         SEC
   676  c370 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   677  c372 8595                       STA tmp1
   678  c374 a59f                       LDA xendh
   679  c376 e59c                       SBC xh
   680  c378 4a                         LSR		; / 8 ->  0-39
   681  c379 a595                       LDA tmp1	; only 1 highest bit
   682  c37b 6a                         ROR		; and 3 lower bits
   683  c37c 4a                         LSR
   684  c37d 4a                         LSR
   685  c37e aa                         TAX		; 8-pixel-blocks count
   686  c37f 68                         PLA		; left end x mask
   687                          
   688                          hl_nextblock
   689  c380 ca                         DEX
   690                          hl_islastblock
   691  c381 3012                       BMI hl_lastblock
   692                          			; leave loop if X<0
   693  c383 206fc1                     JSR gmask	; first with left end mask
   694  c386 18                         CLC		; gaddr += 8
   695  c387 a5a5                       LDA gaddr
   696  c389 6908                       ADC #8
   697  c38b 85a5                       STA gaddr
   698  c38d 9002                       BCC +
   699  c38f e6a6                       INC gaddr+1
   700  c391 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   701  c393 d0eb               	BNE hl_nextblock	; always
   702                          
   703                          hl_lastblock
   704  c395 a696                       LDX tmp2	; xend mask index
   705  c397 3ddcc0                     AND maskright,X ; mask right end
   706  c39a 206fc1                     JSR gmask	; modify
   707  c39d 4c57c1                     JMP gexit	; leave
   708                          
   709                          
   710                          ;-----------------------------------------------------------------
   711                          
   712                          vline
   713  c3a0 20ccc2                     JSR getxy	; get startpoint
   714  c3a3 859c                       STA xh
   715  c3a5 8dd2c0                     STA savexh	; save as cursor too
   716  c3a8 849b                       STY xl
   717  c3aa 8cd1c0                     STY savexl
   718  c3ad 86aa                       STX y
   719                          
   720  c3af 20f1b7                     JSR b_getcomma8bit
   721                          			; get length
   722  c3b2 18                         CLC		; calculate end point
   723  c3b3 8a                         TXA		; length
   724                          ; DON'T-CHANGE: how long to go vertically (needed later)
   725                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   726                          ;	STA tmp1
   727  c3b4 65aa                       ADC y		; length + y
   728  c3b6 c9c8                       CMP #ymax
   729  c3b8 9003                       BCC +
   730                          vline_iq
   731                          !ifdef no_error {
   732                          	RTS
   733                          } else {
   734  c3ba 4c48b2                     JMP b_illquant
   735                          }
   736  c3bd 8593               +	STA yend	; endpoint
   737  c3bf c9c8               	CMP #ymax	; outside?
   738  c3c1 b0f7               	BCS vline_iq
   739                          
   740  c3c3 8dd3c0             	STA savey	; set cursor y position
   741                          
   742  c3c6 205fc1                     JSR ginit	; map in graphic memory
   743                          
   744                          vline_start
   745  c3c9 a593                       LDA yend
   746  c3cb c5aa                       CMP y
   747  c3cd b00a                       BCS vl_noyswap	; yend < y ->
   748  c3cf a5aa                       LDA y		; swap y, yend
   749  c3d1 a693                       LDX yend
   750  c3d3 8593                       STA yend
   751  c3d5 86aa                       STX y
   752  c3d7 f005               	BEQ vl_start	; always (with next branch)
   753                          	; fall through if yend is
   754                          vl_noyswap
   755  c3d9 d003                       BNE vl_start	; y = yend ->
   756  c3db 4ccec4             	JMP plot_start	; single point
   757                          ;	JMP gexit	; no point
   758                          
   759                          vl_start
   760  c3de 2076c1                     JSR position	; graphic position x,y
   761  c3e1 bda4c0                     LDA bitmask,X
   762  c3e4 8596                       STA tmp2	; save mask
   763                          ; DON'T-CHANGE: replace ...
   764  c3e6 38                         SEC
   765  c3e7 a593                       LDA yend
   766  c3e9 e5aa                       SBC y		; vertical length
   767  c3eb aa                         TAX
   768                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   769                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   770                          ;	LDX tmp1
   771  c3ec e8                         INX		; +1 (exit on 0)
   772                          vl_nextline
   773  c3ed a596                       LDA tmp2
   774  c3ef 206fc1                     JSR gmask	; modify 
   775  c3f2 c8                         INY		; go down
   776  c3f3 c008                       CPY #8		; 8-line wrap
   777  c3f5 d00e                       BNE +
   778  c3f7 a5a5                       LDA gaddr	; gaddr += 320
   779  c3f9 693f               	ADC #$40-1	; compensate for C = 1
   780  c3fb 85a5                       STA gaddr
   781  c3fd a5a6                       LDA gaddr+1
   782  c3ff 6901                       ADC #$01
   783  c401 85a6                       STA gaddr+1
   784  c403 a000                       LDY #0		; wrap y offset
   785  c405 ca                 +	DEX		; all vertical positions done?
   786  c406 d0e5                       BNE vl_nextline
   787  c408 4c57c1                     JMP gexit	; leave
   788                          
   789                          
   790                          ;-----------------------------------------------------------------
   791                          
   792                          line
   793  c40b 20ccc2                     JSR getxy	; get startpoint
   794  c40e 849b                       STY xl 
   795  c410 859c                       STA xh
   796  c412 86aa                       STX y
   797                          
   798  c414 20c9c2                     JSR getcommaxy	; get endpoint
   799                          line_start
   800  c417 8cd1c0                     STY savexl	; save as cursor position too
   801  c41a 849e                       STY xendl
   802  c41c 8dd2c0                     STA savexh
   803  c41f 859f                       STA xendh
   804  c421 8ed3c0                     STX savey
   805  c424 8693                       STX yend
   806                          
   807  c426 205fc1                     JSR ginit	; map in graphic memory
   808                          
   809  c429 a000                       LDY #$00	; initialize to 0
   810  c42b 84a8                       STY ydir
   811  c42d 8495                       STY kl
   812  c42f 8496                       STY kh
   813                          
   814  c431 38                         SEC
   815  c432 a59e                       LDA xendl	; calculate dx
   816  c434 e59b                       SBC xl
   817  c436 85ab                       STA dxl
   818  c438 a59f                       LDA xendh
   819  c43a e59c                       SBC xh
   820  c43c 85a7                       STA dxh
   821                          
   822  c43e b025                       BCS li_xend_right
   823                          	; dx != 0
   824  c440 98                         TYA		; negate dx
   825  c441 38                         SEC		; dx = 0 - dx
   826  c442 e5ab                       SBC dxl
   827  c444 85ab                       STA dxl
   828  c446 98                         TYA
   829  c447 e5a7                       SBC dxh
   830  c449 85a7                       STA dxh
   831                          			; C=0 always, needed later
   832  c44b a69b                       LDX xl		; swap x low
   833  c44d a49e                       LDY xendl
   834  c44f 869e                       STX xendl
   835  c451 849b                       STY xl
   836                          
   837  c453 a69c                       LDX xh		; swap x high
   838  c455 a49f                       LDY xendh
   839  c457 869f                       STX xendh
   840  c459 849c                       STY xh
   841                          
   842  c45b a6aa                       LDX y		; swap y
   843  c45d a493                       LDY yend
   844  c45f 8693                       STX yend
   845  c461 84aa                       STY y
   846                          
   847  c463 9009                       BCC li_x_different
   848                          			; C=0 always (from negation before)
   849                          
   850                          li_xend_right
   851  c465 a5ab                       LDA dxl		; dx = 0?
   852  c467 05a7                       ORA dxh
   853  c469 d003                       BNE li_x_different
   854  c46b 4cc9c3                     JMP vline_start	; vertical line case
   855                          
   856                          li_x_different
   857  c46e 38                         SEC		; calculate dy
   858  c46f a593                       LDA yend
   859  c471 e5aa                       SBC y
   860  c473 b006                       BCS li_y_right
   861  c475 49ff                       EOR #$FF	; negate dy (two's complement)
   862  c477 6901                       ADC #$01	; C=0
   863  c479 85a8                       STA ydir	; flag y goes up
   864                          
   865                          li_y_right
   866  c47b 85a9                       STA dy
   867  c47d d003                       BNE +
   868  c47f 4c2ec3                     JMP hline_start	; horizontal line case
   869                          +
   870                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   871                          
   872  c482 a5a7                       LDA dxh		; dx > dy
   873  c484 d017                       BNE line_flat	; yes -> flat
   874  c486 a5a9                       LDA dy		; no -> steep
   875  c488 aa                         TAX
   876  c489 c5ab                       CMP dxl
   877  c48b 9010                       BCC line_flat
   878                          
   879                          line_steep
   880  c48d e8                         INX	
   881  c48e 86a3                       STX cl		; c = dy+1
   882  c490 4a                         LSR		; k = dy/2
   883  c491 8595                       STA kl
   884  c493 a5a8                       LDA ydir
   885  c495 d003                       BNE +
   886  c497 4c87c2                     JMP line_down_steep	; y down, steep
   887  c49a 4c9bc1             +	JMP line_up_steep	; y up, steep
   888                          
   889                          line_flat
   890  c49d a5a7                       LDA dxh
   891  c49f a8                         TAY
   892  c4a0 a6ab                       LDX dxl
   893  c4a2 e8                         INX
   894  c4a3 d001                       BNE +
   895  c4a5 c8                         INY
   896  c4a6 86a3               +	STX cl		; c = dx+1
   897  c4a8 84a4                       STY ch
   898                          
   899  c4aa 4a                         LSR		; k = dx/2
   900  c4ab 8596                       STA kh
   901  c4ad a5ab                       LDA dxl
   902  c4af 6a                         ROR		; dx/2
   903  c4b0 8595                       STA kl
   904  c4b2 a5a8                       LDA ydir	
   905  c4b4 d003                       BNE +
   906  c4b6 4c31c2                     JMP line_down_flat	; y down, flat
   907  c4b9 4cdcc1             +	JMP line_up_flat	; y up, flat
   908                          
   909                          ;-----------------------------------------------------------------
   910                          
   911                          plot
   912  c4bc 20ccc2                     JSR getxy	; get parameter
   913  c4bf 859c                       STA xh		; save x/y
   914  c4c1 849b                       STY xl
   915  c4c3 86aa                       STX y
   916  c4c5 8dd2c0                     STA savexh	; and store as cursor
   917  c4c8 8cd1c0                     STY savexl
   918  c4cb 8ed3c0                     STX savey
   919                          
   920                          plot_start
   921  c4ce 2076c1                     JSR position	; calculate graphical address
   922                          
   923  c4d1 a501                       LDA prozport
   924  c4d3 29fd                       AND #%11111101	; Kernal ROM disable
   925  c4d5 78                         SEI			
   926  c4d6 8501                       STA prozport
   927                          
   928  c4d8 2067c1                     JSR gchange	; change graphical data
   929                          
   930  c4db a501                       LDA prozport
   931  c4dd 0902                       ORA #%00000010	; kernal ROM enable
   932  c4df 8501                       STA prozport
   933  c4e1 58                         CLI
   934  c4e2 60                         RTS
   935                          
   936                          ;-----------------------------------------------------------------
   937                          
   938                          move
   939  c4e3 20ccc2                     JSR getxy	; get parameter
   940  c4e6 8dd2c0                     STA savexh	; just save as cursor
   941  c4e9 8cd1c0                     STY savexl
   942  c4ec 8ed3c0                     STX savey
   943  c4ef 60                         RTS
   944                          
   945                          
   946                          ;-----------------------------------------------------------------
   947                          
   948                          setmode
   949  c4f0 209eb7                     JSR b_get8bit
   950  c4f3 e003                       CPX #$03
   951  c4f5 9003                       BCC +
   952  c4f7 4c48b2                     JMP b_illquant
   953  c4fa e001               +	CPX #$01
   954  c4fc b01a                       BCS set_or_toggle
   955                          
   956                          modereset
   957  c4fe a9c0                       LDA #>(nbitmask)
   958  c500 8d6bc1                     STA gchange_op+2
   959  c503 a9ac                       LDA #<(nbitmask)
   960  c505 8d6ac1                     STA gchange_op+1
   961  c508 a93d                       LDA #$3D		; AND abs,X
   962  c50a 8d69c1                     STA gchange_op
   963  c50d a931                       LDA #$31		; AND (zp),Y
   964  c50f 8d71c1                     STA gmask_op
   965  c512 a9ff                       LDA #$FF		; EOR $#FF, invertieren
   966  c514 8d70c1                     STA gmask_flip+1
   967  c517 60                         RTS
   968                          
   969                          set_or_toggle
   970  c518 d01a                       BNE modetoggle
   971                          modeset
   972  c51a a9c0                       LDA #>(bitmask)
   973  c51c 8d6bc1                     STA gchange_op+2
   974  c51f a9a4                       LDA #<(bitmask)
   975  c521 8d6ac1                     STA gchange_op+1
   976  c524 a91d                       LDA #$1D		; OR abs,X
   977  c526 8d69c1                     STA gchange_op
   978  c529 a911                       LDA #$11		; OR (zp),Y
   979  c52b 8d71c1                     STA gmask_op
   980  c52e a900                       LDA #$00		; EOR #$00, nicht invertieren
   981  c530 8d70c1                     STA gmask_flip+1
   982  c533 60                         RTS
   983                          
   984                          modetoggle
   985  c534 a9c0                       LDA #>(bitmask)
   986  c536 8d6bc1                     STA gchange_op+2
   987  c539 a9a4                       LDA #<(bitmask)
   988  c53b 8d6ac1                     STA gchange_op+1
   989  c53e a95d                       LDA #$5D		; EOR abs,X
   990  c540 8d69c1                     STA gchange_op
   991  c543 a951                       LDA #$51		; EOR (zp),Y
   992  c545 8d71c1                     STA gmask_op
   993  c548 a900                       LDA #$00		; EOR #$00, nicht invertieren
   994  c54a 8d70c1                     STA gmask_flip+1
   995  c54d 60                         RTS
   996                          
   997                          
   998                          ;-----------------------------------------------------------------
   999                          
  1000                          ; get pixel (check if pixel set)
  1001                          ; not used
  1002                          
  1003                          get
  1004  c54e 20c9c2                     JSR getcommaxy
  1005  c551 859c                       STA xh
  1006  c553 849b                       STY xl
  1007  c555 86aa                       STX y
  1008                          
  1009  c557 2076c1                     JSR position
  1010                          
  1011  c55a a501                       LDA prozport
  1012  c55c 29fd               	AND #%11111101	; Kernal ROM disable
  1013  c55e 78                         SEI
  1014  c55f 8501                       STA prozport
  1015                          
  1016  c561 b1a5                       LDA (gaddr),Y
  1017  c563 3da4c0                     AND bitmask,X
  1018  c566 a8                         TAY
  1019  c567 a501                       LDA prozport
  1020  c569 0902               	ORA #%00000010	; kernal ROM enable
  1021  c56b 8501                       STA prozport
  1022  c56d 58                         CLI
  1023  c56e 4ca2b3                     JMP b_byte2fac
  1024                          
  1025                          
  1026                          ;-----------------------------------------------------------------
  1027                          
  1028                          relto
  1029  c571 208aad                     JSR b_getval	; get X offset (+/-)
  1030  c574 a561               	LDA facexp	; FAC exponent
  1031  c576 c990               	CMP #$90	; more than 16 bit
  1032  c578 b031               	BCS relto_error	; illegal quantity
  1033  c57a 209bbc                     JSR b_fac2int	; to signed integer
  1034                          
  1035  c57d 18                         CLC
  1036  c57e a565                       LDA facintl
  1037  c580 6dd1c0                     ADC savexl
  1038  c583 859e                       STA xendl
  1039  c585 a564                       LDA facinth
  1040  c587 6dd2c0                     ADC savexh
  1041  c58a 859f                       STA xendh	; xend = savex+facint
  1042                          
  1043  c58c 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1044  c58f 208aad                     JSR b_getval
  1045  c592 a561                       LDA facexp	; FAC exponent
  1046  c594 c990                       CMP #$90	; more than 16 bit
  1047  c596 b013                       BCS relto_error	; illegal quantity
  1048  c598 209bbc                     JSR b_fac2int	; to signed integer
  1049  c59b 18                         CLC
  1050  c59c a565                       LDA facintl
  1051  c59e 6dd3c0                     ADC savey
  1052  c5a1 8593                       STA yend	; yend = savey+facint
  1053                          
  1054  c5a3 a59f                       LDA xendh	; check end coord. x
  1055  c5a5 c901                       CMP #>xmax
  1056  c5a7 900b                       BCC rt_xok
  1057  c5a9 f003                       BEQ +
  1058                          relto_error
  1059                          !ifdef no_error {
  1060                          	RTS
  1061                          } else {
  1062  c5ab 4c48b2                     JMP b_illquant
  1063                          }
  1064  c5ae a59e               +	LDA xendl
  1065  c5b0 c940                       CMP #<xmax
  1066  c5b2 b0f7                       BCS relto_error
  1067                          rt_xok
  1068  c5b4 a593                       LDA yend	; check end coord. y
  1069  c5b6 c9c8                       CMP #ymax
  1070  c5b8 b0f1                       BCS relto_error
  1071                          
  1072  c5ba add1c0                     LDA savexl
  1073  c5bd 859b                       STA xl
  1074  c5bf add2c0                     LDA savexh
  1075  c5c2 859c                       STA xh
  1076  c5c4 add3c0                     LDA savey
  1077  c5c7 85aa                       STA y
  1078  c5c9 a49e                       LDY xendl
  1079  c5cb a59f                       LDA xendh
  1080  c5cd a693                       LDX yend	; xend/yend = cursor + x/y
  1081                          
  1082  c5cf 4c17c4                     JMP line_start	; draw line x/y to xend/yend
  1083                          
  1084                          
  1085                          ;-----------------------------------------------------------------
  1086                          
  1087                          char
  1088  c5d2 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1089  c5d5 e028                       CPX #40	
  1090  c5d7 9003                       BCC +
  1091                          char_error
  1092  c5d9 4c48b2                     JMP b_illquant
  1093  c5dc 86fb               +	STX gpos	; save x coord.
  1094  c5de 20f1b7                     JSR b_getcomma8bit
  1095                          			; get char. position y 0-24
  1096  c5e1 e019                       CPX #25
  1097  c5e3 b0f4                       BCS char_error
  1098  c5e5 86fc                       STX gpos+1	; save y coord.
  1099                          
  1100  c5e7 20fdae                     JSR b_getcomma	; get string
  1101  c5ea 209ead                     JSR b_getexpr
  1102  c5ed 20a3b6                     JSR b_stringval ; string address in str
  1103  c5f0 48                         PHA		; string length
  1104  c5f1 a6fc                       LDX gpos+1	; y coord. for char. position
  1105  c5f3 8a                         TXA
  1106  c5f4 2903                       AND #$03	; mask 2 bits
  1107  c5f6 a8                         TAY		; table index
  1108  c5f7 a900                       LDA #$00
  1109  c5f9 85fc                       STA gpos+1	; x high
  1110  c5fb a5fb                       LDA gpos	; saved x: multiply by 8
  1111  c5fd 0a                         ASL
  1112  c5fe 0a                         ASL
  1113  c5ff 0a                         ASL
  1114  c600 26fc                       ROL gpos+1	; overflow to high byte
  1115  c602 79b4c0                     ADC ytabl,Y
  1116  c605 85a5                       STA gaddr
  1117  c607 a5fc                       LDA gpos+1	; x high
  1118  c609 7db8c0                     ADC ytabh,X
  1119  c60c 85a6                       STA gaddr+1
  1120  c60e 68                         PLA		; string length
  1121  c60f a000                       LDY #$00	; string index
  1122  c611 aa                         TAX		; length
  1123  c612 e8                         INX		; prepare as counter
  1124                          char_loop
  1125  c613 ca                         DEX
  1126  c614 f008                       BEQ char_exit
  1127  c616 b122                       LDA (str),Y	; read string
  1128  c618 201fc6                     JSR char_display
  1129  c61b c8                         INY
  1130  c61c d0f5                       BNE char_loop
  1131                          char_exit
  1132  c61e 60                         RTS
  1133                          
  1134                          char_display
  1135  c61f 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1136  c621 8a                         TXA		; save register X+Y
  1137  c622 48                         PHA
  1138  c623 98                         TYA
  1139  c624 48                         PHA
  1140  c625 a5d7                       LDA z_tmp	; get saved character
  1141  c627 1049                       BPL char_normal
  1142                          
  1143                          char_inverse
  1144  c629 297f                       AND #%01111111	; mask bit 7
  1145  c62b c97f                       CMP #%01111111	; was 255? (pi)
  1146  c62d d002                       BNE +
  1147  c62f a95e                       LDA #$5E	; screen code for pi
  1148  c631 c920               +	CMP #$20	; control character?
  1149  c633 9038                       BCC char_disp_leave
  1150                          			; yes, skip
  1151  c635 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1152                          			; $C0-$FF -> $40-$7F
  1153                          			; OPT: BNE char_hires
  1154                          			; OPT: char_normal
  1155                          char_hires
  1156  c637 a6c7                       LDX z_reverseflag
  1157  c639 f002                       BEQ +
  1158  c63b 0980                       ORA #%10000000	; invert char.
  1159  c63d 48                 +	PHA		; save char. for later
  1160  c63e 78                         SEI
  1161  c63f a921                       LDA #$21	; char. rom, no basic and kernal rom
  1162  c641 8501                       STA prozport	; char. rom base = $D000
  1163  c643 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1164  c645 85fc                       STA gpos+1	; 
  1165  c647 68                         PLA		; char. code
  1166  c648 0a                         ASL		; *8
  1167  c649 26fc                       ROL gpos+1
  1168  c64b 0a                         ASL
  1169  c64c 26fc                       ROL gpos+1
  1170  c64e 0a                         ASL
  1171  c64f 26fc                       ROL gpos+1
  1172  c651 85fb                       STA gpos	; addr. in char. rom for char.
  1173                          
  1174  c653 a007                       LDY #$07	; 8 hires lines
  1175                          char_line
  1176  c655 b1fb                       LDA (gpos),Y	; read character line
  1177  c657 206fc1                     JSR gmask	; write to hires screen
  1178  c65a 88                         DEY
  1179  c65b 10f8                       BPL char_line
  1180                          
  1181  c65d a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1182  c65f 8501                       STA prozport
  1183  c661 58                         CLI
  1184                          
  1185  c662 18                         CLC		; step char position to left
  1186  c663 a5a5                       LDA gaddr	; ( +8 )
  1187  c665 6908                       ADC #$08
  1188  c667 85a5                       STA gaddr
  1189  c669 9002                       BCC +
  1190  c66b e6a6                       INC gaddr+1
  1191                          +
  1192                          char_disp_leave
  1193  c66d 68                 	PLA		; pass written character back
  1194  c66e a8                         TAY		; restore saved registers
  1195  c66f 68                         PLA
  1196  c670 aa                         TAX
  1197  c671 60                         RTS
  1198                          
  1199                          char_normal
  1200  c672 c920                       CMP #$20	; control character?
  1201  c674 90f7                       BCC char_disp_leave
  1202  c676 c960                       CMP #$60
  1203  c678 9004                       BCC +
  1204  c67a 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1205  c67c d002                       BNE ++
  1206  c67e 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1207  c680 4c37c6             ++	JMP char_hires	; 		OPT: Bxx
  1208                          
  1209                          
  1210                          ;-----------------------------------------------------------------
  1211                          
  1212                          to
  1213  c683 add1c0                     LDA savexl
  1214  c686 859b                       STA xl
  1215  c688 add2c0                     LDA savexh
  1216  c68b 859c                       STA xh
  1217  c68d add3c0                     LDA savey
  1218  c690 85aa                       STA y
  1219  c692 20ccc2                     JSR getxy
  1220  c695 4c17c4                     JMP line_start
  1221                          
  1222                          graext_end
  1223                          

; ******** Source: graext.asm
    37                          
    38                          
