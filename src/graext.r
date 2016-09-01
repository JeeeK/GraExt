
; ******** Source: graext.asm
     1                          !to "graext.o",cbm	
     2                          
     3                          ;  **** gra-ext ****
     4                          ;
     5                          ; 2015-10-05 johann e. klasek, johann at klasek at
     6                          ;
     7                          ; revisions:
     8                          ;	1992-12-28 v 1.18
     9                          ;	1986-03-24 v 1.17
    10                          ;	1985       v 0.00 - 1.16
    11                          ;
    12                          ; the original source has been lost.
    13                          ; development has based on the implemention
    14                          ; done on a forth-64 written with its forth assembler.
    15                          ; the code has been pulled out from there and enriched
    16                          ; with some glue code to get a basic extension.
    17                          
    18                          
    19                          *= $c000
    20                          
    21                          
    22                          ; basic interpreter registers, addresses and entry points
    23                          
    24                          str     = $22		; string address
    25                          ijmp    = $55		; address of JMP (addr)
    26                          chrget  = $73		; basic charget routine
    27                          facintl = $65		; integer result from b_fac2int
    28                          facinth = $64
    29                          facexp  = $61		; fac exponent, after b_getval
    30                          
    31                          z_reverseflag = $C7	; character routine
    32                          z_lastkey = $D7		; original use case, unused here
    33                          z_tmp = z_lastkey	; temporary reused for character routine
    34                          
    35                          v_bascmd = $0308
    36                          
    37                          basic_rom = $A000	; start of BASIC ROM
    38                          
    39                          b_interpreter =$A7AE	; interpreter loop
    40                          b_execstatement =$A7E7	; process statement
    41                          b_getcomma = $AEFD	; read comma from basic text
    42                          b_illquant = $B248	; error "illegal quantity"
    43                          b_get8bit = $B79E	; read 8 bit numeric value from
    44                          			; basic text
    45                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    46                          			; from basic text
    47                          b_getval = $AD8A	; read numeric value from basic text
    48                          b_getexpr = $AD9E	; read expression from basic text
    49                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    50                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    51                          b_fac2int = $BC9B	; convert FAC to integer
    52                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    53                          
    54                          ; hardware registers and values
    55                          
    56                          prozport = $01		; processor port
    57                          memrom = %00110111	; basic+kernal rom
    58                          membas = %00110110	; basic ram+kernal rom
    59                          memram = %00110101	; basic+kernal ram
    60                          
    61                          vic_cr	= $D011		; VIC control register
    62                          vic_mcr	= $D018		; VIC memory control register
    63                          cia_pra	= $DD00		; CIA 2 port register A
    64                          
    65                          cram	= $CC00		; start of color ram
    66                          
    67                          gram	= $e000		; start of graphic bitmap ram
    68                          gramp	= gram >> 8	; start page of bitmap
    69                          
    70                          ; constants 
    71                          
    72                          xmax	= 320		; max x dimension
    73                          ymax	= 200		; max y dimension
    74                          
    75                          ; zeropage variables
    76                          
    77                          x	= $9B		; start coordinate x, low+high
    78                          xl	= x
    79                          xh	= x+1
    80                          y	= $AA		; start coordinate y
    81                          
    82                          xendl	= $9E		; end coordinate x, low+high
    83                          xendh	= $9F
    84                          yend	= $93		; end coordinate y
    85                          
    86                          kl	= $95		; gradient for lines, low+high
    87                          kh	= kl+1
    88                          
    89                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
    90                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
    91                          
    92                          dxl	= $AB		; x delta, low+high
    93                          dxh	= $A7
    94                          
    95                          dy	= $A9		; y delta
    96                          
    97                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
    98                          
    99                          cl	= $A3		; dot count, low+high
   100                          ch	= $A4
   101                          
   102                          gaddr	= $A5		; graphic address
   103                          
   104                          gpos	= $FB		; in graphic position
   105                          
   106                          gcol	= $FD		; graphic color, in "graphic on" context only
   107                          
   108                          ;
   109                          ; initialize extension
   110                          
   111                          init
   112                          
   113                          ; CHANGE: remove ...
   114  c000 a000                       LDY #$00
   115  c002 a220                       LDX #$20	; page count
   116  c004 a9a0                       LDA #>basic_rom	; 
   117  c006 85fc                       STA gpos+1
   118  c008 84fb                       STY gpos
   119                          copyloop
   120  c00a b1fb                       LDA (gpos),Y	; copy basic rom to ram
   121  c00c 91fb                       STA (gpos),Y
   122  c00e c8                         INY
   123  c00f d0f9                       BNE copyloop
   124  c011 e6fc                       INC gpos+1
   125  c013 ca                         DEX
   126  c014 d0f4                       BNE copyloop
   127  c016 a936                       LDA #membas	; switch basic to ram copy
   128  c018 8501                       STA prozport
   129                          ; CHANGE: ... until here: basic ram copy not needed!?
   130                          
   131  c01a a925                       LDA #<(parse)	; basic interpreter parser hook
   132  c01c 8d0803                     STA v_bascmd
   133  c01f a9c0                       LDA #>(parse)
   134  c021 8d0903                     STA v_bascmd+1
   135  c024 60                         RTS
   136                          
   137                          
   138                          ;-----------------------------------------------------------------
   139                          
   140                          ; start parsing an extension command ...
   141                          
   142                          parse
   143  c025 207300                     JSR chrget			; next char.
   144  c028 c926                       CMP #'&'			; command prefix
   145  c02a f005                       BEQ newcmd
   146  c02c c900                       CMP #$00			; restore flags
   147  c02e 4ce7a7                     JMP b_execstatement
   148                          newcmd
   149  c031 207300                     JSR chrget			; command character
   150                          
   151  c034 a00b                       LDY #(cmdsend-cmds)		; map character to
   152                          					; command address ...
   153                          checknextcmd
   154  c036 88                         DEY
   155                          ; BUG: replace
   156  c037 f017                       BEQ $C050			; BUG $C050
   157                          ; BUG: ... by:
   158                          ;	BEQ parse_exit
   159  c039 d957c0                     CMP cmds,Y
   160  c03c d0f8                       BNE checknextcmd		; try next
   161  c03e 88                         DEY				; found
   162  c03f 98                         TYA
   163  c040 0a                         ASL				; *2
   164  c041 a8                         TAY
   165  c042 b973c0                     LDA cmdaddr+1,Y			; high byte from table
   166  c045 8556                       STA ijmp+1
   167  c047 b972c0                     LDA cmdaddr,Y			; low byte from table
   168  c04a 8555                       STA ijmp
   169  c04c 207300                     JSR chrget
   170  c04f 205400                     JSR ijmp-1			; JMP (addr)
   171                          parse_exit
   172  c052 4caea7                     JMP b_interpreter		; continue parsing
   173                          
   174                          ;-----------------------------------------------------------------
   175                          
   176  c055 5347               	!byte $53,$47			; not used
   177                          
   178  c057 20504c524843544d...cmds	!text " PLRHCTMVSG"		; first char. is a dummy
   179                          cmdsend
   180  c062 2020202020202020...	!text "                "	; not used, for future extension
   181                          
   182                          cmdaddr
   183  c072 dbc41ec493c50ec3...        !word plot, line, relto,hline,char,to,move,vline,setmode,graphic
   184                          
   185  c086 0000000000000000...	!byte  00,00,00,00,00,00,00,00,00,00	; not used
   186                          
   187  c090 4a4f48414e4e204b...author	!text "JOHANN KLASEK/24.3.1986/V1.17"
   188                          
   189  c0ad 0000000000000000...	!byte 0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	; not used
   190                          
   191                          bitmask
   192  c0bc 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   193                          nbitmask
   194  c0c4 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   195                          ytabl
   196  c0cc 004080c0           	!byte $00,$40,$80,$c0
   197                          ytabh
   198  c0d0 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   199  c0d4 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   200  c0d8 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   201  c0dc eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   202  c0e0 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   203  c0e4 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   204  c0e8 fe                 	!byte gramp+$1e
   205                          
   206  c0e9 3a                 savexl	!byte $3a
   207  c0ea 01                 savexh	!byte $01
   208  c0eb 71                 savey	!byte $71
   209                          
   210                          ; not used
   211  c0ec 00                 	!byte $00
   212                          ;	!byte $c6,$00,$0d,$00
   213                          
   214                          ; for horiz. line
   215                          
   216  c0ed ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   217                          
   218  c0f5 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   219                          
   220                          
   221                          ;-----------------------------------------------------------------
   222                          
   223                          graphic
   224  c0fd 209eb7                     JSR b_get8bit
   225  c100 e000                       CPX #$00
   226  c102 d013                       BNE graphic_on
   227                          gra0			; &G 0
   228  c104 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   229  c106 8d00dd                     STA cia_pra
   230  c109 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   231                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   232                          			; char addr $1000/4096 = char. ROM
   233  c10b 8d18d0                     STA vic_mcr	; VIC memory control
   234  c10e ad11d0                     LDA vic_cr	; VIC control register
   235  c111 29df                       AND #%11011111	; Hires mode off
   236  c113 8d11d0                     STA vic_cr
   237  c116 60                         RTS
   238                          graphic_on
   239  c117 e001                       CPX #$01
   240  c119 d01f                       BNE gra2
   241                          gra1			; &G 1
   242  c11b a000                       LDY #$00
   243  c11d a220                       LDX #$20	; Pages (8 KByte)
   244  c11f a9e0                       LDA #>gram
   245  c121 85fc                       STA gpos+1
   246  c123 84fb                       STY gpos
   247  c125 a900                       LDA #$00
   248                          gra_clear
   249  c127 91fb                       STA (gpos),Y	; Loop unroll
   250  c129 c8                         INY
   251  c12a 91fb                       STA (gpos),Y
   252  c12c c8                         INY
   253  c12d 91fb                       STA (gpos),Y
   254  c12f c8                         INY
   255  c130 91fb                       STA (gpos),Y
   256  c132 c8                         INY
   257  c133 d0f2                       BNE gra_clear
   258  c135 e6fc                       INC gpos+1
   259  c137 ca                         DEX
   260  c138 d0ed                       BNE gra_clear
   261                          gra2
   262  c13a 20f1b7                     JSR b_getcomma8bit
   263  c13d 8a                         TXA		; foreground color
   264  c13e 0a                         ASL		; upper nibble
   265  c13f 0a                         ASL
   266  c140 0a                         ASL
   267  c141 0a                         ASL
   268  c142 85fd                       STA gcol
   269  c144 20f1b7                     JSR b_getcomma8bit
   270  c147 8a                         TXA		; background color
   271  c148 290f                       AND #$0F
   272  c14a 05fd                       ORA gcol
   273  c14c a000                       LDY #$00
   274                          cram_loop
   275  c14e 9900cc                     STA cram,Y
   276  c151 9900cd                     STA cram+$100,Y
   277  c154 9900ce                     STA cram+$200,Y
   278  c157 99e8ce                     STA cram+$300-24,Y
   279  c15a c8                         INY
   280  c15b d0f1                       BNE cram_loop
   281  c15d a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   282  c15f 8d00dd                     STA cia_pra
   283  c162 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   284  c164 8d18d0                     STA vic_mcr	; VIC memory control
   285  c167 ad11d0                     LDA vic_cr	; VIC control register
   286  c16a 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   287  c16c 8d11d0                     STA vic_cr
   288  c16f 60                         RTS
   289                          
   290                          
   291                          ;-----------------------------------------------------------------
   292                          
   293                          gexit
   294  c170 a501                       LDA prozport
   295  c172 0902                       ORA #%00000010	; kernal ROM enable
   296  c174 8501                       STA prozport
   297  c176 58                         CLI		; allow interrupts
   298  c177 60                         RTS
   299                          
   300                          ;-----------------------------------------------------------------
   301                          
   302                          ginit
   303  c178 a501                       LDA prozport
   304  c17a 29fd                       AND #%11111101	; Kernal ROM disable
   305  c17c 78                         SEI		; disable interrupts
   306  c17d 8501                       STA prozport
   307  c17f 60                         RTS
   308                          
   309                          ;-----------------------------------------------------------------
   310                          
   311                          gchange
   312  c180 b1a5                       LDA (gaddr),Y
   313                          gchange_op
   314  c182 1dbcc0                     ORA bitmask,X
   315  c185 91a5                       STA (gaddr),Y
   316  c187 60                         RTS
   317                          
   318                          ;-----------------------------------------------------------------
   319                          
   320                          gmask
   321                          gmask_flip
   322  c188 4900                       EOR #$00
   323                          gmask_op
   324  c18a 11a5                       ORA (gaddr),Y
   325  c18c 91a5                       STA (gaddr),Y
   326  c18e 60                         RTS
   327                          
   328                          ;-----------------------------------------------------------------
   329                          
   330                          position
   331  c18f a5aa                       LDA y
   332  c191 4a                         LSR
   333  c192 4a                         LSR
   334  c193 4a                         LSR		; y/8
   335  c194 a8                         TAY
   336  c195 2903                       AND #%00000011	; (y/8) mod 4
   337  c197 aa                         TAX
   338  c198 a59b                       LDA xl		; x low
   339  c19a 29f8                       AND #%11111000	; clear bit 2-0
   340  c19c 18                         CLC
   341  c19d 7dccc0                     ADC ytabl,X	; addr low: y base + x part
   342  c1a0 85a5                       STA gaddr
   343  c1a2 a59c                       LDA xh		; addr high: x part
   344  c1a4 79d0c0                     ADC ytabh,Y	; 	+ y base
   345  c1a7 85a6                       STA gaddr+1
   346  c1a9 a5aa                       LDA y		; vertical offset
   347  c1ab 2907                       AND #%00000111	; y mod 8
   348  c1ad a8                         TAY
   349  c1ae a59b                       LDA xl
   350  c1b0 2907                       AND #%00000111	; x mod 8
   351  c1b2 aa                         TAX		; horizonal offset
   352  c1b3 60                         RTS		; (bitmask)
   353                          
   354                          
   355                          ;-----------------------------------------------------------------
   356                          
   357                          ; line y up, x right, dx < dy (case 1)
   358                          
   359                          line_down_steep
   360  c1b4 208fc1                     JSR position	; x,y
   361                          loop_yup_xright
   362  c1b7 2080c1                     JSR gchange	; pixel
   363                          
   364  c1ba 18                         CLC		; k += dx
   365  c1bb a595                       LDA kl
   366  c1bd 65ab                       ADC dxl		; dxh is 0, because dx < dy
   367  c1bf 8595                       STA kl
   368  c1c1 b004                       BCS ++		; k > 255
   369                          
   370  c1c3 c5a9                       CMP dy
   371  c1c5 9016                       BCC +		; k >= dy ->
   372                          
   373  c1c7 e5a9               ++	SBC dy		; k -= dy
   374  c1c9 8595                       STA kl
   375                          
   376  c1cb e8                         INX		; x++
   377  c1cc e008                       CPX #8
   378  c1ce d00d                       BNE +
   379  c1d0 a200                       LDX #0		; x overflow, wrap around
   380  c1d2 18                         CLC
   381  c1d3 a5a5                       LDA gaddr	; x+8: gaddr += 8
   382  c1d5 6908                       ADC #8
   383  c1d7 85a5                       STA gaddr
   384  c1d9 9002                       BCC +
   385  c1db e6a6                       INC gaddr+1
   386                          
   387  c1dd 88                 +	DEY		; y--
   388  c1de 100f                       BPL +++
   389  c1e0 38                         SEC		; y overflow
   390  c1e1 a5a5                       LDA gaddr
   391  c1e3 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   392  c1e5 85a5                       STA gaddr
   393  c1e7 a5a6                       LDA gaddr+1
   394  c1e9 e901                       SBC #$01
   395  c1eb 85a6                       STA gaddr+1
   396  c1ed a007                       LDY #7		; wrap around
   397                          
   398  c1ef c6a3               +++	DEC cl		; until c=0
   399  c1f1 d0c4                       BNE loop_yup_xright
   400  c1f3 4c70c1                     JMP gexit
   401                          
   402                          
   403                          ;-----------------------------------------------------------------
   404                          
   405                          ; line x right, y up, dx > dy (case 2)
   406                          
   407                          line_up_flat
   408  c1f6 208fc1                     JSR position	; x,y
   409                          loop_xright_yup
   410  c1f9 2080c1                     JSR gchange	; pixel
   411                          
   412  c1fc 18                         CLC		; k += dy
   413  c1fd a595                       LDA kl
   414  c1ff 65a9                       ADC dy
   415  c201 8595                       STA kl
   416  c203 9002                       BCC ++
   417  c205 e696                       INC kh
   418                          
   419  c207 c5ab               ++	CMP dxl		; k > dx?
   420  c209 a596                       LDA kh
   421  c20b e5a7                       SBC dxh
   422  c20d 901a                       BCC +
   423                          
   424  c20f 8596                       STA kh		; k -= dx
   425  c211 a595                       LDA kl
   426  c213 e5ab                       SBC dxl
   427  c215 8595                       STA kl
   428                          
   429  c217 88                         DEY		; y--
   430  c218 100f                       BPL +
   431  c21a 38                         SEC		; y overflow
   432  c21b a5a5                       LDA gaddr
   433  c21d e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   434  c21f 85a5                       STA gaddr
   435  c221 a5a6                       LDA gaddr+1
   436  c223 e901                       SBC #$01
   437  c225 85a6                       STA gaddr+1
   438  c227 a007                       LDY #7		; wrap around
   439                          
   440  c229 e8                 +	INX		; x++
   441  c22a e008                       CPX #8		; x overflow?
   442  c22c d00d                       BNE ++
   443  c22e a200                       LDX #0		; wrap around
   444  c230 18                         CLC
   445  c231 a5a5                       LDA gaddr	; x+8: gaddr += 8
   446  c233 6908                       ADC #8
   447  c235 85a5                       STA gaddr
   448  c237 9002                       BCC ++
   449  c239 e6a6                       INC gaddr+1
   450                          ++
   451  c23b a5a3               	LDA cl		; c--
   452  c23d d002                       BNE +++
   453  c23f c6a4                       DEC ch
   454  c241 c6a3               +++	DEC cl
   455                          
   456  c243 a5a3                       LDA cl		; until c=0
   457  c245 05a4                       ORA ch
   458  c247 d0b0                       BNE loop_xright_yup
   459  c249 4c70c1                     JMP gexit
   460                          
   461                          
   462                          
   463                          ;-----------------------------------------------------------------
   464                          
   465                          ; line x right, y down, dx > dy (case 3)
   466                          
   467                          line_down_flat
   468  c24c 208fc1                     JSR position	; x,y
   469                          loop_xright_ydown
   470  c24f 2080c1                     JSR gchange	; pixel
   471                          
   472  c252 18                         CLC		; k += dy
   473  c253 a595                       LDA kl
   474  c255 65a9                       ADC dy
   475  c257 8595                       STA kl
   476  c259 9002                       BCC ++
   477  c25b e696                       INC kh
   478                          
   479  c25d c5ab               ++	CMP dxl		; k > dx
   480  c25f a596                       LDA kh
   481  c261 e5a7                       SBC dxh		; k -= dx
   482  c263 901c                       BCC +
   483                          
   484  c265 8596                       STA kh
   485  c267 a595                       LDA kl
   486  c269 e5ab                       SBC dxl
   487  c26b 8595                       STA kl
   488                          
   489  c26d c8                         INY		; y++
   490  c26e c008                       CPY #8
   491  c270 d00f                       BNE +
   492  c272 18                         CLC		; y overflow
   493  c273 a5a5                       LDA gaddr
   494  c275 6940                       ADC #$40	; y+8: gaddr += 40*8 ($140)
   495  c277 85a5                       STA gaddr
   496  c279 a5a6                       LDA gaddr+1
   497  c27b 6901                       ADC #$01
   498  c27d 85a6                       STA gaddr+1
   499  c27f a000                       LDY #0		; wrap around
   500                          
   501  c281 e8                 +	INX		; x++
   502  c282 e008                       CPX #8		; x overflow ?
   503  c284 d00d                       BNE +++
   504  c286 a200                       LDX #$00	; wrap around
   505  c288 18                         CLC		; gaddr += 8
   506  c289 a5a5                       LDA gaddr
   507  c28b 6908                       ADC #$08
   508  c28d 85a5                       STA gaddr
   509  c28f 9002                       BCC +++
   510  c291 e6a6                       INC gaddr+1
   511                          +++
   512  c293 a5a3               	LDA cl		; c--
   513  c295 d002                       BNE ++
   514  c297 c6a4                       DEC ch
   515  c299 c6a3               ++	DEC cl
   516                          
   517  c29b a5a3                       LDA cl		; until c=0
   518  c29d 05a4                       ORA ch
   519  c29f d0ae                       BNE loop_xright_ydown
   520  c2a1 4c70c1                     JMP gexit
   521                          
   522                          
   523                          ;-----------------------------------------------------------------
   524                          
   525                          ; line y down, x right, dx < dy (case 4)
   526                          
   527                          line_up_steep
   528  c2a4 208fc1                     JSR position	; x,y
   529                          loop_ydown_xright
   530  c2a7 2080c1                     JSR gchange	; pixel
   531                          
   532  c2aa 18                         CLC		; k += dx
   533  c2ab a595                       LDA kl
   534  c2ad 65ab                       ADC dxl		; dxh is 0, because dx < dy
   535  c2af 8595                       STA kl
   536  c2b1 b004                       BCS ++
   537  c2b3 c5a9                       CMP dy		; k > dy?
   538  c2b5 9016                       BCC +
   539  c2b7 e5a9               ++	SBC dy		; k -= dy
   540  c2b9 8595                       STA kl
   541                          
   542  c2bb e8                         INX		; x++
   543  c2bc e008                       CPX #8
   544  c2be d00d                       BNE +		; x overflow?
   545  c2c0 a200                       LDX #0		; wrap around
   546  c2c2 18                         CLC
   547  c2c3 a5a5                       LDA gaddr	; x+9: gaddr += 8
   548  c2c5 6908                       ADC #8
   549  c2c7 85a5                       STA gaddr
   550  c2c9 9002                       BCC +
   551  c2cb e6a6                       INC gaddr+1
   552                          
   553  c2cd c8                 +	INY		; y++
   554  c2ce c008                       CPY #8		; y overflow?
   555  c2d0 d00f                       BNE +++
   556  c2d2 18                         CLC
   557  c2d3 a5a5                       LDA gaddr
   558  c2d5 6940                       ADC #$40	; y+8: gaddr += 40*8 ($140)
   559  c2d7 85a5                       STA gaddr
   560  c2d9 a5a6                       LDA gaddr+1
   561  c2db 6901                       ADC #$01
   562  c2dd 85a6                       STA gaddr+1
   563  c2df a000                       LDY #0		; wrap around
   564                          
   565  c2e1 c6a3               +++	DEC cl		; c--
   566                          			; until c=0
   567  c2e3 d0c2                       BNE loop_ydown_xright
   568  c2e5 4c70c1                     JMP gexit
   569                          
   570                          
   571                          ;-----------------------------------------------------------------
   572                          
   573                          getcommaxy
   574  c2e8 20fdae                     JSR b_getcomma	; check ","
   575                          getxy
   576  c2eb 208aad                     JSR b_getval	; get X coord. value
   577  c2ee 20f7b7                     JSR b_convint
   578  c2f1 c901                       CMP #>xmax
   579  c2f3 9009               	BCC gcxy_xok
   580  c2f5 f003                       BEQ +		; X = $1xx
   581                          error_iq
   582  c2f7 4c48b2                     JMP b_illquant
   583  c2fa c040               +	CPY #<xmax	; check X low
   584  c2fc b0f9                       BCS error_iq	; X to big
   585                          gcxy_xok
   586  c2fe 84fb                       STY gpos	; temporary save X coord.
   587  c300 85fc                       STA gpos+1
   588                          
   589  c302 20f1b7                     JSR b_getcomma8bit
   590                          			; get Y coord. value
   591  c305 e0c8                       CPX #ymax
   592  c307 b0ee                       BCS error_iq	; Y to big
   593                          
   594  c309 a4fb                       LDY gpos	; restory X coord.
   595  c30b a5fc                       LDA gpos+1
   596  c30d 60                         RTS
   597                          
   598                          
   599                          ;-----------------------------------------------------------------
   600                          
   601                          hline
   602  c30e 20ebc2                     JSR getxy	; get startpoint
   603  c311 86aa                       STX y
   604  c313 8eebc0                     STX savey	; save as cursor, too
   605  c316 859c                       STA xh
   606  c318 849b                       STY xl
   607  c31a 20fdae                     JSR b_getcomma	; get length
   608  c31d 208aad                     JSR b_getval
   609  c320 20f7b7                     JSR b_convint
   610                          
   611  c323 c901                       CMP #>xmax
   612  c325 9006                       BCC +		; X < 256
   613  c327 d0ce                       BNE error_iq
   614  c329 c040                       CPY #<xmax
   615  c32b b0ca                       BCS error_iq
   616                          +
   617                          			; calculate end point
   618  c32d aa                         TAX		; save length high byte
   619  c32e 98                         TYA		; length low byte
   620  c32f 18                         CLC
   621  c330 659b                       ADC xl		; low xend = x+length
   622  c332 859e                       STA xendl
   623  c334 8de9c0                     STA savexl	; also save as cursor
   624  c337 8a                         TXA		; high
   625  c338 659c                       ADC xh		; high xend = x+length
   626  c33a 859f                       STA xendh
   627  c33c 8deac0                     STA savexh
   628                          			; BUG: endposition not checked!
   629                          
   630  c33f 2078c1                     JSR ginit	; map in graphic memory
   631                          
   632                          hline_start
   633  c342 a59e                       LDA xendl
   634  c344 c59b                       CMP xl
   635  c346 a59f                       LDA xendh
   636  c348 e59c                       SBC xh
   637  c34a b013                       BCS hl_noxswap	; xend < x ->
   638                          
   639  c34c a69e                       LDX xendl	; swap x, xend
   640  c34e a59b                       LDA xl
   641  c350 869b                       STX xl
   642  c352 859e                       STA xendl
   643  c354 a69f                       LDX xendh
   644  c356 a49c                       LDY xh
   645  c358 849f                       STY xendh
   646  c35a 869c                       STX xh
   647  c35c 4c6ec3                     JMP hl_start	; x != xend
   648                          
   649                          hl_noxswap
   650  c35f a59e                       LDA xendl
   651  c361 c59b                       CMP xl
   652  c363 d009                       BNE hl_start
   653  c365 a59f                       LDA xendh
   654  c367 c59c                       CMP xh
   655  c369 d003                       BNE hl_start
   656                          			; x = xend
   657                          			; CHANGE: shouldn't it be plot?
   658  c36b 4c70c1                     JMP gexit
   659                          
   660                          hl_start
   661  c36e 208fc1                     JSR position	; graphic position x,y
   662  c371 bdedc0                     LDA maskleft,X
   663  c374 48                         PHA		; save left end mask
   664  c375 a59e                       LDA xendl
   665  c377 2907                       AND #%00000111
   666  c379 8596                       STA tmp2	; xend mod 8, mask index
   667  c37b a59b                       LDA xl
   668  c37d 29f8                       AND #%11111000	; (xl div 8) * 8
   669  c37f 8595                       STA tmp1
   670  c381 a59e                       LDA xendl	; (xend div 8) * 8
   671  c383 29f8                       AND #%11111000
   672  c385 38                         SEC
   673  c386 e595                       SBC tmp1
   674  c388 8595                       STA tmp1
   675  c38a a59f                       LDA xendh
   676  c38c e59c                       SBC xh		; (xend div 8 - x div 8 ) *8
   677  c38e 4a                         LSR		; high -> carry
   678  c38f a595                       LDA tmp1
   679  c391 6a                         ROR		; / 8 ->  0-39
   680  c392 6a                         ROR
   681  c393 6a                         ROR
   682  c394 aa                         TAX		; 8-pixel-blocks count
   683  c395 68                         PLA		; left end x mask
   684                          
   685                          hl_nextblock
   686  c396 ca                         DEX
   687                          hl_islastblock
   688  c397 3014                       BMI hl_lastblock
   689                          			; leave loop if X<0
   690  c399 2088c1                     JSR gmask	; first with left end mask
   691  c39c 18                         CLC		; gaddr += 8
   692  c39d a5a5                       LDA gaddr
   693  c39f 6908                       ADC #8
   694  c3a1 85a5                       STA gaddr
   695  c3a3 9002                       BCC +
   696  c3a5 e6a6                       INC gaddr+1
   697  c3a7 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   698                          ; CHANGE: optimize: replace ...
   699  c3a9 ca                         DEX		; CHANGE: replace
   700  c3aa 4c97c3                     JMP hl_islastblock	; CHANGE: replace
   701                          ; CHANGE: ... by:
   702                          ;	BNE hl_nextblock	; always
   703                          
   704                          hl_lastblock
   705  c3ad a696                       LDX tmp2	; xend mask index
   706  c3af 3df5c0                     AND maskright,X ; mask right end
   707  c3b2 2088c1                     JSR gmask	; modify
   708  c3b5 4c70c1                     JMP gexit	; leave
   709                          
   710                          
   711                          ;-----------------------------------------------------------------
   712                          
   713                          vline
   714  c3b8 20ebc2                     JSR getxy	; get startpoint
   715  c3bb 859c                       STA xh
   716  c3bd 8deac0                     STA savexh	; save as cursor too
   717  c3c0 849b                       STY xl
   718  c3c2 8ce9c0                     STY savexl
   719  c3c5 86aa                       STX y
   720                          
   721  c3c7 20f1b7                     JSR b_getcomma8bit
   722                          			; get length
   723  c3ca 18                         CLC		; calculate end point
   724  c3cb 8a                         TXA		; length
   725                          ; DON'T-CHANGE: how long to go vertically (needed later)
   726                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   727                          ;	STA tmp1
   728  c3cc 65aa                       ADC y		; length + y
   729  c3ce c9c8                       CMP #ymax
   730  c3d0 9003                       BCC +
   731  c3d2 4c48b2                     JMP b_illquant
   732                          
   733  c3d5 8593               +	STA yend	; endpoint
   734                          ;	STA savey	; BUG: missing
   735                          
   736  c3d7 2078c1                     JSR ginit	; map in graphic memory
   737                          
   738                          vline_start
   739  c3da a593                       LDA yend
   740  c3dc c5aa                       CMP y
   741  c3de b00b                       BCS vl_noyswap	; yend < y ->
   742  c3e0 a5aa                       LDA y		; swap y, yend
   743  c3e2 a693                       LDX yend
   744  c3e4 8593                       STA yend
   745  c3e6 86aa                       STX y
   746  c3e8 4cf0c3                     JMP vl_start	; CHANGE: replace by (be relative)
   747                          ;	BEQ vl_start	; always (with next branch)
   748                          
   749                          vl_noyswap
   750  c3eb d003                       BNE vl_start	; y = yend ->
   751                          			; CHANGE: shouldn't it be plot?
   752  c3ed 4c70c1                     JMP gexit
   753                          
   754                          vl_start
   755  c3f0 208fc1                     JSR position	; graphic position x,y
   756  c3f3 bdbcc0                     LDA bitmask,X
   757  c3f6 8596                       STA tmp2	; save mask
   758                          ; CHANGE: replace ...
   759  c3f8 38                         SEC
   760  c3f9 a593                       LDA yend
   761  c3fb e5aa                       SBC y		; vertical length
   762  c3fd aa                         TAX
   763                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   764                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   765                          ;	LDX tmp1
   766  c3fe e8                         INX		; +1 (exit on 0)
   767                          vl_nextline
   768  c3ff a596                       LDA tmp2
   769  c401 2088c1                     JSR gmask	; modify 
   770  c404 c8                         INY		; go down
   771  c405 c008                       CPY #8		; 8-line wrap
   772  c407 d00f                       BNE +
   773  c409 18                         CLC		; CHANGE: optimize: C is already set
   774  c40a a5a5                       LDA gaddr	; gaddr += 320
   775  c40c 6940                       ADC #$40	; CHANGE: optimize: replace by
   776                          ;	ADC #$40-1	; compensate for C = 1
   777  c40e 85a5                       STA gaddr
   778  c410 a5a6                       LDA gaddr+1
   779  c412 6901                       ADC #$01
   780  c414 85a6                       STA gaddr+1
   781  c416 a000                       LDY #0		; wrap y offset
   782  c418 ca                 +	DEX		; all vertical positions done?
   783  c419 d0e4                       BNE vl_nextline
   784  c41b 4c70c1                     JMP gexit	; leave
   785                          
   786                          
   787                          ;-----------------------------------------------------------------
   788                          
   789                          line
   790  c41e 20ebc2                     JSR getxy	; get startpoint
   791  c421 849b                       STY xl 
   792  c423 859c                       STA xh
   793  c425 86aa                       STX y
   794                          
   795  c427 20e8c2                     JSR getcommaxy	; get endpoint
   796                          line_start
   797  c42a 8ce9c0                     STY savexl	; save as cursor position too
   798  c42d 849e                       STY xendl
   799  c42f 8deac0                     STA savexh
   800  c432 859f                       STA xendh
   801  c434 8eebc0                     STX savey
   802  c437 8693                       STX yend
   803                          
   804  c439 2078c1                     JSR ginit	; map in graphic memory
   805                          
   806  c43c a000                       LDY #$00	; initialize to 0
   807  c43e 84a8                       STY ydir
   808  c440 8495                       STY kl
   809  c442 8496                       STY kh
   810                          
   811  c444 38                         SEC
   812  c445 a59e                       LDA xendl	; calculate dx
   813  c447 e59b                       SBC xl
   814  c449 85ab                       STA dxl
   815  c44b a59f                       LDA xendh
   816  c44d e59c                       SBC xh
   817  c44f 85a7                       STA dxh
   818                          
   819  c451 b026                       BCS li_xend_right
   820  c453 98                         TYA		; negate dx
   821  c454 38                         SEC		; dx = 0 - dx
   822  c455 e5ab                       SBC dxl
   823  c457 85ab                       STA dxl
   824  c459 98                         TYA
   825  c45a e5a7                       SBC dxh
   826  c45c 85a7                       STA dxh
   827                          
   828  c45e a69b                       LDX xl		; swap x low
   829  c460 a49e                       LDY xendl
   830  c462 869e                       STX xendl
   831  c464 849b                       STY xl
   832                          
   833  c466 a69c                       LDX xh		; swap x high
   834  c468 a49f                       LDY xendh
   835  c46a 869f                       STX xendh
   836  c46c 849c                       STY xh
   837                          
   838  c46e a6aa                       LDX y		; swap y
   839  c470 a493                       LDY yend
   840  c472 8693                       STX yend
   841  c474 84aa                       STY y
   842                          
   843  c476 4c82c4                     JMP li_x_different
   844                          			; CHANGE: branch?
   845                          
   846                          li_xend_right
   847  c479 a5ab                       LDA dxl		; dx = 0?
   848  c47b 05a7                       ORA dxh
   849  c47d d003                       BNE li_x_different
   850  c47f 4cdac3                     JMP vline_start	; vertical line case
   851                          
   852                          li_x_different
   853  c482 38                         SEC		; calculate dy
   854  c483 a593                       LDA yend
   855  c485 e5aa                       SBC y
   856  c487 b006                       BCS li_y_right
   857  c489 49ff                       EOR #$FF	; negate dy (two's complement)
   858  c48b 6901                       ADC #$01	; C=0
   859  c48d 85a8                       STA ydir	; flag y goes up
   860                          
   861                          li_y_right
   862  c48f 85a9                       STA dy
   863  c491 d003                       BNE +
   864  c493 4c42c3                     JMP hline_start	; horizontal line case
   865                          +
   866                          
   867  c496 a5ab                       LDA dxl		; CHANGE: remove: dx and dy is *always* !=0 !!!
   868  c498 05a7                       ORA dxh		; CHANGE: remove
   869  c49a 05a9                       ORA dy		; CHANGE: remove
   870  c49c d003                       BNE +		; CHANGE: remove
   871  c49e 4c70c1                     JMP gexit	; CHANGE: remove
   872                          +
   873                          
   874  c4a1 a5a7                       LDA dxh		; dx > dy
   875  c4a3 d017                       BNE line_flat	; yes -> flat
   876  c4a5 a5a9                       LDA dy		; no -> steep
   877  c4a7 aa                         TAX
   878  c4a8 c5ab                       CMP dxl
   879  c4aa 9010                       BCC line_flat
   880                          
   881                          line_steep
   882  c4ac e8                         INX	
   883  c4ad 86a3                       STX cl		; c = dy+1
   884  c4af 4a                         LSR		; k = dy/2
   885  c4b0 8595                       STA kl
   886  c4b2 a5a8                       LDA ydir
   887  c4b4 d003                       BNE +
   888  c4b6 4ca4c2                     JMP line_up_steep	; y down, steep
   889  c4b9 4cb4c1             +	JMP line_down_steep	; y up, steep
   890                          
   891                          line_flat
   892  c4bc a5a7                       LDA dxh
   893  c4be a8                         TAY
   894  c4bf a6ab                       LDX dxl
   895  c4c1 e8                         INX
   896  c4c2 d001                       BNE +
   897  c4c4 c8                         INY
   898  c4c5 86a3               +	STX cl		; c = dx+1
   899  c4c7 84a4                       STY ch
   900                          
   901  c4c9 4a                         LSR		; k = dx/2
   902  c4ca 8596                       STA kh
   903  c4cc a5ab                       LDA dxl
   904  c4ce 6a                         ROR		; dx/2
   905  c4cf 8595                       STA kl
   906  c4d1 a5a8                       LDA ydir	
   907  c4d3 d003                       BNE +
   908  c4d5 4c4cc2                     JMP line_down_flat	; y down, flat
   909  c4d8 4cf6c1             +	JMP line_up_flat	; y up, flat
   910                          
   911                          ;-----------------------------------------------------------------
   912                          
   913                          plot
   914  c4db 20ebc2                     JSR getxy	; get parameter
   915  c4de 859c                       STA xh		; save x/y
   916  c4e0 849b                       STY xl
   917  c4e2 86aa                       STX y
   918  c4e4 8deac0                     STA savexh	; and store as cursor
   919  c4e7 8ce9c0                     STY savexl
   920  c4ea 8eebc0                     STX savey
   921                          
   922                          ; CHANGE: shift SEI 2 lines lower immediate before STA
   923  c4ed 78                         SEI			
   924  c4ee a501                       LDA prozport
   925  c4f0 29fd                       AND #%11111101	; Kernal ROM disable
   926  c4f2 8501                       STA prozport
   927                          
   928                          ; CHANGE: postion calculation to be moved before Kernal ROM disable
   929  c4f4 208fc1                     JSR position	; calculate graphical address
   930  c4f7 2080c1                     JSR gchange	; change graphical data
   931                          
   932                          			; CHANGE: optimize: JMP gexit
   933  c4fa a501                       LDA prozport
   934  c4fc 0902                       ORA #%00000010	; kernal ROM enable
   935  c4fe 8501                       STA prozport
   936  c500 58                         CLI
   937  c501 60                         RTS
   938                          
   939                          ;-----------------------------------------------------------------
   940                          
   941                          move
   942  c502 20ebc2                     JSR getxy	; get parameter
   943  c505 8deac0                     STA savexh	; just save as cursor
   944  c508 8ce9c0                     STY savexl
   945  c50b 8eebc0                     STX savey
   946  c50e 60                         RTS
   947                          
   948                          
   949                          ;-----------------------------------------------------------------
   950                          
   951                          setmode
   952  c50f 209eb7                     JSR b_get8bit
   953  c512 e003                       CPX #$03
   954  c514 9003                       BCC +
   955  c516 4c48b2                     JMP b_illquant
   956  c519 e001               +	CPX #$01
   957  c51b b01a                       BCS set_or_toggle
   958                          
   959                          modereset
   960  c51d a9c0                       LDA #>(nbitmask)
   961  c51f 8d84c1                     STA gchange_op+2
   962  c522 a9c4                       LDA #<(nbitmask)
   963  c524 8d83c1                     STA gchange_op+1
   964  c527 a93d                       LDA #$3D		; AND abs,X
   965  c529 8d82c1                     STA gchange_op
   966  c52c a931                       LDA #$31		; AND (zp),Y
   967  c52e 8d8ac1                     STA gmask_op
   968  c531 a9ff                       LDA #$FF		; EOR $#FF, invertieren
   969  c533 8d89c1                     STA gmask_flip+1
   970  c536 60                         RTS
   971                          
   972                          set_or_toggle
   973  c537 d01a                       BNE modetoggle
   974                          modeset
   975  c539 a9c0                       LDA #>(bitmask)
   976  c53b 8d84c1                     STA gchange_op+2
   977  c53e a9bc                       LDA #<(bitmask)
   978  c540 8d83c1                     STA gchange_op+1
   979  c543 a91d                       LDA #$1D		; OR abs,X
   980  c545 8d82c1                     STA gchange_op
   981  c548 a911                       LDA #$11		; OR (zp),Y
   982  c54a 8d8ac1                     STA gmask_op
   983  c54d a900                       LDA #$00		; EOR #$00, nicht invertieren
   984  c54f 8d89c1                     STA gmask_flip+1
   985  c552 60                         RTS
   986                          
   987                          modetoggle
   988  c553 a9c0                       LDA #>(bitmask)
   989  c555 8d84c1                     STA gchange_op+2
   990  c558 a9bc                       LDA #<(bitmask)
   991  c55a 8d83c1                     STA gchange_op+1
   992  c55d a95d                       LDA #$5D		; EOR abs,X
   993  c55f 8d82c1                     STA gchange_op
   994  c562 a951                       LDA #$51		; EOR (zp),Y
   995  c564 8d8ac1                     STA gmask_op
   996  c567 a900                       LDA #$00		; EOR #$00, nicht invertieren
   997  c569 8d89c1                     STA gmask_flip+1
   998  c56c 60                         RTS
   999                          
  1000                          
  1001                          ;-----------------------------------------------------------------
  1002                          
  1003                          ; get pixel (check if pixel set)
  1004                          ; not used
  1005                          
  1006                          get
  1007  c56d 20e8c2                     JSR getcommaxy
  1008  c570 859c                       STA xh
  1009  c572 849b                       STY xl
  1010  c574 86aa                       STX y
  1011                          
  1012                          ; CHANGE: shift 2 lines below
  1013  c576 78                         SEI
  1014  c577 a501                       LDA prozport
  1015  c579 29fd               	AND #%11111101	; Kernal ROM disable
  1016  c57b 8501                       STA prozport
  1017                          
  1018                          ; CHANGE: move before ROM disable ...
  1019  c57d 208fc1                     JSR position
  1020  c580 b1a5                       LDA (gaddr),Y
  1021  c582 3dbcc0                     AND bitmask,X
  1022  c585 a8                         TAY
  1023  c586 a501                       LDA prozport
  1024  c588 0902               	ORA #%00000010	; kernal ROM enable
  1025  c58a 8501                       STA prozport
  1026  c58c 58                         CLI
  1027  c58d 4ca2b3                     JMP b_byte2fac
  1028                          
  1029                          
  1030  c590 20fdae                     JSR b_getcomma	; not used
  1031                          
  1032                          
  1033                          ;-----------------------------------------------------------------
  1034                          
  1035                          relto
  1036  c593 208aad                     JSR b_getval	; get X offset (+/-)
  1037  c596 a561               	LDA facexp	; FAC exponent
  1038  c598 c990               	CMP #$90	; more than 16 bit
  1039  c59a b031               	BCS relto_error	; illegal quantity
  1040  c59c 209bbc                     JSR b_fac2int	; to signed integer
  1041                          
  1042  c59f 18                         CLC
  1043  c5a0 a565                       LDA facintl
  1044  c5a2 6de9c0                     ADC savexl
  1045  c5a5 859e                       STA xendl
  1046  c5a7 a564                       LDA facinth
  1047  c5a9 6deac0                     ADC savexh
  1048  c5ac 859f                       STA xendh	; xend = savex+facint
  1049                          
  1050  c5ae 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1051  c5b1 208aad                     JSR b_getval
  1052  c5b4 a561                       LDA facexp	; FAC exponent
  1053  c5b6 c990                       CMP #$90	; more than 16 bit
  1054  c5b8 b013                       BCS relto_error	; illegal quantity
  1055  c5ba 209bbc                     JSR b_fac2int	; to signed integer
  1056  c5bd 18                         CLC
  1057  c5be a565                       LDA facintl
  1058  c5c0 6debc0                     ADC savey
  1059  c5c3 8593                       STA yend	; yend = savey+facint
  1060                          
  1061  c5c5 a59f                       LDA xendh	; check end coord. x
  1062  c5c7 c901                       CMP #>xmax
  1063  c5c9 900b                       BCC rt_xok
  1064  c5cb f003                       BEQ +
  1065                          relto_error
  1066  c5cd 4c48b2                     JMP b_illquant
  1067  c5d0 a59e               +	LDA xendl
  1068  c5d2 c940                       CMP #<xmax
  1069  c5d4 b0f7                       BCS relto_error
  1070                          rt_xok
  1071  c5d6 a593                       LDA yend	; check end coord. y
  1072  c5d8 c9c8                       CMP #ymax
  1073  c5da b0f1                       BCS relto_error
  1074                          
  1075  c5dc ade9c0                     LDA savexl
  1076  c5df 859b                       STA xl
  1077  c5e1 adeac0                     LDA savexh
  1078  c5e4 859c                       STA xh
  1079  c5e6 adebc0                     LDA savey
  1080  c5e9 85aa                       STA y
  1081  c5eb a49e                       LDY xendl
  1082  c5ed a59f                       LDA xendh
  1083  c5ef a693                       LDX yend	; xend/yend = cursor + x/y
  1084                          
  1085  c5f1 4c2ac4                     JMP line_start	; draw line x/y to xend/yend
  1086                          
  1087                          
  1088                          ;-----------------------------------------------------------------
  1089                          
  1090                          char
  1091  c5f4 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1092  c5f7 e028                       CPX #40	
  1093  c5f9 9003                       BCC +
  1094                          char_error
  1095  c5fb 4c48b2                     JMP b_illquant
  1096  c5fe 86fb               +	STX gpos	; save x coord.
  1097  c600 20f1b7                     JSR b_getcomma8bit
  1098                          			; get char. position y 0-24
  1099  c603 e019                       CPX #25
  1100  c605 b0f4                       BCS char_error
  1101  c607 86fc                       STX gpos+1	; save y coord.
  1102                          
  1103  c609 20fdae                     JSR b_getcomma	; get string
  1104  c60c 209ead                     JSR b_getexpr
  1105  c60f 20a3b6                     JSR b_stringval ; string address in str
  1106  c612 48                         PHA		; string length
  1107  c613 a6fc                       LDX gpos+1	; y coord. for char. position
  1108  c615 8a                         TXA
  1109  c616 2903                       AND #$03	; mask 2 bits
  1110  c618 a8                         TAY		; table index
  1111  c619 a900                       LDA #$00
  1112  c61b 85fc                       STA gpos+1	; x high
  1113  c61d a5fb                       LDA gpos	; saved x: multiply by 8
  1114  c61f 0a                         ASL
  1115  c620 0a                         ASL
  1116  c621 0a                         ASL
  1117  c622 26fc                       ROL gpos+1	; overflow to high byte
  1118  c624 79ccc0                     ADC ytabl,Y
  1119  c627 85a5                       STA gaddr
  1120  c629 a5fc                       LDA gpos+1	; x high
  1121  c62b 7dd0c0                     ADC ytabh,X
  1122  c62e 85a6                       STA gaddr+1
  1123  c630 68                         PLA		; string length
  1124  c631 a000                       LDY #$00	; string index
  1125  c633 aa                         TAX		; length
  1126  c634 e8                         INX		; prepare as counter
  1127                          char_loop
  1128  c635 ca                         DEX
  1129  c636 f008                       BEQ char_exit
  1130  c638 b122                       LDA (str),Y	; read string
  1131  c63a 2041c6                     JSR char_display
  1132  c63d c8                         INY
  1133  c63e d0f5                       BNE char_loop
  1134                          char_exit
  1135  c640 60                         RTS
  1136                          
  1137                          char_display
  1138  c641 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1139  c643 8a                         TXA		; save register X+Y
  1140  c644 48                         PHA
  1141  c645 98                         TYA
  1142  c646 48                         PHA
  1143  c647 a5d7                       LDA z_tmp	; get saved character
  1144  c649 1049                       BPL char_normal
  1145                          
  1146                          char_inverse
  1147  c64b 297f                       AND #%01111111	; mask bit 7
  1148  c64d c97f                       CMP #%01111111	; was 255? (pi)
  1149  c64f d002                       BNE +
  1150  c651 a95e                       LDA #$5E	; screen code for pi
  1151  c653 c920               +	CMP #$20	; control character?
  1152  c655 9038                       BCC char_disp_leave
  1153                          			; yes, skip
  1154  c657 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1155                          			; $C0-$FF -> $40-$7F
  1156                          			; OPT: BNE char_hires
  1157                          			; OPT: char_normal
  1158                          char_hires
  1159  c659 a6c7                       LDX z_reverseflag
  1160  c65b f002                       BEQ +
  1161  c65d 0980                       ORA #%10000000	; invert char.
  1162  c65f 48                 +	PHA		; save char. for later
  1163  c660 78                         SEI
  1164  c661 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1165  c663 8501                       STA prozport	; char. rom base = $D000
  1166  c665 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1167  c667 85fc                       STA gpos+1	; 
  1168  c669 68                         PLA		; char. code
  1169  c66a 0a                         ASL		; *8
  1170  c66b 26fc                       ROL gpos+1
  1171  c66d 0a                         ASL
  1172  c66e 26fc                       ROL gpos+1
  1173  c670 0a                         ASL
  1174  c671 26fc                       ROL gpos+1
  1175  c673 85fb                       STA gpos	; addr. in char. rom for char.
  1176                          
  1177  c675 a007                       LDY #$07	; 8 hires lines
  1178                          char_line
  1179  c677 b1fb                       LDA (gpos),Y	; read character line
  1180  c679 2088c1                     JSR gmask	; write to hires screen
  1181  c67c 88                         DEY
  1182  c67d 10f8                       BPL char_line
  1183                          
  1184  c67f a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1185  c681 8501                       STA prozport
  1186  c683 58                         CLI
  1187                          
  1188  c684 18                         CLC		; step char position to left
  1189  c685 a5a5                       LDA gaddr	; ( +8 )
  1190  c687 6908                       ADC #$08
  1191  c689 85a5                       STA gaddr
  1192  c68b 9002                       BCC +
  1193  c68d e6a6                       INC gaddr+1
  1194                          +
  1195                          char_disp_leave
  1196  c68f 68                 	PLA		; pass written character back
  1197  c690 a8                         TAY		; restore saved registers
  1198  c691 68                         PLA
  1199  c692 aa                         TAX
  1200  c693 60                         RTS
  1201                          
  1202                          char_normal
  1203  c694 c920                       CMP #$20	; control character?
  1204  c696 90f7                       BCC char_disp_leave
  1205  c698 c960                       CMP #$60
  1206  c69a 9004                       BCC +
  1207  c69c 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1208  c69e d002                       BNE ++
  1209  c6a0 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1210  c6a2 4c59c6             ++	JMP char_hires	; 		OPT: Bxx
  1211                          
  1212                          
  1213                          ;-----------------------------------------------------------------
  1214                          
  1215                          to
  1216  c6a5 ade9c0                     LDA savexl
  1217  c6a8 859b                       STA xl
  1218  c6aa adeac0                     LDA savexh
  1219  c6ad 859c                       STA xh
  1220  c6af adebc0                     LDA savey
  1221  c6b2 85aa                       STA y
  1222  c6b4 20ebc2                     JSR getxy
  1223  c6b7 4c2ac4                     JMP line_start
  1224                          
  1225  c6ba 000000000000       	!by 0,0,0,0,0,0
