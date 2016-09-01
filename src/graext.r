
; ******** Source: graext.asm
     1                          !to "graext.o",cbm	
     2                          
     3                          ;  **** gra-ext ****
     4                          ;
     5                          ; 2015-10-05 johann e. klasek, johann at klasek at
     6                          ;
     7                          ; revisions:
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
    21                          ;command_rts_style=1
    22                          
    23                          *= $c000
    24                          
    25                          
    26                          ; basic interpreter registers, addresses and entry points
    27                          
    28                          str     = $22		; string address
    29                          ijmp    = $55		; address of JMP (addr)
    30                          chrget  = $73		; basic charget routine
    31                          facintl = $65		; integer result from b_fac2int
    32                          facinth = $64
    33                          facexp  = $61		; fac exponent, after b_getval
    34                          
    35                          z_reverseflag = $C7	; character routine
    36                          z_lastkey = $D7		; original use case, unused here
    37                          z_tmp = z_lastkey	; temporary reused for character routine
    38                          
    39                          v_bascmd = $0308
    40                          
    41                          basic_rom = $A000	; start of BASIC ROM
    42                          
    43                          b_interpreter =$A7AE	; interpreter loop
    44                          b_execstatement =$A7E7	; process statement
    45                          b_getcomma = $AEFD	; read comma from basic text
    46                          b_illquant = $B248	; error "illegal quantity"
    47                          b_get8bit = $B79E	; read 8 bit numeric value from
    48                          			; basic text
    49                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    50                          			; from basic text
    51                          b_getval = $AD8A	; read numeric value from basic text
    52                          b_getexpr = $AD9E	; read expression from basic text
    53                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    54                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    55                          b_fac2int = $BC9B	; convert FAC to integer
    56                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    57                          
    58                          ; hardware registers and values
    59                          
    60                          prozport = $01		; processor port
    61                          memrom = %00110111	; basic+kernal rom
    62                          membas = %00110110	; basic ram+kernal rom
    63                          memram = %00110101	; basic+kernal ram
    64                          
    65                          vic_cr	= $D011		; VIC control register
    66                          vic_mcr	= $D018		; VIC memory control register
    67                          cia_pra	= $DD00		; CIA 2 port register A
    68                          
    69                          cram	= $CC00		; start of color ram
    70                          
    71                          gram	= $e000		; start of graphic bitmap ram
    72                          gramp	= gram >> 8	; start page of bitmap
    73                          
    74                          ; constants 
    75                          
    76                          xmax	= 320		; max x dimension
    77                          ymax	= 200		; max y dimension
    78                          
    79                          ; zeropage variables
    80                          
    81                          x	= $9B		; start coordinate x, low+high
    82                          xl	= x
    83                          xh	= x+1
    84                          y	= $AA		; start coordinate y
    85                          
    86                          xendl	= $9E		; end coordinate x, low+high
    87                          xendh	= $9F
    88                          yend	= $93		; end coordinate y
    89                          
    90                          kl	= $95		; gradient for lines, low+high
    91                          kh	= kl+1
    92                          
    93                          tmp1	= $95		; =kl, temp. var. in context horiz. lines
    94                          tmp2	= $96		; =kh, temp. var. in context horiz. lines
    95                          
    96                          dxl	= $AB		; x delta, low+high
    97                          dxh	= $A7
    98                          
    99                          dy	= $A9		; y delta
   100                          
   101                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   102                          
   103                          cl	= $A3		; dot count, low+high
   104                          ch	= $A4
   105                          
   106                          gaddr	= $A5		; graphic address
   107                          
   108                          gpos	= $FB		; in graphic position
   109                          
   110                          gcol	= $FD		; graphic color, in "graphic on" context only
   111                          
   112                          ;
   113                          ; initialize extension
   114                          
   115                          init
   116  c000 a90b                       LDA #<(parse)	; basic interpreter parser hook
   117  c002 8d0803                     STA v_bascmd
   118  c005 a9c0                       LDA #>(parse)
   119  c007 8d0903                     STA v_bascmd+1
   120  c00a 60                         RTS
   121                          
   122                          
   123                          ;-----------------------------------------------------------------
   124                          
   125                          ; start parsing an extension command ...
   126                          
   127                          parse
   128  c00b 207300                     JSR chrget			; next char.
   129  c00e 08                 	PHP
   130  c00f c926                       CMP #'&'			; command prefix
   131  c011 f004                       BEQ newcmd
   132  c013 28                         PLP
   133  c014 4ce7a7                     JMP b_execstatement
   134                          newcmd
   135  c017 28                 	PLP
   136  c018 207300                     JSR chrget			; command character
   137                          
   138  c01b a00b                       LDY #(cmdsend-cmds)		; map character to
   139                          					; command address ...
   140                          checknextcmd
   141  c01d 88                         DEY
   142  c01e f019               	BEQ parse_exit
   143  c020 d93cc0                     CMP cmds,Y
   144  c023 d0f8                       BNE checknextcmd		; try next
   145  c025 88                         DEY				; found
   146  c026 98                         TYA
   147  c027 0a                         ASL				; *2
   148  c028 a8                         TAY
   149                          !ifndef command_rts_tyle {
   150                          	!set co=0			; command offset in jump table
   151  c029 b948c0                     LDA cmdaddr+1,Y                 ; high byte from table
   152  c02c 8556                       STA ijmp+1
   153  c02e b947c0                     LDA cmdaddr,Y                   ; low byte from table
   154  c031 8555                       STA ijmp
   155  c033 207300                     JSR chrget			; read next byte in basic text
   156  c036 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   157                          } else {
   158                          	!set co=1			; command offset in jump table
   159                          	LDA #>(b_interpreter-1)		; return to interpreter
   160                          	PHA
   161                          	LDA #<(b_interpreter-1)
   162                          	PHA				
   163                                  LDA cmdaddr+1,Y			; command address (RTS style)
   164                                  PHA				; high byte on stack
   165                                  LDA cmdaddr,Y			; command address (RTS style)
   166                                  PHA				; low byte on stack
   167                                  JMP chrget			; read next byte in basic text 
   168                          					; and RTS to command
   169                          }
   170                          parse_exit
   171  c039 4caea7                     JMP b_interpreter		; continue parsing
   172                          
   173                          ;-----------------------------------------------------------------
   174                          
   175  c03c 204743534d525456...cmds	!text " GCSMRTVHLP"		; first char. is a dummy
   176                          cmdsend
   177                          
   178                          cmdaddr
   179  c047 b8c0a8c5c6c4b9c4...        !word graphic-co,char-co,setmode-co,move-co,relto-co
   180  c051 59c676c3c3c2e1c3...        !word to-co,vline-co,hline-co,line-co,plot-co
   181                          
   182  c05b 4a4f48414e4e204b...author	!text "JOHANN KLASEK/1986,2016/V1.20"
   183                          
   184                          bitmask
   185  c078 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   186                          nbitmask
   187  c080 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   188                          ytabl
   189  c088 004080c0           	!byte $00,$40,$80,$c0
   190                          ytabh
   191  c08c e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   192  c090 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   193  c094 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   194  c098 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   195  c09c f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   196  c0a0 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   197  c0a4 fe                 	!byte gramp+$1e
   198                          
   199  c0a5 3a                 savexl	!byte $3a
   200  c0a6 01                 savexh	!byte $01
   201  c0a7 71                 savey	!byte $71
   202                          
   203                          ; for horiz. line
   204                          
   205  c0a8 ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   206                          
   207  c0b0 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   208                          
   209                          
   210                          ;-----------------------------------------------------------------
   211                          
   212                          graphic
   213  c0b8 209eb7                     JSR b_get8bit
   214  c0bb e000                       CPX #$00
   215  c0bd d013                       BNE graphic_on
   216                          gra0			; &G 0
   217  c0bf a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   218  c0c1 8d00dd                     STA cia_pra
   219  c0c4 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   220                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   221                          			; char addr $1000/4096 = char. ROM
   222  c0c6 8d18d0                     STA vic_mcr	; VIC memory control
   223  c0c9 ad11d0                     LDA vic_cr	; VIC control register
   224  c0cc 29df                       AND #%11011111	; Hires mode off
   225  c0ce 8d11d0                     STA vic_cr
   226  c0d1 60                         RTS
   227                          graphic_on
   228  c0d2 e001                       CPX #$01
   229  c0d4 d01f                       BNE gra2
   230                          gra1			; &G 1
   231  c0d6 a000                       LDY #$00
   232  c0d8 a220                       LDX #$20	; Pages (8 KByte)
   233  c0da a9e0                       LDA #>gram
   234  c0dc 85fc                       STA gpos+1
   235  c0de 84fb                       STY gpos
   236  c0e0 a900                       LDA #$00
   237                          gra_clear
   238  c0e2 91fb                       STA (gpos),Y	; Loop unroll
   239  c0e4 c8                         INY
   240  c0e5 91fb                       STA (gpos),Y
   241  c0e7 c8                         INY
   242  c0e8 91fb                       STA (gpos),Y
   243  c0ea c8                         INY
   244  c0eb 91fb                       STA (gpos),Y
   245  c0ed c8                         INY
   246  c0ee d0f2                       BNE gra_clear
   247  c0f0 e6fc                       INC gpos+1
   248  c0f2 ca                         DEX
   249  c0f3 d0ed                       BNE gra_clear
   250                          gra2
   251  c0f5 20f1b7                     JSR b_getcomma8bit
   252  c0f8 8a                         TXA		; foreground color
   253  c0f9 0a                         ASL		; upper nibble
   254  c0fa 0a                         ASL
   255  c0fb 0a                         ASL
   256  c0fc 0a                         ASL
   257  c0fd 85fd                       STA gcol
   258  c0ff 20f1b7                     JSR b_getcomma8bit
   259  c102 8a                         TXA		; background color
   260  c103 290f                       AND #$0F
   261  c105 05fd                       ORA gcol
   262  c107 a000                       LDY #$00
   263                          cram_loop
   264  c109 9900cc                     STA cram,Y
   265  c10c 9900cd                     STA cram+$100,Y
   266  c10f 9900ce                     STA cram+$200,Y
   267  c112 99e8ce                     STA cram+$300-24,Y
   268  c115 c8                         INY
   269  c116 d0f1                       BNE cram_loop
   270  c118 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   271  c11a 8d00dd                     STA cia_pra
   272  c11d a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   273  c11f 8d18d0                     STA vic_mcr	; VIC memory control
   274  c122 ad11d0                     LDA vic_cr	; VIC control register
   275  c125 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   276  c127 8d11d0                     STA vic_cr
   277  c12a 60                         RTS
   278                          
   279                          
   280                          ;-----------------------------------------------------------------
   281                          
   282                          gexit
   283  c12b a501                       LDA prozport
   284  c12d 0902                       ORA #%00000010	; kernal ROM enable
   285  c12f 8501                       STA prozport
   286  c131 58                         CLI		; allow interrupts
   287  c132 60                         RTS
   288                          
   289                          ;-----------------------------------------------------------------
   290                          
   291                          ginit
   292  c133 a501                       LDA prozport
   293  c135 29fd                       AND #%11111101	; Kernal ROM disable
   294  c137 78                         SEI		; disable interrupts
   295  c138 8501                       STA prozport
   296  c13a 60                         RTS
   297                          
   298                          ;-----------------------------------------------------------------
   299                          
   300                          gchange
   301  c13b b1a5                       LDA (gaddr),Y
   302                          gchange_op
   303  c13d 1d78c0                     ORA bitmask,X
   304  c140 91a5                       STA (gaddr),Y
   305  c142 60                         RTS
   306                          
   307                          ;-----------------------------------------------------------------
   308                          
   309                          gmask
   310                          gmask_flip
   311  c143 4900                       EOR #$00
   312                          gmask_op
   313  c145 11a5                       ORA (gaddr),Y
   314  c147 91a5                       STA (gaddr),Y
   315  c149 60                         RTS
   316                          
   317                          ;-----------------------------------------------------------------
   318                          
   319                          position
   320  c14a a5aa                       LDA y
   321  c14c 4a                         LSR
   322  c14d 4a                         LSR
   323  c14e 4a                         LSR		; y/8
   324  c14f a8                         TAY
   325  c150 2903                       AND #%00000011	; (y/8) mod 4
   326  c152 aa                         TAX
   327  c153 a59b                       LDA xl		; x low
   328  c155 29f8                       AND #%11111000	; clear bit 2-0
   329  c157 18                         CLC
   330  c158 7d88c0                     ADC ytabl,X	; addr low: y base + x part
   331  c15b 85a5                       STA gaddr
   332  c15d a59c                       LDA xh		; addr high: x part
   333  c15f 798cc0                     ADC ytabh,Y	; 	+ y base
   334  c162 85a6                       STA gaddr+1
   335  c164 a5aa                       LDA y		; vertical offset
   336  c166 2907                       AND #%00000111	; y mod 8
   337  c168 a8                         TAY
   338  c169 a59b                       LDA xl
   339  c16b 2907                       AND #%00000111	; x mod 8
   340  c16d aa                         TAX		; horizonal offset
   341  c16e 60                         RTS		; (bitmask)
   342                          
   343                          
   344                          ;-----------------------------------------------------------------
   345                          
   346                          ; line y up, x right, dx < dy (case 1)
   347                          
   348                          line_up_steep
   349  c16f 204ac1                     JSR position	; x,y
   350                          loop_yup_xright
   351  c172 203bc1                     JSR gchange	; pixel
   352                          
   353  c175 18                         CLC		; k += dx
   354  c176 a595                       LDA kl
   355  c178 65ab                       ADC dxl		; dxh is 0, because dx < dy
   356  c17a 8595                       STA kl
   357  c17c b004                       BCS ++		; k > 255
   358                          
   359  c17e c5a9                       CMP dy
   360  c180 9015                       BCC +		; k >= dy ->
   361                          
   362  c182 e5a9               ++	SBC dy		; k -= dy
   363  c184 8595                       STA kl
   364                          
   365  c186 e8                         INX		; x++
   366  c187 e008                       CPX #8
   367  c189 d00c                       BNE +
   368                          	; C=1
   369  c18b a200                       LDX #0		; x overflow, wrap around
   370  c18d a5a5                       LDA gaddr	; x+8: gaddr += 8
   371  c18f 6907                       ADC #8-1	; C already set by CPX
   372  c191 85a5                       STA gaddr
   373  c193 9002                       BCC +
   374  c195 e6a6                       INC gaddr+1
   375                          
   376  c197 88                 +	DEY		; y--
   377  c198 100f                       BPL +++
   378  c19a 38                         SEC		; y overflow
   379  c19b a5a5                       LDA gaddr
   380  c19d e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   381  c19f 85a5                       STA gaddr
   382  c1a1 a5a6                       LDA gaddr+1
   383  c1a3 e901               	SBC #1
   384  c1a5 85a6                       STA gaddr+1
   385  c1a7 a007                       LDY #7		; wrap around
   386                          
   387  c1a9 c6a3               +++	DEC cl		; until c=0
   388  c1ab d0c5                       BNE loop_yup_xright
   389  c1ad 4c2bc1                     JMP gexit
   390                          
   391                          
   392                          ;-----------------------------------------------------------------
   393                          
   394                          ; line x right, y up, dx > dy (case 2)
   395                          
   396                          line_up_flat
   397  c1b0 204ac1                     JSR position	; x,y
   398  c1b3 a5a3               	LDA cl		; counter adjustment for
   399  c1b5 f002               	BEQ +		; dec-dec-counting
   400  c1b7 e6a4               	INC ch
   401                          +
   402                          loop_xright_yup
   403  c1b9 203bc1                     JSR gchange	; pixel
   404                          
   405  c1bc 18                         CLC		; k += dy
   406  c1bd a595                       LDA kl
   407  c1bf 65a9                       ADC dy
   408  c1c1 8595                       STA kl
   409  c1c3 9002                       BCC ++
   410  c1c5 e696                       INC kh
   411                          
   412  c1c7 c5ab               ++	CMP dxl		; k > dx?
   413  c1c9 a596                       LDA kh
   414  c1cb e5a7                       SBC dxh
   415  c1cd 901a                       BCC +
   416                          
   417  c1cf 8596                       STA kh		; k -= dx
   418  c1d1 a595                       LDA kl
   419  c1d3 e5ab                       SBC dxl
   420  c1d5 8595                       STA kl
   421                          
   422  c1d7 88                         DEY		; y--
   423  c1d8 100f                       BPL +
   424  c1da 38                 	SEC		; C=1 not always true (SBC above)
   425  c1db a5a5                       LDA gaddr	; y overflow
   426  c1dd e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   427  c1df 85a5                       STA gaddr
   428  c1e1 a5a6                       LDA gaddr+1
   429  c1e3 e901               	SBC #1
   430  c1e5 85a6                       STA gaddr+1
   431  c1e7 a007               	LDY #7		; wrap around
   432                          
   433  c1e9 e8                 +	INX		; x++
   434  c1ea e008                       CPX #8		; x overflow?
   435  c1ec d00c                       BNE ++
   436                          	; C=1
   437  c1ee a200                       LDX #0		; wrap around
   438  c1f0 a5a5                       LDA gaddr	; x+8: gaddr += 8
   439  c1f2 6907                       ADC #8-1	; C already set by CPX
   440  c1f4 85a5                       STA gaddr
   441  c1f6 9002                       BCC ++
   442  c1f8 e6a6                       INC gaddr+1
   443                          ++
   444  c1fa c6a3               	DEC cl		; c--
   445  c1fc d0bb                       BNE loop_xright_yup
   446  c1fe c6a4                       DEC ch		; adjusted high which allows this
   447  c200 d0b7                       BNE loop_xright_yup
   448                          
   449  c202 4c2bc1                     JMP gexit
   450                          
   451                          
   452                          
   453                          ;-----------------------------------------------------------------
   454                          
   455                          ; line x right, y down, dx > dy (case 3)
   456                          
   457                          line_down_flat
   458  c205 204ac1                     JSR position	; x,y
   459  c208 a5a3               	LDA cl		; counter adjustment for
   460  c20a f002               	BEQ +		; dec-dec-counting
   461  c20c e6a4               	INC ch
   462                          +
   463                          loop_xright_ydown
   464  c20e 203bc1                     JSR gchange	; pixel
   465                          
   466  c211 18                         CLC		; k += dy
   467  c212 a595                       LDA kl
   468  c214 65a9                       ADC dy
   469  c216 8595                       STA kl
   470  c218 9002                       BCC ++
   471  c21a e696                       INC kh
   472                          
   473  c21c c5ab               ++	CMP dxl		; k > dx
   474  c21e a596                       LDA kh
   475  c220 e5a7                       SBC dxh		; k -= dx
   476  c222 901b                       BCC +
   477                          
   478  c224 8596                       STA kh
   479  c226 a595                       LDA kl
   480  c228 e5ab                       SBC dxl
   481  c22a 8595                       STA kl
   482                          
   483  c22c c8                         INY		; y++
   484  c22d c008                       CPY #8
   485  c22f d00e                       BNE +
   486                          	; C=1
   487  c231 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   488  c233 693f                       ADC #$40-1	; C already set by CPY
   489  c235 85a5                       STA gaddr
   490  c237 a5a6                       LDA gaddr+1
   491  c239 6901               	ADC #1
   492  c23b 85a6                       STA gaddr+1
   493  c23d a000                       LDY #0		; wrap around
   494                          
   495  c23f e8                 +	INX		; x++
   496  c240 e008                       CPX #8		; x overflow ?
   497  c242 d00c                       BNE +++
   498                          	; C=1
   499  c244 a200                       LDX #$00	; wrap around
   500  c246 a5a5                       LDA gaddr	; gaddr += 8
   501  c248 6907                       ADC #$08-1	; C always set by CPX
   502  c24a 85a5                       STA gaddr
   503  c24c 9002                       BCC +++
   504  c24e e6a6                       INC gaddr+1
   505                          +++
   506  c250 c6a3               	DEC cl		; c--
   507  c252 d0ba                       BNE loop_xright_ydown
   508  c254 c6a4                       DEC ch		; adjusted high which allows this
   509  c256 d0b6                       BNE loop_xright_ydown
   510                          
   511  c258 4c2bc1                     JMP gexit
   512                          
   513                          
   514                          ;-----------------------------------------------------------------
   515                          
   516                          ; line y down, x right, dx < dy (case 4)
   517                          
   518                          line_down_steep
   519  c25b 204ac1                     JSR position	; x,y
   520                          loop_ydown_xright
   521  c25e 203bc1                     JSR gchange	; pixel
   522                          
   523  c261 18                         CLC		; k += dx
   524  c262 a595                       LDA kl
   525  c264 65ab                       ADC dxl		; dxh is 0, because dx < dy
   526  c266 8595                       STA kl
   527  c268 b004                       BCS ++
   528  c26a c5a9                       CMP dy		; k > dy?
   529  c26c 9015                       BCC +
   530  c26e e5a9               ++	SBC dy		; k -= dy
   531  c270 8595                       STA kl
   532                          
   533  c272 e8                         INX		; x++
   534  c273 e008                       CPX #8
   535  c275 d00c                       BNE +		; x overflow?
   536  c277 a200                       LDX #0		; wrap around
   537  c279 a5a5                       LDA gaddr	; x+9: gaddr += 8
   538  c27b 6907                       ADC #8-1	; C already set by CPX
   539  c27d 85a5                       STA gaddr
   540  c27f 9002                       BCC +
   541  c281 e6a6                       INC gaddr+1
   542                          
   543  c283 c8                 +	INY		; y++
   544  c284 c008                       CPY #8		; y overflow?
   545  c286 d00e                       BNE +++
   546  c288 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   547  c28a 693f                       ADC #$40-1	; C already set by CPY
   548  c28c 85a5                       STA gaddr
   549  c28e a5a6                       LDA gaddr+1
   550  c290 6901               	ADC #1
   551  c292 85a6                       STA gaddr+1
   552  c294 a000                       LDY #0		; wrap around
   553                          
   554  c296 c6a3               +++	DEC cl		; c--
   555                          			; until c=0
   556  c298 d0c4                       BNE loop_ydown_xright
   557  c29a 4c2bc1                     JMP gexit
   558                          
   559                          
   560                          ;-----------------------------------------------------------------
   561                          
   562                          getcommaxy
   563  c29d 20fdae                     JSR b_getcomma	; check ","
   564                          getxy
   565  c2a0 208aad                     JSR b_getval	; get X coord. value
   566  c2a3 20f7b7                     JSR b_convint
   567  c2a6 c901                       CMP #>xmax
   568  c2a8 9009               	BCC gcxy_xok
   569  c2aa f003                       BEQ +		; X = $1xx
   570                          error_iq
   571                          !ifdef no_error {
   572                          	RTS
   573                          } else {
   574  c2ac 4c48b2                     JMP b_illquant
   575                          }
   576  c2af c040               +	CPY #<xmax	; check X low
   577  c2b1 b0f9                       BCS error_iq	; X to big
   578                          gcxy_xok
   579  c2b3 84fb                       STY gpos	; temporary save X coord.
   580  c2b5 85fc                       STA gpos+1
   581                          
   582  c2b7 20f1b7                     JSR b_getcomma8bit
   583                          			; get Y coord. value
   584  c2ba e0c8                       CPX #ymax
   585  c2bc b0ee                       BCS error_iq	; Y to big
   586                          
   587  c2be a4fb                       LDY gpos	; restory X coord.
   588  c2c0 a5fc                       LDA gpos+1
   589  c2c2 60                         RTS
   590                          
   591                          
   592                          ;-----------------------------------------------------------------
   593                          
   594                          hline
   595  c2c3 20a0c2                     JSR getxy	; get startpoint
   596  c2c6 86aa                       STX y
   597  c2c8 8ea7c0                     STX savey	; save as cursor, too
   598  c2cb 859c                       STA xh
   599  c2cd 849b                       STY xl
   600  c2cf 20fdae                     JSR b_getcomma	; get length
   601  c2d2 208aad                     JSR b_getval
   602  c2d5 20f7b7                     JSR b_convint
   603                          
   604  c2d8 c901                       CMP #>xmax
   605  c2da 9006                       BCC +		; X < 256
   606  c2dc d0ce                       BNE error_iq
   607  c2de c040                       CPY #<xmax
   608  c2e0 b0ca                       BCS error_iq
   609                          +
   610                          			; calculate end point
   611  c2e2 aa                         TAX		; save length high byte
   612  c2e3 98                         TYA		; length low byte
   613  c2e4 18                         CLC
   614  c2e5 659b                       ADC xl		; low xend = x+length
   615  c2e7 859e                       STA xendl
   616  c2e9 a8                 	TAY
   617  c2ea 8a                         TXA		; high
   618  c2eb 659c                       ADC xh		; high xend = x+length
   619  c2ed 859f                       STA xendh
   620  c2ef aa                 	TAX
   621                          
   622  c2f0 c901               	CMP #>xmax	; endpoint outside?
   623  c2f2 9005               	BCC +
   624  c2f4 98                 	TYA
   625  c2f5 e940               	SBC #<xmax
   626  c2f7 b0b3               	BCS error_iq
   627                          +
   628  c2f9 8ea6c0                     STX savexh
   629  c2fc 8ca5c0                     STY savexl	; also save as cursor
   630                          
   631  c2ff 2033c1                     JSR ginit	; map in graphic memory
   632                          
   633                          hline_start
   634  c302 a59e                       LDA xendl
   635  c304 c59b                       CMP xl
   636  c306 a59f                       LDA xendh
   637  c308 e59c                       SBC xh
   638  c30a b013                       BCS hl_noxswap	; xend < x ->
   639                          
   640  c30c a69e                       LDX xendl	; swap x, xend
   641  c30e a59b                       LDA xl
   642  c310 869b                       STX xl
   643  c312 859e                       STA xendl
   644                          
   645  c314 a69f                       LDX xendh
   646  c316 a49c                       LDY xh
   647  c318 849f                       STY xendh
   648  c31a 869c                       STX xh
   649  c31c 4c2ec3                     JMP hl_start	; x != xend
   650                          
   651                          hl_noxswap
   652  c31f a59e                       LDA xendl
   653  c321 c59b                       CMP xl
   654  c323 d009                       BNE hl_start
   655  c325 a59f                       LDA xendh
   656  c327 c59c                       CMP xh
   657  c329 d003                       BNE hl_start	; x = xend ->
   658  c32b 4ca4c4             	JMP plot_start	; single point
   659                          ;	JMP gexit	; no point
   660                          
   661                          hl_start
   662  c32e 204ac1                     JSR position	; graphic position x,y
   663  c331 bda8c0                     LDA maskleft,X
   664  c334 48                         PHA		; save left end mask
   665  c335 a59e                       LDA xendl
   666  c337 2907                       AND #%00000111
   667  c339 8596                       STA tmp2	; xend mod 8, mask index
   668  c33b a59b                       LDA xl
   669  c33d 29f8                       AND #%11111000	; (xl div 8) * 8
   670  c33f 8595                       STA tmp1
   671  c341 a59e                       LDA xendl	; (xend div 8) * 8
   672  c343 29f8                       AND #%11111000
   673  c345 38                         SEC
   674  c346 e595                       SBC tmp1
   675  c348 8595                       STA tmp1
   676  c34a a59f                       LDA xendh
   677  c34c e59c                       SBC xh		; (xend div 8 - x div 8 ) *8
   678  c34e 4a                         LSR		; high -> carry
   679  c34f a595                       LDA tmp1
   680  c351 6a                         ROR		; / 8 ->  0-39
   681  c352 6a                         ROR
   682  c353 6a                         ROR
   683  c354 aa                         TAX		; 8-pixel-blocks count
   684  c355 68                         PLA		; left end x mask
   685                          
   686                          hl_nextblock
   687  c356 ca                         DEX
   688                          hl_islastblock
   689  c357 3012                       BMI hl_lastblock
   690                          			; leave loop if X<0
   691  c359 2043c1                     JSR gmask	; first with left end mask
   692  c35c 18                         CLC		; gaddr += 8
   693  c35d a5a5                       LDA gaddr
   694  c35f 6908                       ADC #8
   695  c361 85a5                       STA gaddr
   696  c363 9002                       BCC +
   697  c365 e6a6                       INC gaddr+1
   698  c367 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   699  c369 d0eb               	BNE hl_nextblock	; always
   700                          
   701                          hl_lastblock
   702  c36b a696                       LDX tmp2	; xend mask index
   703  c36d 3db0c0                     AND maskright,X ; mask right end
   704  c370 2043c1                     JSR gmask	; modify
   705  c373 4c2bc1                     JMP gexit	; leave
   706                          
   707                          
   708                          ;-----------------------------------------------------------------
   709                          
   710                          vline
   711  c376 20a0c2                     JSR getxy	; get startpoint
   712  c379 859c                       STA xh
   713  c37b 8da6c0                     STA savexh	; save as cursor too
   714  c37e 849b                       STY xl
   715  c380 8ca5c0                     STY savexl
   716  c383 86aa                       STX y
   717                          
   718  c385 20f1b7                     JSR b_getcomma8bit
   719                          			; get length
   720  c388 18                         CLC		; calculate end point
   721  c389 8a                         TXA		; length
   722                          ; DON'T-CHANGE: how long to go vertically (needed later)
   723                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   724                          ;	STA tmp1
   725  c38a 65aa                       ADC y		; length + y
   726  c38c c9c8                       CMP #ymax
   727  c38e 9003                       BCC +
   728                          vline_iq
   729                          !ifdef no_error {
   730                          	RTS
   731                          } else {
   732  c390 4c48b2                     JMP b_illquant
   733                          }
   734  c393 8593               +	STA yend	; endpoint
   735  c395 c9c8               	CMP #ymax	; outside?
   736  c397 b0f7               	BCS vline_iq
   737                          
   738  c399 8da7c0             	STA savey	; set cursor y position
   739                          
   740  c39c 2033c1                     JSR ginit	; map in graphic memory
   741                          
   742                          vline_start
   743  c39f a593                       LDA yend
   744  c3a1 c5aa                       CMP y
   745  c3a3 b00a                       BCS vl_noyswap	; yend < y ->
   746  c3a5 a5aa                       LDA y		; swap y, yend
   747  c3a7 a693                       LDX yend
   748  c3a9 8593                       STA yend
   749  c3ab 86aa                       STX y
   750  c3ad f005               	BEQ vl_start	; always (with next branch)
   751                          	; fall through if yend is
   752                          vl_noyswap
   753  c3af d003                       BNE vl_start	; y = yend ->
   754  c3b1 4ca4c4             	JMP plot_start	; single point
   755                          ;	JMP gexit	; no point
   756                          
   757                          vl_start
   758  c3b4 204ac1                     JSR position	; graphic position x,y
   759  c3b7 bd78c0                     LDA bitmask,X
   760  c3ba 8596                       STA tmp2	; save mask
   761                          ; DON'T-CHANGE: replace ...
   762  c3bc 38                         SEC
   763  c3bd a593                       LDA yend
   764  c3bf e5aa                       SBC y		; vertical length
   765  c3c1 aa                         TAX
   766                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   767                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   768                          ;	LDX tmp1
   769  c3c2 e8                         INX		; +1 (exit on 0)
   770                          vl_nextline
   771  c3c3 a596                       LDA tmp2
   772  c3c5 2043c1                     JSR gmask	; modify 
   773  c3c8 c8                         INY		; go down
   774  c3c9 c008                       CPY #8		; 8-line wrap
   775  c3cb d00e                       BNE +
   776  c3cd a5a5                       LDA gaddr	; gaddr += 320
   777  c3cf 693f               	ADC #$40-1	; compensate for C = 1
   778  c3d1 85a5                       STA gaddr
   779  c3d3 a5a6                       LDA gaddr+1
   780  c3d5 6901                       ADC #$01
   781  c3d7 85a6                       STA gaddr+1
   782  c3d9 a000                       LDY #0		; wrap y offset
   783  c3db ca                 +	DEX		; all vertical positions done?
   784  c3dc d0e5                       BNE vl_nextline
   785  c3de 4c2bc1                     JMP gexit	; leave
   786                          
   787                          
   788                          ;-----------------------------------------------------------------
   789                          
   790                          line
   791  c3e1 20a0c2                     JSR getxy	; get startpoint
   792  c3e4 849b                       STY xl 
   793  c3e6 859c                       STA xh
   794  c3e8 86aa                       STX y
   795                          
   796  c3ea 209dc2                     JSR getcommaxy	; get endpoint
   797                          line_start
   798  c3ed 8ca5c0                     STY savexl	; save as cursor position too
   799  c3f0 849e                       STY xendl
   800  c3f2 8da6c0                     STA savexh
   801  c3f5 859f                       STA xendh
   802  c3f7 8ea7c0                     STX savey
   803  c3fa 8693                       STX yend
   804                          
   805  c3fc 2033c1                     JSR ginit	; map in graphic memory
   806                          
   807  c3ff a000                       LDY #$00	; initialize to 0
   808  c401 84a8                       STY ydir
   809  c403 8495                       STY kl
   810  c405 8496                       STY kh
   811                          
   812  c407 38                         SEC
   813  c408 a59e                       LDA xendl	; calculate dx
   814  c40a e59b                       SBC xl
   815  c40c 85ab                       STA dxl
   816  c40e a59f                       LDA xendh
   817  c410 e59c                       SBC xh
   818  c412 85a7                       STA dxh
   819                          
   820  c414 b025                       BCS li_xend_right
   821                          	; dx != 0
   822  c416 98                         TYA		; negate dx
   823  c417 38                         SEC		; dx = 0 - dx
   824  c418 e5ab                       SBC dxl
   825  c41a 85ab                       STA dxl
   826  c41c 98                         TYA
   827  c41d e5a7                       SBC dxh
   828  c41f 85a7                       STA dxh
   829                          			; C=0 always, needed later
   830  c421 a69b                       LDX xl		; swap x low
   831  c423 a49e                       LDY xendl
   832  c425 869e                       STX xendl
   833  c427 849b                       STY xl
   834                          
   835  c429 a69c                       LDX xh		; swap x high
   836  c42b a49f                       LDY xendh
   837  c42d 869f                       STX xendh
   838  c42f 849c                       STY xh
   839                          
   840  c431 a6aa                       LDX y		; swap y
   841  c433 a493                       LDY yend
   842  c435 8693                       STX yend
   843  c437 84aa                       STY y
   844                          
   845  c439 9009                       BCC li_x_different
   846                          			; C=0 always (from negation before)
   847                          
   848                          li_xend_right
   849  c43b a5ab                       LDA dxl		; dx = 0?
   850  c43d 05a7                       ORA dxh
   851  c43f d003                       BNE li_x_different
   852  c441 4c9fc3                     JMP vline_start	; vertical line case
   853                          
   854                          li_x_different
   855  c444 38                         SEC		; calculate dy
   856  c445 a593                       LDA yend
   857  c447 e5aa                       SBC y
   858  c449 b006                       BCS li_y_right
   859  c44b 49ff                       EOR #$FF	; negate dy (two's complement)
   860  c44d 6901                       ADC #$01	; C=0
   861  c44f 85a8                       STA ydir	; flag y goes up
   862                          
   863                          li_y_right
   864  c451 85a9                       STA dy
   865  c453 d003                       BNE +
   866  c455 4c02c3                     JMP hline_start	; horizontal line case
   867                          +
   868                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   869                          
   870  c458 a5a7                       LDA dxh		; dx > dy
   871  c45a d017                       BNE line_flat	; yes -> flat
   872  c45c a5a9                       LDA dy		; no -> steep
   873  c45e aa                         TAX
   874  c45f c5ab                       CMP dxl
   875  c461 9010                       BCC line_flat
   876                          
   877                          line_steep
   878  c463 e8                         INX	
   879  c464 86a3                       STX cl		; c = dy+1
   880  c466 4a                         LSR		; k = dy/2
   881  c467 8595                       STA kl
   882  c469 a5a8                       LDA ydir
   883  c46b d003                       BNE +
   884  c46d 4c5bc2                     JMP line_down_steep	; y down, steep
   885  c470 4c6fc1             +	JMP line_up_steep	; y up, steep
   886                          
   887                          line_flat
   888  c473 a5a7                       LDA dxh
   889  c475 a8                         TAY
   890  c476 a6ab                       LDX dxl
   891  c478 e8                         INX
   892  c479 d001                       BNE +
   893  c47b c8                         INY
   894  c47c 86a3               +	STX cl		; c = dx+1
   895  c47e 84a4                       STY ch
   896                          
   897  c480 4a                         LSR		; k = dx/2
   898  c481 8596                       STA kh
   899  c483 a5ab                       LDA dxl
   900  c485 6a                         ROR		; dx/2
   901  c486 8595                       STA kl
   902  c488 a5a8                       LDA ydir	
   903  c48a d003                       BNE +
   904  c48c 4c05c2                     JMP line_down_flat	; y down, flat
   905  c48f 4cb0c1             +	JMP line_up_flat	; y up, flat
   906                          
   907                          ;-----------------------------------------------------------------
   908                          
   909                          plot
   910  c492 20a0c2                     JSR getxy	; get parameter
   911  c495 859c                       STA xh		; save x/y
   912  c497 849b                       STY xl
   913  c499 86aa                       STX y
   914  c49b 8da6c0                     STA savexh	; and store as cursor
   915  c49e 8ca5c0                     STY savexl
   916  c4a1 8ea7c0                     STX savey
   917                          
   918                          plot_start
   919  c4a4 204ac1                     JSR position	; calculate graphical address
   920                          
   921  c4a7 a501                       LDA prozport
   922  c4a9 29fd                       AND #%11111101	; Kernal ROM disable
   923  c4ab 78                         SEI			
   924  c4ac 8501                       STA prozport
   925                          
   926  c4ae 203bc1                     JSR gchange	; change graphical data
   927                          
   928  c4b1 a501                       LDA prozport
   929  c4b3 0902                       ORA #%00000010	; kernal ROM enable
   930  c4b5 8501                       STA prozport
   931  c4b7 58                         CLI
   932  c4b8 60                         RTS
   933                          
   934                          ;-----------------------------------------------------------------
   935                          
   936                          move
   937  c4b9 20a0c2                     JSR getxy	; get parameter
   938  c4bc 8da6c0                     STA savexh	; just save as cursor
   939  c4bf 8ca5c0                     STY savexl
   940  c4c2 8ea7c0                     STX savey
   941  c4c5 60                         RTS
   942                          
   943                          
   944                          ;-----------------------------------------------------------------
   945                          
   946                          setmode
   947  c4c6 209eb7                     JSR b_get8bit
   948  c4c9 e003                       CPX #$03
   949  c4cb 9003                       BCC +
   950  c4cd 4c48b2                     JMP b_illquant
   951  c4d0 e001               +	CPX #$01
   952  c4d2 b01a                       BCS set_or_toggle
   953                          
   954                          modereset
   955  c4d4 a9c0                       LDA #>(nbitmask)
   956  c4d6 8d3fc1                     STA gchange_op+2
   957  c4d9 a980                       LDA #<(nbitmask)
   958  c4db 8d3ec1                     STA gchange_op+1
   959  c4de a93d                       LDA #$3D		; AND abs,X
   960  c4e0 8d3dc1                     STA gchange_op
   961  c4e3 a931                       LDA #$31		; AND (zp),Y
   962  c4e5 8d45c1                     STA gmask_op
   963  c4e8 a9ff                       LDA #$FF		; EOR $#FF, invertieren
   964  c4ea 8d44c1                     STA gmask_flip+1
   965  c4ed 60                         RTS
   966                          
   967                          set_or_toggle
   968  c4ee d01a                       BNE modetoggle
   969                          modeset
   970  c4f0 a9c0                       LDA #>(bitmask)
   971  c4f2 8d3fc1                     STA gchange_op+2
   972  c4f5 a978                       LDA #<(bitmask)
   973  c4f7 8d3ec1                     STA gchange_op+1
   974  c4fa a91d                       LDA #$1D		; OR abs,X
   975  c4fc 8d3dc1                     STA gchange_op
   976  c4ff a911                       LDA #$11		; OR (zp),Y
   977  c501 8d45c1                     STA gmask_op
   978  c504 a900                       LDA #$00		; EOR #$00, nicht invertieren
   979  c506 8d44c1                     STA gmask_flip+1
   980  c509 60                         RTS
   981                          
   982                          modetoggle
   983  c50a a9c0                       LDA #>(bitmask)
   984  c50c 8d3fc1                     STA gchange_op+2
   985  c50f a978                       LDA #<(bitmask)
   986  c511 8d3ec1                     STA gchange_op+1
   987  c514 a95d                       LDA #$5D		; EOR abs,X
   988  c516 8d3dc1                     STA gchange_op
   989  c519 a951                       LDA #$51		; EOR (zp),Y
   990  c51b 8d45c1                     STA gmask_op
   991  c51e a900                       LDA #$00		; EOR #$00, nicht invertieren
   992  c520 8d44c1                     STA gmask_flip+1
   993  c523 60                         RTS
   994                          
   995                          
   996                          ;-----------------------------------------------------------------
   997                          
   998                          ; get pixel (check if pixel set)
   999                          ; not used
  1000                          
  1001                          get
  1002  c524 209dc2                     JSR getcommaxy
  1003  c527 859c                       STA xh
  1004  c529 849b                       STY xl
  1005  c52b 86aa                       STX y
  1006                          
  1007  c52d 204ac1                     JSR position
  1008                          
  1009  c530 a501                       LDA prozport
  1010  c532 29fd               	AND #%11111101	; Kernal ROM disable
  1011  c534 78                         SEI
  1012  c535 8501                       STA prozport
  1013                          
  1014  c537 b1a5                       LDA (gaddr),Y
  1015  c539 3d78c0                     AND bitmask,X
  1016  c53c a8                         TAY
  1017  c53d a501                       LDA prozport
  1018  c53f 0902               	ORA #%00000010	; kernal ROM enable
  1019  c541 8501                       STA prozport
  1020  c543 58                         CLI
  1021  c544 4ca2b3                     JMP b_byte2fac
  1022                          
  1023                          
  1024                          ;-----------------------------------------------------------------
  1025                          
  1026                          relto
  1027  c547 208aad                     JSR b_getval	; get X offset (+/-)
  1028  c54a a561               	LDA facexp	; FAC exponent
  1029  c54c c990               	CMP #$90	; more than 16 bit
  1030  c54e b031               	BCS relto_error	; illegal quantity
  1031  c550 209bbc                     JSR b_fac2int	; to signed integer
  1032                          
  1033  c553 18                         CLC
  1034  c554 a565                       LDA facintl
  1035  c556 6da5c0                     ADC savexl
  1036  c559 859e                       STA xendl
  1037  c55b a564                       LDA facinth
  1038  c55d 6da6c0                     ADC savexh
  1039  c560 859f                       STA xendh	; xend = savex+facint
  1040                          
  1041  c562 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1042  c565 208aad                     JSR b_getval
  1043  c568 a561                       LDA facexp	; FAC exponent
  1044  c56a c990                       CMP #$90	; more than 16 bit
  1045  c56c b013                       BCS relto_error	; illegal quantity
  1046  c56e 209bbc                     JSR b_fac2int	; to signed integer
  1047  c571 18                         CLC
  1048  c572 a565                       LDA facintl
  1049  c574 6da7c0                     ADC savey
  1050  c577 8593                       STA yend	; yend = savey+facint
  1051                          
  1052  c579 a59f                       LDA xendh	; check end coord. x
  1053  c57b c901                       CMP #>xmax
  1054  c57d 900b                       BCC rt_xok
  1055  c57f f003                       BEQ +
  1056                          relto_error
  1057                          !ifdef no_error {
  1058                          	RTS
  1059                          } else {
  1060  c581 4c48b2                     JMP b_illquant
  1061                          }
  1062  c584 a59e               +	LDA xendl
  1063  c586 c940                       CMP #<xmax
  1064  c588 b0f7                       BCS relto_error
  1065                          rt_xok
  1066  c58a a593                       LDA yend	; check end coord. y
  1067  c58c c9c8                       CMP #ymax
  1068  c58e b0f1                       BCS relto_error
  1069                          
  1070  c590 ada5c0                     LDA savexl
  1071  c593 859b                       STA xl
  1072  c595 ada6c0                     LDA savexh
  1073  c598 859c                       STA xh
  1074  c59a ada7c0                     LDA savey
  1075  c59d 85aa                       STA y
  1076  c59f a49e                       LDY xendl
  1077  c5a1 a59f                       LDA xendh
  1078  c5a3 a693                       LDX yend	; xend/yend = cursor + x/y
  1079                          
  1080  c5a5 4cedc3                     JMP line_start	; draw line x/y to xend/yend
  1081                          
  1082                          
  1083                          ;-----------------------------------------------------------------
  1084                          
  1085                          char
  1086  c5a8 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1087  c5ab e028                       CPX #40	
  1088  c5ad 9003                       BCC +
  1089                          char_error
  1090  c5af 4c48b2                     JMP b_illquant
  1091  c5b2 86fb               +	STX gpos	; save x coord.
  1092  c5b4 20f1b7                     JSR b_getcomma8bit
  1093                          			; get char. position y 0-24
  1094  c5b7 e019                       CPX #25
  1095  c5b9 b0f4                       BCS char_error
  1096  c5bb 86fc                       STX gpos+1	; save y coord.
  1097                          
  1098  c5bd 20fdae                     JSR b_getcomma	; get string
  1099  c5c0 209ead                     JSR b_getexpr
  1100  c5c3 20a3b6                     JSR b_stringval ; string address in str
  1101  c5c6 48                         PHA		; string length
  1102  c5c7 a6fc                       LDX gpos+1	; y coord. for char. position
  1103  c5c9 8a                         TXA
  1104  c5ca 2903                       AND #$03	; mask 2 bits
  1105  c5cc a8                         TAY		; table index
  1106  c5cd a900                       LDA #$00
  1107  c5cf 85fc                       STA gpos+1	; x high
  1108  c5d1 a5fb                       LDA gpos	; saved x: multiply by 8
  1109  c5d3 0a                         ASL
  1110  c5d4 0a                         ASL
  1111  c5d5 0a                         ASL
  1112  c5d6 26fc                       ROL gpos+1	; overflow to high byte
  1113  c5d8 7988c0                     ADC ytabl,Y
  1114  c5db 85a5                       STA gaddr
  1115  c5dd a5fc                       LDA gpos+1	; x high
  1116  c5df 7d8cc0                     ADC ytabh,X
  1117  c5e2 85a6                       STA gaddr+1
  1118  c5e4 68                         PLA		; string length
  1119  c5e5 a000                       LDY #$00	; string index
  1120  c5e7 aa                         TAX		; length
  1121  c5e8 e8                         INX		; prepare as counter
  1122                          char_loop
  1123  c5e9 ca                         DEX
  1124  c5ea f008                       BEQ char_exit
  1125  c5ec b122                       LDA (str),Y	; read string
  1126  c5ee 20f5c5                     JSR char_display
  1127  c5f1 c8                         INY
  1128  c5f2 d0f5                       BNE char_loop
  1129                          char_exit
  1130  c5f4 60                         RTS
  1131                          
  1132                          char_display
  1133  c5f5 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1134  c5f7 8a                         TXA		; save register X+Y
  1135  c5f8 48                         PHA
  1136  c5f9 98                         TYA
  1137  c5fa 48                         PHA
  1138  c5fb a5d7                       LDA z_tmp	; get saved character
  1139  c5fd 1049                       BPL char_normal
  1140                          
  1141                          char_inverse
  1142  c5ff 297f                       AND #%01111111	; mask bit 7
  1143  c601 c97f                       CMP #%01111111	; was 255? (pi)
  1144  c603 d002                       BNE +
  1145  c605 a95e                       LDA #$5E	; screen code for pi
  1146  c607 c920               +	CMP #$20	; control character?
  1147  c609 9038                       BCC char_disp_leave
  1148                          			; yes, skip
  1149  c60b 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1150                          			; $C0-$FF -> $40-$7F
  1151                          			; OPT: BNE char_hires
  1152                          			; OPT: char_normal
  1153                          char_hires
  1154  c60d a6c7                       LDX z_reverseflag
  1155  c60f f002                       BEQ +
  1156  c611 0980                       ORA #%10000000	; invert char.
  1157  c613 48                 +	PHA		; save char. for later
  1158  c614 78                         SEI
  1159  c615 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1160  c617 8501                       STA prozport	; char. rom base = $D000
  1161  c619 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1162  c61b 85fc                       STA gpos+1	; 
  1163  c61d 68                         PLA		; char. code
  1164  c61e 0a                         ASL		; *8
  1165  c61f 26fc                       ROL gpos+1
  1166  c621 0a                         ASL
  1167  c622 26fc                       ROL gpos+1
  1168  c624 0a                         ASL
  1169  c625 26fc                       ROL gpos+1
  1170  c627 85fb                       STA gpos	; addr. in char. rom for char.
  1171                          
  1172  c629 a007                       LDY #$07	; 8 hires lines
  1173                          char_line
  1174  c62b b1fb                       LDA (gpos),Y	; read character line
  1175  c62d 2043c1                     JSR gmask	; write to hires screen
  1176  c630 88                         DEY
  1177  c631 10f8                       BPL char_line
  1178                          
  1179  c633 a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1180  c635 8501                       STA prozport
  1181  c637 58                         CLI
  1182                          
  1183  c638 18                         CLC		; step char position to left
  1184  c639 a5a5                       LDA gaddr	; ( +8 )
  1185  c63b 6908                       ADC #$08
  1186  c63d 85a5                       STA gaddr
  1187  c63f 9002                       BCC +
  1188  c641 e6a6                       INC gaddr+1
  1189                          +
  1190                          char_disp_leave
  1191  c643 68                 	PLA		; pass written character back
  1192  c644 a8                         TAY		; restore saved registers
  1193  c645 68                         PLA
  1194  c646 aa                         TAX
  1195  c647 60                         RTS
  1196                          
  1197                          char_normal
  1198  c648 c920                       CMP #$20	; control character?
  1199  c64a 90f7                       BCC char_disp_leave
  1200  c64c c960                       CMP #$60
  1201  c64e 9004                       BCC +
  1202  c650 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1203  c652 d002                       BNE ++
  1204  c654 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1205  c656 4c0dc6             ++	JMP char_hires	; 		OPT: Bxx
  1206                          
  1207                          
  1208                          ;-----------------------------------------------------------------
  1209                          
  1210                          to
  1211  c659 ada5c0                     LDA savexl
  1212  c65c 859b                       STA xl
  1213  c65e ada6c0                     LDA savexh
  1214  c661 859c                       STA xh
  1215  c663 ada7c0                     LDA savey
  1216  c666 85aa                       STA y
  1217  c668 20a0c2                     JSR getxy
  1218  c66b 4cedc3                     JMP line_start
  1219                          
  1220  c66e 000000000000       	!by 0,0,0,0,0,0
