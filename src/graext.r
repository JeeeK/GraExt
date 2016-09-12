
; ******** Source: graext.asm
     1                          !to "graext.o",cbm	
     2                          
     3                          ;  **** gra-ext module ****
     4                          ;
     5                          ; 2016-05-18 johann e. klasek, johann at klasek at
     6                          ;
     7                          ; revisions:
     8                          ;	2016-06-16 v 1.24
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
    21  c003 a97d                       lda #<(parse)		; check if basic interpreter parser hook
    22  c005 cd0803                     cmp v_bascmd		; does already exist
    23  c008 d008               	bne start
    24  c00a a9c0                       lda #>(parse)
    25  c00c cd0903                     cmp v_bascmd+1
    26  c00f d001                       bne start
    27  c011 60                 	rts			; hook already in place, no start message
    28                          
    29                          start
    30  c012 2025c0                     jsr init                ; init extension (place hook)
    31  c015 a63a               	ldx $3a			; direct mode flag
    32  c017 e8                 	inx
    33  c018 f001               	beq interactive
    34  c01a 60                 	rts			; simply return in running program
    35                          interactive
    36  c01b a9d3                       lda #<author            ; message ...
    37  c01d a0c0                       ldy #>author
    38  c01f 201eab                     jsr $ab1e               ; output string 
    39                          
    40  c022 4c86e3                     jmp $e386               ; BASIC warm start
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
   141  c025 a97d                       LDA #<(parse)	; basic interpreter parser hook
   142  c027 8d0803                     STA v_bascmd
   143  c02a a9c0                       LDA #>(parse)
   144  c02c 8d0903                     STA v_bascmd+1
   145                          
   146  c02f ad2803                     LDA v_basstp
   147  c032 8d3a03             	STA savevstp
   148  c035 a971                       LDA #<(stop)	; basic interpreter stop hook
   149  c037 8d2803                     STA v_basstp
   150  c03a ad2903                     LDA v_basstp+1
   151  c03d 8d3b03             	STA savevstp+1
   152  c040 a9c0                       LDA #>(stop)
   153  c042 8d2903                     STA v_basstp+1
   154                          
   155  c045 ad0003                     LDA v_baserr
   156  c048 8d3803             	STA saveverr
   157  c04b a96b                       LDA #<(error)	; basic interpreter error hook
   158  c04d 8d0003                     STA v_baserr
   159  c050 ad0103                     LDA v_baserr+1
   160  c053 8d3903             	STA saveverr+1
   161  c056 a9c0                       LDA #>(error)
   162  c058 8d0103                     STA v_baserr+1
   163                          
   164  c05b a200               	LDX #0		; set graphic cursor to (0,0)
   165  c05d 8e3403             	STX savexl
   166  c060 8e3503             	STX savexh
   167  c063 8e3603             	STX savey
   168  c066 e8                 	INX
   169  c067 8e3703             	STX savemo	; set mode 1
   170  c06a 60                         RTS
   171                          
   172                          error	
   173                          	; reg A may destroyed
   174  c06b 2041c1             	JSR gra_off		; uses only reg A
   175  c06e 6c3803             	JMP (saveverr)		; to original vector
   176                          
   177                          stop	
   178                          	; reg A may destroyed
   179  c071 a591               	LDA $91			; Scan code
   180  c073 c97f               	CMP #$7F		; STOP key?
   181  c075 d003               	BNE nostop
   182  c077 2041c1             	JSR gra_off		; uses only reg A
   183                          nostop
   184  c07a 6c3a03             	JMP (savevstp)		; to original vector
   185                          
   186                          ;-----------------------------------------------------------------
   187                          
   188                          ; start parsing an extension command ...
   189                          
   190                          parse
   191  c07d 207300                     JSR chrget			; next char.
   192  c080 08                 	PHP
   193  c081 c926                       CMP #'&'			; command prefix
   194  c083 f004                       BEQ newcmd
   195  c085 28                         PLP
   196  c086 4ce7a7                     JMP b_execstatement
   197                          newcmd
   198  c089 28                 	PLP
   199  c08a 207300                     JSR chrget			; command character
   200                          
   201  c08d a00c                       LDY #(cmdsend-cmds)		; map character to
   202                          					; command address ...
   203                          checknextcmd
   204  c08f 88                         DEY
   205  c090 f01c               	BEQ parse_error
   206  c092 d9b1c0                     CMP cmds,Y
   207  c095 d0f8                       BNE checknextcmd		; try next
   208  c097 88                         DEY				; found
   209  c098 98                         TYA
   210  c099 0a                         ASL				; *2
   211  c09a a8                         TAY
   212                          !ifndef command_rts_tyle {
   213                          	!set co=0			; command offset in jump table
   214  c09b b9bec0                     LDA cmdaddr+1,Y                 ; high byte from table
   215  c09e 8556                       STA ijmp+1
   216  c0a0 b9bdc0                     LDA cmdaddr,Y                   ; low byte from table
   217  c0a3 8555                       STA ijmp
   218  c0a5 207300                     JSR chrget			; read next byte in basic text
   219  c0a8 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   220  c0ab 4caea7                     JMP b_interpreter		; continue parsing
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
   235  c0ae 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   236                          
   237                          ;-----------------------------------------------------------------
   238                          
   239                          ; the most commonly used command placed at the end ...
   240                          
   241  c0b1 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   242                          cmdsend
   243                          
   244                          cmdaddr
   245  c0bd 48c73ac17fc682c5...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   246  c0c9 33c71fc46ec38ac4...        !word to-co,vline-co,hline-co,line-co,plot-co
   247                          
   248  c0d3 934752412d455854...author	!text 147,"GRA-EXT V1.24 1986,2016 JOHANN@KLASEK.AT",0
   249                          
   250                          bitmask
   251  c0fd 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   252                          nbitmask
   253  c105 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   254                          ytabl
   255  c10d 004080c0           	!byte $00,$40,$80,$c0
   256                          ytabh
   257  c111 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   258  c115 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   259  c119 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   260  c11d eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   261  c121 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   262  c125 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   263  c129 fe                 	!byte gramp+$1e
   264                          
   265                          ; for horiz. line
   266                          
   267  c12a ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   268                          
   269  c132 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   270                          
   271                          
   272                          ;-----------------------------------------------------------------
   273                          
   274                          graphic
   275  c13a 209eb7                     JSR b_get8bit
   276  c13d e000                       CPX #$00
   277  c13f d013                       BNE gra_other
   278                          gra0			; &G 0
   279                          gra_off
   280  c141 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   281  c143 8d00dd                     STA cia_pra
   282  c146 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   283                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   284                          			; char addr $1000/4096 = char. ROM
   285  c148 8d18d0                     STA vic_mcr	; VIC memory control
   286  c14b ad11d0                     LDA vic_cr	; VIC control register
   287  c14e 29df                       AND #%11011111	; Hires mode off
   288  c150 8d11d0                     STA vic_cr
   289  c153 60                         RTS
   290                          
   291                          gra_other
   292  c154 e001                       CPX #$01
   293  c156 f00f               	BEQ gra1
   294  c158 e002               	CPX #$02
   295  c15a f00e                       BEQ gra2
   296  c15c e004               	CPX #$04
   297  c15e f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   298  c160 e003               	CPX #$03	; &G 3 (graphic on)
   299  c162 f029               	BEQ gra_on
   300  c164 4c48b2                     JMP b_illquant	; parameter illegal
   301                          	
   302                          gra1			; &G 1
   303  c167 20a3c1             	JSR gra_clear
   304                          
   305                          gra2
   306  c16a 20f1b7                     JSR b_getcomma8bit
   307  c16d 8a                         TXA		; foreground color
   308  c16e 0a                         ASL		; upper nibble
   309  c16f 0a                         ASL
   310  c170 0a                         ASL
   311  c171 0a                         ASL
   312  c172 85fd                       STA gcol
   313  c174 20f1b7                     JSR b_getcomma8bit
   314  c177 8a                         TXA		; background color
   315  c178 290f                       AND #$0F
   316  c17a 05fd                       ORA gcol
   317  c17c a000                       LDY #$00
   318                          cram_loop
   319  c17e 9900cc                     STA cram,Y	; fill color RAM
   320  c181 9900cd                     STA cram+$100,Y
   321  c184 9900ce                     STA cram+$200,Y
   322  c187 99e8ce                     STA cram+$300-24,Y
   323  c18a c8                         INY
   324  c18b d0f1                       BNE cram_loop
   325                          
   326                          gra_on
   327  c18d 20c2c1             	JSR gra_setupcode
   328                          
   329  c190 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   330  c192 8d00dd                     STA cia_pra
   331  c195 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   332  c197 8d18d0                     STA vic_mcr	; VIC memory control
   333  c19a ad11d0                     LDA vic_cr	; VIC control register
   334  c19d 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   335  c19f 8d11d0                     STA vic_cr
   336  c1a2 60                         RTS
   337                          
   338                          gra_clear
   339  c1a3 a220                       LDX #$20	; Pages (8 KByte)
   340  c1a5 a9e0                       LDA #>gram
   341  c1a7 85fc                       STA gpos+1
   342  c1a9 a000                       LDY #$00
   343  c1ab 84fb                       STY gpos
   344  c1ad 98                         TYA
   345                          gra_fill
   346  c1ae 91fb                       STA (gpos),Y	; Loop unroll
   347  c1b0 c8                         INY
   348  c1b1 91fb                       STA (gpos),Y
   349  c1b3 c8                         INY
   350  c1b4 91fb                       STA (gpos),Y
   351  c1b6 c8                         INY
   352  c1b7 91fb                       STA (gpos),Y
   353  c1b9 c8                         INY
   354  c1ba d0f2                       BNE gra_fill
   355  c1bc e6fc                       INC gpos+1
   356  c1be ca                         DEX
   357  c1bf d0ed                       BNE gra_fill
   358  c1c1 60                 	RTS
   359                          
   360                          gra_setupcode
   361  c1c2 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   362                          gra_copycode
   363  c1c4 bde5c1             	LDA gromcode-1,X
   364  c1c7 9dec03             	STA gramcode-1,X
   365  c1ca ca                 	DEX
   366  c1cb d0f7               	BNE gra_copycode
   367  c1cd ad3703             	LDA savemo
   368  c1d0 290f               	AND #$0F
   369  c1d2 aa                 	TAX
   370  c1d3 4ca7c5             	JMP setmode_enter	; re-apply mode to routines
   371                          				; implicit RTS
   372                          
   373                          ;-----------------------------------------------------------------
   374                          
   375                          gexit
   376  c1d6 a501                       LDA prozport
   377  c1d8 0902                       ORA #%00000010	; kernal ROM enable
   378  c1da 8501                       STA prozport
   379  c1dc 58                         CLI		; allow interrupts
   380  c1dd 60                         RTS
   381                          
   382                          ;-----------------------------------------------------------------
   383                          
   384                          ginit
   385  c1de a501                       LDA prozport
   386  c1e0 29fd                       AND #%11111101	; Kernal ROM disable
   387  c1e2 78                         SEI		; disable interrupts
   388  c1e3 8501                       STA prozport
   389  c1e5 60                         RTS
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
   404  c1e6 b1a5                       LDA (gaddr),Y
   405                          gchange_op
   406  c1e8 1dfdc0                     ORA bitmask,X
   407  c1eb 91a5                       STA (gaddr),Y
   408  c1ed 60                         RTS
   409                          
   410                          ; mask a graphic location 
   411                          
   412                          gmask
   413                          gmask_flip
   414  c1ee 4900                       EOR #$00
   415                          gmask_op
   416  c1f0 11a5                       ORA (gaddr),Y
   417  c1f2 91a5                       STA (gaddr),Y
   418  c1f4 60                         RTS
   419                          
   420                          }
   421                          
   422                          gromcode_end
   423                          
   424                          ;-----------------------------------------------------------------
   425                          
   426                          position
   427  c1f5 a5aa                       LDA y
   428  c1f7 4a                         LSR
   429  c1f8 4a                         LSR
   430  c1f9 4a                         LSR		; y/8
   431  c1fa a8                         TAY
   432  c1fb 2903                       AND #%00000011	; (y/8) mod 4
   433  c1fd aa                         TAX
   434  c1fe a59b                       LDA xl		; x low
   435  c200 29f8                       AND #%11111000	; clear bit 2-0
   436  c202 18                         CLC
   437  c203 7d0dc1                     ADC ytabl,X	; addr low: y base + x part
   438  c206 85a5                       STA gaddr
   439  c208 a59c                       LDA xh		; addr high: x part
   440  c20a 7911c1                     ADC ytabh,Y	; 	+ y base
   441  c20d 85a6                       STA gaddr+1
   442  c20f a5aa                       LDA y		; vertical offset
   443  c211 2907                       AND #%00000111	; y mod 8
   444  c213 a8                         TAY
   445  c214 a59b                       LDA xl
   446  c216 2907                       AND #%00000111	; x mod 8
   447  c218 aa                         TAX		; horizonal offset
   448  c219 60                         RTS		; (bitmask)
   449                          
   450                          
   451                          ;-----------------------------------------------------------------
   452                          
   453                          ; line y up, x right, dx < dy (case 1)
   454                          
   455                          line_up_steep
   456  c21a 20f5c1                     JSR position	; x,y
   457                          loop_yup_xright
   458  c21d 20ed03                     JSR gchange	; pixel
   459                          
   460  c220 18                         CLC		; k += dx
   461  c221 a595                       LDA kl
   462  c223 65ab                       ADC dxl		; dxh is 0, because dx < dy
   463  c225 8595                       STA kl
   464  c227 b004                       BCS ++		; k > 255
   465                          
   466  c229 c5a9                       CMP dy
   467  c22b 9015                       BCC +		; k >= dy ->
   468                          
   469  c22d e5a9               ++	SBC dy		; k -= dy
   470  c22f 8595                       STA kl
   471                          
   472  c231 e8                         INX		; x++
   473  c232 e008                       CPX #8
   474  c234 d00c                       BNE +
   475                          	; C=1
   476  c236 a200                       LDX #0		; x overflow, wrap around
   477  c238 a5a5                       LDA gaddr	; x+8: gaddr += 8
   478  c23a 6907                       ADC #8-1	; C already set by CPX
   479  c23c 85a5                       STA gaddr
   480  c23e 9002                       BCC +
   481  c240 e6a6                       INC gaddr+1
   482                          
   483  c242 88                 +	DEY		; y--
   484  c243 100f                       BPL +++
   485  c245 38                         SEC		; y overflow
   486  c246 a5a5                       LDA gaddr
   487  c248 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   488  c24a 85a5                       STA gaddr
   489  c24c a5a6                       LDA gaddr+1
   490  c24e e901               	SBC #1
   491  c250 85a6                       STA gaddr+1
   492  c252 a007                       LDY #7		; wrap around
   493                          
   494  c254 c6a3               +++	DEC cl		; until c=0
   495  c256 d0c5                       BNE loop_yup_xright
   496  c258 4cd6c1                     JMP gexit
   497                          
   498                          
   499                          ;-----------------------------------------------------------------
   500                          
   501                          ; line x right, y up, dx > dy (case 2)
   502                          
   503                          line_up_flat
   504  c25b 20f5c1                     JSR position	; x,y
   505  c25e a5a3               	LDA cl		; counter adjustment for
   506  c260 f002               	BEQ +		; dec-dec-counting
   507  c262 e6a4               	INC ch
   508                          +
   509                          loop_xright_yup
   510  c264 20ed03                     JSR gchange	; pixel
   511                          
   512  c267 18                         CLC		; k += dy
   513  c268 a595                       LDA kl
   514  c26a 65a9                       ADC dy
   515  c26c 8595                       STA kl
   516  c26e 9002                       BCC ++
   517  c270 e696                       INC kh
   518                          
   519  c272 c5ab               ++	CMP dxl		; k > dx?
   520  c274 a596                       LDA kh
   521  c276 e5a7                       SBC dxh
   522  c278 901a                       BCC +
   523                          
   524  c27a 8596                       STA kh		; k -= dx
   525  c27c a595                       LDA kl
   526  c27e e5ab                       SBC dxl
   527  c280 8595                       STA kl
   528                          
   529  c282 88                         DEY		; y--
   530  c283 100f                       BPL +
   531  c285 38                 	SEC		; C=1 not always true (SBC above)
   532  c286 a5a5                       LDA gaddr	; y overflow
   533  c288 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   534  c28a 85a5                       STA gaddr
   535  c28c a5a6                       LDA gaddr+1
   536  c28e e901               	SBC #1
   537  c290 85a6                       STA gaddr+1
   538  c292 a007               	LDY #7		; wrap around
   539                          
   540  c294 e8                 +	INX		; x++
   541  c295 e008                       CPX #8		; x overflow?
   542  c297 d00c                       BNE ++
   543                          	; C=1
   544  c299 a200                       LDX #0		; wrap around
   545  c29b a5a5                       LDA gaddr	; x+8: gaddr += 8
   546  c29d 6907                       ADC #8-1	; C already set by CPX
   547  c29f 85a5                       STA gaddr
   548  c2a1 9002                       BCC ++
   549  c2a3 e6a6                       INC gaddr+1
   550                          ++
   551  c2a5 c6a3               	DEC cl		; c--
   552  c2a7 d0bb                       BNE loop_xright_yup
   553  c2a9 c6a4                       DEC ch		; adjusted high which allows this
   554  c2ab d0b7                       BNE loop_xright_yup
   555                          
   556  c2ad 4cd6c1                     JMP gexit
   557                          
   558                          
   559                          
   560                          ;-----------------------------------------------------------------
   561                          
   562                          ; line x right, y down, dx > dy (case 3)
   563                          
   564                          line_down_flat
   565  c2b0 20f5c1                     JSR position	; x,y
   566  c2b3 a5a3               	LDA cl		; counter adjustment for
   567  c2b5 f002               	BEQ +		; dec-dec-counting
   568  c2b7 e6a4               	INC ch
   569                          +
   570                          loop_xright_ydown
   571  c2b9 20ed03                     JSR gchange	; pixel
   572                          
   573  c2bc 18                         CLC		; k += dy
   574  c2bd a595                       LDA kl
   575  c2bf 65a9                       ADC dy
   576  c2c1 8595                       STA kl
   577  c2c3 9002                       BCC ++
   578  c2c5 e696                       INC kh
   579                          
   580  c2c7 c5ab               ++	CMP dxl		; k > dx
   581  c2c9 a596                       LDA kh
   582  c2cb e5a7                       SBC dxh		; k -= dx
   583  c2cd 901b                       BCC +
   584                          
   585  c2cf 8596                       STA kh
   586  c2d1 a595                       LDA kl
   587  c2d3 e5ab                       SBC dxl
   588  c2d5 8595                       STA kl
   589                          
   590  c2d7 c8                         INY		; y++
   591  c2d8 c008                       CPY #8
   592  c2da d00e                       BNE +
   593                          	; C=1
   594  c2dc a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   595  c2de 693f                       ADC #$40-1	; C already set by CPY
   596  c2e0 85a5                       STA gaddr
   597  c2e2 a5a6                       LDA gaddr+1
   598  c2e4 6901               	ADC #1
   599  c2e6 85a6                       STA gaddr+1
   600  c2e8 a000                       LDY #0		; wrap around
   601                          
   602  c2ea e8                 +	INX		; x++
   603  c2eb e008                       CPX #8		; x overflow ?
   604  c2ed d00c                       BNE +++
   605                          	; C=1
   606  c2ef a200                       LDX #$00	; wrap around
   607  c2f1 a5a5                       LDA gaddr	; gaddr += 8
   608  c2f3 6907                       ADC #$08-1	; C always set by CPX
   609  c2f5 85a5                       STA gaddr
   610  c2f7 9002                       BCC +++
   611  c2f9 e6a6                       INC gaddr+1
   612                          +++
   613  c2fb c6a3               	DEC cl		; c--
   614  c2fd d0ba                       BNE loop_xright_ydown
   615  c2ff c6a4                       DEC ch		; adjusted high which allows this
   616  c301 d0b6                       BNE loop_xright_ydown
   617                          
   618  c303 4cd6c1                     JMP gexit
   619                          
   620                          
   621                          ;-----------------------------------------------------------------
   622                          
   623                          ; line y down, x right, dx < dy (case 4)
   624                          
   625                          line_down_steep
   626  c306 20f5c1                     JSR position	; x,y
   627                          loop_ydown_xright
   628  c309 20ed03                     JSR gchange	; pixel
   629                          
   630  c30c 18                         CLC		; k += dx
   631  c30d a595                       LDA kl
   632  c30f 65ab                       ADC dxl		; dxh is 0, because dx < dy
   633  c311 8595                       STA kl
   634  c313 b004                       BCS ++
   635  c315 c5a9                       CMP dy		; k > dy?
   636  c317 9015                       BCC +
   637  c319 e5a9               ++	SBC dy		; k -= dy
   638  c31b 8595                       STA kl
   639                          
   640  c31d e8                         INX		; x++
   641  c31e e008                       CPX #8
   642  c320 d00c                       BNE +		; x overflow?
   643  c322 a200                       LDX #0		; wrap around
   644  c324 a5a5                       LDA gaddr	; x+9: gaddr += 8
   645  c326 6907                       ADC #8-1	; C already set by CPX
   646  c328 85a5                       STA gaddr
   647  c32a 9002                       BCC +
   648  c32c e6a6                       INC gaddr+1
   649                          
   650  c32e c8                 +	INY		; y++
   651  c32f c008                       CPY #8		; y overflow?
   652  c331 d00e                       BNE +++
   653  c333 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   654  c335 693f                       ADC #$40-1	; C already set by CPY
   655  c337 85a5                       STA gaddr
   656  c339 a5a6                       LDA gaddr+1
   657  c33b 6901               	ADC #1
   658  c33d 85a6                       STA gaddr+1
   659  c33f a000                       LDY #0		; wrap around
   660                          
   661  c341 c6a3               +++	DEC cl		; c--
   662                          			; until c=0
   663  c343 d0c4                       BNE loop_ydown_xright
   664  c345 4cd6c1                     JMP gexit
   665                          
   666                          
   667                          ;-----------------------------------------------------------------
   668                          
   669                          getcommaxy
   670  c348 20fdae                     JSR b_getcomma	; check ","
   671                          getxy
   672  c34b 208aad                     JSR b_getval	; get X coord. value
   673  c34e 20f7b7                     JSR b_convint
   674  c351 c901                       CMP #>xmax
   675  c353 9009               	BCC gcxy_xok
   676  c355 f003                       BEQ +		; X = $1xx
   677                          error_iq
   678  c357 206fc5                     JSR range_error
   679  c35a c040               +	CPY #<xmax	; check X low
   680  c35c b0f9                       BCS error_iq	; X to big
   681                          gcxy_xok
   682  c35e 84fb                       STY gpos	; temporary save X coord.
   683  c360 85fc                       STA gpos+1
   684                          
   685  c362 20f1b7                     JSR b_getcomma8bit
   686                          			; get Y coord. value
   687  c365 e0c8                       CPX #ymax
   688  c367 b0ee                       BCS error_iq	; Y to big
   689                          
   690  c369 a4fb                       LDY gpos	; restory X coord.
   691  c36b a5fc                       LDA gpos+1
   692  c36d 60                         RTS
   693                          
   694                          
   695                          ;-----------------------------------------------------------------
   696                          
   697                          hline
   698  c36e 204bc3                     JSR getxy	; get startpoint
   699  c371 86aa                       STX y
   700  c373 8e3603                     STX savey	; save as cursor, too
   701  c376 859c                       STA xh
   702  c378 849b                       STY xl
   703  c37a 20fdae                     JSR b_getcomma	; get length
   704  c37d 208aad                     JSR b_getval
   705  c380 20f7b7                     JSR b_convint
   706                          
   707  c383 c901                       CMP #>xmax
   708  c385 9006                       BCC +		; X < 256
   709  c387 d0ce                       BNE error_iq
   710  c389 c040                       CPY #<xmax
   711  c38b b0ca                       BCS error_iq
   712                          +
   713                          			; calculate end point
   714  c38d aa                         TAX		; save length high byte
   715  c38e 98                         TYA		; length low byte
   716  c38f 18                         CLC
   717  c390 659b                       ADC xl		; low xend = x+length
   718  c392 859e                       STA xendl
   719  c394 a8                 	TAY
   720  c395 8a                         TXA		; high
   721  c396 659c                       ADC xh		; high xend = x+length
   722  c398 859f                       STA xendh
   723  c39a aa                 	TAX
   724                          
   725  c39b c901               	CMP #>xmax	; endpoint outside?
   726  c39d 9005               	BCC +
   727  c39f 98                 	TYA
   728  c3a0 e940               	SBC #<xmax
   729  c3a2 b0b3               	BCS error_iq
   730                          +
   731  c3a4 8e3503                     STX savexh
   732  c3a7 8c3403                     STY savexl	; also save as cursor
   733                          
   734  c3aa 20dec1                     JSR ginit	; map in graphic memory
   735                          
   736                          hline_start
   737  c3ad a59e                       LDA xendl
   738  c3af c59b                       CMP xl
   739  c3b1 a59f                       LDA xendh
   740  c3b3 e59c                       SBC xh
   741  c3b5 b013                       BCS hl_noxswap	; xend < x ->
   742                          
   743  c3b7 a69e                       LDX xendl	; swap x, xend
   744  c3b9 a59b                       LDA xl
   745  c3bb 869b                       STX xl
   746  c3bd 859e                       STA xendl
   747                          
   748  c3bf a69f                       LDX xendh
   749  c3c1 a49c                       LDY xh
   750  c3c3 849f                       STY xendh
   751  c3c5 869c                       STX xh
   752  c3c7 4cd9c3                     JMP hl_start	; x != xend
   753                          
   754                          hl_noxswap
   755  c3ca a59e                       LDA xendl
   756  c3cc c59b                       CMP xl
   757  c3ce d009                       BNE hl_start
   758  c3d0 a59f                       LDA xendh
   759  c3d2 c59c                       CMP xh
   760  c3d4 d003                       BNE hl_start	; x = xend ->
   761  c3d6 4c4dc5             	JMP plot_start	; single point
   762                          ;	JMP gexit	; no point
   763                          
   764                          hl_start
   765  c3d9 20f5c1                     JSR position	; graphic position x,y
   766  c3dc bd2ac1                     LDA maskleft,X
   767  c3df 48                         PHA		; save left end mask
   768  c3e0 a59e                       LDA xendl
   769  c3e2 2907                       AND #%00000111
   770  c3e4 8596                       STA tmp2	; xend mod 8, mask index
   771  c3e6 a59b                       LDA xl
   772  c3e8 29f8                       AND #%11111000	; (xl div 8)*8
   773  c3ea 8595                       STA tmp1
   774  c3ec a59e                       LDA xendl	; xend unmasked
   775  c3ee 38                         SEC
   776  c3ef e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   777  c3f1 8595                       STA tmp1
   778  c3f3 a59f                       LDA xendh
   779  c3f5 e59c                       SBC xh
   780  c3f7 4a                         LSR		; / 8 ->  0-39
   781  c3f8 a595                       LDA tmp1	; only 1 highest bit
   782  c3fa 6a                         ROR		; and 3 lower bits
   783  c3fb 4a                         LSR
   784  c3fc 4a                         LSR
   785  c3fd aa                         TAX		; 8-pixel-blocks count
   786  c3fe 68                         PLA		; left end x mask
   787                          
   788                          hl_nextblock
   789  c3ff ca                         DEX
   790                          hl_islastblock
   791  c400 3012                       BMI hl_lastblock
   792                          			; leave loop if X<0
   793  c402 20f503                     JSR gmask	; first with left end mask
   794  c405 18                         CLC		; gaddr += 8
   795  c406 a5a5                       LDA gaddr
   796  c408 6908                       ADC #8
   797  c40a 85a5                       STA gaddr
   798  c40c 9002                       BCC +
   799  c40e e6a6                       INC gaddr+1
   800  c410 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   801  c412 d0eb               	BNE hl_nextblock	; always
   802                          
   803                          hl_lastblock
   804  c414 a696                       LDX tmp2	; xend mask index
   805  c416 3d32c1                     AND maskright,X ; mask right end
   806  c419 20f503                     JSR gmask	; modify
   807  c41c 4cd6c1                     JMP gexit	; leave
   808                          
   809                          
   810                          ;-----------------------------------------------------------------
   811                          
   812                          vline
   813  c41f 204bc3                     JSR getxy	; get startpoint
   814  c422 859c                       STA xh
   815  c424 8d3503                     STA savexh	; save as cursor too
   816  c427 849b                       STY xl
   817  c429 8c3403                     STY savexl
   818  c42c 86aa                       STX y
   819                          
   820  c42e 20f1b7                     JSR b_getcomma8bit
   821                          			; get length
   822  c431 18                         CLC		; calculate end point
   823  c432 8a                         TXA		; length
   824                          ; DON'T-CHANGE: how long to go vertically (needed later)
   825                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   826                          ;	STA tmp1
   827  c433 65aa                       ADC y		; length + y
   828  c435 c9c8                       CMP #ymax
   829  c437 9003                       BCC +
   830                          vline_iq
   831  c439 206fc5                     JSR range_error
   832  c43c 8593               +	STA yend	; endpoint
   833  c43e c9c8               	CMP #ymax	; outside?
   834  c440 b0f7               	BCS vline_iq
   835                          
   836  c442 8d3603             	STA savey	; set cursor y position
   837                          
   838  c445 20dec1                     JSR ginit	; map in graphic memory
   839                          
   840                          vline_start
   841  c448 a593                       LDA yend
   842  c44a c5aa                       CMP y
   843  c44c b00a                       BCS vl_noyswap	; yend < y ->
   844  c44e a5aa                       LDA y		; swap y, yend
   845  c450 a693                       LDX yend
   846  c452 8593                       STA yend
   847  c454 86aa                       STX y
   848  c456 f005               	BEQ vl_start	; always (with next branch)
   849                          	; fall through if yend is
   850                          vl_noyswap
   851  c458 d003                       BNE vl_start	; y = yend ->
   852  c45a 4c4dc5             	JMP plot_start	; single point
   853                          ;	JMP gexit	; no point
   854                          
   855                          vl_start
   856  c45d 20f5c1                     JSR position	; graphic position x,y
   857  c460 bdfdc0                     LDA bitmask,X
   858  c463 8596                       STA tmp2	; save mask
   859                          ; DON'T-CHANGE: replace ...
   860  c465 38                         SEC
   861  c466 a593                       LDA yend
   862  c468 e5aa                       SBC y		; vertical length
   863  c46a aa                         TAX
   864                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   865                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   866                          ;	LDX tmp1
   867  c46b e8                         INX		; +1 (exit on 0)
   868                          vl_nextline
   869  c46c a596                       LDA tmp2
   870  c46e 20f503                     JSR gmask	; modify 
   871  c471 c8                         INY		; go down
   872  c472 c008                       CPY #8		; 8-line wrap
   873  c474 d00e                       BNE +
   874  c476 a5a5                       LDA gaddr	; gaddr += 320
   875  c478 693f               	ADC #$40-1	; compensate for C = 1
   876  c47a 85a5                       STA gaddr
   877  c47c a5a6                       LDA gaddr+1
   878  c47e 6901                       ADC #$01
   879  c480 85a6                       STA gaddr+1
   880  c482 a000                       LDY #0		; wrap y offset
   881  c484 ca                 +	DEX		; all vertical positions done?
   882  c485 d0e5                       BNE vl_nextline
   883  c487 4cd6c1                     JMP gexit	; leave
   884                          
   885                          
   886                          ;-----------------------------------------------------------------
   887                          
   888                          line
   889  c48a 204bc3                     JSR getxy	; get startpoint
   890  c48d 849b                       STY xl 
   891  c48f 859c                       STA xh
   892  c491 86aa                       STX y
   893                          
   894  c493 2048c3                     JSR getcommaxy	; get endpoint
   895                          line_start
   896  c496 8c3403                     STY savexl	; save as cursor position too
   897  c499 849e                       STY xendl
   898  c49b 8d3503                     STA savexh
   899  c49e 859f                       STA xendh
   900  c4a0 8e3603                     STX savey
   901  c4a3 8693                       STX yend
   902                          
   903  c4a5 20dec1                     JSR ginit	; map in graphic memory
   904                          
   905  c4a8 a000                       LDY #$00	; initialize to 0
   906  c4aa 84a8                       STY ydir
   907  c4ac 8495                       STY kl
   908  c4ae 8496                       STY kh
   909                          
   910  c4b0 38                         SEC
   911  c4b1 a59e                       LDA xendl	; calculate dx
   912  c4b3 e59b                       SBC xl
   913  c4b5 85ab                       STA dxl
   914  c4b7 a59f                       LDA xendh
   915  c4b9 e59c                       SBC xh
   916  c4bb 85a7                       STA dxh
   917                          
   918  c4bd b025                       BCS li_xend_right
   919                          	; dx != 0
   920  c4bf 98                         TYA		; negate dx
   921  c4c0 38                         SEC		; dx = 0 - dx
   922  c4c1 e5ab                       SBC dxl
   923  c4c3 85ab                       STA dxl
   924  c4c5 98                         TYA
   925  c4c6 e5a7                       SBC dxh
   926  c4c8 85a7                       STA dxh
   927                          			; C=0 always, needed later
   928  c4ca a69b                       LDX xl		; swap x low
   929  c4cc a49e                       LDY xendl
   930  c4ce 869e                       STX xendl
   931  c4d0 849b                       STY xl
   932                          
   933  c4d2 a69c                       LDX xh		; swap x high
   934  c4d4 a49f                       LDY xendh
   935  c4d6 869f                       STX xendh
   936  c4d8 849c                       STY xh
   937                          
   938  c4da a6aa                       LDX y		; swap y
   939  c4dc a493                       LDY yend
   940  c4de 8693                       STX yend
   941  c4e0 84aa                       STY y
   942                          
   943  c4e2 9009                       BCC li_x_different
   944                          			; C=0 always (from negation before)
   945                          
   946                          li_xend_right
   947  c4e4 a5ab                       LDA dxl		; dx = 0?
   948  c4e6 05a7                       ORA dxh
   949  c4e8 d003                       BNE li_x_different
   950  c4ea 4c48c4                     JMP vline_start	; vertical line case
   951                          
   952                          li_x_different
   953  c4ed 38                         SEC		; calculate dy
   954  c4ee a593                       LDA yend
   955  c4f0 e5aa                       SBC y
   956  c4f2 b006                       BCS li_y_right
   957  c4f4 49ff                       EOR #$FF	; negate dy (two's complement)
   958  c4f6 6901                       ADC #$01	; C=0
   959  c4f8 85a8                       STA ydir	; flag y goes up
   960                          
   961                          li_y_right
   962  c4fa 85a9                       STA dy
   963  c4fc d003                       BNE +
   964  c4fe 4cadc3                     JMP hline_start	; horizontal line case
   965                          +
   966                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   967                          
   968  c501 a5a7                       LDA dxh		; dx > dy
   969  c503 d017                       BNE line_flat	; yes -> flat
   970  c505 a5a9                       LDA dy		; no -> steep
   971  c507 aa                         TAX
   972  c508 c5ab                       CMP dxl
   973  c50a 9010                       BCC line_flat
   974                          
   975                          line_steep
   976  c50c e8                         INX	
   977  c50d 86a3                       STX cl		; c = dy+1
   978  c50f 4a                         LSR		; k = dy/2
   979  c510 8595                       STA kl
   980  c512 a5a8                       LDA ydir
   981  c514 d003                       BNE +
   982  c516 4c06c3                     JMP line_down_steep	; y down, steep
   983  c519 4c1ac2             +	JMP line_up_steep	; y up, steep
   984                          
   985                          line_flat
   986  c51c a5a7                       LDA dxh
   987  c51e a8                         TAY
   988  c51f a6ab                       LDX dxl
   989  c521 e8                         INX
   990  c522 d001                       BNE +
   991  c524 c8                         INY
   992  c525 86a3               +	STX cl		; c = dx+1
   993  c527 84a4                       STY ch
   994                          
   995  c529 4a                         LSR		; k = dx/2
   996  c52a 8596                       STA kh
   997  c52c a5ab                       LDA dxl
   998  c52e 6a                         ROR		; dx/2
   999  c52f 8595                       STA kl
  1000  c531 a5a8                       LDA ydir	
  1001  c533 d003                       BNE +
  1002  c535 4cb0c2                     JMP line_down_flat	; y down, flat
  1003  c538 4c5bc2             +	JMP line_up_flat	; y up, flat
  1004                          
  1005                          ;-----------------------------------------------------------------
  1006                          
  1007                          plot
  1008  c53b 204bc3                     JSR getxy	; get parameter
  1009  c53e 859c                       STA xh		; save x/y
  1010  c540 849b                       STY xl
  1011  c542 86aa                       STX y
  1012  c544 8d3503                     STA savexh	; and store as cursor
  1013  c547 8c3403                     STY savexl
  1014  c54a 8e3603                     STX savey
  1015                          
  1016                          plot_start
  1017  c54d 20f5c1                     JSR position	; calculate graphical address
  1018                          
  1019  c550 a501                       LDA prozport
  1020  c552 29fd                       AND #%11111101	; Kernal ROM disable
  1021  c554 78                         SEI			
  1022  c555 8501                       STA prozport
  1023                          
  1024  c557 20ed03                     JSR gchange	; change graphical data
  1025                          
  1026  c55a a501                       LDA prozport
  1027  c55c 0902                       ORA #%00000010	; kernal ROM enable
  1028  c55e 8501                       STA prozport
  1029  c560 58                         CLI
  1030  c561 60                         RTS
  1031                          
  1032                          ;-----------------------------------------------------------------
  1033                          
  1034                          move
  1035  c562 204bc3                     JSR getxy	; get parameter
  1036  c565 8d3503                     STA savexh	; just save as cursor
  1037  c568 8c3403                     STY savexl
  1038  c56b 8e3603                     STX savey
  1039  c56e 60                         RTS
  1040                          
  1041                          
  1042                          ;-----------------------------------------------------------------
  1043                          
  1044                          range_error
  1045  c56f ad3703             	LDA savemo
  1046  c572 29f0               	AND #$F0
  1047  c574 d003               	BNE +
  1048  c576 68                 	PLA			; cleanup JSR
  1049  c577 68                 	PLA
  1050  c578 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1051                          				; error mode 2: cut value: control back
  1052                          				; to handle value correction
  1053  c579 2920               +	AND #$20
  1054  c57b d0fb               	BNE -
  1055  c57d 68                 	PLA			; cleanup JSR
  1056  c57e 68                 	PLA
  1057                          setmode_error
  1058  c57f 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1059                          
  1060                          ;-----------------------------------------------------------------
  1061                          
  1062                          setmode
  1063  c582 209eb7                     JSR b_get8bit
  1064  c585 e003                       CPX #3
  1065  c587 9012                       BCC +			; less then 3, modification mode
  1066  c589 e006               	CPX #6
  1067  c58b b0f2               	BCS setmode_error	; out of range
  1068                          				; error mode
  1069  c58d 690d               	ADC #13			; C=0, therefore -3
  1070                          				; 3-5 -> 16-18
  1071                          				; put A's bit 4-7 into savemo
  1072  c58f 4d3703             	EOR savemo		; ********
  1073  c592 29f0               	AND #$F0		; ****0000
  1074  c594 4d3703             	EOR savemo		; AAAAmmmm
  1075  c597 8d3703             	STA savemo		; 
  1076  c59a 60                 	RTS
  1077                          
  1078  c59b 8a                 +	TXA
  1079  c59c 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1080  c59f 290f               	AND #$0F
  1081  c5a1 4d3703             	EOR savemo
  1082  c5a4 8d3703             	STA savemo
  1083                          setmode_enter
  1084  c5a7 e001               	CPX #$01
  1085  c5a9 b01a                       BCS set_or_toggle
  1086                          
  1087                          modereset
  1088  c5ab a9c1                       LDA #>(nbitmask)
  1089  c5ad 8df103                     STA gchange_op+2
  1090  c5b0 a905                       LDA #<(nbitmask)
  1091  c5b2 8df003                     STA gchange_op+1
  1092  c5b5 a93d                       LDA #$3D		; AND abs,X
  1093  c5b7 8def03                     STA gchange_op
  1094  c5ba a931                       LDA #$31		; AND (zp),Y
  1095  c5bc 8df703                     STA gmask_op
  1096  c5bf a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1097  c5c1 8df603                     STA gmask_flip+1
  1098  c5c4 60                         RTS
  1099                          
  1100                          set_or_toggle
  1101  c5c5 d01a                       BNE modetoggle
  1102                          modeset
  1103  c5c7 a9c0                       LDA #>(bitmask)
  1104  c5c9 8df103                     STA gchange_op+2
  1105  c5cc a9fd                       LDA #<(bitmask)
  1106  c5ce 8df003                     STA gchange_op+1
  1107  c5d1 a91d                       LDA #$1D		; OR abs,X
  1108  c5d3 8def03                     STA gchange_op
  1109  c5d6 a911                       LDA #$11		; OR (zp),Y
  1110  c5d8 8df703                     STA gmask_op
  1111  c5db a900                       LDA #$00		; EOR #$00, nicht invertieren
  1112  c5dd 8df603                     STA gmask_flip+1
  1113  c5e0 60                         RTS
  1114                          
  1115                          modetoggle
  1116  c5e1 a9c0                       LDA #>(bitmask)
  1117  c5e3 8df103                     STA gchange_op+2
  1118  c5e6 a9fd                       LDA #<(bitmask)
  1119  c5e8 8df003                     STA gchange_op+1
  1120  c5eb a95d                       LDA #$5D		; EOR abs,X
  1121  c5ed 8def03                     STA gchange_op
  1122  c5f0 a951                       LDA #$51		; EOR (zp),Y
  1123  c5f2 8df703                     STA gmask_op
  1124  c5f5 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1125  c5f7 8df603                     STA gmask_flip+1
  1126  c5fa 60                         RTS
  1127                          
  1128                          
  1129                          ;-----------------------------------------------------------------
  1130                          
  1131                          ; get pixel (check if pixel set)
  1132                          ; not used
  1133                          
  1134                          get
  1135  c5fb 2048c3                     JSR getcommaxy
  1136  c5fe 859c                       STA xh
  1137  c600 849b                       STY xl
  1138  c602 86aa                       STX y
  1139                          
  1140  c604 20f5c1                     JSR position
  1141                          
  1142  c607 a501                       LDA prozport
  1143  c609 29fd               	AND #%11111101	; Kernal ROM disable
  1144  c60b 78                         SEI
  1145  c60c 8501                       STA prozport
  1146                          
  1147  c60e b1a5                       LDA (gaddr),Y
  1148  c610 3dfdc0                     AND bitmask,X
  1149  c613 a8                         TAY
  1150  c614 a501                       LDA prozport
  1151  c616 0902               	ORA #%00000010	; kernal ROM enable
  1152  c618 8501                       STA prozport
  1153  c61a 58                         CLI
  1154  c61b 4ca2b3                     JMP b_byte2fac
  1155                          
  1156                          
  1157                          ;-----------------------------------------------------------------
  1158                          
  1159                          relto
  1160  c61e 208aad                     JSR b_getval	; get X offset (+/-)
  1161  c621 a561               	LDA facexp	; FAC exponent
  1162  c623 c990               	CMP #$90	; more than 16 bit
  1163  c625 b031               	BCS relto_error	; illegal quantity
  1164  c627 209bbc                     JSR b_fac2int	; to signed integer
  1165                          
  1166  c62a 18                         CLC
  1167  c62b a565                       LDA facintl
  1168  c62d 6d3403                     ADC savexl
  1169  c630 859e                       STA xendl
  1170  c632 a564                       LDA facinth
  1171  c634 6d3503                     ADC savexh
  1172  c637 859f                       STA xendh	; xend = savex+facint
  1173                          
  1174  c639 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1175  c63c 208aad                     JSR b_getval
  1176  c63f a561                       LDA facexp	; FAC exponent
  1177  c641 c990                       CMP #$90	; more than 16 bit
  1178  c643 b013                       BCS relto_error	; illegal quantity
  1179  c645 209bbc                     JSR b_fac2int	; to signed integer
  1180  c648 18                         CLC
  1181  c649 a565                       LDA facintl
  1182  c64b 6d3603                     ADC savey
  1183  c64e 8593                       STA yend	; yend = savey+facint
  1184                          
  1185  c650 a59f                       LDA xendh	; check end coord. x
  1186  c652 c901                       CMP #>xmax
  1187  c654 900b                       BCC rt_xok
  1188  c656 f003                       BEQ +
  1189                          relto_error
  1190  c658 206fc5                     JSR range_error
  1191  c65b a59e               +	LDA xendl
  1192  c65d c940                       CMP #<xmax
  1193  c65f b0f7                       BCS relto_error
  1194                          rt_xok
  1195  c661 a593                       LDA yend	; check end coord. y
  1196  c663 c9c8                       CMP #ymax
  1197  c665 b0f1                       BCS relto_error
  1198                          
  1199  c667 ad3403                     LDA savexl
  1200  c66a 859b                       STA xl
  1201  c66c ad3503                     LDA savexh
  1202  c66f 859c                       STA xh
  1203  c671 ad3603                     LDA savey
  1204  c674 85aa                       STA y
  1205  c676 a49e                       LDY xendl
  1206  c678 a59f                       LDA xendh
  1207  c67a a693                       LDX yend	; xend/yend = cursor + x/y
  1208                          
  1209  c67c 4c96c4                     JMP line_start	; draw line x/y to xend/yend
  1210                          
  1211                          
  1212                          ;-----------------------------------------------------------------
  1213                          
  1214                          char
  1215  c67f 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1216  c682 e028                       CPX #40	
  1217  c684 9003                       BCC +
  1218                          char_error
  1219  c686 4c48b2                     JMP b_illquant
  1220  c689 86fb               +	STX gpos	; save x coord.
  1221  c68b 20f1b7                     JSR b_getcomma8bit
  1222                          			; get char. position y 0-24
  1223  c68e e019                       CPX #25
  1224  c690 b0f4                       BCS char_error
  1225  c692 86fc                       STX gpos+1	; save y coord.
  1226                          
  1227  c694 20fdae                     JSR b_getcomma	; get string
  1228  c697 209ead                     JSR b_getexpr
  1229  c69a 20a3b6                     JSR b_stringval ; string address in str
  1230  c69d 48                         PHA		; string length
  1231  c69e a6fc                       LDX gpos+1	; y coord. for char. position
  1232  c6a0 8a                         TXA
  1233  c6a1 2903                       AND #$03	; mask 2 bits
  1234  c6a3 a8                         TAY		; table index
  1235  c6a4 a900                       LDA #$00
  1236  c6a6 85fc                       STA gpos+1	; x high
  1237  c6a8 a5fb                       LDA gpos	; saved x: multiply by 8
  1238  c6aa 0a                         ASL
  1239  c6ab 0a                         ASL
  1240  c6ac 0a                         ASL
  1241  c6ad 26fc                       ROL gpos+1	; overflow to high byte
  1242  c6af 790dc1                     ADC ytabl,Y
  1243  c6b2 85a5                       STA gaddr
  1244  c6b4 a5fc                       LDA gpos+1	; x high
  1245  c6b6 7d11c1                     ADC ytabh,X
  1246  c6b9 85a6                       STA gaddr+1
  1247  c6bb 68                         PLA		; string length
  1248  c6bc a000                       LDY #$00	; string index
  1249  c6be aa                         TAX		; length
  1250  c6bf e8                         INX		; prepare as counter
  1251                          char_loop
  1252  c6c0 ca                         DEX
  1253  c6c1 f008                       BEQ char_exit
  1254  c6c3 b122                       LDA (str),Y	; read string
  1255  c6c5 20ccc6                     JSR char_display
  1256  c6c8 c8                         INY
  1257  c6c9 d0f5                       BNE char_loop
  1258                          char_exit
  1259  c6cb 60                         RTS
  1260                          
  1261                          char_display
  1262  c6cc 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1263  c6ce 8a                         TXA		; save register X+Y
  1264  c6cf 48                         PHA
  1265  c6d0 98                         TYA
  1266  c6d1 48                         PHA
  1267  c6d2 a5d7                       LDA z_tmp	; get saved character
  1268  c6d4 3012                       BMI char_inverse
  1269                          
  1270                          char_normal
  1271  c6d6 c920                       CMP #$20	; control character?
  1272  c6d8 9054                       BCC char_disp_leave
  1273  c6da c960                       CMP #$60
  1274  c6dc 9004                       BCC +
  1275  c6de 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1276  c6e0 d014                       BNE char_hires
  1277  c6e2 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1278  c6e4 d010               	BNE char_hires
  1279  c6e6 f00e               	BEQ char_hires
  1280                          
  1281                          char_inverse
  1282  c6e8 297f                       AND #%01111111	; mask bit 7
  1283  c6ea c97f                       CMP #%01111111	; was 255? (pi)
  1284  c6ec d002                       BNE +
  1285  c6ee a95e                       LDA #$5E	; screen code for pi
  1286  c6f0 c920               +	CMP #$20	; control character?
  1287  c6f2 903a                       BCC char_disp_leave
  1288                          			; yes, skip
  1289  c6f4 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1290                          			; $C0-$FF -> $40-$7F
  1291                          			; OPT: BNE char_hires
  1292                          			; OPT: char_normal
  1293                          char_hires
  1294  c6f6 a6c7                       LDX z_reverseflag
  1295  c6f8 f002                       BEQ +
  1296  c6fa 0980                       ORA #%10000000	; invert char.
  1297  c6fc aa                 +	TAX		; save char. for later
  1298  c6fd a501                       LDA prozport	; save prozport state
  1299  c6ff 48                 	PHA
  1300  c700 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1301  c702 78                         SEI
  1302  c703 8501                       STA prozport	; char. rom base = $D000
  1303  c705 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1304  c707 85fc                       STA gpos+1	; 
  1305  c709 8a                         TXA		; char. code
  1306  c70a 0a                         ASL		; *8
  1307  c70b 26fc                       ROL gpos+1
  1308  c70d 0a                         ASL
  1309  c70e 26fc                       ROL gpos+1
  1310  c710 0a                         ASL
  1311  c711 26fc                       ROL gpos+1
  1312  c713 85fb                       STA gpos	; addr. in char. rom for char.
  1313                          
  1314  c715 a007                       LDY #$07	; 8 hires lines
  1315                          char_line
  1316  c717 b1fb                       LDA (gpos),Y	; read character line
  1317  c719 20f503                     JSR gmask	; write to hires screen
  1318  c71c 88                         DEY
  1319  c71d 10f8                       BPL char_line
  1320                          
  1321  c71f 68                 	PLA
  1322  c720 8501                       STA prozport
  1323  c722 58                         CLI
  1324                          
  1325  c723 18                         CLC		; step char position to left
  1326  c724 a5a5                       LDA gaddr	; ( +8 )
  1327  c726 6908                       ADC #$08
  1328  c728 85a5                       STA gaddr
  1329  c72a 9002                       BCC +
  1330  c72c e6a6                       INC gaddr+1
  1331                          +
  1332                          char_disp_leave
  1333  c72e 68                 	PLA		; pass written character back
  1334  c72f a8                         TAY		; restore saved registers
  1335  c730 68                         PLA
  1336  c731 aa                         TAX
  1337  c732 60                         RTS
  1338                          
  1339                          
  1340                          ;-----------------------------------------------------------------
  1341                          
  1342                          to
  1343  c733 ad3403                     LDA savexl
  1344  c736 859b                       STA xl
  1345  c738 ad3503                     LDA savexh
  1346  c73b 859c                       STA xh
  1347  c73d ad3603                     LDA savey
  1348  c740 85aa                       STA y
  1349  c742 204bc3                     JSR getxy
  1350  c745 4c96c4                     JMP line_start
  1351                          
  1352                          ;-----------------------------------------------------------------
  1353                          
  1354                          unnew
  1355                          
  1356  c748 a52b               	lda bassta
  1357  c74a 8522               	sta str
  1358  c74c a52c               	lda bassta+1
  1359  c74e 8523               	sta str+1
  1360  c750 a001               	ldy #1
  1361  c752 98                 	tya
  1362  c753 9122               	sta (str),y		; != 0
  1363                          
  1364  c755 2033a5             	jsr b_rechain		; starting from bassta
  1365                          				; result in (str)
  1366  c758 18                 	clc			; str+1 -> new basic end
  1367  c759 a423               	ldy str+1
  1368  c75b a522               	lda str
  1369  c75d 6902               	adc #2
  1370  c75f 852d               	sta basend
  1371  c761 9001               	bcc +
  1372  c763 c8                 	iny
  1373  c764 842e               +	sty basend+1
  1374  c766 4c60a6             	jmp b_clr		; perform CLR
  1375                          
  1376                          ;-----------------------------------------------------------------
  1377                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
