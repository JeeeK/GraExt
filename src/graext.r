
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
    21  c003 a977                       lda #<(parse)		; check if basic interpreter parser hook
    22  c005 cd0803                     cmp v_bascmd		; does already exist
    23  c008 d008               	bne start
    24  c00a a9c0                       lda #>(parse)
    25  c00c cd0903                     cmp v_bascmd+1
    26  c00f d001                       bne start
    27  c011 60                 	rts			; hook already in place, no start message
    28                          
    29                          start
    30  c012 201fc0                     jsr init                ; init extension (place hook)
    31  c015 a9cd                       lda #<author            ; message ...
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
   140  c01f a977                       LDA #<(parse)	; basic interpreter parser hook
   141  c021 8d0803                     STA v_bascmd
   142  c024 a9c0                       LDA #>(parse)
   143  c026 8d0903                     STA v_bascmd+1
   144                          
   145  c029 ad2803                     LDA v_basstp
   146  c02c 8d3a03             	STA savevstp
   147  c02f a96b                       LDA #<(stop)	; basic interpreter stop hook
   148  c031 8d2803                     STA v_basstp
   149  c034 ad2903                     LDA v_basstp+1
   150  c037 8d3b03             	STA savevstp+1
   151  c03a a9c0                       LDA #>(stop)
   152  c03c 8d2903                     STA v_basstp+1
   153                          
   154  c03f ad0003                     LDA v_baserr
   155  c042 8d3803             	STA saveverr
   156  c045 a965                       LDA #<(error)	; basic interpreter error hook
   157  c047 8d0003                     STA v_baserr
   158  c04a ad0103                     LDA v_baserr+1
   159  c04d 8d3903             	STA saveverr+1
   160  c050 a9c0                       LDA #>(error)
   161  c052 8d0103                     STA v_baserr+1
   162                          
   163  c055 a200               	LDX #0		; set graphic cursor to (0,0)
   164  c057 8e3403             	STX savexl
   165  c05a 8e3503             	STX savexh
   166  c05d 8e3603             	STX savey
   167  c060 e8                 	INX
   168  c061 8e3703             	STX savemo	; set mode 1
   169  c064 60                         RTS
   170                          
   171                          error	
   172                          	; reg A may destroyed
   173  c065 203bc1             	JSR gra_off		; uses only reg A
   174  c068 6c3803             	JMP (saveverr)		; to original vector
   175                          
   176                          stop	
   177                          	; reg A may destroyed
   178  c06b a591               	LDA $91			; Scan code
   179  c06d c97f               	CMP #$7F		; STOP key?
   180  c06f d003               	BNE nostop
   181  c071 203bc1             	JSR gra_off		; uses only reg A
   182                          nostop
   183  c074 6c3a03             	JMP (savevstp)		; to original vector
   184                          
   185                          ;-----------------------------------------------------------------
   186                          
   187                          ; start parsing an extension command ...
   188                          
   189                          parse
   190  c077 207300                     JSR chrget			; next char.
   191  c07a 08                 	PHP
   192  c07b c926                       CMP #'&'			; command prefix
   193  c07d f004                       BEQ newcmd
   194  c07f 28                         PLP
   195  c080 4ce7a7                     JMP b_execstatement
   196                          newcmd
   197  c083 28                 	PLP
   198  c084 207300                     JSR chrget			; command character
   199                          
   200  c087 a00c                       LDY #(cmdsend-cmds)		; map character to
   201                          					; command address ...
   202                          checknextcmd
   203  c089 88                         DEY
   204  c08a f01c               	BEQ parse_error
   205  c08c d9abc0                     CMP cmds,Y
   206  c08f d0f8                       BNE checknextcmd		; try next
   207  c091 88                         DEY				; found
   208  c092 98                         TYA
   209  c093 0a                         ASL				; *2
   210  c094 a8                         TAY
   211                          !ifndef command_rts_tyle {
   212                          	!set co=0			; command offset in jump table
   213  c095 b9b8c0                     LDA cmdaddr+1,Y                 ; high byte from table
   214  c098 8556                       STA ijmp+1
   215  c09a b9b7c0                     LDA cmdaddr,Y                   ; low byte from table
   216  c09d 8555                       STA ijmp
   217  c09f 207300                     JSR chrget			; read next byte in basic text
   218  c0a2 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   219  c0a5 4caea7                     JMP b_interpreter		; continue parsing
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
   234  c0a8 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   235                          
   236                          ;-----------------------------------------------------------------
   237                          
   238                          ; the most commonly used command placed at the end ...
   239                          
   240  c0ab 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   241                          cmdsend
   242                          
   243                          cmdaddr
   244  c0b7 3fc734c179c67cc5...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   245  c0c3 2ac719c468c384c4...        !word to-co,vline-co,hline-co,line-co,plot-co
   246                          
   247  c0cd 934752412d455854...author	!text 147,"GRA-EXT V1.23 1986,2016 JOHANN@KLASEK.AT",0
   248                          
   249                          bitmask
   250  c0f7 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   251                          nbitmask
   252  c0ff 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   253                          ytabl
   254  c107 004080c0           	!byte $00,$40,$80,$c0
   255                          ytabh
   256  c10b e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   257  c10f e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   258  c113 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   259  c117 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   260  c11b f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   261  c11f f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   262  c123 fe                 	!byte gramp+$1e
   263                          
   264                          ; for horiz. line
   265                          
   266  c124 ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   267                          
   268  c12c 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   269                          
   270                          
   271                          ;-----------------------------------------------------------------
   272                          
   273                          graphic
   274  c134 209eb7                     JSR b_get8bit
   275  c137 e000                       CPX #$00
   276  c139 d013                       BNE gra_other
   277                          gra0			; &G 0
   278                          gra_off
   279  c13b a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   280  c13d 8d00dd                     STA cia_pra
   281  c140 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   282                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   283                          			; char addr $1000/4096 = char. ROM
   284  c142 8d18d0                     STA vic_mcr	; VIC memory control
   285  c145 ad11d0                     LDA vic_cr	; VIC control register
   286  c148 29df                       AND #%11011111	; Hires mode off
   287  c14a 8d11d0                     STA vic_cr
   288  c14d 60                         RTS
   289                          
   290                          gra_other
   291  c14e e001                       CPX #$01
   292  c150 f00f               	BEQ gra1
   293  c152 e002               	CPX #$02
   294  c154 f00e                       BEQ gra2
   295  c156 e004               	CPX #$04
   296  c158 f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   297  c15a e003               	CPX #$03	; &G 3 (graphic on)
   298  c15c f029               	BEQ gra_on
   299  c15e 4c48b2                     JMP b_illquant	; parameter illegal
   300                          	
   301                          gra1			; &G 1
   302  c161 209dc1             	JSR gra_clear
   303                          
   304                          gra2
   305  c164 20f1b7                     JSR b_getcomma8bit
   306  c167 8a                         TXA		; foreground color
   307  c168 0a                         ASL		; upper nibble
   308  c169 0a                         ASL
   309  c16a 0a                         ASL
   310  c16b 0a                         ASL
   311  c16c 85fd                       STA gcol
   312  c16e 20f1b7                     JSR b_getcomma8bit
   313  c171 8a                         TXA		; background color
   314  c172 290f                       AND #$0F
   315  c174 05fd                       ORA gcol
   316  c176 a000                       LDY #$00
   317                          cram_loop
   318  c178 9900cc                     STA cram,Y	; fill color RAM
   319  c17b 9900cd                     STA cram+$100,Y
   320  c17e 9900ce                     STA cram+$200,Y
   321  c181 99e8ce                     STA cram+$300-24,Y
   322  c184 c8                         INY
   323  c185 d0f1                       BNE cram_loop
   324                          
   325                          gra_on
   326  c187 20bcc1             	JSR gra_setupcode
   327                          
   328  c18a a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   329  c18c 8d00dd                     STA cia_pra
   330  c18f a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   331  c191 8d18d0                     STA vic_mcr	; VIC memory control
   332  c194 ad11d0                     LDA vic_cr	; VIC control register
   333  c197 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   334  c199 8d11d0                     STA vic_cr
   335  c19c 60                         RTS
   336                          
   337                          gra_clear
   338  c19d a220                       LDX #$20	; Pages (8 KByte)
   339  c19f a9e0                       LDA #>gram
   340  c1a1 85fc                       STA gpos+1
   341  c1a3 a000                       LDY #$00
   342  c1a5 84fb                       STY gpos
   343  c1a7 98                         TYA
   344                          gra_fill
   345  c1a8 91fb                       STA (gpos),Y	; Loop unroll
   346  c1aa c8                         INY
   347  c1ab 91fb                       STA (gpos),Y
   348  c1ad c8                         INY
   349  c1ae 91fb                       STA (gpos),Y
   350  c1b0 c8                         INY
   351  c1b1 91fb                       STA (gpos),Y
   352  c1b3 c8                         INY
   353  c1b4 d0f2                       BNE gra_fill
   354  c1b6 e6fc                       INC gpos+1
   355  c1b8 ca                         DEX
   356  c1b9 d0ed                       BNE gra_fill
   357  c1bb 60                 	RTS
   358                          
   359                          gra_setupcode
   360  c1bc a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   361                          gra_copycode
   362  c1be bddfc1             	LDA gromcode-1,X
   363  c1c1 9dec03             	STA gramcode-1,X
   364  c1c4 ca                 	DEX
   365  c1c5 d0f7               	BNE gra_copycode
   366  c1c7 ad3703             	LDA savemo
   367  c1ca 290f               	AND #$0F
   368  c1cc aa                 	TAX
   369  c1cd 4ca1c5             	JMP setmode_enter	; re-apply mode to routines
   370                          				; implicit RTS
   371                          
   372                          ;-----------------------------------------------------------------
   373                          
   374                          gexit
   375  c1d0 a501                       LDA prozport
   376  c1d2 0902                       ORA #%00000010	; kernal ROM enable
   377  c1d4 8501                       STA prozport
   378  c1d6 58                         CLI		; allow interrupts
   379  c1d7 60                         RTS
   380                          
   381                          ;-----------------------------------------------------------------
   382                          
   383                          ginit
   384  c1d8 a501                       LDA prozport
   385  c1da 29fd                       AND #%11111101	; Kernal ROM disable
   386  c1dc 78                         SEI		; disable interrupts
   387  c1dd 8501                       STA prozport
   388  c1df 60                         RTS
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
   403  c1e0 b1a5                       LDA (gaddr),Y
   404                          gchange_op
   405  c1e2 1df7c0                     ORA bitmask,X
   406  c1e5 91a5                       STA (gaddr),Y
   407  c1e7 60                         RTS
   408                          
   409                          ; mask a graphic location 
   410                          
   411                          gmask
   412                          gmask_flip
   413  c1e8 4900                       EOR #$00
   414                          gmask_op
   415  c1ea 11a5                       ORA (gaddr),Y
   416  c1ec 91a5                       STA (gaddr),Y
   417  c1ee 60                         RTS
   418                          
   419                          }
   420                          
   421                          gromcode_end
   422                          
   423                          ;-----------------------------------------------------------------
   424                          
   425                          position
   426  c1ef a5aa                       LDA y
   427  c1f1 4a                         LSR
   428  c1f2 4a                         LSR
   429  c1f3 4a                         LSR		; y/8
   430  c1f4 a8                         TAY
   431  c1f5 2903                       AND #%00000011	; (y/8) mod 4
   432  c1f7 aa                         TAX
   433  c1f8 a59b                       LDA xl		; x low
   434  c1fa 29f8                       AND #%11111000	; clear bit 2-0
   435  c1fc 18                         CLC
   436  c1fd 7d07c1                     ADC ytabl,X	; addr low: y base + x part
   437  c200 85a5                       STA gaddr
   438  c202 a59c                       LDA xh		; addr high: x part
   439  c204 790bc1                     ADC ytabh,Y	; 	+ y base
   440  c207 85a6                       STA gaddr+1
   441  c209 a5aa                       LDA y		; vertical offset
   442  c20b 2907                       AND #%00000111	; y mod 8
   443  c20d a8                         TAY
   444  c20e a59b                       LDA xl
   445  c210 2907                       AND #%00000111	; x mod 8
   446  c212 aa                         TAX		; horizonal offset
   447  c213 60                         RTS		; (bitmask)
   448                          
   449                          
   450                          ;-----------------------------------------------------------------
   451                          
   452                          ; line y up, x right, dx < dy (case 1)
   453                          
   454                          line_up_steep
   455  c214 20efc1                     JSR position	; x,y
   456                          loop_yup_xright
   457  c217 20ed03                     JSR gchange	; pixel
   458                          
   459  c21a 18                         CLC		; k += dx
   460  c21b a595                       LDA kl
   461  c21d 65ab                       ADC dxl		; dxh is 0, because dx < dy
   462  c21f 8595                       STA kl
   463  c221 b004                       BCS ++		; k > 255
   464                          
   465  c223 c5a9                       CMP dy
   466  c225 9015                       BCC +		; k >= dy ->
   467                          
   468  c227 e5a9               ++	SBC dy		; k -= dy
   469  c229 8595                       STA kl
   470                          
   471  c22b e8                         INX		; x++
   472  c22c e008                       CPX #8
   473  c22e d00c                       BNE +
   474                          	; C=1
   475  c230 a200                       LDX #0		; x overflow, wrap around
   476  c232 a5a5                       LDA gaddr	; x+8: gaddr += 8
   477  c234 6907                       ADC #8-1	; C already set by CPX
   478  c236 85a5                       STA gaddr
   479  c238 9002                       BCC +
   480  c23a e6a6                       INC gaddr+1
   481                          
   482  c23c 88                 +	DEY		; y--
   483  c23d 100f                       BPL +++
   484  c23f 38                         SEC		; y overflow
   485  c240 a5a5                       LDA gaddr
   486  c242 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   487  c244 85a5                       STA gaddr
   488  c246 a5a6                       LDA gaddr+1
   489  c248 e901               	SBC #1
   490  c24a 85a6                       STA gaddr+1
   491  c24c a007                       LDY #7		; wrap around
   492                          
   493  c24e c6a3               +++	DEC cl		; until c=0
   494  c250 d0c5                       BNE loop_yup_xright
   495  c252 4cd0c1                     JMP gexit
   496                          
   497                          
   498                          ;-----------------------------------------------------------------
   499                          
   500                          ; line x right, y up, dx > dy (case 2)
   501                          
   502                          line_up_flat
   503  c255 20efc1                     JSR position	; x,y
   504  c258 a5a3               	LDA cl		; counter adjustment for
   505  c25a f002               	BEQ +		; dec-dec-counting
   506  c25c e6a4               	INC ch
   507                          +
   508                          loop_xright_yup
   509  c25e 20ed03                     JSR gchange	; pixel
   510                          
   511  c261 18                         CLC		; k += dy
   512  c262 a595                       LDA kl
   513  c264 65a9                       ADC dy
   514  c266 8595                       STA kl
   515  c268 9002                       BCC ++
   516  c26a e696                       INC kh
   517                          
   518  c26c c5ab               ++	CMP dxl		; k > dx?
   519  c26e a596                       LDA kh
   520  c270 e5a7                       SBC dxh
   521  c272 901a                       BCC +
   522                          
   523  c274 8596                       STA kh		; k -= dx
   524  c276 a595                       LDA kl
   525  c278 e5ab                       SBC dxl
   526  c27a 8595                       STA kl
   527                          
   528  c27c 88                         DEY		; y--
   529  c27d 100f                       BPL +
   530  c27f 38                 	SEC		; C=1 not always true (SBC above)
   531  c280 a5a5                       LDA gaddr	; y overflow
   532  c282 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   533  c284 85a5                       STA gaddr
   534  c286 a5a6                       LDA gaddr+1
   535  c288 e901               	SBC #1
   536  c28a 85a6                       STA gaddr+1
   537  c28c a007               	LDY #7		; wrap around
   538                          
   539  c28e e8                 +	INX		; x++
   540  c28f e008                       CPX #8		; x overflow?
   541  c291 d00c                       BNE ++
   542                          	; C=1
   543  c293 a200                       LDX #0		; wrap around
   544  c295 a5a5                       LDA gaddr	; x+8: gaddr += 8
   545  c297 6907                       ADC #8-1	; C already set by CPX
   546  c299 85a5                       STA gaddr
   547  c29b 9002                       BCC ++
   548  c29d e6a6                       INC gaddr+1
   549                          ++
   550  c29f c6a3               	DEC cl		; c--
   551  c2a1 d0bb                       BNE loop_xright_yup
   552  c2a3 c6a4                       DEC ch		; adjusted high which allows this
   553  c2a5 d0b7                       BNE loop_xright_yup
   554                          
   555  c2a7 4cd0c1                     JMP gexit
   556                          
   557                          
   558                          
   559                          ;-----------------------------------------------------------------
   560                          
   561                          ; line x right, y down, dx > dy (case 3)
   562                          
   563                          line_down_flat
   564  c2aa 20efc1                     JSR position	; x,y
   565  c2ad a5a3               	LDA cl		; counter adjustment for
   566  c2af f002               	BEQ +		; dec-dec-counting
   567  c2b1 e6a4               	INC ch
   568                          +
   569                          loop_xright_ydown
   570  c2b3 20ed03                     JSR gchange	; pixel
   571                          
   572  c2b6 18                         CLC		; k += dy
   573  c2b7 a595                       LDA kl
   574  c2b9 65a9                       ADC dy
   575  c2bb 8595                       STA kl
   576  c2bd 9002                       BCC ++
   577  c2bf e696                       INC kh
   578                          
   579  c2c1 c5ab               ++	CMP dxl		; k > dx
   580  c2c3 a596                       LDA kh
   581  c2c5 e5a7                       SBC dxh		; k -= dx
   582  c2c7 901b                       BCC +
   583                          
   584  c2c9 8596                       STA kh
   585  c2cb a595                       LDA kl
   586  c2cd e5ab                       SBC dxl
   587  c2cf 8595                       STA kl
   588                          
   589  c2d1 c8                         INY		; y++
   590  c2d2 c008                       CPY #8
   591  c2d4 d00e                       BNE +
   592                          	; C=1
   593  c2d6 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   594  c2d8 693f                       ADC #$40-1	; C already set by CPY
   595  c2da 85a5                       STA gaddr
   596  c2dc a5a6                       LDA gaddr+1
   597  c2de 6901               	ADC #1
   598  c2e0 85a6                       STA gaddr+1
   599  c2e2 a000                       LDY #0		; wrap around
   600                          
   601  c2e4 e8                 +	INX		; x++
   602  c2e5 e008                       CPX #8		; x overflow ?
   603  c2e7 d00c                       BNE +++
   604                          	; C=1
   605  c2e9 a200                       LDX #$00	; wrap around
   606  c2eb a5a5                       LDA gaddr	; gaddr += 8
   607  c2ed 6907                       ADC #$08-1	; C always set by CPX
   608  c2ef 85a5                       STA gaddr
   609  c2f1 9002                       BCC +++
   610  c2f3 e6a6                       INC gaddr+1
   611                          +++
   612  c2f5 c6a3               	DEC cl		; c--
   613  c2f7 d0ba                       BNE loop_xright_ydown
   614  c2f9 c6a4                       DEC ch		; adjusted high which allows this
   615  c2fb d0b6                       BNE loop_xright_ydown
   616                          
   617  c2fd 4cd0c1                     JMP gexit
   618                          
   619                          
   620                          ;-----------------------------------------------------------------
   621                          
   622                          ; line y down, x right, dx < dy (case 4)
   623                          
   624                          line_down_steep
   625  c300 20efc1                     JSR position	; x,y
   626                          loop_ydown_xright
   627  c303 20ed03                     JSR gchange	; pixel
   628                          
   629  c306 18                         CLC		; k += dx
   630  c307 a595                       LDA kl
   631  c309 65ab                       ADC dxl		; dxh is 0, because dx < dy
   632  c30b 8595                       STA kl
   633  c30d b004                       BCS ++
   634  c30f c5a9                       CMP dy		; k > dy?
   635  c311 9015                       BCC +
   636  c313 e5a9               ++	SBC dy		; k -= dy
   637  c315 8595                       STA kl
   638                          
   639  c317 e8                         INX		; x++
   640  c318 e008                       CPX #8
   641  c31a d00c                       BNE +		; x overflow?
   642  c31c a200                       LDX #0		; wrap around
   643  c31e a5a5                       LDA gaddr	; x+9: gaddr += 8
   644  c320 6907                       ADC #8-1	; C already set by CPX
   645  c322 85a5                       STA gaddr
   646  c324 9002                       BCC +
   647  c326 e6a6                       INC gaddr+1
   648                          
   649  c328 c8                 +	INY		; y++
   650  c329 c008                       CPY #8		; y overflow?
   651  c32b d00e                       BNE +++
   652  c32d a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   653  c32f 693f                       ADC #$40-1	; C already set by CPY
   654  c331 85a5                       STA gaddr
   655  c333 a5a6                       LDA gaddr+1
   656  c335 6901               	ADC #1
   657  c337 85a6                       STA gaddr+1
   658  c339 a000                       LDY #0		; wrap around
   659                          
   660  c33b c6a3               +++	DEC cl		; c--
   661                          			; until c=0
   662  c33d d0c4                       BNE loop_ydown_xright
   663  c33f 4cd0c1                     JMP gexit
   664                          
   665                          
   666                          ;-----------------------------------------------------------------
   667                          
   668                          getcommaxy
   669  c342 20fdae                     JSR b_getcomma	; check ","
   670                          getxy
   671  c345 208aad                     JSR b_getval	; get X coord. value
   672  c348 20f7b7                     JSR b_convint
   673  c34b c901                       CMP #>xmax
   674  c34d 9009               	BCC gcxy_xok
   675  c34f f003                       BEQ +		; X = $1xx
   676                          error_iq
   677  c351 2069c5                     JSR range_error
   678  c354 c040               +	CPY #<xmax	; check X low
   679  c356 b0f9                       BCS error_iq	; X to big
   680                          gcxy_xok
   681  c358 84fb                       STY gpos	; temporary save X coord.
   682  c35a 85fc                       STA gpos+1
   683                          
   684  c35c 20f1b7                     JSR b_getcomma8bit
   685                          			; get Y coord. value
   686  c35f e0c8                       CPX #ymax
   687  c361 b0ee                       BCS error_iq	; Y to big
   688                          
   689  c363 a4fb                       LDY gpos	; restory X coord.
   690  c365 a5fc                       LDA gpos+1
   691  c367 60                         RTS
   692                          
   693                          
   694                          ;-----------------------------------------------------------------
   695                          
   696                          hline
   697  c368 2045c3                     JSR getxy	; get startpoint
   698  c36b 86aa                       STX y
   699  c36d 8e3603                     STX savey	; save as cursor, too
   700  c370 859c                       STA xh
   701  c372 849b                       STY xl
   702  c374 20fdae                     JSR b_getcomma	; get length
   703  c377 208aad                     JSR b_getval
   704  c37a 20f7b7                     JSR b_convint
   705                          
   706  c37d c901                       CMP #>xmax
   707  c37f 9006                       BCC +		; X < 256
   708  c381 d0ce                       BNE error_iq
   709  c383 c040                       CPY #<xmax
   710  c385 b0ca                       BCS error_iq
   711                          +
   712                          			; calculate end point
   713  c387 aa                         TAX		; save length high byte
   714  c388 98                         TYA		; length low byte
   715  c389 18                         CLC
   716  c38a 659b                       ADC xl		; low xend = x+length
   717  c38c 859e                       STA xendl
   718  c38e a8                 	TAY
   719  c38f 8a                         TXA		; high
   720  c390 659c                       ADC xh		; high xend = x+length
   721  c392 859f                       STA xendh
   722  c394 aa                 	TAX
   723                          
   724  c395 c901               	CMP #>xmax	; endpoint outside?
   725  c397 9005               	BCC +
   726  c399 98                 	TYA
   727  c39a e940               	SBC #<xmax
   728  c39c b0b3               	BCS error_iq
   729                          +
   730  c39e 8e3503                     STX savexh
   731  c3a1 8c3403                     STY savexl	; also save as cursor
   732                          
   733  c3a4 20d8c1                     JSR ginit	; map in graphic memory
   734                          
   735                          hline_start
   736  c3a7 a59e                       LDA xendl
   737  c3a9 c59b                       CMP xl
   738  c3ab a59f                       LDA xendh
   739  c3ad e59c                       SBC xh
   740  c3af b013                       BCS hl_noxswap	; xend < x ->
   741                          
   742  c3b1 a69e                       LDX xendl	; swap x, xend
   743  c3b3 a59b                       LDA xl
   744  c3b5 869b                       STX xl
   745  c3b7 859e                       STA xendl
   746                          
   747  c3b9 a69f                       LDX xendh
   748  c3bb a49c                       LDY xh
   749  c3bd 849f                       STY xendh
   750  c3bf 869c                       STX xh
   751  c3c1 4cd3c3                     JMP hl_start	; x != xend
   752                          
   753                          hl_noxswap
   754  c3c4 a59e                       LDA xendl
   755  c3c6 c59b                       CMP xl
   756  c3c8 d009                       BNE hl_start
   757  c3ca a59f                       LDA xendh
   758  c3cc c59c                       CMP xh
   759  c3ce d003                       BNE hl_start	; x = xend ->
   760  c3d0 4c47c5             	JMP plot_start	; single point
   761                          ;	JMP gexit	; no point
   762                          
   763                          hl_start
   764  c3d3 20efc1                     JSR position	; graphic position x,y
   765  c3d6 bd24c1                     LDA maskleft,X
   766  c3d9 48                         PHA		; save left end mask
   767  c3da a59e                       LDA xendl
   768  c3dc 2907                       AND #%00000111
   769  c3de 8596                       STA tmp2	; xend mod 8, mask index
   770  c3e0 a59b                       LDA xl
   771  c3e2 29f8                       AND #%11111000	; (xl div 8)*8
   772  c3e4 8595                       STA tmp1
   773  c3e6 a59e                       LDA xendl	; xend unmasked
   774  c3e8 38                         SEC
   775  c3e9 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   776  c3eb 8595                       STA tmp1
   777  c3ed a59f                       LDA xendh
   778  c3ef e59c                       SBC xh
   779  c3f1 4a                         LSR		; / 8 ->  0-39
   780  c3f2 a595                       LDA tmp1	; only 1 highest bit
   781  c3f4 6a                         ROR		; and 3 lower bits
   782  c3f5 4a                         LSR
   783  c3f6 4a                         LSR
   784  c3f7 aa                         TAX		; 8-pixel-blocks count
   785  c3f8 68                         PLA		; left end x mask
   786                          
   787                          hl_nextblock
   788  c3f9 ca                         DEX
   789                          hl_islastblock
   790  c3fa 3012                       BMI hl_lastblock
   791                          			; leave loop if X<0
   792  c3fc 20f503                     JSR gmask	; first with left end mask
   793  c3ff 18                         CLC		; gaddr += 8
   794  c400 a5a5                       LDA gaddr
   795  c402 6908                       ADC #8
   796  c404 85a5                       STA gaddr
   797  c406 9002                       BCC +
   798  c408 e6a6                       INC gaddr+1
   799  c40a a9ff               +	LDA #$FF	; following with full 8-pixel mask
   800  c40c d0eb               	BNE hl_nextblock	; always
   801                          
   802                          hl_lastblock
   803  c40e a696                       LDX tmp2	; xend mask index
   804  c410 3d2cc1                     AND maskright,X ; mask right end
   805  c413 20f503                     JSR gmask	; modify
   806  c416 4cd0c1                     JMP gexit	; leave
   807                          
   808                          
   809                          ;-----------------------------------------------------------------
   810                          
   811                          vline
   812  c419 2045c3                     JSR getxy	; get startpoint
   813  c41c 859c                       STA xh
   814  c41e 8d3503                     STA savexh	; save as cursor too
   815  c421 849b                       STY xl
   816  c423 8c3403                     STY savexl
   817  c426 86aa                       STX y
   818                          
   819  c428 20f1b7                     JSR b_getcomma8bit
   820                          			; get length
   821  c42b 18                         CLC		; calculate end point
   822  c42c 8a                         TXA		; length
   823                          ; DON'T-CHANGE: how long to go vertically (needed later)
   824                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   825                          ;	STA tmp1
   826  c42d 65aa                       ADC y		; length + y
   827  c42f c9c8                       CMP #ymax
   828  c431 9003                       BCC +
   829                          vline_iq
   830  c433 2069c5                     JSR range_error
   831  c436 8593               +	STA yend	; endpoint
   832  c438 c9c8               	CMP #ymax	; outside?
   833  c43a b0f7               	BCS vline_iq
   834                          
   835  c43c 8d3603             	STA savey	; set cursor y position
   836                          
   837  c43f 20d8c1                     JSR ginit	; map in graphic memory
   838                          
   839                          vline_start
   840  c442 a593                       LDA yend
   841  c444 c5aa                       CMP y
   842  c446 b00a                       BCS vl_noyswap	; yend < y ->
   843  c448 a5aa                       LDA y		; swap y, yend
   844  c44a a693                       LDX yend
   845  c44c 8593                       STA yend
   846  c44e 86aa                       STX y
   847  c450 f005               	BEQ vl_start	; always (with next branch)
   848                          	; fall through if yend is
   849                          vl_noyswap
   850  c452 d003                       BNE vl_start	; y = yend ->
   851  c454 4c47c5             	JMP plot_start	; single point
   852                          ;	JMP gexit	; no point
   853                          
   854                          vl_start
   855  c457 20efc1                     JSR position	; graphic position x,y
   856  c45a bdf7c0                     LDA bitmask,X
   857  c45d 8596                       STA tmp2	; save mask
   858                          ; DON'T-CHANGE: replace ...
   859  c45f 38                         SEC
   860  c460 a593                       LDA yend
   861  c462 e5aa                       SBC y		; vertical length
   862  c464 aa                         TAX
   863                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   864                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   865                          ;	LDX tmp1
   866  c465 e8                         INX		; +1 (exit on 0)
   867                          vl_nextline
   868  c466 a596                       LDA tmp2
   869  c468 20f503                     JSR gmask	; modify 
   870  c46b c8                         INY		; go down
   871  c46c c008                       CPY #8		; 8-line wrap
   872  c46e d00e                       BNE +
   873  c470 a5a5                       LDA gaddr	; gaddr += 320
   874  c472 693f               	ADC #$40-1	; compensate for C = 1
   875  c474 85a5                       STA gaddr
   876  c476 a5a6                       LDA gaddr+1
   877  c478 6901                       ADC #$01
   878  c47a 85a6                       STA gaddr+1
   879  c47c a000                       LDY #0		; wrap y offset
   880  c47e ca                 +	DEX		; all vertical positions done?
   881  c47f d0e5                       BNE vl_nextline
   882  c481 4cd0c1                     JMP gexit	; leave
   883                          
   884                          
   885                          ;-----------------------------------------------------------------
   886                          
   887                          line
   888  c484 2045c3                     JSR getxy	; get startpoint
   889  c487 849b                       STY xl 
   890  c489 859c                       STA xh
   891  c48b 86aa                       STX y
   892                          
   893  c48d 2042c3                     JSR getcommaxy	; get endpoint
   894                          line_start
   895  c490 8c3403                     STY savexl	; save as cursor position too
   896  c493 849e                       STY xendl
   897  c495 8d3503                     STA savexh
   898  c498 859f                       STA xendh
   899  c49a 8e3603                     STX savey
   900  c49d 8693                       STX yend
   901                          
   902  c49f 20d8c1                     JSR ginit	; map in graphic memory
   903                          
   904  c4a2 a000                       LDY #$00	; initialize to 0
   905  c4a4 84a8                       STY ydir
   906  c4a6 8495                       STY kl
   907  c4a8 8496                       STY kh
   908                          
   909  c4aa 38                         SEC
   910  c4ab a59e                       LDA xendl	; calculate dx
   911  c4ad e59b                       SBC xl
   912  c4af 85ab                       STA dxl
   913  c4b1 a59f                       LDA xendh
   914  c4b3 e59c                       SBC xh
   915  c4b5 85a7                       STA dxh
   916                          
   917  c4b7 b025                       BCS li_xend_right
   918                          	; dx != 0
   919  c4b9 98                         TYA		; negate dx
   920  c4ba 38                         SEC		; dx = 0 - dx
   921  c4bb e5ab                       SBC dxl
   922  c4bd 85ab                       STA dxl
   923  c4bf 98                         TYA
   924  c4c0 e5a7                       SBC dxh
   925  c4c2 85a7                       STA dxh
   926                          			; C=0 always, needed later
   927  c4c4 a69b                       LDX xl		; swap x low
   928  c4c6 a49e                       LDY xendl
   929  c4c8 869e                       STX xendl
   930  c4ca 849b                       STY xl
   931                          
   932  c4cc a69c                       LDX xh		; swap x high
   933  c4ce a49f                       LDY xendh
   934  c4d0 869f                       STX xendh
   935  c4d2 849c                       STY xh
   936                          
   937  c4d4 a6aa                       LDX y		; swap y
   938  c4d6 a493                       LDY yend
   939  c4d8 8693                       STX yend
   940  c4da 84aa                       STY y
   941                          
   942  c4dc 9009                       BCC li_x_different
   943                          			; C=0 always (from negation before)
   944                          
   945                          li_xend_right
   946  c4de a5ab                       LDA dxl		; dx = 0?
   947  c4e0 05a7                       ORA dxh
   948  c4e2 d003                       BNE li_x_different
   949  c4e4 4c42c4                     JMP vline_start	; vertical line case
   950                          
   951                          li_x_different
   952  c4e7 38                         SEC		; calculate dy
   953  c4e8 a593                       LDA yend
   954  c4ea e5aa                       SBC y
   955  c4ec b006                       BCS li_y_right
   956  c4ee 49ff                       EOR #$FF	; negate dy (two's complement)
   957  c4f0 6901                       ADC #$01	; C=0
   958  c4f2 85a8                       STA ydir	; flag y goes up
   959                          
   960                          li_y_right
   961  c4f4 85a9                       STA dy
   962  c4f6 d003                       BNE +
   963  c4f8 4ca7c3                     JMP hline_start	; horizontal line case
   964                          +
   965                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
   966                          
   967  c4fb a5a7                       LDA dxh		; dx > dy
   968  c4fd d017                       BNE line_flat	; yes -> flat
   969  c4ff a5a9                       LDA dy		; no -> steep
   970  c501 aa                         TAX
   971  c502 c5ab                       CMP dxl
   972  c504 9010                       BCC line_flat
   973                          
   974                          line_steep
   975  c506 e8                         INX	
   976  c507 86a3                       STX cl		; c = dy+1
   977  c509 4a                         LSR		; k = dy/2
   978  c50a 8595                       STA kl
   979  c50c a5a8                       LDA ydir
   980  c50e d003                       BNE +
   981  c510 4c00c3                     JMP line_down_steep	; y down, steep
   982  c513 4c14c2             +	JMP line_up_steep	; y up, steep
   983                          
   984                          line_flat
   985  c516 a5a7                       LDA dxh
   986  c518 a8                         TAY
   987  c519 a6ab                       LDX dxl
   988  c51b e8                         INX
   989  c51c d001                       BNE +
   990  c51e c8                         INY
   991  c51f 86a3               +	STX cl		; c = dx+1
   992  c521 84a4                       STY ch
   993                          
   994  c523 4a                         LSR		; k = dx/2
   995  c524 8596                       STA kh
   996  c526 a5ab                       LDA dxl
   997  c528 6a                         ROR		; dx/2
   998  c529 8595                       STA kl
   999  c52b a5a8                       LDA ydir	
  1000  c52d d003                       BNE +
  1001  c52f 4caac2                     JMP line_down_flat	; y down, flat
  1002  c532 4c55c2             +	JMP line_up_flat	; y up, flat
  1003                          
  1004                          ;-----------------------------------------------------------------
  1005                          
  1006                          plot
  1007  c535 2045c3                     JSR getxy	; get parameter
  1008  c538 859c                       STA xh		; save x/y
  1009  c53a 849b                       STY xl
  1010  c53c 86aa                       STX y
  1011  c53e 8d3503                     STA savexh	; and store as cursor
  1012  c541 8c3403                     STY savexl
  1013  c544 8e3603                     STX savey
  1014                          
  1015                          plot_start
  1016  c547 20efc1                     JSR position	; calculate graphical address
  1017                          
  1018  c54a a501                       LDA prozport
  1019  c54c 29fd                       AND #%11111101	; Kernal ROM disable
  1020  c54e 78                         SEI			
  1021  c54f 8501                       STA prozport
  1022                          
  1023  c551 20ed03                     JSR gchange	; change graphical data
  1024                          
  1025  c554 a501                       LDA prozport
  1026  c556 0902                       ORA #%00000010	; kernal ROM enable
  1027  c558 8501                       STA prozport
  1028  c55a 58                         CLI
  1029  c55b 60                         RTS
  1030                          
  1031                          ;-----------------------------------------------------------------
  1032                          
  1033                          move
  1034  c55c 2045c3                     JSR getxy	; get parameter
  1035  c55f 8d3503                     STA savexh	; just save as cursor
  1036  c562 8c3403                     STY savexl
  1037  c565 8e3603                     STX savey
  1038  c568 60                         RTS
  1039                          
  1040                          
  1041                          ;-----------------------------------------------------------------
  1042                          
  1043                          range_error
  1044  c569 ad3703             	LDA savemo
  1045  c56c 29f0               	AND #$F0
  1046  c56e d003               	BNE +
  1047  c570 68                 	PLA			; cleanup JSR
  1048  c571 68                 	PLA
  1049  c572 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1050                          				; error mode 2: cut value: control back
  1051                          				; to handle value correction
  1052  c573 2920               +	AND #$20
  1053  c575 d0fb               	BNE -
  1054  c577 68                 	PLA			; cleanup JSR
  1055  c578 68                 	PLA
  1056                          setmode_error
  1057  c579 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1058                          
  1059                          ;-----------------------------------------------------------------
  1060                          
  1061                          setmode
  1062  c57c 209eb7                     JSR b_get8bit
  1063  c57f e003                       CPX #3
  1064  c581 9012                       BCC +			; less then 3, modification mode
  1065  c583 e006               	CPX #6
  1066  c585 b0f2               	BCS setmode_error	; out of range
  1067                          				; error mode
  1068  c587 690d               	ADC #13			; C=0, therefore -3
  1069                          				; 3-5 -> 16-18
  1070                          				; put A's bit 4-7 into savemo
  1071  c589 4d3703             	EOR savemo		; ********
  1072  c58c 29f0               	AND #$F0		; ****0000
  1073  c58e 4d3703             	EOR savemo		; AAAAmmmm
  1074  c591 8d3703             	STA savemo		; 
  1075  c594 60                 	RTS
  1076                          
  1077  c595 8a                 +	TXA
  1078  c596 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1079  c599 290f               	AND #$0F
  1080  c59b 4d3703             	EOR savemo
  1081  c59e 8d3703             	STA savemo
  1082                          setmode_enter
  1083  c5a1 e001               	CPX #$01
  1084  c5a3 b01a                       BCS set_or_toggle
  1085                          
  1086                          modereset
  1087  c5a5 a9c0                       LDA #>(nbitmask)
  1088  c5a7 8df103                     STA gchange_op+2
  1089  c5aa a9ff                       LDA #<(nbitmask)
  1090  c5ac 8df003                     STA gchange_op+1
  1091  c5af a93d                       LDA #$3D		; AND abs,X
  1092  c5b1 8def03                     STA gchange_op
  1093  c5b4 a931                       LDA #$31		; AND (zp),Y
  1094  c5b6 8df703                     STA gmask_op
  1095  c5b9 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1096  c5bb 8df603                     STA gmask_flip+1
  1097  c5be 60                         RTS
  1098                          
  1099                          set_or_toggle
  1100  c5bf d01a                       BNE modetoggle
  1101                          modeset
  1102  c5c1 a9c0                       LDA #>(bitmask)
  1103  c5c3 8df103                     STA gchange_op+2
  1104  c5c6 a9f7                       LDA #<(bitmask)
  1105  c5c8 8df003                     STA gchange_op+1
  1106  c5cb a91d                       LDA #$1D		; OR abs,X
  1107  c5cd 8def03                     STA gchange_op
  1108  c5d0 a911                       LDA #$11		; OR (zp),Y
  1109  c5d2 8df703                     STA gmask_op
  1110  c5d5 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1111  c5d7 8df603                     STA gmask_flip+1
  1112  c5da 60                         RTS
  1113                          
  1114                          modetoggle
  1115  c5db a9c0                       LDA #>(bitmask)
  1116  c5dd 8df103                     STA gchange_op+2
  1117  c5e0 a9f7                       LDA #<(bitmask)
  1118  c5e2 8df003                     STA gchange_op+1
  1119  c5e5 a95d                       LDA #$5D		; EOR abs,X
  1120  c5e7 8def03                     STA gchange_op
  1121  c5ea a951                       LDA #$51		; EOR (zp),Y
  1122  c5ec 8df703                     STA gmask_op
  1123  c5ef a900                       LDA #$00		; EOR #$00, nicht invertieren
  1124  c5f1 8df603                     STA gmask_flip+1
  1125  c5f4 60                         RTS
  1126                          
  1127                          
  1128                          ;-----------------------------------------------------------------
  1129                          
  1130                          ; get pixel (check if pixel set)
  1131                          ; not used
  1132                          
  1133                          get
  1134  c5f5 2042c3                     JSR getcommaxy
  1135  c5f8 859c                       STA xh
  1136  c5fa 849b                       STY xl
  1137  c5fc 86aa                       STX y
  1138                          
  1139  c5fe 20efc1                     JSR position
  1140                          
  1141  c601 a501                       LDA prozport
  1142  c603 29fd               	AND #%11111101	; Kernal ROM disable
  1143  c605 78                         SEI
  1144  c606 8501                       STA prozport
  1145                          
  1146  c608 b1a5                       LDA (gaddr),Y
  1147  c60a 3df7c0                     AND bitmask,X
  1148  c60d a8                         TAY
  1149  c60e a501                       LDA prozport
  1150  c610 0902               	ORA #%00000010	; kernal ROM enable
  1151  c612 8501                       STA prozport
  1152  c614 58                         CLI
  1153  c615 4ca2b3                     JMP b_byte2fac
  1154                          
  1155                          
  1156                          ;-----------------------------------------------------------------
  1157                          
  1158                          relto
  1159  c618 208aad                     JSR b_getval	; get X offset (+/-)
  1160  c61b a561               	LDA facexp	; FAC exponent
  1161  c61d c990               	CMP #$90	; more than 16 bit
  1162  c61f b031               	BCS relto_error	; illegal quantity
  1163  c621 209bbc                     JSR b_fac2int	; to signed integer
  1164                          
  1165  c624 18                         CLC
  1166  c625 a565                       LDA facintl
  1167  c627 6d3403                     ADC savexl
  1168  c62a 859e                       STA xendl
  1169  c62c a564                       LDA facinth
  1170  c62e 6d3503                     ADC savexh
  1171  c631 859f                       STA xendh	; xend = savex+facint
  1172                          
  1173  c633 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1174  c636 208aad                     JSR b_getval
  1175  c639 a561                       LDA facexp	; FAC exponent
  1176  c63b c990                       CMP #$90	; more than 16 bit
  1177  c63d b013                       BCS relto_error	; illegal quantity
  1178  c63f 209bbc                     JSR b_fac2int	; to signed integer
  1179  c642 18                         CLC
  1180  c643 a565                       LDA facintl
  1181  c645 6d3603                     ADC savey
  1182  c648 8593                       STA yend	; yend = savey+facint
  1183                          
  1184  c64a a59f                       LDA xendh	; check end coord. x
  1185  c64c c901                       CMP #>xmax
  1186  c64e 900b                       BCC rt_xok
  1187  c650 f003                       BEQ +
  1188                          relto_error
  1189  c652 2069c5                     JSR range_error
  1190  c655 a59e               +	LDA xendl
  1191  c657 c940                       CMP #<xmax
  1192  c659 b0f7                       BCS relto_error
  1193                          rt_xok
  1194  c65b a593                       LDA yend	; check end coord. y
  1195  c65d c9c8                       CMP #ymax
  1196  c65f b0f1                       BCS relto_error
  1197                          
  1198  c661 ad3403                     LDA savexl
  1199  c664 859b                       STA xl
  1200  c666 ad3503                     LDA savexh
  1201  c669 859c                       STA xh
  1202  c66b ad3603                     LDA savey
  1203  c66e 85aa                       STA y
  1204  c670 a49e                       LDY xendl
  1205  c672 a59f                       LDA xendh
  1206  c674 a693                       LDX yend	; xend/yend = cursor + x/y
  1207                          
  1208  c676 4c90c4                     JMP line_start	; draw line x/y to xend/yend
  1209                          
  1210                          
  1211                          ;-----------------------------------------------------------------
  1212                          
  1213                          char
  1214  c679 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1215  c67c e028                       CPX #40	
  1216  c67e 9003                       BCC +
  1217                          char_error
  1218  c680 4c48b2                     JMP b_illquant
  1219  c683 86fb               +	STX gpos	; save x coord.
  1220  c685 20f1b7                     JSR b_getcomma8bit
  1221                          			; get char. position y 0-24
  1222  c688 e019                       CPX #25
  1223  c68a b0f4                       BCS char_error
  1224  c68c 86fc                       STX gpos+1	; save y coord.
  1225                          
  1226  c68e 20fdae                     JSR b_getcomma	; get string
  1227  c691 209ead                     JSR b_getexpr
  1228  c694 20a3b6                     JSR b_stringval ; string address in str
  1229  c697 48                         PHA		; string length
  1230  c698 a6fc                       LDX gpos+1	; y coord. for char. position
  1231  c69a 8a                         TXA
  1232  c69b 2903                       AND #$03	; mask 2 bits
  1233  c69d a8                         TAY		; table index
  1234  c69e a900                       LDA #$00
  1235  c6a0 85fc                       STA gpos+1	; x high
  1236  c6a2 a5fb                       LDA gpos	; saved x: multiply by 8
  1237  c6a4 0a                         ASL
  1238  c6a5 0a                         ASL
  1239  c6a6 0a                         ASL
  1240  c6a7 26fc                       ROL gpos+1	; overflow to high byte
  1241  c6a9 7907c1                     ADC ytabl,Y
  1242  c6ac 85a5                       STA gaddr
  1243  c6ae a5fc                       LDA gpos+1	; x high
  1244  c6b0 7d0bc1                     ADC ytabh,X
  1245  c6b3 85a6                       STA gaddr+1
  1246  c6b5 68                         PLA		; string length
  1247  c6b6 a000                       LDY #$00	; string index
  1248  c6b8 aa                         TAX		; length
  1249  c6b9 e8                         INX		; prepare as counter
  1250                          char_loop
  1251  c6ba ca                         DEX
  1252  c6bb f008                       BEQ char_exit
  1253  c6bd b122                       LDA (str),Y	; read string
  1254  c6bf 20c6c6                     JSR char_display
  1255  c6c2 c8                         INY
  1256  c6c3 d0f5                       BNE char_loop
  1257                          char_exit
  1258  c6c5 60                         RTS
  1259                          
  1260                          char_display
  1261  c6c6 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1262  c6c8 8a                         TXA		; save register X+Y
  1263  c6c9 48                         PHA
  1264  c6ca 98                         TYA
  1265  c6cb 48                         PHA
  1266  c6cc a5d7                       LDA z_tmp	; get saved character
  1267  c6ce 1049                       BPL char_normal
  1268                          
  1269                          char_inverse
  1270  c6d0 297f                       AND #%01111111	; mask bit 7
  1271  c6d2 c97f                       CMP #%01111111	; was 255? (pi)
  1272  c6d4 d002                       BNE +
  1273  c6d6 a95e                       LDA #$5E	; screen code for pi
  1274  c6d8 c920               +	CMP #$20	; control character?
  1275  c6da 9038                       BCC char_disp_leave
  1276                          			; yes, skip
  1277  c6dc 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1278                          			; $C0-$FF -> $40-$7F
  1279                          			; OPT: BNE char_hires
  1280                          			; OPT: char_normal
  1281                          char_hires
  1282  c6de a6c7                       LDX z_reverseflag
  1283  c6e0 f002                       BEQ +
  1284  c6e2 0980                       ORA #%10000000	; invert char.
  1285  c6e4 48                 +	PHA		; save char. for later
  1286  c6e5 78                         SEI
  1287  c6e6 a921                       LDA #$21	; char. rom, no basic and kernal rom
  1288  c6e8 8501                       STA prozport	; char. rom base = $D000
  1289  c6ea a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1290  c6ec 85fc                       STA gpos+1	; 
  1291  c6ee 68                         PLA		; char. code
  1292  c6ef 0a                         ASL		; *8
  1293  c6f0 26fc                       ROL gpos+1
  1294  c6f2 0a                         ASL
  1295  c6f3 26fc                       ROL gpos+1
  1296  c6f5 0a                         ASL
  1297  c6f6 26fc                       ROL gpos+1
  1298  c6f8 85fb                       STA gpos	; addr. in char. rom for char.
  1299                          
  1300  c6fa a007                       LDY #$07	; 8 hires lines
  1301                          char_line
  1302  c6fc b1fb                       LDA (gpos),Y	; read character line
  1303  c6fe 20f503                     JSR gmask	; write to hires screen
  1304  c701 88                         DEY
  1305  c702 10f8                       BPL char_line
  1306                          
  1307  c704 a936                       LDA #$36	; no char. rom, basic ram, kernal rom
  1308  c706 8501                       STA prozport
  1309  c708 58                         CLI
  1310                          
  1311  c709 18                         CLC		; step char position to left
  1312  c70a a5a5                       LDA gaddr	; ( +8 )
  1313  c70c 6908                       ADC #$08
  1314  c70e 85a5                       STA gaddr
  1315  c710 9002                       BCC +
  1316  c712 e6a6                       INC gaddr+1
  1317                          +
  1318                          char_disp_leave
  1319  c714 68                 	PLA		; pass written character back
  1320  c715 a8                         TAY		; restore saved registers
  1321  c716 68                         PLA
  1322  c717 aa                         TAX
  1323  c718 60                         RTS
  1324                          
  1325                          char_normal
  1326  c719 c920                       CMP #$20	; control character?
  1327  c71b 90f7                       BCC char_disp_leave
  1328  c71d c960                       CMP #$60
  1329  c71f 9004                       BCC +
  1330  c721 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1331  c723 d002                       BNE ++
  1332  c725 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1333  c727 4cdec6             ++	JMP char_hires	; 		OPT: Bxx
  1334                          
  1335                          
  1336                          ;-----------------------------------------------------------------
  1337                          
  1338                          to
  1339  c72a ad3403                     LDA savexl
  1340  c72d 859b                       STA xl
  1341  c72f ad3503                     LDA savexh
  1342  c732 859c                       STA xh
  1343  c734 ad3603                     LDA savey
  1344  c737 85aa                       STA y
  1345  c739 2045c3                     JSR getxy
  1346  c73c 4c90c4                     JMP line_start
  1347                          
  1348                          ;-----------------------------------------------------------------
  1349                          
  1350                          unnew
  1351                          
  1352  c73f a52b               	lda bassta
  1353  c741 8522               	sta str
  1354  c743 a52c               	lda bassta+1
  1355  c745 8523               	sta str+1
  1356  c747 a001               	ldy #1
  1357  c749 98                 	tya
  1358  c74a 9122               	sta (str),y		; != 0
  1359                          
  1360  c74c 2033a5             	jsr b_rechain		; starting from bassta
  1361                          				; result in (str)
  1362  c74f 18                 	clc			; str+1 -> new basic end
  1363  c750 a423               	ldy str+1
  1364  c752 a522               	lda str
  1365  c754 6902               	adc #2
  1366  c756 852d               	sta basend
  1367  c758 9001               	bcc +
  1368  c75a c8                 	iny
  1369  c75b 842e               +	sty basend+1
  1370  c75d 4c60a6             	jmp b_clr		; perform CLR
  1371                          
  1372                          ;-----------------------------------------------------------------
  1373                          graext_end

; ******** Source: graext.asm
    37                          
    38                          
