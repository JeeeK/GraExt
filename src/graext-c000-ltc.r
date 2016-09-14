
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
    21  c003 a993                       lda #<(parse)		; check if basic interpreter parser hook
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
    36  c01b a91c                       lda #<author            ; message ...
    37  c01d a0c1                       ldy #>author
    38  c01f 201eab                     jsr $ab1e               ; output string 
    39                          
    40  c022 4c86e3                     jmp $e386               ; BASIC warm start
    41                          

; ******** Source: graext-core.asm
     1                          ;  **** gra-ext core ****
     2                          ;
     3                          ; 2015-10-05 johann e. klasek, johann at klasek at
     4                          ;
     5                          !macro version {
     6                          	!text "1.26" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2016-07-09 v 1.26
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
   187  c025 a993                       LDA #<(parse)	; basic interpreter parser hook
   188  c027 8d0803                     STA v_bascmd	; for commands
   189  c02a a9c0                       LDA #>(parse)
   190  c02c 8d0903                     STA v_bascmd+1
   191                          
   192  c02f ad0a03                     LDA v_basexp	; basic interpreter parser hook
   193  c032 8d3803             	STA savevexp	; for expressions
   194  c035 a9c7                       LDA #<(express) ; with save of old pointer
   195  c037 8d0a03                     STA v_basexp
   196  c03a ad0b03                     LDA v_basexp+1
   197  c03d 8d3903             	STA savevexp+1
   198  c040 a9c0                       LDA #>(express)
   199  c042 8d0b03                     STA v_basexp+1
   200                          
   201  c045 ad2803                     LDA v_basstp
   202  c048 8d3603             	STA savevstp
   203  c04b a987                       LDA #<(stop)	; basic interpreter stop hook
   204  c04d 8d2803                     STA v_basstp
   205  c050 ad2903                     LDA v_basstp+1
   206  c053 8d3703             	STA savevstp+1
   207  c056 a9c0                       LDA #>(stop)
   208  c058 8d2903                     STA v_basstp+1
   209                          
   210  c05b ad0003                     LDA v_baserr
   211  c05e 8d3403             	STA saveverr
   212  c061 a981                       LDA #<(error)	; basic interpreter error hook
   213  c063 8d0003                     STA v_baserr
   214  c066 ad0103                     LDA v_baserr+1
   215  c069 8d3503             	STA saveverr+1
   216  c06c a9c0                       LDA #>(error)
   217  c06e 8d0103                     STA v_baserr+1
   218                          
   219  c071 a200               	LDX #0		; set graphic cursor to (0,0)
   220  c073 8e3a03             	STX savexl
   221  c076 8e3b03             	STX savexh
   222  c079 8e3c03             	STX savey
   223  c07c e8                 	INX
   224  c07d 8e3d03             	STX savemo	; set mode 1
   225  c080 60                         RTS
   226                          
   227                          error	
   228                          	; reg A may destroyed
   229  c081 208ac1             	JSR gra_off		; uses only reg A
   230  c084 6c3403             	JMP (saveverr)		; to original vector
   231                          
   232                          stop	
   233                          	; reg A may destroyed
   234  c087 a591               	LDA $91			; Scan code
   235  c089 c97f               	CMP #$7F		; STOP key?
   236  c08b d003               	BNE nostop
   237  c08d 208ac1             	JSR gra_off		; uses only reg A
   238                          nostop
   239  c090 6c3603             	JMP (savevstp)		; to original vector
   240                          
   241                          ;-----------------------------------------------------------------
   242                          
   243                          ; start parsing an extension command ...
   244                          
   245                          parse
   246  c093 207300                     JSR chrget			; next char.
   247  c096 08                 	PHP
   248  c097 c926                       CMP #'&'			; command prefix
   249  c099 f004                       BEQ newcmd
   250  c09b 28                         PLP
   251  c09c 4ce7a7                     JMP b_execstatement
   252                          newcmd
   253  c09f 28                 	PLP
   254  c0a0 207300                     JSR chrget			; command character
   255                          
   256  c0a3 a00c                       LDY #(cmdsend-cmds)		; map character to
   257                          					; command address ...
   258                          checknextcmd
   259  c0a5 88                         DEY
   260  c0a6 f01c               	BEQ parse_error
   261  c0a8 d9fac0                     CMP cmds,Y
   262  c0ab d0f8                       BNE checknextcmd		; try next
   263  c0ad 88                         DEY				; found
   264  c0ae 98                         TYA
   265  c0af 0a                         ASL				; *2
   266  c0b0 a8                         TAY
   267                          !ifndef command_rts_tyle {
   268                          	!set co=0			; command offset in jump table
   269  c0b1 b907c1                     LDA cmdaddr+1,Y                 ; high byte from table
   270  c0b4 8556                       STA ijmp+1
   271  c0b6 b906c1                     LDA cmdaddr,Y                   ; low byte from table
   272  c0b9 8555                       STA ijmp
   273  c0bb 207300                     JSR chrget			; read next byte in basic text
   274  c0be 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   275  c0c1 4caea7                     JMP b_interpreter		; continue parsing
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
   290  c0c4 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   291                          
   292                          ;-----------------------------------------------------------------
   293                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   294                          express
   295  c0c7 a900               	LDA #0
   296  c0c9 850d               	STA type	
   297  c0cb 207300             	JSR chrget
   298  c0ce b003               	BCS exp_nonumber
   299  c0d0 4cf3bc             	JMP b_str2fac
   300                          exp_nonumber
   301  c0d3 c926                       CMP #'&'			; command prefix
   302  c0d5 f00b                       BEQ newfunc
   303  c0d7 a57a               	LDA txtptr			; undo chrget
   304  c0d9 d002               	BNE +
   305  c0db c67b               	DEC txtptr+1
   306  c0dd c67a               +	dec txtptr
   307  c0df 6c3803             	JMP (savevexp)			; original routine	
   308                          ;	JMP b_execexpr
   309                          newfunc
   310  c0e2 207300             	jsr chrget
   311  c0e5 c95a               	CMP #'Z'
   312  c0e7 d003               	BNE +
   313  c0e9 4cd1c6             	JMP get
   314  c0ec c958               +	CMP #'X'
   315  c0ee d003               	BNE +
   316  c0f0 4cbcc6             	JMP getposx
   317  c0f3 c959               +	CMP #'Y'
   318  c0f5 d0cd               	BNE parse_error
   319  c0f7 4cc8c6             	JMP getposy
   320                          
   321                          ;-----------------------------------------------------------------
   322                          
   323                          ; the most commonly used command placed at the end ...
   324                          
   325  c0fa 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   326                          cmdsend
   327                          
   328                          cmdaddr
   329  c106 35c883c16cc742c6...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   330  c112 20c8e0c4d7c346c5...        !word to-co,vline-co,hline-co,line-co,plot-co
   331                          
   332  c11c 934752412d455854...author	!text 147,"GRA-EXT V"
   333  c126 312e3236           	+version
   334  c12a 20313938362c3230...	!text " 1986,2016 JOHANN@KLASEK.AT",0
   335                          
   336                          bitmask
   337  c146 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   338                          nbitmask
   339  c14e 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   340                          ytabl
   341  c156 004080c0           	!byte $00,$40,$80,$c0
   342                          ytabh
   343  c15a e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   344  c15e e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   345  c162 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   346  c166 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   347  c16a f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   348  c16e f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   349  c172 fe                 	!byte gramp+$1e
   350                          
   351                          ; for horiz. line
   352                          
   353  c173 ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   354                          
   355  c17b 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   356                          
   357                          
   358                          ;-----------------------------------------------------------------
   359                          
   360                          graphic
   361  c183 209eb7                     JSR b_get8bit
   362  c186 e000                       CPX #$00
   363  c188 d013                       BNE gra_other
   364                          gra0			; &G 0
   365                          gra_off
   366  c18a a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   367  c18c 8d00dd                     STA cia_pra
   368  c18f a915                       LDA #((1 <<4) + (2 <<1) + 1)
   369                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   370                          			; char addr $1000/4096 = char. ROM
   371  c191 8d18d0                     STA vic_mcr	; VIC memory control
   372  c194 ad11d0                     LDA vic_cr	; VIC control register
   373  c197 29df                       AND #%11011111	; Hires mode off
   374  c199 8d11d0                     STA vic_cr
   375  c19c 60                         RTS
   376                          
   377                          gra_other
   378  c19d e001                       CPX #$01
   379  c19f f00f               	BEQ gra1
   380  c1a1 e002               	CPX #$02
   381  c1a3 f00e                       BEQ gra2
   382  c1a5 e004               	CPX #$04
   383  c1a7 f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   384  c1a9 e003               	CPX #$03	; &G 3 (graphic on)
   385  c1ab f029               	BEQ gra_on
   386  c1ad 4c48b2                     JMP b_illquant	; parameter illegal
   387                          	
   388                          gra1			; &G 1
   389  c1b0 20ecc1             	JSR gra_clear
   390                          
   391                          gra2
   392  c1b3 20f1b7                     JSR b_getcomma8bit
   393  c1b6 8a                         TXA		; foreground color
   394  c1b7 0a                         ASL		; upper nibble
   395  c1b8 0a                         ASL
   396  c1b9 0a                         ASL
   397  c1ba 0a                         ASL
   398  c1bb 85fd                       STA gcol
   399  c1bd 20f1b7                     JSR b_getcomma8bit
   400  c1c0 8a                         TXA		; background color
   401  c1c1 290f                       AND #$0F
   402  c1c3 05fd                       ORA gcol
   403  c1c5 a000                       LDY #$00
   404                          cram_loop
   405  c1c7 9900cc                     STA cram,Y	; fill color RAM
   406  c1ca 9900cd                     STA cram+$100,Y
   407  c1cd 9900ce                     STA cram+$200,Y
   408  c1d0 99e8ce                     STA cram+$300-24,Y
   409  c1d3 c8                         INY
   410  c1d4 d0f1                       BNE cram_loop
   411                          
   412                          gra_on
   413  c1d6 200bc2             	JSR gra_setupcode
   414                          
   415  c1d9 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   416  c1db 8d00dd                     STA cia_pra
   417  c1de a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   418  c1e0 8d18d0                     STA vic_mcr	; VIC memory control
   419  c1e3 ad11d0                     LDA vic_cr	; VIC control register
   420  c1e6 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   421  c1e8 8d11d0                     STA vic_cr
   422  c1eb 60                         RTS
   423                          
   424                          gra_clear
   425  c1ec a220                       LDX #$20	; Pages (8 KByte)
   426  c1ee a9e0                       LDA #>gram
   427  c1f0 85fc                       STA gpos+1
   428  c1f2 a000                       LDY #$00
   429  c1f4 84fb                       STY gpos
   430  c1f6 98                         TYA
   431                          gra_fill
   432  c1f7 91fb                       STA (gpos),Y	; Loop unroll
   433  c1f9 c8                         INY
   434  c1fa 91fb                       STA (gpos),Y
   435  c1fc c8                         INY
   436  c1fd 91fb                       STA (gpos),Y
   437  c1ff c8                         INY
   438  c200 91fb                       STA (gpos),Y
   439  c202 c8                         INY
   440  c203 d0f2                       BNE gra_fill
   441  c205 e6fc                       INC gpos+1
   442  c207 ca                         DEX
   443  c208 d0ed                       BNE gra_fill
   444  c20a 60                 	RTS
   445                          
   446                          gra_setupcode
   447  c20b a229               	LDX #(gromcode_end-gromcode) ; count of bytes
   448                          gra_copycode
   449  c20d bd2ec2             	LDA gromcode-1,X
   450  c210 9dd203             	STA gramcode-1,X
   451  c213 ca                 	DEX
   452  c214 d0f7               	BNE gra_copycode
   453  c216 ad3d03             	LDA savemo
   454  c219 290f               	AND #$0F
   455  c21b aa                 	TAX
   456  c21c 4c68c6             	JMP setmode_enter	; re-apply mode to routines
   457                          				; implicit RTS
   458                          
   459                          ;-----------------------------------------------------------------
   460                          
   461                          gexit
   462  c21f a501                       LDA prozport
   463  c221 0902                       ORA #%00000010	; kernal ROM enable
   464  c223 8501                       STA prozport
   465  c225 58                         CLI		; allow interrupts
   466  c226 60                         RTS
   467                          
   468                          ;-----------------------------------------------------------------
   469                          
   470                          ginit
   471  c227 a501                       LDA prozport
   472  c229 29fd                       AND #%11111101	; Kernal ROM disable
   473  c22b 78                         SEI		; disable interrupts
   474  c22c 8501                       STA prozport
   475  c22e 60                         RTS
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
   492  c22f a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   493  c231 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   494                          }
   495  c235 b1a5                       LDA (gaddr),Y
   496                          gchange_op
   497  c237 1d46c1                     ORA bitmask,X
   498  c23a 91a5                       STA (gaddr),Y
   499                          !ifdef ltc {
   500  c23c a910               	LDA #mc_sim		; vollständige ROM-Simulation
   501  c23e 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   502                          }
   503  c242 60                         RTS
   504                          
   505                          ; mask a graphic location 
   506                          
   507                          gmask
   508                          !ifdef ltc {
   509  c243 eb                 	XBA
   510  c244 a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   511  c246 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   512  c24a eb                 	XBA
   513                          }
   514                          gmask_flip
   515  c24b 4900                       EOR #$00
   516                          gmask_op
   517  c24d 11a5                       ORA (gaddr),Y
   518  c24f 91a5                       STA (gaddr),Y
   519                          !ifdef ltc {
   520  c251 a910               	LDA #mc_sim		; vollständige ROM-Simulation
   521  c253 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   522                          }
   523  c257 60                         RTS
   524                          
   525                          }
   526                          
   527                          gromcode_end
   528                          
   529                          ;-----------------------------------------------------------------
   530                          
   531                          position
   532  c258 a5aa                       LDA y
   533  c25a 4a                         LSR
   534  c25b 4a                         LSR
   535  c25c 4a                         LSR		; y/8
   536  c25d a8                         TAY
   537  c25e 2903                       AND #%00000011	; (y/8) mod 4
   538  c260 aa                         TAX
   539  c261 a59b                       LDA xl		; x low
   540  c263 29f8                       AND #%11111000	; clear bit 2-0
   541  c265 18                         CLC
   542  c266 7d56c1                     ADC ytabl,X	; addr low: y base + x part
   543  c269 85a5                       STA gaddr
   544  c26b a59c                       LDA xh		; addr high: x part
   545  c26d 795ac1                     ADC ytabh,Y	; 	+ y base
   546  c270 85a6                       STA gaddr+1
   547  c272 a5aa                       LDA y		; vertical offset
   548  c274 2907                       AND #%00000111	; y mod 8
   549  c276 a8                         TAY
   550  c277 a59b                       LDA xl
   551  c279 2907                       AND #%00000111	; x mod 8
   552  c27b aa                         TAX		; horizonal offset
   553  c27c 60                         RTS		; (bitmask)
   554                          
   555                          
   556                          ;-----------------------------------------------------------------
   557                          
   558                          ; line y up, x right, dx < dy (case 1)
   559                          
   560                          line_up_steep
   561  c27d 2058c2                     JSR position	; x,y
   562                          loop_yup_xright
   563  c280 20d303                     JSR gchange	; pixel
   564                          
   565  c283 18                         CLC		; k += dx
   566  c284 a595                       LDA kl
   567  c286 65ab                       ADC dxl		; dxh is 0, because dx < dy
   568  c288 8595                       STA kl
   569  c28a b004                       BCS ++		; k > 255
   570                          
   571  c28c c5a9                       CMP dy
   572  c28e 9015                       BCC +		; k >= dy ->
   573                          
   574  c290 e5a9               ++	SBC dy		; k -= dy
   575  c292 8595                       STA kl
   576                          
   577  c294 e8                         INX		; x++
   578  c295 e008                       CPX #8
   579  c297 d00c                       BNE +
   580                          	; C=1
   581  c299 a200                       LDX #0		; x overflow, wrap around
   582  c29b a5a5                       LDA gaddr	; x+8: gaddr += 8
   583  c29d 6907                       ADC #8-1	; C already set by CPX
   584  c29f 85a5                       STA gaddr
   585  c2a1 9002                       BCC +
   586  c2a3 e6a6                       INC gaddr+1
   587                          
   588  c2a5 88                 +	DEY		; y--
   589  c2a6 100f                       BPL +++
   590  c2a8 38                         SEC		; y overflow
   591  c2a9 a5a5                       LDA gaddr
   592  c2ab e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   593  c2ad 85a5                       STA gaddr
   594  c2af a5a6                       LDA gaddr+1
   595  c2b1 e901               	SBC #1
   596  c2b3 85a6                       STA gaddr+1
   597  c2b5 a007                       LDY #7		; wrap around
   598                          
   599  c2b7 c6a3               +++	DEC cl		; until c=0
   600  c2b9 d0c5                       BNE loop_yup_xright
   601  c2bb 4c1fc2                     JMP gexit
   602                          
   603                          
   604                          ;-----------------------------------------------------------------
   605                          
   606                          ; line x right, y up, dx > dy (case 2)
   607                          
   608                          line_up_flat
   609  c2be 2058c2                     JSR position	; x,y
   610  c2c1 a5a3               	LDA cl		; counter adjustment for
   611  c2c3 f002               	BEQ +		; dec-dec-counting
   612  c2c5 e6a4               	INC ch
   613                          +
   614                          loop_xright_yup
   615  c2c7 20d303                     JSR gchange	; pixel
   616                          
   617  c2ca 18                         CLC		; k += dy
   618  c2cb a595                       LDA kl
   619  c2cd 65a9                       ADC dy
   620  c2cf 8595                       STA kl
   621  c2d1 9002                       BCC ++
   622  c2d3 e696                       INC kh
   623                          
   624  c2d5 c5ab               ++	CMP dxl		; k > dx?
   625  c2d7 a596                       LDA kh
   626  c2d9 e5a7                       SBC dxh
   627  c2db 901a                       BCC +
   628                          
   629  c2dd 8596                       STA kh		; k -= dx
   630  c2df a595                       LDA kl
   631  c2e1 e5ab                       SBC dxl
   632  c2e3 8595                       STA kl
   633                          
   634  c2e5 88                         DEY		; y--
   635  c2e6 100f                       BPL +
   636  c2e8 38                 	SEC		; C=1 not always true (SBC above)
   637  c2e9 a5a5                       LDA gaddr	; y overflow
   638  c2eb e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   639  c2ed 85a5                       STA gaddr
   640  c2ef a5a6                       LDA gaddr+1
   641  c2f1 e901               	SBC #1
   642  c2f3 85a6                       STA gaddr+1
   643  c2f5 a007               	LDY #7		; wrap around
   644                          
   645  c2f7 e8                 +	INX		; x++
   646  c2f8 e008                       CPX #8		; x overflow?
   647  c2fa d00c                       BNE ++
   648                          	; C=1
   649  c2fc a200                       LDX #0		; wrap around
   650  c2fe a5a5                       LDA gaddr	; x+8: gaddr += 8
   651  c300 6907                       ADC #8-1	; C already set by CPX
   652  c302 85a5                       STA gaddr
   653  c304 9002                       BCC ++
   654  c306 e6a6                       INC gaddr+1
   655                          ++
   656  c308 c6a3               	DEC cl		; c--
   657  c30a d0bb                       BNE loop_xright_yup
   658  c30c c6a4                       DEC ch		; adjusted high which allows this
   659  c30e d0b7                       BNE loop_xright_yup
   660                          
   661  c310 4c1fc2                     JMP gexit
   662                          
   663                          
   664                          
   665                          ;-----------------------------------------------------------------
   666                          
   667                          ; line x right, y down, dx > dy (case 3)
   668                          
   669                          line_down_flat
   670  c313 2058c2                     JSR position	; x,y
   671  c316 a5a3               	LDA cl		; counter adjustment for
   672  c318 f002               	BEQ +		; dec-dec-counting
   673  c31a e6a4               	INC ch
   674                          +
   675                          loop_xright_ydown
   676  c31c 20d303                     JSR gchange	; pixel
   677                          
   678  c31f 18                         CLC		; k += dy
   679  c320 a595                       LDA kl
   680  c322 65a9                       ADC dy
   681  c324 8595                       STA kl
   682  c326 9002                       BCC ++
   683  c328 e696                       INC kh
   684                          
   685  c32a c5ab               ++	CMP dxl		; k > dx
   686  c32c a596                       LDA kh
   687  c32e e5a7                       SBC dxh		; k -= dx
   688  c330 901b                       BCC +
   689                          
   690  c332 8596                       STA kh
   691  c334 a595                       LDA kl
   692  c336 e5ab                       SBC dxl
   693  c338 8595                       STA kl
   694                          
   695  c33a c8                         INY		; y++
   696  c33b c008                       CPY #8
   697  c33d d00e                       BNE +
   698                          	; C=1
   699  c33f a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   700  c341 693f                       ADC #$40-1	; C already set by CPY
   701  c343 85a5                       STA gaddr
   702  c345 a5a6                       LDA gaddr+1
   703  c347 6901               	ADC #1
   704  c349 85a6                       STA gaddr+1
   705  c34b a000                       LDY #0		; wrap around
   706                          
   707  c34d e8                 +	INX		; x++
   708  c34e e008                       CPX #8		; x overflow ?
   709  c350 d00c                       BNE +++
   710                          	; C=1
   711  c352 a200                       LDX #$00	; wrap around
   712  c354 a5a5                       LDA gaddr	; gaddr += 8
   713  c356 6907                       ADC #$08-1	; C always set by CPX
   714  c358 85a5                       STA gaddr
   715  c35a 9002                       BCC +++
   716  c35c e6a6                       INC gaddr+1
   717                          +++
   718  c35e c6a3               	DEC cl		; c--
   719  c360 d0ba                       BNE loop_xright_ydown
   720  c362 c6a4                       DEC ch		; adjusted high which allows this
   721  c364 d0b6                       BNE loop_xright_ydown
   722                          
   723  c366 4c1fc2                     JMP gexit
   724                          
   725                          
   726                          ;-----------------------------------------------------------------
   727                          
   728                          ; line y down, x right, dx < dy (case 4)
   729                          
   730                          line_down_steep
   731  c369 2058c2                     JSR position	; x,y
   732                          loop_ydown_xright
   733  c36c 20d303                     JSR gchange	; pixel
   734                          
   735  c36f 18                         CLC		; k += dx
   736  c370 a595                       LDA kl
   737  c372 65ab                       ADC dxl		; dxh is 0, because dx < dy
   738  c374 8595                       STA kl
   739  c376 b004                       BCS ++
   740  c378 c5a9                       CMP dy		; k > dy?
   741  c37a 9015                       BCC +
   742  c37c e5a9               ++	SBC dy		; k -= dy
   743  c37e 8595                       STA kl
   744                          
   745  c380 e8                         INX		; x++
   746  c381 e008                       CPX #8
   747  c383 d00c                       BNE +		; x overflow?
   748  c385 a200                       LDX #0		; wrap around
   749  c387 a5a5                       LDA gaddr	; x+9: gaddr += 8
   750  c389 6907                       ADC #8-1	; C already set by CPX
   751  c38b 85a5                       STA gaddr
   752  c38d 9002                       BCC +
   753  c38f e6a6                       INC gaddr+1
   754                          
   755  c391 c8                 +	INY		; y++
   756  c392 c008                       CPY #8		; y overflow?
   757  c394 d00e                       BNE +++
   758  c396 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   759  c398 693f                       ADC #$40-1	; C already set by CPY
   760  c39a 85a5                       STA gaddr
   761  c39c a5a6                       LDA gaddr+1
   762  c39e 6901               	ADC #1
   763  c3a0 85a6                       STA gaddr+1
   764  c3a2 a000                       LDY #0		; wrap around
   765                          
   766  c3a4 c6a3               +++	DEC cl		; c--
   767                          			; until c=0
   768  c3a6 d0c4                       BNE loop_ydown_xright
   769  c3a8 4c1fc2                     JMP gexit
   770                          
   771                          
   772                          ;-----------------------------------------------------------------
   773                          
   774                          getcommaxy
   775  c3ab 20fdae                     JSR b_getcomma	; check ","
   776                          getxy
   777  c3ae 208aad                     JSR b_getval	; get X coord. value
   778  c3b1 20f7b7                     JSR b_convint
   779  c3b4 c901                       CMP #>xmax
   780  c3b6 900c               	BCC gcxy_xok
   781  c3b8 f003                       BEQ ++		; X = $1xx
   782  c3ba 202fc6                     JSR range_error
   783                          
   784  c3bd c040               ++	CPY #<xmax	; check X low
   785  c3bf 9003                       BCC +
   786  c3c1 202fc6                     JSR range_error
   787                          +
   788                          gcxy_xok
   789  c3c4 84fb                       STY gpos	; temporary save X coord.
   790  c3c6 85fc                       STA gpos+1
   791                          
   792  c3c8 20f1b7                     JSR b_getcomma8bit
   793                          			; get Y coord. value
   794  c3cb e0c8                       CPX #ymax
   795  c3cd 9003                       BCC +
   796  c3cf 202fc6                     JSR range_error
   797                          +
   798  c3d2 a4fb                       LDY gpos	; restory X coord.
   799  c3d4 a5fc                       LDA gpos+1
   800  c3d6 60                         RTS
   801                          
   802                          
   803                          ;-----------------------------------------------------------------
   804                          
   805                          hline
   806  c3d7 20aec3                     JSR getxy	; get startpoint
   807  c3da 86aa                       STX y
   808  c3dc 8e3c03                     STX savey	; save as cursor, too
   809  c3df 859c                       STA xh
   810  c3e1 849b                       STY xl
   811  c3e3 20fdae                     JSR b_getcomma	; get length
   812  c3e6 208aad                     JSR b_getval
   813  c3e9 20f7b7                     JSR b_convint
   814                          			; calculate end point
   815  c3ec aa                         TAX		; save length high byte
   816  c3ed 98                         TYA		; length low byte
   817  c3ee 18                         CLC
   818  c3ef 659b                       ADC xl		; low xend = x+length
   819  c3f1 859e                       STA xendl
   820  c3f3 a8                 	TAY
   821  c3f4 8a                         TXA		; high
   822  c3f5 659c                       ADC xh		; high xend = x+length
   823  c3f7 859f                       STA xendh
   824  c3f9 aa                 	TAX
   825                          
   826  c3fa c901               	CMP #>xmax	; endpoint outside?
   827  c3fc 900a               	BCC +
   828  c3fe d005               	BNE ++		; >=$200
   829  c400 98                 	TYA
   830  c401 e940               	SBC #<xmax
   831  c403 9003               	BCC +
   832  c405 202fc6             ++	JSR range_error
   833                          +
   834  c408 8e3b03                     STX savexh
   835  c40b 8c3a03                     STY savexl	; also save as cursor
   836                          
   837  c40e a900               	LDA #0
   838  c410 85a3               	STA ycount
   839  c412 207900             	JSR $0079
   840  c415 f012               	BEQ +
   841  c417 20f1b7             	JSR b_getcomma8bit
   842  c41a 8a                 	TXA
   843  c41b 85a3               	STA ycount
   844  c41d f00a               	BEQ +
   845  c41f 18                 	CLC
   846  c420 65aa               	ADC y		; end position for y coord.
   847  c422 c9c8               	CMP #ymax
   848  c424 9003               	BCC ++
   849  c426 202fc6             	JSR range_error
   850                          ++
   851                          +
   852  c429 2027c2                     JSR ginit	; map in graphic memory
   853  c42c d01a               	BNE hl_noxswap	; ginit left with Z=0
   854                          
   855                          hline_start
   856  c42e a59e                       LDA xendl
   857  c430 c59b                       CMP xl
   858  c432 a59f                       LDA xendh
   859  c434 e59c                       SBC xh
   860  c436 b010                       BCS hl_noxswap	; xend < x ->
   861                          
   862  c438 a69e                       LDX xendl	; swap x, xend
   863  c43a a59b                       LDA xl
   864  c43c 869b                       STX xl
   865  c43e 859e                       STA xendl
   866                          
   867  c440 a69f                       LDX xendh
   868  c442 a49c                       LDY xh
   869  c444 849f                       STY xendh
   870  c446 869c                       STX xh
   871                          hl_noxswap
   872  c448 e6a3               	INC ycount
   873                          hl_start
   874  c44a 2058c2                     JSR position	; graphic position x,y
   875                          
   876  c44d a5a5               	LDA gaddr	; save position for vertical
   877  c44f 85fb               	STA sgaddr
   878  c451 a5a6               	LDA gaddr+1
   879  c453 85fc               	STA sgaddr+1
   880  c455 86fd               	STX xsave
   881  c457 84a9               	STY ysave
   882                          
   883  c459 a59e                       LDA xendl
   884  c45b 2907                       AND #%00000111
   885  c45d 8596                       STA tmp2	; xend mod 8, mask index
   886  c45f a59b                       LDA xl
   887  c461 29f8                       AND #%11111000	; (xl div 8)*8
   888  c463 8595                       STA tmp1
   889  c465 a59e                       LDA xendl	; xend unmasked
   890  c467 38                         SEC
   891  c468 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   892  c46a 8595                       STA tmp1
   893  c46c a59f                       LDA xendh
   894  c46e e59c                       SBC xh
   895  c470 4a                         LSR		; / 8 ->  0-39
   896  c471 a595                       LDA tmp1	; only 1 highest bit
   897  c473 6a                         ROR		; and 3 lower bits
   898  c474 4a                         LSR
   899  c475 4a                         LSR
   900                                  		; 8-pixel-blocks count
   901  c476 85a4               	STA hcount	; save for vertical extension
   902                           
   903                          hl_vertloop
   904  c478 98                 	TYA		; calculate max. Y in 8x8 block
   905  c479 18                 	CLC
   906  c47a 65a3               	ADC ycount
   907  c47c c908               	CMP #8
   908  c47e 9002               	BCC +
   909  c480 a908               	LDA #8
   910  c482 85a8               +	STA ylimit
   911                          
   912  c484 bd73c1                     LDA maskleft,X	; starting mask
   913  c487 8595               	STA tmp1
   914  c489 a6a4               	LDX hcount	; how many blocks
   915                          
   916                          hl_nextblock
   917  c48b ca                         DEX
   918                          hl_islastblock
   919  c48c 301d                       BMI hl_lastblock
   920                          			; leave loop if X<0
   921  c48e a4a9               	LDY ysave
   922  c490 a595               -	LDA tmp1	; mask
   923  c492 20e703             	JSR gmask	; first with left end mask
   924  c495 c8                 	INY		; vertical down
   925  c496 c4a8               	CPY ylimit	; in 8x8 box
   926  c498 d0f6               	BNE -
   927                          
   928  c49a 18                         CLC		; gaddr += 8 (one block to right)
   929  c49b a5a5                       LDA gaddr
   930  c49d 6908                       ADC #8
   931  c49f 85a5                       STA gaddr
   932  c4a1 9002                       BCC +
   933  c4a3 e6a6                       INC gaddr+1
   934                          
   935  c4a5 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   936  c4a7 8595               	STA tmp1
   937  c4a9 d0e0               	BNE hl_nextblock	; always
   938                          
   939                          hl_lastblock
   940  c4ab a696                       LDX tmp2	; xend mask index
   941  c4ad 3d7bc1                     AND maskright,X ; A has current maskt combine with mask right end
   942  c4b0 8595               	STA tmp1	; mask
   943  c4b2 a4a9               	LDY ysave	; start position in 8x8 block
   944  c4b4 a595               -	LDA tmp1	; mask
   945  c4b6 20e703             	JSR gmask	; modify
   946  c4b9 c8                 	INY		; vertical down
   947  c4ba c6a3               	DEC ycount	; overall y counter
   948  c4bc c4a8               	CPY ylimit
   949  c4be d0f4               	BNE -
   950                          
   951  c4c0 a5a3               	LDA ycount	; finished
   952  c4c2 d003               	BNE +
   953  c4c4 4c1fc2                     JMP gexit	; leave
   954                          
   955  c4c7 18                 +	CLC
   956  c4c8 a5fb               	LDA sgaddr
   957  c4ca 6940               	ADC #$40	; next 8-pixel row
   958  c4cc 85fb               	STA sgaddr	; + $140 (320)
   959  c4ce 85a5               	STA gaddr
   960  c4d0 a5fc               	LDA sgaddr+1
   961  c4d2 6901               	ADC #$01
   962  c4d4 85fc               	STA sgaddr+1
   963  c4d6 85a6               	STA gaddr+1
   964  c4d8 a6fd               	LDX xsave
   965  c4da a000               	LDY #0
   966  c4dc 84a9               	STY ysave
   967  c4de f098               	BEQ hl_vertloop
   968                          ;-----------------------------------------------------------------
   969                          
   970                          vline
   971  c4e0 20aec3                     JSR getxy	; get startpoint
   972  c4e3 859c                       STA xh
   973  c4e5 8d3b03                     STA savexh	; save as cursor too
   974  c4e8 849b                       STY xl
   975  c4ea 8c3a03                     STY savexl
   976  c4ed 86aa                       STX y
   977                          
   978  c4ef 20f1b7                     JSR b_getcomma8bit
   979                          			; get length
   980  c4f2 18                         CLC		; calculate end point
   981  c4f3 8a                         TXA		; length
   982                          ; DON'T-CHANGE: how long to go vertically (needed later)
   983                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   984                          ;	STA tmp1
   985  c4f4 65aa                       ADC y		; length + y
   986  c4f6 c9c8                       CMP #ymax	; outside?
   987  c4f8 9003                       BCC +
   988                          vline_iq
   989  c4fa 202fc6                     JSR range_error
   990  c4fd 8593               +	STA yend	; endpoint
   991                          
   992  c4ff 8d3c03             	STA savey	; set cursor y position
   993  c502 2027c2                     JSR ginit	; map in graphic memory
   994  c505 d012               	BNE vl_start	; ginit left with Z=0
   995                          
   996                          vline_start
   997  c507 a593                       LDA yend
   998  c509 c5aa                       CMP y
   999  c50b b00a                       BCS vl_noyswap	; yend < y ->
  1000  c50d a5aa                       LDA y		; swap y, yend
  1001  c50f a693                       LDX yend
  1002  c511 8593                       STA yend
  1003  c513 86aa                       STX y
  1004  c515 f002               	BEQ vl_start	; always (with next branch)
  1005                          	; fall through if yend is
  1006                          vl_noyswap
  1007  c517 d000                       BNE vl_start	; yend > y
  1008                          ;	JMP plot_start	; y = yend -> single point
  1009                          ;	JMP gexit	; no point
  1010                          
  1011                          vl_start
  1012  c519 2058c2                     JSR position	; graphic position x,y
  1013  c51c bd46c1                     LDA bitmask,X
  1014  c51f 8596                       STA tmp2	; save mask
  1015                          ; DON'T-CHANGE: replace ...
  1016  c521 38                         SEC
  1017  c522 a593                       LDA yend
  1018  c524 e5aa                       SBC y		; vertical length
  1019  c526 aa                         TAX
  1020                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1021                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1022                          ;	LDX tmp1
  1023  c527 e8                         INX		; +1 (exit on 0)
  1024                          vl_nextline
  1025  c528 a596                       LDA tmp2
  1026  c52a 20e703                     JSR gmask	; modify 
  1027  c52d c8                         INY		; go down
  1028  c52e c008                       CPY #8		; 8-line wrap
  1029  c530 d00e                       BNE +
  1030  c532 a5a5                       LDA gaddr	; gaddr += 320
  1031  c534 693f               	ADC #$40-1	; compensate for C = 1
  1032  c536 85a5                       STA gaddr
  1033  c538 a5a6                       LDA gaddr+1
  1034  c53a 6901                       ADC #$01
  1035  c53c 85a6                       STA gaddr+1
  1036  c53e a000                       LDY #0		; wrap y offset
  1037  c540 ca                 +	DEX		; all vertical positions done?
  1038  c541 d0e5                       BNE vl_nextline
  1039  c543 4c1fc2                     JMP gexit	; leave
  1040                          
  1041                          
  1042                          ;-----------------------------------------------------------------
  1043                          
  1044                          line
  1045  c546 20aec3                     JSR getxy	; get startpoint
  1046  c549 849b                       STY xl 
  1047  c54b 859c                       STA xh
  1048  c54d 86aa                       STX y
  1049                          
  1050  c54f 20abc3                     JSR getcommaxy	; get endpoint
  1051                          line_start
  1052  c552 8c3a03                     STY savexl	; save as cursor position too
  1053  c555 849e                       STY xendl
  1054  c557 8d3b03                     STA savexh
  1055  c55a 859f                       STA xendh
  1056  c55c 8e3c03                     STX savey
  1057  c55f 8693                       STX yend
  1058                          
  1059  c561 2027c2                     JSR ginit	; map in graphic memory
  1060                          
  1061  c564 a000                       LDY #$00	; initialize to 0
  1062  c566 84a8                       STY ydir
  1063  c568 8495                       STY kl
  1064  c56a 8496                       STY kh
  1065                          
  1066  c56c 38                         SEC
  1067  c56d a59e                       LDA xendl	; calculate dx
  1068  c56f e59b                       SBC xl
  1069  c571 85ab                       STA dxl
  1070  c573 a59f                       LDA xendh
  1071  c575 e59c                       SBC xh
  1072  c577 85a7                       STA dxh
  1073                          
  1074  c579 b025                       BCS li_xend_right
  1075                          	; dx != 0
  1076  c57b 98                         TYA		; negate dx
  1077  c57c 38                         SEC		; dx = 0 - dx
  1078  c57d e5ab                       SBC dxl
  1079  c57f 85ab                       STA dxl
  1080  c581 98                         TYA
  1081  c582 e5a7                       SBC dxh
  1082  c584 85a7                       STA dxh
  1083                          			; C=0 always, needed later
  1084  c586 a69b                       LDX xl		; swap x low
  1085  c588 a49e                       LDY xendl
  1086  c58a 869e                       STX xendl
  1087  c58c 849b                       STY xl
  1088                          
  1089  c58e a69c                       LDX xh		; swap x high
  1090  c590 a49f                       LDY xendh
  1091  c592 869f                       STX xendh
  1092  c594 849c                       STY xh
  1093                          
  1094  c596 a6aa                       LDX y		; swap y
  1095  c598 a493                       LDY yend
  1096  c59a 8693                       STX yend
  1097  c59c 84aa                       STY y
  1098                          
  1099  c59e 9009                       BCC li_x_different
  1100                          			; C=0 always (from negation before)
  1101                          
  1102                          li_xend_right
  1103  c5a0 a5ab                       LDA dxl		; dx = 0?
  1104  c5a2 05a7                       ORA dxh
  1105  c5a4 d003                       BNE li_x_different
  1106  c5a6 4c07c5                     JMP vline_start	; vertical line case
  1107                          
  1108                          li_x_different
  1109  c5a9 38                         SEC		; calculate dy
  1110  c5aa a593                       LDA yend
  1111  c5ac e5aa                       SBC y
  1112  c5ae b006                       BCS li_y_right
  1113  c5b0 49ff                       EOR #$FF	; negate dy (two's complement)
  1114  c5b2 6901                       ADC #$01	; C=0
  1115  c5b4 85a8                       STA ydir	; flag y goes up
  1116                          
  1117                          li_y_right
  1118  c5b6 85a9                       STA dy
  1119  c5b8 d007                       BNE +
  1120  c5ba a900               	LDA #0
  1121  c5bc 85a3               	STA ycount
  1122  c5be 4c2ec4                     JMP hline_start	; horizontal line case
  1123                          +
  1124                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1125                          
  1126  c5c1 a5a7                       LDA dxh		; dx > dy
  1127  c5c3 d017                       BNE line_flat	; yes -> flat
  1128  c5c5 a5a9                       LDA dy		; no -> steep
  1129  c5c7 aa                         TAX
  1130  c5c8 c5ab                       CMP dxl
  1131  c5ca 9010                       BCC line_flat
  1132                          
  1133                          line_steep
  1134  c5cc e8                         INX	
  1135  c5cd 86a3                       STX cl		; c = dy+1
  1136  c5cf 4a                         LSR		; k = dy/2
  1137  c5d0 8595                       STA kl
  1138  c5d2 a5a8                       LDA ydir
  1139  c5d4 d003                       BNE +
  1140  c5d6 4c69c3                     JMP line_down_steep	; y down, steep
  1141  c5d9 4c7dc2             +	JMP line_up_steep	; y up, steep
  1142                          
  1143                          line_flat
  1144  c5dc a5a7                       LDA dxh
  1145  c5de a8                         TAY
  1146  c5df a6ab                       LDX dxl
  1147  c5e1 e8                         INX
  1148  c5e2 d001                       BNE +
  1149  c5e4 c8                         INY
  1150  c5e5 86a3               +	STX cl		; c = dx+1
  1151  c5e7 84a4                       STY ch
  1152                          
  1153  c5e9 4a                         LSR		; k = dx/2
  1154  c5ea 8596                       STA kh
  1155  c5ec a5ab                       LDA dxl
  1156  c5ee 6a                         ROR		; dx/2
  1157  c5ef 8595                       STA kl
  1158  c5f1 a5a8                       LDA ydir	
  1159  c5f3 d003                       BNE +
  1160  c5f5 4c13c3                     JMP line_down_flat	; y down, flat
  1161  c5f8 4cbec2             +	JMP line_up_flat	; y up, flat
  1162                          
  1163                          ;-----------------------------------------------------------------
  1164                          
  1165                          plot
  1166  c5fb 20aec3                     JSR getxy	; get parameter
  1167  c5fe 859c                       STA xh		; save x/y
  1168  c600 849b                       STY xl
  1169  c602 86aa                       STX y
  1170  c604 8d3b03                     STA savexh	; and store as cursor
  1171  c607 8c3a03                     STY savexl
  1172  c60a 8e3c03                     STX savey
  1173                          
  1174                          plot_start
  1175  c60d 2058c2                     JSR position	; calculate graphical address
  1176                          
  1177  c610 a501                       LDA prozport
  1178  c612 29fd                       AND #%11111101	; Kernal ROM disable
  1179  c614 78                         SEI			
  1180  c615 8501                       STA prozport
  1181                          
  1182  c617 20d303                     JSR gchange	; change graphical data
  1183                          
  1184  c61a a501                       LDA prozport
  1185  c61c 0902                       ORA #%00000010	; kernal ROM enable
  1186  c61e 8501                       STA prozport
  1187  c620 58                         CLI
  1188  c621 60                         RTS
  1189                          
  1190                          ;-----------------------------------------------------------------
  1191                          
  1192                          move
  1193  c622 20aec3                     JSR getxy	; get parameter
  1194  c625 8d3b03                     STA savexh	; just save as cursor
  1195  c628 8c3a03                     STY savexl
  1196  c62b 8e3c03                     STX savey
  1197  c62e 60                         RTS
  1198                          
  1199                          
  1200                          ;-----------------------------------------------------------------
  1201                          
  1202                          ; never touch X, Y
  1203                          range_error
  1204  c62f ad3d03             	LDA savemo
  1205  c632 29f0               	AND #$F0
  1206  c634 d003               	BNE +
  1207  c636 68                 	PLA			; cleanup JSR
  1208  c637 68                 	PLA
  1209  c638 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1210                          				; error mode 2: cut value: control back
  1211                          				; to handle value correction
  1212  c639 2920               +	AND #$20
  1213  c63b d0fb               	BNE -
  1214  c63d 68                 	PLA			; cleanup JSR
  1215  c63e 68                 	PLA
  1216                          setmode_error
  1217  c63f 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1218                          
  1219                          ;-----------------------------------------------------------------
  1220                          
  1221                          setmode
  1222  c642 209eb7                     JSR b_get8bit
  1223  c645 e003                       CPX #3
  1224  c647 9013                       BCC +			; less then 3, modification mode
  1225  c649 e006               	CPX #6
  1226  c64b b0f2               	BCS setmode_error	; out of range
  1227                          				; error mode
  1228  c64d 8a                 	TXA
  1229  c64e 690d               	ADC #13			; C=0, therefore -3
  1230                          				; 3-5 -> 16-18
  1231                          				; put A's bit 4-7 into savemo
  1232  c650 4d3d03             	EOR savemo		; ********
  1233  c653 29f0               	AND #$F0		; ****0000
  1234  c655 4d3d03             	EOR savemo		; AAAAmmmm
  1235  c658 8d3d03             	STA savemo		; 
  1236  c65b 60                 	RTS
  1237                          
  1238  c65c 8a                 +	TXA
  1239  c65d 4d3d03             	EOR savemo		; put A's bit 0-3 into savemo
  1240  c660 290f               	AND #$0F
  1241  c662 4d3d03             	EOR savemo
  1242  c665 8d3d03             	STA savemo
  1243                          setmode_enter
  1244  c668 e001               	CPX #$01
  1245  c66a b01a                       BCS set_or_toggle
  1246                          
  1247                          modereset
  1248  c66c a9c1                       LDA #>(nbitmask)
  1249  c66e 8ddd03                     STA gchange_op+2
  1250  c671 a94e                       LDA #<(nbitmask)
  1251  c673 8ddc03                     STA gchange_op+1
  1252  c676 a93d                       LDA #$3D		; AND abs,X
  1253  c678 8ddb03                     STA gchange_op
  1254  c67b a931                       LDA #$31		; AND (zp),Y
  1255  c67d 8df103                     STA gmask_op
  1256  c680 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1257  c682 8df003                     STA gmask_flip+1
  1258  c685 60                         RTS
  1259                          
  1260                          set_or_toggle
  1261  c686 d01a                       BNE modetoggle
  1262                          modeset
  1263  c688 a9c1                       LDA #>(bitmask)
  1264  c68a 8ddd03                     STA gchange_op+2
  1265  c68d a946                       LDA #<(bitmask)
  1266  c68f 8ddc03                     STA gchange_op+1
  1267  c692 a91d                       LDA #$1D		; OR abs,X
  1268  c694 8ddb03                     STA gchange_op
  1269  c697 a911                       LDA #$11		; OR (zp),Y
  1270  c699 8df103                     STA gmask_op
  1271  c69c a900                       LDA #$00		; EOR #$00, nicht invertieren
  1272  c69e 8df003                     STA gmask_flip+1
  1273  c6a1 60                         RTS
  1274                          
  1275                          modetoggle
  1276  c6a2 a9c1                       LDA #>(bitmask)
  1277  c6a4 8ddd03                     STA gchange_op+2
  1278  c6a7 a946                       LDA #<(bitmask)
  1279  c6a9 8ddc03                     STA gchange_op+1
  1280  c6ac a95d                       LDA #$5D		; EOR abs,X
  1281  c6ae 8ddb03                     STA gchange_op
  1282  c6b1 a951                       LDA #$51		; EOR (zp),Y
  1283  c6b3 8df103                     STA gmask_op
  1284  c6b6 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1285  c6b8 8df003                     STA gmask_flip+1
  1286  c6bb 60                         RTS
  1287                          
  1288                          
  1289                          ;-----------------------------------------------------------------
  1290                          ; get current x cursor position
  1291                          
  1292                          getposx
  1293  c6bc ac3a03             	LDY savexl
  1294  c6bf ad3b03             	LDA savexh
  1295  c6c2 2091b3             	JSR b_word2fac
  1296  c6c5 4c7300             	JMP chrget	; last position of expression (function name)
  1297                          
  1298                          ;-----------------------------------------------------------------
  1299                          ; get current y cursor position
  1300                          
  1301                          getposy
  1302  c6c8 ac3c03             	LDY savey
  1303  c6cb 20a2b3             	JSR b_byte2fac
  1304  c6ce 4c7300             	JMP chrget	; last position of expression (function name)
  1305                          
  1306                          ;-----------------------------------------------------------------
  1307                          
  1308                          ; get pixel (check if pixel set)
  1309                          ; not used
  1310                          
  1311                          get
  1312  c6d1 207300             	JSR chrget	; advance past function name
  1313  c6d4 20faae             	JSR b_chkparl	; "("?
  1314  c6d7 20aec3                     JSR getxy	; get X,Y values
  1315  c6da 859c                       STA xh
  1316  c6dc 849b                       STY xl
  1317  c6de 86aa                       STX y
  1318  c6e0 207900             	JSR chrgot
  1319  c6e3 20f7ae             	JSR b_chkparr	; ")"?
  1320                          	
  1321                          
  1322  c6e6 2058c2                     JSR position	; calculate graphic address/position
  1323                          
  1324  c6e9 a501                       LDA prozport
  1325  c6eb 29fd               	AND #%11111101	; Kernal ROM disable
  1326  c6ed 78                         SEI
  1327  c6ee 8501                       STA prozport
  1328                          
  1329  c6f0 b1a5                       LDA (gaddr),Y
  1330  c6f2 3d46c1                     AND bitmask,X	; mask position
  1331  c6f5 a8                         TAY
  1332  c6f6 a501                       LDA prozport
  1333  c6f8 0902               	ORA #%00000010	; kernal ROM enable
  1334  c6fa 8501                       STA prozport
  1335  c6fc 58                         CLI
  1336  c6fd 98                 	TYA
  1337  c6fe f002               	BEQ +
  1338  c700 a001               	LDY #1		; <> 0 -> alway return 1
  1339  c702 4ca2b3             +	JMP b_byte2fac	; still on expr.'s last character
  1340                          
  1341                          ;-----------------------------------------------------------------
  1342                          
  1343                          relto
  1344  c705 208aad                     JSR b_getval	; get X offset (+/-)
  1345  c708 a561               	LDA facexp	; FAC exponent
  1346  c70a c990               	CMP #$90	; more than 16 bit
  1347  c70c b031               	BCS relto_error	; illegal quantity
  1348  c70e 209bbc                     JSR b_fac2int	; to signed integer
  1349                          
  1350  c711 18                         CLC
  1351  c712 a565                       LDA facintl
  1352  c714 6d3a03                     ADC savexl
  1353  c717 859e                       STA xendl
  1354  c719 a564                       LDA facinth
  1355  c71b 6d3b03                     ADC savexh
  1356  c71e 859f                       STA xendh	; xend = savex+facint
  1357                          
  1358  c720 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1359  c723 208aad                     JSR b_getval
  1360  c726 a561                       LDA facexp	; FAC exponent
  1361  c728 c990                       CMP #$90	; more than 16 bit
  1362  c72a b013                       BCS relto_error	; illegal quantity
  1363  c72c 209bbc                     JSR b_fac2int	; to signed integer
  1364  c72f 18                         CLC
  1365  c730 a565                       LDA facintl
  1366  c732 6d3c03                     ADC savey
  1367  c735 8593                       STA yend	; yend = savey+facint
  1368                          
  1369  c737 a59f                       LDA xendh	; check end coord. x
  1370  c739 c901                       CMP #>xmax
  1371  c73b 900e                       BCC rt_xok
  1372  c73d f003                       BEQ +
  1373                          relto_error
  1374  c73f 202fc6                     JSR range_error
  1375  c742 a59e               +	LDA xendl
  1376  c744 c940                       CMP #<xmax
  1377  c746 9003                       BCC +
  1378  c748 202fc6                     JSR range_error
  1379                          +
  1380                          rt_xok
  1381  c74b a593                       LDA yend	; check end coord. y
  1382  c74d c9c8                       CMP #ymax
  1383  c74f 9003                       BCC +
  1384  c751 202fc6                     JSR range_error
  1385                          +
  1386  c754 ad3a03                     LDA savexl
  1387  c757 859b                       STA xl
  1388  c759 ad3b03                     LDA savexh
  1389  c75c 859c                       STA xh
  1390  c75e ad3c03                     LDA savey
  1391  c761 85aa                       STA y
  1392  c763 a49e                       LDY xendl
  1393  c765 a59f                       LDA xendh
  1394  c767 a693                       LDX yend	; xend/yend = cursor + x/y
  1395                          
  1396  c769 4c52c5                     JMP line_start	; draw line x/y to xend/yend
  1397                          
  1398                          
  1399                          ;-----------------------------------------------------------------
  1400                          
  1401                          char
  1402  c76c 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1403  c76f e028                       CPX #40	
  1404  c771 9003                       BCC +
  1405                          char_error
  1406  c773 4c48b2                     JMP b_illquant
  1407  c776 86fb               +	STX gpos	; save x coord.
  1408  c778 20f1b7                     JSR b_getcomma8bit
  1409                          			; get char. position y 0-24
  1410  c77b e019                       CPX #25
  1411  c77d b0f4                       BCS char_error
  1412  c77f 86fc                       STX gpos+1	; save y coord.
  1413                          
  1414  c781 20fdae                     JSR b_getcomma	; get string
  1415  c784 209ead                     JSR b_getexpr
  1416  c787 20a3b6                     JSR b_stringval ; string address in str
  1417  c78a 48                         PHA		; string length
  1418  c78b a6fc                       LDX gpos+1	; y coord. for char. position
  1419  c78d 8a                         TXA
  1420  c78e 2903                       AND #$03	; mask 2 bits
  1421  c790 a8                         TAY		; table index
  1422  c791 a900                       LDA #$00
  1423  c793 85fc                       STA gpos+1	; x high
  1424  c795 a5fb                       LDA gpos	; saved x: multiply by 8
  1425  c797 0a                         ASL
  1426  c798 0a                         ASL
  1427  c799 0a                         ASL
  1428  c79a 26fc                       ROL gpos+1	; overflow to high byte
  1429  c79c 7956c1                     ADC ytabl,Y
  1430  c79f 85a5                       STA gaddr
  1431  c7a1 a5fc                       LDA gpos+1	; x high
  1432  c7a3 7d5ac1                     ADC ytabh,X
  1433  c7a6 85a6                       STA gaddr+1
  1434  c7a8 68                         PLA		; string length
  1435  c7a9 a000                       LDY #$00	; string index
  1436  c7ab aa                         TAX		; length
  1437  c7ac e8                         INX		; prepare as counter
  1438                          char_loop
  1439  c7ad ca                         DEX
  1440  c7ae f008                       BEQ char_exit
  1441  c7b0 b122                       LDA (str),Y	; read string
  1442  c7b2 20b9c7                     JSR char_display
  1443  c7b5 c8                         INY
  1444  c7b6 d0f5                       BNE char_loop
  1445                          char_exit
  1446  c7b8 60                         RTS
  1447                          
  1448                          char_display
  1449  c7b9 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1450  c7bb 8a                         TXA		; save register X+Y
  1451  c7bc 48                         PHA
  1452  c7bd 98                         TYA
  1453  c7be 48                         PHA
  1454  c7bf a5d7                       LDA z_tmp	; get saved character
  1455  c7c1 3012                       BMI char_inverse
  1456                          
  1457                          char_normal
  1458  c7c3 c920                       CMP #$20	; control character?
  1459  c7c5 9054                       BCC char_disp_leave
  1460  c7c7 c960                       CMP #$60
  1461  c7c9 9004                       BCC +
  1462  c7cb 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1463  c7cd d014                       BNE char_hires
  1464  c7cf 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1465  c7d1 d010               	BNE char_hires
  1466  c7d3 f00e               	BEQ char_hires
  1467                          
  1468                          char_inverse
  1469  c7d5 297f                       AND #%01111111	; mask bit 7
  1470  c7d7 c97f                       CMP #%01111111	; was 255? (pi)
  1471  c7d9 d002                       BNE +
  1472  c7db a95e                       LDA #$5E	; screen code for pi
  1473  c7dd c920               +	CMP #$20	; control character?
  1474  c7df 903a                       BCC char_disp_leave
  1475                          			; yes, skip
  1476  c7e1 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1477                          			; $C0-$FF -> $40-$7F
  1478                          			; OPT: BNE char_hires
  1479                          			; OPT: char_normal
  1480                          char_hires
  1481  c7e3 a6c7                       LDX z_reverseflag
  1482  c7e5 f002                       BEQ +
  1483  c7e7 0980                       ORA #%10000000	; invert char.
  1484  c7e9 aa                 +	TAX		; save char. for later
  1485  c7ea a501                       LDA prozport	; save prozport state
  1486  c7ec 48                 	PHA
  1487  c7ed a921                       LDA #$21	; char. rom, no basic and kernal rom
  1488  c7ef 78                         SEI
  1489  c7f0 8501                       STA prozport	; char. rom base = $D000
  1490  c7f2 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1491  c7f4 85fc                       STA gpos+1	; 
  1492  c7f6 8a                         TXA		; char. code
  1493  c7f7 0a                         ASL		; *8
  1494  c7f8 26fc                       ROL gpos+1
  1495  c7fa 0a                         ASL
  1496  c7fb 26fc                       ROL gpos+1
  1497  c7fd 0a                         ASL
  1498  c7fe 26fc                       ROL gpos+1
  1499  c800 85fb                       STA gpos	; addr. in char. rom for char.
  1500                          
  1501  c802 a007                       LDY #$07	; 8 hires lines
  1502                          char_line
  1503  c804 b1fb                       LDA (gpos),Y	; read character line
  1504  c806 20e703                     JSR gmask	; write to hires screen
  1505  c809 88                         DEY
  1506  c80a 10f8                       BPL char_line
  1507                          
  1508  c80c 68                 	PLA
  1509  c80d 8501                       STA prozport
  1510  c80f 58                         CLI
  1511                          
  1512  c810 18                         CLC		; step char position to left
  1513  c811 a5a5                       LDA gaddr	; ( +8 )
  1514  c813 6908                       ADC #$08
  1515  c815 85a5                       STA gaddr
  1516  c817 9002                       BCC +
  1517  c819 e6a6                       INC gaddr+1
  1518                          +
  1519                          char_disp_leave
  1520  c81b 68                 	PLA		; pass written character back
  1521  c81c a8                         TAY		; restore saved registers
  1522  c81d 68                         PLA
  1523  c81e aa                         TAX
  1524  c81f 60                         RTS
  1525                          
  1526                          
  1527                          ;-----------------------------------------------------------------
  1528                          
  1529                          to
  1530  c820 ad3a03                     LDA savexl
  1531  c823 859b                       STA xl
  1532  c825 ad3b03                     LDA savexh
  1533  c828 859c                       STA xh
  1534  c82a ad3c03                     LDA savey
  1535  c82d 85aa                       STA y
  1536  c82f 20aec3                     JSR getxy
  1537  c832 4c52c5                     JMP line_start
  1538                          
  1539                          ;-----------------------------------------------------------------
  1540                          
  1541                          unnew
  1542                          
  1543  c835 a52b               	lda bassta
  1544  c837 8522               	sta str
  1545  c839 a52c               	lda bassta+1
  1546  c83b 8523               	sta str+1
  1547  c83d a001               	ldy #1
  1548  c83f 98                 	tya
  1549  c840 9122               	sta (str),y		; != 0
  1550                          
  1551  c842 2033a5             	jsr b_rechain		; starting from bassta
  1552                          				; result in (str)
  1553  c845 18                 	clc			; str+1 -> new basic end
  1554  c846 a423               	ldy str+1
  1555  c848 a522               	lda str
  1556  c84a 6902               	adc #2
  1557  c84c 852d               	sta basend
  1558  c84e 9001               	bcc +
  1559  c850 c8                 	iny
  1560  c851 842e               +	sty basend+1
  1561  c853 4c60a6             	jmp b_clr		; perform CLR
  1562                          
  1563                          ;-----------------------------------------------------------------
  1564                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
