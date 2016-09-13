
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
   176  c025 a97d                       LDA #<(parse)	; basic interpreter parser hook
   177  c027 8d0803                     STA v_bascmd
   178  c02a a9c0                       LDA #>(parse)
   179  c02c 8d0903                     STA v_bascmd+1
   180                          
   181  c02f ad2803                     LDA v_basstp
   182  c032 8d3a03             	STA savevstp
   183  c035 a971                       LDA #<(stop)	; basic interpreter stop hook
   184  c037 8d2803                     STA v_basstp
   185  c03a ad2903                     LDA v_basstp+1
   186  c03d 8d3b03             	STA savevstp+1
   187  c040 a9c0                       LDA #>(stop)
   188  c042 8d2903                     STA v_basstp+1
   189                          
   190  c045 ad0003                     LDA v_baserr
   191  c048 8d3803             	STA saveverr
   192  c04b a96b                       LDA #<(error)	; basic interpreter error hook
   193  c04d 8d0003                     STA v_baserr
   194  c050 ad0103                     LDA v_baserr+1
   195  c053 8d3903             	STA saveverr+1
   196  c056 a9c0                       LDA #>(error)
   197  c058 8d0103                     STA v_baserr+1
   198                          
   199  c05b a200               	LDX #0		; set graphic cursor to (0,0)
   200  c05d 8e3403             	STX savexl
   201  c060 8e3503             	STX savexh
   202  c063 8e3603             	STX savey
   203  c066 e8                 	INX
   204  c067 8e3703             	STX savemo	; set mode 1
   205  c06a 60                         RTS
   206                          
   207                          error	
   208                          	; reg A may destroyed
   209  c06b 2041c1             	JSR gra_off		; uses only reg A
   210  c06e 6c3803             	JMP (saveverr)		; to original vector
   211                          
   212                          stop	
   213                          	; reg A may destroyed
   214  c071 a591               	LDA $91			; Scan code
   215  c073 c97f               	CMP #$7F		; STOP key?
   216  c075 d003               	BNE nostop
   217  c077 2041c1             	JSR gra_off		; uses only reg A
   218                          nostop
   219  c07a 6c3a03             	JMP (savevstp)		; to original vector
   220                          
   221                          ;-----------------------------------------------------------------
   222                          
   223                          ; start parsing an extension command ...
   224                          
   225                          parse
   226  c07d 207300                     JSR chrget			; next char.
   227  c080 08                 	PHP
   228  c081 c926                       CMP #'&'			; command prefix
   229  c083 f004                       BEQ newcmd
   230  c085 28                         PLP
   231  c086 4ce7a7                     JMP b_execstatement
   232                          newcmd
   233  c089 28                 	PLP
   234  c08a 207300                     JSR chrget			; command character
   235                          
   236  c08d a00c                       LDY #(cmdsend-cmds)		; map character to
   237                          					; command address ...
   238                          checknextcmd
   239  c08f 88                         DEY
   240  c090 f01c               	BEQ parse_error
   241  c092 d9b1c0                     CMP cmds,Y
   242  c095 d0f8                       BNE checknextcmd		; try next
   243  c097 88                         DEY				; found
   244  c098 98                         TYA
   245  c099 0a                         ASL				; *2
   246  c09a a8                         TAY
   247                          !ifndef command_rts_tyle {
   248                          	!set co=0			; command offset in jump table
   249  c09b b9bec0                     LDA cmdaddr+1,Y                 ; high byte from table
   250  c09e 8556                       STA ijmp+1
   251  c0a0 b9bdc0                     LDA cmdaddr,Y                   ; low byte from table
   252  c0a3 8555                       STA ijmp
   253  c0a5 207300                     JSR chrget			; read next byte in basic text
   254  c0a8 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   255  c0ab 4caea7                     JMP b_interpreter		; continue parsing
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
   270  c0ae 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   271                          
   272                          ;-----------------------------------------------------------------
   273                          
   274                          ; the most commonly used command placed at the end ...
   275                          
   276  c0b1 20554743534d5254...cmds	!text " UGCSMRTVHLP"		; first char. is a dummy
   277                          cmdsend
   278                          
   279                          cmdaddr
   280  c0bd c6c73ac1fdc6f9c5...        !word unnew-co,graphic-co,char-co,setmode-co,move-co,relto-co
   281  c0c9 b1c797c48ec3fdc4...        !word to-co,vline-co,hline-co,line-co,plot-co
   282                          
   283  c0d3 934752412d455854...author	!text 147,"GRA-EXT V"
   284  c0dd 312e3235           	+version
   285  c0e1 20313938362c3230...	!text " 1986,2016 JOHANN@KLASEK.AT",0
   286                          
   287                          bitmask
   288  c0fd 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   289                          nbitmask
   290  c105 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   291                          ytabl
   292  c10d 004080c0           	!byte $00,$40,$80,$c0
   293                          ytabh
   294  c111 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   295  c115 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   296  c119 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   297  c11d eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   298  c121 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   299  c125 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   300  c129 fe                 	!byte gramp+$1e
   301                          
   302                          ; for horiz. line
   303                          
   304  c12a ff7f3f1f0f070301   maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   305                          
   306  c132 80c0e0f0f8fcfeff   maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   307                          
   308                          
   309                          ;-----------------------------------------------------------------
   310                          
   311                          graphic
   312  c13a 209eb7                     JSR b_get8bit
   313  c13d e000                       CPX #$00
   314  c13f d013                       BNE gra_other
   315                          gra0			; &G 0
   316                          gra_off
   317  c141 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   318  c143 8d00dd                     STA cia_pra
   319  c146 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   320                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   321                          			; char addr $1000/4096 = char. ROM
   322  c148 8d18d0                     STA vic_mcr	; VIC memory control
   323  c14b ad11d0                     LDA vic_cr	; VIC control register
   324  c14e 29df                       AND #%11011111	; Hires mode off
   325  c150 8d11d0                     STA vic_cr
   326  c153 60                         RTS
   327                          
   328                          gra_other
   329  c154 e001                       CPX #$01
   330  c156 f00f               	BEQ gra1
   331  c158 e002               	CPX #$02
   332  c15a f00e                       BEQ gra2
   333  c15c e004               	CPX #$04
   334  c15e f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   335  c160 e003               	CPX #$03	; &G 3 (graphic on)
   336  c162 f029               	BEQ gra_on
   337  c164 4c48b2                     JMP b_illquant	; parameter illegal
   338                          	
   339                          gra1			; &G 1
   340  c167 20a3c1             	JSR gra_clear
   341                          
   342                          gra2
   343  c16a 20f1b7                     JSR b_getcomma8bit
   344  c16d 8a                         TXA		; foreground color
   345  c16e 0a                         ASL		; upper nibble
   346  c16f 0a                         ASL
   347  c170 0a                         ASL
   348  c171 0a                         ASL
   349  c172 85fd                       STA gcol
   350  c174 20f1b7                     JSR b_getcomma8bit
   351  c177 8a                         TXA		; background color
   352  c178 290f                       AND #$0F
   353  c17a 05fd                       ORA gcol
   354  c17c a000                       LDY #$00
   355                          cram_loop
   356  c17e 9900cc                     STA cram,Y	; fill color RAM
   357  c181 9900cd                     STA cram+$100,Y
   358  c184 9900ce                     STA cram+$200,Y
   359  c187 99e8ce                     STA cram+$300-24,Y
   360  c18a c8                         INY
   361  c18b d0f1                       BNE cram_loop
   362                          
   363                          gra_on
   364  c18d 20c2c1             	JSR gra_setupcode
   365                          
   366  c190 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   367  c192 8d00dd                     STA cia_pra
   368  c195 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   369  c197 8d18d0                     STA vic_mcr	; VIC memory control
   370  c19a ad11d0                     LDA vic_cr	; VIC control register
   371  c19d 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   372  c19f 8d11d0                     STA vic_cr
   373  c1a2 60                         RTS
   374                          
   375                          gra_clear
   376  c1a3 a220                       LDX #$20	; Pages (8 KByte)
   377  c1a5 a9e0                       LDA #>gram
   378  c1a7 85fc                       STA gpos+1
   379  c1a9 a000                       LDY #$00
   380  c1ab 84fb                       STY gpos
   381  c1ad 98                         TYA
   382                          gra_fill
   383  c1ae 91fb                       STA (gpos),Y	; Loop unroll
   384  c1b0 c8                         INY
   385  c1b1 91fb                       STA (gpos),Y
   386  c1b3 c8                         INY
   387  c1b4 91fb                       STA (gpos),Y
   388  c1b6 c8                         INY
   389  c1b7 91fb                       STA (gpos),Y
   390  c1b9 c8                         INY
   391  c1ba d0f2                       BNE gra_fill
   392  c1bc e6fc                       INC gpos+1
   393  c1be ca                         DEX
   394  c1bf d0ed                       BNE gra_fill
   395  c1c1 60                 	RTS
   396                          
   397                          gra_setupcode
   398  c1c2 a229               	LDX #(gromcode_end-gromcode) ; count of bytes
   399                          gra_copycode
   400  c1c4 bde5c1             	LDA gromcode-1,X
   401  c1c7 9dd203             	STA gramcode-1,X
   402  c1ca ca                 	DEX
   403  c1cb d0f7               	BNE gra_copycode
   404  c1cd ad3703             	LDA savemo
   405  c1d0 290f               	AND #$0F
   406  c1d2 aa                 	TAX
   407  c1d3 4c1fc6             	JMP setmode_enter	; re-apply mode to routines
   408                          				; implicit RTS
   409                          
   410                          ;-----------------------------------------------------------------
   411                          
   412                          gexit
   413  c1d6 a501                       LDA prozport
   414  c1d8 0902                       ORA #%00000010	; kernal ROM enable
   415  c1da 8501                       STA prozport
   416  c1dc 58                         CLI		; allow interrupts
   417  c1dd 60                         RTS
   418                          
   419                          ;-----------------------------------------------------------------
   420                          
   421                          ginit
   422  c1de a501                       LDA prozport
   423  c1e0 29fd                       AND #%11111101	; Kernal ROM disable
   424  c1e2 78                         SEI		; disable interrupts
   425  c1e3 8501                       STA prozport
   426  c1e5 60                         RTS
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
   443  c1e6 a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   444  c1e8 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   445                          } else {
   446                          	!fill 6, $ea
   447                          }
   448  c1ec b1a5                       LDA (gaddr),Y
   449                          gchange_op
   450  c1ee 1dfdc0                     ORA bitmask,X
   451  c1f1 91a5                       STA (gaddr),Y
   452                          !ifdef ltc {
   453  c1f3 a910               	LDA #mc_sim		; vollständige ROM-Simulation
   454  c1f5 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   455                          } else {
   456                          	!fill 6, $ea
   457                          }
   458  c1f9 60                         RTS
   459                          
   460                          ; mask a graphic location 
   461                          
   462                          gmask
   463                          !ifdef ltc {
   464  c1fa eb                 	XBA
   465  c1fb a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   466  c1fd 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   467  c201 eb                 	XBA
   468                          } else {
   469                          	!fill 8, $ea
   470                          }
   471                          gmask_flip
   472  c202 4900                       EOR #$00
   473                          gmask_op
   474  c204 11a5                       ORA (gaddr),Y
   475  c206 91a5                       STA (gaddr),Y
   476                          !ifdef ltc {
   477  c208 a910               	LDA #mc_sim		; vollständige ROM-Simulation
   478  c20a 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   479                          } else {
   480                          	!fill 6, $ea
   481                          }
   482  c20e 60                         RTS
   483                          
   484                          }
   485                          
   486                          gromcode_end
   487                          
   488                          ;-----------------------------------------------------------------
   489                          
   490                          position
   491  c20f a5aa                       LDA y
   492  c211 4a                         LSR
   493  c212 4a                         LSR
   494  c213 4a                         LSR		; y/8
   495  c214 a8                         TAY
   496  c215 2903                       AND #%00000011	; (y/8) mod 4
   497  c217 aa                         TAX
   498  c218 a59b                       LDA xl		; x low
   499  c21a 29f8                       AND #%11111000	; clear bit 2-0
   500  c21c 18                         CLC
   501  c21d 7d0dc1                     ADC ytabl,X	; addr low: y base + x part
   502  c220 85a5                       STA gaddr
   503  c222 a59c                       LDA xh		; addr high: x part
   504  c224 7911c1                     ADC ytabh,Y	; 	+ y base
   505  c227 85a6                       STA gaddr+1
   506  c229 a5aa                       LDA y		; vertical offset
   507  c22b 2907                       AND #%00000111	; y mod 8
   508  c22d a8                         TAY
   509  c22e a59b                       LDA xl
   510  c230 2907                       AND #%00000111	; x mod 8
   511  c232 aa                         TAX		; horizonal offset
   512  c233 60                         RTS		; (bitmask)
   513                          
   514                          
   515                          ;-----------------------------------------------------------------
   516                          
   517                          ; line y up, x right, dx < dy (case 1)
   518                          
   519                          line_up_steep
   520  c234 200fc2                     JSR position	; x,y
   521                          loop_yup_xright
   522  c237 20d303                     JSR gchange	; pixel
   523                          
   524  c23a 18                         CLC		; k += dx
   525  c23b a595                       LDA kl
   526  c23d 65ab                       ADC dxl		; dxh is 0, because dx < dy
   527  c23f 8595                       STA kl
   528  c241 b004                       BCS ++		; k > 255
   529                          
   530  c243 c5a9                       CMP dy
   531  c245 9015                       BCC +		; k >= dy ->
   532                          
   533  c247 e5a9               ++	SBC dy		; k -= dy
   534  c249 8595                       STA kl
   535                          
   536  c24b e8                         INX		; x++
   537  c24c e008                       CPX #8
   538  c24e d00c                       BNE +
   539                          	; C=1
   540  c250 a200                       LDX #0		; x overflow, wrap around
   541  c252 a5a5                       LDA gaddr	; x+8: gaddr += 8
   542  c254 6907                       ADC #8-1	; C already set by CPX
   543  c256 85a5                       STA gaddr
   544  c258 9002                       BCC +
   545  c25a e6a6                       INC gaddr+1
   546                          
   547  c25c 88                 +	DEY		; y--
   548  c25d 100f                       BPL +++
   549  c25f 38                         SEC		; y overflow
   550  c260 a5a5                       LDA gaddr
   551  c262 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   552  c264 85a5                       STA gaddr
   553  c266 a5a6                       LDA gaddr+1
   554  c268 e901               	SBC #1
   555  c26a 85a6                       STA gaddr+1
   556  c26c a007                       LDY #7		; wrap around
   557                          
   558  c26e c6a3               +++	DEC cl		; until c=0
   559  c270 d0c5                       BNE loop_yup_xright
   560  c272 4cd6c1                     JMP gexit
   561                          
   562                          
   563                          ;-----------------------------------------------------------------
   564                          
   565                          ; line x right, y up, dx > dy (case 2)
   566                          
   567                          line_up_flat
   568  c275 200fc2                     JSR position	; x,y
   569  c278 a5a3               	LDA cl		; counter adjustment for
   570  c27a f002               	BEQ +		; dec-dec-counting
   571  c27c e6a4               	INC ch
   572                          +
   573                          loop_xright_yup
   574  c27e 20d303                     JSR gchange	; pixel
   575                          
   576  c281 18                         CLC		; k += dy
   577  c282 a595                       LDA kl
   578  c284 65a9                       ADC dy
   579  c286 8595                       STA kl
   580  c288 9002                       BCC ++
   581  c28a e696                       INC kh
   582                          
   583  c28c c5ab               ++	CMP dxl		; k > dx?
   584  c28e a596                       LDA kh
   585  c290 e5a7                       SBC dxh
   586  c292 901a                       BCC +
   587                          
   588  c294 8596                       STA kh		; k -= dx
   589  c296 a595                       LDA kl
   590  c298 e5ab                       SBC dxl
   591  c29a 8595                       STA kl
   592                          
   593  c29c 88                         DEY		; y--
   594  c29d 100f                       BPL +
   595  c29f 38                 	SEC		; C=1 not always true (SBC above)
   596  c2a0 a5a5                       LDA gaddr	; y overflow
   597  c2a2 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   598  c2a4 85a5                       STA gaddr
   599  c2a6 a5a6                       LDA gaddr+1
   600  c2a8 e901               	SBC #1
   601  c2aa 85a6                       STA gaddr+1
   602  c2ac a007               	LDY #7		; wrap around
   603                          
   604  c2ae e8                 +	INX		; x++
   605  c2af e008                       CPX #8		; x overflow?
   606  c2b1 d00c                       BNE ++
   607                          	; C=1
   608  c2b3 a200                       LDX #0		; wrap around
   609  c2b5 a5a5                       LDA gaddr	; x+8: gaddr += 8
   610  c2b7 6907                       ADC #8-1	; C already set by CPX
   611  c2b9 85a5                       STA gaddr
   612  c2bb 9002                       BCC ++
   613  c2bd e6a6                       INC gaddr+1
   614                          ++
   615  c2bf c6a3               	DEC cl		; c--
   616  c2c1 d0bb                       BNE loop_xright_yup
   617  c2c3 c6a4                       DEC ch		; adjusted high which allows this
   618  c2c5 d0b7                       BNE loop_xright_yup
   619                          
   620  c2c7 4cd6c1                     JMP gexit
   621                          
   622                          
   623                          
   624                          ;-----------------------------------------------------------------
   625                          
   626                          ; line x right, y down, dx > dy (case 3)
   627                          
   628                          line_down_flat
   629  c2ca 200fc2                     JSR position	; x,y
   630  c2cd a5a3               	LDA cl		; counter adjustment for
   631  c2cf f002               	BEQ +		; dec-dec-counting
   632  c2d1 e6a4               	INC ch
   633                          +
   634                          loop_xright_ydown
   635  c2d3 20d303                     JSR gchange	; pixel
   636                          
   637  c2d6 18                         CLC		; k += dy
   638  c2d7 a595                       LDA kl
   639  c2d9 65a9                       ADC dy
   640  c2db 8595                       STA kl
   641  c2dd 9002                       BCC ++
   642  c2df e696                       INC kh
   643                          
   644  c2e1 c5ab               ++	CMP dxl		; k > dx
   645  c2e3 a596                       LDA kh
   646  c2e5 e5a7                       SBC dxh		; k -= dx
   647  c2e7 901b                       BCC +
   648                          
   649  c2e9 8596                       STA kh
   650  c2eb a595                       LDA kl
   651  c2ed e5ab                       SBC dxl
   652  c2ef 8595                       STA kl
   653                          
   654  c2f1 c8                         INY		; y++
   655  c2f2 c008                       CPY #8
   656  c2f4 d00e                       BNE +
   657                          	; C=1
   658  c2f6 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   659  c2f8 693f                       ADC #$40-1	; C already set by CPY
   660  c2fa 85a5                       STA gaddr
   661  c2fc a5a6                       LDA gaddr+1
   662  c2fe 6901               	ADC #1
   663  c300 85a6                       STA gaddr+1
   664  c302 a000                       LDY #0		; wrap around
   665                          
   666  c304 e8                 +	INX		; x++
   667  c305 e008                       CPX #8		; x overflow ?
   668  c307 d00c                       BNE +++
   669                          	; C=1
   670  c309 a200                       LDX #$00	; wrap around
   671  c30b a5a5                       LDA gaddr	; gaddr += 8
   672  c30d 6907                       ADC #$08-1	; C always set by CPX
   673  c30f 85a5                       STA gaddr
   674  c311 9002                       BCC +++
   675  c313 e6a6                       INC gaddr+1
   676                          +++
   677  c315 c6a3               	DEC cl		; c--
   678  c317 d0ba                       BNE loop_xright_ydown
   679  c319 c6a4                       DEC ch		; adjusted high which allows this
   680  c31b d0b6                       BNE loop_xright_ydown
   681                          
   682  c31d 4cd6c1                     JMP gexit
   683                          
   684                          
   685                          ;-----------------------------------------------------------------
   686                          
   687                          ; line y down, x right, dx < dy (case 4)
   688                          
   689                          line_down_steep
   690  c320 200fc2                     JSR position	; x,y
   691                          loop_ydown_xright
   692  c323 20d303                     JSR gchange	; pixel
   693                          
   694  c326 18                         CLC		; k += dx
   695  c327 a595                       LDA kl
   696  c329 65ab                       ADC dxl		; dxh is 0, because dx < dy
   697  c32b 8595                       STA kl
   698  c32d b004                       BCS ++
   699  c32f c5a9                       CMP dy		; k > dy?
   700  c331 9015                       BCC +
   701  c333 e5a9               ++	SBC dy		; k -= dy
   702  c335 8595                       STA kl
   703                          
   704  c337 e8                         INX		; x++
   705  c338 e008                       CPX #8
   706  c33a d00c                       BNE +		; x overflow?
   707  c33c a200                       LDX #0		; wrap around
   708  c33e a5a5                       LDA gaddr	; x+9: gaddr += 8
   709  c340 6907                       ADC #8-1	; C already set by CPX
   710  c342 85a5                       STA gaddr
   711  c344 9002                       BCC +
   712  c346 e6a6                       INC gaddr+1
   713                          
   714  c348 c8                 +	INY		; y++
   715  c349 c008                       CPY #8		; y overflow?
   716  c34b d00e                       BNE +++
   717  c34d a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   718  c34f 693f                       ADC #$40-1	; C already set by CPY
   719  c351 85a5                       STA gaddr
   720  c353 a5a6                       LDA gaddr+1
   721  c355 6901               	ADC #1
   722  c357 85a6                       STA gaddr+1
   723  c359 a000                       LDY #0		; wrap around
   724                          
   725  c35b c6a3               +++	DEC cl		; c--
   726                          			; until c=0
   727  c35d d0c4                       BNE loop_ydown_xright
   728  c35f 4cd6c1                     JMP gexit
   729                          
   730                          
   731                          ;-----------------------------------------------------------------
   732                          
   733                          getcommaxy
   734  c362 20fdae                     JSR b_getcomma	; check ","
   735                          getxy
   736  c365 208aad                     JSR b_getval	; get X coord. value
   737  c368 20f7b7                     JSR b_convint
   738  c36b c901                       CMP #>xmax
   739  c36d 900c               	BCC gcxy_xok
   740  c36f f003                       BEQ ++		; X = $1xx
   741  c371 20e6c5                     JSR range_error
   742                          
   743  c374 c040               ++	CPY #<xmax	; check X low
   744  c376 9003                       BCC +
   745  c378 20e6c5                     JSR range_error
   746                          +
   747                          gcxy_xok
   748  c37b 84fb                       STY gpos	; temporary save X coord.
   749  c37d 85fc                       STA gpos+1
   750                          
   751  c37f 20f1b7                     JSR b_getcomma8bit
   752                          			; get Y coord. value
   753  c382 e0c8                       CPX #ymax
   754  c384 9003                       BCC +
   755  c386 20e6c5                     JSR range_error
   756                          +
   757  c389 a4fb                       LDY gpos	; restory X coord.
   758  c38b a5fc                       LDA gpos+1
   759  c38d 60                         RTS
   760                          
   761                          
   762                          ;-----------------------------------------------------------------
   763                          
   764                          hline
   765  c38e 2065c3                     JSR getxy	; get startpoint
   766  c391 86aa                       STX y
   767  c393 8e3603                     STX savey	; save as cursor, too
   768  c396 859c                       STA xh
   769  c398 849b                       STY xl
   770  c39a 20fdae                     JSR b_getcomma	; get length
   771  c39d 208aad                     JSR b_getval
   772  c3a0 20f7b7                     JSR b_convint
   773                          			; calculate end point
   774  c3a3 aa                         TAX		; save length high byte
   775  c3a4 98                         TYA		; length low byte
   776  c3a5 18                         CLC
   777  c3a6 659b                       ADC xl		; low xend = x+length
   778  c3a8 859e                       STA xendl
   779  c3aa a8                 	TAY
   780  c3ab 8a                         TXA		; high
   781  c3ac 659c                       ADC xh		; high xend = x+length
   782  c3ae 859f                       STA xendh
   783  c3b0 aa                 	TAX
   784                          
   785  c3b1 c901               	CMP #>xmax	; endpoint outside?
   786  c3b3 900a               	BCC +
   787  c3b5 d005               	BNE ++		; >=$200
   788  c3b7 98                 	TYA
   789  c3b8 e940               	SBC #<xmax
   790  c3ba 9003               	BCC +
   791  c3bc 20e6c5             ++	JSR range_error
   792                          +
   793  c3bf 8e3503                     STX savexh
   794  c3c2 8c3403                     STY savexl	; also save as cursor
   795                          
   796  c3c5 a900               	LDA #0
   797  c3c7 85a3               	STA ycount
   798  c3c9 207900             	JSR $0079
   799  c3cc f012               	BEQ +
   800  c3ce 20f1b7             	JSR b_getcomma8bit
   801  c3d1 8a                 	TXA
   802  c3d2 85a3               	STA ycount
   803  c3d4 f00a               	BEQ +
   804  c3d6 18                 	CLC
   805  c3d7 65aa               	ADC y		; end position for y coord.
   806  c3d9 c9c8               	CMP #ymax
   807  c3db 9003               	BCC ++
   808  c3dd 20e6c5             	JSR range_error
   809                          ++
   810                          +
   811  c3e0 20dec1                     JSR ginit	; map in graphic memory
   812  c3e3 d01a               	BNE hl_noxswap	; ginit left with Z=0
   813                          
   814                          hline_start
   815  c3e5 a59e                       LDA xendl
   816  c3e7 c59b                       CMP xl
   817  c3e9 a59f                       LDA xendh
   818  c3eb e59c                       SBC xh
   819  c3ed b010                       BCS hl_noxswap	; xend < x ->
   820                          
   821  c3ef a69e                       LDX xendl	; swap x, xend
   822  c3f1 a59b                       LDA xl
   823  c3f3 869b                       STX xl
   824  c3f5 859e                       STA xendl
   825                          
   826  c3f7 a69f                       LDX xendh
   827  c3f9 a49c                       LDY xh
   828  c3fb 849f                       STY xendh
   829  c3fd 869c                       STX xh
   830                          hl_noxswap
   831  c3ff e6a3               	INC ycount
   832                          hl_start
   833  c401 200fc2                     JSR position	; graphic position x,y
   834                          
   835  c404 a5a5               	LDA gaddr	; save position for vertical
   836  c406 85fb               	STA sgaddr
   837  c408 a5a6               	LDA gaddr+1
   838  c40a 85fc               	STA sgaddr+1
   839  c40c 86fd               	STX xsave
   840  c40e 84a9               	STY ysave
   841                          
   842  c410 a59e                       LDA xendl
   843  c412 2907                       AND #%00000111
   844  c414 8596                       STA tmp2	; xend mod 8, mask index
   845  c416 a59b                       LDA xl
   846  c418 29f8                       AND #%11111000	; (xl div 8)*8
   847  c41a 8595                       STA tmp1
   848  c41c a59e                       LDA xendl	; xend unmasked
   849  c41e 38                         SEC
   850  c41f e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   851  c421 8595                       STA tmp1
   852  c423 a59f                       LDA xendh
   853  c425 e59c                       SBC xh
   854  c427 4a                         LSR		; / 8 ->  0-39
   855  c428 a595                       LDA tmp1	; only 1 highest bit
   856  c42a 6a                         ROR		; and 3 lower bits
   857  c42b 4a                         LSR
   858  c42c 4a                         LSR
   859                                  		; 8-pixel-blocks count
   860  c42d 85a4               	STA hcount	; save for vertical extension
   861                           
   862                          hl_vertloop
   863  c42f 98                 	TYA		; calculate max. Y in 8x8 block
   864  c430 18                 	CLC
   865  c431 65a3               	ADC ycount
   866  c433 c908               	CMP #8
   867  c435 9002               	BCC +
   868  c437 a908               	LDA #8
   869  c439 85a8               +	STA ylimit
   870                          
   871  c43b bd2ac1                     LDA maskleft,X	; starting mask
   872  c43e 8595               	STA tmp1
   873  c440 a6a4               	LDX hcount	; how many blocks
   874                          
   875                          hl_nextblock
   876  c442 ca                         DEX
   877                          hl_islastblock
   878  c443 301d                       BMI hl_lastblock
   879                          			; leave loop if X<0
   880  c445 a4a9               	LDY ysave
   881  c447 a595               -	LDA tmp1	; mask
   882  c449 20e703             	JSR gmask	; first with left end mask
   883  c44c c8                 	INY		; vertical down
   884  c44d c4a8               	CPY ylimit	; in 8x8 box
   885  c44f d0f6               	BNE -
   886                          
   887  c451 18                         CLC		; gaddr += 8 (one block to right)
   888  c452 a5a5                       LDA gaddr
   889  c454 6908                       ADC #8
   890  c456 85a5                       STA gaddr
   891  c458 9002                       BCC +
   892  c45a e6a6                       INC gaddr+1
   893                          
   894  c45c a9ff               +	LDA #$FF	; following with full 8-pixel mask
   895  c45e 8595               	STA tmp1
   896  c460 d0e0               	BNE hl_nextblock	; always
   897                          
   898                          hl_lastblock
   899  c462 a696                       LDX tmp2	; xend mask index
   900  c464 3d32c1                     AND maskright,X ; A has current maskt combine with mask right end
   901  c467 8595               	STA tmp1	; mask
   902  c469 a4a9               	LDY ysave	; start position in 8x8 block
   903  c46b a595               -	LDA tmp1	; mask
   904  c46d 20e703             	JSR gmask	; modify
   905  c470 c8                 	INY		; vertical down
   906  c471 c6a3               	DEC ycount	; overall y counter
   907  c473 c4a8               	CPY ylimit
   908  c475 d0f4               	BNE -
   909                          
   910  c477 a5a3               	LDA ycount	; finished
   911  c479 d003               	BNE +
   912  c47b 4cd6c1                     JMP gexit	; leave
   913                          
   914  c47e 18                 +	CLC
   915  c47f a5fb               	LDA sgaddr
   916  c481 6940               	ADC #$40	; next 8-pixel row
   917  c483 85fb               	STA sgaddr	; + $140 (320)
   918  c485 85a5               	STA gaddr
   919  c487 a5fc               	LDA sgaddr+1
   920  c489 6901               	ADC #$01
   921  c48b 85fc               	STA sgaddr+1
   922  c48d 85a6               	STA gaddr+1
   923  c48f a6fd               	LDX xsave
   924  c491 a000               	LDY #0
   925  c493 84a9               	STY ysave
   926  c495 f098               	BEQ hl_vertloop
   927                          ;-----------------------------------------------------------------
   928                          
   929                          vline
   930  c497 2065c3                     JSR getxy	; get startpoint
   931  c49a 859c                       STA xh
   932  c49c 8d3503                     STA savexh	; save as cursor too
   933  c49f 849b                       STY xl
   934  c4a1 8c3403                     STY savexl
   935  c4a4 86aa                       STX y
   936                          
   937  c4a6 20f1b7                     JSR b_getcomma8bit
   938                          			; get length
   939  c4a9 18                         CLC		; calculate end point
   940  c4aa 8a                         TXA		; length
   941                          ; DON'T-CHANGE: how long to go vertically (needed later)
   942                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   943                          ;	STA tmp1
   944  c4ab 65aa                       ADC y		; length + y
   945  c4ad c9c8                       CMP #ymax	; outside?
   946  c4af 9003                       BCC +
   947                          vline_iq
   948  c4b1 20e6c5                     JSR range_error
   949  c4b4 8593               +	STA yend	; endpoint
   950                          
   951  c4b6 8d3603             	STA savey	; set cursor y position
   952  c4b9 20dec1                     JSR ginit	; map in graphic memory
   953  c4bc d012               	BNE vl_start	; ginit left with Z=0
   954                          
   955                          vline_start
   956  c4be a593                       LDA yend
   957  c4c0 c5aa                       CMP y
   958  c4c2 b00a                       BCS vl_noyswap	; yend < y ->
   959  c4c4 a5aa                       LDA y		; swap y, yend
   960  c4c6 a693                       LDX yend
   961  c4c8 8593                       STA yend
   962  c4ca 86aa                       STX y
   963  c4cc f002               	BEQ vl_start	; always (with next branch)
   964                          	; fall through if yend is
   965                          vl_noyswap
   966  c4ce d000                       BNE vl_start	; yend > y
   967                          ;	JMP plot_start	; y = yend -> single point
   968                          ;	JMP gexit	; no point
   969                          
   970                          vl_start
   971  c4d0 200fc2                     JSR position	; graphic position x,y
   972  c4d3 bdfdc0                     LDA bitmask,X
   973  c4d6 8596                       STA tmp2	; save mask
   974                          ; DON'T-CHANGE: replace ...
   975  c4d8 38                         SEC
   976  c4d9 a593                       LDA yend
   977  c4db e5aa                       SBC y		; vertical length
   978  c4dd aa                         TAX
   979                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
   980                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
   981                          ;	LDX tmp1
   982  c4de e8                         INX		; +1 (exit on 0)
   983                          vl_nextline
   984  c4df a596                       LDA tmp2
   985  c4e1 20e703                     JSR gmask	; modify 
   986  c4e4 c8                         INY		; go down
   987  c4e5 c008                       CPY #8		; 8-line wrap
   988  c4e7 d00e                       BNE +
   989  c4e9 a5a5                       LDA gaddr	; gaddr += 320
   990  c4eb 693f               	ADC #$40-1	; compensate for C = 1
   991  c4ed 85a5                       STA gaddr
   992  c4ef a5a6                       LDA gaddr+1
   993  c4f1 6901                       ADC #$01
   994  c4f3 85a6                       STA gaddr+1
   995  c4f5 a000                       LDY #0		; wrap y offset
   996  c4f7 ca                 +	DEX		; all vertical positions done?
   997  c4f8 d0e5                       BNE vl_nextline
   998  c4fa 4cd6c1                     JMP gexit	; leave
   999                          
  1000                          
  1001                          ;-----------------------------------------------------------------
  1002                          
  1003                          line
  1004  c4fd 2065c3                     JSR getxy	; get startpoint
  1005  c500 849b                       STY xl 
  1006  c502 859c                       STA xh
  1007  c504 86aa                       STX y
  1008                          
  1009  c506 2062c3                     JSR getcommaxy	; get endpoint
  1010                          line_start
  1011  c509 8c3403                     STY savexl	; save as cursor position too
  1012  c50c 849e                       STY xendl
  1013  c50e 8d3503                     STA savexh
  1014  c511 859f                       STA xendh
  1015  c513 8e3603                     STX savey
  1016  c516 8693                       STX yend
  1017                          
  1018  c518 20dec1                     JSR ginit	; map in graphic memory
  1019                          
  1020  c51b a000                       LDY #$00	; initialize to 0
  1021  c51d 84a8                       STY ydir
  1022  c51f 8495                       STY kl
  1023  c521 8496                       STY kh
  1024                          
  1025  c523 38                         SEC
  1026  c524 a59e                       LDA xendl	; calculate dx
  1027  c526 e59b                       SBC xl
  1028  c528 85ab                       STA dxl
  1029  c52a a59f                       LDA xendh
  1030  c52c e59c                       SBC xh
  1031  c52e 85a7                       STA dxh
  1032                          
  1033  c530 b025                       BCS li_xend_right
  1034                          	; dx != 0
  1035  c532 98                         TYA		; negate dx
  1036  c533 38                         SEC		; dx = 0 - dx
  1037  c534 e5ab                       SBC dxl
  1038  c536 85ab                       STA dxl
  1039  c538 98                         TYA
  1040  c539 e5a7                       SBC dxh
  1041  c53b 85a7                       STA dxh
  1042                          			; C=0 always, needed later
  1043  c53d a69b                       LDX xl		; swap x low
  1044  c53f a49e                       LDY xendl
  1045  c541 869e                       STX xendl
  1046  c543 849b                       STY xl
  1047                          
  1048  c545 a69c                       LDX xh		; swap x high
  1049  c547 a49f                       LDY xendh
  1050  c549 869f                       STX xendh
  1051  c54b 849c                       STY xh
  1052                          
  1053  c54d a6aa                       LDX y		; swap y
  1054  c54f a493                       LDY yend
  1055  c551 8693                       STX yend
  1056  c553 84aa                       STY y
  1057                          
  1058  c555 9009                       BCC li_x_different
  1059                          			; C=0 always (from negation before)
  1060                          
  1061                          li_xend_right
  1062  c557 a5ab                       LDA dxl		; dx = 0?
  1063  c559 05a7                       ORA dxh
  1064  c55b d003                       BNE li_x_different
  1065  c55d 4cbec4                     JMP vline_start	; vertical line case
  1066                          
  1067                          li_x_different
  1068  c560 38                         SEC		; calculate dy
  1069  c561 a593                       LDA yend
  1070  c563 e5aa                       SBC y
  1071  c565 b006                       BCS li_y_right
  1072  c567 49ff                       EOR #$FF	; negate dy (two's complement)
  1073  c569 6901                       ADC #$01	; C=0
  1074  c56b 85a8                       STA ydir	; flag y goes up
  1075                          
  1076                          li_y_right
  1077  c56d 85a9                       STA dy
  1078  c56f d007                       BNE +
  1079  c571 a900               	LDA #0
  1080  c573 85a3               	STA ycount
  1081  c575 4ce5c3                     JMP hline_start	; horizontal line case
  1082                          +
  1083                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1084                          
  1085  c578 a5a7                       LDA dxh		; dx > dy
  1086  c57a d017                       BNE line_flat	; yes -> flat
  1087  c57c a5a9                       LDA dy		; no -> steep
  1088  c57e aa                         TAX
  1089  c57f c5ab                       CMP dxl
  1090  c581 9010                       BCC line_flat
  1091                          
  1092                          line_steep
  1093  c583 e8                         INX	
  1094  c584 86a3                       STX cl		; c = dy+1
  1095  c586 4a                         LSR		; k = dy/2
  1096  c587 8595                       STA kl
  1097  c589 a5a8                       LDA ydir
  1098  c58b d003                       BNE +
  1099  c58d 4c20c3                     JMP line_down_steep	; y down, steep
  1100  c590 4c34c2             +	JMP line_up_steep	; y up, steep
  1101                          
  1102                          line_flat
  1103  c593 a5a7                       LDA dxh
  1104  c595 a8                         TAY
  1105  c596 a6ab                       LDX dxl
  1106  c598 e8                         INX
  1107  c599 d001                       BNE +
  1108  c59b c8                         INY
  1109  c59c 86a3               +	STX cl		; c = dx+1
  1110  c59e 84a4                       STY ch
  1111                          
  1112  c5a0 4a                         LSR		; k = dx/2
  1113  c5a1 8596                       STA kh
  1114  c5a3 a5ab                       LDA dxl
  1115  c5a5 6a                         ROR		; dx/2
  1116  c5a6 8595                       STA kl
  1117  c5a8 a5a8                       LDA ydir	
  1118  c5aa d003                       BNE +
  1119  c5ac 4ccac2                     JMP line_down_flat	; y down, flat
  1120  c5af 4c75c2             +	JMP line_up_flat	; y up, flat
  1121                          
  1122                          ;-----------------------------------------------------------------
  1123                          
  1124                          plot
  1125  c5b2 2065c3                     JSR getxy	; get parameter
  1126  c5b5 859c                       STA xh		; save x/y
  1127  c5b7 849b                       STY xl
  1128  c5b9 86aa                       STX y
  1129  c5bb 8d3503                     STA savexh	; and store as cursor
  1130  c5be 8c3403                     STY savexl
  1131  c5c1 8e3603                     STX savey
  1132                          
  1133                          plot_start
  1134  c5c4 200fc2                     JSR position	; calculate graphical address
  1135                          
  1136  c5c7 a501                       LDA prozport
  1137  c5c9 29fd                       AND #%11111101	; Kernal ROM disable
  1138  c5cb 78                         SEI			
  1139  c5cc 8501                       STA prozport
  1140                          
  1141  c5ce 20d303                     JSR gchange	; change graphical data
  1142                          
  1143  c5d1 a501                       LDA prozport
  1144  c5d3 0902                       ORA #%00000010	; kernal ROM enable
  1145  c5d5 8501                       STA prozport
  1146  c5d7 58                         CLI
  1147  c5d8 60                         RTS
  1148                          
  1149                          ;-----------------------------------------------------------------
  1150                          
  1151                          move
  1152  c5d9 2065c3                     JSR getxy	; get parameter
  1153  c5dc 8d3503                     STA savexh	; just save as cursor
  1154  c5df 8c3403                     STY savexl
  1155  c5e2 8e3603                     STX savey
  1156  c5e5 60                         RTS
  1157                          
  1158                          
  1159                          ;-----------------------------------------------------------------
  1160                          
  1161                          ; never touch X, Y
  1162                          range_error
  1163  c5e6 ad3703             	LDA savemo
  1164  c5e9 29f0               	AND #$F0
  1165  c5eb d003               	BNE +
  1166  c5ed 68                 	PLA			; cleanup JSR
  1167  c5ee 68                 	PLA
  1168  c5ef 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1169                          				; error mode 2: cut value: control back
  1170                          				; to handle value correction
  1171  c5f0 2920               +	AND #$20
  1172  c5f2 d0fb               	BNE -
  1173  c5f4 68                 	PLA			; cleanup JSR
  1174  c5f5 68                 	PLA
  1175                          setmode_error
  1176  c5f6 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1177                          
  1178                          ;-----------------------------------------------------------------
  1179                          
  1180                          setmode
  1181  c5f9 209eb7                     JSR b_get8bit
  1182  c5fc e003                       CPX #3
  1183  c5fe 9013                       BCC +			; less then 3, modification mode
  1184  c600 e006               	CPX #6
  1185  c602 b0f2               	BCS setmode_error	; out of range
  1186                          				; error mode
  1187  c604 8a                 	TXA
  1188  c605 690d               	ADC #13			; C=0, therefore -3
  1189                          				; 3-5 -> 16-18
  1190                          				; put A's bit 4-7 into savemo
  1191  c607 4d3703             	EOR savemo		; ********
  1192  c60a 29f0               	AND #$F0		; ****0000
  1193  c60c 4d3703             	EOR savemo		; AAAAmmmm
  1194  c60f 8d3703             	STA savemo		; 
  1195  c612 60                 	RTS
  1196                          
  1197  c613 8a                 +	TXA
  1198  c614 4d3703             	EOR savemo		; put A's bit 0-3 into savemo
  1199  c617 290f               	AND #$0F
  1200  c619 4d3703             	EOR savemo
  1201  c61c 8d3703             	STA savemo
  1202                          setmode_enter
  1203  c61f e001               	CPX #$01
  1204  c621 b01a                       BCS set_or_toggle
  1205                          
  1206                          modereset
  1207  c623 a9c1                       LDA #>(nbitmask)
  1208  c625 8ddd03                     STA gchange_op+2
  1209  c628 a905                       LDA #<(nbitmask)
  1210  c62a 8ddc03                     STA gchange_op+1
  1211  c62d a93d                       LDA #$3D		; AND abs,X
  1212  c62f 8ddb03                     STA gchange_op
  1213  c632 a931                       LDA #$31		; AND (zp),Y
  1214  c634 8df103                     STA gmask_op
  1215  c637 a9ff                       LDA #$FF		; EOR $#FF, invertieren
  1216  c639 8df003                     STA gmask_flip+1
  1217  c63c 60                         RTS
  1218                          
  1219                          set_or_toggle
  1220  c63d d01a                       BNE modetoggle
  1221                          modeset
  1222  c63f a9c0                       LDA #>(bitmask)
  1223  c641 8ddd03                     STA gchange_op+2
  1224  c644 a9fd                       LDA #<(bitmask)
  1225  c646 8ddc03                     STA gchange_op+1
  1226  c649 a91d                       LDA #$1D		; OR abs,X
  1227  c64b 8ddb03                     STA gchange_op
  1228  c64e a911                       LDA #$11		; OR (zp),Y
  1229  c650 8df103                     STA gmask_op
  1230  c653 a900                       LDA #$00		; EOR #$00, nicht invertieren
  1231  c655 8df003                     STA gmask_flip+1
  1232  c658 60                         RTS
  1233                          
  1234                          modetoggle
  1235  c659 a9c0                       LDA #>(bitmask)
  1236  c65b 8ddd03                     STA gchange_op+2
  1237  c65e a9fd                       LDA #<(bitmask)
  1238  c660 8ddc03                     STA gchange_op+1
  1239  c663 a95d                       LDA #$5D		; EOR abs,X
  1240  c665 8ddb03                     STA gchange_op
  1241  c668 a951                       LDA #$51		; EOR (zp),Y
  1242  c66a 8df103                     STA gmask_op
  1243  c66d a900                       LDA #$00		; EOR #$00, nicht invertieren
  1244  c66f 8df003                     STA gmask_flip+1
  1245  c672 60                         RTS
  1246                          
  1247                          
  1248                          ;-----------------------------------------------------------------
  1249                          
  1250                          ; get pixel (check if pixel set)
  1251                          ; not used
  1252                          
  1253                          get
  1254  c673 2062c3                     JSR getcommaxy
  1255  c676 859c                       STA xh
  1256  c678 849b                       STY xl
  1257  c67a 86aa                       STX y
  1258                          
  1259  c67c 200fc2                     JSR position
  1260                          
  1261  c67f a501                       LDA prozport
  1262  c681 29fd               	AND #%11111101	; Kernal ROM disable
  1263  c683 78                         SEI
  1264  c684 8501                       STA prozport
  1265                          
  1266  c686 b1a5                       LDA (gaddr),Y
  1267  c688 3dfdc0                     AND bitmask,X
  1268  c68b a8                         TAY
  1269  c68c a501                       LDA prozport
  1270  c68e 0902               	ORA #%00000010	; kernal ROM enable
  1271  c690 8501                       STA prozport
  1272  c692 58                         CLI
  1273  c693 4ca2b3                     JMP b_byte2fac
  1274                          
  1275                          
  1276                          ;-----------------------------------------------------------------
  1277                          
  1278                          relto
  1279  c696 208aad                     JSR b_getval	; get X offset (+/-)
  1280  c699 a561               	LDA facexp	; FAC exponent
  1281  c69b c990               	CMP #$90	; more than 16 bit
  1282  c69d b031               	BCS relto_error	; illegal quantity
  1283  c69f 209bbc                     JSR b_fac2int	; to signed integer
  1284                          
  1285  c6a2 18                         CLC
  1286  c6a3 a565                       LDA facintl
  1287  c6a5 6d3403                     ADC savexl
  1288  c6a8 859e                       STA xendl
  1289  c6aa a564                       LDA facinth
  1290  c6ac 6d3503                     ADC savexh
  1291  c6af 859f                       STA xendh	; xend = savex+facint
  1292                          
  1293  c6b1 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1294  c6b4 208aad                     JSR b_getval
  1295  c6b7 a561                       LDA facexp	; FAC exponent
  1296  c6b9 c990                       CMP #$90	; more than 16 bit
  1297  c6bb b013                       BCS relto_error	; illegal quantity
  1298  c6bd 209bbc                     JSR b_fac2int	; to signed integer
  1299  c6c0 18                         CLC
  1300  c6c1 a565                       LDA facintl
  1301  c6c3 6d3603                     ADC savey
  1302  c6c6 8593                       STA yend	; yend = savey+facint
  1303                          
  1304  c6c8 a59f                       LDA xendh	; check end coord. x
  1305  c6ca c901                       CMP #>xmax
  1306  c6cc 900e                       BCC rt_xok
  1307  c6ce f003                       BEQ +
  1308                          relto_error
  1309  c6d0 20e6c5                     JSR range_error
  1310  c6d3 a59e               +	LDA xendl
  1311  c6d5 c940                       CMP #<xmax
  1312  c6d7 9003                       BCC +
  1313  c6d9 20e6c5                     JSR range_error
  1314                          +
  1315                          rt_xok
  1316  c6dc a593                       LDA yend	; check end coord. y
  1317  c6de c9c8                       CMP #ymax
  1318  c6e0 9003                       BCC +
  1319  c6e2 20e6c5                     JSR range_error
  1320                          +
  1321  c6e5 ad3403                     LDA savexl
  1322  c6e8 859b                       STA xl
  1323  c6ea ad3503                     LDA savexh
  1324  c6ed 859c                       STA xh
  1325  c6ef ad3603                     LDA savey
  1326  c6f2 85aa                       STA y
  1327  c6f4 a49e                       LDY xendl
  1328  c6f6 a59f                       LDA xendh
  1329  c6f8 a693                       LDX yend	; xend/yend = cursor + x/y
  1330                          
  1331  c6fa 4c09c5                     JMP line_start	; draw line x/y to xend/yend
  1332                          
  1333                          
  1334                          ;-----------------------------------------------------------------
  1335                          
  1336                          char
  1337  c6fd 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1338  c700 e028                       CPX #40	
  1339  c702 9003                       BCC +
  1340                          char_error
  1341  c704 4c48b2                     JMP b_illquant
  1342  c707 86fb               +	STX gpos	; save x coord.
  1343  c709 20f1b7                     JSR b_getcomma8bit
  1344                          			; get char. position y 0-24
  1345  c70c e019                       CPX #25
  1346  c70e b0f4                       BCS char_error
  1347  c710 86fc                       STX gpos+1	; save y coord.
  1348                          
  1349  c712 20fdae                     JSR b_getcomma	; get string
  1350  c715 209ead                     JSR b_getexpr
  1351  c718 20a3b6                     JSR b_stringval ; string address in str
  1352  c71b 48                         PHA		; string length
  1353  c71c a6fc                       LDX gpos+1	; y coord. for char. position
  1354  c71e 8a                         TXA
  1355  c71f 2903                       AND #$03	; mask 2 bits
  1356  c721 a8                         TAY		; table index
  1357  c722 a900                       LDA #$00
  1358  c724 85fc                       STA gpos+1	; x high
  1359  c726 a5fb                       LDA gpos	; saved x: multiply by 8
  1360  c728 0a                         ASL
  1361  c729 0a                         ASL
  1362  c72a 0a                         ASL
  1363  c72b 26fc                       ROL gpos+1	; overflow to high byte
  1364  c72d 790dc1                     ADC ytabl,Y
  1365  c730 85a5                       STA gaddr
  1366  c732 a5fc                       LDA gpos+1	; x high
  1367  c734 7d11c1                     ADC ytabh,X
  1368  c737 85a6                       STA gaddr+1
  1369  c739 68                         PLA		; string length
  1370  c73a a000                       LDY #$00	; string index
  1371  c73c aa                         TAX		; length
  1372  c73d e8                         INX		; prepare as counter
  1373                          char_loop
  1374  c73e ca                         DEX
  1375  c73f f008                       BEQ char_exit
  1376  c741 b122                       LDA (str),Y	; read string
  1377  c743 204ac7                     JSR char_display
  1378  c746 c8                         INY
  1379  c747 d0f5                       BNE char_loop
  1380                          char_exit
  1381  c749 60                         RTS
  1382                          
  1383                          char_display
  1384  c74a 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1385  c74c 8a                         TXA		; save register X+Y
  1386  c74d 48                         PHA
  1387  c74e 98                         TYA
  1388  c74f 48                         PHA
  1389  c750 a5d7                       LDA z_tmp	; get saved character
  1390  c752 3012                       BMI char_inverse
  1391                          
  1392                          char_normal
  1393  c754 c920                       CMP #$20	; control character?
  1394  c756 9054                       BCC char_disp_leave
  1395  c758 c960                       CMP #$60
  1396  c75a 9004                       BCC +
  1397  c75c 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1398  c75e d014                       BNE char_hires
  1399  c760 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1400  c762 d010               	BNE char_hires
  1401  c764 f00e               	BEQ char_hires
  1402                          
  1403                          char_inverse
  1404  c766 297f                       AND #%01111111	; mask bit 7
  1405  c768 c97f                       CMP #%01111111	; was 255? (pi)
  1406  c76a d002                       BNE +
  1407  c76c a95e                       LDA #$5E	; screen code for pi
  1408  c76e c920               +	CMP #$20	; control character?
  1409  c770 903a                       BCC char_disp_leave
  1410                          			; yes, skip
  1411  c772 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1412                          			; $C0-$FF -> $40-$7F
  1413                          			; OPT: BNE char_hires
  1414                          			; OPT: char_normal
  1415                          char_hires
  1416  c774 a6c7                       LDX z_reverseflag
  1417  c776 f002                       BEQ +
  1418  c778 0980                       ORA #%10000000	; invert char.
  1419  c77a aa                 +	TAX		; save char. for later
  1420  c77b a501                       LDA prozport	; save prozport state
  1421  c77d 48                 	PHA
  1422  c77e a921                       LDA #$21	; char. rom, no basic and kernal rom
  1423  c780 78                         SEI
  1424  c781 8501                       STA prozport	; char. rom base = $D000
  1425  c783 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1426  c785 85fc                       STA gpos+1	; 
  1427  c787 8a                         TXA		; char. code
  1428  c788 0a                         ASL		; *8
  1429  c789 26fc                       ROL gpos+1
  1430  c78b 0a                         ASL
  1431  c78c 26fc                       ROL gpos+1
  1432  c78e 0a                         ASL
  1433  c78f 26fc                       ROL gpos+1
  1434  c791 85fb                       STA gpos	; addr. in char. rom for char.
  1435                          
  1436  c793 a007                       LDY #$07	; 8 hires lines
  1437                          char_line
  1438  c795 b1fb                       LDA (gpos),Y	; read character line
  1439  c797 20e703                     JSR gmask	; write to hires screen
  1440  c79a 88                         DEY
  1441  c79b 10f8                       BPL char_line
  1442                          
  1443  c79d 68                 	PLA
  1444  c79e 8501                       STA prozport
  1445  c7a0 58                         CLI
  1446                          
  1447  c7a1 18                         CLC		; step char position to left
  1448  c7a2 a5a5                       LDA gaddr	; ( +8 )
  1449  c7a4 6908                       ADC #$08
  1450  c7a6 85a5                       STA gaddr
  1451  c7a8 9002                       BCC +
  1452  c7aa e6a6                       INC gaddr+1
  1453                          +
  1454                          char_disp_leave
  1455  c7ac 68                 	PLA		; pass written character back
  1456  c7ad a8                         TAY		; restore saved registers
  1457  c7ae 68                         PLA
  1458  c7af aa                         TAX
  1459  c7b0 60                         RTS
  1460                          
  1461                          
  1462                          ;-----------------------------------------------------------------
  1463                          
  1464                          to
  1465  c7b1 ad3403                     LDA savexl
  1466  c7b4 859b                       STA xl
  1467  c7b6 ad3503                     LDA savexh
  1468  c7b9 859c                       STA xh
  1469  c7bb ad3603                     LDA savey
  1470  c7be 85aa                       STA y
  1471  c7c0 2065c3                     JSR getxy
  1472  c7c3 4c09c5                     JMP line_start
  1473                          
  1474                          ;-----------------------------------------------------------------
  1475                          
  1476                          unnew
  1477                          
  1478  c7c6 a52b               	lda bassta
  1479  c7c8 8522               	sta str
  1480  c7ca a52c               	lda bassta+1
  1481  c7cc 8523               	sta str+1
  1482  c7ce a001               	ldy #1
  1483  c7d0 98                 	tya
  1484  c7d1 9122               	sta (str),y		; != 0
  1485                          
  1486  c7d3 2033a5             	jsr b_rechain		; starting from bassta
  1487                          				; result in (str)
  1488  c7d6 18                 	clc			; str+1 -> new basic end
  1489  c7d7 a423               	ldy str+1
  1490  c7d9 a522               	lda str
  1491  c7db 6902               	adc #2
  1492  c7dd 852d               	sta basend
  1493  c7df 9001               	bcc +
  1494  c7e1 c8                 	iny
  1495  c7e2 842e               +	sty basend+1
  1496  c7e4 4c60a6             	jmp b_clr		; perform CLR
  1497                          
  1498                          ;-----------------------------------------------------------------
  1499                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
