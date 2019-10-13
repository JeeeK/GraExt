
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
    21  c003 a9b0                       lda #<(parse)		; check if basic interpreter parser hook
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
    36  c01b a937                       lda #<author            ; message ...
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
     6                          	!text "1.29" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2019-10-10 v 1.29
    10                          ;	2016-09-10 v 1.28
    11                          ;	2016-07-13 v 1.27
    12                          ;	2016-07-09 v 1.26
    13                          ;	2016-06-21 v 1.25
    14                          ;	2016-06-16 v 1.24
    15                          ;	2016-05-29 v 1.23
    16                          ;	2016-05-20 v 1.22
    17                          ;	2016-05-16 v 1.21
    18                          ;	2016-02-23 v 1.20
    19                          ;	2016-01-15 v 1.19
    20                          ;	1992-12-28 v 1.18
    21                          ;	1986-03-24 v 1.17
    22                          ;	1985       v 0.00 - 1.16
    23                          ;
    24                          ; the initial development is based on the implemention
    25                          ; done in a Forth environment written with a common 
    26                          ; 6502 forth assembler.
    27                          ; later, the code has been pulled out from there, relocated and 
    28                          ; enriched with some glue code to finally form the first 
    29                          ; basic extension.
    30                          
    31                          ; command dispatcher style JMP/RTS
    32                          ;	(if defined)
    33                          ;command_rts_style=1
    34                          
    35                          ; error handling 
    36                          ;	(if defined)
    37                          ;no_error=1
    38                          
    39                          ; basic interpreter registers, addresses and entry points
    40                          
    41                          type	= $0d
    42                          str     = $22		; string address
    43                          bassta	= $2b		; basic start pointer
    44                          basend	= $2d		; basic end pointer
    45                          basaryend	= $31		; basic end of array +1
    46                          strbot	= $33		; bottom of string heap 
    47                          ijmp    = $55		; address of JMP (addr)
    48                          chrget  = $73		; basic charget routine
    49                          chrgot  = $79		; basic last char got (charget routine)
    50                          txtptr	= $7A		; basic text pointer
    51                          facintl = $65		; integer result from b_fac2int
    52                          facinth = $64
    53                          facexp  = $61		; fac exponent, after b_getval
    54                          
    55                          z_reverseflag = $C7	; character routine
    56                          z_lastkey = $D7		; original use case, unused here
    57                          z_tmp = z_lastkey	; temporary reused for character routine
    58                          
    59                          v_baserr = $0300	; vector error routine
    60                          v_basstp = $0328	; vector error routine
    61                          v_bascmd = $0308	; vector interpreter parsing
    62                          v_basexp = $030a	; vector evaluate expression
    63                          
    64                          basic_rom = $A000	; start of BASIC ROM
    65                          
    66                          b_clr = $A660		; CLR command
    67                          b_interpreter = $A7AE	; interpreter loop
    68                          b_execstatement = $A7E7	; process statement (after chrget) - not used
    69                          b_execexpr =$AE92	; process expression - not used
    70                          b_getcomma = $AEFD	; read comma from basic text
    71                          b_illquant = $B248	; error "illegal quantity"
    72                          b_syntaxerror = $AF08	; error "syntax"
    73                          b_get8bit = $B79E	; read 8 bit numeric value from
    74                          			; basic text
    75                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    76                          			; from basic text
    77                          b_getval = $AD8A	; read numeric value from basic text
    78                          b_getexpr = $AD9E	; read expression from basic text
    79                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    80                          b_word2fac =$B391	; convert Y/Y to FAC (signed 16 bit)
    81                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    82                          b_fac2int = $BC9B	; convert FAC to integer
    83                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    84                          b_rechain = $A533	; rechain basic lines
    85                          b_str2fac = $BCF3	; convert string in FAC (expression handling)
    86                          b_chkparl = $AEFA 	; check '('
    87                          b_chkparr = $AEF7 	; check ')'
    88                          
    89                          ; hardware registers and values
    90                          
    91                          prozport = $01		; processor port
    92                          memrom = %00110111	; basic+kernal rom
    93                          membas = %00110110	; basic ram+kernal rom
    94                          memram = %00110101	; basic+kernal ram
    95                          
    96                          vic_cr	= $D011		; VIC control register
    97                          vic_mcr	= $D018		; VIC memory control register
    98                          cia_pra	= $DD00		; CIA 2 port register A
    99                          
   100                          cram	= $CC00		; start of color ram
   101                          
   102                          gram	= $e000		; start of graphic bitmap ram
   103                          gramp	= gram >> 8	; start page of bitmap
   104                          
   105                          ; constants 
   106                          
   107                          xmax	= 320		; max x dimension
   108                          ymax	= 200		; max y dimension
   109                          
   110                          ; zeropage variables
   111                          
   112                          x	= $9B		; start coordinate x, low+high
   113                          xl	= x
   114                          xh	= x+1
   115                          y	= $AA		; start coordinate y
   116                          
   117                          xendl	= $9E		; end coordinate x, low+high
   118                          xendh	= $9F
   119                          yend	= $93		; end coordinate y
   120                          
   121                          kl	= $95		; gradient for lines, low+high
   122                          kh	= kl+1
   123                          tmp1	= kl		; temp. var. (hline, vline, fill context)
   124                          tmp2	= kh		; temp. var. (hline, vline context)
   125                          fcont	= kh		; fill continuation flags (bit 1,0 for above, below)
   126                          
   127                          dxl	= $AB		; x delta, low+high
   128                          xsave	= dxl		; x register saved (hline, fill context)
   129                          dxh	= $A7
   130                          x8	= dxh		; 8x8 block index: (xh/xl) : 8 (fill context)
   131                          
   132                          dy	= $A9		; y delta
   133                          ysave	= dy		; y saved (hline context, fill context)
   134                          
   135                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   136                          ylimit	= ydir		; y limit in a 8x8 block (hline context)
   137                          fmode   = ydir		; mode mask: 0 | $FF (fill context)
   138                          
   139                          cl	= $A3		; dot count, low+high
   140                          ch	= $A4
   141                          ycount	= cl		; y count overall (hline context)
   142                          hcount	= ch		; horizontal blocks (hline context)
   143                          tmpmask	= cl		; temp. mask (fill context)
   144                          
   145                          gaddr	= $A5		; graphic address
   146                          
   147                          gpos	= $FB		; in graphic position
   148                          sgaddr	= gpos		; saved gaddr (hline context)
   149                          caddr	= gpos		; check gaddr (fill context)
   150                          
   151                          gcol	= $FD		; graphic color, in "graphic on" context only
   152                          fstack = gcol	; fill stack pointer (fill context)
   153                          
   154                          ; static ram areas
   155                          
   156                          savevpars = $0334	; original v_bascmd
   157                          saveverr = savevpars+2	; original v_baserr
   158                          savevstp = saveverr+2	; original v_basstp
   159                          savevexp = savevstp+2	; original v_basexp
   160                          savexl	= savevexp+2	; the graphic cursor: x low 
   161                          savexh	= savexl+1	; the graphic cursor: x high
   162                          savey	= savexh+1	; the graphic cursor: y
   163                          savemo	= savey+1	; the graphic mode
   164                          saveend = savemo+1	; byte after save area
   165                          
   166                          			; real place for gchange and gmask routines,
   167                          !ifdef ltc {
   168                          gramcode = $03ed - 26	; 15 bytes + 4*6+2
   169                          } else {
   170                          gramcode = $03ed	; 15 bytes
   171                          }
   172                          
   173                          ; LTC64 specifics
   174                          
   175                          !ifdef ltc {
   176                          
   177                          !cpu 65816
   178                          
   179                          bank4+3 = $040000
   180                          rombank+3 = $010000     ; c't
   181                          
   182                          ; c't-Karte-Kontrollregister
   183                          
   184                          memconf = bank4 or 1
   185                          mc_off  = $80                   ; CPU 816 ausschalten
   186                          mc_slow = $40                   ; CPU 1 MHz
   187                          mc_epr  = $20                   ; EPROM in Bank0
   188                          mc_sim  = $10                   ; ROM-Simulation Bit
   189                          
   190                          }
   191                          
   192                          
   193                          
   194                          ;
   195                          ; initialize extension
   196                          
   197                          init
   198  c025 ad0803                     LDA v_bascmd	; check if hooks are already 
   199  c028 ae0903                     LDX v_bascmd+1	; in place 
   200  c02b c9b0               	CMP #<(parse)
   201  c02d d004               	BNE +
   202  c02f e0c0               	CPX #>(parse)
   203  c031 f052               	BEQ ++		; already hooked
   204                          
   205  c033 8d3403             +       STA savevpars	; save old vector
   206  c036 8e3503             	STX savevpars+1
   207  c039 a9b0               	LDA #<(parse)	; basic interpreter parser hook
   208  c03b 8d0803                     STA v_bascmd	; for commands
   209  c03e a9c0                       LDA #>(parse)
   210  c040 8d0903                     STA v_bascmd+1
   211                          
   212  c043 ad0a03                     LDA v_basexp	; basic interpreter parser hook
   213  c046 8d3a03             	STA savevexp	; for expressions
   214  c049 a9e4                       LDA #<(express) ; with save of old pointer
   215  c04b 8d0a03                     STA v_basexp
   216  c04e ad0b03                     LDA v_basexp+1
   217  c051 8d3b03             	STA savevexp+1
   218  c054 a9c0                       LDA #>(express)
   219  c056 8d0b03                     STA v_basexp+1
   220                          
   221  c059 ad2803                     LDA v_basstp
   222  c05c 8d3803             	STA savevstp
   223  c05f a99b                       LDA #<(stop)	; basic interpreter stop hook
   224  c061 8d2803                     STA v_basstp
   225  c064 ad2903                     LDA v_basstp+1
   226  c067 8d3903             	STA savevstp+1
   227  c06a a9c0                       LDA #>(stop)
   228  c06c 8d2903                     STA v_basstp+1
   229                          
   230  c06f ad0003                     LDA v_baserr
   231  c072 8d3603             	STA saveverr
   232  c075 a995                       LDA #<(error)	; basic interpreter error hook
   233  c077 8d0003                     STA v_baserr
   234  c07a ad0103                     LDA v_baserr+1
   235  c07d 8d3703             	STA saveverr+1
   236  c080 a9c0                       LDA #>(error)
   237  c082 8d0103                     STA v_baserr+1
   238                          
   239  c085 a200               ++	LDX #0		; set graphic cursor to (0,0)
   240  c087 8e3c03             	STX savexl
   241  c08a 8e3d03             	STX savexh
   242  c08d 8e3e03             	STX savey
   243  c090 e8                 	INX
   244  c091 8e3f03             	STX savemo	; set mode 1
   245  c094 60                         RTS
   246                          
   247                          error	
   248                          	; reg A may destroyed
   249  c095 20a7c1             	JSR gra_off		; uses only reg A
   250  c098 6c3603             	JMP (saveverr)		; to original vector
   251                          
   252                          stop	
   253                          	; reg A may destroyed
   254  c09b a591               	LDA $91			; Scan code
   255  c09d c97f               	CMP #$7F		; STOP key?
   256  c09f d003               	BNE nostop
   257  c0a1 20a7c1             	JSR gra_off		; uses only reg A
   258                          nostop
   259  c0a4 6c3803             	JMP (savevstp)		; to original vector
   260                          
   261                          
   262                          ;-----------------------------------------------------------------
   263                          
   264                          ; undo chrget
   265                          
   266                          undo_chrget
   267  c0a7 a57a               	LDA txtptr		; decrement text pointer by 1
   268  c0a9 d002               	BNE +
   269  c0ab c67b               	DEC txtptr+1
   270  c0ad c67a               +	DEC txtptr
   271  c0af 60                 	RTS
   272                          
   273                          ;-----------------------------------------------------------------
   274                          
   275                          ; start parsing an extension command ...
   276                          
   277                          parse
   278  c0b0 207300                     JSR chrget			; next char.
   279  c0b3 c926                       CMP #'&'			; command prefix
   280  c0b5 f006                       BEQ newcmd
   281  c0b7 20a7c0             	JSR undo_chrget
   282  c0ba 6c3403             	JMP (savevpars)
   283                          newcmd
   284  c0bd 207300                     JSR chrget			; command character
   285                          
   286  c0c0 a00d                       LDY #(cmdsend-cmds)		; map character to
   287                          					; command address ...
   288                          checknextcmd
   289  c0c2 88                         DEY
   290  c0c3 f01c               	BEQ parse_error
   291  c0c5 d912c1                     CMP cmds,Y
   292  c0c8 d0f8                       BNE checknextcmd		; try next
   293  c0ca 88                         DEY				; found
   294  c0cb 98                         TYA
   295  c0cc 0a                         ASL				; *2
   296  c0cd a8                         TAY
   297                          !ifndef command_rts_tyle {
   298                          	!set co=0			; command offset in jump table
   299  c0ce b920c1                     LDA cmdaddr+1,Y                 ; high byte from table
   300  c0d1 8556                       STA ijmp+1
   301  c0d3 b91fc1                     LDA cmdaddr,Y                   ; low byte from table
   302  c0d6 8555                       STA ijmp
   303  c0d8 207300                     JSR chrget			; read next byte in basic text
   304  c0db 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   305  c0de 4caea7                     JMP b_interpreter		; continue parsing
   306                          } else {
   307                          	!set co=1			; command offset in jump table
   308                          	LDA #>(b_interpreter-1)		; return to interpreter
   309                          	PHA
   310                          	LDA #<(b_interpreter-1)
   311                          	PHA				
   312                                  LDA cmdaddr+1,Y			; command address (RTS style)
   313                                  PHA				; high byte on stack
   314                                  LDA cmdaddr,Y			; command address (RTS style)
   315                                  PHA				; low byte on stack
   316                                  JMP chrget			; read next byte in basic text 
   317                          					; and RTS to command
   318                          }
   319                          parse_error
   320  c0e1 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   321                          
   322                          ;-----------------------------------------------------------------
   323                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   324                          express
   325  c0e4 a900               	LDA #0
   326  c0e6 850d               	STA type	
   327  c0e8 207300             	JSR chrget
   328  c0eb b003               	BCS exp_nonumber
   329  c0ed 4cf3bc             	JMP b_str2fac
   330                          exp_nonumber
   331  c0f0 c926                       CMP #'&'			; command prefix
   332  c0f2 f006                       BEQ newfunc
   333  c0f4 20a7c0             	JSR undo_chrget
   334  c0f7 6c3a03             	JMP (savevexp)			; original routine	
   335                          ;	JMP b_execexpr
   336                          newfunc
   337  c0fa 207300             	JSR chrget
   338  c0fd c95a               	CMP #'Z'
   339  c0ff d003               	BNE +
   340  c101 4cb5c6             	JMP get
   341  c104 c958               +	CMP #'X'
   342  c106 d003               	BNE +
   343  c108 4ca0c6             	JMP getposx
   344  c10b c959               +	CMP #'Y'
   345  c10d d0d2               	BNE parse_error
   346  c10f 4cacc6             	JMP getposy
   347                          
   348                          ;-----------------------------------------------------------------
   349                          
   350                          ; the most commonly used command placed at the end ...
   351                          
   352  c112 2055464743534d52...cmds	!text " UFGCSMRTVHLP"		; first char. is a dummy
   353                          cmdsend
   354                          
   355                          cmdaddr
   356  c11f 0aca19c8a0c150c7...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co,relto-co
   357  c12d 04c8cbc4d7c330c5...        !word to-co,vline-co,hline-co,line-co,plot-co
   358                          
   359  c137 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  c141 312e3239            !text "1.29" 

; ******** Source: graext-core.asm
   361  c145 20313938362c3230...	!text " 1986,2019 JOHANN@KLASEK.AT",0
   362                          
   363                          bitmask
   364  c161 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   365                          nbitmask
   366  c169 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   367                          ytabl
   368  c171 004080c0           	!byte $00,$40,$80,$c0
   369                          ytabh
   370  c175 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   371  c179 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   372  c17d eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   373  c181 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   374  c185 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   375  c189 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   376  c18d fe                 	!byte gramp+$1e
   377                          
   378                          ; for horiz. line
   379                          
   380                          maskleft0
   381                          maskleft
   382  c18e ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   383  c196 00                 	!byte $00
   384                          
   385                          maskright0
   386  c197 00                 	!byte $00
   387                          maskright
   388  c198 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   389                          
   390                          ;-----------------------------------------------------------------
   391                          
   392                          graphic
   393  c1a0 209eb7                     JSR b_get8bit
   394  c1a3 e000                       CPX #$00
   395  c1a5 d013                       BNE gra_other
   396                          gra0			; &G 0
   397                          gra_off
   398  c1a7 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   399  c1a9 8d00dd                     STA cia_pra
   400  c1ac a915                       LDA #((1 <<4) + (2 <<1) + 1)
   401                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   402                          			; char addr $1000/4096 = char. ROM
   403  c1ae 8d18d0                     STA vic_mcr	; VIC memory control
   404  c1b1 ad11d0                     LDA vic_cr	; VIC control register
   405  c1b4 29df                       AND #%11011111	; Hires mode off
   406  c1b6 8d11d0                     STA vic_cr
   407  c1b9 60                         RTS
   408                          
   409                          gra_other
   410  c1ba e001                       CPX #$01
   411  c1bc f00f               	BEQ gra1
   412  c1be e002               	CPX #$02
   413  c1c0 f00e                       BEQ gra2
   414  c1c2 e004               	CPX #$04
   415  c1c4 f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   416  c1c6 e003               	CPX #$03	; &G 3 (graphic on)
   417  c1c8 f029               	BEQ gra_on
   418  c1ca 4c48b2                     JMP b_illquant	; parameter illegal
   419                          	
   420                          gra1			; &G 1
   421  c1cd 2009c2             	JSR gra_clear
   422                          
   423                          gra2
   424  c1d0 20f1b7                     JSR b_getcomma8bit
   425  c1d3 8a                         TXA		; foreground color
   426  c1d4 0a                         ASL		; upper nibble
   427  c1d5 0a                         ASL
   428  c1d6 0a                         ASL
   429  c1d7 0a                         ASL
   430  c1d8 85fd                       STA gcol
   431  c1da 20f1b7                     JSR b_getcomma8bit
   432  c1dd 8a                         TXA		; background color
   433  c1de 290f                       AND #$0F
   434  c1e0 05fd                       ORA gcol
   435  c1e2 a000                       LDY #$00
   436                          cram_loop
   437  c1e4 9900cc                     STA cram,Y	; fill color RAM
   438  c1e7 9900cd                     STA cram+$100,Y
   439  c1ea 9900ce                     STA cram+$200,Y
   440  c1ed 99e8ce                     STA cram+$300-24,Y
   441  c1f0 c8                         INY
   442  c1f1 d0f1                       BNE cram_loop
   443                          
   444                          gra_on
   445  c1f3 2028c2             	JSR gra_setupcode
   446                          
   447  c1f6 a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   448  c1f8 8d00dd                     STA cia_pra
   449  c1fb a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   450  c1fd 8d18d0                     STA vic_mcr	; VIC memory control
   451  c200 ad11d0                     LDA vic_cr	; VIC control register
   452  c203 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   453  c205 8d11d0                     STA vic_cr
   454  c208 60                         RTS
   455                          
   456                          gra_clear
   457  c209 a220                       LDX #$20	; Pages (8 KByte)
   458  c20b a9e0                       LDA #>gram
   459  c20d 85fc                       STA gpos+1
   460  c20f a000                       LDY #$00
   461  c211 84fb                       STY gpos
   462  c213 98                         TYA
   463                          gra_fill
   464  c214 91fb                       STA (gpos),Y	; Loop unroll
   465  c216 c8                         INY
   466  c217 91fb                       STA (gpos),Y
   467  c219 c8                         INY
   468  c21a 91fb                       STA (gpos),Y
   469  c21c c8                         INY
   470  c21d 91fb                       STA (gpos),Y
   471  c21f c8                         INY
   472  c220 d0f2                       BNE gra_fill
   473  c222 e6fc                       INC gpos+1
   474  c224 ca                         DEX
   475  c225 d0ed                       BNE gra_fill
   476  c227 60                 	RTS
   477                          
   478                          gra_setupcode
   479  c228 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   480                          gra_copycode
   481  c22a bd4bc2             	LDA gromcode-1,X
   482  c22d 9dec03             	STA gramcode-1,X
   483  c230 ca                 	DEX
   484  c231 d0f7               	BNE gra_copycode
   485  c233 ad3f03             	LDA savemo
   486  c236 290f               	AND #$0F
   487  c238 aa                 	TAX
   488  c239 4c4cc6             	JMP setmode_enter	; re-apply mode to routines
   489                          				; implicit RTS
   490                          
   491                          ;-----------------------------------------------------------------
   492                          
   493                          gexit
   494  c23c a501                       LDA prozport
   495  c23e 0902                       ORA #%00000010	; kernal ROM enable
   496  c240 8501                       STA prozport
   497  c242 58                         CLI		; allow interrupts
   498  c243 60                         RTS
   499                          
   500                          ;-----------------------------------------------------------------
   501                          
   502                          ginit
   503  c244 a501                       LDA prozport
   504  c246 29fd                       AND #%11111101	; Kernal ROM disable
   505  c248 78                         SEI		; disable interrupts
   506  c249 8501                       STA prozport
   507  c24b 60                         RTS
   508                          			; on exit Z=0
   509                          
   510                          ;-----------------------------------------------------------------
   511                          
   512                          ; These are selfmodified routines, which has to placed into RAM
   513                          ; (on every graphic "on")
   514                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   515                          
   516                          gromcode
   517                          
   518                          !pseudopc gramcode {
   519                          
   520                          ; change a graphic location
   521                          
   522                          gchange
   523                          !ifdef ltc {
   524                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   525                          	STA memconf		; damit internes RAM gelesen werden kann!
   526                          }
   527  c24c b1a5                       LDA (gaddr),Y
   528                          gchange_op
   529  c24e 1d61c1                     ORA bitmask,X
   530  c251 91a5                       STA (gaddr),Y
   531                          !ifdef ltc {
   532                          	LDA #mc_sim		; vollständige ROM-Simulation
   533                          	STA memconf		; wieder schnelles RAM ab $C000
   534                          }
   535  c253 60                         RTS
   536                          
   537                          ; mask a graphic location 
   538                          
   539                          gmask
   540                          !ifdef ltc {
   541                          	XBA
   542                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   543                          	STA memconf		; damit internes RAM gelesen werden kann!
   544                          	XBA
   545                          }
   546                          gmask_flip
   547  c254 4900                       EOR #$00
   548                          gmask_op
   549  c256 11a5                       ORA (gaddr),Y
   550  c258 91a5                       STA (gaddr),Y
   551                          !ifdef ltc {
   552                          	LDA #mc_sim		; vollständige ROM-Simulation
   553                          	STA memconf		; wieder schnelles RAM ab $C000
   554                          }
   555  c25a 60                         RTS
   556                          
   557                          }
   558                          
   559                          gromcode_end
   560                          
   561                          ;-----------------------------------------------------------------
   562                          
   563                          position
   564  c25b a5aa                       LDA y
   565  c25d 4a                         LSR
   566  c25e 4a                         LSR
   567  c25f 4a                         LSR		; y/8
   568  c260 a8                         TAY
   569  c261 2903                       AND #%00000011	; (y/8) mod 4
   570  c263 aa                         TAX
   571  c264 a59b                       LDA xl		; x low
   572  c266 29f8                       AND #%11111000	; clear bit 2-0
   573  c268 18                         CLC
   574  c269 7d71c1                     ADC ytabl,X	; addr low: y base + x part
   575  c26c 85a5                       STA gaddr
   576  c26e a59c                       LDA xh		; addr high: x part
   577  c270 7975c1                     ADC ytabh,Y	; 	+ y base
   578  c273 85a6                       STA gaddr+1
   579  c275 a5aa                       LDA y		; vertical offset
   580  c277 2907                       AND #%00000111	; y mod 8
   581  c279 a8                         TAY
   582  c27a a59b                       LDA xl
   583  c27c 2907                       AND #%00000111	; x mod 8
   584  c27e aa                         TAX		; horizonal offset
   585  c27f 60                         RTS		; (bitmask)
   586                          
   587                          
   588                          ;-----------------------------------------------------------------
   589                          
   590                          ; swap tupel xl,xh <-> xendl,xendh
   591                          
   592                          swap_x_xend
   593  c280 a69e                       LDX xendl	; swap x, xend
   594  c282 a49b                       LDY xl
   595  c284 869b                       STX xl
   596  c286 849e                       STY xendl
   597                          
   598  c288 a69f                       LDX xendh
   599  c28a a49c                       LDY xh
   600  c28c 849f                       STY xendh
   601  c28e 869c                       STX xh
   602  c290 60                 	RTS
   603                          
   604                          
   605                          ;-----------------------------------------------------------------
   606                          
   607                          ; line y up, x left, dx < dy (case 1)
   608                          
   609                          line_up_steep
   610  c291 205bc2                     JSR position	; x,y
   611                          loop_yup_xleft
   612  c294 20ed03                     JSR gchange	; pixel
   613                          
   614  c297 18                         CLC		; k += dx
   615  c298 a595                       LDA kl
   616  c29a 65ab                       ADC dxl		; dxh is 0, because dx < dy
   617  c29c 8595                       STA kl
   618  c29e 9014                       BCC +		; k >= 0 ->
   619                          
   620  c2a0 e5a9               ++	SBC dy		; k -= dy (C=1)
   621  c2a2 8595                       STA kl
   622                          
   623  c2a4 ca                  	DEX		; x--
   624  c2a5 100d                       BPL +
   625  c2a7 a207                       LDX #7		; wrap around
   626  c2a9 38                 	SEC
   627  c2aa a5a5                       LDA gaddr	; x-8: gaddr -= 8
   628  c2ac e908                       SBC #8
   629  c2ae 85a5                       STA gaddr
   630  c2b0 b002                       BCS +
   631  c2b2 c6a6                       DEC gaddr+1
   632                          
   633  c2b4 88                 +	DEY		; y--
   634  c2b5 100f                       BPL +++
   635  c2b7 38                         SEC		; y overflow
   636  c2b8 a5a5                       LDA gaddr
   637  c2ba e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   638  c2bc 85a5                       STA gaddr
   639  c2be a5a6                       LDA gaddr+1
   640  c2c0 e901               	SBC #1
   641  c2c2 85a6                       STA gaddr+1
   642  c2c4 a007                       LDY #7		; wrap around
   643                          
   644  c2c6 c6a3               +++	DEC cl		; until c=0
   645  c2c8 d0ca                       BNE loop_yup_xleft
   646  c2ca 4c3cc2                     JMP gexit
   647                          
   648                          
   649                          ;-----------------------------------------------------------------
   650                          
   651                          ; line x left, y up, dx > dy (case 2)
   652                          
   653                          line_up_flat
   654  c2cd 205bc2                     JSR position	; x,y
   655  c2d0 a5a3               	LDA cl		; counter adjustment for
   656  c2d2 f002               	BEQ +		; prepare for dec-dec-counting
   657  c2d4 e6a4               	INC ch
   658                          +
   659                          loop_xleft_yup
   660  c2d6 20ed03                     JSR gchange	; pixel
   661                          
   662  c2d9 18                         CLC		; k += dy
   663  c2da a595                       LDA kl
   664  c2dc 65a9                       ADC dy
   665  c2de 8595                       STA kl
   666  c2e0 9020                       BCC +		; k < 0
   667  c2e2 e696                       INC kh
   668  c2e4 301c               	BMI +		; k < 0
   669                          
   670  c2e6 e5ab                       SBC dxl		; k -= dx (A = kl, C=1)
   671  c2e8 8595                       STA kl
   672  c2ea a596                       LDA kh
   673  c2ec e5a7                       SBC dxh		
   674  c2ee 8596                       STA kh
   675                          
   676  c2f0 88                         DEY		; y--
   677  c2f1 100f                       BPL +
   678  c2f3 38                 	SEC		; C=1 not always true (SBC above)
   679  c2f4 a5a5                       LDA gaddr	; y overflow
   680  c2f6 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   681  c2f8 85a5                       STA gaddr
   682  c2fa a5a6                       LDA gaddr+1
   683  c2fc e901               	SBC #1
   684  c2fe 85a6                       STA gaddr+1
   685  c300 a007               	LDY #7		; wrap around
   686                          
   687  c302 ca                 +	DEX		; x--
   688  c303 100d                       BPL +++
   689  c305 a207                       LDX #7		; wrap around
   690  c307 38                 	SEC
   691  c308 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   692  c30a e908                       SBC #8
   693  c30c 85a5                       STA gaddr
   694  c30e b002                       BCS +++
   695  c310 c6a6                       DEC gaddr+1
   696                          +++
   697  c312 c6a3               	DEC cl		; c--
   698  c314 d0c0                       BNE loop_xleft_yup
   699  c316 c6a4                       DEC ch		; adjusted high which allows this
   700  c318 d0bc                       BNE loop_xleft_yup
   701                          
   702  c31a 4c3cc2                     JMP gexit
   703                          
   704                          
   705                          
   706                          ;-----------------------------------------------------------------
   707                          
   708                          ; line x left, y down, dx > dy (case 3)
   709                          
   710                          line_down_flat
   711  c31d 205bc2                     JSR position	; x,y
   712  c320 a5a3               	LDA cl		; counter adjustment for
   713  c322 f002               	BEQ +		; prepare for dec-dec-counting
   714  c324 e6a4               	INC ch
   715                          +
   716                          loop_xleft_ydown
   717  c326 20ed03                     JSR gchange	; pixel
   718                          
   719  c329 18                         CLC		; k += dy
   720  c32a a595                       LDA kl
   721  c32c 65a9                       ADC dy
   722  c32e 8595                       STA kl
   723  c330 9021                       BCC +		; k < 0
   724  c332 e696                       INC kh
   725  c334 301d               	BMI +		; k < 0
   726                          
   727  c336 e5ab                       SBC dxl		; k -= dx (A = kl, C=1)
   728  c338 8595                       STA kl
   729  c33a a596                       LDA kh
   730  c33c e5a7                       SBC dxh		
   731  c33e 8596                       STA kh
   732                          
   733  c340 c8                         INY		; y++
   734  c341 c008                       CPY #8
   735  c343 d00e                       BNE +
   736                          	; C=1
   737  c345 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   738  c347 693f                       ADC #$40-1	; C already set by CPY
   739  c349 85a5                       STA gaddr
   740  c34b a5a6                       LDA gaddr+1
   741  c34d 6901               	ADC #1
   742  c34f 85a6                       STA gaddr+1
   743  c351 a000                       LDY #0		; wrap around
   744                          
   745  c353 ca                 +	DEX		; x--
   746  c354 100d                       BPL +++
   747  c356 a207                       LDX #7		; wrap around
   748  c358 38                 	SEC
   749  c359 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   750  c35b e908                       SBC #8
   751  c35d 85a5                       STA gaddr
   752  c35f b002                       BCS +++
   753  c361 c6a6                       DEC gaddr+1
   754                          +++
   755  c363 c6a3               	DEC cl		; c--
   756  c365 d0bf               	BNE loop_xleft_ydown
   757  c367 c6a4               	DEC ch		; adjusted high which allows this
   758  c369 d0bb                       BNE loop_xleft_ydown
   759                          
   760  c36b 4c3cc2                     JMP gexit
   761                          
   762                          
   763                          ;-----------------------------------------------------------------
   764                          
   765                          ; line y down, x right, dx < dy (case 4)
   766                          
   767                          line_down_steep
   768  c36e 205bc2                     JSR position	; x,y
   769                          loop_ydown_xleft
   770  c371 20ed03                     JSR gchange	; pixel
   771                          
   772  c374 18                         CLC		; k += dx
   773  c375 a595                       LDA kl
   774  c377 65ab                       ADC dxl		; dxh is 0, because dx < dy
   775  c379 8595                       STA kl
   776  c37b 9014                       BCC +		; k >= 0 ->
   777                          
   778  c37d e5a9               	SBC dy		; k -= dy, C=1
   779  c37f 8595                       STA kl
   780                          
   781  c381 ca                  	DEX		; x--
   782  c382 100d                       BPL +
   783  c384 a207                       LDX #7		; wrap around
   784  c386 38                 	SEC
   785  c387 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   786  c389 e908                       SBC #8
   787  c38b 85a5                       STA gaddr
   788  c38d b002                       BCS +
   789  c38f c6a6                       DEC gaddr+1
   790                          
   791  c391 c8                 +	INY		; y++
   792  c392 c008                       CPY #8		; y overflow?
   793  c394 d00e                       BNE +++
   794  c396 a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   795  c398 693f                       ADC #$40-1	; C already set by CPY
   796  c39a 85a5                       STA gaddr
   797  c39c a5a6                       LDA gaddr+1
   798  c39e 6901               	ADC #1
   799  c3a0 85a6                       STA gaddr+1
   800  c3a2 a000                       LDY #0		; wrap around
   801                          
   802  c3a4 c6a3               +++	DEC cl		; c--
   803                          			; until c=0
   804  c3a6 d0c9                       BNE loop_ydown_xleft
   805  c3a8 4c3cc2                     JMP gexit
   806                          
   807                          
   808                          ;-----------------------------------------------------------------
   809                          
   810                          getcommaxy
   811  c3ab 20fdae                     JSR b_getcomma	; check ","
   812                          getxy
   813  c3ae 208aad                     JSR b_getval	; get X coord. value
   814  c3b1 20f7b7                     JSR b_convint
   815  c3b4 c901                       CMP #>xmax
   816  c3b6 900c               	BCC gcxy_xok
   817  c3b8 f003                       BEQ ++		; X = $1xx
   818  c3ba 2013c6                     JSR range_error
   819                          
   820  c3bd c040               ++	CPY #<xmax	; check X low
   821  c3bf 9003                       BCC +
   822  c3c1 2013c6                     JSR range_error
   823                          +
   824                          gcxy_xok
   825  c3c4 84fb                       STY gpos	; temporary save X coord.
   826  c3c6 85fc                       STA gpos+1
   827                          
   828  c3c8 20f1b7                     JSR b_getcomma8bit
   829                          			; get Y coord. value
   830  c3cb e0c8                       CPX #ymax
   831  c3cd 9003                       BCC +
   832  c3cf 2013c6                     JSR range_error
   833                          +
   834  c3d2 a4fb                       LDY gpos	; restory X coord.
   835  c3d4 a5fc                       LDA gpos+1
   836  c3d6 60                         RTS
   837                          
   838                          
   839                          ;-----------------------------------------------------------------
   840                          
   841                          hline
   842  c3d7 20aec3                     JSR getxy	; get startpoint
   843  c3da 86aa                       STX y
   844  c3dc 8e3e03                     STX savey	; save as cursor, too
   845  c3df 859c                       STA xh
   846  c3e1 849b                       STY xl
   847  c3e3 20fdae                     JSR b_getcomma	; get length
   848  c3e6 208aad                     JSR b_getval
   849  c3e9 20f7b7                     JSR b_convint
   850                          			; calculate end point
   851  c3ec aa                         TAX		; save length high byte
   852  c3ed 98                         TYA		; length low byte
   853  c3ee 18                         CLC
   854  c3ef 659b                       ADC xl		; low xend = x+length
   855  c3f1 859e                       STA xendl
   856  c3f3 a8                 	TAY
   857  c3f4 8a                         TXA		; high
   858  c3f5 659c                       ADC xh		; high xend = x+length
   859  c3f7 859f                       STA xendh
   860  c3f9 aa                 	TAX
   861                          
   862  c3fa c901               	CMP #>xmax	; endpoint outside?
   863  c3fc 900a               	BCC +
   864  c3fe d005               	BNE ++		; >=$200
   865  c400 98                 	TYA
   866  c401 e940               	SBC #<xmax
   867  c403 9003               	BCC +
   868  c405 2013c6             ++	JSR range_error
   869                          			; XXX xend=xmax-1 ?
   870                          +
   871  c408 8e3d03                     STX savexh
   872  c40b 8c3c03                     STY savexl	; also save as final cursor
   873                          
   874  c40e a900               	LDA #0		; default thickness 0 (means 1 pixel)
   875  c410 85a3               	STA ycount
   876  c412 207900             	JSR chrgot	; last char. again
   877  c415 f019               	BEQ +++		; command end? no optional param.
   878  c417 20f1b7             	JSR b_getcomma8bit
   879  c41a 8a                 	TXA		; optional 8-bit parameter
   880  c41b 85a3               	STA ycount	; hline thickness
   881  c41d f011               	BEQ +++		; 0 means 1 pixel
   882  c41f 18                 	CLC
   883  c420 65aa               	ADC y		; end position for y coord.
   884  c422 b004               	BCS +		; > 255
   885  c424 c9c8               	CMP #ymax
   886  c426 9008               	BCC +++
   887                          +			; C=1 from ADC or CMP before
   888  c428 2013c6             	JSR range_error	; corrupts A
   889                          			; XXX ycount=ymax-y-1 ?
   890                          			; xend >= x
   891  c42b b003               	BCS hl_noxswap	; always
   892                          
   893                          hline_start
   894  c42d 2080c2             	JSR swap_x_xend	; xend < x, entry from line
   895                          	
   896                          hl_noxswap
   897                          			; xend > x
   898                          +++
   899  c430 e6a3               	INC ycount	; count to 0
   900  c432 2044c2                     JSR ginit	; map in graphic memory
   901                          
   902  c435 205bc2                     JSR position	; graphic position x,y
   903                          
   904  c438 a5a5               	LDA gaddr	; save position for vertical
   905  c43a 85fb               	STA sgaddr
   906  c43c a5a6               	LDA gaddr+1
   907  c43e 85fc               	STA sgaddr+1
   908  c440 86ab               	STX xsave
   909  c442 84a9               	STY ysave
   910                          
   911  c444 a59e                       LDA xendl
   912  c446 2907                       AND #%00000111
   913  c448 8596                       STA tmp2	; xend mod 8, mask index
   914  c44a a59b                       LDA xl
   915  c44c 29f8                       AND #%11111000	; (xl div 8)*8
   916  c44e 8595                       STA tmp1
   917  c450 a59e                       LDA xendl	; xend unmasked
   918  c452 38                         SEC
   919  c453 e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   920  c455 8595                       STA tmp1
   921  c457 a59f                       LDA xendh
   922  c459 e59c                       SBC xh
   923  c45b 4a                         LSR		; / 8 ->  0-39
   924  c45c a595                       LDA tmp1	; only 1 highest bit
   925  c45e 6a                         ROR		; and 3 lower bits
   926  c45f 4a                         LSR
   927  c460 4a                         LSR
   928                                  		; 8-pixel-blocks count
   929  c461 85a4               	STA hcount	; save for vertical extension
   930                           
   931                          hl_vertloop
   932  c463 98                 	TYA		; calculate max. Y in 8x8 block
   933  c464 18                 	CLC
   934  c465 65a3               	ADC ycount
   935  c467 c908               	CMP #8
   936  c469 9002               	BCC +
   937  c46b a908               	LDA #8
   938  c46d 85a8               +	STA ylimit
   939                          
   940  c46f bd8ec1                     LDA maskleft,X	; starting mask
   941  c472 8595               	STA tmp1
   942  c474 a6a4               	LDX hcount	; how many blocks
   943                          
   944                          hl_nextblock
   945  c476 ca                         DEX
   946                          hl_islastblock
   947  c477 301d                       BMI hl_lastblock
   948                          			; leave loop if X<0
   949  c479 a4a9               	LDY ysave
   950  c47b a595               -	LDA tmp1	; mask
   951  c47d 20f503             	JSR gmask	; first with left end mask
   952  c480 c8                 	INY		; vertical down
   953  c481 c4a8               	CPY ylimit	; in 8x8 box
   954  c483 d0f6               	BNE -
   955                          
   956  c485 18                         CLC		; gaddr += 8 (one block to right)
   957  c486 a5a5                       LDA gaddr
   958  c488 6908                       ADC #8
   959  c48a 85a5                       STA gaddr
   960  c48c 9002                       BCC +
   961  c48e e6a6                       INC gaddr+1
   962                          
   963  c490 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   964  c492 8595               	STA tmp1
   965  c494 d0e0               	BNE hl_nextblock	; always
   966                          
   967                          hl_lastblock
   968  c496 a696                       LDX tmp2	; xend mask index
   969  c498 3d98c1                     AND maskright,X ; A has current maskt combine with mask right end
   970  c49b 8595               	STA tmp1	; mask
   971  c49d a4a9               	LDY ysave	; start position in 8x8 block
   972  c49f a595               -	LDA tmp1	; mask
   973  c4a1 20f503             	JSR gmask	; modify
   974  c4a4 c8                 	INY		; vertical down
   975  c4a5 c6a3               	DEC ycount	; overall y counter
   976  c4a7 c4a8               	CPY ylimit
   977  c4a9 d0f4               	BNE -
   978                          
   979  c4ab a5a3               	LDA ycount	; finished
   980  c4ad d003               	BNE +		; roll-over into 8x8 block below
   981  c4af 4c3cc2                     JMP gexit	; leave
   982                          
   983  c4b2 18                 +	CLC
   984  c4b3 a5fb               	LDA sgaddr
   985  c4b5 6940               	ADC #$40	; next 8-pixel row below
   986  c4b7 85fb               	STA sgaddr	; + $140 (320)
   987  c4b9 85a5               	STA gaddr
   988  c4bb a5fc               	LDA sgaddr+1
   989  c4bd 6901               	ADC #$01
   990  c4bf 85fc               	STA sgaddr+1
   991  c4c1 85a6               	STA gaddr+1
   992  c4c3 a6ab               	LDX xsave	; initial mask index
   993  c4c5 a000               	LDY #0		; start on top of 8x8
   994  c4c7 84a9               	STY ysave
   995  c4c9 f098               	BEQ hl_vertloop
   996                          ;-----------------------------------------------------------------
   997                          
   998                          vline
   999  c4cb 20aec3                     JSR getxy	; get startpoint
  1000  c4ce 859c                       STA xh
  1001  c4d0 8d3d03                     STA savexh	; save as cursor too
  1002  c4d3 849b                       STY xl
  1003  c4d5 8c3c03                     STY savexl
  1004  c4d8 8693                       STX yend	; initial point is endpoint
  1005                          
  1006  c4da 20f1b7                     JSR b_getcomma8bit
  1007                          			; get length
  1008  c4dd 18                         CLC		; calculate end point
  1009  c4de 8a                         TXA		; length
  1010                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1011                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1012                          ;	STA tmp1
  1013  c4df 6593                       ADC yend	; length + initial point is startpoint
  1014  c4e1 b005               	BCS vline_iq	; > 255
  1015  c4e3 c9c8                       CMP #ymax	; outside?
  1016  c4e5 a8                 	TAY		; keep startpoint
  1017  c4e6 9003                       BCC +
  1018                          vline_iq
  1019  c4e8 2013c6                     JSR range_error ; corrupts A
  1020                          			; XXX Y = ymax-1 ?
  1021  c4eb 84aa               +	STY y		; startpoint
  1022  c4ed 8c3e03             	STY savey	; set cursor y position
  1023  c4f0 18                 	CLC
  1024  c4f1 900e               	BCC +++		; skip following, because y, yend are already ordered
  1025                          
  1026                          vline_start		; entry point from line command (only)
  1027  c4f3 a5aa               	LDA y		; order of y, yend is not defined
  1028  c4f5 c593               	CMP yend
  1029  c4f7 b008               	BCS vl_noyswap	; yend > y ->
  1030  c4f9 a5aa               	LDA y		; swap y, yend
  1031  c4fb a693               	LDX yend
  1032  c4fd 8593               	STA yend
  1033  c4ff 86aa               	STX y
  1034                          vl_noyswap
  1035                          			; startpoint is below the endpoint
  1036  c501 2044c2             +++	JSR ginit	; map in graphic memory
  1037                          
  1038                          vl_start
  1039  c504 205bc2                     JSR position	; graphic position x,y
  1040  c507 bd61c1                     LDA bitmask,X
  1041  c50a 8596                       STA tmp2	; save mask
  1042                          ; DON'T-CHANGE: replace ...
  1043  c50c 38                         SEC
  1044  c50d a5aa                       LDA y		; startpoint is greater!
  1045  c50f e593                       SBC yend	; vertical length
  1046  c511 aa                         TAX
  1047                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1048                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1049                          ;	LDX tmp1
  1050  c512 e8                         INX		; +1 (exit on 0)
  1051  c513 38                 	SEC		; for subtraction, never changed!
  1052                          vl_nextline
  1053  c514 a596                       LDA tmp2
  1054  c516 20f503                     JSR gmask	; modify 
  1055  c519 88                         DEY		; go up
  1056  c51a 100e                       BPL +
  1057  c51c a5a5                       LDA gaddr	; C=1
  1058  c51e e940               	SBC #$40	; gaddr -= 320
  1059  c520 85a5                       STA gaddr
  1060  c522 a5a6                       LDA gaddr+1
  1061  c524 e901                       SBC #$01
  1062  c526 85a6                       STA gaddr+1
  1063  c528 a007                       LDY #7		; wrap y offset
  1064  c52a ca                 +	DEX		; all vertical positions done?
  1065  c52b d0e7                       BNE vl_nextline
  1066  c52d 4c3cc2                     JMP gexit	; leave
  1067                          
  1068                          
  1069                          ;-----------------------------------------------------------------
  1070                          
  1071                          line
  1072  c530 20aec3                     JSR getxy	; get startpoint
  1073  c533 849b                       STY xl 
  1074  c535 859c                       STA xh
  1075  c537 86aa                       STX y
  1076                          
  1077  c539 20abc3                     JSR getcommaxy	; get endpoint
  1078                          line_start
  1079  c53c 8c3c03                     STY savexl	; save as cursor position too
  1080  c53f 849e                       STY xendl
  1081  c541 8d3d03                     STA savexh
  1082  c544 859f                       STA xendh
  1083  c546 8e3e03                     STX savey
  1084  c549 8693                       STX yend
  1085                          
  1086  c54b a000                       LDY #$00	; initialize to 0
  1087  c54d 84a8                       STY ydir
  1088  c54f 8495                       STY kl
  1089  c551 8496                       STY kh
  1090                          
  1091  c553 38                         SEC
  1092  c554 a59b                       LDA xl		; calculate dx
  1093  c556 e59e                       SBC xendl
  1094  c558 85ab                       STA dxl
  1095  c55a a59c                       LDA xh
  1096  c55c e59f                       SBC xendh
  1097  c55e 85a7                       STA dxh
  1098                          
  1099  c560 b018                       BCS li_xend_left
  1100                          	; dx != 0
  1101                          			; negate dx:
  1102  c562 98                         TYA		; Y=A=0
  1103  c563 38                         SEC		; dx = 0 - dx
  1104  c564 e5ab                       SBC dxl
  1105  c566 85ab                       STA dxl
  1106  c568 98                         TYA		; Y=A=0
  1107  c569 e5a7                       SBC dxh
  1108  c56b 85a7                       STA dxh
  1109                          			; C=0 always, needed later
  1110  c56d 2080c2             	jsr swap_x_xend
  1111  c570 a6aa                       LDX y		; swap y
  1112  c572 a493                       LDY yend
  1113  c574 8693                       STX yend
  1114  c576 84aa                       STY y
  1115                          
  1116  c578 9007                       BCC li_x_different
  1117                          			; C=0 always (from negation before)
  1118                          
  1119                          li_xend_left
  1120                                  		; A already contains dxh
  1121  c57a 05ab                       ORA dxl		; dx = 0?
  1122  c57c d003                       BNE li_x_different
  1123  c57e 4cf3c4                     JMP vline_start	; vertical line case
  1124                          
  1125                          li_x_different
  1126  c581 38                         SEC		; calculate dy
  1127  c582 a593                       LDA yend
  1128  c584 e5aa                       SBC y
  1129  c586 b006                       BCS li_y_right	; yend >= y?
  1130  c588 49ff                       EOR #$FF	; no, negate dy (two's complement)
  1131  c58a 6901                       ADC #$01	; C=0
  1132  c58c 85a8                       STA ydir	; always not 0: flag y goes up
  1133                          
  1134                          li_y_right
  1135  c58e 85a9                       STA dy
  1136  c590 d007                       BNE +
  1137  c592 a900               	LDA #0		; line thickness = 1
  1138  c594 85a3               	STA ycount
  1139  c596 4c2dc4                     JMP hline_start	; horizontal line case
  1140                          +
  1141                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1142                          
  1143  c599 a5a7                       LDA dxh		; dx > dy
  1144  c59b d01c                       BNE line_flat	; yes -> flat
  1145  c59d a5a9                       LDA dy		; no -> steep
  1146  c59f aa                         TAX
  1147  c5a0 c5ab                       CMP dxl
  1148  c5a2 9015                       BCC line_flat
  1149                          
  1150                          line_steep
  1151  c5a4 e8                         INX	
  1152  c5a5 86a3                       STX cl		; c = dy+1
  1153  c5a7 4a                         LSR		; dy/2
  1154  c5a8 49ff               	EOR #$FF	; one's complement
  1155  c5aa 8595                       STA kl		; k = -dy/2 -1
  1156                          
  1157  c5ac 2044c2                     JSR ginit	; map in graphic memory
  1158                          
  1159  c5af a5a8                       LDA ydir
  1160  c5b1 d003                       BNE +
  1161  c5b3 4c6ec3                     JMP line_down_steep	; y down, steep
  1162  c5b6 4c91c2             +	JMP line_up_steep	; y up, steep
  1163                          
  1164                          line_flat
  1165  c5b9 a5a7                       LDA dxh
  1166  c5bb a8                         TAY
  1167  c5bc a6ab                       LDX dxl
  1168  c5be e8                         INX
  1169  c5bf d001                       BNE +
  1170  c5c1 c8                         INY
  1171  c5c2 86a3               +	STX cl		; c = dx+1
  1172  c5c4 84a4                       STY ch
  1173                          
  1174  c5c6 4a                         LSR		; dx/2 high
  1175  c5c7 49ff               	EOR #$FF	; one's complement
  1176  c5c9 8596                       STA kh
  1177  c5cb a5ab                       LDA dxl
  1178  c5cd 6a                         ROR		; dx/2 low
  1179  c5ce 49ff               	EOR #$FF	; one's complement
  1180  c5d0 8595                       STA kl		; k = -dx/2 - 1
  1181                          
  1182  c5d2 2044c2                     JSR ginit	; map in graphic memory
  1183                          
  1184  c5d5 a5a8                       LDA ydir	
  1185  c5d7 d003                       BNE +
  1186  c5d9 4c1dc3                     JMP line_down_flat	; y down, flat
  1187  c5dc 4ccdc2             +	JMP line_up_flat	; y up, flat
  1188                          
  1189                          ;-----------------------------------------------------------------
  1190                          
  1191                          plot
  1192  c5df 20aec3                     JSR getxy	; get parameter
  1193  c5e2 859c                       STA xh		; save x/y
  1194  c5e4 849b                       STY xl
  1195  c5e6 86aa                       STX y
  1196  c5e8 8d3d03                     STA savexh	; and store as cursor
  1197  c5eb 8c3c03                     STY savexl
  1198  c5ee 8e3e03                     STX savey
  1199                          
  1200                          plot_start
  1201  c5f1 205bc2                     JSR position	; calculate graphical address
  1202                          
  1203  c5f4 a501                       LDA prozport
  1204  c5f6 29fd                       AND #%11111101	; Kernal ROM disable
  1205  c5f8 78                         SEI			
  1206  c5f9 8501                       STA prozport
  1207                          
  1208  c5fb 20ed03                     JSR gchange	; change graphical data
  1209                          
  1210  c5fe a501                       LDA prozport
  1211  c600 0902                       ORA #%00000010	; kernal ROM enable
  1212  c602 8501                       STA prozport
  1213  c604 58                         CLI
  1214  c605 60                         RTS
  1215                          
  1216                          ;-----------------------------------------------------------------
  1217                          
  1218                          move
  1219  c606 20aec3                     JSR getxy	; get parameter
  1220  c609 8d3d03                     STA savexh	; just save as cursor
  1221  c60c 8c3c03                     STY savexl
  1222  c60f 8e3e03                     STX savey
  1223  c612 60                         RTS
  1224                          
  1225                          
  1226                          ;-----------------------------------------------------------------
  1227                          
  1228                          ; never touches X, Y, C-flag
  1229                          ; on exit: A corrupted, Z=0
  1230                          
  1231                          range_error
  1232  c613 ad3f03             	LDA savemo
  1233  c616 29f0               	AND #$F0
  1234  c618 d003               	BNE +
  1235  c61a 68                 	PLA			; cleanup JSR
  1236  c61b 68                 	PLA			; highbyte of return address >0
  1237  c61c 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1238                          				; error mode 2: cut value: control back
  1239                          				; to handle value correction
  1240                          				; Z=0
  1241  c61d 2920               +	AND #$20
  1242  c61f d0fb               	BNE -			; Z=0
  1243  c621 68                 	PLA			; cleanup JSR
  1244  c622 68                 	PLA
  1245                          setmode_error
  1246  c623 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1247                          
  1248                          ;-----------------------------------------------------------------
  1249                          
  1250                          setmode
  1251  c626 209eb7                     JSR b_get8bit
  1252  c629 e003                       CPX #3
  1253  c62b 9013                       BCC +			; less then 3, modification mode
  1254  c62d e006               	CPX #6
  1255  c62f b0f2               	BCS setmode_error	; out of range
  1256                          				; error mode
  1257  c631 8a                 	TXA
  1258  c632 690d               	ADC #13			; C=0, therefore -3
  1259                          				; 3-5 -> 16-18
  1260                          				; put A's bit 4-7 into savemo
  1261  c634 4d3f03             	EOR savemo		; ********
  1262  c637 29f0               	AND #%11110000		; ****0000
  1263  c639 4d3f03             	EOR savemo		; AAAAmmmm
  1264  c63c 8d3f03             	STA savemo		; 
  1265  c63f 60                 	RTS
  1266                          
  1267  c640 8a                 +	TXA
  1268  c641 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1269  c644 290f               	AND #%00001111
  1270  c646 4d3f03             	EOR savemo
  1271  c649 8d3f03             	STA savemo
  1272                          setmode_enter
  1273  c64c e001               	CPX #$01
  1274  c64e b01a                       BCS set_or_toggle
  1275                          
  1276                          modereset
  1277  c650 a9c1                       LDA #>(nbitmask)
  1278  c652 8df103                     STA gchange_op+2
  1279  c655 a969                       LDA #<(nbitmask)
  1280  c657 8df003                     STA gchange_op+1
  1281  c65a a93d                       LDA #$3D		; opcode AND abs,X
  1282  c65c 8def03                     STA gchange_op
  1283  c65f a931                       LDA #$31		; opcode AND (zp),Y
  1284  c661 8df703                     STA gmask_op
  1285  c664 a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1286  c666 8df603                     STA gmask_flip+1
  1287  c669 60                         RTS
  1288                          
  1289                          set_or_toggle
  1290  c66a d01a                       BNE modetoggle
  1291                          modeset
  1292  c66c a9c1                       LDA #>(bitmask)
  1293  c66e 8df103                     STA gchange_op+2
  1294  c671 a961                       LDA #<(bitmask)
  1295  c673 8df003                     STA gchange_op+1
  1296  c676 a91d                       LDA #$1D		; opcode OR abs,X
  1297  c678 8def03                     STA gchange_op
  1298  c67b a911                       LDA #$11		; opcode OR (zp),Y
  1299  c67d 8df703                     STA gmask_op
  1300  c680 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1301  c682 8df603                     STA gmask_flip+1
  1302  c685 60                         RTS
  1303                          
  1304                          modetoggle
  1305  c686 a9c1                       LDA #>(bitmask)
  1306  c688 8df103                     STA gchange_op+2
  1307  c68b a961                       LDA #<(bitmask)
  1308  c68d 8df003                     STA gchange_op+1
  1309  c690 a95d                       LDA #$5D		; opcode EOR abs,X
  1310  c692 8def03                     STA gchange_op
  1311  c695 a951                       LDA #$51		; opcode EOR (zp),Y
  1312  c697 8df703                     STA gmask_op
  1313  c69a a900                       LDA #$00		; mask, EOR #$00, not inverting
  1314  c69c 8df603                     STA gmask_flip+1
  1315  c69f 60                         RTS
  1316                          
  1317                          
  1318                          ;-----------------------------------------------------------------
  1319                          ; get current x cursor position
  1320                          
  1321                          getposx
  1322  c6a0 ac3c03             	LDY savexl
  1323  c6a3 ad3d03             	LDA savexh
  1324  c6a6 2091b3             	JSR b_word2fac
  1325  c6a9 4c7300             	JMP chrget	; last position of expression (function name)
  1326                          
  1327                          ;-----------------------------------------------------------------
  1328                          ; get current y cursor position
  1329                          
  1330                          getposy
  1331  c6ac ac3e03             	LDY savey
  1332  c6af 20a2b3             	JSR b_byte2fac
  1333  c6b2 4c7300             	JMP chrget	; last position of expression (function name)
  1334                          
  1335                          ;-----------------------------------------------------------------
  1336                          
  1337                          ; get pixel (check if pixel set)
  1338                          ; not used
  1339                          
  1340                          get
  1341  c6b5 207300             	JSR chrget	; advance past function name
  1342  c6b8 20faae             	JSR b_chkparl	; "("?
  1343  c6bb 20aec3                     JSR getxy	; get X,Y values
  1344  c6be 859c                       STA xh
  1345  c6c0 849b                       STY xl
  1346  c6c2 86aa                       STX y
  1347  c6c4 207900             	JSR chrgot
  1348  c6c7 20f7ae             	JSR b_chkparr	; ")"?
  1349                          	
  1350                          
  1351  c6ca 205bc2                     JSR position	; calculate graphic address/position
  1352                          
  1353  c6cd a501                       LDA prozport
  1354  c6cf 29fd               	AND #%11111101	; Kernal ROM disable
  1355  c6d1 78                         SEI
  1356  c6d2 8501                       STA prozport
  1357                          
  1358  c6d4 b1a5                       LDA (gaddr),Y
  1359  c6d6 3d61c1                     AND bitmask,X	; mask position
  1360  c6d9 a8                         TAY
  1361  c6da a501                       LDA prozport
  1362  c6dc 0902               	ORA #%00000010	; kernal ROM enable
  1363  c6de 8501                       STA prozport
  1364  c6e0 58                         CLI
  1365  c6e1 98                 	TYA
  1366  c6e2 f002               	BEQ +
  1367  c6e4 a001               	LDY #1		; <> 0 -> always return 1
  1368  c6e6 4ca2b3             +	JMP b_byte2fac	; still on expr.'s last character
  1369                          
  1370                          ;-----------------------------------------------------------------
  1371                          
  1372                          relto
  1373  c6e9 208aad                     JSR b_getval	; get X offset (+/-)
  1374  c6ec a561               	LDA facexp	; FAC exponent
  1375  c6ee c990               	CMP #$90	; more than 16 bit
  1376  c6f0 b031               	BCS relto_error	; illegal quantity
  1377  c6f2 209bbc                     JSR b_fac2int	; to signed integer
  1378                          
  1379  c6f5 18                         CLC
  1380  c6f6 a565                       LDA facintl
  1381  c6f8 6d3c03                     ADC savexl
  1382  c6fb 859e                       STA xendl
  1383  c6fd a564                       LDA facinth
  1384  c6ff 6d3d03                     ADC savexh
  1385  c702 859f                       STA xendh	; xend = savex+facint
  1386                          
  1387  c704 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1388  c707 208aad                     JSR b_getval
  1389  c70a a561                       LDA facexp	; FAC exponent
  1390  c70c c990                       CMP #$90	; more than 16 bit
  1391  c70e b013                       BCS relto_error	; illegal quantity
  1392  c710 209bbc                     JSR b_fac2int	; to signed integer
  1393  c713 18                         CLC
  1394  c714 a565                       LDA facintl
  1395  c716 6d3e03                     ADC savey
  1396  c719 8593                       STA yend	; yend = savey+facint
  1397                          
  1398  c71b a59f                       LDA xendh	; check end coord. x
  1399  c71d c901                       CMP #>xmax
  1400  c71f 900e                       BCC rt_xok
  1401  c721 f003                       BEQ +
  1402                          relto_error
  1403  c723 2013c6                     JSR range_error
  1404  c726 a59e               +	LDA xendl
  1405  c728 c940                       CMP #<xmax
  1406  c72a 9003                       BCC +
  1407  c72c 2013c6                     JSR range_error
  1408                          +
  1409                          rt_xok
  1410  c72f a593                       LDA yend	; check end coord. y
  1411  c731 c9c8                       CMP #ymax
  1412  c733 9003                       BCC +
  1413  c735 2013c6                     JSR range_error
  1414                          +
  1415  c738 ad3c03                     LDA savexl
  1416  c73b 859b                       STA xl
  1417  c73d ad3d03                     LDA savexh
  1418  c740 859c                       STA xh
  1419  c742 ad3e03                     LDA savey
  1420  c745 85aa                       STA y
  1421  c747 a49e                       LDY xendl
  1422  c749 a59f                       LDA xendh
  1423  c74b a693                       LDX yend	; xend/yend = cursor + x/y
  1424                          
  1425  c74d 4c3cc5                     JMP line_start	; draw line x/y to xend/yend
  1426                          
  1427                          
  1428                          ;-----------------------------------------------------------------
  1429                          
  1430                          char
  1431  c750 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1432  c753 e028                       CPX #40	
  1433  c755 9003                       BCC +
  1434                          char_error
  1435  c757 4c48b2                     JMP b_illquant
  1436  c75a 86fb               +	STX gpos	; save x coord.
  1437  c75c 20f1b7                     JSR b_getcomma8bit
  1438                          			; get char. position y 0-24
  1439  c75f e019                       CPX #25
  1440  c761 b0f4                       BCS char_error
  1441  c763 86fc                       STX gpos+1	; save y coord.
  1442                          
  1443  c765 20fdae                     JSR b_getcomma	; get string
  1444  c768 209ead                     JSR b_getexpr
  1445  c76b 20a3b6                     JSR b_stringval ; string address in str
  1446  c76e 48                         PHA		; string length
  1447  c76f a6fc                       LDX gpos+1	; y coord. for char. position
  1448  c771 8a                         TXA
  1449  c772 2903                       AND #$03	; mask 2 bits
  1450  c774 a8                         TAY		; table index
  1451  c775 a900                       LDA #$00
  1452  c777 85fc                       STA gpos+1	; x high
  1453  c779 a5fb                       LDA gpos	; saved x: multiply by 8
  1454  c77b 0a                         ASL
  1455  c77c 0a                         ASL
  1456  c77d 0a                         ASL
  1457  c77e 26fc                       ROL gpos+1	; overflow to high byte
  1458  c780 7971c1                     ADC ytabl,Y
  1459  c783 85a5                       STA gaddr
  1460  c785 a5fc                       LDA gpos+1	; x high
  1461  c787 7d75c1                     ADC ytabh,X
  1462  c78a 85a6                       STA gaddr+1
  1463  c78c 68                         PLA		; string length
  1464  c78d a000                       LDY #$00	; string index
  1465  c78f aa                         TAX		; length
  1466  c790 e8                         INX		; prepare as counter
  1467                          char_loop
  1468  c791 ca                         DEX
  1469  c792 f008                       BEQ char_exit
  1470  c794 b122                       LDA (str),Y	; read string
  1471  c796 209dc7                     JSR char_display
  1472  c799 c8                         INY
  1473  c79a d0f5                       BNE char_loop
  1474                          char_exit
  1475  c79c 60                         RTS
  1476                          
  1477                          char_display
  1478  c79d 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1479  c79f 8a                         TXA		; save register X+Y
  1480  c7a0 48                         PHA
  1481  c7a1 98                         TYA
  1482  c7a2 48                         PHA
  1483  c7a3 a5d7                       LDA z_tmp	; get saved character
  1484  c7a5 3012                       BMI char_inverse
  1485                          
  1486                          char_normal
  1487  c7a7 c920                       CMP #$20	; control character?
  1488  c7a9 9054                       BCC char_disp_leave
  1489  c7ab c960                       CMP #$60
  1490  c7ad 9004                       BCC +
  1491  c7af 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1492  c7b1 d014                       BNE char_hires
  1493  c7b3 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1494  c7b5 d010               	BNE char_hires
  1495  c7b7 f00e               	BEQ char_hires
  1496                          
  1497                          char_inverse
  1498  c7b9 297f                       AND #%01111111	; mask bit 7
  1499  c7bb c97f                       CMP #%01111111	; was 255? (pi)
  1500  c7bd d002                       BNE +
  1501  c7bf a95e                       LDA #$5E	; screen code for pi
  1502  c7c1 c920               +	CMP #$20	; control character?
  1503  c7c3 903a                       BCC char_disp_leave
  1504                          			; yes, skip
  1505  c7c5 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1506                          			; $C0-$FF -> $40-$7F
  1507                          			; OPT: BNE char_hires
  1508                          			; OPT: char_normal
  1509                          char_hires
  1510  c7c7 a6c7                       LDX z_reverseflag
  1511  c7c9 f002                       BEQ +
  1512  c7cb 0980                       ORA #%10000000	; invert char.
  1513  c7cd aa                 +	TAX		; save char. for later
  1514  c7ce a501                       LDA prozport	; save prozport state
  1515  c7d0 48                 	PHA
  1516  c7d1 a921                       LDA #%00100001	; char. rom, no basic and kernal rom
  1517  c7d3 78                         SEI
  1518  c7d4 8501                       STA prozport	; char. rom base = $D000
  1519  c7d6 a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1520  c7d8 85fc                       STA gpos+1	; 
  1521  c7da 8a                         TXA		; char. code
  1522  c7db 0a                         ASL		; *8
  1523  c7dc 26fc                       ROL gpos+1
  1524  c7de 0a                         ASL
  1525  c7df 26fc                       ROL gpos+1
  1526  c7e1 0a                         ASL
  1527  c7e2 26fc                       ROL gpos+1
  1528  c7e4 85fb                       STA gpos	; addr. in char. rom for char.
  1529                          
  1530  c7e6 a007                       LDY #$07	; 8 hires lines
  1531                          char_line
  1532  c7e8 b1fb                       LDA (gpos),Y	; read character line
  1533  c7ea 20f503                     JSR gmask	; write to hires screen
  1534  c7ed 88                         DEY
  1535  c7ee 10f8                       BPL char_line
  1536                          
  1537  c7f0 68                 	PLA
  1538  c7f1 8501                       STA prozport
  1539  c7f3 58                         CLI
  1540                          
  1541  c7f4 18                         CLC		; step char position to left
  1542  c7f5 a5a5                       LDA gaddr	; ( +8 )
  1543  c7f7 6908                       ADC #$08
  1544  c7f9 85a5                       STA gaddr
  1545  c7fb 9002                       BCC +
  1546  c7fd e6a6                       INC gaddr+1
  1547                          +
  1548                          char_disp_leave
  1549  c7ff 68                 	PLA		; pass written character back
  1550  c800 a8                         TAY		; restore saved registers
  1551  c801 68                         PLA
  1552  c802 aa                         TAX
  1553  c803 60                         RTS
  1554                          
  1555                          
  1556                          ;-----------------------------------------------------------------
  1557                          
  1558                          to
  1559  c804 ad3c03                     LDA savexl
  1560  c807 859b                       STA xl
  1561  c809 ad3d03                     LDA savexh
  1562  c80c 859c                       STA xh
  1563  c80e ad3e03                     LDA savey
  1564  c811 85aa                       STA y
  1565  c813 20aec3                     JSR getxy
  1566  c816 4c3cc5                     JMP line_start
  1567                          
  1568                          ;-----------------------------------------------------------------
  1569                          
  1570                          fill
  1571  c819 20aec3                     JSR getxy
  1572  c81c 859c                       STA xh			; save x/y
  1573  c81e 849b                       STY xl
  1574  c820 86aa                       STX y
  1575  c822 8d3d03                     STA savexh		; and store as cursor
  1576  c825 8c3c03                     STY savexl
  1577  c828 8e3e03                     STX savey
  1578                                  
  1579  c82b a531                       LDA basaryend		; initialize fill stack pointer
  1580  c82d 38                 	SEC
  1581  c82e e904               	SBC #4			; one element below
  1582  c830 85fd                       STA fstack		; use space between basic arrays
  1583  c832 a532                       LDA basaryend+1		; and string heap bottom
  1584  c834 e900               	SBC #0			; take borrow
  1585  c836 85fe                       STA fstack+1
  1586                          
  1587  c838 205bc2             	JSR position		; graphic position in (gaddr)+Y, bit X
  1588                          
  1589  c83b a59c               	LDA xh			; setup 8x8 block index (x8)
  1590  c83d 4a                 	LSR			; high bit into C
  1591  c83e a59b               	LDA xl
  1592  c840 2a                 	ROL			; take high bit
  1593  c841 4a                 	LSR
  1594  c842 4a                 	LSR			; finally divide by 8
  1595  c843 85a7               	STA x8			; = index of 8x8 block in bitmap
  1596                          
  1597                          	; set fmode (from mode)
  1598  c845 ad3f03             	LDA savemo
  1599  c848 2903               	AND #3
  1600  c84a aa                 	TAX
  1601  c84b ca                 	DEX
  1602  c84c 3003               	BMI +			; mode = 0 -> invertmask: $FF
  1603  c84e f001               	BEQ +			; mode = 1 -> invertmask: $00
  1604  c850 ca                 	DEX			; mode = 2 -> ? (same as mode=0)
  1605  c851 86a8               +	STX fmode		; mode set or reset
  1606                          
  1607  c853 2044c2             	JSR ginit		; map in bitmap memory
  1608                          
  1609  c856 b1a5               	LDA (gaddr),y		; graphic position in Y (in index in 8x8 block)
  1610  c858 45a8               	EOR fmode
  1611  c85a 8595               	STA tmp1		; bitmap, for later usage
  1612                          
  1613  c85c 3d61c1             	AND bitmask,x		; test start pixel
  1614  c85f f003               	BEQ +			; not set
  1615                          f_exit
  1616  c861 4c3cc2             	JMP gexit		; leave if start pixel is already set
  1617                          +
  1618                          f_start				; the start: in mid of a line to fill ...
  1619  c864 a900               	LDA #0
  1620  c866 8596               	STA fcont		; initialize continuation flag for line above und below
  1621                          
  1622  c868 a595               	LDA tmp1		; graphic pixel data
  1623                          				; extent bitmask to the right
  1624  c86a 86ab               	STX xsave
  1625  c86c 3d8ec1             	AND maskleft,x		; mask out left part, bits right from starting point remain
  1626  c86f 20ffc9             	JSR bitposr		; find the first set bit from start to right (border)
  1627  c872 bd97c1             	LDA maskright0,x	; get a mask from the right border to left
  1628  c875 85a3               	STA tmpmask		
  1629                          
  1630                          leftcont
  1631  c877 a595               	LDA tmp1		; graphic pixel data
  1632  c879 a6ab               	LDX xsave
  1633                          leftcont_a
  1634  c87b 3d98c1             	AND maskright,x		; mask out right part, bits left from starting point remain
  1635  c87e f00e               	BEQ stepleft8		; no left border in this pixel line
  1636  c880 20f3c9             	JSR bitposl		; find the first set bit from start to left (border)
  1637  c883 bd8ec1             	LDA maskleft0,x		; get a mask from the left border to right
  1638  c886 25a3               	AND tmpmask		; intersect masks
  1639  c888 85a3               	STA tmpmask		; and store it for later
  1640  c88a f021               	BEQ next_block		; empty mask immediate continue to right
  1641  c88c d047               	BNE to_right		; start to walk and fill to the right border
  1642                          
  1643                          stepleft8
  1644  c88e a5a7               	LDA x8 			; 8x8 block position
  1645  c890 f043               	BEQ to_right		; =0, hit screen border
  1646  c892 c6a7               	DEC x8			; count step 8x8 block to left
  1647  c894 a9ff               	LDA #$ff
  1648  c896 85a3               	STA tmpmask		; initial mask full pixel line
  1649                          
  1650  c898 38                 	SEC 			; graphic address to to next pixel line/block
  1651  c899 a5a5               	LDA gaddr
  1652  c89b e908               	SBC #8
  1653  c89d b002               	BCS +
  1654  c89f c6a6               	DEC gaddr+1
  1655  c8a1 85a5               +	STA gaddr
  1656                          
  1657                          	; y left unchanged
  1658  c8a3 b1a5               	LDA (gaddr),y		; real graphic pixel data from bitmap
  1659  c8a5 45a8               	EOR fmode		; set/reset mode
  1660  c8a7 8595               	STA tmp1		; graphic pixel data
  1661  c8a9 a207               	LDX #7			; start bit 0 (index 7, rightmost)
  1662  c8ab d0ce               	BNE leftcont_a		; loop to left border search
  1663                          	
  1664                          next_block
  1665  c8ad e6a7               	INC x8			; step right a block
  1666  c8af a5a7               	LDA x8
  1667  c8b1 c928               	CMP #40			; beyond last horizontal block?
  1668  c8b3 b077               	BCS process_stack	; done if right screen border
  1669                          	; C = 0
  1670  c8b5 a5a5               	LDA gaddr		; advance to block right
  1671  c8b7 6908               	ADC #8			; gaddr = gaddr + 8
  1672  c8b9 85a5               	STA gaddr
  1673  c8bb 9002               	BCC +
  1674  c8bd e6a6               	INC gaddr+1
  1675  c8bf a9ff               +	LDA #$ff		; asume "all pixels" mask
  1676  c8c1 85a3               	STA tmpmask
  1677  c8c3 b1a5               	LDA (gaddr),y		; pixel data
  1678  c8c5 45a8               	EOR fmode		; set/reset mode
  1679  c8c7 f00c               	BEQ to_right		; empty -> finally to to_right
  1680  c8c9 20ffc9             	JSR bitposr		; search right border
  1681  c8cc bd97c1             	LDA maskright0,x	; mask out the right part
  1682  c8cf 25a3               	AND tmpmask		; shorten mask accordingly
  1683  c8d1 85a3               	STA tmpmask
  1684  c8d3 f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1685                          				; leading to 0 mask (fill_check wont't
  1686                          				; handle this special case)
  1687                          
  1688                          				; continue to fill to right ...
  1689                          to_right			; fill loop towards right border
  1690  c8d5 a5a3               	LDA tmpmask		; fill mask
  1691                          				; assert:    (bitmap & tempmask) == 0
  1692                          				;         || (bitmap & tempmask) == tempmask
  1693  c8d7 51a5               	EOR (gaddr),y		; set/reset to fill
  1694  c8d9 91a5               	STA (gaddr),y		; into bitmap - the actual fill action!
  1695                          	
  1696                          check_above
  1697  c8db 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1698                          				; c = 0!
  1699  c8dd 84a9               	STY ysave		; to be restored later
  1700  c8df a5a5               	LDA gaddr		; current graphic position
  1701  c8e1 a6a6               	LDX gaddr+1
  1702  c8e3 88                 	DEY			; line above
  1703  c8e4 100f               	BPL +			; leaving 8x8 block?
  1704                          	; c=0 (asl fcont)
  1705  c8e6 e93f               	SBC #$40-1		; block above:
  1706  c8e8 85fb               	STA caddr		; caddr = gaddr - $140
  1707  c8ea 8a                 	TXA
  1708  c8eb e901               	SBC #$01
  1709  c8ed aa                 	TAX
  1710  c8ee c9e0               	CMP #>gram		; still graphic ram?
  1711  c8f0 900a               	BCC skip_above
  1712  c8f2 a007               	LDY #7			; last line in block in new block
  1713  c8f4 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1714  c8f5 85fb               +	STA caddr		; still in same block
  1715  c8f7 86fc               ++	STX caddr+1		; shared store
  1716  c8f9 208ac9             	JSR fill_check
  1717                          skip_above
  1718                          
  1719                          check_below
  1720  c8fc 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1721  c8fe a5a5               	LDA gaddr		; current graphic position
  1722  c900 a6a6               	LDX gaddr+1
  1723  c902 a4a9               	LDY ysave		; restore original y position
  1724  c904 c8                 	INY			; line below
  1725  c905 c008               	CPY #8			; crossing 8x8 block?
  1726  c907 9014               	BCC +			; less then 8
  1727                          	; c=1 (cpy)
  1728  c909 693f               	ADC #$40-1		; block below: accu has gaddr
  1729  c90b 85fb               	STA caddr		; caddr = gaddr + $140
  1730  c90d a8                 	TAY			; for compare later
  1731  c90e 8a                 	TXA			; gaddr high
  1732  c90f 6901               	ADC #$01
  1733  c911 aa                 	TAX
  1734  c912 b010               	BCS skip_below		; > $10000  -> skip
  1735  c914 c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1736  c916 e9ff               	SBC #>(gram+8000)
  1737  c918 b00a               	BCS skip_below		; greater, so skip
  1738  c91a a000               	LDY #0			; first line in block
  1739  c91c 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1740  c91d 85fb               +	STA caddr		; transfer unchanged
  1741  c91f 86fc               ++	STX caddr+1		; shared store
  1742  c921 208ac9             	JSR fill_check
  1743                          skip_below
  1744                          
  1745  c924 a4a9               	LDY ysave		; restore original y position
  1746  c926 a5a3               	LDA tmpmask		; mask:
  1747  c928 2901               	AND #%00000001		; open to right, continue?
  1748  c92a d081               	BNE next_block		; to next block if open
  1749                          ; long branch version
  1750                          ;	BEQ process_stack	; not open, finished
  1751                          ;	JMP next_block		; to next block if open
  1752                          
  1753                          process_stack
  1754  c92c a5fd               	LDA fstack		; stack empty?
  1755  c92e c531               	CMP basaryend
  1756  c930 a5fe               	LDA fstack+1
  1757  c932 e532               	SBC basaryend+1
  1758  c934 b003               	BCS +			; fstack >= basaryend -> not empty
  1759  c936 4c3cc2             	JMP gexit		; empty, we are finished
  1760                          
  1761  c939 a003               +	LDY #4-1		; top of stack, element's last component
  1762  c93b b1fd               	LDA (fstack),y
  1763  c93d 85a7               	STA x8			; 8x8 block position
  1764  c93f 88                 	DEY
  1765  c940 b1fd               	LDA (fstack),y
  1766  c942 85a3               	STA tmpmask		; pixel mask
  1767  c944 88                 	DEY
  1768  c945 b1fd               	LDA (fstack),y
  1769  c947 85a6               	STA gaddr+1		; graphic addr high byte
  1770  c949 88                 	DEY
  1771  c94a b1fd               	LDA (fstack),y		; graphic addr low byte combined with y-line
  1772  c94c aa                 	TAX			; needed twice
  1773  c94d 29f8               	AND #%11111000		; split off address
  1774  c94f 85a5               	STA gaddr
  1775  c951 8a                 	TXA
  1776  c952 2907               	AND #%00000111		; split off y-line
  1777  c954 a8                 	TAY
  1778                          	
  1779  c955 b1a5               	LDA (gaddr),y		; get pixels
  1780  c957 45a8               	EOR fmode		; according to set/reset
  1781  c959 aa                 	TAX			; keep it for later
  1782  c95a 25a3               	AND tmpmask		; focus on masked pixels
  1783  c95c 08                 	PHP			; save Z flag
  1784  c95d f004               	BEQ pop_stack		; all bits unset, remove from stack
  1785                          				; and fill it!
  1786  c95f c5a3               	CMP tmpmask		; all gaps filled?
  1787  c961 d010               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1788                          	; all gaps filled, next on stack 
  1789                          pop_stack
  1790  c963 38                 	SEC	
  1791  c964 a5fd               	LDA fstack		; remove entry from stack
  1792  c966 e904               	SBC #4			; entry size
  1793  c968 85fd               	STA fstack
  1794  c96a b002               	BCS +
  1795  c96c c6fe               	DEC fstack+1
  1796  c96e 28                 +	PLP
  1797  c96f d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1798                          				; all bits unset,
  1799  c971 f001               	BEQ ++			; stack already cleaned up
  1800  c973 28                 +++	PLP			; stack cleanup
  1801                          
  1802                          	; set bits outside mask to 1
  1803  c974 8a                 ++	TXA			; bitmap
  1804                          				; 00100110	
  1805  c975 49ff               	EOR #$ff		; 11011001
  1806  c977 25a3               	AND tmpmask		; 00011100 -> 00011000
  1807  c979 49ff               	EOR #$ff		; 11100111
  1808                          				; pixel outside tmpmask now set!
  1809  c97b a2ff               	LDX #$ff		; pixel gap search: first one from left
  1810  c97d e8                 -	INX
  1811  c97e 0a                 	ASL			; counting from left
  1812  c97f b0fc               	BCS -			; loop if pixel is set
  1813                          				; X has the bit number of the unset pixel
  1814  c981 b1a5               	LDA (gaddr),y		; setup value for processing a new line
  1815  c983 45a8               	EOR fmode		; set/reset mode
  1816  c985 8595               	STA tmp1		; temporary bitmap pixels
  1817  c987 4c64c8             	JMP f_start		; long (to far away) jump to fill line start
  1818                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1819                          
  1820                          
  1821                          ; Check upper or lower fill path
  1822                          ;		destroys x
  1823                          
  1824                          fill_check
  1825  c98a b1fb               	LDA (caddr),y
  1826  c98c 45a8               	EOR fmode		; pixel data
  1827  c98e aa                 	TAX			; save for later
  1828  c98f 25a3               	AND tmpmask		; mask to fill
  1829  c991 f015               	BEQ fc_cleared		; all masked pixels cleared?
  1830  c993 c5a3               	CMP tmpmask		; check for gaps
  1831  c995 f05b               	BEQ fc_exit		; all gaps filled, finished
  1832                          				; if not so, some pixels still set
  1833  c997 a5a3               	LDA tmpmask
  1834                          fc_checkstart			; no continuation, init flag based on
  1835                          				; rightmost pixel:
  1836  c999 4a                 	LSR			; mask bit 0 to carry
  1837  c99a 9019               	BCC fc_nocont		; maskbit empty?
  1838  c99c 8a                 	TXA			; pixel data
  1839  c99d 4a                 	LSR			; pixel bit 0 to carry
  1840  c99e b015               	BCS fc_nocont		; bit 0 set
  1841                          				; -> mask is 1 and pixel 0
  1842                          fc_cont
  1843  c9a0 a596               	LDA fcont		; set flag for continuation
  1844  c9a2 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1845  c9a4 8596               	STA fcont
  1846  c9a6 d013               	BNE push_to_stack	; always non zero
  1847                          
  1848                          fc_cleared
  1849  c9a8 a5a3               	LDA tmpmask		; pixel & mask -> 0
  1850                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1851                          				; the caller asserts that this never happens
  1852  c9aa c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1853  c9ac d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1854                          				; 8 pixel line empty
  1855  c9ae a596               	LDA fcont		; continued gap?
  1856  c9b0 2902               	AND #%00000010		; check bit 2
  1857  c9b2 f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1858  c9b4 60                 	RTS			; gap continued and already on stack, leave
  1859                          
  1860                          fc_nocont
  1861  c9b5 a596               	LDA fcont		; clear continuation flag
  1862  c9b7 29fd               	AND #%11111101		; clear bit 2
  1863  c9b9 8596               	STA fcont
  1864                          
  1865                          push_to_stack
  1866  c9bb 18                 	CLC			; fstack points to top of stack
  1867  c9bc a5fd               	LDA fstack		; to next free stack element
  1868  c9be 6904               	ADC #4			; entry size
  1869  c9c0 85fd               	STA fstack
  1870  c9c2 9002               	BCC +
  1871  c9c4 e6fe               	INC fstack+1
  1872                          +
  1873  c9c6 a534               	LDA strbot+1		; check stack space
  1874  c9c8 c5fe               	CMP fstack+1
  1875  c9ca b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  1876                          				; strbot MSB < fstack MSB
  1877                          out_of_memory			
  1878  c9cc 203cc2             	JSR gexit
  1879  c9cf a210               	LDX #$10		; out of memory error
  1880  c9d1 6c0003             	JMP (v_baserr)		; basic error handler
  1881  c9d4 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  1882  c9d6 a5fd               	LDA fstack		; MSB equal, check LSB
  1883  c9d8 c533               	CMP strbot
  1884  c9da b0f0               	BCS out_of_memory	; fstack collides with string heap!
  1885                          
  1886                          fc_put
  1887  c9dc 98                 	TYA			; y-line (value 0-7) merged with
  1888  c9dd 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  1889  c9df a000               	LDY #0			; stack structure index, on next free element
  1890  c9e1 91fd               	STA (fstack),y
  1891  c9e3 c8                 	INY
  1892  c9e4 a5fc               	LDA caddr+1
  1893  c9e6 91fd               	STA (fstack),y		; graphic address high
  1894  c9e8 c8                 	INY
  1895  c9e9 a5a3               	LDA tmpmask
  1896  c9eb 91fd               	STA (fstack),y
  1897  c9ed c8                 	INY
  1898  c9ee a5a7               	LDA x8			; 8x8 block position
  1899  c9f0 91fd               	STA (fstack),y
  1900                          	
  1901  c9f2 60                 fc_exit	RTS
  1902                          	
  1903                          
  1904                          
  1905                          ; Get the pixel position of the first set pixel from the right.
  1906                          ; 76543210  bit ->
  1907                          ; XXXXXXXX
  1908                          ; 01234567  -> index
  1909                          
  1910                          ; 00000000 -> 0 -> $FF 
  1911                          ; 10000000 -> 1 -> $7F
  1912                          ; X1000000 -> 2 -> $3F
  1913                          ; XX100000 -> 3 -> $1F
  1914                          ; XXX10000 -> 4 -> $0F
  1915                          ; XXXX1000 -> 5 -> $07
  1916                          ; XXXXX100 -> 6 -> $03
  1917                          ; XXXXXX10 -> 7 -> $01
  1918                          ; XXXXXXX1 -> 8 -> $00
  1919                          
  1920                          ; usage: lda maskleft0,X
  1921                          
  1922                          ; speed consideration: for results from X 0 to 4 it is faster than
  1923                          ; a table-driven approach.
  1924                          
  1925                          bitposl
  1926  c9f3 a2ff               	LDX #$ff
  1927  c9f5 c900               	CMP #0		; special case (no bit set at all)
  1928  c9f7 f004               	BEQ +
  1929  c9f9 e8                 -	INX
  1930  c9fa 0a                 	ASL		; shift to left
  1931  c9fb d0fc               	BNE -		; until byte is empty
  1932  c9fd e8                 +	INX
  1933  c9fe 60                 	RTS
  1934                          
  1935                          ; Get the pixel position of the first set pixel from the left.
  1936                          ; 76543210  bit ->
  1937                          ; XXXXXXXX
  1938                          ; 01234567  -> index
  1939                          
  1940                          ; 00000000 -> 8 -> $FF
  1941                          ; 00000001 -> 7 -> $FE
  1942                          ; 0000001X -> 6 -> $FC
  1943                          ; 000001XX -> 5 -> $F8
  1944                          ; 00001XXX -> 4 -> $F0
  1945                          ; 0001XXXX -> 3 -> $E0
  1946                          ; 001XXXXX -> 2 -> $C0
  1947                          ; 01XXXXXX -> 1 -> $80
  1948                          ; 1XXXXXXX -> 0 -> $00
  1949                          
  1950                          ; usage: lda maskright0,X
  1951                          
  1952                          ; speed consideration: for results of X from 4 to 8 it is faster than
  1953                          ; a table-driven approach.
  1954                          
  1955                          bitposr
  1956  c9ff a208               	LDX #8
  1957  ca01 c900               	CMP #0		; special case (no bit set at all)
  1958  ca03 f004               	BEQ +
  1959  ca05 ca                 -	DEX
  1960  ca06 4a                 	LSR		; shift to right
  1961  ca07 d0fc               	BNE -		; until byte is empty
  1962  ca09 60                 +	RTS
  1963                          
  1964                          ;-----------------------------------------------------------------
  1965                          
  1966                          unnew
  1967                          
  1968  ca0a a52b               	LDA bassta
  1969  ca0c 8522               	STA str
  1970  ca0e a52c               	LDA bassta+1
  1971  ca10 8523               	STA str+1
  1972  ca12 a001               	LDY #1
  1973  ca14 98                 	TYA
  1974  ca15 9122               	STA (str),y		; != 0
  1975                          
  1976  ca17 2033a5             	JSR b_rechain		; starting from bassta
  1977                          				; result in (str)
  1978  ca1a 18                 	CLC			; str+1 -> new basic end
  1979  ca1b a423               	LDY str+1
  1980  ca1d a522               	LDA str
  1981  ca1f 6902               	ADC #2
  1982  ca21 852d               	STA basend
  1983  ca23 9001               	BCC +
  1984  ca25 c8                 	INY
  1985  ca26 842e               +	STY basend+1
  1986  ca28 4c60a6             	JMP b_clr		; perform CLR
  1987                          
  1988                          
  1989                          ;-----------------------------------------------------------------
  1990                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
