
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
    36  c01b a93a                       lda #<author            ; message ...
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
     6                          	!text "1.34" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2022-04-24 v 1.34
    10                          ;	2022-03-27 v 1.33
    11                          ;	2020-05-03 v 1.32
    12                          ;	2019-10-30 v 1.31
    13                          ;	2019-10-24 v 1.30
    14                          ;	2019-10-10 v 1.29
    15                          ;	2016-09-10 v 1.28
    16                          ;	2016-07-13 v 1.27
    17                          ;	2016-07-09 v 1.26
    18                          ;	2016-06-21 v 1.25
    19                          ;	2016-06-16 v 1.24
    20                          ;	2016-05-29 v 1.23
    21                          ;	2016-05-20 v 1.22
    22                          ;	2016-05-16 v 1.21
    23                          ;	2016-02-23 v 1.20
    24                          ;	2016-01-15 v 1.19
    25                          ;	1992-12-28 v 1.18
    26                          ;	1986-03-24 v 1.17
    27                          ;	1985       v 0.00 - 1.16
    28                          ;
    29                          ; the initial development is based on the implemention
    30                          ; done in a Forth environment written with a common 
    31                          ; 6502 forth assembler.
    32                          ; later, the code has been pulled out from there, relocated and 
    33                          ; enriched with some glue code to finally form the first 
    34                          ; basic extension.
    35                          
    36                          ; command dispatcher style JMP/RTS
    37                          ;	(if defined)
    38                          ;command_rts_style=1
    39                          
    40                          ; error handling 
    41                          ;	(if defined)
    42                          ;no_error=1
    43                          
    44                          ; optimize for space (at runtime)
    45                          ;opt_space=1
    46                          
    47                          
    48                          ; basic interpreter registers, addresses and entry points
    49                          
    50                          type	= $0d
    51                          str     = $22		; string address
    52                          bassta	= $2b		; basic start pointer
    53                          basend	= $2d		; basic end pointer
    54                          basaryend	= $31		; basic end of array +1
    55                          strbot	= $33		; bottom of string heap 
    56                          ijmp    = $55		; address of JMP (addr)
    57                          chrget  = $73		; basic charget routine
    58                          chrgot  = $79		; basic last char got (charget routine)
    59                          txtptr	= $7A		; basic text pointer
    60                          facintl = $65		; integer result from b_fac2int
    61                          facinth = $64
    62                          facexp  = $61		; fac exponent, after b_getval
    63                          
    64                          z_reverseflag = $C7	; character routine
    65                          z_lastkey = $D7		; original use case, unused here
    66                          z_tmp = z_lastkey	; temporary reused for character routine
    67                          
    68                          v_baserr = $0300	; vector error routine
    69                          v_basstp = $0328	; vector error routine
    70                          v_bascmd = $0308	; vector interpreter parsing
    71                          v_basexp = $030a	; vector evaluate expression
    72                          
    73                          basic_rom = $A000	; start of BASIC ROM
    74                          
    75                          b_clr = $A660		; CLR command
    76                          b_interpreter = $A7AE	; interpreter loop
    77                          b_execstatement = $A7E7	; process statement (after chrget) - not used
    78                          b_execexpr =$AE92	; process expression - not used
    79                          b_getcomma = $AEFD	; read comma from basic text
    80                          b_illquant = $B248	; error "illegal quantity"
    81                          b_syntaxerror = $AF08	; error "syntax"
    82                          b_get8bit = $B79E	; read 8 bit numeric value from
    83                          			; basic text
    84                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    85                          			; from basic text
    86                          b_getval = $AD8A	; read numeric value from basic text
    87                          b_getexpr = $AD9E	; read expression from basic text
    88                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    89                          b_word2fac =$B391	; convert Y/Y to FAC (signed 16 bit)
    90                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    91                          b_fac2int = $BC9B	; convert FAC to integer
    92                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    93                          b_rechain = $A533	; rechain basic lines
    94                          b_str2fac = $BCF3	; convert string in FAC (expression handling)
    95                          b_chkparl = $AEFA 	; check '('
    96                          b_chkparr = $AEF7 	; check ')'
    97                          
    98                          t_to = $A4		; keyword TO token
    99                          
   100                          ; hardware registers and values
   101                          
   102                          prozport = $01		; processor port
   103                          memrom = %00110111	; basic+kernal rom
   104                          membas = %00110110	; basic ram+kernal rom
   105                          memram = %00110101	; basic+kernal ram
   106                          
   107                          vic_cr	= $D011		; VIC control register
   108                          vic_mcr	= $D018		; VIC memory control register
   109                          cia_pra	= $DD00		; CIA 2 port register A
   110                          
   111                          cram	= $CC00		; start of color ram
   112                          
   113                          gram	= $e000		; start of graphic bitmap ram
   114                          gramp	= gram >> 8	; start page of bitmap
   115                          
   116                          ; constants 
   117                          
   118                          xmax	= 320		; max x dimension
   119                          ymax	= 200		; max y dimension
   120                          
   121                          !ifdef opt_space {
   122                          fesize	= 3		; Fill stack entry size without block position
   123                          } else {
   124                          fesize	= 4		; Fill stack entry size with block position
   125                          }
   126                          
   127                          ; zeropage variables
   128                          
   129                          x	= $9B		; start coordinate x, low+high
   130                          xl	= x
   131                          xh	= x+1
   132                          y	= $AA		; start coordinate y
   133                          
   134                          xendl	= $9E		; end coordinate x, low+high
   135                          xendh	= $9F
   136                          yend	= $93		; end coordinate y
   137                          
   138                          kl	= $95		; gradient for lines, low+high
   139                          kh	= kl+1
   140                          tmpbits	= kl		; temp. var. (hline, vline, fill context)
   141                          tmp2	= kh		; temp. var. (hline, vline context)
   142                          fcont	= kh		; fill continuation flags (bit 1,0 for above, below)
   143                          
   144                          dxl	= $AB		; x delta, low+high
   145                          xsave	= dxl		; x register saved (hline, fill context)
   146                          dxh	= $A7
   147                          x8	= dxh		; 8x8 block index: (xh/xl) : 8 (fill context)
   148                          
   149                          dy	= $A9		; y delta
   150                          ysave	= dy		; y saved (hline context, fill context)
   151                          
   152                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   153                          ylimit	= ydir		; y limit in a 8x8 block (hline context)
   154                          fmode   = ydir		; mode mask: 0 | $FF (fill context)
   155                          
   156                          cl	= $A3		; dot count, low+high
   157                          ch	= $A4
   158                          ycount	= cl		; y count overall (hline context)
   159                          hcount	= ch		; horizontal blocks (hline context)
   160                          tmpmask	= cl		; temp. mask (fill context)
   161                          
   162                          gaddr	= $A5		; graphic address
   163                          
   164                          gpos	= $FB		; in graphic position
   165                          sgaddr	= gpos		; saved gaddr (hline context)
   166                          caddr	= gpos		; check gaddr (fill context)
   167                          
   168                          gcol	= $FD		; graphic color, in "graphic on" context only
   169                          fstack = gcol	; fill stack pointer (fill context)
   170                          
   171                          ; static ram areas
   172                          
   173                          savevpars = $0334	; original v_bascmd
   174                          saveverr = savevpars+2	; original v_baserr
   175                          savevstp = saveverr+2	; original v_basstp
   176                          savevexp = savevstp+2	; original v_basexp
   177                          savexl	= savevexp+2	; the graphic cursor: x low 
   178                          savexh	= savexl+1	; the graphic cursor: x high
   179                          savey	= savexh+1	; the graphic cursor: y
   180                          savemo	= savey+1	; the graphic mode
   181                          saveend = savemo+1	; byte after save area
   182                          
   183                          			; real place for gchange and gmask routines,
   184                          !ifdef ltc {
   185                          gramcode = $03ed - 26	; 15 bytes + 4*6+2
   186                          } else {
   187                          gramcode = $03ed	; 15 bytes
   188                          }
   189                          
   190                          ; LTC64 specifics
   191                          
   192                          !ifdef ltc {
   193                          
   194                          !cpu 65816
   195                          
   196                          bank4+3 = $040000
   197                          rombank+3 = $010000	; c't
   198                          
   199                          ; c't-Karte-Kontrollregister
   200                          
   201                          memconf = bank4 or 1
   202                          mc_off  = $80		; CPU 816 ausschalten
   203                          mc_slow = $40		; CPU 1 MHz
   204                          mc_epr  = $20		; EPROM in Bank0
   205                          mc_sim  = $10		; ROM-Simulation Bit
   206                          
   207                          }
   208                          
   209                          
   210                          
   211                          ;
   212                          ; initialize extension
   213                          
   214                          init
   215  c025 ad0803                     LDA v_bascmd		; check if hooks are already 
   216  c028 ae0903                     LDX v_bascmd+1		; in place 
   217  c02b c9b0               	CMP #<(parse)
   218  c02d d004               	BNE +
   219  c02f e0c0               	CPX #>(parse)
   220  c031 f052               	BEQ ++			; already hooked
   221                          
   222  c033 8d3403             +       STA savevpars		; save old vector
   223  c036 8e3503             	STX savevpars+1
   224  c039 a9b0               	LDA #<(parse)		; basic interpreter parser hook
   225  c03b 8d0803                     STA v_bascmd		; for commands
   226  c03e a9c0                       LDA #>(parse)
   227  c040 8d0903                     STA v_bascmd+1
   228                          
   229  c043 ad0a03                     LDA v_basexp		; basic interpreter parser hook
   230  c046 8d3a03             	STA savevexp		; for expressions
   231  c049 a9e4                       LDA #<(express)		; with save of old pointer
   232  c04b 8d0a03                     STA v_basexp
   233  c04e ad0b03                     LDA v_basexp+1
   234  c051 8d3b03             	STA savevexp+1
   235  c054 a9c0                       LDA #>(express)
   236  c056 8d0b03                     STA v_basexp+1
   237                          
   238  c059 ad2803                     LDA v_basstp
   239  c05c 8d3803             	STA savevstp
   240  c05f a99b                       LDA #<(stop)		; basic interpreter stop hook
   241  c061 8d2803                     STA v_basstp
   242  c064 ad2903                     LDA v_basstp+1
   243  c067 8d3903             	STA savevstp+1
   244  c06a a9c0                       LDA #>(stop)
   245  c06c 8d2903                     STA v_basstp+1
   246                          
   247  c06f ad0003                     LDA v_baserr
   248  c072 8d3603             	STA saveverr
   249  c075 a995                       LDA #<(error)		; basic interpreter error hook
   250  c077 8d0003                     STA v_baserr
   251  c07a ad0103                     LDA v_baserr+1
   252  c07d 8d3703             	STA saveverr+1
   253  c080 a9c0                       LDA #>(error)
   254  c082 8d0103                     STA v_baserr+1
   255                          
   256  c085 a200               ++	LDX #0			; set graphic cursor to (0,0)
   257  c087 8e3c03             	STX savexl
   258  c08a 8e3d03             	STX savexh
   259  c08d 8e3e03             	STX savey
   260  c090 e8                 	INX
   261  c091 8e3f03             	STX savemo		; set mode 1
   262  c094 60                         RTS
   263                          
   264                          error	
   265                          	; reg A may destroyed
   266  c095 20aac1             	JSR gra_off		; uses only reg A
   267  c098 6c3603             	JMP (saveverr)		; to original vector
   268                          
   269                          stop	
   270                          	; reg A may destroyed
   271  c09b a591               	LDA $91			; Scan code
   272  c09d c97f               	CMP #$7F		; STOP key?
   273  c09f d003               	BNE nostop
   274  c0a1 20aac1             	JSR gra_off		; uses only reg A
   275                          nostop
   276  c0a4 6c3803             	JMP (savevstp)		; to original vector
   277                          
   278                          
   279                          ;-----------------------------------------------------------------
   280                          
   281                          ; undo chrget
   282                          
   283                          undo_chrget
   284  c0a7 a57a               	LDA txtptr		; decrement text pointer by 1
   285  c0a9 d002               	BNE +
   286  c0ab c67b               	DEC txtptr+1
   287  c0ad c67a               +	DEC txtptr
   288  c0af 60                 	RTS
   289                          
   290                          ;-----------------------------------------------------------------
   291                          
   292                          ; start parsing an extension command ...
   293                          
   294                          parse
   295  c0b0 207300                     JSR chrget		; next char.
   296  c0b3 c926                       CMP #'&'		; command prefix
   297  c0b5 f006                       BEQ newcmd
   298  c0b7 20a7c0             	JSR undo_chrget
   299  c0ba 6c3403             	JMP (savevpars)
   300                          newcmd
   301  c0bd 207300                     JSR chrget		; command character
   302                          
   303  c0c0 a00e                       LDY #(cmdsend-cmds)	; map character to
   304                          				; command address ...
   305                          checknextcmd
   306  c0c2 88                         DEY
   307  c0c3 f01c               	BEQ parse_error
   308  c0c5 d912c1                     CMP cmds,Y
   309  c0c8 d0f8                       BNE checknextcmd	; try next
   310  c0ca 88                         DEY			; found
   311  c0cb 98                         TYA
   312  c0cc 0a                         ASL			; *2
   313  c0cd a8                         TAY
   314                          !ifndef command_rts_tyle {
   315                          	!set co=0		; command offset in jump table
   316  c0ce b921c1                     LDA cmdaddr+1,Y		; high byte from table
   317  c0d1 8556                       STA ijmp+1
   318  c0d3 b920c1                     LDA cmdaddr,Y		; low byte from table
   319  c0d6 8555                       STA ijmp
   320  c0d8 207300                     JSR chrget		; read next byte in basic text
   321  c0db 205400                     JSR ijmp-1		; go to command by JMP (addr)
   322  c0de 4caea7                     JMP b_interpreter	; continue parsing
   323                          } else {
   324                          	!set co=1		; command offset in jump table
   325                          	LDA #>(b_interpreter-1)	; return to interpreter
   326                          	PHA
   327                          	LDA #<(b_interpreter-1)
   328                          	PHA				
   329                                  LDA cmdaddr+1,Y		; command address (RTS style)
   330                                  PHA			; high byte on stack
   331                                  LDA cmdaddr,Y		; command address (RTS style)
   332                                  PHA			; low byte on stack
   333                                  JMP chrget		; read next byte in basic text 
   334                          				; and RTS to command
   335                          }
   336                          parse_error
   337  c0e1 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
   338                          
   339                          ;-----------------------------------------------------------------
   340                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   341                          express
   342  c0e4 a900               	LDA #0
   343  c0e6 850d               	STA type	
   344  c0e8 207300             	JSR chrget
   345  c0eb b003               	BCS exp_nonumber
   346  c0ed 4cf3bc             	JMP b_str2fac
   347                          exp_nonumber
   348  c0f0 c926                       CMP #'&'		; command prefix
   349  c0f2 f006                       BEQ newfunc
   350  c0f4 20a7c0             	JSR undo_chrget
   351  c0f7 6c3a03             	JMP (savevexp)		; original routine	
   352                          ;	JMP b_execexpr
   353                          newfunc
   354  c0fa 207300             	JSR chrget
   355  c0fd c95a               	CMP #'Z'
   356  c0ff d003               	BNE +
   357  c101 4cc6c6             	JMP get
   358  c104 c958               +	CMP #'X'
   359  c106 d003               	BNE +
   360  c108 4cb1c6             	JMP getposx
   361  c10b c959               +	CMP #'Y'
   362  c10d d0d2               	BNE parse_error
   363  c10f 4cbdc6             	JMP getposy
   364                          
   365                          ;-----------------------------------------------------------------
   366                          
   367                          ; the most commonly used command placed at the end ...
   368                          
   369  c112 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   370                          cmdsend
   371                          
   372                          cmdaddr
   373  c120 48ca90c8a3c171c7...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   374  c12c 49c8fdc628c8d8c4...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
   375                          
   376  c13a 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  c144 312e3334            !text "1.34" 

; ******** Source: graext-core.asm
   378  c148 20313938362c3230...	!text " 1986,2022 JOHANN@KLASEK.AT",0
   379                          
   380                          bitmask
   381  c164 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   382                          nbitmask
   383  c16c 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   384                          ytabl
   385  c174 004080c0           	!byte $00,$40,$80,$c0
   386                          ytabh
   387  c178 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   388  c17c e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   389  c180 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   390  c184 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   391  c188 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   392  c18c f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   393  c190 fe                 	!byte gramp+$1e
   394                          
   395                          ; for horiz. line
   396                          
   397                          maskleft0
   398                          maskleft
   399  c191 ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   400  c199 00                 	!byte $00
   401                          
   402                          maskright0
   403  c19a 00                 	!byte $00
   404                          maskright
   405  c19b 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   406                          
   407                          ;-----------------------------------------------------------------
   408                          
   409                          graphic
   410  c1a3 209eb7                     JSR b_get8bit
   411  c1a6 e000                       CPX #$00
   412  c1a8 d013                       BNE gra_other
   413                          gra0				; &G 0
   414                          gra_off
   415  c1aa a9c7                       LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   416  c1ac 8d00dd                     STA cia_pra
   417  c1af a915                       LDA #((1 <<4) + (2 <<1) + 1)
   418                          				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   419                          				; char addr $1000/4096 = char. ROM
   420  c1b1 8d18d0                     STA vic_mcr		; VIC memory control
   421  c1b4 ad11d0                     LDA vic_cr		; VIC control register
   422  c1b7 29df                       AND #%11011111		; Hires mode off
   423  c1b9 8d11d0                     STA vic_cr
   424  c1bc 60                         RTS
   425                          
   426                          gra_other
   427  c1bd e001                       CPX #$01
   428  c1bf f00f               	BEQ gra1
   429  c1c1 e002               	CPX #$02
   430  c1c3 f00e                       BEQ gra2
   431  c1c5 e004               	CPX #$04
   432  c1c7 f043                       BEQ gra_clear		; &G 4 (erase only, leave mode)
   433  c1c9 e003               	CPX #$03		; &G 3 (graphic on)
   434  c1cb f029               	BEQ gra_on
   435  c1cd 4c48b2                     JMP b_illquant		; parameter illegal
   436                          	
   437                          gra1				; &G 1
   438  c1d0 200cc2             	JSR gra_clear
   439                          
   440                          gra2
   441  c1d3 20f1b7                     JSR b_getcomma8bit
   442  c1d6 8a                         TXA			; foreground color
   443  c1d7 0a                         ASL			; upper nibble
   444  c1d8 0a                         ASL
   445  c1d9 0a                         ASL
   446  c1da 0a                         ASL
   447  c1db 85fd                       STA gcol
   448  c1dd 20f1b7                     JSR b_getcomma8bit
   449  c1e0 8a                         TXA			; background color
   450  c1e1 290f                       AND #$0F
   451  c1e3 05fd                       ORA gcol
   452  c1e5 a000                       LDY #$00
   453                          cram_loop
   454  c1e7 9900cc                     STA cram,Y		; fill color RAM
   455  c1ea 9900cd                     STA cram+$100,Y
   456  c1ed 9900ce                     STA cram+$200,Y
   457  c1f0 99e8ce                     STA cram+$300-24,Y
   458  c1f3 c8                         INY
   459  c1f4 d0f1                       BNE cram_loop
   460                          
   461                          gra_on
   462  c1f6 202bc2             	JSR gra_setupcode
   463                          
   464  c1f9 a9c4                       LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   465  c1fb 8d00dd                     STA cia_pra
   466  c1fe a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   467  c200 8d18d0                     STA vic_mcr		; VIC memory control
   468  c203 ad11d0                     LDA vic_cr		; VIC control register
   469  c206 0920                       ORA #%00100000		; Bit 5 = 1: Hires on
   470  c208 8d11d0                     STA vic_cr
   471  c20b 60                         RTS
   472                          
   473                          gra_clear
   474  c20c a220                       LDX #$20		; Pages (8 KByte)
   475  c20e a9e0                       LDA #>gram
   476  c210 85fc                       STA gpos+1
   477  c212 a000                       LDY #$00
   478  c214 84fb                       STY gpos
   479  c216 98                         TYA
   480                          gra_fill
   481  c217 91fb                       STA (gpos),Y		; Loop unroll
   482  c219 c8                         INY
   483  c21a 91fb                       STA (gpos),Y
   484  c21c c8                         INY
   485  c21d 91fb                       STA (gpos),Y
   486  c21f c8                         INY
   487  c220 91fb                       STA (gpos),Y
   488  c222 c8                         INY
   489  c223 d0f2                       BNE gra_fill
   490  c225 e6fc                       INC gpos+1
   491  c227 ca                         DEX
   492  c228 d0ed                       BNE gra_fill
   493  c22a 60                 	RTS
   494                          
   495                          gra_setupcode
   496  c22b a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   497                          gra_copycode
   498  c22d bd4ec2             	LDA gromcode-1,X
   499  c230 9dec03             	STA gramcode-1,X
   500  c233 ca                 	DEX
   501  c234 d0f7               	BNE gra_copycode
   502  c236 ad3f03             	LDA savemo
   503  c239 290f               	AND #$0F
   504  c23b aa                 	TAX
   505  c23c 4c5dc6             	JMP setmode_enter	; re-apply mode to routines
   506                          				; implicit RTS
   507                          
   508                          ;-----------------------------------------------------------------
   509                          
   510                          gexit
   511  c23f a501                       LDA prozport
   512  c241 0902                       ORA #%00000010		; kernal ROM enable
   513  c243 8501                       STA prozport
   514  c245 58                         CLI			; allow interrupts
   515  c246 60                         RTS
   516                          
   517                          ;-----------------------------------------------------------------
   518                          
   519                          ginit
   520  c247 a501                       LDA prozport
   521  c249 29fd                       AND #%11111101		; Kernal ROM disable
   522  c24b 78                         SEI			; disable interrupts
   523  c24c 8501                       STA prozport
   524  c24e 60                         RTS
   525                          				; on exit Z=0
   526                          
   527                          ;-----------------------------------------------------------------
   528                          
   529                          ; These are selfmodified routines, which has to placed into RAM
   530                          ; (on every graphic "on")
   531                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   532                          
   533                          gromcode
   534                          
   535                          !pseudopc gramcode {
   536                          
   537                          ; change a graphic location
   538                          
   539                          gchange
   540                          !ifdef ltc {
   541                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   542                          	STA memconf		; damit internes RAM gelesen werden kann!
   543                          }
   544  c24f b1a5                       LDA (gaddr),Y
   545                          gchange_op
   546  c251 1d64c1                     ORA bitmask,X
   547  c254 91a5                       STA (gaddr),Y
   548                          !ifdef ltc {
   549                          	LDA #mc_sim		; vollständige ROM-Simulation
   550                          	STA memconf		; wieder schnelles RAM ab $C000
   551                          }
   552  c256 60                         RTS
   553                          
   554                          ; mask a graphic location 
   555                          
   556                          gmask
   557                          !ifdef ltc {
   558                          	XBA
   559                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   560                          	STA memconf		; damit internes RAM gelesen werden kann!
   561                          	XBA
   562                          }
   563                          gmask_flip
   564  c257 4900                       EOR #$00
   565                          gmask_op
   566  c259 11a5                       ORA (gaddr),Y
   567  c25b 91a5                       STA (gaddr),Y
   568                          !ifdef ltc {
   569                          	LDA #mc_sim		; vollständige ROM-Simulation
   570                          	STA memconf		; wieder schnelles RAM ab $C000
   571                          }
   572  c25d 60                         RTS
   573                          
   574                          }
   575                          
   576                          gromcode_end
   577                          
   578                          ;-----------------------------------------------------------------
   579                          
   580                          position
   581  c25e a5aa                       LDA y
   582  c260 4a                         LSR
   583  c261 4a                         LSR
   584  c262 4a                         LSR			; y/8
   585  c263 a8                         TAY
   586  c264 2903                       AND #%00000011		; (y/8) mod 4
   587  c266 aa                         TAX
   588  c267 a59b                       LDA xl			; x low
   589  c269 29f8                       AND #%11111000		; clear bit 2-0
   590  c26b 18                         CLC
   591  c26c 7d74c1                     ADC ytabl,X		; addr low: y base + x part
   592  c26f 85a5                       STA gaddr
   593  c271 a59c                       LDA xh			; addr high: x part
   594  c273 7978c1                     ADC ytabh,Y		; 	+ y base
   595  c276 85a6                       STA gaddr+1
   596  c278 a5aa                       LDA y			; vertical offset
   597  c27a 2907                       AND #%00000111		; y mod 8
   598  c27c a8                         TAY
   599  c27d a59b                       LDA xl
   600  c27f 2907                       AND #%00000111		; x mod 8
   601  c281 aa                         TAX			; horizonal offset
   602  c282 60                         RTS			; (bitmask)
   603                          
   604                          
   605                          ;-----------------------------------------------------------------
   606                          
   607                          ; swap tupel xl,xh <-> xendl,xendh
   608                          
   609                          swap_x_xend
   610  c283 a69e                       LDX xendl		; swap x, xend
   611  c285 a49b                       LDY xl
   612  c287 869b                       STX xl
   613  c289 849e                       STY xendl
   614                          
   615  c28b a69f                       LDX xendh
   616  c28d a49c                       LDY xh
   617  c28f 849f                       STY xendh
   618  c291 869c                       STX xh
   619  c293 60                 	RTS
   620                          
   621                          
   622                          ;-----------------------------------------------------------------
   623                          
   624                          ; line y up, x left, dx < dy (case 1)
   625                          
   626                          line_up_steep
   627  c294 205ec2                     JSR position		; x,y
   628                          loop_yup_xleft
   629  c297 20ed03                     JSR gchange		; pixel
   630                          
   631  c29a 18                         CLC			; k += dx
   632  c29b a595                       LDA kl
   633  c29d 65ab                       ADC dxl			; dxh is 0, because dx < dy
   634  c29f 8595                       STA kl
   635  c2a1 9014                       BCC +			; k >= 0 ->
   636                          
   637  c2a3 e5a9               ++	SBC dy			; k -= dy (C=1)
   638  c2a5 8595                       STA kl
   639                          
   640  c2a7 ca                  	DEX			; x--
   641  c2a8 100d                       BPL +
   642  c2aa a207                       LDX #7			; wrap around
   643  c2ac 38                 	SEC
   644  c2ad a5a5                       LDA gaddr		; x-8: gaddr -= 8
   645  c2af e908                       SBC #8
   646  c2b1 85a5                       STA gaddr
   647  c2b3 b002                       BCS +
   648  c2b5 c6a6                       DEC gaddr+1
   649                          
   650  c2b7 88                 +	DEY			; y--
   651  c2b8 100f                       BPL +++
   652  c2ba 38                         SEC			; y overflow
   653  c2bb a5a5                       LDA gaddr
   654  c2bd e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   655  c2bf 85a5                       STA gaddr
   656  c2c1 a5a6                       LDA gaddr+1
   657  c2c3 e901               	SBC #1
   658  c2c5 85a6                       STA gaddr+1
   659  c2c7 a007                       LDY #7			; wrap around
   660                          
   661  c2c9 c6a3               +++	DEC cl			; until c=0
   662  c2cb d0ca                       BNE loop_yup_xleft
   663  c2cd 4c3fc2                     JMP gexit
   664                          
   665                          
   666                          ;-----------------------------------------------------------------
   667                          
   668                          ; line x left, y up, dx > dy (case 2)
   669                          
   670                          line_up_flat
   671  c2d0 205ec2                     JSR position		; x,y
   672  c2d3 a5a3               	LDA cl			; counter adjustment for
   673  c2d5 f002               	BEQ +			; prepare for dec-dec-counting
   674  c2d7 e6a4               	INC ch
   675                          +
   676                          loop_xleft_yup
   677  c2d9 20ed03                     JSR gchange		; pixel
   678                          
   679  c2dc 18                         CLC			; k += dy
   680  c2dd a595                       LDA kl
   681  c2df 65a9                       ADC dy
   682  c2e1 8595                       STA kl
   683  c2e3 9020                       BCC +			; k < 0
   684  c2e5 e696                       INC kh
   685  c2e7 301c               	BMI +			; k < 0
   686                          
   687  c2e9 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   688  c2eb 8595                       STA kl
   689  c2ed a596                       LDA kh
   690  c2ef e5a7                       SBC dxh		
   691  c2f1 8596                       STA kh
   692                          
   693  c2f3 88                         DEY			; y--
   694  c2f4 100f                       BPL +
   695  c2f6 38                 	SEC			; C=1 not always true (SBC above)
   696  c2f7 a5a5                       LDA gaddr		; y overflow
   697  c2f9 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   698  c2fb 85a5                       STA gaddr
   699  c2fd a5a6                       LDA gaddr+1
   700  c2ff e901               	SBC #1
   701  c301 85a6                       STA gaddr+1
   702  c303 a007               	LDY #7			; wrap around
   703                          
   704  c305 ca                 +	DEX			; x--
   705  c306 100d                       BPL +++
   706  c308 a207                       LDX #7			; wrap around
   707  c30a 38                 	SEC
   708  c30b a5a5                       LDA gaddr		; x-8: gaddr -= 8
   709  c30d e908                       SBC #8
   710  c30f 85a5                       STA gaddr
   711  c311 b002                       BCS +++
   712  c313 c6a6                       DEC gaddr+1
   713                          +++
   714  c315 c6a3               	DEC cl			; c--
   715  c317 d0c0                       BNE loop_xleft_yup
   716  c319 c6a4                       DEC ch			; adjusted high which allows this
   717  c31b d0bc                       BNE loop_xleft_yup
   718                          
   719  c31d 4c3fc2                     JMP gexit
   720                          
   721                          
   722                          
   723                          ;-----------------------------------------------------------------
   724                          
   725                          ; line x left, y down, dx > dy (case 3)
   726                          
   727                          line_down_flat
   728  c320 205ec2                     JSR position		; x,y
   729  c323 a5a3               	LDA cl			; counter adjustment for
   730  c325 f002               	BEQ +			; prepare for dec-dec-counting
   731  c327 e6a4               	INC ch
   732                          +
   733                          loop_xleft_ydown
   734  c329 20ed03                     JSR gchange		; pixel
   735                          
   736  c32c 18                         CLC			; k += dy
   737  c32d a595                       LDA kl
   738  c32f 65a9                       ADC dy
   739  c331 8595                       STA kl
   740  c333 9021                       BCC +			; k < 0
   741  c335 e696                       INC kh
   742  c337 301d               	BMI +			; k < 0
   743                          
   744  c339 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   745  c33b 8595                       STA kl
   746  c33d a596                       LDA kh
   747  c33f e5a7                       SBC dxh		
   748  c341 8596                       STA kh
   749                          
   750  c343 c8                         INY			; y++
   751  c344 c008                       CPY #8
   752  c346 d00e                       BNE +
   753                          	; C=1
   754  c348 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   755  c34a 693f                       ADC #$40-1		; C already set by CPY
   756  c34c 85a5                       STA gaddr
   757  c34e a5a6                       LDA gaddr+1
   758  c350 6901               	ADC #1
   759  c352 85a6                       STA gaddr+1
   760  c354 a000                       LDY #0			; wrap around
   761                          
   762  c356 ca                 +	DEX			; x--
   763  c357 100d                       BPL +++
   764  c359 a207                       LDX #7			; wrap around
   765  c35b 38                 	SEC
   766  c35c a5a5                       LDA gaddr		; x-8: gaddr -= 8
   767  c35e e908                       SBC #8
   768  c360 85a5                       STA gaddr
   769  c362 b002                       BCS +++
   770  c364 c6a6                       DEC gaddr+1
   771                          +++
   772  c366 c6a3               	DEC cl			; c--
   773  c368 d0bf               	BNE loop_xleft_ydown
   774  c36a c6a4               	DEC ch			; adjusted high which allows this
   775  c36c d0bb                       BNE loop_xleft_ydown
   776                          
   777  c36e 4c3fc2                     JMP gexit
   778                          
   779                          
   780                          ;-----------------------------------------------------------------
   781                          
   782                          ; line y down, x right, dx < dy (case 4)
   783                          
   784                          line_down_steep
   785  c371 205ec2                     JSR position		; x,y
   786                          loop_ydown_xleft
   787  c374 20ed03                     JSR gchange		; pixel
   788                          
   789  c377 18                         CLC			; k += dx
   790  c378 a595                       LDA kl
   791  c37a 65ab                       ADC dxl			; dxh is 0, because dx < dy
   792  c37c 8595                       STA kl
   793  c37e 9014                       BCC +			; k >= 0 ->
   794                          
   795  c380 e5a9               	SBC dy			; k -= dy, C=1
   796  c382 8595                       STA kl
   797                          
   798  c384 ca                  	DEX			; x--
   799  c385 100d                       BPL +
   800  c387 a207                       LDX #7			; wrap around
   801  c389 38                 	SEC
   802  c38a a5a5                       LDA gaddr		; x-8: gaddr -= 8
   803  c38c e908                       SBC #8
   804  c38e 85a5                       STA gaddr
   805  c390 b002                       BCS +
   806  c392 c6a6                       DEC gaddr+1
   807                          
   808  c394 c8                 +	INY			; y++
   809  c395 c008                       CPY #8			; y overflow?
   810  c397 d00e                       BNE +++
   811  c399 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   812  c39b 693f                       ADC #$40-1		; C already set by CPY
   813  c39d 85a5                       STA gaddr
   814  c39f a5a6                       LDA gaddr+1
   815  c3a1 6901               	ADC #1
   816  c3a3 85a6                       STA gaddr+1
   817  c3a5 a000                       LDY #0			; wrap around
   818                          
   819  c3a7 c6a3               +++	DEC cl			; c--
   820                          				; until c=0
   821  c3a9 d0c9                       BNE loop_ydown_xleft
   822  c3ab 4c3fc2                     JMP gexit
   823                          
   824                          
   825                          ;-----------------------------------------------------------------
   826                          
   827                          getcommaxy
   828  c3ae 20fdae                     JSR b_getcomma		; check ","
   829                          getxy
   830  c3b1 208aad                     JSR b_getval		; get X coord. value
   831  c3b4 20f7b7                     JSR b_convint
   832  c3b7 c901                       CMP #>xmax
   833  c3b9 900c               	BCC gcxy_xok
   834  c3bb f003                       BEQ ++			; X = $1xx
   835  c3bd 2020c6                     JSR range_error
   836                          
   837  c3c0 c040               ++	CPY #<xmax		; check X low
   838  c3c2 9003                       BCC +
   839  c3c4 2020c6                     JSR range_error
   840                          +
   841                          gcxy_xok
   842  c3c7 84fb                       STY gpos		; temporary save X coord.
   843  c3c9 85fc                       STA gpos+1
   844                          
   845  c3cb 20f1b7                     JSR b_getcomma8bit
   846                          				; get Y coord. value
   847  c3ce e0c8                       CPX #ymax
   848  c3d0 9003                       BCC +
   849  c3d2 2020c6                     JSR range_error
   850                          +
   851  c3d5 a4fb                       LDY gpos		; restory X coord.
   852  c3d7 a5fc                       LDA gpos+1
   853  c3d9 60                         RTS
   854                          
   855                          
   856                          ;-----------------------------------------------------------------
   857                          
   858                          para_hline_box
   859  c3da 20b1c3                     JSR getxy		; get startpoint
   860  c3dd 86aa                       STX y
   861  c3df 8e3e03                     STX savey		; save as cursor, too
   862  c3e2 859c                       STA xh
   863  c3e4 849b                       STY xl
   864  c3e6 8d3d03             	STA savexh
   865  c3e9 8c3c03             	STY savexl
   866  c3ec 20fdae                     JSR b_getcomma		; get length
   867  c3ef 208aad                     JSR b_getval
   868  c3f2 20f7b7                     JSR b_convint
   869                          				; calculate end point
   870  c3f5 aa                         TAX			; save length high byte
   871  c3f6 98                         TYA			; length low byte
   872  c3f7 18                         CLC
   873  c3f8 659b                       ADC xl			; low xend = x+length
   874  c3fa 859e                       STA xendl
   875  c3fc a8                 	TAY
   876  c3fd 8a                         TXA			; high
   877  c3fe 659c                       ADC xh			; high xend = x+length
   878  c400 859f                       STA xendh
   879  c402 aa                 	TAX
   880                          
   881  c403 c901               	CMP #>xmax		; endpoint outside?
   882  c405 9005               	BCC +
   883  c407 d003               	BNE +			; >$200 (512)
   884  c409 98                 	TYA
   885  c40a e940               	SBC #<xmax
   886  c40c 60                 +	RTS			; C=1 out of range, C=0 ok
   887                          
   888                          ;-----------------------------------------------------------------
   889                          
   890                          hline
   891  c40d 20dac3             	JSR para_hline_box
   892  c410 9003               	BCC +
   893  c412 2020c6             	JSR range_error
   894                          				; XXX xend=xmax-1 ?
   895                          +
   896  c415 8e3d03                     STX savexh
   897  c418 8c3c03                     STY savexl		; also save as final cursor
   898                          
   899  c41b a900               	LDA #0			; default thickness 0 (means 1 pixel)
   900  c41d 85a3               	STA ycount
   901  c41f 207900             	JSR chrgot		; last char. again
   902  c422 f019               	BEQ +++			; command end? no optional param.
   903  c424 20f1b7             	JSR b_getcomma8bit
   904  c427 8a                 	TXA			; optional 8-bit parameter
   905  c428 85a3               	STA ycount		; hline thickness
   906  c42a f011               	BEQ +++			; 0 means 1 pixel
   907  c42c 18                 	CLC
   908  c42d 65aa               	ADC y			; end position for y coord.
   909  c42f b004               	BCS +			; > 255
   910  c431 c9c8               	CMP #ymax
   911  c433 9008               	BCC +++
   912                          +				; C=1 from ADC or CMP before
   913  c435 2020c6             	JSR range_error		; corrupts A
   914                          				; XXX ycount=ymax-y-1 ?
   915                          				; xend >= x
   916  c438 b003               	BCS hl_noxswap		; always
   917                          
   918                          hline_start
   919  c43a 2083c2             	JSR swap_x_xend		; xend < x, entry from line
   920                          	
   921                          hl_noxswap
   922                          				; xend > x
   923                          +++
   924  c43d e6a3               	INC ycount		; count to 0
   925  c43f 2047c2                     JSR ginit		; map in graphic memory
   926                          
   927  c442 205ec2                     JSR position		; graphic position x,y
   928                          
   929  c445 a5a5               	LDA gaddr		; save position for vertical
   930  c447 85fb               	STA sgaddr
   931  c449 a5a6               	LDA gaddr+1
   932  c44b 85fc               	STA sgaddr+1
   933  c44d 86ab               	STX xsave
   934  c44f 84a9               	STY ysave
   935                          
   936  c451 a59e                       LDA xendl
   937  c453 2907                       AND #%00000111
   938  c455 8596                       STA tmp2		; xend mod 8, mask index
   939  c457 a59b                       LDA xl
   940  c459 29f8                       AND #%11111000		; (xl div 8)*8
   941  c45b 8595                       STA tmpbits
   942  c45d a59e                       LDA xendl		; xend unmasked
   943  c45f 38                         SEC
   944  c460 e595                       SBC tmpbits		; finally: xend - (x div 8)*8 
   945  c462 8595                       STA tmpbits
   946  c464 a59f                       LDA xendh
   947  c466 e59c                       SBC xh
   948  c468 4a                         LSR			; / 8 ->  0-39
   949  c469 a595                       LDA tmpbits		; only 1 highest bit
   950  c46b 6a                         ROR			; and 3 lower bits
   951  c46c 4a                         LSR
   952  c46d 4a                         LSR
   953                                  			; 8-pixel-blocks count
   954  c46e 85a4               	STA hcount		; save for vertical extension
   955                           
   956                          hl_vertloop
   957  c470 98                 	TYA			; calculate max. Y in 8x8 block
   958  c471 18                 	CLC
   959  c472 65a3               	ADC ycount
   960  c474 c908               	CMP #8
   961  c476 9002               	BCC +
   962  c478 a908               	LDA #8
   963  c47a 85a8               +	STA ylimit
   964                          
   965  c47c bd91c1                     LDA maskleft,X		; starting mask
   966  c47f 8595               	STA tmpbits
   967  c481 a6a4               	LDX hcount		; how many blocks
   968                          
   969                          hl_nextblock
   970  c483 ca                         DEX
   971                          hl_islastblock
   972  c484 301d                       BMI hl_lastblock
   973                          				; leave loop if X<0
   974  c486 a4a9               	LDY ysave
   975  c488 a595               -	LDA tmpbits		; mask
   976  c48a 20f503             	JSR gmask		; first with left end mask
   977  c48d c8                 	INY			; vertical down
   978  c48e c4a8               	CPY ylimit		; in 8x8 box
   979  c490 d0f6               	BNE -
   980                          
   981  c492 18                         CLC			; gaddr += 8 (one block to right)
   982  c493 a5a5                       LDA gaddr
   983  c495 6908                       ADC #8
   984  c497 85a5                       STA gaddr
   985  c499 9002                       BCC +
   986  c49b e6a6                       INC gaddr+1
   987                          
   988  c49d a9ff               +	LDA #$FF		; following with full 8-pixel mask
   989  c49f 8595               	STA tmpbits
   990  c4a1 d0e0               	BNE hl_nextblock	; always
   991                          
   992                          hl_lastblock
   993  c4a3 a696                       LDX tmp2		; xend mask index
   994  c4a5 3d9bc1                     AND maskright,X		; current mask combined with mask right end
   995  c4a8 8595               	STA tmpbits		; mask
   996  c4aa a4a9               	LDY ysave		; start position in 8x8 block
   997  c4ac a595               -	LDA tmpbits		; mask
   998  c4ae 20f503             	JSR gmask		; modify
   999  c4b1 c8                 	INY			; vertical down
  1000  c4b2 c6a3               	DEC ycount		; overall y counter
  1001  c4b4 c4a8               	CPY ylimit
  1002  c4b6 d0f4               	BNE -
  1003                          
  1004  c4b8 a5a3               	LDA ycount		; finished
  1005  c4ba d003               	BNE +			; roll-over into 8x8 block below
  1006  c4bc 4c3fc2                     JMP gexit		; leave
  1007                          
  1008  c4bf 18                 +	CLC
  1009  c4c0 a5fb               	LDA sgaddr
  1010  c4c2 6940               	ADC #$40		; next 8-pixel row below
  1011  c4c4 85fb               	STA sgaddr		; + $140 (320)
  1012  c4c6 85a5               	STA gaddr
  1013  c4c8 a5fc               	LDA sgaddr+1
  1014  c4ca 6901               	ADC #$01
  1015  c4cc 85fc               	STA sgaddr+1
  1016  c4ce 85a6               	STA gaddr+1
  1017  c4d0 a6ab               	LDX xsave		; initial mask index
  1018  c4d2 a000               	LDY #0			; start on top of 8x8
  1019  c4d4 84a9               	STY ysave
  1020  c4d6 f098               	BEQ hl_vertloop
  1021                          ;-----------------------------------------------------------------
  1022                          
  1023                          vline
  1024  c4d8 20b1c3                     JSR getxy		; get startpoint
  1025  c4db 859c                       STA xh
  1026  c4dd 8d3d03                     STA savexh		; save as cursor too
  1027  c4e0 849b                       STY xl
  1028  c4e2 8c3c03                     STY savexl
  1029  c4e5 8693                       STX yend		; initial point is endpoint
  1030                          
  1031  c4e7 20f1b7                     JSR b_getcomma8bit
  1032                          				; get length
  1033  c4ea 18                         CLC			; calculate end point
  1034  c4eb 8a                         TXA			; length
  1035                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1036                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1037                          ;	STA tmpbits
  1038  c4ec 6593                       ADC yend		; length + initial point is startpoint
  1039  c4ee b005               	BCS vline_iq		; > 255
  1040  c4f0 c9c8                       CMP #ymax		; outside?
  1041  c4f2 a8                 	TAY			; keep startpoint
  1042  c4f3 9003                       BCC +
  1043                          vline_iq
  1044  c4f5 2020c6                     JSR range_error		; corrupts A
  1045                          				; XXX Y = ymax-1 ?
  1046  c4f8 84aa               +	STY y			; startpoint
  1047  c4fa 8c3e03             	STY savey		; set cursor y position
  1048  c4fd 18                 	CLC
  1049  c4fe 900e               	BCC +++			; skip following, because y, yend are already ordered
  1050                          
  1051                          vline_start			; entry point from line command (only)
  1052  c500 a5aa               	LDA y			; order of y, yend is not defined
  1053  c502 c593               	CMP yend
  1054  c504 b008               	BCS vl_noyswap		; yend > y ->
  1055  c506 a5aa               	LDA y			; swap y, yend
  1056  c508 a693               	LDX yend
  1057  c50a 8593               	STA yend
  1058  c50c 86aa               	STX y
  1059                          vl_noyswap
  1060                          				; startpoint is below the endpoint
  1061  c50e 2047c2             +++	JSR ginit		; map in graphic memory
  1062                          
  1063                          vl_start
  1064  c511 205ec2                     JSR position		; graphic position x,y
  1065  c514 bd64c1                     LDA bitmask,X
  1066  c517 8596                       STA tmp2		; save mask
  1067                          ; DON'T-CHANGE: replace ...
  1068  c519 38                         SEC
  1069  c51a a5aa                       LDA y			; startpoint is greater!
  1070  c51c e593                       SBC yend		; vertical length
  1071  c51e aa                         TAX
  1072                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmpbits)
  1073                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1074                          ;	LDX tmpbits
  1075  c51f e8                         INX			; +1 (exit on 0)
  1076  c520 38                 	SEC			; for subtraction, never changed!
  1077                          vl_nextline
  1078  c521 a596                       LDA tmp2
  1079  c523 20f503                     JSR gmask		; modify 
  1080  c526 88                         DEY			; go up
  1081  c527 100e                       BPL +
  1082  c529 a5a5                       LDA gaddr		; C=1
  1083  c52b e940               	SBC #$40		; gaddr -= 320
  1084  c52d 85a5                       STA gaddr
  1085  c52f a5a6                       LDA gaddr+1
  1086  c531 e901                       SBC #$01
  1087  c533 85a6                       STA gaddr+1
  1088  c535 a007                       LDY #7			; wrap y offset
  1089  c537 ca                 +	DEX			; all vertical positions done?
  1090  c538 d0e7                       BNE vl_nextline
  1091  c53a 4c3fc2                     JMP gexit		; leave
  1092                          
  1093                          
  1094                          ;-----------------------------------------------------------------
  1095                          
  1096                          line
  1097  c53d 20b1c3                     JSR getxy		; get startpoint
  1098  c540 849b                       STY xl 
  1099  c542 859c                       STA xh
  1100  c544 86aa                       STX y
  1101                          
  1102  c546 20aec3                     JSR getcommaxy		; get endpoint
  1103                          line_start
  1104  c549 8c3c03                     STY savexl		; save as cursor position too
  1105  c54c 849e                       STY xendl
  1106  c54e 8d3d03                     STA savexh
  1107  c551 859f                       STA xendh
  1108  c553 8e3e03                     STX savey
  1109  c556 8693                       STX yend
  1110                          
  1111  c558 a000                       LDY #$00		; initialize to 0
  1112  c55a 84a8                       STY ydir
  1113  c55c 8495                       STY kl
  1114  c55e 8496                       STY kh
  1115                          
  1116  c560 38                         SEC
  1117  c561 a59b                       LDA xl			; calculate dx
  1118  c563 e59e                       SBC xendl
  1119  c565 85ab                       STA dxl
  1120  c567 a59c                       LDA xh
  1121  c569 e59f                       SBC xendh
  1122  c56b 85a7                       STA dxh
  1123                          
  1124  c56d b018                       BCS li_xend_left
  1125                          	; dx != 0
  1126                          				; negate dx:
  1127  c56f 98                         TYA			; Y=A=0
  1128  c570 38                         SEC			; dx = 0 - dx
  1129  c571 e5ab                       SBC dxl
  1130  c573 85ab                       STA dxl
  1131  c575 98                         TYA			; Y=A=0
  1132  c576 e5a7                       SBC dxh
  1133  c578 85a7                       STA dxh
  1134                          				; C=0 always, needed later
  1135  c57a 2083c2             	jsr swap_x_xend
  1136  c57d a6aa                       LDX y			; swap y
  1137  c57f a493                       LDY yend
  1138  c581 8693                       STX yend
  1139  c583 84aa                       STY y
  1140                          
  1141  c585 9007                       BCC li_x_different
  1142                          				; C=0 always (from negation before)
  1143                          
  1144                          li_xend_left
  1145                                  			; A already contains dxh
  1146  c587 05ab                       ORA dxl			; dx = 0?
  1147  c589 d003                       BNE li_x_different
  1148  c58b 4c00c5                     JMP vline_start		; vertical line case
  1149                          
  1150                          li_x_different
  1151  c58e 38                         SEC			; calculate dy
  1152  c58f a593                       LDA yend
  1153  c591 e5aa                       SBC y
  1154  c593 b006                       BCS li_y_right		; yend >= y?
  1155  c595 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1156  c597 6901                       ADC #$01		; C=0
  1157  c599 85a8                       STA ydir		; always not 0: flag y goes up
  1158                          
  1159                          li_y_right
  1160  c59b 85a9                       STA dy
  1161  c59d d007                       BNE +
  1162  c59f a900               	LDA #0			; line thickness = 1
  1163  c5a1 85a3               	STA ycount
  1164  c5a3 4c3ac4                     JMP hline_start		; horizontal line case
  1165                          +
  1166                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1167                          
  1168  c5a6 a5a7                       LDA dxh			; dx > dy
  1169  c5a8 d01c                       BNE line_flat		; yes -> flat
  1170  c5aa a5a9                       LDA dy			; no -> steep
  1171  c5ac aa                         TAX
  1172  c5ad c5ab                       CMP dxl
  1173  c5af 9015                       BCC line_flat
  1174                          
  1175                          line_steep
  1176  c5b1 e8                         INX	
  1177  c5b2 86a3                       STX cl			; c = dy+1
  1178  c5b4 4a                         LSR			; dy/2
  1179  c5b5 49ff               	EOR #$FF		; one's complement
  1180  c5b7 8595                       STA kl			; k = -dy/2 -1
  1181                          
  1182  c5b9 2047c2                     JSR ginit		; map in graphic memory
  1183                          
  1184  c5bc a5a8                       LDA ydir
  1185  c5be d003                       BNE +
  1186  c5c0 4c71c3                     JMP line_down_steep	; y down, steep
  1187  c5c3 4c94c2             +	JMP line_up_steep	; y up, steep
  1188                          
  1189                          line_flat
  1190  c5c6 a5a7                       LDA dxh
  1191  c5c8 a8                         TAY
  1192  c5c9 a6ab                       LDX dxl
  1193  c5cb e8                         INX
  1194  c5cc d001                       BNE +
  1195  c5ce c8                         INY
  1196  c5cf 86a3               +	STX cl			; c = dx+1
  1197  c5d1 84a4                       STY ch
  1198                          
  1199  c5d3 4a                         LSR			; dx/2 high
  1200  c5d4 49ff               	EOR #$FF		; one's complement
  1201  c5d6 8596                       STA kh
  1202  c5d8 a5ab                       LDA dxl
  1203  c5da 6a                         ROR			; dx/2 low
  1204  c5db 49ff               	EOR #$FF		; one's complement
  1205  c5dd 8595                       STA kl			; k = -dx/2 - 1
  1206                          
  1207  c5df 2047c2                     JSR ginit		; map in graphic memory
  1208                          
  1209  c5e2 a5a8                       LDA ydir	
  1210  c5e4 d003                       BNE +
  1211  c5e6 4c20c3                     JMP line_down_flat	; y down, flat
  1212  c5e9 4cd0c2             +	JMP line_up_flat	; y up, flat
  1213                          
  1214                          ;-----------------------------------------------------------------
  1215                          
  1216                          plot
  1217  c5ec 20b1c3                     JSR getxy		; get parameter
  1218  c5ef 859c                       STA xh			; save x/y
  1219  c5f1 849b                       STY xl
  1220  c5f3 86aa                       STX y
  1221  c5f5 8d3d03                     STA savexh		; and store as cursor
  1222  c5f8 8c3c03                     STY savexl
  1223  c5fb 8e3e03                     STX savey
  1224                          
  1225                          plot_start
  1226  c5fe 205ec2                     JSR position		; calculate graphical address
  1227                          
  1228  c601 a501                       LDA prozport
  1229  c603 29fd                       AND #%11111101		; Kernal ROM disable
  1230  c605 78                         SEI			
  1231  c606 8501                       STA prozport
  1232                          
  1233  c608 20ed03                     JSR gchange		; change graphical data
  1234                          
  1235  c60b a501                       LDA prozport
  1236  c60d 0902                       ORA #%00000010		; kernal ROM enable
  1237  c60f 8501                       STA prozport
  1238  c611 58                         CLI
  1239  c612 60                         RTS
  1240                          
  1241                          ;-----------------------------------------------------------------
  1242                          
  1243                          move
  1244  c613 20b1c3                     JSR getxy		; get parameter
  1245  c616 8d3d03                     STA savexh		; just save as cursor
  1246  c619 8c3c03                     STY savexl
  1247  c61c 8e3e03                     STX savey
  1248  c61f 60                         RTS
  1249                          
  1250                          
  1251                          ;-----------------------------------------------------------------
  1252                          
  1253                          ; never touches X, Y, C-flag
  1254                          ; on exit: A corrupted, Z=0
  1255                          
  1256                          range_error
  1257  c620 ad3f03             	LDA savemo
  1258  c623 29f0               	AND #$F0
  1259  c625 d003               	BNE +
  1260                          				; error mode 3: abort command (silent)
  1261  c627 68                 	PLA			; cleanup JSR
  1262  c628 68                 	PLA			; highbyte of return address >0
  1263                          
  1264  c629 60                 -	RTS			; error mode 5: back to command
  1265                          				; to handle value correction
  1266                          				; Z=0
  1267  c62a 2920               +	AND #$20		; mode 5?
  1268  c62c d0fb               	BNE -			; exit with Z=0
  1269  c62e 68                 	PLA			; error mode 4: terminate with error
  1270  c62f 68                 	PLA			; cleanup JSR
  1271                          setmode_error
  1272  c630 4c48b2             	JMP b_illquant		; throw error message
  1273                          
  1274                          ;-----------------------------------------------------------------
  1275                          
  1276                          setmode
  1277  c633 209eb7                     JSR b_get8bit
  1278  c636 e003                       CPX #3
  1279  c638 9017                       BCC +			; less then 3, modification mode
  1280  c63a e006               	CPX #6
  1281  c63c b0f2               	BCS setmode_error	; out of range
  1282                          				; error mode
  1283  c63e 8a                 	TXA
  1284  c63f e902               	SBC #2			; C=0, therefore -3
  1285  c641 0a                 	ASL			; 0-2 -> 16,32 or 48
  1286  c642 0a                 	ASL			; shift to upper nibble
  1287  c643 0a                 	ASL
  1288  c644 0a                 	ASL
  1289                          				; put A's bit 4-7 into savemo
  1290  c645 4d3f03             	EOR savemo		; ********
  1291  c648 29f0               	AND #%11110000		; ****0000
  1292  c64a 4d3f03             	EOR savemo		; AAAAmmmm
  1293  c64d 8d3f03             	STA savemo		; 
  1294  c650 60                 	RTS
  1295                          
  1296  c651 8a                 +	TXA
  1297  c652 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1298  c655 290f               	AND #%00001111
  1299  c657 4d3f03             	EOR savemo
  1300  c65a 8d3f03             	STA savemo
  1301                          setmode_enter
  1302  c65d e001               	CPX #$01
  1303  c65f b01a                       BCS set_or_toggle
  1304                          
  1305                          modereset
  1306  c661 a9c1                       LDA #>(nbitmask)
  1307  c663 8df103                     STA gchange_op+2
  1308  c666 a96c                       LDA #<(nbitmask)
  1309  c668 8df003                     STA gchange_op+1
  1310  c66b a93d                       LDA #$3D		; opcode AND abs,X
  1311  c66d 8def03                     STA gchange_op
  1312  c670 a931                       LDA #$31		; opcode AND (zp),Y
  1313  c672 8df703                     STA gmask_op
  1314  c675 a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1315  c677 8df603                     STA gmask_flip+1
  1316  c67a 60                         RTS
  1317                          
  1318                          set_or_toggle
  1319  c67b d01a                       BNE modetoggle
  1320                          modeset
  1321  c67d a9c1                       LDA #>(bitmask)
  1322  c67f 8df103                     STA gchange_op+2
  1323  c682 a964                       LDA #<(bitmask)
  1324  c684 8df003                     STA gchange_op+1
  1325  c687 a91d                       LDA #$1D		; opcode OR abs,X
  1326  c689 8def03                     STA gchange_op
  1327  c68c a911                       LDA #$11		; opcode OR (zp),Y
  1328  c68e 8df703                     STA gmask_op
  1329  c691 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1330  c693 8df603                     STA gmask_flip+1
  1331  c696 60                         RTS
  1332                          
  1333                          modetoggle
  1334  c697 a9c1                       LDA #>(bitmask)
  1335  c699 8df103                     STA gchange_op+2
  1336  c69c a964                       LDA #<(bitmask)
  1337  c69e 8df003                     STA gchange_op+1
  1338  c6a1 a95d                       LDA #$5D		; opcode EOR abs,X
  1339  c6a3 8def03                     STA gchange_op
  1340  c6a6 a951                       LDA #$51		; opcode EOR (zp),Y
  1341  c6a8 8df703                     STA gmask_op
  1342  c6ab a900                       LDA #$00		; mask, EOR #$00, not inverting
  1343  c6ad 8df603                     STA gmask_flip+1
  1344  c6b0 60                         RTS
  1345                          
  1346                          
  1347                          ;-----------------------------------------------------------------
  1348                          ; get current x cursor position
  1349                          
  1350                          getposx
  1351  c6b1 ac3c03             	LDY savexl
  1352  c6b4 ad3d03             	LDA savexh
  1353  c6b7 2091b3             	JSR b_word2fac
  1354  c6ba 4c7300             	JMP chrget		; last position of expression (function name)
  1355                          
  1356                          ;-----------------------------------------------------------------
  1357                          ; get current y cursor position
  1358                          
  1359                          getposy
  1360  c6bd ac3e03             	LDY savey
  1361  c6c0 20a2b3             	JSR b_byte2fac
  1362  c6c3 4c7300             	JMP chrget		; last position of expression (function name)
  1363                          
  1364                          ;-----------------------------------------------------------------
  1365                          
  1366                          ; get pixel (check if pixel set)
  1367                          ; not used
  1368                          
  1369                          get
  1370  c6c6 207300             	JSR chrget		; advance past function name
  1371  c6c9 20faae             	JSR b_chkparl		; "("?
  1372  c6cc 20b1c3                     JSR getxy		; get X,Y values
  1373  c6cf 859c                       STA xh
  1374  c6d1 849b                       STY xl
  1375  c6d3 86aa                       STX y
  1376  c6d5 207900             	JSR chrgot
  1377  c6d8 20f7ae             	JSR b_chkparr		; ")"?
  1378                          	
  1379                          
  1380  c6db 205ec2                     JSR position		; calculate graphic address/position
  1381                          
  1382  c6de a501                       LDA prozport
  1383  c6e0 29fd               	AND #%11111101		; Kernal ROM disable
  1384  c6e2 78                         SEI
  1385  c6e3 8501                       STA prozport
  1386                          
  1387  c6e5 b1a5                       LDA (gaddr),Y
  1388  c6e7 3d64c1                     AND bitmask,X		; mask position
  1389  c6ea a8                         TAY
  1390  c6eb a501                       LDA prozport
  1391  c6ed 0902               	ORA #%00000010		; kernal ROM enable
  1392  c6ef 8501                       STA prozport
  1393  c6f1 58                         CLI
  1394  c6f2 98                 	TYA
  1395  c6f3 f002               	BEQ +
  1396  c6f5 a001               	LDY #1			; <> 0 -> always return 1
  1397  c6f7 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1398                          
  1399                          ;-----------------------------------------------------------------
  1400                          
  1401                          relto_cont
  1402                          				; continue
  1403  c6fa 207300             	JSR chrget		; skip TO token
  1404                          relto
  1405  c6fd 208aad                     JSR b_getval		; get X offset (+/-)
  1406  c700 a561               	LDA facexp		; FAC exponent
  1407  c702 c990               	CMP #$90		; more than 16 bit
  1408  c704 b031               	BCS relto_error		; illegal quantity
  1409  c706 209bbc                     JSR b_fac2int		; to signed integer
  1410                          
  1411  c709 18                         CLC
  1412  c70a a565                       LDA facintl
  1413  c70c 6d3c03                     ADC savexl
  1414  c70f 859e                       STA xendl
  1415  c711 a564                       LDA facinth
  1416  c713 6d3d03                     ADC savexh
  1417  c716 859f                       STA xendh		; xend = savex+facint
  1418                          
  1419  c718 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1420  c71b 208aad                     JSR b_getval
  1421  c71e a561                       LDA facexp		; FAC exponent
  1422  c720 c990                       CMP #$90		; more than 16 bit
  1423  c722 b013                       BCS relto_error		; illegal quantity
  1424  c724 209bbc                     JSR b_fac2int		; to signed integer
  1425  c727 18                         CLC
  1426  c728 a565                       LDA facintl
  1427  c72a 6d3e03                     ADC savey
  1428  c72d 8593                       STA yend		; yend = savey+facint
  1429                          
  1430  c72f a59f                       LDA xendh		; check end coord. x
  1431  c731 c901                       CMP #>xmax
  1432  c733 900e                       BCC rt_xok
  1433  c735 f003                       BEQ +
  1434                          relto_error
  1435  c737 2020c6                     JSR range_error
  1436  c73a a59e               +	LDA xendl
  1437  c73c c940                       CMP #<xmax
  1438  c73e 9003                       BCC +
  1439  c740 2020c6                     JSR range_error
  1440                          +
  1441                          rt_xok
  1442  c743 a593                       LDA yend		; check end coord. y
  1443  c745 c9c8                       CMP #ymax
  1444  c747 9003                       BCC +
  1445  c749 2020c6                     JSR range_error
  1446                          +
  1447  c74c ad3c03                     LDA savexl
  1448  c74f 859b                       STA xl
  1449  c751 ad3d03                     LDA savexh
  1450  c754 859c                       STA xh
  1451  c756 ad3e03                     LDA savey
  1452  c759 85aa                       STA y
  1453  c75b a49e                       LDY xendl
  1454  c75d a59f                       LDA xendh
  1455  c75f a693                       LDX yend		; xend/yend = cursor + x/y
  1456                          
  1457  c761 2049c5                     JSR line_start		; draw line x/y to xend/yend
  1458                          
  1459  c764 207900             	JSR chrgot
  1460  c767 d001               	BNE +
  1461  c769 60                 	RTS
  1462  c76a c9a4               +	CMP #t_to		; TO keyword?
  1463  c76c f08c               	BEQ relto_cont
  1464  c76e 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1465                          
  1466                          ;-----------------------------------------------------------------
  1467                          
  1468                          char
  1469  c771 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1470  c774 e028                       CPX #40	
  1471  c776 9003                       BCC +
  1472                          char_error
  1473  c778 4c48b2                     JMP b_illquant
  1474  c77b 86fb               +	STX gpos		; save x coord.
  1475  c77d 20f1b7                     JSR b_getcomma8bit
  1476                          				; get char. position y 0-24
  1477  c780 e019                       CPX #25
  1478  c782 b0f4                       BCS char_error
  1479  c784 86fc                       STX gpos+1		; save y coord.
  1480                          
  1481  c786 20fdae                     JSR b_getcomma		; get string
  1482  c789 209ead                     JSR b_getexpr
  1483  c78c 20a3b6                     JSR b_stringval		 ; string address in str
  1484  c78f 48                         PHA			; string length
  1485  c790 a6fc                       LDX gpos+1		; y coord. for char. position
  1486  c792 8a                         TXA
  1487  c793 2903                       AND #$03		; mask 2 bits
  1488  c795 a8                         TAY			; table index
  1489  c796 a900                       LDA #$00
  1490  c798 85fc                       STA gpos+1		; x high
  1491  c79a a5fb                       LDA gpos		; saved x: multiply by 8
  1492  c79c 0a                         ASL
  1493  c79d 0a                         ASL
  1494  c79e 0a                         ASL
  1495  c79f 26fc                       ROL gpos+1		; overflow to high byte
  1496  c7a1 7974c1                     ADC ytabl,Y
  1497  c7a4 85a5                       STA gaddr
  1498  c7a6 a5fc                       LDA gpos+1		; x high
  1499  c7a8 7d78c1                     ADC ytabh,X
  1500  c7ab 85a6                       STA gaddr+1
  1501  c7ad 68                         PLA			; string length
  1502  c7ae a000                       LDY #$00		; string index
  1503  c7b0 aa                         TAX			; length
  1504  c7b1 e8                         INX			; prepare as counter
  1505                          char_loop
  1506  c7b2 ca                         DEX
  1507  c7b3 f008                       BEQ char_exit
  1508  c7b5 b122                       LDA (str),Y		; read string
  1509  c7b7 20bec7                     JSR char_display
  1510  c7ba c8                         INY
  1511  c7bb d0f5                       BNE char_loop
  1512                          char_exit
  1513  c7bd 60                         RTS
  1514                          
  1515                          char_display
  1516  c7be 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1517  c7c0 8a                         TXA			; save register X+Y
  1518  c7c1 48                         PHA
  1519  c7c2 98                         TYA
  1520  c7c3 48                         PHA
  1521  c7c4 a5d7                       LDA z_tmp		; get saved character
  1522  c7c6 3012                       BMI char_inverse
  1523                          
  1524                          char_normal
  1525  c7c8 c920                       CMP #$20		; control character?
  1526  c7ca 9054                       BCC char_disp_leave
  1527  c7cc c960                       CMP #$60
  1528  c7ce 9004                       BCC +
  1529  c7d0 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1530  c7d2 d014                       BNE char_hires
  1531  c7d4 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1532  c7d6 d010               	BNE char_hires
  1533  c7d8 f00e               	BEQ char_hires
  1534                          
  1535                          char_inverse
  1536  c7da 297f                       AND #%01111111		; mask bit 7
  1537  c7dc c97f                       CMP #%01111111		; was 255? (pi)
  1538  c7de d002                       BNE +
  1539  c7e0 a95e                       LDA #$5E		; screen code for pi
  1540  c7e2 c920               +	CMP #$20		; control character?
  1541  c7e4 903a                       BCC char_disp_leave
  1542                          				; yes, skip
  1543  c7e6 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1544                          				; $C0-$FF -> $40-$7F
  1545                          				; OPT: BNE char_hires
  1546                          				; OPT: char_normal
  1547                          char_hires
  1548  c7e8 a6c7                       LDX z_reverseflag
  1549  c7ea f002                       BEQ +
  1550  c7ec 0980                       ORA #%10000000		; invert char.
  1551  c7ee aa                 +	TAX			; save char. for later
  1552  c7ef a501                       LDA prozport		; save prozport state
  1553  c7f1 48                 	PHA
  1554  c7f2 a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1555  c7f4 78                         SEI
  1556  c7f5 8501                       STA prozport		; char. rom base = $D000
  1557  c7f7 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1558  c7f9 85fc                       STA gpos+1		; 
  1559  c7fb 8a                         TXA			; char. code
  1560  c7fc 0a                         ASL			; *8
  1561  c7fd 26fc                       ROL gpos+1
  1562  c7ff 0a                         ASL
  1563  c800 26fc                       ROL gpos+1
  1564  c802 0a                         ASL
  1565  c803 26fc                       ROL gpos+1
  1566  c805 85fb                       STA gpos		; addr. in char. rom for char.
  1567                          
  1568  c807 a007                       LDY #$07		; 8 hires lines
  1569                          char_line
  1570  c809 b1fb                       LDA (gpos),Y		; read character line
  1571  c80b 20f503                     JSR gmask		; write to hires screen
  1572  c80e 88                         DEY
  1573  c80f 10f8                       BPL char_line
  1574                          
  1575  c811 68                 	PLA
  1576  c812 8501                       STA prozport
  1577  c814 58                         CLI
  1578                          
  1579  c815 18                         CLC			; step char position to left
  1580  c816 a5a5                       LDA gaddr		; ( +8 )
  1581  c818 6908                       ADC #$08
  1582  c81a 85a5                       STA gaddr
  1583  c81c 9002                       BCC +
  1584  c81e e6a6                       INC gaddr+1
  1585                          +
  1586                          char_disp_leave
  1587  c820 68                 	PLA			; pass written character back
  1588  c821 a8                         TAY			; restore saved registers
  1589  c822 68                         PLA
  1590  c823 aa                         TAX
  1591  c824 60                 -       RTS
  1592                          
  1593                          
  1594                          ;-----------------------------------------------------------------
  1595                          
  1596                          to_cont
  1597                          				; continue
  1598  c825 207300             	JSR chrget		; skip TO token
  1599                          to
  1600  c828 ad3c03                     LDA savexl
  1601  c82b 859b                       STA xl
  1602  c82d ad3d03                     LDA savexh
  1603  c830 859c                       STA xh
  1604  c832 ad3e03                     LDA savey
  1605  c835 85aa                       STA y
  1606  c837 20b1c3                     JSR getxy
  1607  c83a 2049c5                     JSR line_start
  1608  c83d 207900             	JSR chrgot
  1609  c840 f0e2               	BEQ -
  1610  c842 c9a4               	CMP #t_to		; TO keyword?
  1611  c844 f0df               	BEQ to_cont
  1612  c846 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1613                          
  1614                          ;-----------------------------------------------------------------
  1615                          
  1616                          box
  1617  c849 20dac3                     JSR para_hline_box
  1618  c84c 9003               	BCC +
  1619  c84e 2020c6             	JSR range_error
  1620                          				; XXX xend=xmax-1 ?
  1621                          +
  1622  c851 20f1b7             	JSR b_getcomma8bit
  1623  c854 8a                 	TXA			; optional 8-bit parameter
  1624                          				; height
  1625  c855 f00c               	BEQ +++			; 0 means 1, box is just a line
  1626  c857 18                 	CLC
  1627  c858 65aa               	ADC y			; end position for y coord.
  1628  c85a b004               	BCS +			; > 255
  1629  c85c c9c8               	CMP #ymax
  1630  c85e 9003               	BCC +++
  1631                          +				; C=1 from ADC or CMP before
  1632  c860 2020c6             	JSR range_error		; corrupts A
  1633                          				; XXX ycount=ymax-y-1 ?
  1634                          				; xend >= x
  1635  c863 48                 +++	PHA			; yend
  1636  c864 a900               	LDA #0
  1637  c866 85a3               	STA ycount		; line thickness 1
  1638  c868 203dc4             	JSR hl_noxswap		; upper horizontal line
  1639                          
  1640                          				; right vertical line
  1641  c86b 68                 	PLA			; if 0, heigth is 1
  1642  c86c d001               	BNE +			; no 
  1643  c86e 60                 	RTS			; exit, if box is degenerated (line)
  1644  c86f a6aa               +	LDX y			; start point at higher values
  1645  c871 85aa               	STA y
  1646  c873 8693               	STX yend
  1647  c875 a59e               	LDA xendl
  1648  c877 859b               	STA xl
  1649  c879 a59f               	LDA xendh
  1650  c87b 859c               	STA xh
  1651  c87d 200ec5             	JSR vl_noyswap		; xend,yend -> xend,y
  1652                          				; lower horizontal line
  1653  c880 ad3c03             	LDA savexl
  1654  c883 859b               	STA xl
  1655  c885 ad3d03             	LDA savexh
  1656  c888 859c               	STA xh			; xend already set
  1657  c88a 203dc4             	JSR hl_noxswap		; x,yend -> xend,yend
  1658                          				; left vertical line
  1659  c88d 4c0ec5             	JMP vl_noyswap		; x,y -> x,xend
  1660                          
  1661                          ;-----------------------------------------------------------------
  1662                          
  1663                          fill
  1664  c890 20b1c3             	JSR getxy
  1665  c893 859c               	STA xh			; save x/y
  1666  c895 849b               	STY xl
  1667  c897 86aa               	STX y
  1668  c899 8d3d03             	STA savexh		; and store as cursor
  1669  c89c 8c3c03             	STY savexl
  1670  c89f 8e3e03             	STX savey
  1671                                  
  1672  c8a2 a531                       LDA basaryend		; initialize fill stack pointer
  1673  c8a4 38                 	SEC
  1674  c8a5 e904               	SBC #fesize		; one element below
  1675  c8a7 85fd               	STA fstack		; use space between basic arrays
  1676  c8a9 a532               	LDA basaryend+1		; and string heap bottom
  1677  c8ab e900               	SBC #0			; take borrow
  1678  c8ad 85fe               	STA fstack+1
  1679                          
  1680  c8af 205ec2             	JSR position		; graphic position in (gaddr)+Y, bit X
  1681  c8b2 bd64c1             	LDA bitmask,X		; start pixel
  1682  c8b5 85a3               	STA tmpmask		; initial single pixel mask
  1683                          
  1684  c8b7 a59c               	LDA xh			; setup 8x8 block index (x8)
  1685  c8b9 4a                 	LSR			; high bit into C
  1686  c8ba a59b               	LDA xl
  1687  c8bc 6a                 	ROR			; take high bit
  1688  c8bd 4a                 	LSR
  1689  c8be 4a                 	LSR			; finally divide by 8
  1690  c8bf 85a7               	STA x8			; = index of 8x8 block in bitmap
  1691                          
  1692  c8c1 2047c2             	JSR ginit		; map in bitmap memory
  1693                          
  1694                          	; set fmode (from mode)
  1695  c8c4 ad3f03             	LDA savemo
  1696  c8c7 2901               	AND #1			; mode = 0 -> invertmask: $FF
  1697  c8c9 38                 	SEC			; mode = 1 -> invertmask: $00
  1698  c8ca e901               	SBC #1			; mode = 2 -> same as mode=0
  1699  c8cc 85a8               	STA fmode		; mode set or reset
  1700                          
  1701                                  ; test start pixel
  1702  c8ce 51a5                       EOR (gaddr),Y           ; bitmap according to mode
  1703  c8d0 8595                       STA tmpbits             ; mask bits
  1704  c8d2 24a3                       BIT tmpmask             ; check single bit, and preserve A
  1705  c8d4 f00f                       BEQ +			; not set, bit position already in X
  1706  c8d6 4c3fc2                     JMP gexit		; set, we are finished early
  1707                          
  1708                          f_line				; start fill in the mid of a line ...
  1709                          
  1710                          	; Get the index of the first leftmost unset pixel inside tmpmask.
  1711                          	; Just the single leftmost gap is filled, others are processed later
  1712                          	; from the element left on the stack.
  1713                          	; Normally comming from process_stack.
  1714                          
  1715                          	; set bits outside mask to 1
  1716  c8d9 a5a3               	LDA tmpmask		; 00011100
  1717  c8db 49ff               	EOR #$ff		; 11100011
  1718  c8dd 0595               	ORA tmpbits		; 00101010 merge with graphic pixel data
  1719                          				; 11101011 pixel outside tmpmask now set! 
  1720  c8df a2ff               	LDX #$ff		; pixel gap search: first one from left
  1721  c8e1 e8                 -	INX
  1722  c8e2 0a                 	ASL			; counting from left
  1723  c8e3 b0fc               	BCS -			; loop if pixel is set
  1724                          				; bit number of the leftmost unset pixel in X
  1725                          
  1726                          	; in: pixels in tmpbits, bit index in X
  1727  c8e5 a900               +	LDA #0			; initialize continuation flag
  1728  c8e7 8596               	STA fcont		; for line above und below
  1729  c8e9 a595               	LDA tmpbits		; 01000010 graphic pixel data
  1730  c8eb 3d9bc1             	AND maskright,X		; 1111X000 clear right from starting point
  1731  c8ee d01b               	BNE left_border		; 01000000 left border remains if any
  1732                          	; open to left, continue left
  1733  c8f0 a9ff               	LDA #$ff		; no left border, next block to left
  1734  c8f2 85a3               	STA tmpmask		; initial mask full pixel line
  1735                          stepleft8
  1736  c8f4 a6a7               	LDX x8 			; 8x8 block position
  1737  c8f6 f019               	BEQ left_end		; hit left screen border, X=0 -> tmpmask=$FF!
  1738  c8f8 c6a7               	DEC x8			; count step 8x8 block to left
  1739                          
  1740  c8fa 38                 	SEC 			; graphic address to to next pixel line/block
  1741  c8fb a5a5               	LDA gaddr
  1742  c8fd e908               	SBC #8			; graphic address -8 -> next block left
  1743  c8ff b002               	BCS +
  1744  c901 c6a6               	DEC gaddr+1		; carry to high byte 
  1745  c903 85a5               +	STA gaddr
  1746                          
  1747                          	; y left unchanged
  1748  c905 b1a5               	LDA (gaddr),Y		; real graphic pixel data from bitmap
  1749  c907 45a8               	EOR fmode		; set/reset mode
  1750  c909 f0e9               	BEQ stepleft8		; step block left if empty (no border)
  1751                          
  1752                          left_border
  1753                          
  1754                          	; Find first set bit from start to left (border)
  1755                          	; Get the pixel position of the first set pixel from the right.
  1756                          
  1757                          	; bit 76543210  -> index 12345678
  1758                          	; input    index   maskleft0
  1759                          	; 00000000 -> 0 -> $FF 
  1760                          	; 10000000 -> 1 -> $7F
  1761                          	; X1000000 -> 2 -> $3F
  1762                          	; XX100000 -> 3 -> $1F
  1763                          	; XXX10000 -> 4 -> $0F
  1764                          	; XXXX1000 -> 5 -> $07
  1765                          	; XXXXX100 -> 6 -> $03
  1766                          	; XXXXXX10 -> 7 -> $01
  1767                          	; XXXXXXX1 -> 8 -> $00
  1768                          	; Speed consideration: for results from X 0 to 4 it is faster than
  1769                          	; a table-driven approach.
  1770                          	; A is never 0!
  1771  c90b a200               	LDX #0
  1772  c90d e8                 -	INX
  1773  c90e 0a                 	ASL			; shift to left
  1774  c90f d0fc               	BNE -			; until byte is empty
  1775                          
  1776                          left_end
  1777  c911 bd91c1             	LDA maskleft0,X		; get a mask from the left border to right
  1778                          				; 00X11111
  1779  c914 d014               	BNE right_start		; start to walk and fill towards the right border
  1780                          				; empty mask immediate continue to right
  1781                          stepright8
  1782  c916 e6a7               	INC x8			; step right a block
  1783  c918 a5a7               	LDA x8
  1784  c91a c928               	CMP #40			; beyond last horizontal block?
  1785  c91c b078               	BCS process_stack	; done if right screen border
  1786                          	; C = 0
  1787  c91e a5a5               	LDA gaddr		; advance to block right
  1788  c920 6908               	ADC #8			; gaddr = gaddr + 8
  1789  c922 9002               	BCC +
  1790  c924 e6a6               	INC gaddr+1		; carry to high byte
  1791  c926 85a5               +	STA gaddr
  1792  c928 a9ff               	LDA #$ff		; force "all pixels" mask, because intial
  1793                          				; mask might be a partial one
  1794                          right_start
  1795  c92a 85a3               	STA tmpmask		; 00111111 store all/distinct mask
  1796  c92c b1a5               	LDA (gaddr),Y		; 01000010 pixel data
  1797  c92e 45a8               	EOR fmode		; set/reset mode
  1798  c930 25a3               	AND tmpmask		; 00000010 mask out left border and beyond
  1799  c932 f00f               	BEQ fill_to_right	; empty -> finally start to fill
  1800                          
  1801                          	; Get the pixel position of the first set pixel from the left, to
  1802                          	; find the right border:
  1803                          
  1804                          	; bit 76543210  -> index 01234567  -> index
  1805                          	; input    index   maskright0
  1806                          	; 00000000 -> 8 -> $FF
  1807                          	; 00000001 -> 7 -> $FE
  1808                          	; 0000001X -> 6 -> $FC
  1809                          	; 000001XX -> 5 -> $F8
  1810                          	; 00001XXX -> 4 -> $F0
  1811                          	; 0001XXXX -> 3 -> $E0
  1812                          	; 001XXXXX -> 2 -> $C0
  1813                          	; 01XXXXXX -> 1 -> $80
  1814                          	; 1XXXXXXX -> 0 -> $00
  1815                          	; Speed consideration: for results of X from 4 to 8 it is faster than
  1816                          	; a table-driven approach.
  1817                          	; A is never 0!
  1818  c934 a208               	LDX #8
  1819  c936 ca                 -	DEX
  1820  c937 4a                 	LSR			; shift to right
  1821  c938 d0fc               	BNE -			; until byte is empty
  1822                          
  1823                          	; search right border
  1824  c93a bd9ac1             	LDA maskright0,X	; 11111X00 mask out the right part
  1825  c93d 25a3               	AND tmpmask		; 00111111 intersect with mask from left
  1826                          				; 00111100
  1827  c93f f055               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1828                          				; leading to 0 mask (fill_check wont't
  1829                          				; handle this special case)
  1830  c941 85a3               	STA tmpmask		; 00111100 save intersected masks from left and right
  1831                          				; continue to fill to right ...
  1832                          fill_to_right			; fill loop towards right border
  1833  c943 a5a3               	LDA tmpmask		; fill mask
  1834                          				; assert:    (bitmap & tempmask) == 0
  1835                          				;         || (bitmap & tempmask) == tempmask
  1836  c945 51a5               	EOR (gaddr),Y		; set/reset to fill
  1837  c947 91a5               	STA (gaddr),Y		; into bitmap - the actual fill action!
  1838                          	
  1839                          check_above
  1840  c949 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1841                          				; c = 0!
  1842  c94b 84a9               	STY ysave		; to be restored later
  1843  c94d a5a5               	LDA gaddr		; current graphic position
  1844  c94f 85fb               	STA caddr		; check position
  1845  c951 a6a6               	LDX gaddr+1
  1846  c953 88                 	DEY			; line above
  1847  c954 100e               	BPL +			; leaving 8x8 block?
  1848                          	; c=0 (asl fcont)
  1849  c956 e93f               	SBC #$40-1		; block above:
  1850  c958 85fb               	STA caddr		; caddr = gaddr - $140
  1851                          
  1852  c95a ca                 	DEX			; subtract high byte
  1853  c95b b001               	BCS ++			; borrow from low byte
  1854  c95d ca                 	DEX			; subtract borrow
  1855  c95e e0e0               ++	CPX #>gram		; still graphic ram?
  1856                          
  1857  c960 9007               	BCC skip_above
  1858  c962 a007               	LDY #7			; last line in block in new block
  1859  c964 86fc               +	STX caddr+1		; shared store
  1860  c966 20e1c9             	JSR fill_check
  1861                          skip_above
  1862                          
  1863                          check_below
  1864  c969 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1865  c96b a5a5               	LDA gaddr		; current graphic position
  1866  c96d 85fb               	STA caddr		; check position
  1867  c96f a6a6               	LDX gaddr+1
  1868  c971 a4a9               	LDY ysave		; restore original y position
  1869  c973 c8                 	INY			; line below
  1870  c974 c008               	CPY #8			; crossing 8x8 block?
  1871  c976 9013               	BCC +			; less then 8
  1872                          	; c=1 (cpy)
  1873  c978 693f               	ADC #$40-1		; block below: accu has gaddr
  1874  c97a 85fb               	STA caddr		; caddr = gaddr + $140
  1875                          
  1876                          ;	INX
  1877                          ;	BCC ++
  1878                          ;	INX			; add carry
  1879                          ;++	BEQ skip_below		; skip to $100xx, out of range
  1880                          ;	CPX #>(gram+8000)
  1881                          ;	BCC +++			; below GRAM end
  1882                          ;	BNE skip_below		; high byte above, out of range
  1883                          ;	CMP #<(gram+8000)	; low byte check if in last page of GRAM
  1884                          	; 12 T if GRAM not in last page!
  1885                          
  1886  c97c a8                 	TAY			; for compare later
  1887  c97d 8a                 	TXA			; gaddr high
  1888  c97e 6901               	ADC #$01
  1889  c980 aa                 	TAX
  1890  c981 b00d               	BCS skip_below		; > $10000  -> skip
  1891  c983 c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1892  c985 e9ff               	SBC #>(gram+8000)
  1893                          	; 16 T
  1894  c987 b007               	BCS skip_below		; greater, so skip
  1895  c989 a000               +++	LDY #0			; first line in block
  1896  c98b 86fc               +	STX caddr+1		; shared store
  1897  c98d 20e1c9             	JSR fill_check
  1898                          skip_below
  1899                          
  1900  c990 a4a9               	LDY ysave		; restore original y position
  1901  c992 46a3               	LSR tmpmask
  1902                          				; bit 0 to carry, open to right, continue?
  1903  c994 b080               	BCS stepright8		; to next block if open
  1904                          ; long branch version
  1905                          ;	BCC +			; not open, finished
  1906                          ;	JMP stepright8		; to next block if open
  1907                          ;+
  1908                          
  1909                          
  1910                          ; get next location to fill from stack (if any)
  1911                          ;	in: fstack, fmode
  1912                          ; 	out: fstack, gaddr, tmpmask, tmpbits, x8, Y
  1913                          ; 	destroys: A
  1914                          
  1915                          process_stack
  1916  c996 a5fd               	LDA fstack		; stack empty?
  1917  c998 c531               	CMP basaryend
  1918  c99a a5fe               	LDA fstack+1
  1919  c99c e532               	SBC basaryend+1
  1920  c99e b003               	BCS +			; fstack >= basaryend -> not empty
  1921  c9a0 4c3fc2             	JMP gexit		; empty, we are finished
  1922                          
  1923                          	; top of stack: fetched multiple times until mask is completly filled!
  1924  c9a3 a003               +	LDY #fesize-1		; element's last component
  1925                          !ifndef opt_space {
  1926  c9a5 b1fd               	LDA (fstack),Y
  1927  c9a7 85a7               	STA x8			; 8x8 block position
  1928  c9a9 88                 	DEY
  1929                          }
  1930  c9aa b1fd               	LDA (fstack),Y
  1931  c9ac 85a3               	STA tmpmask		; pixel mask
  1932  c9ae 88                 	DEY
  1933  c9af b1fd               	LDA (fstack),Y
  1934  c9b1 85a6               	STA gaddr+1		; graphic addr high byte
  1935  c9b3 88                 	DEY
  1936  c9b4 b1fd               	LDA (fstack),Y		; graphic addr low byte combined with y-line
  1937  c9b6 aa                 	TAX			; needed twice
  1938  c9b7 29f8               	AND #%11111000		; split off address
  1939  c9b9 85a5               	STA gaddr
  1940                          !ifdef opt_space {
  1941                          	ORA #%00000100		; end bit marker (if 0 all bits are shifted)
  1942                          	STA x8			; low byte without least significant 3 bits
  1943                          				; x8 temporary reused. Calculated later ...
  1944                          }
  1945  c9bb 8a                 	TXA
  1946  c9bc 2907               	AND #%00000111		; split off y-line
  1947  c9be a8                 	TAY
  1948                          
  1949  c9bf b1a5               	LDA (gaddr),Y		; get pixels
  1950  c9c1 45a8               	EOR fmode		; according to set/reset
  1951  c9c3 8595               	STA tmpbits		; keep it for later
  1952  c9c5 25a3               	AND tmpmask		; focus on masked pixels
  1953  c9c7 08                 	PHP			; save Z flag
  1954  c9c8 f004               	BEQ pop_stack		; all bits unset, remove from stack, because
  1955                          				; it could be filled in one step!
  1956  c9ca c5a3               	CMP tmpmask		; all gaps filled?
  1957  c9cc d00f               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1958                          	; all gaps filled, next on stack 
  1959                          pop_stack
  1960  c9ce 38                 	SEC	
  1961  c9cf a5fd               	LDA fstack		; remove entry from stack
  1962  c9d1 e904               	SBC #fesize		; entry size
  1963  c9d3 85fd               	STA fstack
  1964  c9d5 b002               	BCS +
  1965  c9d7 c6fe               	DEC fstack+1
  1966  c9d9 28                 +	PLP			; all bits to fill empty?
  1967  c9da d0ba               	BNE process_stack	; all masked bits are set, next stack element
  1968                          				; all bits unset,
  1969  c9dc 24                 	!by $24			; = bit $ll, skip next statement (1 byte)
  1970                          				; stack already cleaned up
  1971  c9dd 28                 +++	PLP			; notstack cleanup
  1972                          
  1973                          !ifdef opt_space {
  1974                          	; Calculate the 8x8 block index from the the graphic address.
  1975                          	; Delayed, only if popped position is not already filled ...
  1976                          	; ((addr & 0x1fff) >> 3) % 40
  1977                          	; Takes 4 iterations. Register X, Y left untouched, 
  1978                          	; x8 contains gaddr low and has bit 2 set as end marker, bit 0, 1 is cleared.
  1979                          	; (312/8) % 40  -> 39
  1980                          	; 1 00111.000 : 101000
  1981                          	LDA gaddr+1		; divident high byte, mask out upper 3 bits
  1982                          	AND #$1f		; range 0 to 1f3f
  1983                          	ASL x8			; $1f always < 40
  1984                          -	ROL			; shift into high byte, carry from low byte
  1985                          	CMP #40			; modulo 40
  1986                          	BCC +			; dividend less divisor
  1987                          	SBC #40			; greater or equal divisor, c=1
  1988                          				; nothing done to keep the quotient
  1989                          +	ASL x8			; shift low byte divident
  1990                          	BNE -			; if end-marker bit shifted out -> 0
  1991                          	STA x8			; modulo in accu, stored to final location
  1992                          }
  1993                          
  1994  c9de 4cd9c8             	JMP f_line		; long (to far away) jump to fill line start
  1995                          
  1996                          
  1997                          ; Check upper or lower fill path
  1998                          ;	in: caddr, fmode, tmpmask, fcont, fstack(, x8)
  1999                          ;	out: fcont, fstack
  2000                          ;	destroys: A,X,Y
  2001                          
  2002                          fill_check
  2003  c9e1 b1fb               	LDA (caddr),Y
  2004  c9e3 45a8               	EOR fmode		; pixel data
  2005  c9e5 aa                 	TAX			; save for later
  2006  c9e6 25a3               	AND tmpmask		; mask to fill
  2007  c9e8 f013               	BEQ fc_cleared		; all masked pixels cleared?
  2008  c9ea c5a3               	CMP tmpmask		; check for gaps
  2009  c9ec f01b               	BEQ fc_exit		; all gaps filled, finished
  2010                          				; if not so, some pixels still set
  2011  c9ee a5a3               	LDA tmpmask
  2012                          fc_checkstart			; no continuation, init flag based on
  2013                          				; rightmost pixel:
  2014  c9f0 4a                 	LSR			; mask bit 0 to carry
  2015  c9f1 9017               	BCC fc_nocont		; maskbit empty?
  2016  c9f3 8a                 	TXA			; pixel data
  2017  c9f4 4a                 	LSR			; pixel bit 0 to carry
  2018  c9f5 b013               	BCS fc_nocont		; bit 0 set
  2019                          				; -> mask is 1 and pixel 0
  2020                          fc_cont
  2021  c9f7 a596               	LDA fcont		; set flag for continuation
  2022  c9f9 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  2023  c9fb d011               	BNE push_to_stack	; always non zero
  2024                          
  2025                          fc_cleared
  2026  c9fd a5a3               	LDA tmpmask		; pixel & mask -> 0
  2027                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  2028                          				; the caller asserts that this never happens
  2029  c9ff c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  2030  ca01 d0ed               	BNE fc_checkstart	; maybe a continuation ...
  2031                          				; 8 pixel line empty
  2032  ca03 a596               	LDA fcont		; continued gap?
  2033  ca05 2902               	AND #%00000010		; check bit 2
  2034  ca07 f0ee               	BEQ fc_cont		; new gap, start it and push on stack
  2035  ca09 60                 fc_exit	RTS			; gap continued and already on stack, leave
  2036                          
  2037                          fc_nocont
  2038  ca0a a596               	LDA fcont		; clear continuation flag
  2039  ca0c 29fd               	AND #%11111101		; clear bit 2
  2040                          
  2041                          push_to_stack
  2042  ca0e 8596               	STA fcont
  2043  ca10 18                 	CLC			; fstack points to top of stack
  2044  ca11 a5fd               	LDA fstack		; to next free stack element
  2045  ca13 6904               	ADC #fesize		; entry size
  2046  ca15 85fd               	STA fstack
  2047  ca17 9002               	BCC +
  2048  ca19 e6fe               	INC fstack+1
  2049                          +
  2050  ca1b a534               	LDA strbot+1		; check stack space
  2051  ca1d c5fe               	CMP fstack+1
  2052  ca1f b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  2053                          				; strbot MSB < fstack MSB
  2054                          out_of_memory			
  2055  ca21 203fc2             	JSR gexit
  2056  ca24 a210               	LDX #$10		; out of memory error
  2057  ca26 6c0003             	JMP (v_baserr)		; basic error handler
  2058  ca29 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  2059  ca2b a5fd               	LDA fstack		; MSB equal, check LSB
  2060  ca2d c533               	CMP strbot
  2061  ca2f b0f0               	BCS out_of_memory	; fstack collides with string heap!
  2062                          
  2063                          fc_put
  2064  ca31 98                 	TYA			; y-line (value 0-7) merged with
  2065  ca32 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  2066  ca34 a000               	LDY #0			; stack structure index, on next free element
  2067  ca36 91fd               	STA (fstack),Y
  2068  ca38 c8                 	INY
  2069  ca39 a5fc               	LDA caddr+1
  2070  ca3b 91fd               	STA (fstack),Y		; graphic address high
  2071  ca3d c8                 	INY
  2072  ca3e a5a3               	LDA tmpmask
  2073  ca40 91fd               	STA (fstack),Y
  2074                          !ifndef opt_space {
  2075  ca42 c8                 	INY
  2076  ca43 a5a7               	LDA x8			; 8x8 block position
  2077  ca45 91fd               	STA (fstack),Y
  2078                          }
  2079                          	
  2080  ca47 60                 	RTS
  2081                          	
  2082                          
  2083                          
  2084                          
  2085                          
  2086                          
  2087                          ;-----------------------------------------------------------------
  2088                          
  2089                          unnew
  2090                          
  2091  ca48 a52b               	LDA bassta
  2092  ca4a 8522               	STA str
  2093  ca4c a52c               	LDA bassta+1
  2094  ca4e 8523               	STA str+1
  2095  ca50 a001               	LDY #1
  2096  ca52 98                 	TYA
  2097  ca53 9122               	STA (str),y		; != 0
  2098                          
  2099  ca55 2033a5             	JSR b_rechain		; starting from bassta
  2100                          				; result in (str)
  2101  ca58 18                 	CLC			; str+1 -> new basic end
  2102  ca59 a423               	LDY str+1
  2103  ca5b a522               	LDA str
  2104  ca5d 6902               	ADC #2
  2105  ca5f 852d               	STA basend
  2106  ca61 9001               	BCC +
  2107  ca63 c8                 	INY
  2108  ca64 842e               +	STY basend+1
  2109  ca66 4c60a6             	JMP b_clr		; perform CLR
  2110                          
  2111                          
  2112                          ;-----------------------------------------------------------------
  2113                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
