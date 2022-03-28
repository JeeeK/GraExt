
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
     6                          	!text "1.33" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2022-03-27 v 1.33
    10                          ;	2020-05-03 v 1.32
    11                          ;	2019-10-30 v 1.31
    12                          ;	2019-10-24 v 1.30
    13                          ;	2019-10-10 v 1.29
    14                          ;	2016-09-10 v 1.28
    15                          ;	2016-07-13 v 1.27
    16                          ;	2016-07-09 v 1.26
    17                          ;	2016-06-21 v 1.25
    18                          ;	2016-06-16 v 1.24
    19                          ;	2016-05-29 v 1.23
    20                          ;	2016-05-20 v 1.22
    21                          ;	2016-05-16 v 1.21
    22                          ;	2016-02-23 v 1.20
    23                          ;	2016-01-15 v 1.19
    24                          ;	1992-12-28 v 1.18
    25                          ;	1986-03-24 v 1.17
    26                          ;	1985       v 0.00 - 1.16
    27                          ;
    28                          ; the initial development is based on the implemention
    29                          ; done in a Forth environment written with a common 
    30                          ; 6502 forth assembler.
    31                          ; later, the code has been pulled out from there, relocated and 
    32                          ; enriched with some glue code to finally form the first 
    33                          ; basic extension.
    34                          
    35                          ; command dispatcher style JMP/RTS
    36                          ;	(if defined)
    37                          ;command_rts_style=1
    38                          
    39                          ; error handling 
    40                          ;	(if defined)
    41                          ;no_error=1
    42                          
    43                          ; optimize for space (at runtime)
    44                          opt_space=1
    45                          
    46                          
    47                          ; basic interpreter registers, addresses and entry points
    48                          
    49                          type	= $0d
    50                          str     = $22		; string address
    51                          bassta	= $2b		; basic start pointer
    52                          basend	= $2d		; basic end pointer
    53                          basaryend	= $31		; basic end of array +1
    54                          strbot	= $33		; bottom of string heap 
    55                          ijmp    = $55		; address of JMP (addr)
    56                          chrget  = $73		; basic charget routine
    57                          chrgot  = $79		; basic last char got (charget routine)
    58                          txtptr	= $7A		; basic text pointer
    59                          facintl = $65		; integer result from b_fac2int
    60                          facinth = $64
    61                          facexp  = $61		; fac exponent, after b_getval
    62                          
    63                          z_reverseflag = $C7	; character routine
    64                          z_lastkey = $D7		; original use case, unused here
    65                          z_tmp = z_lastkey	; temporary reused for character routine
    66                          
    67                          v_baserr = $0300	; vector error routine
    68                          v_basstp = $0328	; vector error routine
    69                          v_bascmd = $0308	; vector interpreter parsing
    70                          v_basexp = $030a	; vector evaluate expression
    71                          
    72                          basic_rom = $A000	; start of BASIC ROM
    73                          
    74                          b_clr = $A660		; CLR command
    75                          b_interpreter = $A7AE	; interpreter loop
    76                          b_execstatement = $A7E7	; process statement (after chrget) - not used
    77                          b_execexpr =$AE92	; process expression - not used
    78                          b_getcomma = $AEFD	; read comma from basic text
    79                          b_illquant = $B248	; error "illegal quantity"
    80                          b_syntaxerror = $AF08	; error "syntax"
    81                          b_get8bit = $B79E	; read 8 bit numeric value from
    82                          			; basic text
    83                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    84                          			; from basic text
    85                          b_getval = $AD8A	; read numeric value from basic text
    86                          b_getexpr = $AD9E	; read expression from basic text
    87                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    88                          b_word2fac =$B391	; convert Y/Y to FAC (signed 16 bit)
    89                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    90                          b_fac2int = $BC9B	; convert FAC to integer
    91                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    92                          b_rechain = $A533	; rechain basic lines
    93                          b_str2fac = $BCF3	; convert string in FAC (expression handling)
    94                          b_chkparl = $AEFA 	; check '('
    95                          b_chkparr = $AEF7 	; check ')'
    96                          
    97                          t_to = $A4		; keyword TO token
    98                          
    99                          ; hardware registers and values
   100                          
   101                          prozport = $01		; processor port
   102                          memrom = %00110111	; basic+kernal rom
   103                          membas = %00110110	; basic ram+kernal rom
   104                          memram = %00110101	; basic+kernal ram
   105                          
   106                          vic_cr	= $D011		; VIC control register
   107                          vic_mcr	= $D018		; VIC memory control register
   108                          cia_pra	= $DD00		; CIA 2 port register A
   109                          
   110                          cram	= $CC00		; start of color ram
   111                          
   112                          gram	= $e000		; start of graphic bitmap ram
   113                          gramp	= gram >> 8	; start page of bitmap
   114                          
   115                          ; constants 
   116                          
   117                          xmax	= 320		; max x dimension
   118                          ymax	= 200		; max y dimension
   119                          
   120                          !ifdef opt_space {
   121                          fesize	= 3		; Fill stack entry size without block position
   122                          } else {
   123                          fesize	= 4		; Fill stack entry size with block position
   124                          }
   125                          
   126                          ; zeropage variables
   127                          
   128                          x	= $9B		; start coordinate x, low+high
   129                          xl	= x
   130                          xh	= x+1
   131                          y	= $AA		; start coordinate y
   132                          
   133                          xendl	= $9E		; end coordinate x, low+high
   134                          xendh	= $9F
   135                          yend	= $93		; end coordinate y
   136                          
   137                          kl	= $95		; gradient for lines, low+high
   138                          kh	= kl+1
   139                          tmpbits	= kl		; temp. var. (hline, vline, fill context)
   140                          tmp2	= kh		; temp. var. (hline, vline context)
   141                          fcont	= kh		; fill continuation flags (bit 1,0 for above, below)
   142                          
   143                          dxl	= $AB		; x delta, low+high
   144                          xsave	= dxl		; x register saved (hline, fill context)
   145                          dxh	= $A7
   146                          x8	= dxh		; 8x8 block index: (xh/xl) : 8 (fill context)
   147                          
   148                          dy	= $A9		; y delta
   149                          ysave	= dy		; y saved (hline context, fill context)
   150                          
   151                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   152                          ylimit	= ydir		; y limit in a 8x8 block (hline context)
   153                          fmode   = ydir		; mode mask: 0 | $FF (fill context)
   154                          
   155                          cl	= $A3		; dot count, low+high
   156                          ch	= $A4
   157                          ycount	= cl		; y count overall (hline context)
   158                          hcount	= ch		; horizontal blocks (hline context)
   159                          tmpmask	= cl		; temp. mask (fill context)
   160                          
   161                          gaddr	= $A5		; graphic address
   162                          
   163                          gpos	= $FB		; in graphic position
   164                          sgaddr	= gpos		; saved gaddr (hline context)
   165                          caddr	= gpos		; check gaddr (fill context)
   166                          
   167                          gcol	= $FD		; graphic color, in "graphic on" context only
   168                          fstack = gcol	; fill stack pointer (fill context)
   169                          
   170                          ; static ram areas
   171                          
   172                          savevpars = $0334	; original v_bascmd
   173                          saveverr = savevpars+2	; original v_baserr
   174                          savevstp = saveverr+2	; original v_basstp
   175                          savevexp = savevstp+2	; original v_basexp
   176                          savexl	= savevexp+2	; the graphic cursor: x low 
   177                          savexh	= savexl+1	; the graphic cursor: x high
   178                          savey	= savexh+1	; the graphic cursor: y
   179                          savemo	= savey+1	; the graphic mode
   180                          saveend = savemo+1	; byte after save area
   181                          
   182                          			; real place for gchange and gmask routines,
   183                          !ifdef ltc {
   184                          gramcode = $03ed - 26	; 15 bytes + 4*6+2
   185                          } else {
   186                          gramcode = $03ed	; 15 bytes
   187                          }
   188                          
   189                          ; LTC64 specifics
   190                          
   191                          !ifdef ltc {
   192                          
   193                          !cpu 65816
   194                          
   195                          bank4+3 = $040000
   196                          rombank+3 = $010000	; c't
   197                          
   198                          ; c't-Karte-Kontrollregister
   199                          
   200                          memconf = bank4 or 1
   201                          mc_off  = $80		; CPU 816 ausschalten
   202                          mc_slow = $40		; CPU 1 MHz
   203                          mc_epr  = $20		; EPROM in Bank0
   204                          mc_sim  = $10		; ROM-Simulation Bit
   205                          
   206                          }
   207                          
   208                          
   209                          
   210                          ;
   211                          ; initialize extension
   212                          
   213                          init
   214  c025 ad0803                     LDA v_bascmd		; check if hooks are already 
   215  c028 ae0903                     LDX v_bascmd+1		; in place 
   216  c02b c9b0               	CMP #<(parse)
   217  c02d d004               	BNE +
   218  c02f e0c0               	CPX #>(parse)
   219  c031 f052               	BEQ ++			; already hooked
   220                          
   221  c033 8d3403             +       STA savevpars		; save old vector
   222  c036 8e3503             	STX savevpars+1
   223  c039 a9b0               	LDA #<(parse)		; basic interpreter parser hook
   224  c03b 8d0803                     STA v_bascmd		; for commands
   225  c03e a9c0                       LDA #>(parse)
   226  c040 8d0903                     STA v_bascmd+1
   227                          
   228  c043 ad0a03                     LDA v_basexp		; basic interpreter parser hook
   229  c046 8d3a03             	STA savevexp		; for expressions
   230  c049 a9e4                       LDA #<(express)		; with save of old pointer
   231  c04b 8d0a03                     STA v_basexp
   232  c04e ad0b03                     LDA v_basexp+1
   233  c051 8d3b03             	STA savevexp+1
   234  c054 a9c0                       LDA #>(express)
   235  c056 8d0b03                     STA v_basexp+1
   236                          
   237  c059 ad2803                     LDA v_basstp
   238  c05c 8d3803             	STA savevstp
   239  c05f a99b                       LDA #<(stop)		; basic interpreter stop hook
   240  c061 8d2803                     STA v_basstp
   241  c064 ad2903                     LDA v_basstp+1
   242  c067 8d3903             	STA savevstp+1
   243  c06a a9c0                       LDA #>(stop)
   244  c06c 8d2903                     STA v_basstp+1
   245                          
   246  c06f ad0003                     LDA v_baserr
   247  c072 8d3603             	STA saveverr
   248  c075 a995                       LDA #<(error)		; basic interpreter error hook
   249  c077 8d0003                     STA v_baserr
   250  c07a ad0103                     LDA v_baserr+1
   251  c07d 8d3703             	STA saveverr+1
   252  c080 a9c0                       LDA #>(error)
   253  c082 8d0103                     STA v_baserr+1
   254                          
   255  c085 a200               ++	LDX #0			; set graphic cursor to (0,0)
   256  c087 8e3c03             	STX savexl
   257  c08a 8e3d03             	STX savexh
   258  c08d 8e3e03             	STX savey
   259  c090 e8                 	INX
   260  c091 8e3f03             	STX savemo		; set mode 1
   261  c094 60                         RTS
   262                          
   263                          error	
   264                          	; reg A may destroyed
   265  c095 20aac1             	JSR gra_off		; uses only reg A
   266  c098 6c3603             	JMP (saveverr)		; to original vector
   267                          
   268                          stop	
   269                          	; reg A may destroyed
   270  c09b a591               	LDA $91			; Scan code
   271  c09d c97f               	CMP #$7F		; STOP key?
   272  c09f d003               	BNE nostop
   273  c0a1 20aac1             	JSR gra_off		; uses only reg A
   274                          nostop
   275  c0a4 6c3803             	JMP (savevstp)		; to original vector
   276                          
   277                          
   278                          ;-----------------------------------------------------------------
   279                          
   280                          ; undo chrget
   281                          
   282                          undo_chrget
   283  c0a7 a57a               	LDA txtptr		; decrement text pointer by 1
   284  c0a9 d002               	BNE +
   285  c0ab c67b               	DEC txtptr+1
   286  c0ad c67a               +	DEC txtptr
   287  c0af 60                 	RTS
   288                          
   289                          ;-----------------------------------------------------------------
   290                          
   291                          ; start parsing an extension command ...
   292                          
   293                          parse
   294  c0b0 207300                     JSR chrget		; next char.
   295  c0b3 c926                       CMP #'&'		; command prefix
   296  c0b5 f006                       BEQ newcmd
   297  c0b7 20a7c0             	JSR undo_chrget
   298  c0ba 6c3403             	JMP (savevpars)
   299                          newcmd
   300  c0bd 207300                     JSR chrget		; command character
   301                          
   302  c0c0 a00e                       LDY #(cmdsend-cmds)	; map character to
   303                          				; command address ...
   304                          checknextcmd
   305  c0c2 88                         DEY
   306  c0c3 f01c               	BEQ parse_error
   307  c0c5 d912c1                     CMP cmds,Y
   308  c0c8 d0f8                       BNE checknextcmd	; try next
   309  c0ca 88                         DEY			; found
   310  c0cb 98                         TYA
   311  c0cc 0a                         ASL			; *2
   312  c0cd a8                         TAY
   313                          !ifndef command_rts_tyle {
   314                          	!set co=0		; command offset in jump table
   315  c0ce b921c1                     LDA cmdaddr+1,Y		; high byte from table
   316  c0d1 8556                       STA ijmp+1
   317  c0d3 b920c1                     LDA cmdaddr,Y		; low byte from table
   318  c0d6 8555                       STA ijmp
   319  c0d8 207300                     JSR chrget		; read next byte in basic text
   320  c0db 205400                     JSR ijmp-1		; go to command by JMP (addr)
   321  c0de 4caea7                     JMP b_interpreter	; continue parsing
   322                          } else {
   323                          	!set co=1		; command offset in jump table
   324                          	LDA #>(b_interpreter-1)	; return to interpreter
   325                          	PHA
   326                          	LDA #<(b_interpreter-1)
   327                          	PHA				
   328                                  LDA cmdaddr+1,Y		; command address (RTS style)
   329                                  PHA			; high byte on stack
   330                                  LDA cmdaddr,Y		; command address (RTS style)
   331                                  PHA			; low byte on stack
   332                                  JMP chrget		; read next byte in basic text 
   333                          				; and RTS to command
   334                          }
   335                          parse_error
   336  c0e1 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
   337                          
   338                          ;-----------------------------------------------------------------
   339                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   340                          express
   341  c0e4 a900               	LDA #0
   342  c0e6 850d               	STA type	
   343  c0e8 207300             	JSR chrget
   344  c0eb b003               	BCS exp_nonumber
   345  c0ed 4cf3bc             	JMP b_str2fac
   346                          exp_nonumber
   347  c0f0 c926                       CMP #'&'		; command prefix
   348  c0f2 f006                       BEQ newfunc
   349  c0f4 20a7c0             	JSR undo_chrget
   350  c0f7 6c3a03             	JMP (savevexp)		; original routine	
   351                          ;	JMP b_execexpr
   352                          newfunc
   353  c0fa 207300             	JSR chrget
   354  c0fd c95a               	CMP #'Z'
   355  c0ff d003               	BNE +
   356  c101 4ce0c6             	JMP get
   357  c104 c958               +	CMP #'X'
   358  c106 d003               	BNE +
   359  c108 4ccbc6             	JMP getposx
   360  c10b c959               +	CMP #'Y'
   361  c10d d0d2               	BNE parse_error
   362  c10f 4cd7c6             	JMP getposy
   363                          
   364                          ;-----------------------------------------------------------------
   365                          
   366                          ; the most commonly used command placed at the end ...
   367                          
   368  c112 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   369                          cmdsend
   370                          
   371                          cmdaddr
   372  c120 97caaac8a3c18bc7...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   373  c12c 63c817c742c8f2c4...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
   374                          
   375  c13a 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  c144 312e3333            !text "1.33" 

; ******** Source: graext-core.asm
   377  c148 20313938362c3230...	!text " 1986,2022 JOHANN@KLASEK.AT",0
   378                          
   379                          bitmask
   380  c164 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   381                          nbitmask
   382  c16c 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   383                          ytabl
   384  c174 004080c0           	!byte $00,$40,$80,$c0
   385                          ytabh
   386  c178 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   387  c17c e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   388  c180 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   389  c184 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   390  c188 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   391  c18c f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   392  c190 fe                 	!byte gramp+$1e
   393                          
   394                          ; for horiz. line
   395                          
   396                          maskleft0
   397                          maskleft
   398  c191 ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   399  c199 00                 	!byte $00
   400                          
   401                          maskright0
   402  c19a 00                 	!byte $00
   403                          maskright
   404  c19b 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   405                          
   406                          ;-----------------------------------------------------------------
   407                          
   408                          graphic
   409  c1a3 209eb7                     JSR b_get8bit
   410  c1a6 e000                       CPX #$00
   411  c1a8 d013                       BNE gra_other
   412                          gra0				; &G 0
   413                          gra_off
   414  c1aa a9c7                       LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   415  c1ac 8d00dd                     STA cia_pra
   416  c1af a915                       LDA #((1 <<4) + (2 <<1) + 1)
   417                          				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   418                          				; char addr $1000/4096 = char. ROM
   419  c1b1 8d18d0                     STA vic_mcr		; VIC memory control
   420  c1b4 ad11d0                     LDA vic_cr		; VIC control register
   421  c1b7 29df                       AND #%11011111		; Hires mode off
   422  c1b9 8d11d0                     STA vic_cr
   423  c1bc 60                         RTS
   424                          
   425                          gra_other
   426  c1bd e001                       CPX #$01
   427  c1bf f00f               	BEQ gra1
   428  c1c1 e002               	CPX #$02
   429  c1c3 f00e                       BEQ gra2
   430  c1c5 e004               	CPX #$04
   431  c1c7 f043                       BEQ gra_clear		; &G 4 (erase only, leave mode)
   432  c1c9 e003               	CPX #$03		; &G 3 (graphic on)
   433  c1cb f029               	BEQ gra_on
   434  c1cd 4c48b2                     JMP b_illquant		; parameter illegal
   435                          	
   436                          gra1				; &G 1
   437  c1d0 200cc2             	JSR gra_clear
   438                          
   439                          gra2
   440  c1d3 20f1b7                     JSR b_getcomma8bit
   441  c1d6 8a                         TXA			; foreground color
   442  c1d7 0a                         ASL			; upper nibble
   443  c1d8 0a                         ASL
   444  c1d9 0a                         ASL
   445  c1da 0a                         ASL
   446  c1db 85fd                       STA gcol
   447  c1dd 20f1b7                     JSR b_getcomma8bit
   448  c1e0 8a                         TXA			; background color
   449  c1e1 290f                       AND #$0F
   450  c1e3 05fd                       ORA gcol
   451  c1e5 a000                       LDY #$00
   452                          cram_loop
   453  c1e7 9900cc                     STA cram,Y		; fill color RAM
   454  c1ea 9900cd                     STA cram+$100,Y
   455  c1ed 9900ce                     STA cram+$200,Y
   456  c1f0 99e8ce                     STA cram+$300-24,Y
   457  c1f3 c8                         INY
   458  c1f4 d0f1                       BNE cram_loop
   459                          
   460                          gra_on
   461  c1f6 202bc2             	JSR gra_setupcode
   462                          
   463  c1f9 a9c4                       LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   464  c1fb 8d00dd                     STA cia_pra
   465  c1fe a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   466  c200 8d18d0                     STA vic_mcr		; VIC memory control
   467  c203 ad11d0                     LDA vic_cr		; VIC control register
   468  c206 0920                       ORA #%00100000		; Bit 5 = 1: Hires on
   469  c208 8d11d0                     STA vic_cr
   470  c20b 60                         RTS
   471                          
   472                          gra_clear
   473  c20c a220                       LDX #$20		; Pages (8 KByte)
   474  c20e a9e0                       LDA #>gram
   475  c210 85fc                       STA gpos+1
   476  c212 a000                       LDY #$00
   477  c214 84fb                       STY gpos
   478  c216 98                         TYA
   479                          gra_fill
   480  c217 91fb                       STA (gpos),Y		; Loop unroll
   481  c219 c8                         INY
   482  c21a 91fb                       STA (gpos),Y
   483  c21c c8                         INY
   484  c21d 91fb                       STA (gpos),Y
   485  c21f c8                         INY
   486  c220 91fb                       STA (gpos),Y
   487  c222 c8                         INY
   488  c223 d0f2                       BNE gra_fill
   489  c225 e6fc                       INC gpos+1
   490  c227 ca                         DEX
   491  c228 d0ed                       BNE gra_fill
   492  c22a 60                 	RTS
   493                          
   494                          gra_setupcode
   495  c22b a229               	LDX #(gromcode_end-gromcode) ; count of bytes
   496                          gra_copycode
   497  c22d bd4ec2             	LDA gromcode-1,X
   498  c230 9dd203             	STA gramcode-1,X
   499  c233 ca                 	DEX
   500  c234 d0f7               	BNE gra_copycode
   501  c236 ad3f03             	LDA savemo
   502  c239 290f               	AND #$0F
   503  c23b aa                 	TAX
   504  c23c 4c77c6             	JMP setmode_enter	; re-apply mode to routines
   505                          				; implicit RTS
   506                          
   507                          ;-----------------------------------------------------------------
   508                          
   509                          gexit
   510  c23f a501                       LDA prozport
   511  c241 0902                       ORA #%00000010		; kernal ROM enable
   512  c243 8501                       STA prozport
   513  c245 58                         CLI			; allow interrupts
   514  c246 60                         RTS
   515                          
   516                          ;-----------------------------------------------------------------
   517                          
   518                          ginit
   519  c247 a501                       LDA prozport
   520  c249 29fd                       AND #%11111101		; Kernal ROM disable
   521  c24b 78                         SEI			; disable interrupts
   522  c24c 8501                       STA prozport
   523  c24e 60                         RTS
   524                          				; on exit Z=0
   525                          
   526                          ;-----------------------------------------------------------------
   527                          
   528                          ; These are selfmodified routines, which has to placed into RAM
   529                          ; (on every graphic "on")
   530                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   531                          
   532                          gromcode
   533                          
   534                          !pseudopc gramcode {
   535                          
   536                          ; change a graphic location
   537                          
   538                          gchange
   539                          !ifdef ltc {
   540  c24f a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   541  c251 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   542                          }
   543  c255 b1a5                       LDA (gaddr),Y
   544                          gchange_op
   545  c257 1d64c1                     ORA bitmask,X
   546  c25a 91a5                       STA (gaddr),Y
   547                          !ifdef ltc {
   548  c25c a910               	LDA #mc_sim		; vollständige ROM-Simulation
   549  c25e 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   550                          }
   551  c262 60                         RTS
   552                          
   553                          ; mask a graphic location 
   554                          
   555                          gmask
   556                          !ifdef ltc {
   557  c263 eb                 	XBA
   558  c264 a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   559  c266 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   560  c26a eb                 	XBA
   561                          }
   562                          gmask_flip
   563  c26b 4900                       EOR #$00
   564                          gmask_op
   565  c26d 11a5                       ORA (gaddr),Y
   566  c26f 91a5                       STA (gaddr),Y
   567                          !ifdef ltc {
   568  c271 a910               	LDA #mc_sim		; vollständige ROM-Simulation
   569  c273 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   570                          }
   571  c277 60                         RTS
   572                          
   573                          }
   574                          
   575                          gromcode_end
   576                          
   577                          ;-----------------------------------------------------------------
   578                          
   579                          position
   580  c278 a5aa                       LDA y
   581  c27a 4a                         LSR
   582  c27b 4a                         LSR
   583  c27c 4a                         LSR			; y/8
   584  c27d a8                         TAY
   585  c27e 2903                       AND #%00000011		; (y/8) mod 4
   586  c280 aa                         TAX
   587  c281 a59b                       LDA xl			; x low
   588  c283 29f8                       AND #%11111000		; clear bit 2-0
   589  c285 18                         CLC
   590  c286 7d74c1                     ADC ytabl,X		; addr low: y base + x part
   591  c289 85a5                       STA gaddr
   592  c28b a59c                       LDA xh			; addr high: x part
   593  c28d 7978c1                     ADC ytabh,Y		; 	+ y base
   594  c290 85a6                       STA gaddr+1
   595  c292 a5aa                       LDA y			; vertical offset
   596  c294 2907                       AND #%00000111		; y mod 8
   597  c296 a8                         TAY
   598  c297 a59b                       LDA xl
   599  c299 2907                       AND #%00000111		; x mod 8
   600  c29b aa                         TAX			; horizonal offset
   601  c29c 60                         RTS			; (bitmask)
   602                          
   603                          
   604                          ;-----------------------------------------------------------------
   605                          
   606                          ; swap tupel xl,xh <-> xendl,xendh
   607                          
   608                          swap_x_xend
   609  c29d a69e                       LDX xendl		; swap x, xend
   610  c29f a49b                       LDY xl
   611  c2a1 869b                       STX xl
   612  c2a3 849e                       STY xendl
   613                          
   614  c2a5 a69f                       LDX xendh
   615  c2a7 a49c                       LDY xh
   616  c2a9 849f                       STY xendh
   617  c2ab 869c                       STX xh
   618  c2ad 60                 	RTS
   619                          
   620                          
   621                          ;-----------------------------------------------------------------
   622                          
   623                          ; line y up, x left, dx < dy (case 1)
   624                          
   625                          line_up_steep
   626  c2ae 2078c2                     JSR position		; x,y
   627                          loop_yup_xleft
   628  c2b1 20d303                     JSR gchange		; pixel
   629                          
   630  c2b4 18                         CLC			; k += dx
   631  c2b5 a595                       LDA kl
   632  c2b7 65ab                       ADC dxl			; dxh is 0, because dx < dy
   633  c2b9 8595                       STA kl
   634  c2bb 9014                       BCC +			; k >= 0 ->
   635                          
   636  c2bd e5a9               ++	SBC dy			; k -= dy (C=1)
   637  c2bf 8595                       STA kl
   638                          
   639  c2c1 ca                  	DEX			; x--
   640  c2c2 100d                       BPL +
   641  c2c4 a207                       LDX #7			; wrap around
   642  c2c6 38                 	SEC
   643  c2c7 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   644  c2c9 e908                       SBC #8
   645  c2cb 85a5                       STA gaddr
   646  c2cd b002                       BCS +
   647  c2cf c6a6                       DEC gaddr+1
   648                          
   649  c2d1 88                 +	DEY			; y--
   650  c2d2 100f                       BPL +++
   651  c2d4 38                         SEC			; y overflow
   652  c2d5 a5a5                       LDA gaddr
   653  c2d7 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   654  c2d9 85a5                       STA gaddr
   655  c2db a5a6                       LDA gaddr+1
   656  c2dd e901               	SBC #1
   657  c2df 85a6                       STA gaddr+1
   658  c2e1 a007                       LDY #7			; wrap around
   659                          
   660  c2e3 c6a3               +++	DEC cl			; until c=0
   661  c2e5 d0ca                       BNE loop_yup_xleft
   662  c2e7 4c3fc2                     JMP gexit
   663                          
   664                          
   665                          ;-----------------------------------------------------------------
   666                          
   667                          ; line x left, y up, dx > dy (case 2)
   668                          
   669                          line_up_flat
   670  c2ea 2078c2                     JSR position		; x,y
   671  c2ed a5a3               	LDA cl			; counter adjustment for
   672  c2ef f002               	BEQ +			; prepare for dec-dec-counting
   673  c2f1 e6a4               	INC ch
   674                          +
   675                          loop_xleft_yup
   676  c2f3 20d303                     JSR gchange		; pixel
   677                          
   678  c2f6 18                         CLC			; k += dy
   679  c2f7 a595                       LDA kl
   680  c2f9 65a9                       ADC dy
   681  c2fb 8595                       STA kl
   682  c2fd 9020                       BCC +			; k < 0
   683  c2ff e696                       INC kh
   684  c301 301c               	BMI +			; k < 0
   685                          
   686  c303 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   687  c305 8595                       STA kl
   688  c307 a596                       LDA kh
   689  c309 e5a7                       SBC dxh		
   690  c30b 8596                       STA kh
   691                          
   692  c30d 88                         DEY			; y--
   693  c30e 100f                       BPL +
   694  c310 38                 	SEC			; C=1 not always true (SBC above)
   695  c311 a5a5                       LDA gaddr		; y overflow
   696  c313 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   697  c315 85a5                       STA gaddr
   698  c317 a5a6                       LDA gaddr+1
   699  c319 e901               	SBC #1
   700  c31b 85a6                       STA gaddr+1
   701  c31d a007               	LDY #7			; wrap around
   702                          
   703  c31f ca                 +	DEX			; x--
   704  c320 100d                       BPL +++
   705  c322 a207                       LDX #7			; wrap around
   706  c324 38                 	SEC
   707  c325 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   708  c327 e908                       SBC #8
   709  c329 85a5                       STA gaddr
   710  c32b b002                       BCS +++
   711  c32d c6a6                       DEC gaddr+1
   712                          +++
   713  c32f c6a3               	DEC cl			; c--
   714  c331 d0c0                       BNE loop_xleft_yup
   715  c333 c6a4                       DEC ch			; adjusted high which allows this
   716  c335 d0bc                       BNE loop_xleft_yup
   717                          
   718  c337 4c3fc2                     JMP gexit
   719                          
   720                          
   721                          
   722                          ;-----------------------------------------------------------------
   723                          
   724                          ; line x left, y down, dx > dy (case 3)
   725                          
   726                          line_down_flat
   727  c33a 2078c2                     JSR position		; x,y
   728  c33d a5a3               	LDA cl			; counter adjustment for
   729  c33f f002               	BEQ +			; prepare for dec-dec-counting
   730  c341 e6a4               	INC ch
   731                          +
   732                          loop_xleft_ydown
   733  c343 20d303                     JSR gchange		; pixel
   734                          
   735  c346 18                         CLC			; k += dy
   736  c347 a595                       LDA kl
   737  c349 65a9                       ADC dy
   738  c34b 8595                       STA kl
   739  c34d 9021                       BCC +			; k < 0
   740  c34f e696                       INC kh
   741  c351 301d               	BMI +			; k < 0
   742                          
   743  c353 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   744  c355 8595                       STA kl
   745  c357 a596                       LDA kh
   746  c359 e5a7                       SBC dxh		
   747  c35b 8596                       STA kh
   748                          
   749  c35d c8                         INY			; y++
   750  c35e c008                       CPY #8
   751  c360 d00e                       BNE +
   752                          	; C=1
   753  c362 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   754  c364 693f                       ADC #$40-1		; C already set by CPY
   755  c366 85a5                       STA gaddr
   756  c368 a5a6                       LDA gaddr+1
   757  c36a 6901               	ADC #1
   758  c36c 85a6                       STA gaddr+1
   759  c36e a000                       LDY #0			; wrap around
   760                          
   761  c370 ca                 +	DEX			; x--
   762  c371 100d                       BPL +++
   763  c373 a207                       LDX #7			; wrap around
   764  c375 38                 	SEC
   765  c376 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   766  c378 e908                       SBC #8
   767  c37a 85a5                       STA gaddr
   768  c37c b002                       BCS +++
   769  c37e c6a6                       DEC gaddr+1
   770                          +++
   771  c380 c6a3               	DEC cl			; c--
   772  c382 d0bf               	BNE loop_xleft_ydown
   773  c384 c6a4               	DEC ch			; adjusted high which allows this
   774  c386 d0bb                       BNE loop_xleft_ydown
   775                          
   776  c388 4c3fc2                     JMP gexit
   777                          
   778                          
   779                          ;-----------------------------------------------------------------
   780                          
   781                          ; line y down, x right, dx < dy (case 4)
   782                          
   783                          line_down_steep
   784  c38b 2078c2                     JSR position		; x,y
   785                          loop_ydown_xleft
   786  c38e 20d303                     JSR gchange		; pixel
   787                          
   788  c391 18                         CLC			; k += dx
   789  c392 a595                       LDA kl
   790  c394 65ab                       ADC dxl			; dxh is 0, because dx < dy
   791  c396 8595                       STA kl
   792  c398 9014                       BCC +			; k >= 0 ->
   793                          
   794  c39a e5a9               	SBC dy			; k -= dy, C=1
   795  c39c 8595                       STA kl
   796                          
   797  c39e ca                  	DEX			; x--
   798  c39f 100d                       BPL +
   799  c3a1 a207                       LDX #7			; wrap around
   800  c3a3 38                 	SEC
   801  c3a4 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   802  c3a6 e908                       SBC #8
   803  c3a8 85a5                       STA gaddr
   804  c3aa b002                       BCS +
   805  c3ac c6a6                       DEC gaddr+1
   806                          
   807  c3ae c8                 +	INY			; y++
   808  c3af c008                       CPY #8			; y overflow?
   809  c3b1 d00e                       BNE +++
   810  c3b3 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   811  c3b5 693f                       ADC #$40-1		; C already set by CPY
   812  c3b7 85a5                       STA gaddr
   813  c3b9 a5a6                       LDA gaddr+1
   814  c3bb 6901               	ADC #1
   815  c3bd 85a6                       STA gaddr+1
   816  c3bf a000                       LDY #0			; wrap around
   817                          
   818  c3c1 c6a3               +++	DEC cl			; c--
   819                          				; until c=0
   820  c3c3 d0c9                       BNE loop_ydown_xleft
   821  c3c5 4c3fc2                     JMP gexit
   822                          
   823                          
   824                          ;-----------------------------------------------------------------
   825                          
   826                          getcommaxy
   827  c3c8 20fdae                     JSR b_getcomma		; check ","
   828                          getxy
   829  c3cb 208aad                     JSR b_getval		; get X coord. value
   830  c3ce 20f7b7                     JSR b_convint
   831  c3d1 c901                       CMP #>xmax
   832  c3d3 900c               	BCC gcxy_xok
   833  c3d5 f003                       BEQ ++			; X = $1xx
   834  c3d7 203ac6                     JSR range_error
   835                          
   836  c3da c040               ++	CPY #<xmax		; check X low
   837  c3dc 9003                       BCC +
   838  c3de 203ac6                     JSR range_error
   839                          +
   840                          gcxy_xok
   841  c3e1 84fb                       STY gpos		; temporary save X coord.
   842  c3e3 85fc                       STA gpos+1
   843                          
   844  c3e5 20f1b7                     JSR b_getcomma8bit
   845                          				; get Y coord. value
   846  c3e8 e0c8                       CPX #ymax
   847  c3ea 9003                       BCC +
   848  c3ec 203ac6                     JSR range_error
   849                          +
   850  c3ef a4fb                       LDY gpos		; restory X coord.
   851  c3f1 a5fc                       LDA gpos+1
   852  c3f3 60                         RTS
   853                          
   854                          
   855                          ;-----------------------------------------------------------------
   856                          
   857                          para_hline_box
   858  c3f4 20cbc3                     JSR getxy		; get startpoint
   859  c3f7 86aa                       STX y
   860  c3f9 8e3e03                     STX savey		; save as cursor, too
   861  c3fc 859c                       STA xh
   862  c3fe 849b                       STY xl
   863  c400 8d3d03             	STA savexh
   864  c403 8c3c03             	STY savexl
   865  c406 20fdae                     JSR b_getcomma		; get length
   866  c409 208aad                     JSR b_getval
   867  c40c 20f7b7                     JSR b_convint
   868                          				; calculate end point
   869  c40f aa                         TAX			; save length high byte
   870  c410 98                         TYA			; length low byte
   871  c411 18                         CLC
   872  c412 659b                       ADC xl			; low xend = x+length
   873  c414 859e                       STA xendl
   874  c416 a8                 	TAY
   875  c417 8a                         TXA			; high
   876  c418 659c                       ADC xh			; high xend = x+length
   877  c41a 859f                       STA xendh
   878  c41c aa                 	TAX
   879                          
   880  c41d c901               	CMP #>xmax		; endpoint outside?
   881  c41f 9005               	BCC +
   882  c421 d003               	BNE +			; >$200 (512)
   883  c423 98                 	TYA
   884  c424 e940               	SBC #<xmax
   885  c426 60                 +	RTS			; C=1 out of range, C=0 ok
   886                          
   887                          ;-----------------------------------------------------------------
   888                          
   889                          hline
   890  c427 20f4c3             	JSR para_hline_box
   891  c42a 9003               	BCC +
   892  c42c 203ac6             	JSR range_error
   893                          				; XXX xend=xmax-1 ?
   894                          +
   895  c42f 8e3d03                     STX savexh
   896  c432 8c3c03                     STY savexl		; also save as final cursor
   897                          
   898  c435 a900               	LDA #0			; default thickness 0 (means 1 pixel)
   899  c437 85a3               	STA ycount
   900  c439 207900             	JSR chrgot		; last char. again
   901  c43c f019               	BEQ +++			; command end? no optional param.
   902  c43e 20f1b7             	JSR b_getcomma8bit
   903  c441 8a                 	TXA			; optional 8-bit parameter
   904  c442 85a3               	STA ycount		; hline thickness
   905  c444 f011               	BEQ +++			; 0 means 1 pixel
   906  c446 18                 	CLC
   907  c447 65aa               	ADC y			; end position for y coord.
   908  c449 b004               	BCS +			; > 255
   909  c44b c9c8               	CMP #ymax
   910  c44d 9008               	BCC +++
   911                          +				; C=1 from ADC or CMP before
   912  c44f 203ac6             	JSR range_error		; corrupts A
   913                          				; XXX ycount=ymax-y-1 ?
   914                          				; xend >= x
   915  c452 b003               	BCS hl_noxswap		; always
   916                          
   917                          hline_start
   918  c454 209dc2             	JSR swap_x_xend		; xend < x, entry from line
   919                          	
   920                          hl_noxswap
   921                          				; xend > x
   922                          +++
   923  c457 e6a3               	INC ycount		; count to 0
   924  c459 2047c2                     JSR ginit		; map in graphic memory
   925                          
   926  c45c 2078c2                     JSR position		; graphic position x,y
   927                          
   928  c45f a5a5               	LDA gaddr		; save position for vertical
   929  c461 85fb               	STA sgaddr
   930  c463 a5a6               	LDA gaddr+1
   931  c465 85fc               	STA sgaddr+1
   932  c467 86ab               	STX xsave
   933  c469 84a9               	STY ysave
   934                          
   935  c46b a59e                       LDA xendl
   936  c46d 2907                       AND #%00000111
   937  c46f 8596                       STA tmp2		; xend mod 8, mask index
   938  c471 a59b                       LDA xl
   939  c473 29f8                       AND #%11111000		; (xl div 8)*8
   940  c475 8595                       STA tmpbits
   941  c477 a59e                       LDA xendl		; xend unmasked
   942  c479 38                         SEC
   943  c47a e595                       SBC tmpbits		; finally: xend - (x div 8)*8 
   944  c47c 8595                       STA tmpbits
   945  c47e a59f                       LDA xendh
   946  c480 e59c                       SBC xh
   947  c482 4a                         LSR			; / 8 ->  0-39
   948  c483 a595                       LDA tmpbits		; only 1 highest bit
   949  c485 6a                         ROR			; and 3 lower bits
   950  c486 4a                         LSR
   951  c487 4a                         LSR
   952                                  			; 8-pixel-blocks count
   953  c488 85a4               	STA hcount		; save for vertical extension
   954                           
   955                          hl_vertloop
   956  c48a 98                 	TYA			; calculate max. Y in 8x8 block
   957  c48b 18                 	CLC
   958  c48c 65a3               	ADC ycount
   959  c48e c908               	CMP #8
   960  c490 9002               	BCC +
   961  c492 a908               	LDA #8
   962  c494 85a8               +	STA ylimit
   963                          
   964  c496 bd91c1                     LDA maskleft,X		; starting mask
   965  c499 8595               	STA tmpbits
   966  c49b a6a4               	LDX hcount		; how many blocks
   967                          
   968                          hl_nextblock
   969  c49d ca                         DEX
   970                          hl_islastblock
   971  c49e 301d                       BMI hl_lastblock
   972                          				; leave loop if X<0
   973  c4a0 a4a9               	LDY ysave
   974  c4a2 a595               -	LDA tmpbits		; mask
   975  c4a4 20e703             	JSR gmask		; first with left end mask
   976  c4a7 c8                 	INY			; vertical down
   977  c4a8 c4a8               	CPY ylimit		; in 8x8 box
   978  c4aa d0f6               	BNE -
   979                          
   980  c4ac 18                         CLC			; gaddr += 8 (one block to right)
   981  c4ad a5a5                       LDA gaddr
   982  c4af 6908                       ADC #8
   983  c4b1 85a5                       STA gaddr
   984  c4b3 9002                       BCC +
   985  c4b5 e6a6                       INC gaddr+1
   986                          
   987  c4b7 a9ff               +	LDA #$FF		; following with full 8-pixel mask
   988  c4b9 8595               	STA tmpbits
   989  c4bb d0e0               	BNE hl_nextblock	; always
   990                          
   991                          hl_lastblock
   992  c4bd a696                       LDX tmp2		; xend mask index
   993  c4bf 3d9bc1                     AND maskright,X		; current mask combined with mask right end
   994  c4c2 8595               	STA tmpbits		; mask
   995  c4c4 a4a9               	LDY ysave		; start position in 8x8 block
   996  c4c6 a595               -	LDA tmpbits		; mask
   997  c4c8 20e703             	JSR gmask		; modify
   998  c4cb c8                 	INY			; vertical down
   999  c4cc c6a3               	DEC ycount		; overall y counter
  1000  c4ce c4a8               	CPY ylimit
  1001  c4d0 d0f4               	BNE -
  1002                          
  1003  c4d2 a5a3               	LDA ycount		; finished
  1004  c4d4 d003               	BNE +			; roll-over into 8x8 block below
  1005  c4d6 4c3fc2                     JMP gexit		; leave
  1006                          
  1007  c4d9 18                 +	CLC
  1008  c4da a5fb               	LDA sgaddr
  1009  c4dc 6940               	ADC #$40		; next 8-pixel row below
  1010  c4de 85fb               	STA sgaddr		; + $140 (320)
  1011  c4e0 85a5               	STA gaddr
  1012  c4e2 a5fc               	LDA sgaddr+1
  1013  c4e4 6901               	ADC #$01
  1014  c4e6 85fc               	STA sgaddr+1
  1015  c4e8 85a6               	STA gaddr+1
  1016  c4ea a6ab               	LDX xsave		; initial mask index
  1017  c4ec a000               	LDY #0			; start on top of 8x8
  1018  c4ee 84a9               	STY ysave
  1019  c4f0 f098               	BEQ hl_vertloop
  1020                          ;-----------------------------------------------------------------
  1021                          
  1022                          vline
  1023  c4f2 20cbc3                     JSR getxy		; get startpoint
  1024  c4f5 859c                       STA xh
  1025  c4f7 8d3d03                     STA savexh		; save as cursor too
  1026  c4fa 849b                       STY xl
  1027  c4fc 8c3c03                     STY savexl
  1028  c4ff 8693                       STX yend		; initial point is endpoint
  1029                          
  1030  c501 20f1b7                     JSR b_getcomma8bit
  1031                          				; get length
  1032  c504 18                         CLC			; calculate end point
  1033  c505 8a                         TXA			; length
  1034                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1035                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1036                          ;	STA tmpbits
  1037  c506 6593                       ADC yend		; length + initial point is startpoint
  1038  c508 b005               	BCS vline_iq		; > 255
  1039  c50a c9c8                       CMP #ymax		; outside?
  1040  c50c a8                 	TAY			; keep startpoint
  1041  c50d 9003                       BCC +
  1042                          vline_iq
  1043  c50f 203ac6                     JSR range_error		; corrupts A
  1044                          				; XXX Y = ymax-1 ?
  1045  c512 84aa               +	STY y			; startpoint
  1046  c514 8c3e03             	STY savey		; set cursor y position
  1047  c517 18                 	CLC
  1048  c518 900e               	BCC +++			; skip following, because y, yend are already ordered
  1049                          
  1050                          vline_start			; entry point from line command (only)
  1051  c51a a5aa               	LDA y			; order of y, yend is not defined
  1052  c51c c593               	CMP yend
  1053  c51e b008               	BCS vl_noyswap		; yend > y ->
  1054  c520 a5aa               	LDA y			; swap y, yend
  1055  c522 a693               	LDX yend
  1056  c524 8593               	STA yend
  1057  c526 86aa               	STX y
  1058                          vl_noyswap
  1059                          				; startpoint is below the endpoint
  1060  c528 2047c2             +++	JSR ginit		; map in graphic memory
  1061                          
  1062                          vl_start
  1063  c52b 2078c2                     JSR position		; graphic position x,y
  1064  c52e bd64c1                     LDA bitmask,X
  1065  c531 8596                       STA tmp2		; save mask
  1066                          ; DON'T-CHANGE: replace ...
  1067  c533 38                         SEC
  1068  c534 a5aa                       LDA y			; startpoint is greater!
  1069  c536 e593                       SBC yend		; vertical length
  1070  c538 aa                         TAX
  1071                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmpbits)
  1072                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1073                          ;	LDX tmpbits
  1074  c539 e8                         INX			; +1 (exit on 0)
  1075  c53a 38                 	SEC			; for subtraction, never changed!
  1076                          vl_nextline
  1077  c53b a596                       LDA tmp2
  1078  c53d 20e703                     JSR gmask		; modify 
  1079  c540 88                         DEY			; go up
  1080  c541 100e                       BPL +
  1081  c543 a5a5                       LDA gaddr		; C=1
  1082  c545 e940               	SBC #$40		; gaddr -= 320
  1083  c547 85a5                       STA gaddr
  1084  c549 a5a6                       LDA gaddr+1
  1085  c54b e901                       SBC #$01
  1086  c54d 85a6                       STA gaddr+1
  1087  c54f a007                       LDY #7			; wrap y offset
  1088  c551 ca                 +	DEX			; all vertical positions done?
  1089  c552 d0e7                       BNE vl_nextline
  1090  c554 4c3fc2                     JMP gexit		; leave
  1091                          
  1092                          
  1093                          ;-----------------------------------------------------------------
  1094                          
  1095                          line
  1096  c557 20cbc3                     JSR getxy		; get startpoint
  1097  c55a 849b                       STY xl 
  1098  c55c 859c                       STA xh
  1099  c55e 86aa                       STX y
  1100                          
  1101  c560 20c8c3                     JSR getcommaxy		; get endpoint
  1102                          line_start
  1103  c563 8c3c03                     STY savexl		; save as cursor position too
  1104  c566 849e                       STY xendl
  1105  c568 8d3d03                     STA savexh
  1106  c56b 859f                       STA xendh
  1107  c56d 8e3e03                     STX savey
  1108  c570 8693                       STX yend
  1109                          
  1110  c572 a000                       LDY #$00		; initialize to 0
  1111  c574 84a8                       STY ydir
  1112  c576 8495                       STY kl
  1113  c578 8496                       STY kh
  1114                          
  1115  c57a 38                         SEC
  1116  c57b a59b                       LDA xl			; calculate dx
  1117  c57d e59e                       SBC xendl
  1118  c57f 85ab                       STA dxl
  1119  c581 a59c                       LDA xh
  1120  c583 e59f                       SBC xendh
  1121  c585 85a7                       STA dxh
  1122                          
  1123  c587 b018                       BCS li_xend_left
  1124                          	; dx != 0
  1125                          				; negate dx:
  1126  c589 98                         TYA			; Y=A=0
  1127  c58a 38                         SEC			; dx = 0 - dx
  1128  c58b e5ab                       SBC dxl
  1129  c58d 85ab                       STA dxl
  1130  c58f 98                         TYA			; Y=A=0
  1131  c590 e5a7                       SBC dxh
  1132  c592 85a7                       STA dxh
  1133                          				; C=0 always, needed later
  1134  c594 209dc2             	jsr swap_x_xend
  1135  c597 a6aa                       LDX y			; swap y
  1136  c599 a493                       LDY yend
  1137  c59b 8693                       STX yend
  1138  c59d 84aa                       STY y
  1139                          
  1140  c59f 9007                       BCC li_x_different
  1141                          				; C=0 always (from negation before)
  1142                          
  1143                          li_xend_left
  1144                                  			; A already contains dxh
  1145  c5a1 05ab                       ORA dxl			; dx = 0?
  1146  c5a3 d003                       BNE li_x_different
  1147  c5a5 4c1ac5                     JMP vline_start		; vertical line case
  1148                          
  1149                          li_x_different
  1150  c5a8 38                         SEC			; calculate dy
  1151  c5a9 a593                       LDA yend
  1152  c5ab e5aa                       SBC y
  1153  c5ad b006                       BCS li_y_right		; yend >= y?
  1154  c5af 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1155  c5b1 6901                       ADC #$01		; C=0
  1156  c5b3 85a8                       STA ydir		; always not 0: flag y goes up
  1157                          
  1158                          li_y_right
  1159  c5b5 85a9                       STA dy
  1160  c5b7 d007                       BNE +
  1161  c5b9 a900               	LDA #0			; line thickness = 1
  1162  c5bb 85a3               	STA ycount
  1163  c5bd 4c54c4                     JMP hline_start		; horizontal line case
  1164                          +
  1165                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1166                          
  1167  c5c0 a5a7                       LDA dxh			; dx > dy
  1168  c5c2 d01c                       BNE line_flat		; yes -> flat
  1169  c5c4 a5a9                       LDA dy			; no -> steep
  1170  c5c6 aa                         TAX
  1171  c5c7 c5ab                       CMP dxl
  1172  c5c9 9015                       BCC line_flat
  1173                          
  1174                          line_steep
  1175  c5cb e8                         INX	
  1176  c5cc 86a3                       STX cl			; c = dy+1
  1177  c5ce 4a                         LSR			; dy/2
  1178  c5cf 49ff               	EOR #$FF		; one's complement
  1179  c5d1 8595                       STA kl			; k = -dy/2 -1
  1180                          
  1181  c5d3 2047c2                     JSR ginit		; map in graphic memory
  1182                          
  1183  c5d6 a5a8                       LDA ydir
  1184  c5d8 d003                       BNE +
  1185  c5da 4c8bc3                     JMP line_down_steep	; y down, steep
  1186  c5dd 4caec2             +	JMP line_up_steep	; y up, steep
  1187                          
  1188                          line_flat
  1189  c5e0 a5a7                       LDA dxh
  1190  c5e2 a8                         TAY
  1191  c5e3 a6ab                       LDX dxl
  1192  c5e5 e8                         INX
  1193  c5e6 d001                       BNE +
  1194  c5e8 c8                         INY
  1195  c5e9 86a3               +	STX cl			; c = dx+1
  1196  c5eb 84a4                       STY ch
  1197                          
  1198  c5ed 4a                         LSR			; dx/2 high
  1199  c5ee 49ff               	EOR #$FF		; one's complement
  1200  c5f0 8596                       STA kh
  1201  c5f2 a5ab                       LDA dxl
  1202  c5f4 6a                         ROR			; dx/2 low
  1203  c5f5 49ff               	EOR #$FF		; one's complement
  1204  c5f7 8595                       STA kl			; k = -dx/2 - 1
  1205                          
  1206  c5f9 2047c2                     JSR ginit		; map in graphic memory
  1207                          
  1208  c5fc a5a8                       LDA ydir	
  1209  c5fe d003                       BNE +
  1210  c600 4c3ac3                     JMP line_down_flat	; y down, flat
  1211  c603 4ceac2             +	JMP line_up_flat	; y up, flat
  1212                          
  1213                          ;-----------------------------------------------------------------
  1214                          
  1215                          plot
  1216  c606 20cbc3                     JSR getxy		; get parameter
  1217  c609 859c                       STA xh			; save x/y
  1218  c60b 849b                       STY xl
  1219  c60d 86aa                       STX y
  1220  c60f 8d3d03                     STA savexh		; and store as cursor
  1221  c612 8c3c03                     STY savexl
  1222  c615 8e3e03                     STX savey
  1223                          
  1224                          plot_start
  1225  c618 2078c2                     JSR position		; calculate graphical address
  1226                          
  1227  c61b a501                       LDA prozport
  1228  c61d 29fd                       AND #%11111101		; Kernal ROM disable
  1229  c61f 78                         SEI			
  1230  c620 8501                       STA prozport
  1231                          
  1232  c622 20d303                     JSR gchange		; change graphical data
  1233                          
  1234  c625 a501                       LDA prozport
  1235  c627 0902                       ORA #%00000010		; kernal ROM enable
  1236  c629 8501                       STA prozport
  1237  c62b 58                         CLI
  1238  c62c 60                         RTS
  1239                          
  1240                          ;-----------------------------------------------------------------
  1241                          
  1242                          move
  1243  c62d 20cbc3                     JSR getxy		; get parameter
  1244  c630 8d3d03                     STA savexh		; just save as cursor
  1245  c633 8c3c03                     STY savexl
  1246  c636 8e3e03                     STX savey
  1247  c639 60                         RTS
  1248                          
  1249                          
  1250                          ;-----------------------------------------------------------------
  1251                          
  1252                          ; never touches X, Y, C-flag
  1253                          ; on exit: A corrupted, Z=0
  1254                          
  1255                          range_error
  1256  c63a ad3f03             	LDA savemo
  1257  c63d 29f0               	AND #$F0
  1258  c63f d003               	BNE +
  1259                          				; error mode 3: abort command (silent)
  1260  c641 68                 	PLA			; cleanup JSR
  1261  c642 68                 	PLA			; highbyte of return address >0
  1262                          
  1263  c643 60                 -	RTS			; error mode 5: back to command
  1264                          				; to handle value correction
  1265                          				; Z=0
  1266  c644 2920               +	AND #$20		; mode 5?
  1267  c646 d0fb               	BNE -			; exit with Z=0
  1268  c648 68                 	PLA			; error mode 4: terminate with error
  1269  c649 68                 	PLA			; cleanup JSR
  1270                          setmode_error
  1271  c64a 4c48b2             	JMP b_illquant		; throw error message
  1272                          
  1273                          ;-----------------------------------------------------------------
  1274                          
  1275                          setmode
  1276  c64d 209eb7                     JSR b_get8bit
  1277  c650 e003                       CPX #3
  1278  c652 9017                       BCC +			; less then 3, modification mode
  1279  c654 e006               	CPX #6
  1280  c656 b0f2               	BCS setmode_error	; out of range
  1281                          				; error mode
  1282  c658 8a                 	TXA
  1283  c659 e902               	SBC #2			; C=0, therefore -3
  1284  c65b 0a                 	ASL			; 0-2 -> 16,32 or 48
  1285  c65c 0a                 	ASL			; shift to upper nibble
  1286  c65d 0a                 	ASL
  1287  c65e 0a                 	ASL
  1288                          				; put A's bit 4-7 into savemo
  1289  c65f 4d3f03             	EOR savemo		; ********
  1290  c662 29f0               	AND #%11110000		; ****0000
  1291  c664 4d3f03             	EOR savemo		; AAAAmmmm
  1292  c667 8d3f03             	STA savemo		; 
  1293  c66a 60                 	RTS
  1294                          
  1295  c66b 8a                 +	TXA
  1296  c66c 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1297  c66f 290f               	AND #%00001111
  1298  c671 4d3f03             	EOR savemo
  1299  c674 8d3f03             	STA savemo
  1300                          setmode_enter
  1301  c677 e001               	CPX #$01
  1302  c679 b01a                       BCS set_or_toggle
  1303                          
  1304                          modereset
  1305  c67b a9c1                       LDA #>(nbitmask)
  1306  c67d 8ddd03                     STA gchange_op+2
  1307  c680 a96c                       LDA #<(nbitmask)
  1308  c682 8ddc03                     STA gchange_op+1
  1309  c685 a93d                       LDA #$3D		; opcode AND abs,X
  1310  c687 8ddb03                     STA gchange_op
  1311  c68a a931                       LDA #$31		; opcode AND (zp),Y
  1312  c68c 8df103                     STA gmask_op
  1313  c68f a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1314  c691 8df003                     STA gmask_flip+1
  1315  c694 60                         RTS
  1316                          
  1317                          set_or_toggle
  1318  c695 d01a                       BNE modetoggle
  1319                          modeset
  1320  c697 a9c1                       LDA #>(bitmask)
  1321  c699 8ddd03                     STA gchange_op+2
  1322  c69c a964                       LDA #<(bitmask)
  1323  c69e 8ddc03                     STA gchange_op+1
  1324  c6a1 a91d                       LDA #$1D		; opcode OR abs,X
  1325  c6a3 8ddb03                     STA gchange_op
  1326  c6a6 a911                       LDA #$11		; opcode OR (zp),Y
  1327  c6a8 8df103                     STA gmask_op
  1328  c6ab a900                       LDA #$00		; mask, EOR #$00, not inverting
  1329  c6ad 8df003                     STA gmask_flip+1
  1330  c6b0 60                         RTS
  1331                          
  1332                          modetoggle
  1333  c6b1 a9c1                       LDA #>(bitmask)
  1334  c6b3 8ddd03                     STA gchange_op+2
  1335  c6b6 a964                       LDA #<(bitmask)
  1336  c6b8 8ddc03                     STA gchange_op+1
  1337  c6bb a95d                       LDA #$5D		; opcode EOR abs,X
  1338  c6bd 8ddb03                     STA gchange_op
  1339  c6c0 a951                       LDA #$51		; opcode EOR (zp),Y
  1340  c6c2 8df103                     STA gmask_op
  1341  c6c5 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1342  c6c7 8df003                     STA gmask_flip+1
  1343  c6ca 60                         RTS
  1344                          
  1345                          
  1346                          ;-----------------------------------------------------------------
  1347                          ; get current x cursor position
  1348                          
  1349                          getposx
  1350  c6cb ac3c03             	LDY savexl
  1351  c6ce ad3d03             	LDA savexh
  1352  c6d1 2091b3             	JSR b_word2fac
  1353  c6d4 4c7300             	JMP chrget		; last position of expression (function name)
  1354                          
  1355                          ;-----------------------------------------------------------------
  1356                          ; get current y cursor position
  1357                          
  1358                          getposy
  1359  c6d7 ac3e03             	LDY savey
  1360  c6da 20a2b3             	JSR b_byte2fac
  1361  c6dd 4c7300             	JMP chrget		; last position of expression (function name)
  1362                          
  1363                          ;-----------------------------------------------------------------
  1364                          
  1365                          ; get pixel (check if pixel set)
  1366                          ; not used
  1367                          
  1368                          get
  1369  c6e0 207300             	JSR chrget		; advance past function name
  1370  c6e3 20faae             	JSR b_chkparl		; "("?
  1371  c6e6 20cbc3                     JSR getxy		; get X,Y values
  1372  c6e9 859c                       STA xh
  1373  c6eb 849b                       STY xl
  1374  c6ed 86aa                       STX y
  1375  c6ef 207900             	JSR chrgot
  1376  c6f2 20f7ae             	JSR b_chkparr		; ")"?
  1377                          	
  1378                          
  1379  c6f5 2078c2                     JSR position		; calculate graphic address/position
  1380                          
  1381  c6f8 a501                       LDA prozport
  1382  c6fa 29fd               	AND #%11111101		; Kernal ROM disable
  1383  c6fc 78                         SEI
  1384  c6fd 8501                       STA prozport
  1385                          
  1386  c6ff b1a5                       LDA (gaddr),Y
  1387  c701 3d64c1                     AND bitmask,X		; mask position
  1388  c704 a8                         TAY
  1389  c705 a501                       LDA prozport
  1390  c707 0902               	ORA #%00000010		; kernal ROM enable
  1391  c709 8501                       STA prozport
  1392  c70b 58                         CLI
  1393  c70c 98                 	TYA
  1394  c70d f002               	BEQ +
  1395  c70f a001               	LDY #1			; <> 0 -> always return 1
  1396  c711 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1397                          
  1398                          ;-----------------------------------------------------------------
  1399                          
  1400                          relto_cont
  1401                          				; continue
  1402  c714 207300             	JSR chrget		; skip TO token
  1403                          relto
  1404  c717 208aad                     JSR b_getval		; get X offset (+/-)
  1405  c71a a561               	LDA facexp		; FAC exponent
  1406  c71c c990               	CMP #$90		; more than 16 bit
  1407  c71e b031               	BCS relto_error		; illegal quantity
  1408  c720 209bbc                     JSR b_fac2int		; to signed integer
  1409                          
  1410  c723 18                         CLC
  1411  c724 a565                       LDA facintl
  1412  c726 6d3c03                     ADC savexl
  1413  c729 859e                       STA xendl
  1414  c72b a564                       LDA facinth
  1415  c72d 6d3d03                     ADC savexh
  1416  c730 859f                       STA xendh		; xend = savex+facint
  1417                          
  1418  c732 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1419  c735 208aad                     JSR b_getval
  1420  c738 a561                       LDA facexp		; FAC exponent
  1421  c73a c990                       CMP #$90		; more than 16 bit
  1422  c73c b013                       BCS relto_error		; illegal quantity
  1423  c73e 209bbc                     JSR b_fac2int		; to signed integer
  1424  c741 18                         CLC
  1425  c742 a565                       LDA facintl
  1426  c744 6d3e03                     ADC savey
  1427  c747 8593                       STA yend		; yend = savey+facint
  1428                          
  1429  c749 a59f                       LDA xendh		; check end coord. x
  1430  c74b c901                       CMP #>xmax
  1431  c74d 900e                       BCC rt_xok
  1432  c74f f003                       BEQ +
  1433                          relto_error
  1434  c751 203ac6                     JSR range_error
  1435  c754 a59e               +	LDA xendl
  1436  c756 c940                       CMP #<xmax
  1437  c758 9003                       BCC +
  1438  c75a 203ac6                     JSR range_error
  1439                          +
  1440                          rt_xok
  1441  c75d a593                       LDA yend		; check end coord. y
  1442  c75f c9c8                       CMP #ymax
  1443  c761 9003                       BCC +
  1444  c763 203ac6                     JSR range_error
  1445                          +
  1446  c766 ad3c03                     LDA savexl
  1447  c769 859b                       STA xl
  1448  c76b ad3d03                     LDA savexh
  1449  c76e 859c                       STA xh
  1450  c770 ad3e03                     LDA savey
  1451  c773 85aa                       STA y
  1452  c775 a49e                       LDY xendl
  1453  c777 a59f                       LDA xendh
  1454  c779 a693                       LDX yend		; xend/yend = cursor + x/y
  1455                          
  1456  c77b 2063c5                     JSR line_start		; draw line x/y to xend/yend
  1457                          
  1458  c77e 207900             	JSR chrgot
  1459  c781 d001               	BNE +
  1460  c783 60                 	RTS
  1461  c784 c9a4               +	CMP #t_to		; TO keyword?
  1462  c786 f08c               	BEQ relto_cont
  1463  c788 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1464                          
  1465                          ;-----------------------------------------------------------------
  1466                          
  1467                          char
  1468  c78b 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1469  c78e e028                       CPX #40	
  1470  c790 9003                       BCC +
  1471                          char_error
  1472  c792 4c48b2                     JMP b_illquant
  1473  c795 86fb               +	STX gpos		; save x coord.
  1474  c797 20f1b7                     JSR b_getcomma8bit
  1475                          				; get char. position y 0-24
  1476  c79a e019                       CPX #25
  1477  c79c b0f4                       BCS char_error
  1478  c79e 86fc                       STX gpos+1		; save y coord.
  1479                          
  1480  c7a0 20fdae                     JSR b_getcomma		; get string
  1481  c7a3 209ead                     JSR b_getexpr
  1482  c7a6 20a3b6                     JSR b_stringval		 ; string address in str
  1483  c7a9 48                         PHA			; string length
  1484  c7aa a6fc                       LDX gpos+1		; y coord. for char. position
  1485  c7ac 8a                         TXA
  1486  c7ad 2903                       AND #$03		; mask 2 bits
  1487  c7af a8                         TAY			; table index
  1488  c7b0 a900                       LDA #$00
  1489  c7b2 85fc                       STA gpos+1		; x high
  1490  c7b4 a5fb                       LDA gpos		; saved x: multiply by 8
  1491  c7b6 0a                         ASL
  1492  c7b7 0a                         ASL
  1493  c7b8 0a                         ASL
  1494  c7b9 26fc                       ROL gpos+1		; overflow to high byte
  1495  c7bb 7974c1                     ADC ytabl,Y
  1496  c7be 85a5                       STA gaddr
  1497  c7c0 a5fc                       LDA gpos+1		; x high
  1498  c7c2 7d78c1                     ADC ytabh,X
  1499  c7c5 85a6                       STA gaddr+1
  1500  c7c7 68                         PLA			; string length
  1501  c7c8 a000                       LDY #$00		; string index
  1502  c7ca aa                         TAX			; length
  1503  c7cb e8                         INX			; prepare as counter
  1504                          char_loop
  1505  c7cc ca                         DEX
  1506  c7cd f008                       BEQ char_exit
  1507  c7cf b122                       LDA (str),Y		; read string
  1508  c7d1 20d8c7                     JSR char_display
  1509  c7d4 c8                         INY
  1510  c7d5 d0f5                       BNE char_loop
  1511                          char_exit
  1512  c7d7 60                         RTS
  1513                          
  1514                          char_display
  1515  c7d8 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1516  c7da 8a                         TXA			; save register X+Y
  1517  c7db 48                         PHA
  1518  c7dc 98                         TYA
  1519  c7dd 48                         PHA
  1520  c7de a5d7                       LDA z_tmp		; get saved character
  1521  c7e0 3012                       BMI char_inverse
  1522                          
  1523                          char_normal
  1524  c7e2 c920                       CMP #$20		; control character?
  1525  c7e4 9054                       BCC char_disp_leave
  1526  c7e6 c960                       CMP #$60
  1527  c7e8 9004                       BCC +
  1528  c7ea 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1529  c7ec d014                       BNE char_hires
  1530  c7ee 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1531  c7f0 d010               	BNE char_hires
  1532  c7f2 f00e               	BEQ char_hires
  1533                          
  1534                          char_inverse
  1535  c7f4 297f                       AND #%01111111		; mask bit 7
  1536  c7f6 c97f                       CMP #%01111111		; was 255? (pi)
  1537  c7f8 d002                       BNE +
  1538  c7fa a95e                       LDA #$5E		; screen code for pi
  1539  c7fc c920               +	CMP #$20		; control character?
  1540  c7fe 903a                       BCC char_disp_leave
  1541                          				; yes, skip
  1542  c800 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1543                          				; $C0-$FF -> $40-$7F
  1544                          				; OPT: BNE char_hires
  1545                          				; OPT: char_normal
  1546                          char_hires
  1547  c802 a6c7                       LDX z_reverseflag
  1548  c804 f002                       BEQ +
  1549  c806 0980                       ORA #%10000000		; invert char.
  1550  c808 aa                 +	TAX			; save char. for later
  1551  c809 a501                       LDA prozport		; save prozport state
  1552  c80b 48                 	PHA
  1553  c80c a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1554  c80e 78                         SEI
  1555  c80f 8501                       STA prozport		; char. rom base = $D000
  1556  c811 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1557  c813 85fc                       STA gpos+1		; 
  1558  c815 8a                         TXA			; char. code
  1559  c816 0a                         ASL			; *8
  1560  c817 26fc                       ROL gpos+1
  1561  c819 0a                         ASL
  1562  c81a 26fc                       ROL gpos+1
  1563  c81c 0a                         ASL
  1564  c81d 26fc                       ROL gpos+1
  1565  c81f 85fb                       STA gpos		; addr. in char. rom for char.
  1566                          
  1567  c821 a007                       LDY #$07		; 8 hires lines
  1568                          char_line
  1569  c823 b1fb                       LDA (gpos),Y		; read character line
  1570  c825 20e703                     JSR gmask		; write to hires screen
  1571  c828 88                         DEY
  1572  c829 10f8                       BPL char_line
  1573                          
  1574  c82b 68                 	PLA
  1575  c82c 8501                       STA prozport
  1576  c82e 58                         CLI
  1577                          
  1578  c82f 18                         CLC			; step char position to left
  1579  c830 a5a5                       LDA gaddr		; ( +8 )
  1580  c832 6908                       ADC #$08
  1581  c834 85a5                       STA gaddr
  1582  c836 9002                       BCC +
  1583  c838 e6a6                       INC gaddr+1
  1584                          +
  1585                          char_disp_leave
  1586  c83a 68                 	PLA			; pass written character back
  1587  c83b a8                         TAY			; restore saved registers
  1588  c83c 68                         PLA
  1589  c83d aa                         TAX
  1590  c83e 60                 -       RTS
  1591                          
  1592                          
  1593                          ;-----------------------------------------------------------------
  1594                          
  1595                          to_cont
  1596                          				; continue
  1597  c83f 207300             	JSR chrget		; skip TO token
  1598                          to
  1599  c842 ad3c03                     LDA savexl
  1600  c845 859b                       STA xl
  1601  c847 ad3d03                     LDA savexh
  1602  c84a 859c                       STA xh
  1603  c84c ad3e03                     LDA savey
  1604  c84f 85aa                       STA y
  1605  c851 20cbc3                     JSR getxy
  1606  c854 2063c5                     JSR line_start
  1607  c857 207900             	JSR chrgot
  1608  c85a f0e2               	BEQ -
  1609  c85c c9a4               	CMP #t_to		; TO keyword?
  1610  c85e f0df               	BEQ to_cont
  1611  c860 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1612                          
  1613                          ;-----------------------------------------------------------------
  1614                          
  1615                          box
  1616  c863 20f4c3                     JSR para_hline_box
  1617  c866 9003               	BCC +
  1618  c868 203ac6             	JSR range_error
  1619                          				; XXX xend=xmax-1 ?
  1620                          +
  1621  c86b 20f1b7             	JSR b_getcomma8bit
  1622  c86e 8a                 	TXA			; optional 8-bit parameter
  1623                          				; height
  1624  c86f f00c               	BEQ +++			; 0 means 1, box is just a line
  1625  c871 18                 	CLC
  1626  c872 65aa               	ADC y			; end position for y coord.
  1627  c874 b004               	BCS +			; > 255
  1628  c876 c9c8               	CMP #ymax
  1629  c878 9003               	BCC +++
  1630                          +				; C=1 from ADC or CMP before
  1631  c87a 203ac6             	JSR range_error		; corrupts A
  1632                          				; XXX ycount=ymax-y-1 ?
  1633                          				; xend >= x
  1634  c87d 48                 +++	PHA			; yend
  1635  c87e a900               	LDA #0
  1636  c880 85a3               	STA ycount		; line thickness 1
  1637  c882 2057c4             	JSR hl_noxswap		; upper horizontal line
  1638                          
  1639                          				; right vertical line
  1640  c885 68                 	PLA			; if 0, heigth is 1
  1641  c886 d001               	BNE +			; no 
  1642  c888 60                 	RTS			; exit, if box is degenerated (line)
  1643  c889 a6aa               +	LDX y			; start point at higher values
  1644  c88b 85aa               	STA y
  1645  c88d 8693               	STX yend
  1646  c88f a59e               	LDA xendl
  1647  c891 859b               	STA xl
  1648  c893 a59f               	LDA xendh
  1649  c895 859c               	STA xh
  1650  c897 2028c5             	JSR vl_noyswap		; xend,yend -> xend,y
  1651                          				; lower horizontal line
  1652  c89a ad3c03             	LDA savexl
  1653  c89d 859b               	STA xl
  1654  c89f ad3d03             	LDA savexh
  1655  c8a2 859c               	STA xh			; xend already set
  1656  c8a4 2057c4             	JSR hl_noxswap		; x,yend -> xend,yend
  1657                          				; left vertical line
  1658  c8a7 4c28c5             	JMP vl_noyswap		; x,y -> x,xend
  1659                          
  1660                          ;-----------------------------------------------------------------
  1661                          
  1662                          fill
  1663  c8aa 20cbc3             	JSR getxy
  1664  c8ad 859c               	STA xh			; save x/y
  1665  c8af 849b               	STY xl
  1666  c8b1 86aa               	STX y
  1667  c8b3 8d3d03             	STA savexh		; and store as cursor
  1668  c8b6 8c3c03             	STY savexl
  1669  c8b9 8e3e03             	STX savey
  1670                                  
  1671  c8bc a531                       LDA basaryend		; initialize fill stack pointer
  1672  c8be 38                 	SEC
  1673  c8bf e903               	SBC #fesize		; one element below
  1674  c8c1 85fd               	STA fstack		; use space between basic arrays
  1675  c8c3 a532               	LDA basaryend+1		; and string heap bottom
  1676  c8c5 e900               	SBC #0			; take borrow
  1677  c8c7 85fe               	STA fstack+1
  1678                          
  1679  c8c9 2078c2             	JSR position		; graphic position in (gaddr)+Y, bit X
  1680  c8cc bd64c1             	LDA bitmask,X		; start pixel
  1681  c8cf 85a3               	STA tmpmask		; initial single pixel mask
  1682                          
  1683  c8d1 a59c               	LDA xh			; setup 8x8 block index (x8)
  1684  c8d3 4a                 	LSR			; high bit into C
  1685  c8d4 a59b               	LDA xl
  1686  c8d6 6a                 	ROR			; take high bit
  1687  c8d7 4a                 	LSR
  1688  c8d8 4a                 	LSR			; finally divide by 8
  1689  c8d9 85a7               	STA x8			; = index of 8x8 block in bitmap
  1690                          
  1691                          	; set fmode (from mode)
  1692  c8db ad3f03             	LDA savemo
  1693  c8de 2901               	AND #1			; mode = 0 -> invertmask: $FF
  1694  c8e0 38                 	SEC			; mode = 1 -> invertmask: $00
  1695  c8e1 e901               	SBC #1			; mode = 2 -> same as mode=0
  1696  c8e3 85a8               	STA fmode		; mode set or reset
  1697                          
  1698  c8e5 84a9               	STY ysave		; save y, will be destroyed
  1699  c8e7 2059ca             	JSR push_to_stack	; place dummy on stack (data ignored)
  1700  c8ea a4a9               	LDY ysave
  1701  c8ec 2047c2             	JSR ginit		; map in bitmap memory
  1702  c8ef 4cf3c9             	JMP try_stack		; process current position and 
  1703                          				; pull from stack, comes back to f_start
  1704                          				; if pixel is already set, it never returns.
  1705                          				; graphic data in tmpbits
  1706                          
  1707                          f_start				; start fill in the mid of a line ...
  1708  c8f2 a900               	LDA #0			; initialize continuation flag
  1709  c8f4 8596               	STA fcont		; for line above und below
  1710                          
  1711                          	; tmpmask will be extended to left and right to the borders
  1712                          
  1713                          	; set bits outside mask to 1
  1714  c8f6 a5a3               	LDA tmpmask		; 00011100
  1715  c8f8 49ff               	EOR #$ff		; 11100011
  1716  c8fa 0595               	ORA tmpbits		; 00101010 merge with graphic pixel data
  1717                          				; 11101011 pixel outside tmpmask now set! 
  1718  c8fc a2ff               	LDX #$ff		; pixel gap search: first one from left
  1719  c8fe e8                 -	INX
  1720  c8ff 0a                 	ASL			; counting from left
  1721  c900 b0fc               	BCS -			; loop if pixel is set
  1722                          				; bit number of the leftmost unset pixel in X
  1723  c902 a595               	LDA tmpbits		; 01000010 graphic pixel data
  1724                                                          ; extent bitmask to the right
  1725  c904 86ab               	STX xsave		; needed again later
  1726  c906 3d91c1             	AND maskleft,X		; 0000S111 clear left from starting point, to
  1727                          				; 00000010 -> X=6
  1728  c909 208cca             	JSR bitposright		; find the first set bit to right (border)
  1729  c90c bd9ac1             	LDA maskright0,X	; get a mask from the right border to left
  1730  c90f 85a3               	STA tmpmask		; 1111S100
  1731                          
  1732  c911 a6ab               	LDX xsave		; starting position
  1733  c913 a595               	LDA tmpbits		; 01000010 graphic pixel data
  1734  c915 3d9bc1             	AND maskright,X		; 11111000 clear right from starting point
  1735  c918 f015               	BEQ stepleft8		; open to left, continue left
  1736                          leftcont
  1737                          	; find first set bit from start to left (border)
  1738                          	; Get the pixel position of the first set pixel from the right.
  1739                          	; bit 76543210  -> mask 01234567
  1740                          	; input    index   maskleft0
  1741                          	; 00000000 -> 0 -> $FF 
  1742                          	; 10000000 -> 1 -> $7F
  1743                          	; X1000000 -> 2 -> $3F
  1744                          	; XX100000 -> 3 -> $1F
  1745                          	; XXX10000 -> 4 -> $0F
  1746                          	; XXXX1000 -> 5 -> $07
  1747                          	; XXXXX100 -> 6 -> $03
  1748                          	; XXXXXX10 -> 7 -> $01
  1749                          	; XXXXXXX1 -> 8 -> $00
  1750                          	; speed consideration: for results from X 0 to 4 it is faster than
  1751                          	; a table-driven approach.
  1752  c91a a200               	LDX #0
  1753  c91c c900               	CMP #0			; special case (no bit set at all)
  1754  c91e f004               	BEQ +
  1755  c920 e8                 -	INX
  1756  c921 0a                 	ASL			; shift to left
  1757  c922 d0fc               	BNE -			; until byte is empty
  1758                          +
  1759  c924 bd91c1             	LDA maskleft0,X		; get a mask from the left border to right
  1760                          				; 0011S111
  1761  c927 25a3               	AND tmpmask		; intersect with right mask 1111S100
  1762  c929 85a3               	STA tmpmask		; and store it for later 0011S100
  1763  c92b f01f               	BEQ next_block		; empty mask immediate continue to right
  1764  c92d d045               	BNE to_right		; start to walk and fill to the right border
  1765                          
  1766                          stepleft8
  1767  c92f a5a7               	LDA x8 			; 8x8 block position
  1768  c931 f041               	BEQ to_right		; =0, hit left screen border
  1769  c933 c6a7               	DEC x8			; count step 8x8 block to left
  1770  c935 a9ff               	LDA #$ff
  1771  c937 85a3               	STA tmpmask		; initial mask full pixel line
  1772                          
  1773  c939 38                 	SEC 			; graphic address to to next pixel line/block
  1774  c93a a5a5               	LDA gaddr
  1775  c93c e908               	SBC #8
  1776  c93e b002               	BCS +
  1777  c940 c6a6               	DEC gaddr+1
  1778  c942 85a5               +	STA gaddr
  1779                          
  1780                          	; y left unchanged
  1781  c944 b1a5               	LDA (gaddr),Y		; real graphic pixel data from bitmap
  1782  c946 45a8               	EOR fmode		; set/reset mode
  1783  c948 f0e5               	BEQ stepleft8		; step block left if empty
  1784  c94a d0ce               	BNE leftcont		; find left border
  1785                          	
  1786                          next_block
  1787  c94c e6a7               	INC x8			; step right a block
  1788  c94e a5a7               	LDA x8
  1789  c950 c928               	CMP #40			; beyond last horizontal block?
  1790  c952 b077               	BCS process_stack	; done if right screen border
  1791                          	; C = 0
  1792  c954 a5a5               	LDA gaddr		; advance to block right
  1793  c956 6908               	ADC #8			; gaddr = gaddr + 8
  1794  c958 85a5               	STA gaddr
  1795  c95a 9002               	BCC +
  1796  c95c e6a6               	INC gaddr+1
  1797  c95e a9ff               +	LDA #$ff		; asume "all pixels" mask
  1798  c960 85a3               	STA tmpmask
  1799  c962 b1a5               	LDA (gaddr),Y		; pixel data
  1800  c964 45a8               	EOR fmode		; set/reset mode
  1801  c966 f00c               	BEQ to_right		; empty -> finally to to_right
  1802  c968 208cca             	JSR bitposright	        ; search right border
  1803  c96b bd9ac1             	LDA maskright0,X	; mask out the right part
  1804  c96e 25a3               	AND tmpmask		; shorten mask accordingly
  1805  c970 85a3               	STA tmpmask
  1806  c972 f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1807                          				; leading to 0 mask (fill_check wont't
  1808                          				; handle this special case)
  1809                          
  1810                          				; continue to fill to right ...
  1811                          to_right			; fill loop towards right border
  1812  c974 a5a3               	LDA tmpmask		; fill mask
  1813                          				; assert:    (bitmap & tempmask) == 0
  1814                          				;         || (bitmap & tempmask) == tempmask
  1815  c976 51a5               	EOR (gaddr),Y		; set/reset to fill
  1816  c978 91a5               	STA (gaddr),Y		; into bitmap - the actual fill action!
  1817                          	
  1818                          check_above
  1819  c97a 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1820                          				; c = 0!
  1821  c97c 84a9               	STY ysave		; to be restored later
  1822  c97e a5a5               	LDA gaddr		; current graphic position
  1823  c980 a6a6               	LDX gaddr+1
  1824  c982 88                 	DEY			; line above
  1825  c983 100f               	BPL +			; leaving 8x8 block?
  1826                          	; c=0 (asl fcont)
  1827  c985 e93f               	SBC #$40-1		; block above:
  1828  c987 85fb               	STA caddr		; caddr = gaddr - $140
  1829  c989 8a                 	TXA
  1830  c98a e901               	SBC #$01
  1831  c98c aa                 	TAX
  1832  c98d c9e0               	CMP #>gram		; still graphic ram?
  1833  c98f 900a               	BCC skip_above
  1834  c991 a007               	LDY #7			; last line in block in new block
  1835  c993 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1836  c994 85fb               +	STA caddr		; still in same block
  1837  c996 86fc               ++	STX caddr+1		; shared store
  1838  c998 2028ca             	JSR fill_check
  1839                          skip_above
  1840                          
  1841                          check_below
  1842  c99b 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1843  c99d a5a5               	LDA gaddr		; current graphic position
  1844  c99f a6a6               	LDX gaddr+1
  1845  c9a1 a4a9               	LDY ysave		; restore original y position
  1846  c9a3 c8                 	INY			; line below
  1847  c9a4 c008               	CPY #8			; crossing 8x8 block?
  1848  c9a6 9014               	BCC +			; less then 8
  1849                          	; c=1 (cpy)
  1850  c9a8 693f               	ADC #$40-1		; block below: accu has gaddr
  1851  c9aa 85fb               	STA caddr		; caddr = gaddr + $140
  1852  c9ac a8                 	TAY			; for compare later
  1853  c9ad 8a                 	TXA			; gaddr high
  1854  c9ae 6901               	ADC #$01
  1855  c9b0 aa                 	TAX
  1856  c9b1 b010               	BCS skip_below		; > $10000  -> skip
  1857  c9b3 c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1858  c9b5 e9ff               	SBC #>(gram+8000)
  1859  c9b7 b00a               	BCS skip_below		; greater, so skip
  1860  c9b9 a000               	LDY #0			; first line in block
  1861  c9bb 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1862  c9bc 85fb               +	STA caddr		; transfer unchanged
  1863  c9be 86fc               ++	STX caddr+1		; shared store
  1864  c9c0 2028ca             	JSR fill_check
  1865                          skip_below
  1866                          
  1867  c9c3 a4a9               	LDY ysave		; restore original y position
  1868  c9c5 a5a3               	LDA tmpmask		; mask:
  1869  c9c7 2901               	AND #%00000001		; open to right, continue?
  1870  c9c9 d081               	BNE next_block		; to next block if open
  1871                          ; long branch version
  1872                          ;	BEQ process_stack	; not open, finished
  1873                          ;	JMP next_block		; to next block if open
  1874                          
  1875                          process_stack
  1876  c9cb a5fd               	LDA fstack		; stack empty?
  1877  c9cd c531               	CMP basaryend
  1878  c9cf a5fe               	LDA fstack+1
  1879  c9d1 e532               	SBC basaryend+1
  1880  c9d3 b003               	BCS +			; fstack >= basaryend -> not empty
  1881  c9d5 4c3fc2             	JMP gexit		; empty, we are finished
  1882                          
  1883                          	; top of stack: fetched multiple times until mask is completly filled!
  1884  c9d8 a002               +	LDY #fesize-1		; element's last component
  1885                          !ifndef opt_space {
  1886                          	LDA (fstack),Y
  1887                          	STA x8			; 8x8 block position
  1888                          	DEY
  1889                          }
  1890  c9da b1fd               	LDA (fstack),Y
  1891  c9dc 85a3               	STA tmpmask		; pixel mask
  1892  c9de 88                 	DEY
  1893  c9df b1fd               	LDA (fstack),Y
  1894  c9e1 85a6               	STA gaddr+1		; graphic addr high byte
  1895  c9e3 88                 	DEY
  1896  c9e4 b1fd               	LDA (fstack),Y		; graphic addr low byte combined with y-line
  1897  c9e6 aa                 	TAX			; needed twice
  1898  c9e7 29f8               	AND #%11111000		; split off address
  1899  c9e9 85a5               	STA gaddr
  1900                          !ifdef opt_space {
  1901  c9eb 0904               	ORA #%00000100		; end bit marker (if 0 all bits are shifted)
  1902  c9ed 85a7               	STA x8			; low byte without least significant 3 bits
  1903                          				; x8 temporary reused. Calculated later ...
  1904                          }
  1905  c9ef 8a                 	TXA
  1906  c9f0 2907               	AND #%00000111		; split off y-line
  1907  c9f2 a8                 	TAY
  1908                          try_stack
  1909  c9f3 b1a5               	LDA (gaddr),Y		; get pixels
  1910  c9f5 45a8               	EOR fmode		; according to set/reset
  1911  c9f7 8595               	STA tmpbits		; keep it for later
  1912  c9f9 25a3               	AND tmpmask		; focus on masked pixels
  1913  c9fb 08                 	PHP			; save Z flag
  1914  c9fc f004               	BEQ pop_stack		; all bits unset, remove from stack, because
  1915                          				; it could be filled in one step!
  1916  c9fe c5a3               	CMP tmpmask		; all gaps filled?
  1917  ca00 d00f               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1918                          	; all gaps filled, next on stack 
  1919                          pop_stack
  1920  ca02 38                 	SEC	
  1921  ca03 a5fd               	LDA fstack		; remove entry from stack
  1922  ca05 e903               	SBC #fesize		; entry size
  1923  ca07 85fd               	STA fstack
  1924  ca09 b002               	BCS +
  1925  ca0b c6fe               	DEC fstack+1
  1926  ca0d 28                 +	PLP			; all bits to fill empty?
  1927  ca0e d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1928                          				; all bits unset,
  1929  ca10 24                 	!by $24			; = bit $ll, skip next statement (1 byte)
  1930                          				; stack already cleaned up
  1931  ca11 28                 +++	PLP			; notstack cleanup
  1932                          
  1933                          !ifdef opt_space {
  1934                          	; Calculate the 8x8 block index from the the graphic address.
  1935                          	; Delayed, only if popped position is not already filled ...
  1936                          	; ((addr & 0x1fff) >> 3) % 40
  1937                          	; Takes 4 iterations. Register X, Y left untouched, 
  1938                          	; x8 contains gaddr low and has bit 2 set as end marker, bit 0, 1 is cleared.
  1939                          	; (312/8) % 40  -> 39
  1940                          	; 1 00111.000 : 101000
  1941  ca12 a5a6               	LDA gaddr+1		; divident high byte, mask out upper 3 bits
  1942  ca14 291f               	AND #$1f		; range 0 to 1f3f
  1943  ca16 06a7               	ASL x8			; $1f always < 40
  1944  ca18 2a                 -	ROL			; shift into high byte, carry from low byte
  1945  ca19 c928               	CMP #40			; modulo 40
  1946  ca1b 9002               	BCC +			; dividend less divisor
  1947  ca1d e928               	SBC #40			; greater or equal divisor, c=1
  1948                          				; nothing done to keep the quotient
  1949  ca1f 06a7               +	ASL x8			; shift low byte divident
  1950  ca21 d0f5               	BNE -			; if end-marker bit shifted out -> 0
  1951  ca23 85a7               	STA x8			; modulo in accu, stored to final location
  1952                          }
  1953                          
  1954  ca25 4cf2c8             	JMP f_start		; long (to far away) jump to fill line start
  1955                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1956                          
  1957                          
  1958                          ; Check upper or lower fill path
  1959                          ;		destroys x
  1960                          
  1961                          fill_check
  1962  ca28 b1fb               	LDA (caddr),Y
  1963  ca2a 45a8               	EOR fmode		; pixel data
  1964  ca2c aa                 	TAX			; save for later
  1965  ca2d 25a3               	AND tmpmask		; mask to fill
  1966  ca2f f015               	BEQ fc_cleared		; all masked pixels cleared?
  1967  ca31 c5a3               	CMP tmpmask		; check for gaps
  1968  ca33 f056               	BEQ fc_exit		; all gaps filled, finished
  1969                          				; if not so, some pixels still set
  1970  ca35 a5a3               	LDA tmpmask
  1971                          fc_checkstart			; no continuation, init flag based on
  1972                          				; rightmost pixel:
  1973  ca37 4a                 	LSR			; mask bit 0 to carry
  1974  ca38 9019               	BCC fc_nocont		; maskbit empty?
  1975  ca3a 8a                 	TXA			; pixel data
  1976  ca3b 4a                 	LSR			; pixel bit 0 to carry
  1977  ca3c b015               	BCS fc_nocont		; bit 0 set
  1978                          				; -> mask is 1 and pixel 0
  1979                          fc_cont
  1980  ca3e a596               	LDA fcont		; set flag for continuation
  1981  ca40 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1982  ca42 8596               	STA fcont
  1983  ca44 d013               	BNE push_to_stack	; always non zero
  1984                          
  1985                          fc_cleared
  1986  ca46 a5a3               	LDA tmpmask		; pixel & mask -> 0
  1987                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1988                          				; the caller asserts that this never happens
  1989  ca48 c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1990  ca4a d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1991                          				; 8 pixel line empty
  1992  ca4c a596               	LDA fcont		; continued gap?
  1993  ca4e 2902               	AND #%00000010		; check bit 2
  1994  ca50 f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1995  ca52 60                 	RTS			; gap continued and already on stack, leave
  1996                          
  1997                          fc_nocont
  1998  ca53 a596               	LDA fcont		; clear continuation flag
  1999  ca55 29fd               	AND #%11111101		; clear bit 2
  2000  ca57 8596               	STA fcont
  2001                          
  2002                          push_to_stack
  2003  ca59 18                 	CLC			; fstack points to top of stack
  2004  ca5a a5fd               	LDA fstack		; to next free stack element
  2005  ca5c 6903               	ADC #fesize		; entry size
  2006  ca5e 85fd               	STA fstack
  2007  ca60 9002               	BCC +
  2008  ca62 e6fe               	INC fstack+1
  2009                          +
  2010  ca64 a534               	LDA strbot+1		; check stack space
  2011  ca66 c5fe               	CMP fstack+1
  2012  ca68 b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  2013                          				; strbot MSB < fstack MSB
  2014                          out_of_memory			
  2015  ca6a 203fc2             	JSR gexit
  2016  ca6d a210               	LDX #$10		; out of memory error
  2017  ca6f 6c0003             	JMP (v_baserr)		; basic error handler
  2018  ca72 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  2019  ca74 a5fd               	LDA fstack		; MSB equal, check LSB
  2020  ca76 c533               	CMP strbot
  2021  ca78 b0f0               	BCS out_of_memory	; fstack collides with string heap!
  2022                          
  2023                          fc_put
  2024  ca7a 98                 	TYA			; y-line (value 0-7) merged with
  2025  ca7b 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  2026  ca7d a000               	LDY #0			; stack structure index, on next free element
  2027  ca7f 91fd               	STA (fstack),Y
  2028  ca81 c8                 	INY
  2029  ca82 a5fc               	LDA caddr+1
  2030  ca84 91fd               	STA (fstack),Y		; graphic address high
  2031  ca86 c8                 	INY
  2032  ca87 a5a3               	LDA tmpmask
  2033  ca89 91fd               	STA (fstack),Y
  2034                          !ifndef opt_space {
  2035                          	INY
  2036                          	LDA x8			; 8x8 block position
  2037                          	STA (fstack),Y
  2038                          }
  2039                          	
  2040  ca8b 60                 fc_exit	RTS
  2041                          	
  2042                          
  2043                          
  2044                          
  2045                          
  2046                          ; Get the pixel position of the first set pixel from the left.
  2047                          ; 76543210  bit ->
  2048                          ; XXXXXXXX
  2049                          ; 01234567  -> index
  2050                          
  2051                          ; 00000000 -> 8 -> $FF
  2052                          ; 00000001 -> 7 -> $FE
  2053                          ; 0000001X -> 6 -> $FC
  2054                          ; 000001XX -> 5 -> $F8
  2055                          ; 00001XXX -> 4 -> $F0
  2056                          ; 0001XXXX -> 3 -> $E0
  2057                          ; 001XXXXX -> 2 -> $C0
  2058                          ; 01XXXXXX -> 1 -> $80
  2059                          ; 1XXXXXXX -> 0 -> $00
  2060                          
  2061                          ; usage: lda maskright0,X
  2062                          
  2063                          ; speed consideration: for results of X from 4 to 8 it is faster than
  2064                          ; a table-driven approach.
  2065                          
  2066                          bitposright
  2067  ca8c a208               	LDX #8
  2068  ca8e c900               	CMP #0			; special case (no bit set at all)
  2069  ca90 f004               	BEQ +
  2070  ca92 ca                 -	DEX
  2071  ca93 4a                 	LSR			; shift to right
  2072  ca94 d0fc               	BNE -			; until byte is empty
  2073  ca96 60                 +	RTS
  2074                          
  2075                          ;-----------------------------------------------------------------
  2076                          
  2077                          unnew
  2078                          
  2079  ca97 a52b               	LDA bassta
  2080  ca99 8522               	STA str
  2081  ca9b a52c               	LDA bassta+1
  2082  ca9d 8523               	STA str+1
  2083  ca9f a001               	LDY #1
  2084  caa1 98                 	TYA
  2085  caa2 9122               	STA (str),y		; != 0
  2086                          
  2087  caa4 2033a5             	JSR b_rechain		; starting from bassta
  2088                          				; result in (str)
  2089  caa7 18                 	CLC			; str+1 -> new basic end
  2090  caa8 a423               	LDY str+1
  2091  caaa a522               	LDA str
  2092  caac 6902               	ADC #2
  2093  caae 852d               	STA basend
  2094  cab0 9001               	BCC +
  2095  cab2 c8                 	INY
  2096  cab3 842e               +	STY basend+1
  2097  cab5 4c60a6             	JMP b_clr		; perform CLR
  2098                          
  2099                          
  2100                          ;-----------------------------------------------------------------
  2101                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
