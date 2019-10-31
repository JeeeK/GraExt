
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
     6                          	!text "1.31" ; current version
     7                          }
     8                          ; revisions:
     9                          ;	2019-10-30 v 1.31
    10                          ;	2019-10-24 v 1.30
    11                          ;	2019-10-10 v 1.29
    12                          ;	2016-09-10 v 1.28
    13                          ;	2016-07-13 v 1.27
    14                          ;	2016-07-09 v 1.26
    15                          ;	2016-06-21 v 1.25
    16                          ;	2016-06-16 v 1.24
    17                          ;	2016-05-29 v 1.23
    18                          ;	2016-05-20 v 1.22
    19                          ;	2016-05-16 v 1.21
    20                          ;	2016-02-23 v 1.20
    21                          ;	2016-01-15 v 1.19
    22                          ;	1992-12-28 v 1.18
    23                          ;	1986-03-24 v 1.17
    24                          ;	1985       v 0.00 - 1.16
    25                          ;
    26                          ; the initial development is based on the implemention
    27                          ; done in a Forth environment written with a common 
    28                          ; 6502 forth assembler.
    29                          ; later, the code has been pulled out from there, relocated and 
    30                          ; enriched with some glue code to finally form the first 
    31                          ; basic extension.
    32                          
    33                          ; command dispatcher style JMP/RTS
    34                          ;	(if defined)
    35                          ;command_rts_style=1
    36                          
    37                          ; error handling 
    38                          ;	(if defined)
    39                          ;no_error=1
    40                          
    41                          ; basic interpreter registers, addresses and entry points
    42                          
    43                          type	= $0d
    44                          str     = $22		; string address
    45                          bassta	= $2b		; basic start pointer
    46                          basend	= $2d		; basic end pointer
    47                          basaryend	= $31		; basic end of array +1
    48                          strbot	= $33		; bottom of string heap 
    49                          ijmp    = $55		; address of JMP (addr)
    50                          chrget  = $73		; basic charget routine
    51                          chrgot  = $79		; basic last char got (charget routine)
    52                          txtptr	= $7A		; basic text pointer
    53                          facintl = $65		; integer result from b_fac2int
    54                          facinth = $64
    55                          facexp  = $61		; fac exponent, after b_getval
    56                          
    57                          z_reverseflag = $C7	; character routine
    58                          z_lastkey = $D7		; original use case, unused here
    59                          z_tmp = z_lastkey	; temporary reused for character routine
    60                          
    61                          v_baserr = $0300	; vector error routine
    62                          v_basstp = $0328	; vector error routine
    63                          v_bascmd = $0308	; vector interpreter parsing
    64                          v_basexp = $030a	; vector evaluate expression
    65                          
    66                          basic_rom = $A000	; start of BASIC ROM
    67                          
    68                          b_clr = $A660		; CLR command
    69                          b_interpreter = $A7AE	; interpreter loop
    70                          b_execstatement = $A7E7	; process statement (after chrget) - not used
    71                          b_execexpr =$AE92	; process expression - not used
    72                          b_getcomma = $AEFD	; read comma from basic text
    73                          b_illquant = $B248	; error "illegal quantity"
    74                          b_syntaxerror = $AF08	; error "syntax"
    75                          b_get8bit = $B79E	; read 8 bit numeric value from
    76                          			; basic text
    77                          b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
    78                          			; from basic text
    79                          b_getval = $AD8A	; read numeric value from basic text
    80                          b_getexpr = $AD9E	; read expression from basic text
    81                          b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
    82                          b_word2fac =$B391	; convert Y/Y to FAC (signed 16 bit)
    83                          b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
    84                          b_fac2int = $BC9B	; convert FAC to integer
    85                          b_stringval = $B6A3	; take epression as string $22/$23 (str)
    86                          b_rechain = $A533	; rechain basic lines
    87                          b_str2fac = $BCF3	; convert string in FAC (expression handling)
    88                          b_chkparl = $AEFA 	; check '('
    89                          b_chkparr = $AEF7 	; check ')'
    90                          
    91                          t_to = $A4		; keyword TO token
    92                          
    93                          ; hardware registers and values
    94                          
    95                          prozport = $01		; processor port
    96                          memrom = %00110111	; basic+kernal rom
    97                          membas = %00110110	; basic ram+kernal rom
    98                          memram = %00110101	; basic+kernal ram
    99                          
   100                          vic_cr	= $D011		; VIC control register
   101                          vic_mcr	= $D018		; VIC memory control register
   102                          cia_pra	= $DD00		; CIA 2 port register A
   103                          
   104                          cram	= $CC00		; start of color ram
   105                          
   106                          gram	= $e000		; start of graphic bitmap ram
   107                          gramp	= gram >> 8	; start page of bitmap
   108                          
   109                          ; constants 
   110                          
   111                          xmax	= 320		; max x dimension
   112                          ymax	= 200		; max y dimension
   113                          
   114                          ; zeropage variables
   115                          
   116                          x	= $9B		; start coordinate x, low+high
   117                          xl	= x
   118                          xh	= x+1
   119                          y	= $AA		; start coordinate y
   120                          
   121                          xendl	= $9E		; end coordinate x, low+high
   122                          xendh	= $9F
   123                          yend	= $93		; end coordinate y
   124                          
   125                          kl	= $95		; gradient for lines, low+high
   126                          kh	= kl+1
   127                          tmp1	= kl		; temp. var. (hline, vline, fill context)
   128                          tmp2	= kh		; temp. var. (hline, vline context)
   129                          fcont	= kh		; fill continuation flags (bit 1,0 for above, below)
   130                          
   131                          dxl	= $AB		; x delta, low+high
   132                          xsave	= dxl		; x register saved (hline, fill context)
   133                          dxh	= $A7
   134                          x8	= dxh		; 8x8 block index: (xh/xl) : 8 (fill context)
   135                          
   136                          dy	= $A9		; y delta
   137                          ysave	= dy		; y saved (hline context, fill context)
   138                          
   139                          ydir	= $A8		; y direction: 0 | !=0 ... down | up
   140                          ylimit	= ydir		; y limit in a 8x8 block (hline context)
   141                          fmode   = ydir		; mode mask: 0 | $FF (fill context)
   142                          
   143                          cl	= $A3		; dot count, low+high
   144                          ch	= $A4
   145                          ycount	= cl		; y count overall (hline context)
   146                          hcount	= ch		; horizontal blocks (hline context)
   147                          tmpmask	= cl		; temp. mask (fill context)
   148                          
   149                          gaddr	= $A5		; graphic address
   150                          
   151                          gpos	= $FB		; in graphic position
   152                          sgaddr	= gpos		; saved gaddr (hline context)
   153                          caddr	= gpos		; check gaddr (fill context)
   154                          
   155                          gcol	= $FD		; graphic color, in "graphic on" context only
   156                          fstack = gcol	; fill stack pointer (fill context)
   157                          
   158                          ; static ram areas
   159                          
   160                          savevpars = $0334	; original v_bascmd
   161                          saveverr = savevpars+2	; original v_baserr
   162                          savevstp = saveverr+2	; original v_basstp
   163                          savevexp = savevstp+2	; original v_basexp
   164                          savexl	= savevexp+2	; the graphic cursor: x low 
   165                          savexh	= savexl+1	; the graphic cursor: x high
   166                          savey	= savexh+1	; the graphic cursor: y
   167                          savemo	= savey+1	; the graphic mode
   168                          saveend = savemo+1	; byte after save area
   169                          
   170                          			; real place for gchange and gmask routines,
   171                          !ifdef ltc {
   172                          gramcode = $03ed - 26	; 15 bytes + 4*6+2
   173                          } else {
   174                          gramcode = $03ed	; 15 bytes
   175                          }
   176                          
   177                          ; LTC64 specifics
   178                          
   179                          !ifdef ltc {
   180                          
   181                          !cpu 65816
   182                          
   183                          bank4+3 = $040000
   184                          rombank+3 = $010000	; c't
   185                          
   186                          ; c't-Karte-Kontrollregister
   187                          
   188                          memconf = bank4 or 1
   189                          mc_off  = $80		; CPU 816 ausschalten
   190                          mc_slow = $40		; CPU 1 MHz
   191                          mc_epr  = $20		; EPROM in Bank0
   192                          mc_sim  = $10		; ROM-Simulation Bit
   193                          
   194                          }
   195                          
   196                          
   197                          
   198                          ;
   199                          ; initialize extension
   200                          
   201                          init
   202  c025 ad0803                     LDA v_bascmd		; check if hooks are already 
   203  c028 ae0903                     LDX v_bascmd+1		; in place 
   204  c02b c9b0               	CMP #<(parse)
   205  c02d d004               	BNE +
   206  c02f e0c0               	CPX #>(parse)
   207  c031 f052               	BEQ ++			; already hooked
   208                          
   209  c033 8d3403             +       STA savevpars		; save old vector
   210  c036 8e3503             	STX savevpars+1
   211  c039 a9b0               	LDA #<(parse)		; basic interpreter parser hook
   212  c03b 8d0803                     STA v_bascmd		; for commands
   213  c03e a9c0                       LDA #>(parse)
   214  c040 8d0903                     STA v_bascmd+1
   215                          
   216  c043 ad0a03                     LDA v_basexp		; basic interpreter parser hook
   217  c046 8d3a03             	STA savevexp		; for expressions
   218  c049 a9e4                       LDA #<(express)		; with save of old pointer
   219  c04b 8d0a03                     STA v_basexp
   220  c04e ad0b03                     LDA v_basexp+1
   221  c051 8d3b03             	STA savevexp+1
   222  c054 a9c0                       LDA #>(express)
   223  c056 8d0b03                     STA v_basexp+1
   224                          
   225  c059 ad2803                     LDA v_basstp
   226  c05c 8d3803             	STA savevstp
   227  c05f a99b                       LDA #<(stop)		; basic interpreter stop hook
   228  c061 8d2803                     STA v_basstp
   229  c064 ad2903                     LDA v_basstp+1
   230  c067 8d3903             	STA savevstp+1
   231  c06a a9c0                       LDA #>(stop)
   232  c06c 8d2903                     STA v_basstp+1
   233                          
   234  c06f ad0003                     LDA v_baserr
   235  c072 8d3603             	STA saveverr
   236  c075 a995                       LDA #<(error)		; basic interpreter error hook
   237  c077 8d0003                     STA v_baserr
   238  c07a ad0103                     LDA v_baserr+1
   239  c07d 8d3703             	STA saveverr+1
   240  c080 a9c0                       LDA #>(error)
   241  c082 8d0103                     STA v_baserr+1
   242                          
   243  c085 a200               ++	LDX #0			; set graphic cursor to (0,0)
   244  c087 8e3c03             	STX savexl
   245  c08a 8e3d03             	STX savexh
   246  c08d 8e3e03             	STX savey
   247  c090 e8                 	INX
   248  c091 8e3f03             	STX savemo		; set mode 1
   249  c094 60                         RTS
   250                          
   251                          error	
   252                          	; reg A may destroyed
   253  c095 20aac1             	JSR gra_off		; uses only reg A
   254  c098 6c3603             	JMP (saveverr)		; to original vector
   255                          
   256                          stop	
   257                          	; reg A may destroyed
   258  c09b a591               	LDA $91			; Scan code
   259  c09d c97f               	CMP #$7F		; STOP key?
   260  c09f d003               	BNE nostop
   261  c0a1 20aac1             	JSR gra_off		; uses only reg A
   262                          nostop
   263  c0a4 6c3803             	JMP (savevstp)		; to original vector
   264                          
   265                          
   266                          ;-----------------------------------------------------------------
   267                          
   268                          ; undo chrget
   269                          
   270                          undo_chrget
   271  c0a7 a57a               	LDA txtptr		; decrement text pointer by 1
   272  c0a9 d002               	BNE +
   273  c0ab c67b               	DEC txtptr+1
   274  c0ad c67a               +	DEC txtptr
   275  c0af 60                 	RTS
   276                          
   277                          ;-----------------------------------------------------------------
   278                          
   279                          ; start parsing an extension command ...
   280                          
   281                          parse
   282  c0b0 207300                     JSR chrget		; next char.
   283  c0b3 c926                       CMP #'&'		; command prefix
   284  c0b5 f006                       BEQ newcmd
   285  c0b7 20a7c0             	JSR undo_chrget
   286  c0ba 6c3403             	JMP (savevpars)
   287                          newcmd
   288  c0bd 207300                     JSR chrget		; command character
   289                          
   290  c0c0 a00e                       LDY #(cmdsend-cmds)	; map character to
   291                          				; command address ...
   292                          checknextcmd
   293  c0c2 88                         DEY
   294  c0c3 f01c               	BEQ parse_error
   295  c0c5 d912c1                     CMP cmds,Y
   296  c0c8 d0f8                       BNE checknextcmd	; try next
   297  c0ca 88                         DEY			; found
   298  c0cb 98                         TYA
   299  c0cc 0a                         ASL			; *2
   300  c0cd a8                         TAY
   301                          !ifndef command_rts_tyle {
   302                          	!set co=0		; command offset in jump table
   303  c0ce b921c1                     LDA cmdaddr+1,Y		; high byte from table
   304  c0d1 8556                       STA ijmp+1
   305  c0d3 b920c1                     LDA cmdaddr,Y		; low byte from table
   306  c0d6 8555                       STA ijmp
   307  c0d8 207300                     JSR chrget		; read next byte in basic text
   308  c0db 205400                     JSR ijmp-1		; go to command by JMP (addr)
   309  c0de 4caea7                     JMP b_interpreter	; continue parsing
   310                          } else {
   311                          	!set co=1		; command offset in jump table
   312                          	LDA #>(b_interpreter-1)	; return to interpreter
   313                          	PHA
   314                          	LDA #<(b_interpreter-1)
   315                          	PHA				
   316                                  LDA cmdaddr+1,Y		; command address (RTS style)
   317                                  PHA			; high byte on stack
   318                                  LDA cmdaddr,Y		; command address (RTS style)
   319                                  PHA			; low byte on stack
   320                                  JMP chrget		; read next byte in basic text 
   321                          				; and RTS to command
   322                          }
   323                          parse_error
   324  c0e1 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
   325                          
   326                          ;-----------------------------------------------------------------
   327                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   328                          express
   329  c0e4 a900               	LDA #0
   330  c0e6 850d               	STA type	
   331  c0e8 207300             	JSR chrget
   332  c0eb b003               	BCS exp_nonumber
   333  c0ed 4cf3bc             	JMP b_str2fac
   334                          exp_nonumber
   335  c0f0 c926                       CMP #'&'		; command prefix
   336  c0f2 f006                       BEQ newfunc
   337  c0f4 20a7c0             	JSR undo_chrget
   338  c0f7 6c3a03             	JMP (savevexp)		; original routine	
   339                          ;	JMP b_execexpr
   340                          newfunc
   341  c0fa 207300             	JSR chrget
   342  c0fd c95a               	CMP #'Z'
   343  c0ff d003               	BNE +
   344  c101 4cc6c6             	JMP get
   345  c104 c958               +	CMP #'X'
   346  c106 d003               	BNE +
   347  c108 4cb1c6             	JMP getposx
   348  c10b c959               +	CMP #'Y'
   349  c10d d0d2               	BNE parse_error
   350  c10f 4cbdc6             	JMP getposy
   351                          
   352                          ;-----------------------------------------------------------------
   353                          
   354                          ; the most commonly used command placed at the end ...
   355                          
   356  c112 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   357                          cmdsend
   358                          
   359                          cmdaddr
   360  c120 81ca90c8a3c171c7...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   361  c12c 49c8fdc628c8d8c4...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
   362                          
   363  c13a 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  c144 312e3331            !text "1.31" 

; ******** Source: graext-core.asm
   365  c148 20313938362c3230...	!text " 1986,2019 JOHANN@KLASEK.AT",0
   366                          
   367                          bitmask
   368  c164 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   369                          nbitmask
   370  c16c 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   371                          ytabl
   372  c174 004080c0           	!byte $00,$40,$80,$c0
   373                          ytabh
   374  c178 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   375  c17c e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   376  c180 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   377  c184 eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   378  c188 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   379  c18c f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   380  c190 fe                 	!byte gramp+$1e
   381                          
   382                          ; for horiz. line
   383                          
   384                          maskleft0
   385                          maskleft
   386  c191 ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   387  c199 00                 	!byte $00
   388                          
   389                          maskright0
   390  c19a 00                 	!byte $00
   391                          maskright
   392  c19b 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   393                          
   394                          ;-----------------------------------------------------------------
   395                          
   396                          graphic
   397  c1a3 209eb7                     JSR b_get8bit
   398  c1a6 e000                       CPX #$00
   399  c1a8 d013                       BNE gra_other
   400                          gra0				; &G 0
   401                          gra_off
   402  c1aa a9c7                       LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   403  c1ac 8d00dd                     STA cia_pra
   404  c1af a915                       LDA #((1 <<4) + (2 <<1) + 1)
   405                          				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   406                          				; char addr $1000/4096 = char. ROM
   407  c1b1 8d18d0                     STA vic_mcr		; VIC memory control
   408  c1b4 ad11d0                     LDA vic_cr		; VIC control register
   409  c1b7 29df                       AND #%11011111		; Hires mode off
   410  c1b9 8d11d0                     STA vic_cr
   411  c1bc 60                         RTS
   412                          
   413                          gra_other
   414  c1bd e001                       CPX #$01
   415  c1bf f00f               	BEQ gra1
   416  c1c1 e002               	CPX #$02
   417  c1c3 f00e                       BEQ gra2
   418  c1c5 e004               	CPX #$04
   419  c1c7 f043                       BEQ gra_clear		; &G 4 (erase only, leave mode)
   420  c1c9 e003               	CPX #$03		; &G 3 (graphic on)
   421  c1cb f029               	BEQ gra_on
   422  c1cd 4c48b2                     JMP b_illquant		; parameter illegal
   423                          	
   424                          gra1				; &G 1
   425  c1d0 200cc2             	JSR gra_clear
   426                          
   427                          gra2
   428  c1d3 20f1b7                     JSR b_getcomma8bit
   429  c1d6 8a                         TXA			; foreground color
   430  c1d7 0a                         ASL			; upper nibble
   431  c1d8 0a                         ASL
   432  c1d9 0a                         ASL
   433  c1da 0a                         ASL
   434  c1db 85fd                       STA gcol
   435  c1dd 20f1b7                     JSR b_getcomma8bit
   436  c1e0 8a                         TXA			; background color
   437  c1e1 290f                       AND #$0F
   438  c1e3 05fd                       ORA gcol
   439  c1e5 a000                       LDY #$00
   440                          cram_loop
   441  c1e7 9900cc                     STA cram,Y		; fill color RAM
   442  c1ea 9900cd                     STA cram+$100,Y
   443  c1ed 9900ce                     STA cram+$200,Y
   444  c1f0 99e8ce                     STA cram+$300-24,Y
   445  c1f3 c8                         INY
   446  c1f4 d0f1                       BNE cram_loop
   447                          
   448                          gra_on
   449  c1f6 202bc2             	JSR gra_setupcode
   450                          
   451  c1f9 a9c4                       LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   452  c1fb 8d00dd                     STA cia_pra
   453  c1fe a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   454  c200 8d18d0                     STA vic_mcr		; VIC memory control
   455  c203 ad11d0                     LDA vic_cr		; VIC control register
   456  c206 0920                       ORA #%00100000		; Bit 5 = 1: Hires on
   457  c208 8d11d0                     STA vic_cr
   458  c20b 60                         RTS
   459                          
   460                          gra_clear
   461  c20c a220                       LDX #$20		; Pages (8 KByte)
   462  c20e a9e0                       LDA #>gram
   463  c210 85fc                       STA gpos+1
   464  c212 a000                       LDY #$00
   465  c214 84fb                       STY gpos
   466  c216 98                         TYA
   467                          gra_fill
   468  c217 91fb                       STA (gpos),Y		; Loop unroll
   469  c219 c8                         INY
   470  c21a 91fb                       STA (gpos),Y
   471  c21c c8                         INY
   472  c21d 91fb                       STA (gpos),Y
   473  c21f c8                         INY
   474  c220 91fb                       STA (gpos),Y
   475  c222 c8                         INY
   476  c223 d0f2                       BNE gra_fill
   477  c225 e6fc                       INC gpos+1
   478  c227 ca                         DEX
   479  c228 d0ed                       BNE gra_fill
   480  c22a 60                 	RTS
   481                          
   482                          gra_setupcode
   483  c22b a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   484                          gra_copycode
   485  c22d bd4ec2             	LDA gromcode-1,X
   486  c230 9dec03             	STA gramcode-1,X
   487  c233 ca                 	DEX
   488  c234 d0f7               	BNE gra_copycode
   489  c236 ad3f03             	LDA savemo
   490  c239 290f               	AND #$0F
   491  c23b aa                 	TAX
   492  c23c 4c5dc6             	JMP setmode_enter	; re-apply mode to routines
   493                          				; implicit RTS
   494                          
   495                          ;-----------------------------------------------------------------
   496                          
   497                          gexit
   498  c23f a501                       LDA prozport
   499  c241 0902                       ORA #%00000010		; kernal ROM enable
   500  c243 8501                       STA prozport
   501  c245 58                         CLI			; allow interrupts
   502  c246 60                         RTS
   503                          
   504                          ;-----------------------------------------------------------------
   505                          
   506                          ginit
   507  c247 a501                       LDA prozport
   508  c249 29fd                       AND #%11111101		; Kernal ROM disable
   509  c24b 78                         SEI			; disable interrupts
   510  c24c 8501                       STA prozport
   511  c24e 60                         RTS
   512                          				; on exit Z=0
   513                          
   514                          ;-----------------------------------------------------------------
   515                          
   516                          ; These are selfmodified routines, which has to placed into RAM
   517                          ; (on every graphic "on")
   518                          ; Code gromcode to gromcode_end-1 is relocated to gramcode
   519                          
   520                          gromcode
   521                          
   522                          !pseudopc gramcode {
   523                          
   524                          ; change a graphic location
   525                          
   526                          gchange
   527                          !ifdef ltc {
   528                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   529                          	STA memconf		; damit internes RAM gelesen werden kann!
   530                          }
   531  c24f b1a5                       LDA (gaddr),Y
   532                          gchange_op
   533  c251 1d64c1                     ORA bitmask,X
   534  c254 91a5                       STA (gaddr),Y
   535                          !ifdef ltc {
   536                          	LDA #mc_sim		; vollständige ROM-Simulation
   537                          	STA memconf		; wieder schnelles RAM ab $C000
   538                          }
   539  c256 60                         RTS
   540                          
   541                          ; mask a graphic location 
   542                          
   543                          gmask
   544                          !ifdef ltc {
   545                          	XBA
   546                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   547                          	STA memconf		; damit internes RAM gelesen werden kann!
   548                          	XBA
   549                          }
   550                          gmask_flip
   551  c257 4900                       EOR #$00
   552                          gmask_op
   553  c259 11a5                       ORA (gaddr),Y
   554  c25b 91a5                       STA (gaddr),Y
   555                          !ifdef ltc {
   556                          	LDA #mc_sim		; vollständige ROM-Simulation
   557                          	STA memconf		; wieder schnelles RAM ab $C000
   558                          }
   559  c25d 60                         RTS
   560                          
   561                          }
   562                          
   563                          gromcode_end
   564                          
   565                          ;-----------------------------------------------------------------
   566                          
   567                          position
   568  c25e a5aa                       LDA y
   569  c260 4a                         LSR
   570  c261 4a                         LSR
   571  c262 4a                         LSR			; y/8
   572  c263 a8                         TAY
   573  c264 2903                       AND #%00000011		; (y/8) mod 4
   574  c266 aa                         TAX
   575  c267 a59b                       LDA xl			; x low
   576  c269 29f8                       AND #%11111000		; clear bit 2-0
   577  c26b 18                         CLC
   578  c26c 7d74c1                     ADC ytabl,X		; addr low: y base + x part
   579  c26f 85a5                       STA gaddr
   580  c271 a59c                       LDA xh			; addr high: x part
   581  c273 7978c1                     ADC ytabh,Y		; 	+ y base
   582  c276 85a6                       STA gaddr+1
   583  c278 a5aa                       LDA y			; vertical offset
   584  c27a 2907                       AND #%00000111		; y mod 8
   585  c27c a8                         TAY
   586  c27d a59b                       LDA xl
   587  c27f 2907                       AND #%00000111		; x mod 8
   588  c281 aa                         TAX			; horizonal offset
   589  c282 60                         RTS			; (bitmask)
   590                          
   591                          
   592                          ;-----------------------------------------------------------------
   593                          
   594                          ; swap tupel xl,xh <-> xendl,xendh
   595                          
   596                          swap_x_xend
   597  c283 a69e                       LDX xendl		; swap x, xend
   598  c285 a49b                       LDY xl
   599  c287 869b                       STX xl
   600  c289 849e                       STY xendl
   601                          
   602  c28b a69f                       LDX xendh
   603  c28d a49c                       LDY xh
   604  c28f 849f                       STY xendh
   605  c291 869c                       STX xh
   606  c293 60                 	RTS
   607                          
   608                          
   609                          ;-----------------------------------------------------------------
   610                          
   611                          ; line y up, x left, dx < dy (case 1)
   612                          
   613                          line_up_steep
   614  c294 205ec2                     JSR position		; x,y
   615                          loop_yup_xleft
   616  c297 20ed03                     JSR gchange		; pixel
   617                          
   618  c29a 18                         CLC			; k += dx
   619  c29b a595                       LDA kl
   620  c29d 65ab                       ADC dxl			; dxh is 0, because dx < dy
   621  c29f 8595                       STA kl
   622  c2a1 9014                       BCC +			; k >= 0 ->
   623                          
   624  c2a3 e5a9               ++	SBC dy			; k -= dy (C=1)
   625  c2a5 8595                       STA kl
   626                          
   627  c2a7 ca                  	DEX			; x--
   628  c2a8 100d                       BPL +
   629  c2aa a207                       LDX #7			; wrap around
   630  c2ac 38                 	SEC
   631  c2ad a5a5                       LDA gaddr		; x-8: gaddr -= 8
   632  c2af e908                       SBC #8
   633  c2b1 85a5                       STA gaddr
   634  c2b3 b002                       BCS +
   635  c2b5 c6a6                       DEC gaddr+1
   636                          
   637  c2b7 88                 +	DEY			; y--
   638  c2b8 100f                       BPL +++
   639  c2ba 38                         SEC			; y overflow
   640  c2bb a5a5                       LDA gaddr
   641  c2bd e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   642  c2bf 85a5                       STA gaddr
   643  c2c1 a5a6                       LDA gaddr+1
   644  c2c3 e901               	SBC #1
   645  c2c5 85a6                       STA gaddr+1
   646  c2c7 a007                       LDY #7			; wrap around
   647                          
   648  c2c9 c6a3               +++	DEC cl			; until c=0
   649  c2cb d0ca                       BNE loop_yup_xleft
   650  c2cd 4c3fc2                     JMP gexit
   651                          
   652                          
   653                          ;-----------------------------------------------------------------
   654                          
   655                          ; line x left, y up, dx > dy (case 2)
   656                          
   657                          line_up_flat
   658  c2d0 205ec2                     JSR position		; x,y
   659  c2d3 a5a3               	LDA cl			; counter adjustment for
   660  c2d5 f002               	BEQ +			; prepare for dec-dec-counting
   661  c2d7 e6a4               	INC ch
   662                          +
   663                          loop_xleft_yup
   664  c2d9 20ed03                     JSR gchange		; pixel
   665                          
   666  c2dc 18                         CLC			; k += dy
   667  c2dd a595                       LDA kl
   668  c2df 65a9                       ADC dy
   669  c2e1 8595                       STA kl
   670  c2e3 9020                       BCC +			; k < 0
   671  c2e5 e696                       INC kh
   672  c2e7 301c               	BMI +			; k < 0
   673                          
   674  c2e9 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   675  c2eb 8595                       STA kl
   676  c2ed a596                       LDA kh
   677  c2ef e5a7                       SBC dxh		
   678  c2f1 8596                       STA kh
   679                          
   680  c2f3 88                         DEY			; y--
   681  c2f4 100f                       BPL +
   682  c2f6 38                 	SEC			; C=1 not always true (SBC above)
   683  c2f7 a5a5                       LDA gaddr		; y overflow
   684  c2f9 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   685  c2fb 85a5                       STA gaddr
   686  c2fd a5a6                       LDA gaddr+1
   687  c2ff e901               	SBC #1
   688  c301 85a6                       STA gaddr+1
   689  c303 a007               	LDY #7			; wrap around
   690                          
   691  c305 ca                 +	DEX			; x--
   692  c306 100d                       BPL +++
   693  c308 a207                       LDX #7			; wrap around
   694  c30a 38                 	SEC
   695  c30b a5a5                       LDA gaddr		; x-8: gaddr -= 8
   696  c30d e908                       SBC #8
   697  c30f 85a5                       STA gaddr
   698  c311 b002                       BCS +++
   699  c313 c6a6                       DEC gaddr+1
   700                          +++
   701  c315 c6a3               	DEC cl			; c--
   702  c317 d0c0                       BNE loop_xleft_yup
   703  c319 c6a4                       DEC ch			; adjusted high which allows this
   704  c31b d0bc                       BNE loop_xleft_yup
   705                          
   706  c31d 4c3fc2                     JMP gexit
   707                          
   708                          
   709                          
   710                          ;-----------------------------------------------------------------
   711                          
   712                          ; line x left, y down, dx > dy (case 3)
   713                          
   714                          line_down_flat
   715  c320 205ec2                     JSR position		; x,y
   716  c323 a5a3               	LDA cl			; counter adjustment for
   717  c325 f002               	BEQ +			; prepare for dec-dec-counting
   718  c327 e6a4               	INC ch
   719                          +
   720                          loop_xleft_ydown
   721  c329 20ed03                     JSR gchange		; pixel
   722                          
   723  c32c 18                         CLC			; k += dy
   724  c32d a595                       LDA kl
   725  c32f 65a9                       ADC dy
   726  c331 8595                       STA kl
   727  c333 9021                       BCC +			; k < 0
   728  c335 e696                       INC kh
   729  c337 301d               	BMI +			; k < 0
   730                          
   731  c339 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   732  c33b 8595                       STA kl
   733  c33d a596                       LDA kh
   734  c33f e5a7                       SBC dxh		
   735  c341 8596                       STA kh
   736                          
   737  c343 c8                         INY			; y++
   738  c344 c008                       CPY #8
   739  c346 d00e                       BNE +
   740                          	; C=1
   741  c348 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   742  c34a 693f                       ADC #$40-1		; C already set by CPY
   743  c34c 85a5                       STA gaddr
   744  c34e a5a6                       LDA gaddr+1
   745  c350 6901               	ADC #1
   746  c352 85a6                       STA gaddr+1
   747  c354 a000                       LDY #0			; wrap around
   748                          
   749  c356 ca                 +	DEX			; x--
   750  c357 100d                       BPL +++
   751  c359 a207                       LDX #7			; wrap around
   752  c35b 38                 	SEC
   753  c35c a5a5                       LDA gaddr		; x-8: gaddr -= 8
   754  c35e e908                       SBC #8
   755  c360 85a5                       STA gaddr
   756  c362 b002                       BCS +++
   757  c364 c6a6                       DEC gaddr+1
   758                          +++
   759  c366 c6a3               	DEC cl			; c--
   760  c368 d0bf               	BNE loop_xleft_ydown
   761  c36a c6a4               	DEC ch			; adjusted high which allows this
   762  c36c d0bb                       BNE loop_xleft_ydown
   763                          
   764  c36e 4c3fc2                     JMP gexit
   765                          
   766                          
   767                          ;-----------------------------------------------------------------
   768                          
   769                          ; line y down, x right, dx < dy (case 4)
   770                          
   771                          line_down_steep
   772  c371 205ec2                     JSR position		; x,y
   773                          loop_ydown_xleft
   774  c374 20ed03                     JSR gchange		; pixel
   775                          
   776  c377 18                         CLC			; k += dx
   777  c378 a595                       LDA kl
   778  c37a 65ab                       ADC dxl			; dxh is 0, because dx < dy
   779  c37c 8595                       STA kl
   780  c37e 9014                       BCC +			; k >= 0 ->
   781                          
   782  c380 e5a9               	SBC dy			; k -= dy, C=1
   783  c382 8595                       STA kl
   784                          
   785  c384 ca                  	DEX			; x--
   786  c385 100d                       BPL +
   787  c387 a207                       LDX #7			; wrap around
   788  c389 38                 	SEC
   789  c38a a5a5                       LDA gaddr		; x-8: gaddr -= 8
   790  c38c e908                       SBC #8
   791  c38e 85a5                       STA gaddr
   792  c390 b002                       BCS +
   793  c392 c6a6                       DEC gaddr+1
   794                          
   795  c394 c8                 +	INY			; y++
   796  c395 c008                       CPY #8			; y overflow?
   797  c397 d00e                       BNE +++
   798  c399 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   799  c39b 693f                       ADC #$40-1		; C already set by CPY
   800  c39d 85a5                       STA gaddr
   801  c39f a5a6                       LDA gaddr+1
   802  c3a1 6901               	ADC #1
   803  c3a3 85a6                       STA gaddr+1
   804  c3a5 a000                       LDY #0			; wrap around
   805                          
   806  c3a7 c6a3               +++	DEC cl			; c--
   807                          				; until c=0
   808  c3a9 d0c9                       BNE loop_ydown_xleft
   809  c3ab 4c3fc2                     JMP gexit
   810                          
   811                          
   812                          ;-----------------------------------------------------------------
   813                          
   814                          getcommaxy
   815  c3ae 20fdae                     JSR b_getcomma		; check ","
   816                          getxy
   817  c3b1 208aad                     JSR b_getval		; get X coord. value
   818  c3b4 20f7b7                     JSR b_convint
   819  c3b7 c901                       CMP #>xmax
   820  c3b9 900c               	BCC gcxy_xok
   821  c3bb f003                       BEQ ++			; X = $1xx
   822  c3bd 2020c6                     JSR range_error
   823                          
   824  c3c0 c040               ++	CPY #<xmax		; check X low
   825  c3c2 9003                       BCC +
   826  c3c4 2020c6                     JSR range_error
   827                          +
   828                          gcxy_xok
   829  c3c7 84fb                       STY gpos		; temporary save X coord.
   830  c3c9 85fc                       STA gpos+1
   831                          
   832  c3cb 20f1b7                     JSR b_getcomma8bit
   833                          				; get Y coord. value
   834  c3ce e0c8                       CPX #ymax
   835  c3d0 9003                       BCC +
   836  c3d2 2020c6                     JSR range_error
   837                          +
   838  c3d5 a4fb                       LDY gpos		; restory X coord.
   839  c3d7 a5fc                       LDA gpos+1
   840  c3d9 60                         RTS
   841                          
   842                          
   843                          ;-----------------------------------------------------------------
   844                          
   845                          para_hline_box
   846  c3da 20b1c3                     JSR getxy		; get startpoint
   847  c3dd 86aa                       STX y
   848  c3df 8e3e03                     STX savey		; save as cursor, too
   849  c3e2 859c                       STA xh
   850  c3e4 849b                       STY xl
   851  c3e6 8d3d03             	STA savexh
   852  c3e9 8c3c03             	STY savexl
   853  c3ec 20fdae                     JSR b_getcomma		; get length
   854  c3ef 208aad                     JSR b_getval
   855  c3f2 20f7b7                     JSR b_convint
   856                          				; calculate end point
   857  c3f5 aa                         TAX			; save length high byte
   858  c3f6 98                         TYA			; length low byte
   859  c3f7 18                         CLC
   860  c3f8 659b                       ADC xl			; low xend = x+length
   861  c3fa 859e                       STA xendl
   862  c3fc a8                 	TAY
   863  c3fd 8a                         TXA			; high
   864  c3fe 659c                       ADC xh			; high xend = x+length
   865  c400 859f                       STA xendh
   866  c402 aa                 	TAX
   867                          
   868  c403 c901               	CMP #>xmax		; endpoint outside?
   869  c405 9005               	BCC +
   870  c407 d003               	BNE +			; >$200 (512)
   871  c409 98                 	TYA
   872  c40a e940               	SBC #<xmax
   873  c40c 60                 +	RTS			; C=1 out of range, C=0 ok
   874                          
   875                          ;-----------------------------------------------------------------
   876                          
   877                          hline
   878  c40d 20dac3             	JSR para_hline_box
   879  c410 9003               	BCC +
   880  c412 2020c6             	JSR range_error
   881                          				; XXX xend=xmax-1 ?
   882                          +
   883  c415 8e3d03                     STX savexh
   884  c418 8c3c03                     STY savexl		; also save as final cursor
   885                          
   886  c41b a900               	LDA #0			; default thickness 0 (means 1 pixel)
   887  c41d 85a3               	STA ycount
   888  c41f 207900             	JSR chrgot		; last char. again
   889  c422 f019               	BEQ +++			; command end? no optional param.
   890  c424 20f1b7             	JSR b_getcomma8bit
   891  c427 8a                 	TXA			; optional 8-bit parameter
   892  c428 85a3               	STA ycount		; hline thickness
   893  c42a f011               	BEQ +++			; 0 means 1 pixel
   894  c42c 18                 	CLC
   895  c42d 65aa               	ADC y			; end position for y coord.
   896  c42f b004               	BCS +			; > 255
   897  c431 c9c8               	CMP #ymax
   898  c433 9008               	BCC +++
   899                          +				; C=1 from ADC or CMP before
   900  c435 2020c6             	JSR range_error		; corrupts A
   901                          				; XXX ycount=ymax-y-1 ?
   902                          				; xend >= x
   903  c438 b003               	BCS hl_noxswap		; always
   904                          
   905                          hline_start
   906  c43a 2083c2             	JSR swap_x_xend		; xend < x, entry from line
   907                          	
   908                          hl_noxswap
   909                          				; xend > x
   910                          +++
   911  c43d e6a3               	INC ycount		; count to 0
   912  c43f 2047c2                     JSR ginit		; map in graphic memory
   913                          
   914  c442 205ec2                     JSR position		; graphic position x,y
   915                          
   916  c445 a5a5               	LDA gaddr		; save position for vertical
   917  c447 85fb               	STA sgaddr
   918  c449 a5a6               	LDA gaddr+1
   919  c44b 85fc               	STA sgaddr+1
   920  c44d 86ab               	STX xsave
   921  c44f 84a9               	STY ysave
   922                          
   923  c451 a59e                       LDA xendl
   924  c453 2907                       AND #%00000111
   925  c455 8596                       STA tmp2		; xend mod 8, mask index
   926  c457 a59b                       LDA xl
   927  c459 29f8                       AND #%11111000		; (xl div 8)*8
   928  c45b 8595                       STA tmp1
   929  c45d a59e                       LDA xendl		; xend unmasked
   930  c45f 38                         SEC
   931  c460 e595                       SBC tmp1		; finally: xend - (x div 8)*8 
   932  c462 8595                       STA tmp1
   933  c464 a59f                       LDA xendh
   934  c466 e59c                       SBC xh
   935  c468 4a                         LSR			; / 8 ->  0-39
   936  c469 a595                       LDA tmp1		; only 1 highest bit
   937  c46b 6a                         ROR			; and 3 lower bits
   938  c46c 4a                         LSR
   939  c46d 4a                         LSR
   940                                  			; 8-pixel-blocks count
   941  c46e 85a4               	STA hcount		; save for vertical extension
   942                           
   943                          hl_vertloop
   944  c470 98                 	TYA			; calculate max. Y in 8x8 block
   945  c471 18                 	CLC
   946  c472 65a3               	ADC ycount
   947  c474 c908               	CMP #8
   948  c476 9002               	BCC +
   949  c478 a908               	LDA #8
   950  c47a 85a8               +	STA ylimit
   951                          
   952  c47c bd91c1                     LDA maskleft,X		; starting mask
   953  c47f 8595               	STA tmp1
   954  c481 a6a4               	LDX hcount		; how many blocks
   955                          
   956                          hl_nextblock
   957  c483 ca                         DEX
   958                          hl_islastblock
   959  c484 301d                       BMI hl_lastblock
   960                          				; leave loop if X<0
   961  c486 a4a9               	LDY ysave
   962  c488 a595               -	LDA tmp1		; mask
   963  c48a 20f503             	JSR gmask		; first with left end mask
   964  c48d c8                 	INY			; vertical down
   965  c48e c4a8               	CPY ylimit		; in 8x8 box
   966  c490 d0f6               	BNE -
   967                          
   968  c492 18                         CLC			; gaddr += 8 (one block to right)
   969  c493 a5a5                       LDA gaddr
   970  c495 6908                       ADC #8
   971  c497 85a5                       STA gaddr
   972  c499 9002                       BCC +
   973  c49b e6a6                       INC gaddr+1
   974                          
   975  c49d a9ff               +	LDA #$FF		; following with full 8-pixel mask
   976  c49f 8595               	STA tmp1
   977  c4a1 d0e0               	BNE hl_nextblock	; always
   978                          
   979                          hl_lastblock
   980  c4a3 a696                       LDX tmp2		; xend mask index
   981  c4a5 3d9bc1                     AND maskright,X		; A has current maskt combine with mask right end
   982  c4a8 8595               	STA tmp1		; mask
   983  c4aa a4a9               	LDY ysave		; start position in 8x8 block
   984  c4ac a595               -	LDA tmp1		; mask
   985  c4ae 20f503             	JSR gmask		; modify
   986  c4b1 c8                 	INY			; vertical down
   987  c4b2 c6a3               	DEC ycount		; overall y counter
   988  c4b4 c4a8               	CPY ylimit
   989  c4b6 d0f4               	BNE -
   990                          
   991  c4b8 a5a3               	LDA ycount		; finished
   992  c4ba d003               	BNE +			; roll-over into 8x8 block below
   993  c4bc 4c3fc2                     JMP gexit		; leave
   994                          
   995  c4bf 18                 +	CLC
   996  c4c0 a5fb               	LDA sgaddr
   997  c4c2 6940               	ADC #$40		; next 8-pixel row below
   998  c4c4 85fb               	STA sgaddr		; + $140 (320)
   999  c4c6 85a5               	STA gaddr
  1000  c4c8 a5fc               	LDA sgaddr+1
  1001  c4ca 6901               	ADC #$01
  1002  c4cc 85fc               	STA sgaddr+1
  1003  c4ce 85a6               	STA gaddr+1
  1004  c4d0 a6ab               	LDX xsave		; initial mask index
  1005  c4d2 a000               	LDY #0			; start on top of 8x8
  1006  c4d4 84a9               	STY ysave
  1007  c4d6 f098               	BEQ hl_vertloop
  1008                          ;-----------------------------------------------------------------
  1009                          
  1010                          vline
  1011  c4d8 20b1c3                     JSR getxy		; get startpoint
  1012  c4db 859c                       STA xh
  1013  c4dd 8d3d03                     STA savexh		; save as cursor too
  1014  c4e0 849b                       STY xl
  1015  c4e2 8c3c03                     STY savexl
  1016  c4e5 8693                       STX yend		; initial point is endpoint
  1017                          
  1018  c4e7 20f1b7                     JSR b_getcomma8bit
  1019                          				; get length
  1020  c4ea 18                         CLC			; calculate end point
  1021  c4eb 8a                         TXA			; length
  1022                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1023                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1024                          ;	STA tmp1
  1025  c4ec 6593                       ADC yend		; length + initial point is startpoint
  1026  c4ee b005               	BCS vline_iq		; > 255
  1027  c4f0 c9c8                       CMP #ymax		; outside?
  1028  c4f2 a8                 	TAY			; keep startpoint
  1029  c4f3 9003                       BCC +
  1030                          vline_iq
  1031  c4f5 2020c6                     JSR range_error		; corrupts A
  1032                          				; XXX Y = ymax-1 ?
  1033  c4f8 84aa               +	STY y			; startpoint
  1034  c4fa 8c3e03             	STY savey		; set cursor y position
  1035  c4fd 18                 	CLC
  1036  c4fe 900e               	BCC +++			; skip following, because y, yend are already ordered
  1037                          
  1038                          vline_start			; entry point from line command (only)
  1039  c500 a5aa               	LDA y			; order of y, yend is not defined
  1040  c502 c593               	CMP yend
  1041  c504 b008               	BCS vl_noyswap		; yend > y ->
  1042  c506 a5aa               	LDA y			; swap y, yend
  1043  c508 a693               	LDX yend
  1044  c50a 8593               	STA yend
  1045  c50c 86aa               	STX y
  1046                          vl_noyswap
  1047                          				; startpoint is below the endpoint
  1048  c50e 2047c2             +++	JSR ginit		; map in graphic memory
  1049                          
  1050                          vl_start
  1051  c511 205ec2                     JSR position		; graphic position x,y
  1052  c514 bd64c1                     LDA bitmask,X
  1053  c517 8596                       STA tmp2		; save mask
  1054                          ; DON'T-CHANGE: replace ...
  1055  c519 38                         SEC
  1056  c51a a5aa                       LDA y			; startpoint is greater!
  1057  c51c e593                       SBC yend		; vertical length
  1058  c51e aa                         TAX
  1059                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1060                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1061                          ;	LDX tmp1
  1062  c51f e8                         INX			; +1 (exit on 0)
  1063  c520 38                 	SEC			; for subtraction, never changed!
  1064                          vl_nextline
  1065  c521 a596                       LDA tmp2
  1066  c523 20f503                     JSR gmask		; modify 
  1067  c526 88                         DEY			; go up
  1068  c527 100e                       BPL +
  1069  c529 a5a5                       LDA gaddr		; C=1
  1070  c52b e940               	SBC #$40		; gaddr -= 320
  1071  c52d 85a5                       STA gaddr
  1072  c52f a5a6                       LDA gaddr+1
  1073  c531 e901                       SBC #$01
  1074  c533 85a6                       STA gaddr+1
  1075  c535 a007                       LDY #7			; wrap y offset
  1076  c537 ca                 +	DEX			; all vertical positions done?
  1077  c538 d0e7                       BNE vl_nextline
  1078  c53a 4c3fc2                     JMP gexit		; leave
  1079                          
  1080                          
  1081                          ;-----------------------------------------------------------------
  1082                          
  1083                          line
  1084  c53d 20b1c3                     JSR getxy		; get startpoint
  1085  c540 849b                       STY xl 
  1086  c542 859c                       STA xh
  1087  c544 86aa                       STX y
  1088                          
  1089  c546 20aec3                     JSR getcommaxy		; get endpoint
  1090                          line_start
  1091  c549 8c3c03                     STY savexl		; save as cursor position too
  1092  c54c 849e                       STY xendl
  1093  c54e 8d3d03                     STA savexh
  1094  c551 859f                       STA xendh
  1095  c553 8e3e03                     STX savey
  1096  c556 8693                       STX yend
  1097                          
  1098  c558 a000                       LDY #$00		; initialize to 0
  1099  c55a 84a8                       STY ydir
  1100  c55c 8495                       STY kl
  1101  c55e 8496                       STY kh
  1102                          
  1103  c560 38                         SEC
  1104  c561 a59b                       LDA xl			; calculate dx
  1105  c563 e59e                       SBC xendl
  1106  c565 85ab                       STA dxl
  1107  c567 a59c                       LDA xh
  1108  c569 e59f                       SBC xendh
  1109  c56b 85a7                       STA dxh
  1110                          
  1111  c56d b018                       BCS li_xend_left
  1112                          	; dx != 0
  1113                          				; negate dx:
  1114  c56f 98                         TYA			; Y=A=0
  1115  c570 38                         SEC			; dx = 0 - dx
  1116  c571 e5ab                       SBC dxl
  1117  c573 85ab                       STA dxl
  1118  c575 98                         TYA			; Y=A=0
  1119  c576 e5a7                       SBC dxh
  1120  c578 85a7                       STA dxh
  1121                          				; C=0 always, needed later
  1122  c57a 2083c2             	jsr swap_x_xend
  1123  c57d a6aa                       LDX y			; swap y
  1124  c57f a493                       LDY yend
  1125  c581 8693                       STX yend
  1126  c583 84aa                       STY y
  1127                          
  1128  c585 9007                       BCC li_x_different
  1129                          				; C=0 always (from negation before)
  1130                          
  1131                          li_xend_left
  1132                                  			; A already contains dxh
  1133  c587 05ab                       ORA dxl			; dx = 0?
  1134  c589 d003                       BNE li_x_different
  1135  c58b 4c00c5                     JMP vline_start		; vertical line case
  1136                          
  1137                          li_x_different
  1138  c58e 38                         SEC			; calculate dy
  1139  c58f a593                       LDA yend
  1140  c591 e5aa                       SBC y
  1141  c593 b006                       BCS li_y_right		; yend >= y?
  1142  c595 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1143  c597 6901                       ADC #$01		; C=0
  1144  c599 85a8                       STA ydir		; always not 0: flag y goes up
  1145                          
  1146                          li_y_right
  1147  c59b 85a9                       STA dy
  1148  c59d d007                       BNE +
  1149  c59f a900               	LDA #0			; line thickness = 1
  1150  c5a1 85a3               	STA ycount
  1151  c5a3 4c3ac4                     JMP hline_start		; horizontal line case
  1152                          +
  1153                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1154                          
  1155  c5a6 a5a7                       LDA dxh			; dx > dy
  1156  c5a8 d01c                       BNE line_flat		; yes -> flat
  1157  c5aa a5a9                       LDA dy			; no -> steep
  1158  c5ac aa                         TAX
  1159  c5ad c5ab                       CMP dxl
  1160  c5af 9015                       BCC line_flat
  1161                          
  1162                          line_steep
  1163  c5b1 e8                         INX	
  1164  c5b2 86a3                       STX cl			; c = dy+1
  1165  c5b4 4a                         LSR			; dy/2
  1166  c5b5 49ff               	EOR #$FF		; one's complement
  1167  c5b7 8595                       STA kl			; k = -dy/2 -1
  1168                          
  1169  c5b9 2047c2                     JSR ginit		; map in graphic memory
  1170                          
  1171  c5bc a5a8                       LDA ydir
  1172  c5be d003                       BNE +
  1173  c5c0 4c71c3                     JMP line_down_steep	; y down, steep
  1174  c5c3 4c94c2             +	JMP line_up_steep	; y up, steep
  1175                          
  1176                          line_flat
  1177  c5c6 a5a7                       LDA dxh
  1178  c5c8 a8                         TAY
  1179  c5c9 a6ab                       LDX dxl
  1180  c5cb e8                         INX
  1181  c5cc d001                       BNE +
  1182  c5ce c8                         INY
  1183  c5cf 86a3               +	STX cl			; c = dx+1
  1184  c5d1 84a4                       STY ch
  1185                          
  1186  c5d3 4a                         LSR			; dx/2 high
  1187  c5d4 49ff               	EOR #$FF		; one's complement
  1188  c5d6 8596                       STA kh
  1189  c5d8 a5ab                       LDA dxl
  1190  c5da 6a                         ROR			; dx/2 low
  1191  c5db 49ff               	EOR #$FF		; one's complement
  1192  c5dd 8595                       STA kl			; k = -dx/2 - 1
  1193                          
  1194  c5df 2047c2                     JSR ginit		; map in graphic memory
  1195                          
  1196  c5e2 a5a8                       LDA ydir	
  1197  c5e4 d003                       BNE +
  1198  c5e6 4c20c3                     JMP line_down_flat	; y down, flat
  1199  c5e9 4cd0c2             +	JMP line_up_flat	; y up, flat
  1200                          
  1201                          ;-----------------------------------------------------------------
  1202                          
  1203                          plot
  1204  c5ec 20b1c3                     JSR getxy		; get parameter
  1205  c5ef 859c                       STA xh			; save x/y
  1206  c5f1 849b                       STY xl
  1207  c5f3 86aa                       STX y
  1208  c5f5 8d3d03                     STA savexh		; and store as cursor
  1209  c5f8 8c3c03                     STY savexl
  1210  c5fb 8e3e03                     STX savey
  1211                          
  1212                          plot_start
  1213  c5fe 205ec2                     JSR position		; calculate graphical address
  1214                          
  1215  c601 a501                       LDA prozport
  1216  c603 29fd                       AND #%11111101		; Kernal ROM disable
  1217  c605 78                         SEI			
  1218  c606 8501                       STA prozport
  1219                          
  1220  c608 20ed03                     JSR gchange		; change graphical data
  1221                          
  1222  c60b a501                       LDA prozport
  1223  c60d 0902                       ORA #%00000010		; kernal ROM enable
  1224  c60f 8501                       STA prozport
  1225  c611 58                         CLI
  1226  c612 60                         RTS
  1227                          
  1228                          ;-----------------------------------------------------------------
  1229                          
  1230                          move
  1231  c613 20b1c3                     JSR getxy		; get parameter
  1232  c616 8d3d03                     STA savexh		; just save as cursor
  1233  c619 8c3c03                     STY savexl
  1234  c61c 8e3e03                     STX savey
  1235  c61f 60                         RTS
  1236                          
  1237                          
  1238                          ;-----------------------------------------------------------------
  1239                          
  1240                          ; never touches X, Y, C-flag
  1241                          ; on exit: A corrupted, Z=0
  1242                          
  1243                          range_error
  1244  c620 ad3f03             	LDA savemo
  1245  c623 29f0               	AND #$F0
  1246  c625 d003               	BNE +
  1247                          				; error mode 3: abort command (silent)
  1248  c627 68                 	PLA			; cleanup JSR
  1249  c628 68                 	PLA			; highbyte of return address >0
  1250                          
  1251  c629 60                 -	RTS			; error mode 5: back to command
  1252                          				; to handle value correction
  1253                          				; Z=0
  1254  c62a 2920               +	AND #$20		; mode 5?
  1255  c62c d0fb               	BNE -			; exit with Z=0
  1256  c62e 68                 	PLA			; error mode 4: terminate with error
  1257  c62f 68                 	PLA			; cleanup JSR
  1258                          setmode_error
  1259  c630 4c48b2             	JMP b_illquant		; throw error message
  1260                          
  1261                          ;-----------------------------------------------------------------
  1262                          
  1263                          setmode
  1264  c633 209eb7                     JSR b_get8bit
  1265  c636 e003                       CPX #3
  1266  c638 9017                       BCC +			; less then 3, modification mode
  1267  c63a e006               	CPX #6
  1268  c63c b0f2               	BCS setmode_error	; out of range
  1269                          				; error mode
  1270  c63e 8a                 	TXA
  1271  c63f e902               	SBC #2			; C=0, therefore -3
  1272  c641 0a                 	ASL			; 0-2 -> 16,32 or 48
  1273  c642 0a                 	ASL			; shift to upper nibble
  1274  c643 0a                 	ASL
  1275  c644 0a                 	ASL
  1276                          				; put A's bit 4-7 into savemo
  1277  c645 4d3f03             	EOR savemo		; ********
  1278  c648 29f0               	AND #%11110000		; ****0000
  1279  c64a 4d3f03             	EOR savemo		; AAAAmmmm
  1280  c64d 8d3f03             	STA savemo		; 
  1281  c650 60                 	RTS
  1282                          
  1283  c651 8a                 +	TXA
  1284  c652 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1285  c655 290f               	AND #%00001111
  1286  c657 4d3f03             	EOR savemo
  1287  c65a 8d3f03             	STA savemo
  1288                          setmode_enter
  1289  c65d e001               	CPX #$01
  1290  c65f b01a                       BCS set_or_toggle
  1291                          
  1292                          modereset
  1293  c661 a9c1                       LDA #>(nbitmask)
  1294  c663 8df103                     STA gchange_op+2
  1295  c666 a96c                       LDA #<(nbitmask)
  1296  c668 8df003                     STA gchange_op+1
  1297  c66b a93d                       LDA #$3D		; opcode AND abs,X
  1298  c66d 8def03                     STA gchange_op
  1299  c670 a931                       LDA #$31		; opcode AND (zp),Y
  1300  c672 8df703                     STA gmask_op
  1301  c675 a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1302  c677 8df603                     STA gmask_flip+1
  1303  c67a 60                         RTS
  1304                          
  1305                          set_or_toggle
  1306  c67b d01a                       BNE modetoggle
  1307                          modeset
  1308  c67d a9c1                       LDA #>(bitmask)
  1309  c67f 8df103                     STA gchange_op+2
  1310  c682 a964                       LDA #<(bitmask)
  1311  c684 8df003                     STA gchange_op+1
  1312  c687 a91d                       LDA #$1D		; opcode OR abs,X
  1313  c689 8def03                     STA gchange_op
  1314  c68c a911                       LDA #$11		; opcode OR (zp),Y
  1315  c68e 8df703                     STA gmask_op
  1316  c691 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1317  c693 8df603                     STA gmask_flip+1
  1318  c696 60                         RTS
  1319                          
  1320                          modetoggle
  1321  c697 a9c1                       LDA #>(bitmask)
  1322  c699 8df103                     STA gchange_op+2
  1323  c69c a964                       LDA #<(bitmask)
  1324  c69e 8df003                     STA gchange_op+1
  1325  c6a1 a95d                       LDA #$5D		; opcode EOR abs,X
  1326  c6a3 8def03                     STA gchange_op
  1327  c6a6 a951                       LDA #$51		; opcode EOR (zp),Y
  1328  c6a8 8df703                     STA gmask_op
  1329  c6ab a900                       LDA #$00		; mask, EOR #$00, not inverting
  1330  c6ad 8df603                     STA gmask_flip+1
  1331  c6b0 60                         RTS
  1332                          
  1333                          
  1334                          ;-----------------------------------------------------------------
  1335                          ; get current x cursor position
  1336                          
  1337                          getposx
  1338  c6b1 ac3c03             	LDY savexl
  1339  c6b4 ad3d03             	LDA savexh
  1340  c6b7 2091b3             	JSR b_word2fac
  1341  c6ba 4c7300             	JMP chrget		; last position of expression (function name)
  1342                          
  1343                          ;-----------------------------------------------------------------
  1344                          ; get current y cursor position
  1345                          
  1346                          getposy
  1347  c6bd ac3e03             	LDY savey
  1348  c6c0 20a2b3             	JSR b_byte2fac
  1349  c6c3 4c7300             	JMP chrget		; last position of expression (function name)
  1350                          
  1351                          ;-----------------------------------------------------------------
  1352                          
  1353                          ; get pixel (check if pixel set)
  1354                          ; not used
  1355                          
  1356                          get
  1357  c6c6 207300             	JSR chrget		; advance past function name
  1358  c6c9 20faae             	JSR b_chkparl		; "("?
  1359  c6cc 20b1c3                     JSR getxy		; get X,Y values
  1360  c6cf 859c                       STA xh
  1361  c6d1 849b                       STY xl
  1362  c6d3 86aa                       STX y
  1363  c6d5 207900             	JSR chrgot
  1364  c6d8 20f7ae             	JSR b_chkparr		; ")"?
  1365                          	
  1366                          
  1367  c6db 205ec2                     JSR position		; calculate graphic address/position
  1368                          
  1369  c6de a501                       LDA prozport
  1370  c6e0 29fd               	AND #%11111101		; Kernal ROM disable
  1371  c6e2 78                         SEI
  1372  c6e3 8501                       STA prozport
  1373                          
  1374  c6e5 b1a5                       LDA (gaddr),Y
  1375  c6e7 3d64c1                     AND bitmask,X		; mask position
  1376  c6ea a8                         TAY
  1377  c6eb a501                       LDA prozport
  1378  c6ed 0902               	ORA #%00000010		; kernal ROM enable
  1379  c6ef 8501                       STA prozport
  1380  c6f1 58                         CLI
  1381  c6f2 98                 	TYA
  1382  c6f3 f002               	BEQ +
  1383  c6f5 a001               	LDY #1			; <> 0 -> always return 1
  1384  c6f7 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1385                          
  1386                          ;-----------------------------------------------------------------
  1387                          
  1388                          relto_cont
  1389                          				; continue
  1390  c6fa 207300             	JSR chrget		; skip TO token
  1391                          relto
  1392  c6fd 208aad                     JSR b_getval		; get X offset (+/-)
  1393  c700 a561               	LDA facexp		; FAC exponent
  1394  c702 c990               	CMP #$90		; more than 16 bit
  1395  c704 b031               	BCS relto_error		; illegal quantity
  1396  c706 209bbc                     JSR b_fac2int		; to signed integer
  1397                          
  1398  c709 18                         CLC
  1399  c70a a565                       LDA facintl
  1400  c70c 6d3c03                     ADC savexl
  1401  c70f 859e                       STA xendl
  1402  c711 a564                       LDA facinth
  1403  c713 6d3d03                     ADC savexh
  1404  c716 859f                       STA xendh		; xend = savex+facint
  1405                          
  1406  c718 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1407  c71b 208aad                     JSR b_getval
  1408  c71e a561                       LDA facexp		; FAC exponent
  1409  c720 c990                       CMP #$90		; more than 16 bit
  1410  c722 b013                       BCS relto_error		; illegal quantity
  1411  c724 209bbc                     JSR b_fac2int		; to signed integer
  1412  c727 18                         CLC
  1413  c728 a565                       LDA facintl
  1414  c72a 6d3e03                     ADC savey
  1415  c72d 8593                       STA yend		; yend = savey+facint
  1416                          
  1417  c72f a59f                       LDA xendh		; check end coord. x
  1418  c731 c901                       CMP #>xmax
  1419  c733 900e                       BCC rt_xok
  1420  c735 f003                       BEQ +
  1421                          relto_error
  1422  c737 2020c6                     JSR range_error
  1423  c73a a59e               +	LDA xendl
  1424  c73c c940                       CMP #<xmax
  1425  c73e 9003                       BCC +
  1426  c740 2020c6                     JSR range_error
  1427                          +
  1428                          rt_xok
  1429  c743 a593                       LDA yend		; check end coord. y
  1430  c745 c9c8                       CMP #ymax
  1431  c747 9003                       BCC +
  1432  c749 2020c6                     JSR range_error
  1433                          +
  1434  c74c ad3c03                     LDA savexl
  1435  c74f 859b                       STA xl
  1436  c751 ad3d03                     LDA savexh
  1437  c754 859c                       STA xh
  1438  c756 ad3e03                     LDA savey
  1439  c759 85aa                       STA y
  1440  c75b a49e                       LDY xendl
  1441  c75d a59f                       LDA xendh
  1442  c75f a693                       LDX yend		; xend/yend = cursor + x/y
  1443                          
  1444  c761 2049c5                     JSR line_start		; draw line x/y to xend/yend
  1445                          
  1446  c764 207900             	JSR chrgot
  1447  c767 d001               	BNE +
  1448  c769 60                 	RTS
  1449  c76a c9a4               +	CMP #t_to		; TO keyword?
  1450  c76c f08c               	BEQ relto_cont
  1451  c76e 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1452                          
  1453                          ;-----------------------------------------------------------------
  1454                          
  1455                          char
  1456  c771 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1457  c774 e028                       CPX #40	
  1458  c776 9003                       BCC +
  1459                          char_error
  1460  c778 4c48b2                     JMP b_illquant
  1461  c77b 86fb               +	STX gpos		; save x coord.
  1462  c77d 20f1b7                     JSR b_getcomma8bit
  1463                          				; get char. position y 0-24
  1464  c780 e019                       CPX #25
  1465  c782 b0f4                       BCS char_error
  1466  c784 86fc                       STX gpos+1		; save y coord.
  1467                          
  1468  c786 20fdae                     JSR b_getcomma		; get string
  1469  c789 209ead                     JSR b_getexpr
  1470  c78c 20a3b6                     JSR b_stringval		 ; string address in str
  1471  c78f 48                         PHA			; string length
  1472  c790 a6fc                       LDX gpos+1		; y coord. for char. position
  1473  c792 8a                         TXA
  1474  c793 2903                       AND #$03		; mask 2 bits
  1475  c795 a8                         TAY			; table index
  1476  c796 a900                       LDA #$00
  1477  c798 85fc                       STA gpos+1		; x high
  1478  c79a a5fb                       LDA gpos		; saved x: multiply by 8
  1479  c79c 0a                         ASL
  1480  c79d 0a                         ASL
  1481  c79e 0a                         ASL
  1482  c79f 26fc                       ROL gpos+1		; overflow to high byte
  1483  c7a1 7974c1                     ADC ytabl,Y
  1484  c7a4 85a5                       STA gaddr
  1485  c7a6 a5fc                       LDA gpos+1		; x high
  1486  c7a8 7d78c1                     ADC ytabh,X
  1487  c7ab 85a6                       STA gaddr+1
  1488  c7ad 68                         PLA			; string length
  1489  c7ae a000                       LDY #$00		; string index
  1490  c7b0 aa                         TAX			; length
  1491  c7b1 e8                         INX			; prepare as counter
  1492                          char_loop
  1493  c7b2 ca                         DEX
  1494  c7b3 f008                       BEQ char_exit
  1495  c7b5 b122                       LDA (str),Y		; read string
  1496  c7b7 20bec7                     JSR char_display
  1497  c7ba c8                         INY
  1498  c7bb d0f5                       BNE char_loop
  1499                          char_exit
  1500  c7bd 60                         RTS
  1501                          
  1502                          char_display
  1503  c7be 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1504  c7c0 8a                         TXA			; save register X+Y
  1505  c7c1 48                         PHA
  1506  c7c2 98                         TYA
  1507  c7c3 48                         PHA
  1508  c7c4 a5d7                       LDA z_tmp		; get saved character
  1509  c7c6 3012                       BMI char_inverse
  1510                          
  1511                          char_normal
  1512  c7c8 c920                       CMP #$20		; control character?
  1513  c7ca 9054                       BCC char_disp_leave
  1514  c7cc c960                       CMP #$60
  1515  c7ce 9004                       BCC +
  1516  c7d0 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1517  c7d2 d014                       BNE char_hires
  1518  c7d4 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1519  c7d6 d010               	BNE char_hires
  1520  c7d8 f00e               	BEQ char_hires
  1521                          
  1522                          char_inverse
  1523  c7da 297f                       AND #%01111111		; mask bit 7
  1524  c7dc c97f                       CMP #%01111111		; was 255? (pi)
  1525  c7de d002                       BNE +
  1526  c7e0 a95e                       LDA #$5E		; screen code for pi
  1527  c7e2 c920               +	CMP #$20		; control character?
  1528  c7e4 903a                       BCC char_disp_leave
  1529                          				; yes, skip
  1530  c7e6 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1531                          				; $C0-$FF -> $40-$7F
  1532                          				; OPT: BNE char_hires
  1533                          				; OPT: char_normal
  1534                          char_hires
  1535  c7e8 a6c7                       LDX z_reverseflag
  1536  c7ea f002                       BEQ +
  1537  c7ec 0980                       ORA #%10000000		; invert char.
  1538  c7ee aa                 +	TAX			; save char. for later
  1539  c7ef a501                       LDA prozport		; save prozport state
  1540  c7f1 48                 	PHA
  1541  c7f2 a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1542  c7f4 78                         SEI
  1543  c7f5 8501                       STA prozport		; char. rom base = $D000
  1544  c7f7 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1545  c7f9 85fc                       STA gpos+1		; 
  1546  c7fb 8a                         TXA			; char. code
  1547  c7fc 0a                         ASL			; *8
  1548  c7fd 26fc                       ROL gpos+1
  1549  c7ff 0a                         ASL
  1550  c800 26fc                       ROL gpos+1
  1551  c802 0a                         ASL
  1552  c803 26fc                       ROL gpos+1
  1553  c805 85fb                       STA gpos		; addr. in char. rom for char.
  1554                          
  1555  c807 a007                       LDY #$07		; 8 hires lines
  1556                          char_line
  1557  c809 b1fb                       LDA (gpos),Y		; read character line
  1558  c80b 20f503                     JSR gmask		; write to hires screen
  1559  c80e 88                         DEY
  1560  c80f 10f8                       BPL char_line
  1561                          
  1562  c811 68                 	PLA
  1563  c812 8501                       STA prozport
  1564  c814 58                         CLI
  1565                          
  1566  c815 18                         CLC			; step char position to left
  1567  c816 a5a5                       LDA gaddr		; ( +8 )
  1568  c818 6908                       ADC #$08
  1569  c81a 85a5                       STA gaddr
  1570  c81c 9002                       BCC +
  1571  c81e e6a6                       INC gaddr+1
  1572                          +
  1573                          char_disp_leave
  1574  c820 68                 	PLA			; pass written character back
  1575  c821 a8                         TAY			; restore saved registers
  1576  c822 68                         PLA
  1577  c823 aa                         TAX
  1578  c824 60                 -       RTS
  1579                          
  1580                          
  1581                          ;-----------------------------------------------------------------
  1582                          
  1583                          to_cont
  1584                          				; continue
  1585  c825 207300             	JSR chrget		; skip TO token
  1586                          to
  1587  c828 ad3c03                     LDA savexl
  1588  c82b 859b                       STA xl
  1589  c82d ad3d03                     LDA savexh
  1590  c830 859c                       STA xh
  1591  c832 ad3e03                     LDA savey
  1592  c835 85aa                       STA y
  1593  c837 20b1c3                     JSR getxy
  1594  c83a 2049c5                     JSR line_start
  1595  c83d 207900             	JSR chrgot
  1596  c840 f0e2               	BEQ -
  1597  c842 c9a4               	CMP #t_to		; TO keyword?
  1598  c844 f0df               	BEQ to_cont
  1599  c846 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1600                          
  1601                          ;-----------------------------------------------------------------
  1602                          
  1603                          box
  1604  c849 20dac3                     JSR para_hline_box
  1605  c84c 9003               	BCC +
  1606  c84e 2020c6             	JSR range_error
  1607                          				; XXX xend=xmax-1 ?
  1608                          +
  1609  c851 20f1b7             	JSR b_getcomma8bit
  1610  c854 8a                 	TXA			; optional 8-bit parameter
  1611                          				; height
  1612  c855 f00c               	BEQ +++			; 0 means 1, box is just a line
  1613  c857 18                 	CLC
  1614  c858 65aa               	ADC y			; end position for y coord.
  1615  c85a b004               	BCS +			; > 255
  1616  c85c c9c8               	CMP #ymax
  1617  c85e 9003               	BCC +++
  1618                          +				; C=1 from ADC or CMP before
  1619  c860 2020c6             	JSR range_error		; corrupts A
  1620                          				; XXX ycount=ymax-y-1 ?
  1621                          				; xend >= x
  1622  c863 48                 +++	PHA			; yend
  1623  c864 a900               	LDA #0
  1624  c866 85a3               	STA ycount		; line thickness 1
  1625  c868 203dc4             	JSR hl_noxswap		; upper horizontal line
  1626                          
  1627                          				; right vertical line
  1628  c86b 68                 	PLA			; if 0, heigth is 1
  1629  c86c d001               	BNE +			; no 
  1630  c86e 60                 	RTS			; exit, if box is degenerated (line)
  1631  c86f a6aa               +	LDX y			; start point at higher values
  1632  c871 85aa               	STA y
  1633  c873 8693               	STX yend
  1634  c875 a59e               	LDA xendl
  1635  c877 859b               	STA xl
  1636  c879 a59f               	LDA xendh
  1637  c87b 859c               	STA xh
  1638  c87d 200ec5             	JSR vl_noyswap		; xend,yend -> xend,y
  1639                          				; lower horizontal line
  1640  c880 ad3c03             	LDA savexl
  1641  c883 859b               	STA xl
  1642  c885 ad3d03             	LDA savexh
  1643  c888 859c               	STA xh			; xend already set
  1644  c88a 203dc4             	JSR hl_noxswap		; x,yend -> xend,yend
  1645                          				; left vertical line
  1646  c88d 4c0ec5             	JMP vl_noyswap		; x,y -> x,xend
  1647                          
  1648                          ;-----------------------------------------------------------------
  1649                          
  1650                          fill
  1651  c890 20b1c3                     JSR getxy
  1652  c893 859c                       STA xh			; save x/y
  1653  c895 849b                       STY xl
  1654  c897 86aa                       STX y
  1655  c899 8d3d03                     STA savexh		; and store as cursor
  1656  c89c 8c3c03                     STY savexl
  1657  c89f 8e3e03                     STX savey
  1658                                  
  1659  c8a2 a531                       LDA basaryend		; initialize fill stack pointer
  1660  c8a4 38                 	SEC
  1661  c8a5 e904               	SBC #4			; one element below
  1662  c8a7 85fd                       STA fstack		; use space between basic arrays
  1663  c8a9 a532                       LDA basaryend+1		; and string heap bottom
  1664  c8ab e900               	SBC #0			; take borrow
  1665  c8ad 85fe                       STA fstack+1
  1666                          
  1667  c8af 205ec2             	JSR position		; graphic position in (gaddr)+Y, bit X
  1668                          
  1669  c8b2 a59c               	LDA xh			; setup 8x8 block index (x8)
  1670  c8b4 4a                 	LSR			; high bit into C
  1671  c8b5 a59b               	LDA xl
  1672  c8b7 2a                 	ROL			; take high bit
  1673  c8b8 4a                 	LSR
  1674  c8b9 4a                 	LSR			; finally divide by 8
  1675  c8ba 85a7               	STA x8			; = index of 8x8 block in bitmap
  1676                          
  1677                          	; set fmode (from mode)
  1678  c8bc ad3f03             	LDA savemo
  1679  c8bf 2903               	AND #3
  1680  c8c1 aa                 	TAX
  1681  c8c2 ca                 	DEX
  1682  c8c3 3003               	BMI +			; mode = 0 -> invertmask: $FF
  1683  c8c5 f001               	BEQ +			; mode = 1 -> invertmask: $00
  1684  c8c7 ca                 	DEX			; mode = 2 -> ? (same as mode=0)
  1685  c8c8 86a8               +	STX fmode		; mode set or reset
  1686                          
  1687  c8ca 2047c2             	JSR ginit		; map in bitmap memory
  1688                          
  1689  c8cd b1a5               	LDA (gaddr),y		; graphic position in Y (in index in 8x8 block)
  1690  c8cf 45a8               	EOR fmode
  1691  c8d1 8595               	STA tmp1		; bitmap, for later usage
  1692                          
  1693  c8d3 3d64c1             	AND bitmask,x		; test start pixel
  1694  c8d6 f003               	BEQ +			; not set
  1695                          f_exit
  1696  c8d8 4c3fc2             	JMP gexit		; leave if start pixel is already set
  1697                          +
  1698                          f_start				; the start: in mid of a line to fill ...
  1699  c8db a900               	LDA #0
  1700  c8dd 8596               	STA fcont		; initialize continuation flag for line above und below
  1701                          
  1702  c8df a595               	LDA tmp1		; graphic pixel data
  1703                          				; extent bitmask to the right
  1704  c8e1 86ab               	STX xsave
  1705  c8e3 3d91c1             	AND maskleft,x		; mask out left part, bits right from starting point remain
  1706  c8e6 2076ca             	JSR bitposr		; find the first set bit from start to right (border)
  1707  c8e9 bd9ac1             	LDA maskright0,x	; get a mask from the right border to left
  1708  c8ec 85a3               	STA tmpmask		
  1709                          
  1710                          leftcont
  1711  c8ee a595               	LDA tmp1		; graphic pixel data
  1712  c8f0 a6ab               	LDX xsave
  1713                          leftcont_a
  1714  c8f2 3d9bc1             	AND maskright,x		; mask out right part, bits left from starting point remain
  1715  c8f5 f00e               	BEQ stepleft8		; no left border in this pixel line
  1716  c8f7 206aca             	JSR bitposl		; find the first set bit from start to left (border)
  1717  c8fa bd91c1             	LDA maskleft0,x		; get a mask from the left border to right
  1718  c8fd 25a3               	AND tmpmask		; intersect masks
  1719  c8ff 85a3               	STA tmpmask		; and store it for later
  1720  c901 f021               	BEQ next_block		; empty mask immediate continue to right
  1721  c903 d047               	BNE to_right		; start to walk and fill to the right border
  1722                          
  1723                          stepleft8
  1724  c905 a5a7               	LDA x8 			; 8x8 block position
  1725  c907 f043               	BEQ to_right		; =0, hit screen border
  1726  c909 c6a7               	DEC x8			; count step 8x8 block to left
  1727  c90b a9ff               	LDA #$ff
  1728  c90d 85a3               	STA tmpmask		; initial mask full pixel line
  1729                          
  1730  c90f 38                 	SEC 			; graphic address to to next pixel line/block
  1731  c910 a5a5               	LDA gaddr
  1732  c912 e908               	SBC #8
  1733  c914 b002               	BCS +
  1734  c916 c6a6               	DEC gaddr+1
  1735  c918 85a5               +	STA gaddr
  1736                          
  1737                          	; y left unchanged
  1738  c91a b1a5               	LDA (gaddr),y		; real graphic pixel data from bitmap
  1739  c91c 45a8               	EOR fmode		; set/reset mode
  1740  c91e 8595               	STA tmp1		; graphic pixel data
  1741  c920 a207               	LDX #7			; start bit 0 (index 7, rightmost)
  1742  c922 d0ce               	BNE leftcont_a		; loop to left border search
  1743                          	
  1744                          next_block
  1745  c924 e6a7               	INC x8			; step right a block
  1746  c926 a5a7               	LDA x8
  1747  c928 c928               	CMP #40			; beyond last horizontal block?
  1748  c92a b077               	BCS process_stack	; done if right screen border
  1749                          	; C = 0
  1750  c92c a5a5               	LDA gaddr		; advance to block right
  1751  c92e 6908               	ADC #8			; gaddr = gaddr + 8
  1752  c930 85a5               	STA gaddr
  1753  c932 9002               	BCC +
  1754  c934 e6a6               	INC gaddr+1
  1755  c936 a9ff               +	LDA #$ff		; asume "all pixels" mask
  1756  c938 85a3               	STA tmpmask
  1757  c93a b1a5               	LDA (gaddr),y		; pixel data
  1758  c93c 45a8               	EOR fmode		; set/reset mode
  1759  c93e f00c               	BEQ to_right		; empty -> finally to to_right
  1760  c940 2076ca             	JSR bitposr		; search right border
  1761  c943 bd9ac1             	LDA maskright0,x	; mask out the right part
  1762  c946 25a3               	AND tmpmask		; shorten mask accordingly
  1763  c948 85a3               	STA tmpmask
  1764  c94a f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1765                          				; leading to 0 mask (fill_check wont't
  1766                          				; handle this special case)
  1767                          
  1768                          				; continue to fill to right ...
  1769                          to_right			; fill loop towards right border
  1770  c94c a5a3               	LDA tmpmask		; fill mask
  1771                          				; assert:    (bitmap & tempmask) == 0
  1772                          				;         || (bitmap & tempmask) == tempmask
  1773  c94e 51a5               	EOR (gaddr),y		; set/reset to fill
  1774  c950 91a5               	STA (gaddr),y		; into bitmap - the actual fill action!
  1775                          	
  1776                          check_above
  1777  c952 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1778                          				; c = 0!
  1779  c954 84a9               	STY ysave		; to be restored later
  1780  c956 a5a5               	LDA gaddr		; current graphic position
  1781  c958 a6a6               	LDX gaddr+1
  1782  c95a 88                 	DEY			; line above
  1783  c95b 100f               	BPL +			; leaving 8x8 block?
  1784                          	; c=0 (asl fcont)
  1785  c95d e93f               	SBC #$40-1		; block above:
  1786  c95f 85fb               	STA caddr		; caddr = gaddr - $140
  1787  c961 8a                 	TXA
  1788  c962 e901               	SBC #$01
  1789  c964 aa                 	TAX
  1790  c965 c9e0               	CMP #>gram		; still graphic ram?
  1791  c967 900a               	BCC skip_above
  1792  c969 a007               	LDY #7			; last line in block in new block
  1793  c96b 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1794  c96c 85fb               +	STA caddr		; still in same block
  1795  c96e 86fc               ++	STX caddr+1		; shared store
  1796  c970 2001ca             	JSR fill_check
  1797                          skip_above
  1798                          
  1799                          check_below
  1800  c973 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1801  c975 a5a5               	LDA gaddr		; current graphic position
  1802  c977 a6a6               	LDX gaddr+1
  1803  c979 a4a9               	LDY ysave		; restore original y position
  1804  c97b c8                 	INY			; line below
  1805  c97c c008               	CPY #8			; crossing 8x8 block?
  1806  c97e 9014               	BCC +			; less then 8
  1807                          	; c=1 (cpy)
  1808  c980 693f               	ADC #$40-1		; block below: accu has gaddr
  1809  c982 85fb               	STA caddr		; caddr = gaddr + $140
  1810  c984 a8                 	TAY			; for compare later
  1811  c985 8a                 	TXA			; gaddr high
  1812  c986 6901               	ADC #$01
  1813  c988 aa                 	TAX
  1814  c989 b010               	BCS skip_below		; > $10000  -> skip
  1815  c98b c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1816  c98d e9ff               	SBC #>(gram+8000)
  1817  c98f b00a               	BCS skip_below		; greater, so skip
  1818  c991 a000               	LDY #0			; first line in block
  1819  c993 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1820  c994 85fb               +	STA caddr		; transfer unchanged
  1821  c996 86fc               ++	STX caddr+1		; shared store
  1822  c998 2001ca             	JSR fill_check
  1823                          skip_below
  1824                          
  1825  c99b a4a9               	LDY ysave		; restore original y position
  1826  c99d a5a3               	LDA tmpmask		; mask:
  1827  c99f 2901               	AND #%00000001		; open to right, continue?
  1828  c9a1 d081               	BNE next_block		; to next block if open
  1829                          ; long branch version
  1830                          ;	BEQ process_stack	; not open, finished
  1831                          ;	JMP next_block		; to next block if open
  1832                          
  1833                          process_stack
  1834  c9a3 a5fd               	LDA fstack		; stack empty?
  1835  c9a5 c531               	CMP basaryend
  1836  c9a7 a5fe               	LDA fstack+1
  1837  c9a9 e532               	SBC basaryend+1
  1838  c9ab b003               	BCS +			; fstack >= basaryend -> not empty
  1839  c9ad 4c3fc2             	JMP gexit		; empty, we are finished
  1840                          
  1841  c9b0 a003               +	LDY #4-1		; top of stack, element's last component
  1842  c9b2 b1fd               	LDA (fstack),y
  1843  c9b4 85a7               	STA x8			; 8x8 block position
  1844  c9b6 88                 	DEY
  1845  c9b7 b1fd               	LDA (fstack),y
  1846  c9b9 85a3               	STA tmpmask		; pixel mask
  1847  c9bb 88                 	DEY
  1848  c9bc b1fd               	LDA (fstack),y
  1849  c9be 85a6               	STA gaddr+1		; graphic addr high byte
  1850  c9c0 88                 	DEY
  1851  c9c1 b1fd               	LDA (fstack),y		; graphic addr low byte combined with y-line
  1852  c9c3 aa                 	TAX			; needed twice
  1853  c9c4 29f8               	AND #%11111000		; split off address
  1854  c9c6 85a5               	STA gaddr
  1855  c9c8 8a                 	TXA
  1856  c9c9 2907               	AND #%00000111		; split off y-line
  1857  c9cb a8                 	TAY
  1858                          	
  1859  c9cc b1a5               	LDA (gaddr),y		; get pixels
  1860  c9ce 45a8               	EOR fmode		; according to set/reset
  1861  c9d0 aa                 	TAX			; keep it for later
  1862  c9d1 25a3               	AND tmpmask		; focus on masked pixels
  1863  c9d3 08                 	PHP			; save Z flag
  1864  c9d4 f004               	BEQ pop_stack		; all bits unset, remove from stack
  1865                          				; and fill it!
  1866  c9d6 c5a3               	CMP tmpmask		; all gaps filled?
  1867  c9d8 d010               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1868                          	; all gaps filled, next on stack 
  1869                          pop_stack
  1870  c9da 38                 	SEC	
  1871  c9db a5fd               	LDA fstack		; remove entry from stack
  1872  c9dd e904               	SBC #4			; entry size
  1873  c9df 85fd               	STA fstack
  1874  c9e1 b002               	BCS +
  1875  c9e3 c6fe               	DEC fstack+1
  1876  c9e5 28                 +	PLP
  1877  c9e6 d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1878                          				; all bits unset,
  1879  c9e8 f001               	BEQ ++			; stack already cleaned up
  1880  c9ea 28                 +++	PLP			; stack cleanup
  1881                          
  1882                          	; set bits outside mask to 1
  1883  c9eb 8a                 ++	TXA			; bitmap
  1884                          				; 00100110	
  1885  c9ec 49ff               	EOR #$ff		; 11011001
  1886  c9ee 25a3               	AND tmpmask		; 00011100 -> 00011000
  1887  c9f0 49ff               	EOR #$ff		; 11100111
  1888                          				; pixel outside tmpmask now set!
  1889  c9f2 a2ff               	LDX #$ff		; pixel gap search: first one from left
  1890  c9f4 e8                 -	INX
  1891  c9f5 0a                 	ASL			; counting from left
  1892  c9f6 b0fc               	BCS -			; loop if pixel is set
  1893                          				; X has the bit number of the unset pixel
  1894  c9f8 b1a5               	LDA (gaddr),y		; setup value for processing a new line
  1895  c9fa 45a8               	EOR fmode		; set/reset mode
  1896  c9fc 8595               	STA tmp1		; temporary bitmap pixels
  1897  c9fe 4cdbc8             	JMP f_start		; long (to far away) jump to fill line start
  1898                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1899                          
  1900                          
  1901                          ; Check upper or lower fill path
  1902                          ;		destroys x
  1903                          
  1904                          fill_check
  1905  ca01 b1fb               	LDA (caddr),y
  1906  ca03 45a8               	EOR fmode		; pixel data
  1907  ca05 aa                 	TAX			; save for later
  1908  ca06 25a3               	AND tmpmask		; mask to fill
  1909  ca08 f015               	BEQ fc_cleared		; all masked pixels cleared?
  1910  ca0a c5a3               	CMP tmpmask		; check for gaps
  1911  ca0c f05b               	BEQ fc_exit		; all gaps filled, finished
  1912                          				; if not so, some pixels still set
  1913  ca0e a5a3               	LDA tmpmask
  1914                          fc_checkstart			; no continuation, init flag based on
  1915                          				; rightmost pixel:
  1916  ca10 4a                 	LSR			; mask bit 0 to carry
  1917  ca11 9019               	BCC fc_nocont		; maskbit empty?
  1918  ca13 8a                 	TXA			; pixel data
  1919  ca14 4a                 	LSR			; pixel bit 0 to carry
  1920  ca15 b015               	BCS fc_nocont		; bit 0 set
  1921                          				; -> mask is 1 and pixel 0
  1922                          fc_cont
  1923  ca17 a596               	LDA fcont		; set flag for continuation
  1924  ca19 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1925  ca1b 8596               	STA fcont
  1926  ca1d d013               	BNE push_to_stack	; always non zero
  1927                          
  1928                          fc_cleared
  1929  ca1f a5a3               	LDA tmpmask		; pixel & mask -> 0
  1930                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1931                          				; the caller asserts that this never happens
  1932  ca21 c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1933  ca23 d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1934                          				; 8 pixel line empty
  1935  ca25 a596               	LDA fcont		; continued gap?
  1936  ca27 2902               	AND #%00000010		; check bit 2
  1937  ca29 f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1938  ca2b 60                 	RTS			; gap continued and already on stack, leave
  1939                          
  1940                          fc_nocont
  1941  ca2c a596               	LDA fcont		; clear continuation flag
  1942  ca2e 29fd               	AND #%11111101		; clear bit 2
  1943  ca30 8596               	STA fcont
  1944                          
  1945                          push_to_stack
  1946  ca32 18                 	CLC			; fstack points to top of stack
  1947  ca33 a5fd               	LDA fstack		; to next free stack element
  1948  ca35 6904               	ADC #4			; entry size
  1949  ca37 85fd               	STA fstack
  1950  ca39 9002               	BCC +
  1951  ca3b e6fe               	INC fstack+1
  1952                          +
  1953  ca3d a534               	LDA strbot+1		; check stack space
  1954  ca3f c5fe               	CMP fstack+1
  1955  ca41 b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  1956                          				; strbot MSB < fstack MSB
  1957                          out_of_memory			
  1958  ca43 203fc2             	JSR gexit
  1959  ca46 a210               	LDX #$10		; out of memory error
  1960  ca48 6c0003             	JMP (v_baserr)		; basic error handler
  1961  ca4b d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  1962  ca4d a5fd               	LDA fstack		; MSB equal, check LSB
  1963  ca4f c533               	CMP strbot
  1964  ca51 b0f0               	BCS out_of_memory	; fstack collides with string heap!
  1965                          
  1966                          fc_put
  1967  ca53 98                 	TYA			; y-line (value 0-7) merged with
  1968  ca54 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  1969  ca56 a000               	LDY #0			; stack structure index, on next free element
  1970  ca58 91fd               	STA (fstack),y
  1971  ca5a c8                 	INY
  1972  ca5b a5fc               	LDA caddr+1
  1973  ca5d 91fd               	STA (fstack),y		; graphic address high
  1974  ca5f c8                 	INY
  1975  ca60 a5a3               	LDA tmpmask
  1976  ca62 91fd               	STA (fstack),y
  1977  ca64 c8                 	INY
  1978  ca65 a5a7               	LDA x8			; 8x8 block position
  1979  ca67 91fd               	STA (fstack),y
  1980                          	
  1981  ca69 60                 fc_exit	RTS
  1982                          	
  1983                          
  1984                          
  1985                          ; Get the pixel position of the first set pixel from the right.
  1986                          ; 76543210  bit ->
  1987                          ; XXXXXXXX
  1988                          ; 01234567  -> index
  1989                          
  1990                          ; 00000000 -> 0 -> $FF 
  1991                          ; 10000000 -> 1 -> $7F
  1992                          ; X1000000 -> 2 -> $3F
  1993                          ; XX100000 -> 3 -> $1F
  1994                          ; XXX10000 -> 4 -> $0F
  1995                          ; XXXX1000 -> 5 -> $07
  1996                          ; XXXXX100 -> 6 -> $03
  1997                          ; XXXXXX10 -> 7 -> $01
  1998                          ; XXXXXXX1 -> 8 -> $00
  1999                          
  2000                          ; usage: lda maskleft0,X
  2001                          
  2002                          ; speed consideration: for results from X 0 to 4 it is faster than
  2003                          ; a table-driven approach.
  2004                          
  2005                          bitposl
  2006  ca6a a2ff               	LDX #$ff
  2007  ca6c c900               	CMP #0		; special case (no bit set at all)
  2008  ca6e f004               	BEQ +
  2009  ca70 e8                 -	INX
  2010  ca71 0a                 	ASL		; shift to left
  2011  ca72 d0fc               	BNE -		; until byte is empty
  2012  ca74 e8                 +	INX
  2013  ca75 60                 	RTS
  2014                          
  2015                          ; Get the pixel position of the first set pixel from the left.
  2016                          ; 76543210  bit ->
  2017                          ; XXXXXXXX
  2018                          ; 01234567  -> index
  2019                          
  2020                          ; 00000000 -> 8 -> $FF
  2021                          ; 00000001 -> 7 -> $FE
  2022                          ; 0000001X -> 6 -> $FC
  2023                          ; 000001XX -> 5 -> $F8
  2024                          ; 00001XXX -> 4 -> $F0
  2025                          ; 0001XXXX -> 3 -> $E0
  2026                          ; 001XXXXX -> 2 -> $C0
  2027                          ; 01XXXXXX -> 1 -> $80
  2028                          ; 1XXXXXXX -> 0 -> $00
  2029                          
  2030                          ; usage: lda maskright0,X
  2031                          
  2032                          ; speed consideration: for results of X from 4 to 8 it is faster than
  2033                          ; a table-driven approach.
  2034                          
  2035                          bitposr
  2036  ca76 a208               	LDX #8
  2037  ca78 c900               	CMP #0			; special case (no bit set at all)
  2038  ca7a f004               	BEQ +
  2039  ca7c ca                 -	DEX
  2040  ca7d 4a                 	LSR			; shift to right
  2041  ca7e d0fc               	BNE -			; until byte is empty
  2042  ca80 60                 +	RTS
  2043                          
  2044                          ;-----------------------------------------------------------------
  2045                          
  2046                          unnew
  2047                          
  2048  ca81 a52b               	LDA bassta
  2049  ca83 8522               	STA str
  2050  ca85 a52c               	LDA bassta+1
  2051  ca87 8523               	STA str+1
  2052  ca89 a001               	LDY #1
  2053  ca8b 98                 	TYA
  2054  ca8c 9122               	STA (str),y		; != 0
  2055                          
  2056  ca8e 2033a5             	JSR b_rechain		; starting from bassta
  2057                          				; result in (str)
  2058  ca91 18                 	CLC			; str+1 -> new basic end
  2059  ca92 a423               	LDY str+1
  2060  ca94 a522               	LDA str
  2061  ca96 6902               	ADC #2
  2062  ca98 852d               	STA basend
  2063  ca9a 9001               	BCC +
  2064  ca9c c8                 	INY
  2065  ca9d 842e               +	STY basend+1
  2066  ca9f 4c60a6             	JMP b_clr		; perform CLR
  2067                          
  2068                          
  2069                          ;-----------------------------------------------------------------
  2070                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
