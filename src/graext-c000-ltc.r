
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
   344  c101 4ce0c6             	JMP get
   345  c104 c958               +	CMP #'X'
   346  c106 d003               	BNE +
   347  c108 4ccbc6             	JMP getposx
   348  c10b c959               +	CMP #'Y'
   349  c10d d0d2               	BNE parse_error
   350  c10f 4cd7c6             	JMP getposy
   351                          
   352                          ;-----------------------------------------------------------------
   353                          
   354                          ; the most commonly used command placed at the end ...
   355                          
   356  c112 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   357                          cmdsend
   358                          
   359                          cmdaddr
   360  c120 9bcaaac8a3c18bc7...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   361  c12c 63c817c742c8f2c4...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
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
   483  c22b a229               	LDX #(gromcode_end-gromcode) ; count of bytes
   484                          gra_copycode
   485  c22d bd4ec2             	LDA gromcode-1,X
   486  c230 9dd203             	STA gramcode-1,X
   487  c233 ca                 	DEX
   488  c234 d0f7               	BNE gra_copycode
   489  c236 ad3f03             	LDA savemo
   490  c239 290f               	AND #$0F
   491  c23b aa                 	TAX
   492  c23c 4c77c6             	JMP setmode_enter	; re-apply mode to routines
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
   528  c24f a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   529  c251 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   530                          }
   531  c255 b1a5                       LDA (gaddr),Y
   532                          gchange_op
   533  c257 1d64c1                     ORA bitmask,X
   534  c25a 91a5                       STA (gaddr),Y
   535                          !ifdef ltc {
   536  c25c a910               	LDA #mc_sim		; vollständige ROM-Simulation
   537  c25e 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   538                          }
   539  c262 60                         RTS
   540                          
   541                          ; mask a graphic location 
   542                          
   543                          gmask
   544                          !ifdef ltc {
   545  c263 eb                 	XBA
   546  c264 a920               	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   547  c266 8f010004           	STA memconf		; damit internes RAM gelesen werden kann!
   548  c26a eb                 	XBA
   549                          }
   550                          gmask_flip
   551  c26b 4900                       EOR #$00
   552                          gmask_op
   553  c26d 11a5                       ORA (gaddr),Y
   554  c26f 91a5                       STA (gaddr),Y
   555                          !ifdef ltc {
   556  c271 a910               	LDA #mc_sim		; vollständige ROM-Simulation
   557  c273 8f010004           	STA memconf		; wieder schnelles RAM ab $C000
   558                          }
   559  c277 60                         RTS
   560                          
   561                          }
   562                          
   563                          gromcode_end
   564                          
   565                          ;-----------------------------------------------------------------
   566                          
   567                          position
   568  c278 a5aa                       LDA y
   569  c27a 4a                         LSR
   570  c27b 4a                         LSR
   571  c27c 4a                         LSR			; y/8
   572  c27d a8                         TAY
   573  c27e 2903                       AND #%00000011		; (y/8) mod 4
   574  c280 aa                         TAX
   575  c281 a59b                       LDA xl			; x low
   576  c283 29f8                       AND #%11111000		; clear bit 2-0
   577  c285 18                         CLC
   578  c286 7d74c1                     ADC ytabl,X		; addr low: y base + x part
   579  c289 85a5                       STA gaddr
   580  c28b a59c                       LDA xh			; addr high: x part
   581  c28d 7978c1                     ADC ytabh,Y		; 	+ y base
   582  c290 85a6                       STA gaddr+1
   583  c292 a5aa                       LDA y			; vertical offset
   584  c294 2907                       AND #%00000111		; y mod 8
   585  c296 a8                         TAY
   586  c297 a59b                       LDA xl
   587  c299 2907                       AND #%00000111		; x mod 8
   588  c29b aa                         TAX			; horizonal offset
   589  c29c 60                         RTS			; (bitmask)
   590                          
   591                          
   592                          ;-----------------------------------------------------------------
   593                          
   594                          ; swap tupel xl,xh <-> xendl,xendh
   595                          
   596                          swap_x_xend
   597  c29d a69e                       LDX xendl		; swap x, xend
   598  c29f a49b                       LDY xl
   599  c2a1 869b                       STX xl
   600  c2a3 849e                       STY xendl
   601                          
   602  c2a5 a69f                       LDX xendh
   603  c2a7 a49c                       LDY xh
   604  c2a9 849f                       STY xendh
   605  c2ab 869c                       STX xh
   606  c2ad 60                 	RTS
   607                          
   608                          
   609                          ;-----------------------------------------------------------------
   610                          
   611                          ; line y up, x left, dx < dy (case 1)
   612                          
   613                          line_up_steep
   614  c2ae 2078c2                     JSR position		; x,y
   615                          loop_yup_xleft
   616  c2b1 20d303                     JSR gchange		; pixel
   617                          
   618  c2b4 18                         CLC			; k += dx
   619  c2b5 a595                       LDA kl
   620  c2b7 65ab                       ADC dxl			; dxh is 0, because dx < dy
   621  c2b9 8595                       STA kl
   622  c2bb 9014                       BCC +			; k >= 0 ->
   623                          
   624  c2bd e5a9               ++	SBC dy			; k -= dy (C=1)
   625  c2bf 8595                       STA kl
   626                          
   627  c2c1 ca                  	DEX			; x--
   628  c2c2 100d                       BPL +
   629  c2c4 a207                       LDX #7			; wrap around
   630  c2c6 38                 	SEC
   631  c2c7 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   632  c2c9 e908                       SBC #8
   633  c2cb 85a5                       STA gaddr
   634  c2cd b002                       BCS +
   635  c2cf c6a6                       DEC gaddr+1
   636                          
   637  c2d1 88                 +	DEY			; y--
   638  c2d2 100f                       BPL +++
   639  c2d4 38                         SEC			; y overflow
   640  c2d5 a5a5                       LDA gaddr
   641  c2d7 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   642  c2d9 85a5                       STA gaddr
   643  c2db a5a6                       LDA gaddr+1
   644  c2dd e901               	SBC #1
   645  c2df 85a6                       STA gaddr+1
   646  c2e1 a007                       LDY #7			; wrap around
   647                          
   648  c2e3 c6a3               +++	DEC cl			; until c=0
   649  c2e5 d0ca                       BNE loop_yup_xleft
   650  c2e7 4c3fc2                     JMP gexit
   651                          
   652                          
   653                          ;-----------------------------------------------------------------
   654                          
   655                          ; line x left, y up, dx > dy (case 2)
   656                          
   657                          line_up_flat
   658  c2ea 2078c2                     JSR position		; x,y
   659  c2ed a5a3               	LDA cl			; counter adjustment for
   660  c2ef f002               	BEQ +			; prepare for dec-dec-counting
   661  c2f1 e6a4               	INC ch
   662                          +
   663                          loop_xleft_yup
   664  c2f3 20d303                     JSR gchange		; pixel
   665                          
   666  c2f6 18                         CLC			; k += dy
   667  c2f7 a595                       LDA kl
   668  c2f9 65a9                       ADC dy
   669  c2fb 8595                       STA kl
   670  c2fd 9020                       BCC +			; k < 0
   671  c2ff e696                       INC kh
   672  c301 301c               	BMI +			; k < 0
   673                          
   674  c303 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   675  c305 8595                       STA kl
   676  c307 a596                       LDA kh
   677  c309 e5a7                       SBC dxh		
   678  c30b 8596                       STA kh
   679                          
   680  c30d 88                         DEY			; y--
   681  c30e 100f                       BPL +
   682  c310 38                 	SEC			; C=1 not always true (SBC above)
   683  c311 a5a5                       LDA gaddr		; y overflow
   684  c313 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   685  c315 85a5                       STA gaddr
   686  c317 a5a6                       LDA gaddr+1
   687  c319 e901               	SBC #1
   688  c31b 85a6                       STA gaddr+1
   689  c31d a007               	LDY #7			; wrap around
   690                          
   691  c31f ca                 +	DEX			; x--
   692  c320 100d                       BPL +++
   693  c322 a207                       LDX #7			; wrap around
   694  c324 38                 	SEC
   695  c325 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   696  c327 e908                       SBC #8
   697  c329 85a5                       STA gaddr
   698  c32b b002                       BCS +++
   699  c32d c6a6                       DEC gaddr+1
   700                          +++
   701  c32f c6a3               	DEC cl			; c--
   702  c331 d0c0                       BNE loop_xleft_yup
   703  c333 c6a4                       DEC ch			; adjusted high which allows this
   704  c335 d0bc                       BNE loop_xleft_yup
   705                          
   706  c337 4c3fc2                     JMP gexit
   707                          
   708                          
   709                          
   710                          ;-----------------------------------------------------------------
   711                          
   712                          ; line x left, y down, dx > dy (case 3)
   713                          
   714                          line_down_flat
   715  c33a 2078c2                     JSR position		; x,y
   716  c33d a5a3               	LDA cl			; counter adjustment for
   717  c33f f002               	BEQ +			; prepare for dec-dec-counting
   718  c341 e6a4               	INC ch
   719                          +
   720                          loop_xleft_ydown
   721  c343 20d303                     JSR gchange		; pixel
   722                          
   723  c346 18                         CLC			; k += dy
   724  c347 a595                       LDA kl
   725  c349 65a9                       ADC dy
   726  c34b 8595                       STA kl
   727  c34d 9021                       BCC +			; k < 0
   728  c34f e696                       INC kh
   729  c351 301d               	BMI +			; k < 0
   730                          
   731  c353 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   732  c355 8595                       STA kl
   733  c357 a596                       LDA kh
   734  c359 e5a7                       SBC dxh		
   735  c35b 8596                       STA kh
   736                          
   737  c35d c8                         INY			; y++
   738  c35e c008                       CPY #8
   739  c360 d00e                       BNE +
   740                          	; C=1
   741  c362 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   742  c364 693f                       ADC #$40-1		; C already set by CPY
   743  c366 85a5                       STA gaddr
   744  c368 a5a6                       LDA gaddr+1
   745  c36a 6901               	ADC #1
   746  c36c 85a6                       STA gaddr+1
   747  c36e a000                       LDY #0			; wrap around
   748                          
   749  c370 ca                 +	DEX			; x--
   750  c371 100d                       BPL +++
   751  c373 a207                       LDX #7			; wrap around
   752  c375 38                 	SEC
   753  c376 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   754  c378 e908                       SBC #8
   755  c37a 85a5                       STA gaddr
   756  c37c b002                       BCS +++
   757  c37e c6a6                       DEC gaddr+1
   758                          +++
   759  c380 c6a3               	DEC cl			; c--
   760  c382 d0bf               	BNE loop_xleft_ydown
   761  c384 c6a4               	DEC ch			; adjusted high which allows this
   762  c386 d0bb                       BNE loop_xleft_ydown
   763                          
   764  c388 4c3fc2                     JMP gexit
   765                          
   766                          
   767                          ;-----------------------------------------------------------------
   768                          
   769                          ; line y down, x right, dx < dy (case 4)
   770                          
   771                          line_down_steep
   772  c38b 2078c2                     JSR position		; x,y
   773                          loop_ydown_xleft
   774  c38e 20d303                     JSR gchange		; pixel
   775                          
   776  c391 18                         CLC			; k += dx
   777  c392 a595                       LDA kl
   778  c394 65ab                       ADC dxl			; dxh is 0, because dx < dy
   779  c396 8595                       STA kl
   780  c398 9014                       BCC +			; k >= 0 ->
   781                          
   782  c39a e5a9               	SBC dy			; k -= dy, C=1
   783  c39c 8595                       STA kl
   784                          
   785  c39e ca                  	DEX			; x--
   786  c39f 100d                       BPL +
   787  c3a1 a207                       LDX #7			; wrap around
   788  c3a3 38                 	SEC
   789  c3a4 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   790  c3a6 e908                       SBC #8
   791  c3a8 85a5                       STA gaddr
   792  c3aa b002                       BCS +
   793  c3ac c6a6                       DEC gaddr+1
   794                          
   795  c3ae c8                 +	INY			; y++
   796  c3af c008                       CPY #8			; y overflow?
   797  c3b1 d00e                       BNE +++
   798  c3b3 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   799  c3b5 693f                       ADC #$40-1		; C already set by CPY
   800  c3b7 85a5                       STA gaddr
   801  c3b9 a5a6                       LDA gaddr+1
   802  c3bb 6901               	ADC #1
   803  c3bd 85a6                       STA gaddr+1
   804  c3bf a000                       LDY #0			; wrap around
   805                          
   806  c3c1 c6a3               +++	DEC cl			; c--
   807                          				; until c=0
   808  c3c3 d0c9                       BNE loop_ydown_xleft
   809  c3c5 4c3fc2                     JMP gexit
   810                          
   811                          
   812                          ;-----------------------------------------------------------------
   813                          
   814                          getcommaxy
   815  c3c8 20fdae                     JSR b_getcomma		; check ","
   816                          getxy
   817  c3cb 208aad                     JSR b_getval		; get X coord. value
   818  c3ce 20f7b7                     JSR b_convint
   819  c3d1 c901                       CMP #>xmax
   820  c3d3 900c               	BCC gcxy_xok
   821  c3d5 f003                       BEQ ++			; X = $1xx
   822  c3d7 203ac6                     JSR range_error
   823                          
   824  c3da c040               ++	CPY #<xmax		; check X low
   825  c3dc 9003                       BCC +
   826  c3de 203ac6                     JSR range_error
   827                          +
   828                          gcxy_xok
   829  c3e1 84fb                       STY gpos		; temporary save X coord.
   830  c3e3 85fc                       STA gpos+1
   831                          
   832  c3e5 20f1b7                     JSR b_getcomma8bit
   833                          				; get Y coord. value
   834  c3e8 e0c8                       CPX #ymax
   835  c3ea 9003                       BCC +
   836  c3ec 203ac6                     JSR range_error
   837                          +
   838  c3ef a4fb                       LDY gpos		; restory X coord.
   839  c3f1 a5fc                       LDA gpos+1
   840  c3f3 60                         RTS
   841                          
   842                          
   843                          ;-----------------------------------------------------------------
   844                          
   845                          para_hline_box
   846  c3f4 20cbc3                     JSR getxy		; get startpoint
   847  c3f7 86aa                       STX y
   848  c3f9 8e3e03                     STX savey		; save as cursor, too
   849  c3fc 859c                       STA xh
   850  c3fe 849b                       STY xl
   851  c400 8d3d03             	STA savexh
   852  c403 8c3c03             	STY savexl
   853  c406 20fdae                     JSR b_getcomma		; get length
   854  c409 208aad                     JSR b_getval
   855  c40c 20f7b7                     JSR b_convint
   856                          				; calculate end point
   857  c40f aa                         TAX			; save length high byte
   858  c410 98                         TYA			; length low byte
   859  c411 18                         CLC
   860  c412 659b                       ADC xl			; low xend = x+length
   861  c414 859e                       STA xendl
   862  c416 a8                 	TAY
   863  c417 8a                         TXA			; high
   864  c418 659c                       ADC xh			; high xend = x+length
   865  c41a 859f                       STA xendh
   866  c41c aa                 	TAX
   867                          
   868  c41d c901               	CMP #>xmax		; endpoint outside?
   869  c41f 9005               	BCC +
   870  c421 d003               	BNE +			; >$200 (512)
   871  c423 98                 	TYA
   872  c424 e940               	SBC #<xmax
   873  c426 60                 +	RTS			; C=1 out of range, C=0 ok
   874                          
   875                          ;-----------------------------------------------------------------
   876                          
   877                          hline
   878  c427 20f4c3             	JSR para_hline_box
   879  c42a 9003               	BCC +
   880  c42c 203ac6             	JSR range_error
   881                          				; XXX xend=xmax-1 ?
   882                          +
   883  c42f 8e3d03                     STX savexh
   884  c432 8c3c03                     STY savexl		; also save as final cursor
   885                          
   886  c435 a900               	LDA #0			; default thickness 0 (means 1 pixel)
   887  c437 85a3               	STA ycount
   888  c439 207900             	JSR chrgot		; last char. again
   889  c43c f019               	BEQ +++			; command end? no optional param.
   890  c43e 20f1b7             	JSR b_getcomma8bit
   891  c441 8a                 	TXA			; optional 8-bit parameter
   892  c442 85a3               	STA ycount		; hline thickness
   893  c444 f011               	BEQ +++			; 0 means 1 pixel
   894  c446 18                 	CLC
   895  c447 65aa               	ADC y			; end position for y coord.
   896  c449 b004               	BCS +			; > 255
   897  c44b c9c8               	CMP #ymax
   898  c44d 9008               	BCC +++
   899                          +				; C=1 from ADC or CMP before
   900  c44f 203ac6             	JSR range_error		; corrupts A
   901                          				; XXX ycount=ymax-y-1 ?
   902                          				; xend >= x
   903  c452 b003               	BCS hl_noxswap		; always
   904                          
   905                          hline_start
   906  c454 209dc2             	JSR swap_x_xend		; xend < x, entry from line
   907                          	
   908                          hl_noxswap
   909                          				; xend > x
   910                          +++
   911  c457 e6a3               	INC ycount		; count to 0
   912  c459 2047c2                     JSR ginit		; map in graphic memory
   913                          
   914  c45c 2078c2                     JSR position		; graphic position x,y
   915                          
   916  c45f a5a5               	LDA gaddr		; save position for vertical
   917  c461 85fb               	STA sgaddr
   918  c463 a5a6               	LDA gaddr+1
   919  c465 85fc               	STA sgaddr+1
   920  c467 86ab               	STX xsave
   921  c469 84a9               	STY ysave
   922                          
   923  c46b a59e                       LDA xendl
   924  c46d 2907                       AND #%00000111
   925  c46f 8596                       STA tmp2		; xend mod 8, mask index
   926  c471 a59b                       LDA xl
   927  c473 29f8                       AND #%11111000		; (xl div 8)*8
   928  c475 8595                       STA tmp1
   929  c477 a59e                       LDA xendl		; xend unmasked
   930  c479 38                         SEC
   931  c47a e595                       SBC tmp1		; finally: xend - (x div 8)*8 
   932  c47c 8595                       STA tmp1
   933  c47e a59f                       LDA xendh
   934  c480 e59c                       SBC xh
   935  c482 4a                         LSR			; / 8 ->  0-39
   936  c483 a595                       LDA tmp1		; only 1 highest bit
   937  c485 6a                         ROR			; and 3 lower bits
   938  c486 4a                         LSR
   939  c487 4a                         LSR
   940                                  			; 8-pixel-blocks count
   941  c488 85a4               	STA hcount		; save for vertical extension
   942                           
   943                          hl_vertloop
   944  c48a 98                 	TYA			; calculate max. Y in 8x8 block
   945  c48b 18                 	CLC
   946  c48c 65a3               	ADC ycount
   947  c48e c908               	CMP #8
   948  c490 9002               	BCC +
   949  c492 a908               	LDA #8
   950  c494 85a8               +	STA ylimit
   951                          
   952  c496 bd91c1                     LDA maskleft,X		; starting mask
   953  c499 8595               	STA tmp1
   954  c49b a6a4               	LDX hcount		; how many blocks
   955                          
   956                          hl_nextblock
   957  c49d ca                         DEX
   958                          hl_islastblock
   959  c49e 301d                       BMI hl_lastblock
   960                          				; leave loop if X<0
   961  c4a0 a4a9               	LDY ysave
   962  c4a2 a595               -	LDA tmp1		; mask
   963  c4a4 20e703             	JSR gmask		; first with left end mask
   964  c4a7 c8                 	INY			; vertical down
   965  c4a8 c4a8               	CPY ylimit		; in 8x8 box
   966  c4aa d0f6               	BNE -
   967                          
   968  c4ac 18                         CLC			; gaddr += 8 (one block to right)
   969  c4ad a5a5                       LDA gaddr
   970  c4af 6908                       ADC #8
   971  c4b1 85a5                       STA gaddr
   972  c4b3 9002                       BCC +
   973  c4b5 e6a6                       INC gaddr+1
   974                          
   975  c4b7 a9ff               +	LDA #$FF		; following with full 8-pixel mask
   976  c4b9 8595               	STA tmp1
   977  c4bb d0e0               	BNE hl_nextblock	; always
   978                          
   979                          hl_lastblock
   980  c4bd a696                       LDX tmp2		; xend mask index
   981  c4bf 3d9bc1                     AND maskright,X		; A has current maskt combine with mask right end
   982  c4c2 8595               	STA tmp1		; mask
   983  c4c4 a4a9               	LDY ysave		; start position in 8x8 block
   984  c4c6 a595               -	LDA tmp1		; mask
   985  c4c8 20e703             	JSR gmask		; modify
   986  c4cb c8                 	INY			; vertical down
   987  c4cc c6a3               	DEC ycount		; overall y counter
   988  c4ce c4a8               	CPY ylimit
   989  c4d0 d0f4               	BNE -
   990                          
   991  c4d2 a5a3               	LDA ycount		; finished
   992  c4d4 d003               	BNE +			; roll-over into 8x8 block below
   993  c4d6 4c3fc2                     JMP gexit		; leave
   994                          
   995  c4d9 18                 +	CLC
   996  c4da a5fb               	LDA sgaddr
   997  c4dc 6940               	ADC #$40		; next 8-pixel row below
   998  c4de 85fb               	STA sgaddr		; + $140 (320)
   999  c4e0 85a5               	STA gaddr
  1000  c4e2 a5fc               	LDA sgaddr+1
  1001  c4e4 6901               	ADC #$01
  1002  c4e6 85fc               	STA sgaddr+1
  1003  c4e8 85a6               	STA gaddr+1
  1004  c4ea a6ab               	LDX xsave		; initial mask index
  1005  c4ec a000               	LDY #0			; start on top of 8x8
  1006  c4ee 84a9               	STY ysave
  1007  c4f0 f098               	BEQ hl_vertloop
  1008                          ;-----------------------------------------------------------------
  1009                          
  1010                          vline
  1011  c4f2 20cbc3                     JSR getxy		; get startpoint
  1012  c4f5 859c                       STA xh
  1013  c4f7 8d3d03                     STA savexh		; save as cursor too
  1014  c4fa 849b                       STY xl
  1015  c4fc 8c3c03                     STY savexl
  1016  c4ff 8693                       STX yend		; initial point is endpoint
  1017                          
  1018  c501 20f1b7                     JSR b_getcomma8bit
  1019                          				; get length
  1020  c504 18                         CLC			; calculate end point
  1021  c505 8a                         TXA			; length
  1022                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1023                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1024                          ;	STA tmp1
  1025  c506 6593                       ADC yend		; length + initial point is startpoint
  1026  c508 b005               	BCS vline_iq		; > 255
  1027  c50a c9c8                       CMP #ymax		; outside?
  1028  c50c a8                 	TAY			; keep startpoint
  1029  c50d 9003                       BCC +
  1030                          vline_iq
  1031  c50f 203ac6                     JSR range_error		; corrupts A
  1032                          				; XXX Y = ymax-1 ?
  1033  c512 84aa               +	STY y			; startpoint
  1034  c514 8c3e03             	STY savey		; set cursor y position
  1035  c517 18                 	CLC
  1036  c518 900e               	BCC +++			; skip following, because y, yend are already ordered
  1037                          
  1038                          vline_start			; entry point from line command (only)
  1039  c51a a5aa               	LDA y			; order of y, yend is not defined
  1040  c51c c593               	CMP yend
  1041  c51e b008               	BCS vl_noyswap		; yend > y ->
  1042  c520 a5aa               	LDA y			; swap y, yend
  1043  c522 a693               	LDX yend
  1044  c524 8593               	STA yend
  1045  c526 86aa               	STX y
  1046                          vl_noyswap
  1047                          				; startpoint is below the endpoint
  1048  c528 2047c2             +++	JSR ginit		; map in graphic memory
  1049                          
  1050                          vl_start
  1051  c52b 2078c2                     JSR position		; graphic position x,y
  1052  c52e bd64c1                     LDA bitmask,X
  1053  c531 8596                       STA tmp2		; save mask
  1054                          ; DON'T-CHANGE: replace ...
  1055  c533 38                         SEC
  1056  c534 a5aa                       LDA y			; startpoint is greater!
  1057  c536 e593                       SBC yend		; vertical length
  1058  c538 aa                         TAX
  1059                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1060                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1061                          ;	LDX tmp1
  1062  c539 e8                         INX			; +1 (exit on 0)
  1063  c53a 38                 	SEC			; for subtraction, never changed!
  1064                          vl_nextline
  1065  c53b a596                       LDA tmp2
  1066  c53d 20e703                     JSR gmask		; modify 
  1067  c540 88                         DEY			; go up
  1068  c541 100e                       BPL +
  1069  c543 a5a5                       LDA gaddr		; C=1
  1070  c545 e940               	SBC #$40		; gaddr -= 320
  1071  c547 85a5                       STA gaddr
  1072  c549 a5a6                       LDA gaddr+1
  1073  c54b e901                       SBC #$01
  1074  c54d 85a6                       STA gaddr+1
  1075  c54f a007                       LDY #7			; wrap y offset
  1076  c551 ca                 +	DEX			; all vertical positions done?
  1077  c552 d0e7                       BNE vl_nextline
  1078  c554 4c3fc2                     JMP gexit		; leave
  1079                          
  1080                          
  1081                          ;-----------------------------------------------------------------
  1082                          
  1083                          line
  1084  c557 20cbc3                     JSR getxy		; get startpoint
  1085  c55a 849b                       STY xl 
  1086  c55c 859c                       STA xh
  1087  c55e 86aa                       STX y
  1088                          
  1089  c560 20c8c3                     JSR getcommaxy		; get endpoint
  1090                          line_start
  1091  c563 8c3c03                     STY savexl		; save as cursor position too
  1092  c566 849e                       STY xendl
  1093  c568 8d3d03                     STA savexh
  1094  c56b 859f                       STA xendh
  1095  c56d 8e3e03                     STX savey
  1096  c570 8693                       STX yend
  1097                          
  1098  c572 a000                       LDY #$00		; initialize to 0
  1099  c574 84a8                       STY ydir
  1100  c576 8495                       STY kl
  1101  c578 8496                       STY kh
  1102                          
  1103  c57a 38                         SEC
  1104  c57b a59b                       LDA xl			; calculate dx
  1105  c57d e59e                       SBC xendl
  1106  c57f 85ab                       STA dxl
  1107  c581 a59c                       LDA xh
  1108  c583 e59f                       SBC xendh
  1109  c585 85a7                       STA dxh
  1110                          
  1111  c587 b018                       BCS li_xend_left
  1112                          	; dx != 0
  1113                          				; negate dx:
  1114  c589 98                         TYA			; Y=A=0
  1115  c58a 38                         SEC			; dx = 0 - dx
  1116  c58b e5ab                       SBC dxl
  1117  c58d 85ab                       STA dxl
  1118  c58f 98                         TYA			; Y=A=0
  1119  c590 e5a7                       SBC dxh
  1120  c592 85a7                       STA dxh
  1121                          				; C=0 always, needed later
  1122  c594 209dc2             	jsr swap_x_xend
  1123  c597 a6aa                       LDX y			; swap y
  1124  c599 a493                       LDY yend
  1125  c59b 8693                       STX yend
  1126  c59d 84aa                       STY y
  1127                          
  1128  c59f 9007                       BCC li_x_different
  1129                          				; C=0 always (from negation before)
  1130                          
  1131                          li_xend_left
  1132                                  			; A already contains dxh
  1133  c5a1 05ab                       ORA dxl			; dx = 0?
  1134  c5a3 d003                       BNE li_x_different
  1135  c5a5 4c1ac5                     JMP vline_start		; vertical line case
  1136                          
  1137                          li_x_different
  1138  c5a8 38                         SEC			; calculate dy
  1139  c5a9 a593                       LDA yend
  1140  c5ab e5aa                       SBC y
  1141  c5ad b006                       BCS li_y_right		; yend >= y?
  1142  c5af 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1143  c5b1 6901                       ADC #$01		; C=0
  1144  c5b3 85a8                       STA ydir		; always not 0: flag y goes up
  1145                          
  1146                          li_y_right
  1147  c5b5 85a9                       STA dy
  1148  c5b7 d007                       BNE +
  1149  c5b9 a900               	LDA #0			; line thickness = 1
  1150  c5bb 85a3               	STA ycount
  1151  c5bd 4c54c4                     JMP hline_start		; horizontal line case
  1152                          +
  1153                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1154                          
  1155  c5c0 a5a7                       LDA dxh			; dx > dy
  1156  c5c2 d01c                       BNE line_flat		; yes -> flat
  1157  c5c4 a5a9                       LDA dy			; no -> steep
  1158  c5c6 aa                         TAX
  1159  c5c7 c5ab                       CMP dxl
  1160  c5c9 9015                       BCC line_flat
  1161                          
  1162                          line_steep
  1163  c5cb e8                         INX	
  1164  c5cc 86a3                       STX cl			; c = dy+1
  1165  c5ce 4a                         LSR			; dy/2
  1166  c5cf 49ff               	EOR #$FF		; one's complement
  1167  c5d1 8595                       STA kl			; k = -dy/2 -1
  1168                          
  1169  c5d3 2047c2                     JSR ginit		; map in graphic memory
  1170                          
  1171  c5d6 a5a8                       LDA ydir
  1172  c5d8 d003                       BNE +
  1173  c5da 4c8bc3                     JMP line_down_steep	; y down, steep
  1174  c5dd 4caec2             +	JMP line_up_steep	; y up, steep
  1175                          
  1176                          line_flat
  1177  c5e0 a5a7                       LDA dxh
  1178  c5e2 a8                         TAY
  1179  c5e3 a6ab                       LDX dxl
  1180  c5e5 e8                         INX
  1181  c5e6 d001                       BNE +
  1182  c5e8 c8                         INY
  1183  c5e9 86a3               +	STX cl			; c = dx+1
  1184  c5eb 84a4                       STY ch
  1185                          
  1186  c5ed 4a                         LSR			; dx/2 high
  1187  c5ee 49ff               	EOR #$FF		; one's complement
  1188  c5f0 8596                       STA kh
  1189  c5f2 a5ab                       LDA dxl
  1190  c5f4 6a                         ROR			; dx/2 low
  1191  c5f5 49ff               	EOR #$FF		; one's complement
  1192  c5f7 8595                       STA kl			; k = -dx/2 - 1
  1193                          
  1194  c5f9 2047c2                     JSR ginit		; map in graphic memory
  1195                          
  1196  c5fc a5a8                       LDA ydir	
  1197  c5fe d003                       BNE +
  1198  c600 4c3ac3                     JMP line_down_flat	; y down, flat
  1199  c603 4ceac2             +	JMP line_up_flat	; y up, flat
  1200                          
  1201                          ;-----------------------------------------------------------------
  1202                          
  1203                          plot
  1204  c606 20cbc3                     JSR getxy		; get parameter
  1205  c609 859c                       STA xh			; save x/y
  1206  c60b 849b                       STY xl
  1207  c60d 86aa                       STX y
  1208  c60f 8d3d03                     STA savexh		; and store as cursor
  1209  c612 8c3c03                     STY savexl
  1210  c615 8e3e03                     STX savey
  1211                          
  1212                          plot_start
  1213  c618 2078c2                     JSR position		; calculate graphical address
  1214                          
  1215  c61b a501                       LDA prozport
  1216  c61d 29fd                       AND #%11111101		; Kernal ROM disable
  1217  c61f 78                         SEI			
  1218  c620 8501                       STA prozport
  1219                          
  1220  c622 20d303                     JSR gchange		; change graphical data
  1221                          
  1222  c625 a501                       LDA prozport
  1223  c627 0902                       ORA #%00000010		; kernal ROM enable
  1224  c629 8501                       STA prozport
  1225  c62b 58                         CLI
  1226  c62c 60                         RTS
  1227                          
  1228                          ;-----------------------------------------------------------------
  1229                          
  1230                          move
  1231  c62d 20cbc3                     JSR getxy		; get parameter
  1232  c630 8d3d03                     STA savexh		; just save as cursor
  1233  c633 8c3c03                     STY savexl
  1234  c636 8e3e03                     STX savey
  1235  c639 60                         RTS
  1236                          
  1237                          
  1238                          ;-----------------------------------------------------------------
  1239                          
  1240                          ; never touches X, Y, C-flag
  1241                          ; on exit: A corrupted, Z=0
  1242                          
  1243                          range_error
  1244  c63a ad3f03             	LDA savemo
  1245  c63d 29f0               	AND #$F0
  1246  c63f d003               	BNE +
  1247                          				; error mode 3: abort command (silent)
  1248  c641 68                 	PLA			; cleanup JSR
  1249  c642 68                 	PLA			; highbyte of return address >0
  1250                          
  1251  c643 60                 -	RTS			; error mode 5: back to command
  1252                          				; to handle value correction
  1253                          				; Z=0
  1254  c644 2920               +	AND #$20		; mode 5?
  1255  c646 d0fb               	BNE -			; exit with Z=0
  1256  c648 68                 	PLA			; error mode 4: terminate with error
  1257  c649 68                 	PLA			; cleanup JSR
  1258                          setmode_error
  1259  c64a 4c48b2             	JMP b_illquant		; throw error message
  1260                          
  1261                          ;-----------------------------------------------------------------
  1262                          
  1263                          setmode
  1264  c64d 209eb7                     JSR b_get8bit
  1265  c650 e003                       CPX #3
  1266  c652 9017                       BCC +			; less then 3, modification mode
  1267  c654 e006               	CPX #6
  1268  c656 b0f2               	BCS setmode_error	; out of range
  1269                          				; error mode
  1270  c658 8a                 	TXA
  1271  c659 e902               	SBC #2			; C=0, therefore -3
  1272  c65b 0a                 	ASL			; 0-2 -> 16,32 or 48
  1273  c65c 0a                 	ASL			; shift to upper nibble
  1274  c65d 0a                 	ASL
  1275  c65e 0a                 	ASL
  1276                          				; put A's bit 4-7 into savemo
  1277  c65f 4d3f03             	EOR savemo		; ********
  1278  c662 29f0               	AND #%11110000		; ****0000
  1279  c664 4d3f03             	EOR savemo		; AAAAmmmm
  1280  c667 8d3f03             	STA savemo		; 
  1281  c66a 60                 	RTS
  1282                          
  1283  c66b 8a                 +	TXA
  1284  c66c 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1285  c66f 290f               	AND #%00001111
  1286  c671 4d3f03             	EOR savemo
  1287  c674 8d3f03             	STA savemo
  1288                          setmode_enter
  1289  c677 e001               	CPX #$01
  1290  c679 b01a                       BCS set_or_toggle
  1291                          
  1292                          modereset
  1293  c67b a9c1                       LDA #>(nbitmask)
  1294  c67d 8ddd03                     STA gchange_op+2
  1295  c680 a96c                       LDA #<(nbitmask)
  1296  c682 8ddc03                     STA gchange_op+1
  1297  c685 a93d                       LDA #$3D		; opcode AND abs,X
  1298  c687 8ddb03                     STA gchange_op
  1299  c68a a931                       LDA #$31		; opcode AND (zp),Y
  1300  c68c 8df103                     STA gmask_op
  1301  c68f a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1302  c691 8df003                     STA gmask_flip+1
  1303  c694 60                         RTS
  1304                          
  1305                          set_or_toggle
  1306  c695 d01a                       BNE modetoggle
  1307                          modeset
  1308  c697 a9c1                       LDA #>(bitmask)
  1309  c699 8ddd03                     STA gchange_op+2
  1310  c69c a964                       LDA #<(bitmask)
  1311  c69e 8ddc03                     STA gchange_op+1
  1312  c6a1 a91d                       LDA #$1D		; opcode OR abs,X
  1313  c6a3 8ddb03                     STA gchange_op
  1314  c6a6 a911                       LDA #$11		; opcode OR (zp),Y
  1315  c6a8 8df103                     STA gmask_op
  1316  c6ab a900                       LDA #$00		; mask, EOR #$00, not inverting
  1317  c6ad 8df003                     STA gmask_flip+1
  1318  c6b0 60                         RTS
  1319                          
  1320                          modetoggle
  1321  c6b1 a9c1                       LDA #>(bitmask)
  1322  c6b3 8ddd03                     STA gchange_op+2
  1323  c6b6 a964                       LDA #<(bitmask)
  1324  c6b8 8ddc03                     STA gchange_op+1
  1325  c6bb a95d                       LDA #$5D		; opcode EOR abs,X
  1326  c6bd 8ddb03                     STA gchange_op
  1327  c6c0 a951                       LDA #$51		; opcode EOR (zp),Y
  1328  c6c2 8df103                     STA gmask_op
  1329  c6c5 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1330  c6c7 8df003                     STA gmask_flip+1
  1331  c6ca 60                         RTS
  1332                          
  1333                          
  1334                          ;-----------------------------------------------------------------
  1335                          ; get current x cursor position
  1336                          
  1337                          getposx
  1338  c6cb ac3c03             	LDY savexl
  1339  c6ce ad3d03             	LDA savexh
  1340  c6d1 2091b3             	JSR b_word2fac
  1341  c6d4 4c7300             	JMP chrget		; last position of expression (function name)
  1342                          
  1343                          ;-----------------------------------------------------------------
  1344                          ; get current y cursor position
  1345                          
  1346                          getposy
  1347  c6d7 ac3e03             	LDY savey
  1348  c6da 20a2b3             	JSR b_byte2fac
  1349  c6dd 4c7300             	JMP chrget		; last position of expression (function name)
  1350                          
  1351                          ;-----------------------------------------------------------------
  1352                          
  1353                          ; get pixel (check if pixel set)
  1354                          ; not used
  1355                          
  1356                          get
  1357  c6e0 207300             	JSR chrget		; advance past function name
  1358  c6e3 20faae             	JSR b_chkparl		; "("?
  1359  c6e6 20cbc3                     JSR getxy		; get X,Y values
  1360  c6e9 859c                       STA xh
  1361  c6eb 849b                       STY xl
  1362  c6ed 86aa                       STX y
  1363  c6ef 207900             	JSR chrgot
  1364  c6f2 20f7ae             	JSR b_chkparr		; ")"?
  1365                          	
  1366                          
  1367  c6f5 2078c2                     JSR position		; calculate graphic address/position
  1368                          
  1369  c6f8 a501                       LDA prozport
  1370  c6fa 29fd               	AND #%11111101		; Kernal ROM disable
  1371  c6fc 78                         SEI
  1372  c6fd 8501                       STA prozport
  1373                          
  1374  c6ff b1a5                       LDA (gaddr),Y
  1375  c701 3d64c1                     AND bitmask,X		; mask position
  1376  c704 a8                         TAY
  1377  c705 a501                       LDA prozport
  1378  c707 0902               	ORA #%00000010		; kernal ROM enable
  1379  c709 8501                       STA prozport
  1380  c70b 58                         CLI
  1381  c70c 98                 	TYA
  1382  c70d f002               	BEQ +
  1383  c70f a001               	LDY #1			; <> 0 -> always return 1
  1384  c711 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1385                          
  1386                          ;-----------------------------------------------------------------
  1387                          
  1388                          relto_cont
  1389                          				; continue
  1390  c714 207300             	JSR chrget		; skip TO token
  1391                          relto
  1392  c717 208aad                     JSR b_getval		; get X offset (+/-)
  1393  c71a a561               	LDA facexp		; FAC exponent
  1394  c71c c990               	CMP #$90		; more than 16 bit
  1395  c71e b031               	BCS relto_error		; illegal quantity
  1396  c720 209bbc                     JSR b_fac2int		; to signed integer
  1397                          
  1398  c723 18                         CLC
  1399  c724 a565                       LDA facintl
  1400  c726 6d3c03                     ADC savexl
  1401  c729 859e                       STA xendl
  1402  c72b a564                       LDA facinth
  1403  c72d 6d3d03                     ADC savexh
  1404  c730 859f                       STA xendh		; xend = savex+facint
  1405                          
  1406  c732 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1407  c735 208aad                     JSR b_getval
  1408  c738 a561                       LDA facexp		; FAC exponent
  1409  c73a c990                       CMP #$90		; more than 16 bit
  1410  c73c b013                       BCS relto_error		; illegal quantity
  1411  c73e 209bbc                     JSR b_fac2int		; to signed integer
  1412  c741 18                         CLC
  1413  c742 a565                       LDA facintl
  1414  c744 6d3e03                     ADC savey
  1415  c747 8593                       STA yend		; yend = savey+facint
  1416                          
  1417  c749 a59f                       LDA xendh		; check end coord. x
  1418  c74b c901                       CMP #>xmax
  1419  c74d 900e                       BCC rt_xok
  1420  c74f f003                       BEQ +
  1421                          relto_error
  1422  c751 203ac6                     JSR range_error
  1423  c754 a59e               +	LDA xendl
  1424  c756 c940                       CMP #<xmax
  1425  c758 9003                       BCC +
  1426  c75a 203ac6                     JSR range_error
  1427                          +
  1428                          rt_xok
  1429  c75d a593                       LDA yend		; check end coord. y
  1430  c75f c9c8                       CMP #ymax
  1431  c761 9003                       BCC +
  1432  c763 203ac6                     JSR range_error
  1433                          +
  1434  c766 ad3c03                     LDA savexl
  1435  c769 859b                       STA xl
  1436  c76b ad3d03                     LDA savexh
  1437  c76e 859c                       STA xh
  1438  c770 ad3e03                     LDA savey
  1439  c773 85aa                       STA y
  1440  c775 a49e                       LDY xendl
  1441  c777 a59f                       LDA xendh
  1442  c779 a693                       LDX yend		; xend/yend = cursor + x/y
  1443                          
  1444  c77b 2063c5                     JSR line_start		; draw line x/y to xend/yend
  1445                          
  1446  c77e 207900             	JSR chrgot
  1447  c781 d001               	BNE +
  1448  c783 60                 	RTS
  1449  c784 c9a4               +	CMP #t_to		; TO keyword?
  1450  c786 f08c               	BEQ relto_cont
  1451  c788 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1452                          
  1453                          ;-----------------------------------------------------------------
  1454                          
  1455                          char
  1456  c78b 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1457  c78e e028                       CPX #40	
  1458  c790 9003                       BCC +
  1459                          char_error
  1460  c792 4c48b2                     JMP b_illquant
  1461  c795 86fb               +	STX gpos		; save x coord.
  1462  c797 20f1b7                     JSR b_getcomma8bit
  1463                          				; get char. position y 0-24
  1464  c79a e019                       CPX #25
  1465  c79c b0f4                       BCS char_error
  1466  c79e 86fc                       STX gpos+1		; save y coord.
  1467                          
  1468  c7a0 20fdae                     JSR b_getcomma		; get string
  1469  c7a3 209ead                     JSR b_getexpr
  1470  c7a6 20a3b6                     JSR b_stringval		 ; string address in str
  1471  c7a9 48                         PHA			; string length
  1472  c7aa a6fc                       LDX gpos+1		; y coord. for char. position
  1473  c7ac 8a                         TXA
  1474  c7ad 2903                       AND #$03		; mask 2 bits
  1475  c7af a8                         TAY			; table index
  1476  c7b0 a900                       LDA #$00
  1477  c7b2 85fc                       STA gpos+1		; x high
  1478  c7b4 a5fb                       LDA gpos		; saved x: multiply by 8
  1479  c7b6 0a                         ASL
  1480  c7b7 0a                         ASL
  1481  c7b8 0a                         ASL
  1482  c7b9 26fc                       ROL gpos+1		; overflow to high byte
  1483  c7bb 7974c1                     ADC ytabl,Y
  1484  c7be 85a5                       STA gaddr
  1485  c7c0 a5fc                       LDA gpos+1		; x high
  1486  c7c2 7d78c1                     ADC ytabh,X
  1487  c7c5 85a6                       STA gaddr+1
  1488  c7c7 68                         PLA			; string length
  1489  c7c8 a000                       LDY #$00		; string index
  1490  c7ca aa                         TAX			; length
  1491  c7cb e8                         INX			; prepare as counter
  1492                          char_loop
  1493  c7cc ca                         DEX
  1494  c7cd f008                       BEQ char_exit
  1495  c7cf b122                       LDA (str),Y		; read string
  1496  c7d1 20d8c7                     JSR char_display
  1497  c7d4 c8                         INY
  1498  c7d5 d0f5                       BNE char_loop
  1499                          char_exit
  1500  c7d7 60                         RTS
  1501                          
  1502                          char_display
  1503  c7d8 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1504  c7da 8a                         TXA			; save register X+Y
  1505  c7db 48                         PHA
  1506  c7dc 98                         TYA
  1507  c7dd 48                         PHA
  1508  c7de a5d7                       LDA z_tmp		; get saved character
  1509  c7e0 3012                       BMI char_inverse
  1510                          
  1511                          char_normal
  1512  c7e2 c920                       CMP #$20		; control character?
  1513  c7e4 9054                       BCC char_disp_leave
  1514  c7e6 c960                       CMP #$60
  1515  c7e8 9004                       BCC +
  1516  c7ea 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1517  c7ec d014                       BNE char_hires
  1518  c7ee 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1519  c7f0 d010               	BNE char_hires
  1520  c7f2 f00e               	BEQ char_hires
  1521                          
  1522                          char_inverse
  1523  c7f4 297f                       AND #%01111111		; mask bit 7
  1524  c7f6 c97f                       CMP #%01111111		; was 255? (pi)
  1525  c7f8 d002                       BNE +
  1526  c7fa a95e                       LDA #$5E		; screen code for pi
  1527  c7fc c920               +	CMP #$20		; control character?
  1528  c7fe 903a                       BCC char_disp_leave
  1529                          				; yes, skip
  1530  c800 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1531                          				; $C0-$FF -> $40-$7F
  1532                          				; OPT: BNE char_hires
  1533                          				; OPT: char_normal
  1534                          char_hires
  1535  c802 a6c7                       LDX z_reverseflag
  1536  c804 f002                       BEQ +
  1537  c806 0980                       ORA #%10000000		; invert char.
  1538  c808 aa                 +	TAX			; save char. for later
  1539  c809 a501                       LDA prozport		; save prozport state
  1540  c80b 48                 	PHA
  1541  c80c a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1542  c80e 78                         SEI
  1543  c80f 8501                       STA prozport		; char. rom base = $D000
  1544  c811 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1545  c813 85fc                       STA gpos+1		; 
  1546  c815 8a                         TXA			; char. code
  1547  c816 0a                         ASL			; *8
  1548  c817 26fc                       ROL gpos+1
  1549  c819 0a                         ASL
  1550  c81a 26fc                       ROL gpos+1
  1551  c81c 0a                         ASL
  1552  c81d 26fc                       ROL gpos+1
  1553  c81f 85fb                       STA gpos		; addr. in char. rom for char.
  1554                          
  1555  c821 a007                       LDY #$07		; 8 hires lines
  1556                          char_line
  1557  c823 b1fb                       LDA (gpos),Y		; read character line
  1558  c825 20e703                     JSR gmask		; write to hires screen
  1559  c828 88                         DEY
  1560  c829 10f8                       BPL char_line
  1561                          
  1562  c82b 68                 	PLA
  1563  c82c 8501                       STA prozport
  1564  c82e 58                         CLI
  1565                          
  1566  c82f 18                         CLC			; step char position to left
  1567  c830 a5a5                       LDA gaddr		; ( +8 )
  1568  c832 6908                       ADC #$08
  1569  c834 85a5                       STA gaddr
  1570  c836 9002                       BCC +
  1571  c838 e6a6                       INC gaddr+1
  1572                          +
  1573                          char_disp_leave
  1574  c83a 68                 	PLA			; pass written character back
  1575  c83b a8                         TAY			; restore saved registers
  1576  c83c 68                         PLA
  1577  c83d aa                         TAX
  1578  c83e 60                 -       RTS
  1579                          
  1580                          
  1581                          ;-----------------------------------------------------------------
  1582                          
  1583                          to_cont
  1584                          				; continue
  1585  c83f 207300             	JSR chrget		; skip TO token
  1586                          to
  1587  c842 ad3c03                     LDA savexl
  1588  c845 859b                       STA xl
  1589  c847 ad3d03                     LDA savexh
  1590  c84a 859c                       STA xh
  1591  c84c ad3e03                     LDA savey
  1592  c84f 85aa                       STA y
  1593  c851 20cbc3                     JSR getxy
  1594  c854 2063c5                     JSR line_start
  1595  c857 207900             	JSR chrgot
  1596  c85a f0e2               	BEQ -
  1597  c85c c9a4               	CMP #t_to		; TO keyword?
  1598  c85e f0df               	BEQ to_cont
  1599  c860 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1600                          
  1601                          ;-----------------------------------------------------------------
  1602                          
  1603                          box
  1604  c863 20f4c3                     JSR para_hline_box
  1605  c866 9003               	BCC +
  1606  c868 203ac6             	JSR range_error
  1607                          				; XXX xend=xmax-1 ?
  1608                          +
  1609  c86b 20f1b7             	JSR b_getcomma8bit
  1610  c86e 8a                 	TXA			; optional 8-bit parameter
  1611                          				; height
  1612  c86f f00c               	BEQ +++			; 0 means 1, box is just a line
  1613  c871 18                 	CLC
  1614  c872 65aa               	ADC y			; end position for y coord.
  1615  c874 b004               	BCS +			; > 255
  1616  c876 c9c8               	CMP #ymax
  1617  c878 9003               	BCC +++
  1618                          +				; C=1 from ADC or CMP before
  1619  c87a 203ac6             	JSR range_error		; corrupts A
  1620                          				; XXX ycount=ymax-y-1 ?
  1621                          				; xend >= x
  1622  c87d 48                 +++	PHA			; yend
  1623  c87e a900               	LDA #0
  1624  c880 85a3               	STA ycount		; line thickness 1
  1625  c882 2057c4             	JSR hl_noxswap		; upper horizontal line
  1626                          
  1627                          				; right vertical line
  1628  c885 68                 	PLA			; if 0, heigth is 1
  1629  c886 d001               	BNE +			; no 
  1630  c888 60                 	RTS			; exit, if box is degenerated (line)
  1631  c889 a6aa               +	LDX y			; start point at higher values
  1632  c88b 85aa               	STA y
  1633  c88d 8693               	STX yend
  1634  c88f a59e               	LDA xendl
  1635  c891 859b               	STA xl
  1636  c893 a59f               	LDA xendh
  1637  c895 859c               	STA xh
  1638  c897 2028c5             	JSR vl_noyswap		; xend,yend -> xend,y
  1639                          				; lower horizontal line
  1640  c89a ad3c03             	LDA savexl
  1641  c89d 859b               	STA xl
  1642  c89f ad3d03             	LDA savexh
  1643  c8a2 859c               	STA xh			; xend already set
  1644  c8a4 2057c4             	JSR hl_noxswap		; x,yend -> xend,yend
  1645                          				; left vertical line
  1646  c8a7 4c28c5             	JMP vl_noyswap		; x,y -> x,xend
  1647                          
  1648                          ;-----------------------------------------------------------------
  1649                          
  1650                          fill
  1651  c8aa 20cbc3                     JSR getxy
  1652  c8ad 859c                       STA xh			; save x/y
  1653  c8af 849b                       STY xl
  1654  c8b1 86aa                       STX y
  1655  c8b3 8d3d03                     STA savexh		; and store as cursor
  1656  c8b6 8c3c03                     STY savexl
  1657  c8b9 8e3e03                     STX savey
  1658                                  
  1659  c8bc a531                       LDA basaryend		; initialize fill stack pointer
  1660  c8be 38                 	SEC
  1661  c8bf e904               	SBC #4			; one element below
  1662  c8c1 85fd                       STA fstack		; use space between basic arrays
  1663  c8c3 a532                       LDA basaryend+1		; and string heap bottom
  1664  c8c5 e900               	SBC #0			; take borrow
  1665  c8c7 85fe                       STA fstack+1
  1666                          
  1667  c8c9 2078c2             	JSR position		; graphic position in (gaddr)+Y, bit X
  1668                          
  1669  c8cc a59c               	LDA xh			; setup 8x8 block index (x8)
  1670  c8ce 4a                 	LSR			; high bit into C
  1671  c8cf a59b               	LDA xl
  1672  c8d1 2a                 	ROL			; take high bit
  1673  c8d2 4a                 	LSR
  1674  c8d3 4a                 	LSR			; finally divide by 8
  1675  c8d4 85a7               	STA x8			; = index of 8x8 block in bitmap
  1676                          
  1677                          	; set fmode (from mode)
  1678  c8d6 ad3f03             	LDA savemo
  1679  c8d9 2903               	AND #3
  1680  c8db aa                 	TAX
  1681  c8dc ca                 	DEX
  1682  c8dd 3003               	BMI +			; mode = 0 -> invertmask: $FF
  1683  c8df f001               	BEQ +			; mode = 1 -> invertmask: $00
  1684  c8e1 ca                 	DEX			; mode = 2 -> ? (same as mode=0)
  1685  c8e2 86a8               +	STX fmode		; mode set or reset
  1686                          
  1687  c8e4 2047c2             	JSR ginit		; map in bitmap memory
  1688                          
  1689  c8e7 b1a5               	LDA (gaddr),y		; graphic position in Y (in index in 8x8 block)
  1690  c8e9 45a8               	EOR fmode
  1691  c8eb 8595               	STA tmp1		; bitmap, for later usage
  1692                          
  1693  c8ed 3d64c1             	AND bitmask,x		; test start pixel
  1694  c8f0 f003               	BEQ +			; not set
  1695                          f_exit
  1696  c8f2 4c3fc2             	JMP gexit		; leave if start pixel is already set
  1697                          +
  1698                          f_start				; the start: in mid of a line to fill ...
  1699  c8f5 a900               	LDA #0
  1700  c8f7 8596               	STA fcont		; initialize continuation flag for line above und below
  1701                          
  1702  c8f9 a595               	LDA tmp1		; graphic pixel data
  1703                          				; extent bitmask to the right
  1704  c8fb 86ab               	STX xsave
  1705  c8fd 3d91c1             	AND maskleft,x		; mask out left part, bits right from starting point remain
  1706  c900 2090ca             	JSR bitposr		; find the first set bit from start to right (border)
  1707  c903 bd9ac1             	LDA maskright0,x	; get a mask from the right border to left
  1708  c906 85a3               	STA tmpmask		
  1709                          
  1710                          leftcont
  1711  c908 a595               	LDA tmp1		; graphic pixel data
  1712  c90a a6ab               	LDX xsave
  1713                          leftcont_a
  1714  c90c 3d9bc1             	AND maskright,x		; mask out right part, bits left from starting point remain
  1715  c90f f00e               	BEQ stepleft8		; no left border in this pixel line
  1716  c911 2084ca             	JSR bitposl		; find the first set bit from start to left (border)
  1717  c914 bd91c1             	LDA maskleft0,x		; get a mask from the left border to right
  1718  c917 25a3               	AND tmpmask		; intersect masks
  1719  c919 85a3               	STA tmpmask		; and store it for later
  1720  c91b f021               	BEQ next_block		; empty mask immediate continue to right
  1721  c91d d047               	BNE to_right		; start to walk and fill to the right border
  1722                          
  1723                          stepleft8
  1724  c91f a5a7               	LDA x8 			; 8x8 block position
  1725  c921 f043               	BEQ to_right		; =0, hit screen border
  1726  c923 c6a7               	DEC x8			; count step 8x8 block to left
  1727  c925 a9ff               	LDA #$ff
  1728  c927 85a3               	STA tmpmask		; initial mask full pixel line
  1729                          
  1730  c929 38                 	SEC 			; graphic address to to next pixel line/block
  1731  c92a a5a5               	LDA gaddr
  1732  c92c e908               	SBC #8
  1733  c92e b002               	BCS +
  1734  c930 c6a6               	DEC gaddr+1
  1735  c932 85a5               +	STA gaddr
  1736                          
  1737                          	; y left unchanged
  1738  c934 b1a5               	LDA (gaddr),y		; real graphic pixel data from bitmap
  1739  c936 45a8               	EOR fmode		; set/reset mode
  1740  c938 8595               	STA tmp1		; graphic pixel data
  1741  c93a a207               	LDX #7			; start bit 0 (index 7, rightmost)
  1742  c93c d0ce               	BNE leftcont_a		; loop to left border search
  1743                          	
  1744                          next_block
  1745  c93e e6a7               	INC x8			; step right a block
  1746  c940 a5a7               	LDA x8
  1747  c942 c928               	CMP #40			; beyond last horizontal block?
  1748  c944 b077               	BCS process_stack	; done if right screen border
  1749                          	; C = 0
  1750  c946 a5a5               	LDA gaddr		; advance to block right
  1751  c948 6908               	ADC #8			; gaddr = gaddr + 8
  1752  c94a 85a5               	STA gaddr
  1753  c94c 9002               	BCC +
  1754  c94e e6a6               	INC gaddr+1
  1755  c950 a9ff               +	LDA #$ff		; asume "all pixels" mask
  1756  c952 85a3               	STA tmpmask
  1757  c954 b1a5               	LDA (gaddr),y		; pixel data
  1758  c956 45a8               	EOR fmode		; set/reset mode
  1759  c958 f00c               	BEQ to_right		; empty -> finally to to_right
  1760  c95a 2090ca             	JSR bitposr		; search right border
  1761  c95d bd9ac1             	LDA maskright0,x	; mask out the right part
  1762  c960 25a3               	AND tmpmask		; shorten mask accordingly
  1763  c962 85a3               	STA tmpmask
  1764  c964 f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1765                          				; leading to 0 mask (fill_check wont't
  1766                          				; handle this special case)
  1767                          
  1768                          				; continue to fill to right ...
  1769                          to_right			; fill loop towards right border
  1770  c966 a5a3               	LDA tmpmask		; fill mask
  1771                          				; assert:    (bitmap & tempmask) == 0
  1772                          				;         || (bitmap & tempmask) == tempmask
  1773  c968 51a5               	EOR (gaddr),y		; set/reset to fill
  1774  c96a 91a5               	STA (gaddr),y		; into bitmap - the actual fill action!
  1775                          	
  1776                          check_above
  1777  c96c 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1778                          				; c = 0!
  1779  c96e 84a9               	STY ysave		; to be restored later
  1780  c970 a5a5               	LDA gaddr		; current graphic position
  1781  c972 a6a6               	LDX gaddr+1
  1782  c974 88                 	DEY			; line above
  1783  c975 100f               	BPL +			; leaving 8x8 block?
  1784                          	; c=0 (asl fcont)
  1785  c977 e93f               	SBC #$40-1		; block above:
  1786  c979 85fb               	STA caddr		; caddr = gaddr - $140
  1787  c97b 8a                 	TXA
  1788  c97c e901               	SBC #$01
  1789  c97e aa                 	TAX
  1790  c97f c9e0               	CMP #>gram		; still graphic ram?
  1791  c981 900a               	BCC skip_above
  1792  c983 a007               	LDY #7			; last line in block in new block
  1793  c985 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1794  c986 85fb               +	STA caddr		; still in same block
  1795  c988 86fc               ++	STX caddr+1		; shared store
  1796  c98a 201bca             	JSR fill_check
  1797                          skip_above
  1798                          
  1799                          check_below
  1800  c98d 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1801  c98f a5a5               	LDA gaddr		; current graphic position
  1802  c991 a6a6               	LDX gaddr+1
  1803  c993 a4a9               	LDY ysave		; restore original y position
  1804  c995 c8                 	INY			; line below
  1805  c996 c008               	CPY #8			; crossing 8x8 block?
  1806  c998 9014               	BCC +			; less then 8
  1807                          	; c=1 (cpy)
  1808  c99a 693f               	ADC #$40-1		; block below: accu has gaddr
  1809  c99c 85fb               	STA caddr		; caddr = gaddr + $140
  1810  c99e a8                 	TAY			; for compare later
  1811  c99f 8a                 	TXA			; gaddr high
  1812  c9a0 6901               	ADC #$01
  1813  c9a2 aa                 	TAX
  1814  c9a3 b010               	BCS skip_below		; > $10000  -> skip
  1815  c9a5 c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1816  c9a7 e9ff               	SBC #>(gram+8000)
  1817  c9a9 b00a               	BCS skip_below		; greater, so skip
  1818  c9ab a000               	LDY #0			; first line in block
  1819  c9ad 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1820  c9ae 85fb               +	STA caddr		; transfer unchanged
  1821  c9b0 86fc               ++	STX caddr+1		; shared store
  1822  c9b2 201bca             	JSR fill_check
  1823                          skip_below
  1824                          
  1825  c9b5 a4a9               	LDY ysave		; restore original y position
  1826  c9b7 a5a3               	LDA tmpmask		; mask:
  1827  c9b9 2901               	AND #%00000001		; open to right, continue?
  1828  c9bb d081               	BNE next_block		; to next block if open
  1829                          ; long branch version
  1830                          ;	BEQ process_stack	; not open, finished
  1831                          ;	JMP next_block		; to next block if open
  1832                          
  1833                          process_stack
  1834  c9bd a5fd               	LDA fstack		; stack empty?
  1835  c9bf c531               	CMP basaryend
  1836  c9c1 a5fe               	LDA fstack+1
  1837  c9c3 e532               	SBC basaryend+1
  1838  c9c5 b003               	BCS +			; fstack >= basaryend -> not empty
  1839  c9c7 4c3fc2             	JMP gexit		; empty, we are finished
  1840                          
  1841  c9ca a003               +	LDY #4-1		; top of stack, element's last component
  1842  c9cc b1fd               	LDA (fstack),y
  1843  c9ce 85a7               	STA x8			; 8x8 block position
  1844  c9d0 88                 	DEY
  1845  c9d1 b1fd               	LDA (fstack),y
  1846  c9d3 85a3               	STA tmpmask		; pixel mask
  1847  c9d5 88                 	DEY
  1848  c9d6 b1fd               	LDA (fstack),y
  1849  c9d8 85a6               	STA gaddr+1		; graphic addr high byte
  1850  c9da 88                 	DEY
  1851  c9db b1fd               	LDA (fstack),y		; graphic addr low byte combined with y-line
  1852  c9dd aa                 	TAX			; needed twice
  1853  c9de 29f8               	AND #%11111000		; split off address
  1854  c9e0 85a5               	STA gaddr
  1855  c9e2 8a                 	TXA
  1856  c9e3 2907               	AND #%00000111		; split off y-line
  1857  c9e5 a8                 	TAY
  1858                          	
  1859  c9e6 b1a5               	LDA (gaddr),y		; get pixels
  1860  c9e8 45a8               	EOR fmode		; according to set/reset
  1861  c9ea aa                 	TAX			; keep it for later
  1862  c9eb 25a3               	AND tmpmask		; focus on masked pixels
  1863  c9ed 08                 	PHP			; save Z flag
  1864  c9ee f004               	BEQ pop_stack		; all bits unset, remove from stack
  1865                          				; and fill it!
  1866  c9f0 c5a3               	CMP tmpmask		; all gaps filled?
  1867  c9f2 d010               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1868                          	; all gaps filled, next on stack 
  1869                          pop_stack
  1870  c9f4 38                 	SEC	
  1871  c9f5 a5fd               	LDA fstack		; remove entry from stack
  1872  c9f7 e904               	SBC #4			; entry size
  1873  c9f9 85fd               	STA fstack
  1874  c9fb b002               	BCS +
  1875  c9fd c6fe               	DEC fstack+1
  1876  c9ff 28                 +	PLP
  1877  ca00 d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1878                          				; all bits unset,
  1879  ca02 f001               	BEQ ++			; stack already cleaned up
  1880  ca04 28                 +++	PLP			; stack cleanup
  1881                          
  1882                          	; set bits outside mask to 1
  1883  ca05 8a                 ++	TXA			; bitmap
  1884                          				; 00100110	
  1885  ca06 49ff               	EOR #$ff		; 11011001
  1886  ca08 25a3               	AND tmpmask		; 00011100 -> 00011000
  1887  ca0a 49ff               	EOR #$ff		; 11100111
  1888                          				; pixel outside tmpmask now set!
  1889  ca0c a2ff               	LDX #$ff		; pixel gap search: first one from left
  1890  ca0e e8                 -	INX
  1891  ca0f 0a                 	ASL			; counting from left
  1892  ca10 b0fc               	BCS -			; loop if pixel is set
  1893                          				; X has the bit number of the unset pixel
  1894  ca12 b1a5               	LDA (gaddr),y		; setup value for processing a new line
  1895  ca14 45a8               	EOR fmode		; set/reset mode
  1896  ca16 8595               	STA tmp1		; temporary bitmap pixels
  1897  ca18 4cf5c8             	JMP f_start		; long (to far away) jump to fill line start
  1898                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1899                          
  1900                          
  1901                          ; Check upper or lower fill path
  1902                          ;		destroys x
  1903                          
  1904                          fill_check
  1905  ca1b b1fb               	LDA (caddr),y
  1906  ca1d 45a8               	EOR fmode		; pixel data
  1907  ca1f aa                 	TAX			; save for later
  1908  ca20 25a3               	AND tmpmask		; mask to fill
  1909  ca22 f015               	BEQ fc_cleared		; all masked pixels cleared?
  1910  ca24 c5a3               	CMP tmpmask		; check for gaps
  1911  ca26 f05b               	BEQ fc_exit		; all gaps filled, finished
  1912                          				; if not so, some pixels still set
  1913  ca28 a5a3               	LDA tmpmask
  1914                          fc_checkstart			; no continuation, init flag based on
  1915                          				; rightmost pixel:
  1916  ca2a 4a                 	LSR			; mask bit 0 to carry
  1917  ca2b 9019               	BCC fc_nocont		; maskbit empty?
  1918  ca2d 8a                 	TXA			; pixel data
  1919  ca2e 4a                 	LSR			; pixel bit 0 to carry
  1920  ca2f b015               	BCS fc_nocont		; bit 0 set
  1921                          				; -> mask is 1 and pixel 0
  1922                          fc_cont
  1923  ca31 a596               	LDA fcont		; set flag for continuation
  1924  ca33 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1925  ca35 8596               	STA fcont
  1926  ca37 d013               	BNE push_to_stack	; always non zero
  1927                          
  1928                          fc_cleared
  1929  ca39 a5a3               	LDA tmpmask		; pixel & mask -> 0
  1930                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1931                          				; the caller asserts that this never happens
  1932  ca3b c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1933  ca3d d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1934                          				; 8 pixel line empty
  1935  ca3f a596               	LDA fcont		; continued gap?
  1936  ca41 2902               	AND #%00000010		; check bit 2
  1937  ca43 f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1938  ca45 60                 	RTS			; gap continued and already on stack, leave
  1939                          
  1940                          fc_nocont
  1941  ca46 a596               	LDA fcont		; clear continuation flag
  1942  ca48 29fd               	AND #%11111101		; clear bit 2
  1943  ca4a 8596               	STA fcont
  1944                          
  1945                          push_to_stack
  1946  ca4c 18                 	CLC			; fstack points to top of stack
  1947  ca4d a5fd               	LDA fstack		; to next free stack element
  1948  ca4f 6904               	ADC #4			; entry size
  1949  ca51 85fd               	STA fstack
  1950  ca53 9002               	BCC +
  1951  ca55 e6fe               	INC fstack+1
  1952                          +
  1953  ca57 a534               	LDA strbot+1		; check stack space
  1954  ca59 c5fe               	CMP fstack+1
  1955  ca5b b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  1956                          				; strbot MSB < fstack MSB
  1957                          out_of_memory			
  1958  ca5d 203fc2             	JSR gexit
  1959  ca60 a210               	LDX #$10		; out of memory error
  1960  ca62 6c0003             	JMP (v_baserr)		; basic error handler
  1961  ca65 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  1962  ca67 a5fd               	LDA fstack		; MSB equal, check LSB
  1963  ca69 c533               	CMP strbot
  1964  ca6b b0f0               	BCS out_of_memory	; fstack collides with string heap!
  1965                          
  1966                          fc_put
  1967  ca6d 98                 	TYA			; y-line (value 0-7) merged with
  1968  ca6e 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  1969  ca70 a000               	LDY #0			; stack structure index, on next free element
  1970  ca72 91fd               	STA (fstack),y
  1971  ca74 c8                 	INY
  1972  ca75 a5fc               	LDA caddr+1
  1973  ca77 91fd               	STA (fstack),y		; graphic address high
  1974  ca79 c8                 	INY
  1975  ca7a a5a3               	LDA tmpmask
  1976  ca7c 91fd               	STA (fstack),y
  1977  ca7e c8                 	INY
  1978  ca7f a5a7               	LDA x8			; 8x8 block position
  1979  ca81 91fd               	STA (fstack),y
  1980                          	
  1981  ca83 60                 fc_exit	RTS
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
  2006  ca84 a2ff               	LDX #$ff
  2007  ca86 c900               	CMP #0		; special case (no bit set at all)
  2008  ca88 f004               	BEQ +
  2009  ca8a e8                 -	INX
  2010  ca8b 0a                 	ASL		; shift to left
  2011  ca8c d0fc               	BNE -		; until byte is empty
  2012  ca8e e8                 +	INX
  2013  ca8f 60                 	RTS
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
  2036  ca90 a208               	LDX #8
  2037  ca92 c900               	CMP #0			; special case (no bit set at all)
  2038  ca94 f004               	BEQ +
  2039  ca96 ca                 -	DEX
  2040  ca97 4a                 	LSR			; shift to right
  2041  ca98 d0fc               	BNE -			; until byte is empty
  2042  ca9a 60                 +	RTS
  2043                          
  2044                          ;-----------------------------------------------------------------
  2045                          
  2046                          unnew
  2047                          
  2048  ca9b a52b               	LDA bassta
  2049  ca9d 8522               	STA str
  2050  ca9f a52c               	LDA bassta+1
  2051  caa1 8523               	STA str+1
  2052  caa3 a001               	LDY #1
  2053  caa5 98                 	TYA
  2054  caa6 9122               	STA (str),y		; != 0
  2055                          
  2056  caa8 2033a5             	JSR b_rechain		; starting from bassta
  2057                          				; result in (str)
  2058  caab 18                 	CLC			; str+1 -> new basic end
  2059  caac a423               	LDY str+1
  2060  caae a522               	LDA str
  2061  cab0 6902               	ADC #2
  2062  cab2 852d               	STA basend
  2063  cab4 9001               	BCC +
  2064  cab6 c8                 	INY
  2065  cab7 842e               +	STY basend+1
  2066  cab9 4c60a6             	JMP b_clr		; perform CLR
  2067                          
  2068                          
  2069                          ;-----------------------------------------------------------------
  2070                          graext_end

; ******** Source: graext.asm
    43                          
    44                          
