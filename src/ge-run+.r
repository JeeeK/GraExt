
; ******** Source: ge-run.asm
     1                          !to "ge-run.prg",cbm	
     2                          
     3                          ;  **** gra-ext loader ****
     4                          ;
     5                          ; 2016-05-18 johann e. klasek, johann at klasek at
     6                          ;
     7                          ; revisions:
     8                          ;	2019-10-12 v 1.29
     9                          ;
    10                          ;
    11                          ; Usage: RUN
    12                          ;
    13                          
    14                          ; loader for BASIC
    15                          
    16                          *= $0801
    17                          basic_start
    18                          ;       2019 sys....:REM by ....
    19  0801 2008e3079e                 !by <EOP,>EOP,<(2019),>(2019),$9E ; Link-Adresse, Zeilennummer, SYS-Token
    20  0806 32                         !by '0' + loader % 10000 / 1000   ; Ziffern für SYS-Argument
    21  0807 30                         !by '0' + loader %  1000 /  100
    22  0808 38                         !by '0' + loader %   100 /   10
    23  0809 32                         !by '0' + loader %    10
    24                                  ; ":rem "-Teil
    25  080a 3a8f204752414558...        !pet $3a, $8f, " graext by j.e.e.k."
    26  081f 00                         !by 0                             ; BASIC "End of Line"
    27  0820 0000               EOP     !wo 0                             ; BASIC-Programmende
    28                          
    29                          loader
    30                          
    31  0822 a282               	ldx #<graext_end	; setup basic
    32  0824 a012               	ldy #>graext_end
    33  0826 18                 	clc			; set if C=0
    34  0827 209cff             	jsr $ff9c		; KERNAL: system RAM bottom
    35  082a 862b               	stx $2b			; BASIC text start
    36  082c 842c               	sty $2c
    37  082e 2016e4             	jsr $e416		; setup BASIC text start
    38  0831 203e08             	jsr init		; init extension (place hook)
    39  0834 a953               	lda #<author		; message ...
    40  0836 a009               	ldy #>author
    41  0838 2041e4             	jsr $e441		; output string and perform BASIC NEW (set remaining pointers)
    42  083b 4c86e3             	jmp $e386		; BASIC warm start
    43                          

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
   215  083e ad0803                     LDA v_bascmd		; check if hooks are already 
   216  0841 ae0903                     LDX v_bascmd+1		; in place 
   217  0844 c9c9               	CMP #<(parse)
   218  0846 d004               	BNE +
   219  0848 e008               	CPX #>(parse)
   220  084a f052               	BEQ ++			; already hooked
   221                          
   222  084c 8d3403             +       STA savevpars		; save old vector
   223  084f 8e3503             	STX savevpars+1
   224  0852 a9c9               	LDA #<(parse)		; basic interpreter parser hook
   225  0854 8d0803                     STA v_bascmd		; for commands
   226  0857 a908                       LDA #>(parse)
   227  0859 8d0903                     STA v_bascmd+1
   228                          
   229  085c ad0a03                     LDA v_basexp		; basic interpreter parser hook
   230  085f 8d3a03             	STA savevexp		; for expressions
   231  0862 a9fd                       LDA #<(express)		; with save of old pointer
   232  0864 8d0a03                     STA v_basexp
   233  0867 ad0b03                     LDA v_basexp+1
   234  086a 8d3b03             	STA savevexp+1
   235  086d a908                       LDA #>(express)
   236  086f 8d0b03                     STA v_basexp+1
   237                          
   238  0872 ad2803                     LDA v_basstp
   239  0875 8d3803             	STA savevstp
   240  0878 a9b4                       LDA #<(stop)		; basic interpreter stop hook
   241  087a 8d2803                     STA v_basstp
   242  087d ad2903                     LDA v_basstp+1
   243  0880 8d3903             	STA savevstp+1
   244  0883 a908                       LDA #>(stop)
   245  0885 8d2903                     STA v_basstp+1
   246                          
   247  0888 ad0003                     LDA v_baserr
   248  088b 8d3603             	STA saveverr
   249  088e a9ae                       LDA #<(error)		; basic interpreter error hook
   250  0890 8d0003                     STA v_baserr
   251  0893 ad0103                     LDA v_baserr+1
   252  0896 8d3703             	STA saveverr+1
   253  0899 a908                       LDA #>(error)
   254  089b 8d0103                     STA v_baserr+1
   255                          
   256  089e a200               ++	LDX #0			; set graphic cursor to (0,0)
   257  08a0 8e3c03             	STX savexl
   258  08a3 8e3d03             	STX savexh
   259  08a6 8e3e03             	STX savey
   260  08a9 e8                 	INX
   261  08aa 8e3f03             	STX savemo		; set mode 1
   262  08ad 60                         RTS
   263                          
   264                          error	
   265                          	; reg A may destroyed
   266  08ae 20c309             	JSR gra_off		; uses only reg A
   267  08b1 6c3603             	JMP (saveverr)		; to original vector
   268                          
   269                          stop	
   270                          	; reg A may destroyed
   271  08b4 a591               	LDA $91			; Scan code
   272  08b6 c97f               	CMP #$7F		; STOP key?
   273  08b8 d003               	BNE nostop
   274  08ba 20c309             	JSR gra_off		; uses only reg A
   275                          nostop
   276  08bd 6c3803             	JMP (savevstp)		; to original vector
   277                          
   278                          
   279                          ;-----------------------------------------------------------------
   280                          
   281                          ; undo chrget
   282                          
   283                          undo_chrget
   284  08c0 a57a               	LDA txtptr		; decrement text pointer by 1
   285  08c2 d002               	BNE +
   286  08c4 c67b               	DEC txtptr+1
   287  08c6 c67a               +	DEC txtptr
   288  08c8 60                 	RTS
   289                          
   290                          ;-----------------------------------------------------------------
   291                          
   292                          ; start parsing an extension command ...
   293                          
   294                          parse
   295  08c9 207300                     JSR chrget		; next char.
   296  08cc c926                       CMP #'&'		; command prefix
   297  08ce f006                       BEQ newcmd
   298  08d0 20c008             	JSR undo_chrget
   299  08d3 6c3403             	JMP (savevpars)
   300                          newcmd
   301  08d6 207300                     JSR chrget		; command character
   302                          
   303  08d9 a00e                       LDY #(cmdsend-cmds)	; map character to
   304                          				; command address ...
   305                          checknextcmd
   306  08db 88                         DEY
   307  08dc f01c               	BEQ parse_error
   308  08de d92b09                     CMP cmds,Y
   309  08e1 d0f8                       BNE checknextcmd	; try next
   310  08e3 88                         DEY			; found
   311  08e4 98                         TYA
   312  08e5 0a                         ASL			; *2
   313  08e6 a8                         TAY
   314                          !ifndef command_rts_tyle {
   315                          	!set co=0		; command offset in jump table
   316  08e7 b93a09                     LDA cmdaddr+1,Y		; high byte from table
   317  08ea 8556                       STA ijmp+1
   318  08ec b93909                     LDA cmdaddr,Y		; low byte from table
   319  08ef 8555                       STA ijmp
   320  08f1 207300                     JSR chrget		; read next byte in basic text
   321  08f4 205400                     JSR ijmp-1		; go to command by JMP (addr)
   322  08f7 4caea7                     JMP b_interpreter	; continue parsing
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
   337  08fa 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
   338                          
   339                          ;-----------------------------------------------------------------
   340                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   341                          express
   342  08fd a900               	LDA #0
   343  08ff 850d               	STA type	
   344  0901 207300             	JSR chrget
   345  0904 b003               	BCS exp_nonumber
   346  0906 4cf3bc             	JMP b_str2fac
   347                          exp_nonumber
   348  0909 c926                       CMP #'&'		; command prefix
   349  090b f006                       BEQ newfunc
   350  090d 20c008             	JSR undo_chrget
   351  0910 6c3a03             	JMP (savevexp)		; original routine	
   352                          ;	JMP b_execexpr
   353                          newfunc
   354  0913 207300             	JSR chrget
   355  0916 c95a               	CMP #'Z'
   356  0918 d003               	BNE +
   357  091a 4cdf0e             	JMP get
   358  091d c958               +	CMP #'X'
   359  091f d003               	BNE +
   360  0921 4cca0e             	JMP getposx
   361  0924 c959               +	CMP #'Y'
   362  0926 d0d2               	BNE parse_error
   363  0928 4cd60e             	JMP getposy
   364                          
   365                          ;-----------------------------------------------------------------
   366                          
   367                          ; the most commonly used command placed at the end ...
   368                          
   369  092b 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   370                          cmdsend
   371                          
   372                          cmdaddr
   373  0939 6112a910bc098a0f...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   374  0945 6210160f4110f10c...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
   375                          
   376  0953 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  095d 312e3334            !text "1.34" 

; ******** Source: graext-core.asm
   378  0961 20313938362c3230...	!text " 1986,2022 JOHANN@KLASEK.AT",0
   379                          
   380                          bitmask
   381  097d 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   382                          nbitmask
   383  0985 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   384                          ytabl
   385  098d 004080c0           	!byte $00,$40,$80,$c0
   386                          ytabh
   387  0991 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   388  0995 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   389  0999 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   390  099d eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   391  09a1 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   392  09a5 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   393  09a9 fe                 	!byte gramp+$1e
   394                          
   395                          ; for horiz. line
   396                          
   397                          maskleft0
   398                          maskleft
   399  09aa ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   400  09b2 00                 	!byte $00
   401                          
   402                          maskright0
   403  09b3 00                 	!byte $00
   404                          maskright
   405  09b4 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   406                          
   407                          ;-----------------------------------------------------------------
   408                          
   409                          graphic
   410  09bc 209eb7                     JSR b_get8bit
   411  09bf e000                       CPX #$00
   412  09c1 d013                       BNE gra_other
   413                          gra0				; &G 0
   414                          gra_off
   415  09c3 a9c7                       LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   416  09c5 8d00dd                     STA cia_pra
   417  09c8 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   418                          				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   419                          				; char addr $1000/4096 = char. ROM
   420  09ca 8d18d0                     STA vic_mcr		; VIC memory control
   421  09cd ad11d0                     LDA vic_cr		; VIC control register
   422  09d0 29df                       AND #%11011111		; Hires mode off
   423  09d2 8d11d0                     STA vic_cr
   424  09d5 60                         RTS
   425                          
   426                          gra_other
   427  09d6 e001                       CPX #$01
   428  09d8 f00f               	BEQ gra1
   429  09da e002               	CPX #$02
   430  09dc f00e                       BEQ gra2
   431  09de e004               	CPX #$04
   432  09e0 f043                       BEQ gra_clear		; &G 4 (erase only, leave mode)
   433  09e2 e003               	CPX #$03		; &G 3 (graphic on)
   434  09e4 f029               	BEQ gra_on
   435  09e6 4c48b2                     JMP b_illquant		; parameter illegal
   436                          	
   437                          gra1				; &G 1
   438  09e9 20250a             	JSR gra_clear
   439                          
   440                          gra2
   441  09ec 20f1b7                     JSR b_getcomma8bit
   442  09ef 8a                         TXA			; foreground color
   443  09f0 0a                         ASL			; upper nibble
   444  09f1 0a                         ASL
   445  09f2 0a                         ASL
   446  09f3 0a                         ASL
   447  09f4 85fd                       STA gcol
   448  09f6 20f1b7                     JSR b_getcomma8bit
   449  09f9 8a                         TXA			; background color
   450  09fa 290f                       AND #$0F
   451  09fc 05fd                       ORA gcol
   452  09fe a000                       LDY #$00
   453                          cram_loop
   454  0a00 9900cc                     STA cram,Y		; fill color RAM
   455  0a03 9900cd                     STA cram+$100,Y
   456  0a06 9900ce                     STA cram+$200,Y
   457  0a09 99e8ce                     STA cram+$300-24,Y
   458  0a0c c8                         INY
   459  0a0d d0f1                       BNE cram_loop
   460                          
   461                          gra_on
   462  0a0f 20440a             	JSR gra_setupcode
   463                          
   464  0a12 a9c4                       LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   465  0a14 8d00dd                     STA cia_pra
   466  0a17 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   467  0a19 8d18d0                     STA vic_mcr		; VIC memory control
   468  0a1c ad11d0                     LDA vic_cr		; VIC control register
   469  0a1f 0920                       ORA #%00100000		; Bit 5 = 1: Hires on
   470  0a21 8d11d0                     STA vic_cr
   471  0a24 60                         RTS
   472                          
   473                          gra_clear
   474  0a25 a220                       LDX #$20		; Pages (8 KByte)
   475  0a27 a9e0                       LDA #>gram
   476  0a29 85fc                       STA gpos+1
   477  0a2b a000                       LDY #$00
   478  0a2d 84fb                       STY gpos
   479  0a2f 98                         TYA
   480                          gra_fill
   481  0a30 91fb                       STA (gpos),Y		; Loop unroll
   482  0a32 c8                         INY
   483  0a33 91fb                       STA (gpos),Y
   484  0a35 c8                         INY
   485  0a36 91fb                       STA (gpos),Y
   486  0a38 c8                         INY
   487  0a39 91fb                       STA (gpos),Y
   488  0a3b c8                         INY
   489  0a3c d0f2                       BNE gra_fill
   490  0a3e e6fc                       INC gpos+1
   491  0a40 ca                         DEX
   492  0a41 d0ed                       BNE gra_fill
   493  0a43 60                 	RTS
   494                          
   495                          gra_setupcode
   496  0a44 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   497                          gra_copycode
   498  0a46 bd670a             	LDA gromcode-1,X
   499  0a49 9dec03             	STA gramcode-1,X
   500  0a4c ca                 	DEX
   501  0a4d d0f7               	BNE gra_copycode
   502  0a4f ad3f03             	LDA savemo
   503  0a52 290f               	AND #$0F
   504  0a54 aa                 	TAX
   505  0a55 4c760e             	JMP setmode_enter	; re-apply mode to routines
   506                          				; implicit RTS
   507                          
   508                          ;-----------------------------------------------------------------
   509                          
   510                          gexit
   511  0a58 a501                       LDA prozport
   512  0a5a 0902                       ORA #%00000010		; kernal ROM enable
   513  0a5c 8501                       STA prozport
   514  0a5e 58                         CLI			; allow interrupts
   515  0a5f 60                         RTS
   516                          
   517                          ;-----------------------------------------------------------------
   518                          
   519                          ginit
   520  0a60 a501                       LDA prozport
   521  0a62 29fd                       AND #%11111101		; Kernal ROM disable
   522  0a64 78                         SEI			; disable interrupts
   523  0a65 8501                       STA prozport
   524  0a67 60                         RTS
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
   544  0a68 b1a5                       LDA (gaddr),Y
   545                          gchange_op
   546  0a6a 1d7d09                     ORA bitmask,X
   547  0a6d 91a5                       STA (gaddr),Y
   548                          !ifdef ltc {
   549                          	LDA #mc_sim		; vollständige ROM-Simulation
   550                          	STA memconf		; wieder schnelles RAM ab $C000
   551                          }
   552  0a6f 60                         RTS
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
   564  0a70 4900                       EOR #$00
   565                          gmask_op
   566  0a72 11a5                       ORA (gaddr),Y
   567  0a74 91a5                       STA (gaddr),Y
   568                          !ifdef ltc {
   569                          	LDA #mc_sim		; vollständige ROM-Simulation
   570                          	STA memconf		; wieder schnelles RAM ab $C000
   571                          }
   572  0a76 60                         RTS
   573                          
   574                          }
   575                          
   576                          gromcode_end
   577                          
   578                          ;-----------------------------------------------------------------
   579                          
   580                          position
   581  0a77 a5aa                       LDA y
   582  0a79 4a                         LSR
   583  0a7a 4a                         LSR
   584  0a7b 4a                         LSR			; y/8
   585  0a7c a8                         TAY
   586  0a7d 2903                       AND #%00000011		; (y/8) mod 4
   587  0a7f aa                         TAX
   588  0a80 a59b                       LDA xl			; x low
   589  0a82 29f8                       AND #%11111000		; clear bit 2-0
   590  0a84 18                         CLC
   591  0a85 7d8d09                     ADC ytabl,X		; addr low: y base + x part
   592  0a88 85a5                       STA gaddr
   593  0a8a a59c                       LDA xh			; addr high: x part
   594  0a8c 799109                     ADC ytabh,Y		; 	+ y base
   595  0a8f 85a6                       STA gaddr+1
   596  0a91 a5aa                       LDA y			; vertical offset
   597  0a93 2907                       AND #%00000111		; y mod 8
   598  0a95 a8                         TAY
   599  0a96 a59b                       LDA xl
   600  0a98 2907                       AND #%00000111		; x mod 8
   601  0a9a aa                         TAX			; horizonal offset
   602  0a9b 60                         RTS			; (bitmask)
   603                          
   604                          
   605                          ;-----------------------------------------------------------------
   606                          
   607                          ; swap tupel xl,xh <-> xendl,xendh
   608                          
   609                          swap_x_xend
   610  0a9c a69e                       LDX xendl		; swap x, xend
   611  0a9e a49b                       LDY xl
   612  0aa0 869b                       STX xl
   613  0aa2 849e                       STY xendl
   614                          
   615  0aa4 a69f                       LDX xendh
   616  0aa6 a49c                       LDY xh
   617  0aa8 849f                       STY xendh
   618  0aaa 869c                       STX xh
   619  0aac 60                 	RTS
   620                          
   621                          
   622                          ;-----------------------------------------------------------------
   623                          
   624                          ; line y up, x left, dx < dy (case 1)
   625                          
   626                          line_up_steep
   627  0aad 20770a                     JSR position		; x,y
   628                          loop_yup_xleft
   629  0ab0 20ed03                     JSR gchange		; pixel
   630                          
   631  0ab3 18                         CLC			; k += dx
   632  0ab4 a595                       LDA kl
   633  0ab6 65ab                       ADC dxl			; dxh is 0, because dx < dy
   634  0ab8 8595                       STA kl
   635  0aba 9014                       BCC +			; k >= 0 ->
   636                          
   637  0abc e5a9               ++	SBC dy			; k -= dy (C=1)
   638  0abe 8595                       STA kl
   639                          
   640  0ac0 ca                  	DEX			; x--
   641  0ac1 100d                       BPL +
   642  0ac3 a207                       LDX #7			; wrap around
   643  0ac5 38                 	SEC
   644  0ac6 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   645  0ac8 e908                       SBC #8
   646  0aca 85a5                       STA gaddr
   647  0acc b002                       BCS +
   648  0ace c6a6                       DEC gaddr+1
   649                          
   650  0ad0 88                 +	DEY			; y--
   651  0ad1 100f                       BPL +++
   652  0ad3 38                         SEC			; y overflow
   653  0ad4 a5a5                       LDA gaddr
   654  0ad6 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   655  0ad8 85a5                       STA gaddr
   656  0ada a5a6                       LDA gaddr+1
   657  0adc e901               	SBC #1
   658  0ade 85a6                       STA gaddr+1
   659  0ae0 a007                       LDY #7			; wrap around
   660                          
   661  0ae2 c6a3               +++	DEC cl			; until c=0
   662  0ae4 d0ca                       BNE loop_yup_xleft
   663  0ae6 4c580a                     JMP gexit
   664                          
   665                          
   666                          ;-----------------------------------------------------------------
   667                          
   668                          ; line x left, y up, dx > dy (case 2)
   669                          
   670                          line_up_flat
   671  0ae9 20770a                     JSR position		; x,y
   672  0aec a5a3               	LDA cl			; counter adjustment for
   673  0aee f002               	BEQ +			; prepare for dec-dec-counting
   674  0af0 e6a4               	INC ch
   675                          +
   676                          loop_xleft_yup
   677  0af2 20ed03                     JSR gchange		; pixel
   678                          
   679  0af5 18                         CLC			; k += dy
   680  0af6 a595                       LDA kl
   681  0af8 65a9                       ADC dy
   682  0afa 8595                       STA kl
   683  0afc 9020                       BCC +			; k < 0
   684  0afe e696                       INC kh
   685  0b00 301c               	BMI +			; k < 0
   686                          
   687  0b02 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   688  0b04 8595                       STA kl
   689  0b06 a596                       LDA kh
   690  0b08 e5a7                       SBC dxh		
   691  0b0a 8596                       STA kh
   692                          
   693  0b0c 88                         DEY			; y--
   694  0b0d 100f                       BPL +
   695  0b0f 38                 	SEC			; C=1 not always true (SBC above)
   696  0b10 a5a5                       LDA gaddr		; y overflow
   697  0b12 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   698  0b14 85a5                       STA gaddr
   699  0b16 a5a6                       LDA gaddr+1
   700  0b18 e901               	SBC #1
   701  0b1a 85a6                       STA gaddr+1
   702  0b1c a007               	LDY #7			; wrap around
   703                          
   704  0b1e ca                 +	DEX			; x--
   705  0b1f 100d                       BPL +++
   706  0b21 a207                       LDX #7			; wrap around
   707  0b23 38                 	SEC
   708  0b24 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   709  0b26 e908                       SBC #8
   710  0b28 85a5                       STA gaddr
   711  0b2a b002                       BCS +++
   712  0b2c c6a6                       DEC gaddr+1
   713                          +++
   714  0b2e c6a3               	DEC cl			; c--
   715  0b30 d0c0                       BNE loop_xleft_yup
   716  0b32 c6a4                       DEC ch			; adjusted high which allows this
   717  0b34 d0bc                       BNE loop_xleft_yup
   718                          
   719  0b36 4c580a                     JMP gexit
   720                          
   721                          
   722                          
   723                          ;-----------------------------------------------------------------
   724                          
   725                          ; line x left, y down, dx > dy (case 3)
   726                          
   727                          line_down_flat
   728  0b39 20770a                     JSR position		; x,y
   729  0b3c a5a3               	LDA cl			; counter adjustment for
   730  0b3e f002               	BEQ +			; prepare for dec-dec-counting
   731  0b40 e6a4               	INC ch
   732                          +
   733                          loop_xleft_ydown
   734  0b42 20ed03                     JSR gchange		; pixel
   735                          
   736  0b45 18                         CLC			; k += dy
   737  0b46 a595                       LDA kl
   738  0b48 65a9                       ADC dy
   739  0b4a 8595                       STA kl
   740  0b4c 9021                       BCC +			; k < 0
   741  0b4e e696                       INC kh
   742  0b50 301d               	BMI +			; k < 0
   743                          
   744  0b52 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   745  0b54 8595                       STA kl
   746  0b56 a596                       LDA kh
   747  0b58 e5a7                       SBC dxh		
   748  0b5a 8596                       STA kh
   749                          
   750  0b5c c8                         INY			; y++
   751  0b5d c008                       CPY #8
   752  0b5f d00e                       BNE +
   753                          	; C=1
   754  0b61 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   755  0b63 693f                       ADC #$40-1		; C already set by CPY
   756  0b65 85a5                       STA gaddr
   757  0b67 a5a6                       LDA gaddr+1
   758  0b69 6901               	ADC #1
   759  0b6b 85a6                       STA gaddr+1
   760  0b6d a000                       LDY #0			; wrap around
   761                          
   762  0b6f ca                 +	DEX			; x--
   763  0b70 100d                       BPL +++
   764  0b72 a207                       LDX #7			; wrap around
   765  0b74 38                 	SEC
   766  0b75 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   767  0b77 e908                       SBC #8
   768  0b79 85a5                       STA gaddr
   769  0b7b b002                       BCS +++
   770  0b7d c6a6                       DEC gaddr+1
   771                          +++
   772  0b7f c6a3               	DEC cl			; c--
   773  0b81 d0bf               	BNE loop_xleft_ydown
   774  0b83 c6a4               	DEC ch			; adjusted high which allows this
   775  0b85 d0bb                       BNE loop_xleft_ydown
   776                          
   777  0b87 4c580a                     JMP gexit
   778                          
   779                          
   780                          ;-----------------------------------------------------------------
   781                          
   782                          ; line y down, x right, dx < dy (case 4)
   783                          
   784                          line_down_steep
   785  0b8a 20770a                     JSR position		; x,y
   786                          loop_ydown_xleft
   787  0b8d 20ed03                     JSR gchange		; pixel
   788                          
   789  0b90 18                         CLC			; k += dx
   790  0b91 a595                       LDA kl
   791  0b93 65ab                       ADC dxl			; dxh is 0, because dx < dy
   792  0b95 8595                       STA kl
   793  0b97 9014                       BCC +			; k >= 0 ->
   794                          
   795  0b99 e5a9               	SBC dy			; k -= dy, C=1
   796  0b9b 8595                       STA kl
   797                          
   798  0b9d ca                  	DEX			; x--
   799  0b9e 100d                       BPL +
   800  0ba0 a207                       LDX #7			; wrap around
   801  0ba2 38                 	SEC
   802  0ba3 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   803  0ba5 e908                       SBC #8
   804  0ba7 85a5                       STA gaddr
   805  0ba9 b002                       BCS +
   806  0bab c6a6                       DEC gaddr+1
   807                          
   808  0bad c8                 +	INY			; y++
   809  0bae c008                       CPY #8			; y overflow?
   810  0bb0 d00e                       BNE +++
   811  0bb2 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   812  0bb4 693f                       ADC #$40-1		; C already set by CPY
   813  0bb6 85a5                       STA gaddr
   814  0bb8 a5a6                       LDA gaddr+1
   815  0bba 6901               	ADC #1
   816  0bbc 85a6                       STA gaddr+1
   817  0bbe a000                       LDY #0			; wrap around
   818                          
   819  0bc0 c6a3               +++	DEC cl			; c--
   820                          				; until c=0
   821  0bc2 d0c9                       BNE loop_ydown_xleft
   822  0bc4 4c580a                     JMP gexit
   823                          
   824                          
   825                          ;-----------------------------------------------------------------
   826                          
   827                          getcommaxy
   828  0bc7 20fdae                     JSR b_getcomma		; check ","
   829                          getxy
   830  0bca 208aad                     JSR b_getval		; get X coord. value
   831  0bcd 20f7b7                     JSR b_convint
   832  0bd0 c901                       CMP #>xmax
   833  0bd2 900c               	BCC gcxy_xok
   834  0bd4 f003                       BEQ ++			; X = $1xx
   835  0bd6 20390e                     JSR range_error
   836                          
   837  0bd9 c040               ++	CPY #<xmax		; check X low
   838  0bdb 9003                       BCC +
   839  0bdd 20390e                     JSR range_error
   840                          +
   841                          gcxy_xok
   842  0be0 84fb                       STY gpos		; temporary save X coord.
   843  0be2 85fc                       STA gpos+1
   844                          
   845  0be4 20f1b7                     JSR b_getcomma8bit
   846                          				; get Y coord. value
   847  0be7 e0c8                       CPX #ymax
   848  0be9 9003                       BCC +
   849  0beb 20390e                     JSR range_error
   850                          +
   851  0bee a4fb                       LDY gpos		; restory X coord.
   852  0bf0 a5fc                       LDA gpos+1
   853  0bf2 60                         RTS
   854                          
   855                          
   856                          ;-----------------------------------------------------------------
   857                          
   858                          para_hline_box
   859  0bf3 20ca0b                     JSR getxy		; get startpoint
   860  0bf6 86aa                       STX y
   861  0bf8 8e3e03                     STX savey		; save as cursor, too
   862  0bfb 859c                       STA xh
   863  0bfd 849b                       STY xl
   864  0bff 8d3d03             	STA savexh
   865  0c02 8c3c03             	STY savexl
   866  0c05 20fdae                     JSR b_getcomma		; get length
   867  0c08 208aad                     JSR b_getval
   868  0c0b 20f7b7                     JSR b_convint
   869                          				; calculate end point
   870  0c0e aa                         TAX			; save length high byte
   871  0c0f 98                         TYA			; length low byte
   872  0c10 18                         CLC
   873  0c11 659b                       ADC xl			; low xend = x+length
   874  0c13 859e                       STA xendl
   875  0c15 a8                 	TAY
   876  0c16 8a                         TXA			; high
   877  0c17 659c                       ADC xh			; high xend = x+length
   878  0c19 859f                       STA xendh
   879  0c1b aa                 	TAX
   880                          
   881  0c1c c901               	CMP #>xmax		; endpoint outside?
   882  0c1e 9005               	BCC +
   883  0c20 d003               	BNE +			; >$200 (512)
   884  0c22 98                 	TYA
   885  0c23 e940               	SBC #<xmax
   886  0c25 60                 +	RTS			; C=1 out of range, C=0 ok
   887                          
   888                          ;-----------------------------------------------------------------
   889                          
   890                          hline
   891  0c26 20f30b             	JSR para_hline_box
   892  0c29 9003               	BCC +
   893  0c2b 20390e             	JSR range_error
   894                          				; XXX xend=xmax-1 ?
   895                          +
   896  0c2e 8e3d03                     STX savexh
   897  0c31 8c3c03                     STY savexl		; also save as final cursor
   898                          
   899  0c34 a900               	LDA #0			; default thickness 0 (means 1 pixel)
   900  0c36 85a3               	STA ycount
   901  0c38 207900             	JSR chrgot		; last char. again
   902  0c3b f019               	BEQ +++			; command end? no optional param.
   903  0c3d 20f1b7             	JSR b_getcomma8bit
   904  0c40 8a                 	TXA			; optional 8-bit parameter
   905  0c41 85a3               	STA ycount		; hline thickness
   906  0c43 f011               	BEQ +++			; 0 means 1 pixel
   907  0c45 18                 	CLC
   908  0c46 65aa               	ADC y			; end position for y coord.
   909  0c48 b004               	BCS +			; > 255
   910  0c4a c9c8               	CMP #ymax
   911  0c4c 9008               	BCC +++
   912                          +				; C=1 from ADC or CMP before
   913  0c4e 20390e             	JSR range_error		; corrupts A
   914                          				; XXX ycount=ymax-y-1 ?
   915                          				; xend >= x
   916  0c51 b003               	BCS hl_noxswap		; always
   917                          
   918                          hline_start
   919  0c53 209c0a             	JSR swap_x_xend		; xend < x, entry from line
   920                          	
   921                          hl_noxswap
   922                          				; xend > x
   923                          +++
   924  0c56 e6a3               	INC ycount		; count to 0
   925  0c58 20600a                     JSR ginit		; map in graphic memory
   926                          
   927  0c5b 20770a                     JSR position		; graphic position x,y
   928                          
   929  0c5e a5a5               	LDA gaddr		; save position for vertical
   930  0c60 85fb               	STA sgaddr
   931  0c62 a5a6               	LDA gaddr+1
   932  0c64 85fc               	STA sgaddr+1
   933  0c66 86ab               	STX xsave
   934  0c68 84a9               	STY ysave
   935                          
   936  0c6a a59e                       LDA xendl
   937  0c6c 2907                       AND #%00000111
   938  0c6e 8596                       STA tmp2		; xend mod 8, mask index
   939  0c70 a59b                       LDA xl
   940  0c72 29f8                       AND #%11111000		; (xl div 8)*8
   941  0c74 8595                       STA tmpbits
   942  0c76 a59e                       LDA xendl		; xend unmasked
   943  0c78 38                         SEC
   944  0c79 e595                       SBC tmpbits		; finally: xend - (x div 8)*8 
   945  0c7b 8595                       STA tmpbits
   946  0c7d a59f                       LDA xendh
   947  0c7f e59c                       SBC xh
   948  0c81 4a                         LSR			; / 8 ->  0-39
   949  0c82 a595                       LDA tmpbits		; only 1 highest bit
   950  0c84 6a                         ROR			; and 3 lower bits
   951  0c85 4a                         LSR
   952  0c86 4a                         LSR
   953                                  			; 8-pixel-blocks count
   954  0c87 85a4               	STA hcount		; save for vertical extension
   955                           
   956                          hl_vertloop
   957  0c89 98                 	TYA			; calculate max. Y in 8x8 block
   958  0c8a 18                 	CLC
   959  0c8b 65a3               	ADC ycount
   960  0c8d c908               	CMP #8
   961  0c8f 9002               	BCC +
   962  0c91 a908               	LDA #8
   963  0c93 85a8               +	STA ylimit
   964                          
   965  0c95 bdaa09                     LDA maskleft,X		; starting mask
   966  0c98 8595               	STA tmpbits
   967  0c9a a6a4               	LDX hcount		; how many blocks
   968                          
   969                          hl_nextblock
   970  0c9c ca                         DEX
   971                          hl_islastblock
   972  0c9d 301d                       BMI hl_lastblock
   973                          				; leave loop if X<0
   974  0c9f a4a9               	LDY ysave
   975  0ca1 a595               -	LDA tmpbits		; mask
   976  0ca3 20f503             	JSR gmask		; first with left end mask
   977  0ca6 c8                 	INY			; vertical down
   978  0ca7 c4a8               	CPY ylimit		; in 8x8 box
   979  0ca9 d0f6               	BNE -
   980                          
   981  0cab 18                         CLC			; gaddr += 8 (one block to right)
   982  0cac a5a5                       LDA gaddr
   983  0cae 6908                       ADC #8
   984  0cb0 85a5                       STA gaddr
   985  0cb2 9002                       BCC +
   986  0cb4 e6a6                       INC gaddr+1
   987                          
   988  0cb6 a9ff               +	LDA #$FF		; following with full 8-pixel mask
   989  0cb8 8595               	STA tmpbits
   990  0cba d0e0               	BNE hl_nextblock	; always
   991                          
   992                          hl_lastblock
   993  0cbc a696                       LDX tmp2		; xend mask index
   994  0cbe 3db409                     AND maskright,X		; current mask combined with mask right end
   995  0cc1 8595               	STA tmpbits		; mask
   996  0cc3 a4a9               	LDY ysave		; start position in 8x8 block
   997  0cc5 a595               -	LDA tmpbits		; mask
   998  0cc7 20f503             	JSR gmask		; modify
   999  0cca c8                 	INY			; vertical down
  1000  0ccb c6a3               	DEC ycount		; overall y counter
  1001  0ccd c4a8               	CPY ylimit
  1002  0ccf d0f4               	BNE -
  1003                          
  1004  0cd1 a5a3               	LDA ycount		; finished
  1005  0cd3 d003               	BNE +			; roll-over into 8x8 block below
  1006  0cd5 4c580a                     JMP gexit		; leave
  1007                          
  1008  0cd8 18                 +	CLC
  1009  0cd9 a5fb               	LDA sgaddr
  1010  0cdb 6940               	ADC #$40		; next 8-pixel row below
  1011  0cdd 85fb               	STA sgaddr		; + $140 (320)
  1012  0cdf 85a5               	STA gaddr
  1013  0ce1 a5fc               	LDA sgaddr+1
  1014  0ce3 6901               	ADC #$01
  1015  0ce5 85fc               	STA sgaddr+1
  1016  0ce7 85a6               	STA gaddr+1
  1017  0ce9 a6ab               	LDX xsave		; initial mask index
  1018  0ceb a000               	LDY #0			; start on top of 8x8
  1019  0ced 84a9               	STY ysave
  1020  0cef f098               	BEQ hl_vertloop
  1021                          ;-----------------------------------------------------------------
  1022                          
  1023                          vline
  1024  0cf1 20ca0b                     JSR getxy		; get startpoint
  1025  0cf4 859c                       STA xh
  1026  0cf6 8d3d03                     STA savexh		; save as cursor too
  1027  0cf9 849b                       STY xl
  1028  0cfb 8c3c03                     STY savexl
  1029  0cfe 8693                       STX yend		; initial point is endpoint
  1030                          
  1031  0d00 20f1b7                     JSR b_getcomma8bit
  1032                          				; get length
  1033  0d03 18                         CLC			; calculate end point
  1034  0d04 8a                         TXA			; length
  1035                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1036                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1037                          ;	STA tmpbits
  1038  0d05 6593                       ADC yend		; length + initial point is startpoint
  1039  0d07 b005               	BCS vline_iq		; > 255
  1040  0d09 c9c8                       CMP #ymax		; outside?
  1041  0d0b a8                 	TAY			; keep startpoint
  1042  0d0c 9003                       BCC +
  1043                          vline_iq
  1044  0d0e 20390e                     JSR range_error		; corrupts A
  1045                          				; XXX Y = ymax-1 ?
  1046  0d11 84aa               +	STY y			; startpoint
  1047  0d13 8c3e03             	STY savey		; set cursor y position
  1048  0d16 18                 	CLC
  1049  0d17 900e               	BCC +++			; skip following, because y, yend are already ordered
  1050                          
  1051                          vline_start			; entry point from line command (only)
  1052  0d19 a5aa               	LDA y			; order of y, yend is not defined
  1053  0d1b c593               	CMP yend
  1054  0d1d b008               	BCS vl_noyswap		; yend > y ->
  1055  0d1f a5aa               	LDA y			; swap y, yend
  1056  0d21 a693               	LDX yend
  1057  0d23 8593               	STA yend
  1058  0d25 86aa               	STX y
  1059                          vl_noyswap
  1060                          				; startpoint is below the endpoint
  1061  0d27 20600a             +++	JSR ginit		; map in graphic memory
  1062                          
  1063                          vl_start
  1064  0d2a 20770a                     JSR position		; graphic position x,y
  1065  0d2d bd7d09                     LDA bitmask,X
  1066  0d30 8596                       STA tmp2		; save mask
  1067                          ; DON'T-CHANGE: replace ...
  1068  0d32 38                         SEC
  1069  0d33 a5aa                       LDA y			; startpoint is greater!
  1070  0d35 e593                       SBC yend		; vertical length
  1071  0d37 aa                         TAX
  1072                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmpbits)
  1073                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1074                          ;	LDX tmpbits
  1075  0d38 e8                         INX			; +1 (exit on 0)
  1076  0d39 38                 	SEC			; for subtraction, never changed!
  1077                          vl_nextline
  1078  0d3a a596                       LDA tmp2
  1079  0d3c 20f503                     JSR gmask		; modify 
  1080  0d3f 88                         DEY			; go up
  1081  0d40 100e                       BPL +
  1082  0d42 a5a5                       LDA gaddr		; C=1
  1083  0d44 e940               	SBC #$40		; gaddr -= 320
  1084  0d46 85a5                       STA gaddr
  1085  0d48 a5a6                       LDA gaddr+1
  1086  0d4a e901                       SBC #$01
  1087  0d4c 85a6                       STA gaddr+1
  1088  0d4e a007                       LDY #7			; wrap y offset
  1089  0d50 ca                 +	DEX			; all vertical positions done?
  1090  0d51 d0e7                       BNE vl_nextline
  1091  0d53 4c580a                     JMP gexit		; leave
  1092                          
  1093                          
  1094                          ;-----------------------------------------------------------------
  1095                          
  1096                          line
  1097  0d56 20ca0b                     JSR getxy		; get startpoint
  1098  0d59 849b                       STY xl 
  1099  0d5b 859c                       STA xh
  1100  0d5d 86aa                       STX y
  1101                          
  1102  0d5f 20c70b                     JSR getcommaxy		; get endpoint
  1103                          line_start
  1104  0d62 8c3c03                     STY savexl		; save as cursor position too
  1105  0d65 849e                       STY xendl
  1106  0d67 8d3d03                     STA savexh
  1107  0d6a 859f                       STA xendh
  1108  0d6c 8e3e03                     STX savey
  1109  0d6f 8693                       STX yend
  1110                          
  1111  0d71 a000                       LDY #$00		; initialize to 0
  1112  0d73 84a8                       STY ydir
  1113  0d75 8495                       STY kl
  1114  0d77 8496                       STY kh
  1115                          
  1116  0d79 38                         SEC
  1117  0d7a a59b                       LDA xl			; calculate dx
  1118  0d7c e59e                       SBC xendl
  1119  0d7e 85ab                       STA dxl
  1120  0d80 a59c                       LDA xh
  1121  0d82 e59f                       SBC xendh
  1122  0d84 85a7                       STA dxh
  1123                          
  1124  0d86 b018                       BCS li_xend_left
  1125                          	; dx != 0
  1126                          				; negate dx:
  1127  0d88 98                         TYA			; Y=A=0
  1128  0d89 38                         SEC			; dx = 0 - dx
  1129  0d8a e5ab                       SBC dxl
  1130  0d8c 85ab                       STA dxl
  1131  0d8e 98                         TYA			; Y=A=0
  1132  0d8f e5a7                       SBC dxh
  1133  0d91 85a7                       STA dxh
  1134                          				; C=0 always, needed later
  1135  0d93 209c0a             	jsr swap_x_xend
  1136  0d96 a6aa                       LDX y			; swap y
  1137  0d98 a493                       LDY yend
  1138  0d9a 8693                       STX yend
  1139  0d9c 84aa                       STY y
  1140                          
  1141  0d9e 9007                       BCC li_x_different
  1142                          				; C=0 always (from negation before)
  1143                          
  1144                          li_xend_left
  1145                                  			; A already contains dxh
  1146  0da0 05ab                       ORA dxl			; dx = 0?
  1147  0da2 d003                       BNE li_x_different
  1148  0da4 4c190d                     JMP vline_start		; vertical line case
  1149                          
  1150                          li_x_different
  1151  0da7 38                         SEC			; calculate dy
  1152  0da8 a593                       LDA yend
  1153  0daa e5aa                       SBC y
  1154  0dac b006                       BCS li_y_right		; yend >= y?
  1155  0dae 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1156  0db0 6901                       ADC #$01		; C=0
  1157  0db2 85a8                       STA ydir		; always not 0: flag y goes up
  1158                          
  1159                          li_y_right
  1160  0db4 85a9                       STA dy
  1161  0db6 d007                       BNE +
  1162  0db8 a900               	LDA #0			; line thickness = 1
  1163  0dba 85a3               	STA ycount
  1164  0dbc 4c530c                     JMP hline_start		; horizontal line case
  1165                          +
  1166                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1167                          
  1168  0dbf a5a7                       LDA dxh			; dx > dy
  1169  0dc1 d01c                       BNE line_flat		; yes -> flat
  1170  0dc3 a5a9                       LDA dy			; no -> steep
  1171  0dc5 aa                         TAX
  1172  0dc6 c5ab                       CMP dxl
  1173  0dc8 9015                       BCC line_flat
  1174                          
  1175                          line_steep
  1176  0dca e8                         INX	
  1177  0dcb 86a3                       STX cl			; c = dy+1
  1178  0dcd 4a                         LSR			; dy/2
  1179  0dce 49ff               	EOR #$FF		; one's complement
  1180  0dd0 8595                       STA kl			; k = -dy/2 -1
  1181                          
  1182  0dd2 20600a                     JSR ginit		; map in graphic memory
  1183                          
  1184  0dd5 a5a8                       LDA ydir
  1185  0dd7 d003                       BNE +
  1186  0dd9 4c8a0b                     JMP line_down_steep	; y down, steep
  1187  0ddc 4cad0a             +	JMP line_up_steep	; y up, steep
  1188                          
  1189                          line_flat
  1190  0ddf a5a7                       LDA dxh
  1191  0de1 a8                         TAY
  1192  0de2 a6ab                       LDX dxl
  1193  0de4 e8                         INX
  1194  0de5 d001                       BNE +
  1195  0de7 c8                         INY
  1196  0de8 86a3               +	STX cl			; c = dx+1
  1197  0dea 84a4                       STY ch
  1198                          
  1199  0dec 4a                         LSR			; dx/2 high
  1200  0ded 49ff               	EOR #$FF		; one's complement
  1201  0def 8596                       STA kh
  1202  0df1 a5ab                       LDA dxl
  1203  0df3 6a                         ROR			; dx/2 low
  1204  0df4 49ff               	EOR #$FF		; one's complement
  1205  0df6 8595                       STA kl			; k = -dx/2 - 1
  1206                          
  1207  0df8 20600a                     JSR ginit		; map in graphic memory
  1208                          
  1209  0dfb a5a8                       LDA ydir	
  1210  0dfd d003                       BNE +
  1211  0dff 4c390b                     JMP line_down_flat	; y down, flat
  1212  0e02 4ce90a             +	JMP line_up_flat	; y up, flat
  1213                          
  1214                          ;-----------------------------------------------------------------
  1215                          
  1216                          plot
  1217  0e05 20ca0b                     JSR getxy		; get parameter
  1218  0e08 859c                       STA xh			; save x/y
  1219  0e0a 849b                       STY xl
  1220  0e0c 86aa                       STX y
  1221  0e0e 8d3d03                     STA savexh		; and store as cursor
  1222  0e11 8c3c03                     STY savexl
  1223  0e14 8e3e03                     STX savey
  1224                          
  1225                          plot_start
  1226  0e17 20770a                     JSR position		; calculate graphical address
  1227                          
  1228  0e1a a501                       LDA prozport
  1229  0e1c 29fd                       AND #%11111101		; Kernal ROM disable
  1230  0e1e 78                         SEI			
  1231  0e1f 8501                       STA prozport
  1232                          
  1233  0e21 20ed03                     JSR gchange		; change graphical data
  1234                          
  1235  0e24 a501                       LDA prozport
  1236  0e26 0902                       ORA #%00000010		; kernal ROM enable
  1237  0e28 8501                       STA prozport
  1238  0e2a 58                         CLI
  1239  0e2b 60                         RTS
  1240                          
  1241                          ;-----------------------------------------------------------------
  1242                          
  1243                          move
  1244  0e2c 20ca0b                     JSR getxy		; get parameter
  1245  0e2f 8d3d03                     STA savexh		; just save as cursor
  1246  0e32 8c3c03                     STY savexl
  1247  0e35 8e3e03                     STX savey
  1248  0e38 60                         RTS
  1249                          
  1250                          
  1251                          ;-----------------------------------------------------------------
  1252                          
  1253                          ; never touches X, Y, C-flag
  1254                          ; on exit: A corrupted, Z=0
  1255                          
  1256                          range_error
  1257  0e39 ad3f03             	LDA savemo
  1258  0e3c 29f0               	AND #$F0
  1259  0e3e d003               	BNE +
  1260                          				; error mode 3: abort command (silent)
  1261  0e40 68                 	PLA			; cleanup JSR
  1262  0e41 68                 	PLA			; highbyte of return address >0
  1263                          
  1264  0e42 60                 -	RTS			; error mode 5: back to command
  1265                          				; to handle value correction
  1266                          				; Z=0
  1267  0e43 2920               +	AND #$20		; mode 5?
  1268  0e45 d0fb               	BNE -			; exit with Z=0
  1269  0e47 68                 	PLA			; error mode 4: terminate with error
  1270  0e48 68                 	PLA			; cleanup JSR
  1271                          setmode_error
  1272  0e49 4c48b2             	JMP b_illquant		; throw error message
  1273                          
  1274                          ;-----------------------------------------------------------------
  1275                          
  1276                          setmode
  1277  0e4c 209eb7                     JSR b_get8bit
  1278  0e4f e003                       CPX #3
  1279  0e51 9017                       BCC +			; less then 3, modification mode
  1280  0e53 e006               	CPX #6
  1281  0e55 b0f2               	BCS setmode_error	; out of range
  1282                          				; error mode
  1283  0e57 8a                 	TXA
  1284  0e58 e902               	SBC #2			; C=0, therefore -3
  1285  0e5a 0a                 	ASL			; 0-2 -> 16,32 or 48
  1286  0e5b 0a                 	ASL			; shift to upper nibble
  1287  0e5c 0a                 	ASL
  1288  0e5d 0a                 	ASL
  1289                          				; put A's bit 4-7 into savemo
  1290  0e5e 4d3f03             	EOR savemo		; ********
  1291  0e61 29f0               	AND #%11110000		; ****0000
  1292  0e63 4d3f03             	EOR savemo		; AAAAmmmm
  1293  0e66 8d3f03             	STA savemo		; 
  1294  0e69 60                 	RTS
  1295                          
  1296  0e6a 8a                 +	TXA
  1297  0e6b 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1298  0e6e 290f               	AND #%00001111
  1299  0e70 4d3f03             	EOR savemo
  1300  0e73 8d3f03             	STA savemo
  1301                          setmode_enter
  1302  0e76 e001               	CPX #$01
  1303  0e78 b01a                       BCS set_or_toggle
  1304                          
  1305                          modereset
  1306  0e7a a909                       LDA #>(nbitmask)
  1307  0e7c 8df103                     STA gchange_op+2
  1308  0e7f a985                       LDA #<(nbitmask)
  1309  0e81 8df003                     STA gchange_op+1
  1310  0e84 a93d                       LDA #$3D		; opcode AND abs,X
  1311  0e86 8def03                     STA gchange_op
  1312  0e89 a931                       LDA #$31		; opcode AND (zp),Y
  1313  0e8b 8df703                     STA gmask_op
  1314  0e8e a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1315  0e90 8df603                     STA gmask_flip+1
  1316  0e93 60                         RTS
  1317                          
  1318                          set_or_toggle
  1319  0e94 d01a                       BNE modetoggle
  1320                          modeset
  1321  0e96 a909                       LDA #>(bitmask)
  1322  0e98 8df103                     STA gchange_op+2
  1323  0e9b a97d                       LDA #<(bitmask)
  1324  0e9d 8df003                     STA gchange_op+1
  1325  0ea0 a91d                       LDA #$1D		; opcode OR abs,X
  1326  0ea2 8def03                     STA gchange_op
  1327  0ea5 a911                       LDA #$11		; opcode OR (zp),Y
  1328  0ea7 8df703                     STA gmask_op
  1329  0eaa a900                       LDA #$00		; mask, EOR #$00, not inverting
  1330  0eac 8df603                     STA gmask_flip+1
  1331  0eaf 60                         RTS
  1332                          
  1333                          modetoggle
  1334  0eb0 a909                       LDA #>(bitmask)
  1335  0eb2 8df103                     STA gchange_op+2
  1336  0eb5 a97d                       LDA #<(bitmask)
  1337  0eb7 8df003                     STA gchange_op+1
  1338  0eba a95d                       LDA #$5D		; opcode EOR abs,X
  1339  0ebc 8def03                     STA gchange_op
  1340  0ebf a951                       LDA #$51		; opcode EOR (zp),Y
  1341  0ec1 8df703                     STA gmask_op
  1342  0ec4 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1343  0ec6 8df603                     STA gmask_flip+1
  1344  0ec9 60                         RTS
  1345                          
  1346                          
  1347                          ;-----------------------------------------------------------------
  1348                          ; get current x cursor position
  1349                          
  1350                          getposx
  1351  0eca ac3c03             	LDY savexl
  1352  0ecd ad3d03             	LDA savexh
  1353  0ed0 2091b3             	JSR b_word2fac
  1354  0ed3 4c7300             	JMP chrget		; last position of expression (function name)
  1355                          
  1356                          ;-----------------------------------------------------------------
  1357                          ; get current y cursor position
  1358                          
  1359                          getposy
  1360  0ed6 ac3e03             	LDY savey
  1361  0ed9 20a2b3             	JSR b_byte2fac
  1362  0edc 4c7300             	JMP chrget		; last position of expression (function name)
  1363                          
  1364                          ;-----------------------------------------------------------------
  1365                          
  1366                          ; get pixel (check if pixel set)
  1367                          ; not used
  1368                          
  1369                          get
  1370  0edf 207300             	JSR chrget		; advance past function name
  1371  0ee2 20faae             	JSR b_chkparl		; "("?
  1372  0ee5 20ca0b                     JSR getxy		; get X,Y values
  1373  0ee8 859c                       STA xh
  1374  0eea 849b                       STY xl
  1375  0eec 86aa                       STX y
  1376  0eee 207900             	JSR chrgot
  1377  0ef1 20f7ae             	JSR b_chkparr		; ")"?
  1378                          	
  1379                          
  1380  0ef4 20770a                     JSR position		; calculate graphic address/position
  1381                          
  1382  0ef7 a501                       LDA prozport
  1383  0ef9 29fd               	AND #%11111101		; Kernal ROM disable
  1384  0efb 78                         SEI
  1385  0efc 8501                       STA prozport
  1386                          
  1387  0efe b1a5                       LDA (gaddr),Y
  1388  0f00 3d7d09                     AND bitmask,X		; mask position
  1389  0f03 a8                         TAY
  1390  0f04 a501                       LDA prozport
  1391  0f06 0902               	ORA #%00000010		; kernal ROM enable
  1392  0f08 8501                       STA prozport
  1393  0f0a 58                         CLI
  1394  0f0b 98                 	TYA
  1395  0f0c f002               	BEQ +
  1396  0f0e a001               	LDY #1			; <> 0 -> always return 1
  1397  0f10 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1398                          
  1399                          ;-----------------------------------------------------------------
  1400                          
  1401                          relto_cont
  1402                          				; continue
  1403  0f13 207300             	JSR chrget		; skip TO token
  1404                          relto
  1405  0f16 208aad                     JSR b_getval		; get X offset (+/-)
  1406  0f19 a561               	LDA facexp		; FAC exponent
  1407  0f1b c990               	CMP #$90		; more than 16 bit
  1408  0f1d b031               	BCS relto_error		; illegal quantity
  1409  0f1f 209bbc                     JSR b_fac2int		; to signed integer
  1410                          
  1411  0f22 18                         CLC
  1412  0f23 a565                       LDA facintl
  1413  0f25 6d3c03                     ADC savexl
  1414  0f28 859e                       STA xendl
  1415  0f2a a564                       LDA facinth
  1416  0f2c 6d3d03                     ADC savexh
  1417  0f2f 859f                       STA xendh		; xend = savex+facint
  1418                          
  1419  0f31 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1420  0f34 208aad                     JSR b_getval
  1421  0f37 a561                       LDA facexp		; FAC exponent
  1422  0f39 c990                       CMP #$90		; more than 16 bit
  1423  0f3b b013                       BCS relto_error		; illegal quantity
  1424  0f3d 209bbc                     JSR b_fac2int		; to signed integer
  1425  0f40 18                         CLC
  1426  0f41 a565                       LDA facintl
  1427  0f43 6d3e03                     ADC savey
  1428  0f46 8593                       STA yend		; yend = savey+facint
  1429                          
  1430  0f48 a59f                       LDA xendh		; check end coord. x
  1431  0f4a c901                       CMP #>xmax
  1432  0f4c 900e                       BCC rt_xok
  1433  0f4e f003                       BEQ +
  1434                          relto_error
  1435  0f50 20390e                     JSR range_error
  1436  0f53 a59e               +	LDA xendl
  1437  0f55 c940                       CMP #<xmax
  1438  0f57 9003                       BCC +
  1439  0f59 20390e                     JSR range_error
  1440                          +
  1441                          rt_xok
  1442  0f5c a593                       LDA yend		; check end coord. y
  1443  0f5e c9c8                       CMP #ymax
  1444  0f60 9003                       BCC +
  1445  0f62 20390e                     JSR range_error
  1446                          +
  1447  0f65 ad3c03                     LDA savexl
  1448  0f68 859b                       STA xl
  1449  0f6a ad3d03                     LDA savexh
  1450  0f6d 859c                       STA xh
  1451  0f6f ad3e03                     LDA savey
  1452  0f72 85aa                       STA y
  1453  0f74 a49e                       LDY xendl
  1454  0f76 a59f                       LDA xendh
  1455  0f78 a693                       LDX yend		; xend/yend = cursor + x/y
  1456                          
  1457  0f7a 20620d                     JSR line_start		; draw line x/y to xend/yend
  1458                          
  1459  0f7d 207900             	JSR chrgot
  1460  0f80 d001               	BNE +
  1461  0f82 60                 	RTS
  1462  0f83 c9a4               +	CMP #t_to		; TO keyword?
  1463  0f85 f08c               	BEQ relto_cont
  1464  0f87 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1465                          
  1466                          ;-----------------------------------------------------------------
  1467                          
  1468                          char
  1469  0f8a 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1470  0f8d e028                       CPX #40	
  1471  0f8f 9003                       BCC +
  1472                          char_error
  1473  0f91 4c48b2                     JMP b_illquant
  1474  0f94 86fb               +	STX gpos		; save x coord.
  1475  0f96 20f1b7                     JSR b_getcomma8bit
  1476                          				; get char. position y 0-24
  1477  0f99 e019                       CPX #25
  1478  0f9b b0f4                       BCS char_error
  1479  0f9d 86fc                       STX gpos+1		; save y coord.
  1480                          
  1481  0f9f 20fdae                     JSR b_getcomma		; get string
  1482  0fa2 209ead                     JSR b_getexpr
  1483  0fa5 20a3b6                     JSR b_stringval		 ; string address in str
  1484  0fa8 48                         PHA			; string length
  1485  0fa9 a6fc                       LDX gpos+1		; y coord. for char. position
  1486  0fab 8a                         TXA
  1487  0fac 2903                       AND #$03		; mask 2 bits
  1488  0fae a8                         TAY			; table index
  1489  0faf a900                       LDA #$00
  1490  0fb1 85fc                       STA gpos+1		; x high
  1491  0fb3 a5fb                       LDA gpos		; saved x: multiply by 8
  1492  0fb5 0a                         ASL
  1493  0fb6 0a                         ASL
  1494  0fb7 0a                         ASL
  1495  0fb8 26fc                       ROL gpos+1		; overflow to high byte
  1496  0fba 798d09                     ADC ytabl,Y
  1497  0fbd 85a5                       STA gaddr
  1498  0fbf a5fc                       LDA gpos+1		; x high
  1499  0fc1 7d9109                     ADC ytabh,X
  1500  0fc4 85a6                       STA gaddr+1
  1501  0fc6 68                         PLA			; string length
  1502  0fc7 a000                       LDY #$00		; string index
  1503  0fc9 aa                         TAX			; length
  1504  0fca e8                         INX			; prepare as counter
  1505                          char_loop
  1506  0fcb ca                         DEX
  1507  0fcc f008                       BEQ char_exit
  1508  0fce b122                       LDA (str),Y		; read string
  1509  0fd0 20d70f                     JSR char_display
  1510  0fd3 c8                         INY
  1511  0fd4 d0f5                       BNE char_loop
  1512                          char_exit
  1513  0fd6 60                         RTS
  1514                          
  1515                          char_display
  1516  0fd7 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1517  0fd9 8a                         TXA			; save register X+Y
  1518  0fda 48                         PHA
  1519  0fdb 98                         TYA
  1520  0fdc 48                         PHA
  1521  0fdd a5d7                       LDA z_tmp		; get saved character
  1522  0fdf 3012                       BMI char_inverse
  1523                          
  1524                          char_normal
  1525  0fe1 c920                       CMP #$20		; control character?
  1526  0fe3 9054                       BCC char_disp_leave
  1527  0fe5 c960                       CMP #$60
  1528  0fe7 9004                       BCC +
  1529  0fe9 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1530  0feb d014                       BNE char_hires
  1531  0fed 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1532  0fef d010               	BNE char_hires
  1533  0ff1 f00e               	BEQ char_hires
  1534                          
  1535                          char_inverse
  1536  0ff3 297f                       AND #%01111111		; mask bit 7
  1537  0ff5 c97f                       CMP #%01111111		; was 255? (pi)
  1538  0ff7 d002                       BNE +
  1539  0ff9 a95e                       LDA #$5E		; screen code for pi
  1540  0ffb c920               +	CMP #$20		; control character?
  1541  0ffd 903a                       BCC char_disp_leave
  1542                          				; yes, skip
  1543  0fff 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1544                          				; $C0-$FF -> $40-$7F
  1545                          				; OPT: BNE char_hires
  1546                          				; OPT: char_normal
  1547                          char_hires
  1548  1001 a6c7                       LDX z_reverseflag
  1549  1003 f002                       BEQ +
  1550  1005 0980                       ORA #%10000000		; invert char.
  1551  1007 aa                 +	TAX			; save char. for later
  1552  1008 a501                       LDA prozport		; save prozport state
  1553  100a 48                 	PHA
  1554  100b a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1555  100d 78                         SEI
  1556  100e 8501                       STA prozport		; char. rom base = $D000
  1557  1010 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1558  1012 85fc                       STA gpos+1		; 
  1559  1014 8a                         TXA			; char. code
  1560  1015 0a                         ASL			; *8
  1561  1016 26fc                       ROL gpos+1
  1562  1018 0a                         ASL
  1563  1019 26fc                       ROL gpos+1
  1564  101b 0a                         ASL
  1565  101c 26fc                       ROL gpos+1
  1566  101e 85fb                       STA gpos		; addr. in char. rom for char.
  1567                          
  1568  1020 a007                       LDY #$07		; 8 hires lines
  1569                          char_line
  1570  1022 b1fb                       LDA (gpos),Y		; read character line
  1571  1024 20f503                     JSR gmask		; write to hires screen
  1572  1027 88                         DEY
  1573  1028 10f8                       BPL char_line
  1574                          
  1575  102a 68                 	PLA
  1576  102b 8501                       STA prozport
  1577  102d 58                         CLI
  1578                          
  1579  102e 18                         CLC			; step char position to left
  1580  102f a5a5                       LDA gaddr		; ( +8 )
  1581  1031 6908                       ADC #$08
  1582  1033 85a5                       STA gaddr
  1583  1035 9002                       BCC +
  1584  1037 e6a6                       INC gaddr+1
  1585                          +
  1586                          char_disp_leave
  1587  1039 68                 	PLA			; pass written character back
  1588  103a a8                         TAY			; restore saved registers
  1589  103b 68                         PLA
  1590  103c aa                         TAX
  1591  103d 60                 -       RTS
  1592                          
  1593                          
  1594                          ;-----------------------------------------------------------------
  1595                          
  1596                          to_cont
  1597                          				; continue
  1598  103e 207300             	JSR chrget		; skip TO token
  1599                          to
  1600  1041 ad3c03                     LDA savexl
  1601  1044 859b                       STA xl
  1602  1046 ad3d03                     LDA savexh
  1603  1049 859c                       STA xh
  1604  104b ad3e03                     LDA savey
  1605  104e 85aa                       STA y
  1606  1050 20ca0b                     JSR getxy
  1607  1053 20620d                     JSR line_start
  1608  1056 207900             	JSR chrgot
  1609  1059 f0e2               	BEQ -
  1610  105b c9a4               	CMP #t_to		; TO keyword?
  1611  105d f0df               	BEQ to_cont
  1612  105f 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1613                          
  1614                          ;-----------------------------------------------------------------
  1615                          
  1616                          box
  1617  1062 20f30b                     JSR para_hline_box
  1618  1065 9003               	BCC +
  1619  1067 20390e             	JSR range_error
  1620                          				; XXX xend=xmax-1 ?
  1621                          +
  1622  106a 20f1b7             	JSR b_getcomma8bit
  1623  106d 8a                 	TXA			; optional 8-bit parameter
  1624                          				; height
  1625  106e f00c               	BEQ +++			; 0 means 1, box is just a line
  1626  1070 18                 	CLC
  1627  1071 65aa               	ADC y			; end position for y coord.
  1628  1073 b004               	BCS +			; > 255
  1629  1075 c9c8               	CMP #ymax
  1630  1077 9003               	BCC +++
  1631                          +				; C=1 from ADC or CMP before
  1632  1079 20390e             	JSR range_error		; corrupts A
  1633                          				; XXX ycount=ymax-y-1 ?
  1634                          				; xend >= x
  1635  107c 48                 +++	PHA			; yend
  1636  107d a900               	LDA #0
  1637  107f 85a3               	STA ycount		; line thickness 1
  1638  1081 20560c             	JSR hl_noxswap		; upper horizontal line
  1639                          
  1640                          				; right vertical line
  1641  1084 68                 	PLA			; if 0, heigth is 1
  1642  1085 d001               	BNE +			; no 
  1643  1087 60                 	RTS			; exit, if box is degenerated (line)
  1644  1088 a6aa               +	LDX y			; start point at higher values
  1645  108a 85aa               	STA y
  1646  108c 8693               	STX yend
  1647  108e a59e               	LDA xendl
  1648  1090 859b               	STA xl
  1649  1092 a59f               	LDA xendh
  1650  1094 859c               	STA xh
  1651  1096 20270d             	JSR vl_noyswap		; xend,yend -> xend,y
  1652                          				; lower horizontal line
  1653  1099 ad3c03             	LDA savexl
  1654  109c 859b               	STA xl
  1655  109e ad3d03             	LDA savexh
  1656  10a1 859c               	STA xh			; xend already set
  1657  10a3 20560c             	JSR hl_noxswap		; x,yend -> xend,yend
  1658                          				; left vertical line
  1659  10a6 4c270d             	JMP vl_noyswap		; x,y -> x,xend
  1660                          
  1661                          ;-----------------------------------------------------------------
  1662                          
  1663                          fill
  1664  10a9 20ca0b             	JSR getxy
  1665  10ac 859c               	STA xh			; save x/y
  1666  10ae 849b               	STY xl
  1667  10b0 86aa               	STX y
  1668  10b2 8d3d03             	STA savexh		; and store as cursor
  1669  10b5 8c3c03             	STY savexl
  1670  10b8 8e3e03             	STX savey
  1671                                  
  1672  10bb a531                       LDA basaryend		; initialize fill stack pointer
  1673  10bd 38                 	SEC
  1674  10be e904               	SBC #fesize		; one element below
  1675  10c0 85fd               	STA fstack		; use space between basic arrays
  1676  10c2 a532               	LDA basaryend+1		; and string heap bottom
  1677  10c4 e900               	SBC #0			; take borrow
  1678  10c6 85fe               	STA fstack+1
  1679                          
  1680  10c8 20770a             	JSR position		; graphic position in (gaddr)+Y, bit X
  1681  10cb bd7d09             	LDA bitmask,X		; start pixel
  1682  10ce 85a3               	STA tmpmask		; initial single pixel mask
  1683                          
  1684  10d0 a59c               	LDA xh			; setup 8x8 block index (x8)
  1685  10d2 4a                 	LSR			; high bit into C
  1686  10d3 a59b               	LDA xl
  1687  10d5 6a                 	ROR			; take high bit
  1688  10d6 4a                 	LSR
  1689  10d7 4a                 	LSR			; finally divide by 8
  1690  10d8 85a7               	STA x8			; = index of 8x8 block in bitmap
  1691                          
  1692  10da 20600a             	JSR ginit		; map in bitmap memory
  1693                          
  1694                          	; set fmode (from mode)
  1695  10dd ad3f03             	LDA savemo
  1696  10e0 2901               	AND #1			; mode = 0 -> invertmask: $FF
  1697  10e2 38                 	SEC			; mode = 1 -> invertmask: $00
  1698  10e3 e901               	SBC #1			; mode = 2 -> same as mode=0
  1699  10e5 85a8               	STA fmode		; mode set or reset
  1700                          
  1701                                  ; test start pixel
  1702  10e7 51a5                       EOR (gaddr),Y           ; bitmap according to mode
  1703  10e9 8595                       STA tmpbits             ; mask bits
  1704  10eb 24a3                       BIT tmpmask             ; check single bit, and preserve A
  1705  10ed f00f                       BEQ +			; not set, bit position already in X
  1706  10ef 4c580a                     JMP gexit		; set, we are finished early
  1707                          
  1708                          f_line				; start fill in the mid of a line ...
  1709                          
  1710                          	; Get the index of the first leftmost unset pixel inside tmpmask.
  1711                          	; Just the single leftmost gap is filled, others are processed later
  1712                          	; from the element left on the stack.
  1713                          	; Normally comming from process_stack.
  1714                          
  1715                          	; set bits outside mask to 1
  1716  10f2 a5a3               	LDA tmpmask		; 00011100
  1717  10f4 49ff               	EOR #$ff		; 11100011
  1718  10f6 0595               	ORA tmpbits		; 00101010 merge with graphic pixel data
  1719                          				; 11101011 pixel outside tmpmask now set! 
  1720  10f8 a2ff               	LDX #$ff		; pixel gap search: first one from left
  1721  10fa e8                 -	INX
  1722  10fb 0a                 	ASL			; counting from left
  1723  10fc b0fc               	BCS -			; loop if pixel is set
  1724                          				; bit number of the leftmost unset pixel in X
  1725                          
  1726                          	; in: pixels in tmpbits, bit index in X
  1727  10fe a900               +	LDA #0			; initialize continuation flag
  1728  1100 8596               	STA fcont		; for line above und below
  1729  1102 a595               	LDA tmpbits		; 01000010 graphic pixel data
  1730  1104 3db409             	AND maskright,X		; 1111X000 clear right from starting point
  1731  1107 d01b               	BNE left_border		; 01000000 left border remains if any
  1732                          	; open to left, continue left
  1733  1109 a9ff               	LDA #$ff		; no left border, next block to left
  1734  110b 85a3               	STA tmpmask		; initial mask full pixel line
  1735                          stepleft8
  1736  110d a6a7               	LDX x8 			; 8x8 block position
  1737  110f f019               	BEQ left_end		; hit left screen border, X=0 -> tmpmask=$FF!
  1738  1111 c6a7               	DEC x8			; count step 8x8 block to left
  1739                          
  1740  1113 38                 	SEC 			; graphic address to to next pixel line/block
  1741  1114 a5a5               	LDA gaddr
  1742  1116 e908               	SBC #8			; graphic address -8 -> next block left
  1743  1118 b002               	BCS +
  1744  111a c6a6               	DEC gaddr+1		; carry to high byte 
  1745  111c 85a5               +	STA gaddr
  1746                          
  1747                          	; y left unchanged
  1748  111e b1a5               	LDA (gaddr),Y		; real graphic pixel data from bitmap
  1749  1120 45a8               	EOR fmode		; set/reset mode
  1750  1122 f0e9               	BEQ stepleft8		; step block left if empty (no border)
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
  1771  1124 a200               	LDX #0
  1772  1126 e8                 -	INX
  1773  1127 0a                 	ASL			; shift to left
  1774  1128 d0fc               	BNE -			; until byte is empty
  1775                          
  1776                          left_end
  1777  112a bdaa09             	LDA maskleft0,X		; get a mask from the left border to right
  1778                          				; 00X11111
  1779  112d d014               	BNE right_start		; start to walk and fill towards the right border
  1780                          				; empty mask immediate continue to right
  1781                          stepright8
  1782  112f e6a7               	INC x8			; step right a block
  1783  1131 a5a7               	LDA x8
  1784  1133 c928               	CMP #40			; beyond last horizontal block?
  1785  1135 b078               	BCS process_stack	; done if right screen border
  1786                          	; C = 0
  1787  1137 a5a5               	LDA gaddr		; advance to block right
  1788  1139 6908               	ADC #8			; gaddr = gaddr + 8
  1789  113b 9002               	BCC +
  1790  113d e6a6               	INC gaddr+1		; carry to high byte
  1791  113f 85a5               +	STA gaddr
  1792  1141 a9ff               	LDA #$ff		; force "all pixels" mask, because intial
  1793                          				; mask might be a partial one
  1794                          right_start
  1795  1143 85a3               	STA tmpmask		; 00111111 store all/distinct mask
  1796  1145 b1a5               	LDA (gaddr),Y		; 01000010 pixel data
  1797  1147 45a8               	EOR fmode		; set/reset mode
  1798  1149 25a3               	AND tmpmask		; 00000010 mask out left border and beyond
  1799  114b f00f               	BEQ fill_to_right	; empty -> finally start to fill
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
  1818  114d a208               	LDX #8
  1819  114f ca                 -	DEX
  1820  1150 4a                 	LSR			; shift to right
  1821  1151 d0fc               	BNE -			; until byte is empty
  1822                          
  1823                          	; search right border
  1824  1153 bdb309             	LDA maskright0,X	; 11111X00 mask out the right part
  1825  1156 25a3               	AND tmpmask		; 00111111 intersect with mask from left
  1826                          				; 00111100
  1827  1158 f055               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1828                          				; leading to 0 mask (fill_check wont't
  1829                          				; handle this special case)
  1830  115a 85a3               	STA tmpmask		; 00111100 save intersected masks from left and right
  1831                          				; continue to fill to right ...
  1832                          fill_to_right			; fill loop towards right border
  1833  115c a5a3               	LDA tmpmask		; fill mask
  1834                          				; assert:    (bitmap & tempmask) == 0
  1835                          				;         || (bitmap & tempmask) == tempmask
  1836  115e 51a5               	EOR (gaddr),Y		; set/reset to fill
  1837  1160 91a5               	STA (gaddr),Y		; into bitmap - the actual fill action!
  1838                          	
  1839                          check_above
  1840  1162 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1841                          				; c = 0!
  1842  1164 84a9               	STY ysave		; to be restored later
  1843  1166 a5a5               	LDA gaddr		; current graphic position
  1844  1168 85fb               	STA caddr		; check position
  1845  116a a6a6               	LDX gaddr+1
  1846  116c 88                 	DEY			; line above
  1847  116d 100e               	BPL +			; leaving 8x8 block?
  1848                          	; c=0 (asl fcont)
  1849  116f e93f               	SBC #$40-1		; block above:
  1850  1171 85fb               	STA caddr		; caddr = gaddr - $140
  1851                          
  1852  1173 ca                 	DEX			; subtract high byte
  1853  1174 b001               	BCS ++			; borrow from low byte
  1854  1176 ca                 	DEX			; subtract borrow
  1855  1177 e0e0               ++	CPX #>gram		; still graphic ram?
  1856                          
  1857  1179 9007               	BCC skip_above
  1858  117b a007               	LDY #7			; last line in block in new block
  1859  117d 86fc               +	STX caddr+1		; shared store
  1860  117f 20fa11             	JSR fill_check
  1861                          skip_above
  1862                          
  1863                          check_below
  1864  1182 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1865  1184 a5a5               	LDA gaddr		; current graphic position
  1866  1186 85fb               	STA caddr		; check position
  1867  1188 a6a6               	LDX gaddr+1
  1868  118a a4a9               	LDY ysave		; restore original y position
  1869  118c c8                 	INY			; line below
  1870  118d c008               	CPY #8			; crossing 8x8 block?
  1871  118f 9013               	BCC +			; less then 8
  1872                          	; c=1 (cpy)
  1873  1191 693f               	ADC #$40-1		; block below: accu has gaddr
  1874  1193 85fb               	STA caddr		; caddr = gaddr + $140
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
  1886  1195 a8                 	TAY			; for compare later
  1887  1196 8a                 	TXA			; gaddr high
  1888  1197 6901               	ADC #$01
  1889  1199 aa                 	TAX
  1890  119a b00d               	BCS skip_below		; > $10000  -> skip
  1891  119c c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1892  119e e9ff               	SBC #>(gram+8000)
  1893                          	; 16 T
  1894  11a0 b007               	BCS skip_below		; greater, so skip
  1895  11a2 a000               +++	LDY #0			; first line in block
  1896  11a4 86fc               +	STX caddr+1		; shared store
  1897  11a6 20fa11             	JSR fill_check
  1898                          skip_below
  1899                          
  1900  11a9 a4a9               	LDY ysave		; restore original y position
  1901  11ab 46a3               	LSR tmpmask
  1902                          				; bit 0 to carry, open to right, continue?
  1903  11ad b080               	BCS stepright8		; to next block if open
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
  1916  11af a5fd               	LDA fstack		; stack empty?
  1917  11b1 c531               	CMP basaryend
  1918  11b3 a5fe               	LDA fstack+1
  1919  11b5 e532               	SBC basaryend+1
  1920  11b7 b003               	BCS +			; fstack >= basaryend -> not empty
  1921  11b9 4c580a             	JMP gexit		; empty, we are finished
  1922                          
  1923                          	; top of stack: fetched multiple times until mask is completly filled!
  1924  11bc a003               +	LDY #fesize-1		; element's last component
  1925                          !ifndef opt_space {
  1926  11be b1fd               	LDA (fstack),Y
  1927  11c0 85a7               	STA x8			; 8x8 block position
  1928  11c2 88                 	DEY
  1929                          }
  1930  11c3 b1fd               	LDA (fstack),Y
  1931  11c5 85a3               	STA tmpmask		; pixel mask
  1932  11c7 88                 	DEY
  1933  11c8 b1fd               	LDA (fstack),Y
  1934  11ca 85a6               	STA gaddr+1		; graphic addr high byte
  1935  11cc 88                 	DEY
  1936  11cd b1fd               	LDA (fstack),Y		; graphic addr low byte combined with y-line
  1937  11cf aa                 	TAX			; needed twice
  1938  11d0 29f8               	AND #%11111000		; split off address
  1939  11d2 85a5               	STA gaddr
  1940                          !ifdef opt_space {
  1941                          	ORA #%00000100		; end bit marker (if 0 all bits are shifted)
  1942                          	STA x8			; low byte without least significant 3 bits
  1943                          				; x8 temporary reused. Calculated later ...
  1944                          }
  1945  11d4 8a                 	TXA
  1946  11d5 2907               	AND #%00000111		; split off y-line
  1947  11d7 a8                 	TAY
  1948                          
  1949  11d8 b1a5               	LDA (gaddr),Y		; get pixels
  1950  11da 45a8               	EOR fmode		; according to set/reset
  1951  11dc 8595               	STA tmpbits		; keep it for later
  1952  11de 25a3               	AND tmpmask		; focus on masked pixels
  1953  11e0 08                 	PHP			; save Z flag
  1954  11e1 f004               	BEQ pop_stack		; all bits unset, remove from stack, because
  1955                          				; it could be filled in one step!
  1956  11e3 c5a3               	CMP tmpmask		; all gaps filled?
  1957  11e5 d00f               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1958                          	; all gaps filled, next on stack 
  1959                          pop_stack
  1960  11e7 38                 	SEC	
  1961  11e8 a5fd               	LDA fstack		; remove entry from stack
  1962  11ea e904               	SBC #fesize		; entry size
  1963  11ec 85fd               	STA fstack
  1964  11ee b002               	BCS +
  1965  11f0 c6fe               	DEC fstack+1
  1966  11f2 28                 +	PLP			; all bits to fill empty?
  1967  11f3 d0ba               	BNE process_stack	; all masked bits are set, next stack element
  1968                          				; all bits unset,
  1969  11f5 24                 	!by $24			; = bit $ll, skip next statement (1 byte)
  1970                          				; stack already cleaned up
  1971  11f6 28                 +++	PLP			; notstack cleanup
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
  1994  11f7 4cf210             	JMP f_line		; long (to far away) jump to fill line start
  1995                          
  1996                          
  1997                          ; Check upper or lower fill path
  1998                          ;	in: caddr, fmode, tmpmask, fcont, fstack(, x8)
  1999                          ;	out: fcont, fstack
  2000                          ;	destroys: A,X,Y
  2001                          
  2002                          fill_check
  2003  11fa b1fb               	LDA (caddr),Y
  2004  11fc 45a8               	EOR fmode		; pixel data
  2005  11fe aa                 	TAX			; save for later
  2006  11ff 25a3               	AND tmpmask		; mask to fill
  2007  1201 f013               	BEQ fc_cleared		; all masked pixels cleared?
  2008  1203 c5a3               	CMP tmpmask		; check for gaps
  2009  1205 f01b               	BEQ fc_exit		; all gaps filled, finished
  2010                          				; if not so, some pixels still set
  2011  1207 a5a3               	LDA tmpmask
  2012                          fc_checkstart			; no continuation, init flag based on
  2013                          				; rightmost pixel:
  2014  1209 4a                 	LSR			; mask bit 0 to carry
  2015  120a 9017               	BCC fc_nocont		; maskbit empty?
  2016  120c 8a                 	TXA			; pixel data
  2017  120d 4a                 	LSR			; pixel bit 0 to carry
  2018  120e b013               	BCS fc_nocont		; bit 0 set
  2019                          				; -> mask is 1 and pixel 0
  2020                          fc_cont
  2021  1210 a596               	LDA fcont		; set flag for continuation
  2022  1212 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  2023  1214 d011               	BNE push_to_stack	; always non zero
  2024                          
  2025                          fc_cleared
  2026  1216 a5a3               	LDA tmpmask		; pixel & mask -> 0
  2027                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  2028                          				; the caller asserts that this never happens
  2029  1218 c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  2030  121a d0ed               	BNE fc_checkstart	; maybe a continuation ...
  2031                          				; 8 pixel line empty
  2032  121c a596               	LDA fcont		; continued gap?
  2033  121e 2902               	AND #%00000010		; check bit 2
  2034  1220 f0ee               	BEQ fc_cont		; new gap, start it and push on stack
  2035  1222 60                 fc_exit	RTS			; gap continued and already on stack, leave
  2036                          
  2037                          fc_nocont
  2038  1223 a596               	LDA fcont		; clear continuation flag
  2039  1225 29fd               	AND #%11111101		; clear bit 2
  2040                          
  2041                          push_to_stack
  2042  1227 8596               	STA fcont
  2043  1229 18                 	CLC			; fstack points to top of stack
  2044  122a a5fd               	LDA fstack		; to next free stack element
  2045  122c 6904               	ADC #fesize		; entry size
  2046  122e 85fd               	STA fstack
  2047  1230 9002               	BCC +
  2048  1232 e6fe               	INC fstack+1
  2049                          +
  2050  1234 a534               	LDA strbot+1		; check stack space
  2051  1236 c5fe               	CMP fstack+1
  2052  1238 b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  2053                          				; strbot MSB < fstack MSB
  2054                          out_of_memory			
  2055  123a 20580a             	JSR gexit
  2056  123d a210               	LDX #$10		; out of memory error
  2057  123f 6c0003             	JMP (v_baserr)		; basic error handler
  2058  1242 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  2059  1244 a5fd               	LDA fstack		; MSB equal, check LSB
  2060  1246 c533               	CMP strbot
  2061  1248 b0f0               	BCS out_of_memory	; fstack collides with string heap!
  2062                          
  2063                          fc_put
  2064  124a 98                 	TYA			; y-line (value 0-7) merged with
  2065  124b 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  2066  124d a000               	LDY #0			; stack structure index, on next free element
  2067  124f 91fd               	STA (fstack),Y
  2068  1251 c8                 	INY
  2069  1252 a5fc               	LDA caddr+1
  2070  1254 91fd               	STA (fstack),Y		; graphic address high
  2071  1256 c8                 	INY
  2072  1257 a5a3               	LDA tmpmask
  2073  1259 91fd               	STA (fstack),Y
  2074                          !ifndef opt_space {
  2075  125b c8                 	INY
  2076  125c a5a7               	LDA x8			; 8x8 block position
  2077  125e 91fd               	STA (fstack),Y
  2078                          }
  2079                          	
  2080  1260 60                 	RTS
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
  2091  1261 a52b               	LDA bassta
  2092  1263 8522               	STA str
  2093  1265 a52c               	LDA bassta+1
  2094  1267 8523               	STA str+1
  2095  1269 a001               	LDY #1
  2096  126b 98                 	TYA
  2097  126c 9122               	STA (str),y		; != 0
  2098                          
  2099  126e 2033a5             	JSR b_rechain		; starting from bassta
  2100                          				; result in (str)
  2101  1271 18                 	CLC			; str+1 -> new basic end
  2102  1272 a423               	LDY str+1
  2103  1274 a522               	LDA str
  2104  1276 6902               	ADC #2
  2105  1278 852d               	STA basend
  2106  127a 9001               	BCC +
  2107  127c c8                 	INY
  2108  127d 842e               +	STY basend+1
  2109  127f 4c60a6             	JMP b_clr		; perform CLR
  2110                          
  2111                          
  2112                          ;-----------------------------------------------------------------
  2113                          graext_end

; ******** Source: ge-run.asm
    45                          
    46                          
