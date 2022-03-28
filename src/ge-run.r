
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
    31  0822 a2b7               	ldx #<graext_end	; setup basic
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
   214  083e ad0803                     LDA v_bascmd		; check if hooks are already 
   215  0841 ae0903                     LDX v_bascmd+1		; in place 
   216  0844 c9c9               	CMP #<(parse)
   217  0846 d004               	BNE +
   218  0848 e008               	CPX #>(parse)
   219  084a f052               	BEQ ++			; already hooked
   220                          
   221  084c 8d3403             +       STA savevpars		; save old vector
   222  084f 8e3503             	STX savevpars+1
   223  0852 a9c9               	LDA #<(parse)		; basic interpreter parser hook
   224  0854 8d0803                     STA v_bascmd		; for commands
   225  0857 a908                       LDA #>(parse)
   226  0859 8d0903                     STA v_bascmd+1
   227                          
   228  085c ad0a03                     LDA v_basexp		; basic interpreter parser hook
   229  085f 8d3a03             	STA savevexp		; for expressions
   230  0862 a9fd                       LDA #<(express)		; with save of old pointer
   231  0864 8d0a03                     STA v_basexp
   232  0867 ad0b03                     LDA v_basexp+1
   233  086a 8d3b03             	STA savevexp+1
   234  086d a908                       LDA #>(express)
   235  086f 8d0b03                     STA v_basexp+1
   236                          
   237  0872 ad2803                     LDA v_basstp
   238  0875 8d3803             	STA savevstp
   239  0878 a9b4                       LDA #<(stop)		; basic interpreter stop hook
   240  087a 8d2803                     STA v_basstp
   241  087d ad2903                     LDA v_basstp+1
   242  0880 8d3903             	STA savevstp+1
   243  0883 a908                       LDA #>(stop)
   244  0885 8d2903                     STA v_basstp+1
   245                          
   246  0888 ad0003                     LDA v_baserr
   247  088b 8d3603             	STA saveverr
   248  088e a9ae                       LDA #<(error)		; basic interpreter error hook
   249  0890 8d0003                     STA v_baserr
   250  0893 ad0103                     LDA v_baserr+1
   251  0896 8d3703             	STA saveverr+1
   252  0899 a908                       LDA #>(error)
   253  089b 8d0103                     STA v_baserr+1
   254                          
   255  089e a200               ++	LDX #0			; set graphic cursor to (0,0)
   256  08a0 8e3c03             	STX savexl
   257  08a3 8e3d03             	STX savexh
   258  08a6 8e3e03             	STX savey
   259  08a9 e8                 	INX
   260  08aa 8e3f03             	STX savemo		; set mode 1
   261  08ad 60                         RTS
   262                          
   263                          error	
   264                          	; reg A may destroyed
   265  08ae 20c309             	JSR gra_off		; uses only reg A
   266  08b1 6c3603             	JMP (saveverr)		; to original vector
   267                          
   268                          stop	
   269                          	; reg A may destroyed
   270  08b4 a591               	LDA $91			; Scan code
   271  08b6 c97f               	CMP #$7F		; STOP key?
   272  08b8 d003               	BNE nostop
   273  08ba 20c309             	JSR gra_off		; uses only reg A
   274                          nostop
   275  08bd 6c3803             	JMP (savevstp)		; to original vector
   276                          
   277                          
   278                          ;-----------------------------------------------------------------
   279                          
   280                          ; undo chrget
   281                          
   282                          undo_chrget
   283  08c0 a57a               	LDA txtptr		; decrement text pointer by 1
   284  08c2 d002               	BNE +
   285  08c4 c67b               	DEC txtptr+1
   286  08c6 c67a               +	DEC txtptr
   287  08c8 60                 	RTS
   288                          
   289                          ;-----------------------------------------------------------------
   290                          
   291                          ; start parsing an extension command ...
   292                          
   293                          parse
   294  08c9 207300                     JSR chrget		; next char.
   295  08cc c926                       CMP #'&'		; command prefix
   296  08ce f006                       BEQ newcmd
   297  08d0 20c008             	JSR undo_chrget
   298  08d3 6c3403             	JMP (savevpars)
   299                          newcmd
   300  08d6 207300                     JSR chrget		; command character
   301                          
   302  08d9 a00e                       LDY #(cmdsend-cmds)	; map character to
   303                          				; command address ...
   304                          checknextcmd
   305  08db 88                         DEY
   306  08dc f01c               	BEQ parse_error
   307  08de d92b09                     CMP cmds,Y
   308  08e1 d0f8                       BNE checknextcmd	; try next
   309  08e3 88                         DEY			; found
   310  08e4 98                         TYA
   311  08e5 0a                         ASL			; *2
   312  08e6 a8                         TAY
   313                          !ifndef command_rts_tyle {
   314                          	!set co=0		; command offset in jump table
   315  08e7 b93a09                     LDA cmdaddr+1,Y		; high byte from table
   316  08ea 8556                       STA ijmp+1
   317  08ec b93909                     LDA cmdaddr,Y		; low byte from table
   318  08ef 8555                       STA ijmp
   319  08f1 207300                     JSR chrget		; read next byte in basic text
   320  08f4 205400                     JSR ijmp-1		; go to command by JMP (addr)
   321  08f7 4caea7                     JMP b_interpreter	; continue parsing
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
   336  08fa 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
   337                          
   338                          ;-----------------------------------------------------------------
   339                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   340                          express
   341  08fd a900               	LDA #0
   342  08ff 850d               	STA type	
   343  0901 207300             	JSR chrget
   344  0904 b003               	BCS exp_nonumber
   345  0906 4cf3bc             	JMP b_str2fac
   346                          exp_nonumber
   347  0909 c926                       CMP #'&'		; command prefix
   348  090b f006                       BEQ newfunc
   349  090d 20c008             	JSR undo_chrget
   350  0910 6c3a03             	JMP (savevexp)		; original routine	
   351                          ;	JMP b_execexpr
   352                          newfunc
   353  0913 207300             	JSR chrget
   354  0916 c95a               	CMP #'Z'
   355  0918 d003               	BNE +
   356  091a 4cdf0e             	JMP get
   357  091d c958               +	CMP #'X'
   358  091f d003               	BNE +
   359  0921 4cca0e             	JMP getposx
   360  0924 c959               +	CMP #'Y'
   361  0926 d0d2               	BNE parse_error
   362  0928 4cd60e             	JMP getposy
   363                          
   364                          ;-----------------------------------------------------------------
   365                          
   366                          ; the most commonly used command placed at the end ...
   367                          
   368  092b 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   369                          cmdsend
   370                          
   371                          cmdaddr
   372  0939 9612a910bc098a0f...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   373  0945 6210160f4110f10c...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
   374                          
   375  0953 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  095d 312e3333            !text "1.33" 

; ******** Source: graext-core.asm
   377  0961 20313938362c3230...	!text " 1986,2022 JOHANN@KLASEK.AT",0
   378                          
   379                          bitmask
   380  097d 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   381                          nbitmask
   382  0985 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   383                          ytabl
   384  098d 004080c0           	!byte $00,$40,$80,$c0
   385                          ytabh
   386  0991 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   387  0995 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   388  0999 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   389  099d eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   390  09a1 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   391  09a5 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   392  09a9 fe                 	!byte gramp+$1e
   393                          
   394                          ; for horiz. line
   395                          
   396                          maskleft0
   397                          maskleft
   398  09aa ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   399  09b2 00                 	!byte $00
   400                          
   401                          maskright0
   402  09b3 00                 	!byte $00
   403                          maskright
   404  09b4 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   405                          
   406                          ;-----------------------------------------------------------------
   407                          
   408                          graphic
   409  09bc 209eb7                     JSR b_get8bit
   410  09bf e000                       CPX #$00
   411  09c1 d013                       BNE gra_other
   412                          gra0				; &G 0
   413                          gra_off
   414  09c3 a9c7                       LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   415  09c5 8d00dd                     STA cia_pra
   416  09c8 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   417                          				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   418                          				; char addr $1000/4096 = char. ROM
   419  09ca 8d18d0                     STA vic_mcr		; VIC memory control
   420  09cd ad11d0                     LDA vic_cr		; VIC control register
   421  09d0 29df                       AND #%11011111		; Hires mode off
   422  09d2 8d11d0                     STA vic_cr
   423  09d5 60                         RTS
   424                          
   425                          gra_other
   426  09d6 e001                       CPX #$01
   427  09d8 f00f               	BEQ gra1
   428  09da e002               	CPX #$02
   429  09dc f00e                       BEQ gra2
   430  09de e004               	CPX #$04
   431  09e0 f043                       BEQ gra_clear		; &G 4 (erase only, leave mode)
   432  09e2 e003               	CPX #$03		; &G 3 (graphic on)
   433  09e4 f029               	BEQ gra_on
   434  09e6 4c48b2                     JMP b_illquant		; parameter illegal
   435                          	
   436                          gra1				; &G 1
   437  09e9 20250a             	JSR gra_clear
   438                          
   439                          gra2
   440  09ec 20f1b7                     JSR b_getcomma8bit
   441  09ef 8a                         TXA			; foreground color
   442  09f0 0a                         ASL			; upper nibble
   443  09f1 0a                         ASL
   444  09f2 0a                         ASL
   445  09f3 0a                         ASL
   446  09f4 85fd                       STA gcol
   447  09f6 20f1b7                     JSR b_getcomma8bit
   448  09f9 8a                         TXA			; background color
   449  09fa 290f                       AND #$0F
   450  09fc 05fd                       ORA gcol
   451  09fe a000                       LDY #$00
   452                          cram_loop
   453  0a00 9900cc                     STA cram,Y		; fill color RAM
   454  0a03 9900cd                     STA cram+$100,Y
   455  0a06 9900ce                     STA cram+$200,Y
   456  0a09 99e8ce                     STA cram+$300-24,Y
   457  0a0c c8                         INY
   458  0a0d d0f1                       BNE cram_loop
   459                          
   460                          gra_on
   461  0a0f 20440a             	JSR gra_setupcode
   462                          
   463  0a12 a9c4                       LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   464  0a14 8d00dd                     STA cia_pra
   465  0a17 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   466  0a19 8d18d0                     STA vic_mcr		; VIC memory control
   467  0a1c ad11d0                     LDA vic_cr		; VIC control register
   468  0a1f 0920                       ORA #%00100000		; Bit 5 = 1: Hires on
   469  0a21 8d11d0                     STA vic_cr
   470  0a24 60                         RTS
   471                          
   472                          gra_clear
   473  0a25 a220                       LDX #$20		; Pages (8 KByte)
   474  0a27 a9e0                       LDA #>gram
   475  0a29 85fc                       STA gpos+1
   476  0a2b a000                       LDY #$00
   477  0a2d 84fb                       STY gpos
   478  0a2f 98                         TYA
   479                          gra_fill
   480  0a30 91fb                       STA (gpos),Y		; Loop unroll
   481  0a32 c8                         INY
   482  0a33 91fb                       STA (gpos),Y
   483  0a35 c8                         INY
   484  0a36 91fb                       STA (gpos),Y
   485  0a38 c8                         INY
   486  0a39 91fb                       STA (gpos),Y
   487  0a3b c8                         INY
   488  0a3c d0f2                       BNE gra_fill
   489  0a3e e6fc                       INC gpos+1
   490  0a40 ca                         DEX
   491  0a41 d0ed                       BNE gra_fill
   492  0a43 60                 	RTS
   493                          
   494                          gra_setupcode
   495  0a44 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   496                          gra_copycode
   497  0a46 bd670a             	LDA gromcode-1,X
   498  0a49 9dec03             	STA gramcode-1,X
   499  0a4c ca                 	DEX
   500  0a4d d0f7               	BNE gra_copycode
   501  0a4f ad3f03             	LDA savemo
   502  0a52 290f               	AND #$0F
   503  0a54 aa                 	TAX
   504  0a55 4c760e             	JMP setmode_enter	; re-apply mode to routines
   505                          				; implicit RTS
   506                          
   507                          ;-----------------------------------------------------------------
   508                          
   509                          gexit
   510  0a58 a501                       LDA prozport
   511  0a5a 0902                       ORA #%00000010		; kernal ROM enable
   512  0a5c 8501                       STA prozport
   513  0a5e 58                         CLI			; allow interrupts
   514  0a5f 60                         RTS
   515                          
   516                          ;-----------------------------------------------------------------
   517                          
   518                          ginit
   519  0a60 a501                       LDA prozport
   520  0a62 29fd                       AND #%11111101		; Kernal ROM disable
   521  0a64 78                         SEI			; disable interrupts
   522  0a65 8501                       STA prozport
   523  0a67 60                         RTS
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
   540                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   541                          	STA memconf		; damit internes RAM gelesen werden kann!
   542                          }
   543  0a68 b1a5                       LDA (gaddr),Y
   544                          gchange_op
   545  0a6a 1d7d09                     ORA bitmask,X
   546  0a6d 91a5                       STA (gaddr),Y
   547                          !ifdef ltc {
   548                          	LDA #mc_sim		; vollständige ROM-Simulation
   549                          	STA memconf		; wieder schnelles RAM ab $C000
   550                          }
   551  0a6f 60                         RTS
   552                          
   553                          ; mask a graphic location 
   554                          
   555                          gmask
   556                          !ifdef ltc {
   557                          	XBA
   558                          	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
   559                          	STA memconf		; damit internes RAM gelesen werden kann!
   560                          	XBA
   561                          }
   562                          gmask_flip
   563  0a70 4900                       EOR #$00
   564                          gmask_op
   565  0a72 11a5                       ORA (gaddr),Y
   566  0a74 91a5                       STA (gaddr),Y
   567                          !ifdef ltc {
   568                          	LDA #mc_sim		; vollständige ROM-Simulation
   569                          	STA memconf		; wieder schnelles RAM ab $C000
   570                          }
   571  0a76 60                         RTS
   572                          
   573                          }
   574                          
   575                          gromcode_end
   576                          
   577                          ;-----------------------------------------------------------------
   578                          
   579                          position
   580  0a77 a5aa                       LDA y
   581  0a79 4a                         LSR
   582  0a7a 4a                         LSR
   583  0a7b 4a                         LSR			; y/8
   584  0a7c a8                         TAY
   585  0a7d 2903                       AND #%00000011		; (y/8) mod 4
   586  0a7f aa                         TAX
   587  0a80 a59b                       LDA xl			; x low
   588  0a82 29f8                       AND #%11111000		; clear bit 2-0
   589  0a84 18                         CLC
   590  0a85 7d8d09                     ADC ytabl,X		; addr low: y base + x part
   591  0a88 85a5                       STA gaddr
   592  0a8a a59c                       LDA xh			; addr high: x part
   593  0a8c 799109                     ADC ytabh,Y		; 	+ y base
   594  0a8f 85a6                       STA gaddr+1
   595  0a91 a5aa                       LDA y			; vertical offset
   596  0a93 2907                       AND #%00000111		; y mod 8
   597  0a95 a8                         TAY
   598  0a96 a59b                       LDA xl
   599  0a98 2907                       AND #%00000111		; x mod 8
   600  0a9a aa                         TAX			; horizonal offset
   601  0a9b 60                         RTS			; (bitmask)
   602                          
   603                          
   604                          ;-----------------------------------------------------------------
   605                          
   606                          ; swap tupel xl,xh <-> xendl,xendh
   607                          
   608                          swap_x_xend
   609  0a9c a69e                       LDX xendl		; swap x, xend
   610  0a9e a49b                       LDY xl
   611  0aa0 869b                       STX xl
   612  0aa2 849e                       STY xendl
   613                          
   614  0aa4 a69f                       LDX xendh
   615  0aa6 a49c                       LDY xh
   616  0aa8 849f                       STY xendh
   617  0aaa 869c                       STX xh
   618  0aac 60                 	RTS
   619                          
   620                          
   621                          ;-----------------------------------------------------------------
   622                          
   623                          ; line y up, x left, dx < dy (case 1)
   624                          
   625                          line_up_steep
   626  0aad 20770a                     JSR position		; x,y
   627                          loop_yup_xleft
   628  0ab0 20ed03                     JSR gchange		; pixel
   629                          
   630  0ab3 18                         CLC			; k += dx
   631  0ab4 a595                       LDA kl
   632  0ab6 65ab                       ADC dxl			; dxh is 0, because dx < dy
   633  0ab8 8595                       STA kl
   634  0aba 9014                       BCC +			; k >= 0 ->
   635                          
   636  0abc e5a9               ++	SBC dy			; k -= dy (C=1)
   637  0abe 8595                       STA kl
   638                          
   639  0ac0 ca                  	DEX			; x--
   640  0ac1 100d                       BPL +
   641  0ac3 a207                       LDX #7			; wrap around
   642  0ac5 38                 	SEC
   643  0ac6 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   644  0ac8 e908                       SBC #8
   645  0aca 85a5                       STA gaddr
   646  0acc b002                       BCS +
   647  0ace c6a6                       DEC gaddr+1
   648                          
   649  0ad0 88                 +	DEY			; y--
   650  0ad1 100f                       BPL +++
   651  0ad3 38                         SEC			; y overflow
   652  0ad4 a5a5                       LDA gaddr
   653  0ad6 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   654  0ad8 85a5                       STA gaddr
   655  0ada a5a6                       LDA gaddr+1
   656  0adc e901               	SBC #1
   657  0ade 85a6                       STA gaddr+1
   658  0ae0 a007                       LDY #7			; wrap around
   659                          
   660  0ae2 c6a3               +++	DEC cl			; until c=0
   661  0ae4 d0ca                       BNE loop_yup_xleft
   662  0ae6 4c580a                     JMP gexit
   663                          
   664                          
   665                          ;-----------------------------------------------------------------
   666                          
   667                          ; line x left, y up, dx > dy (case 2)
   668                          
   669                          line_up_flat
   670  0ae9 20770a                     JSR position		; x,y
   671  0aec a5a3               	LDA cl			; counter adjustment for
   672  0aee f002               	BEQ +			; prepare for dec-dec-counting
   673  0af0 e6a4               	INC ch
   674                          +
   675                          loop_xleft_yup
   676  0af2 20ed03                     JSR gchange		; pixel
   677                          
   678  0af5 18                         CLC			; k += dy
   679  0af6 a595                       LDA kl
   680  0af8 65a9                       ADC dy
   681  0afa 8595                       STA kl
   682  0afc 9020                       BCC +			; k < 0
   683  0afe e696                       INC kh
   684  0b00 301c               	BMI +			; k < 0
   685                          
   686  0b02 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   687  0b04 8595                       STA kl
   688  0b06 a596                       LDA kh
   689  0b08 e5a7                       SBC dxh		
   690  0b0a 8596                       STA kh
   691                          
   692  0b0c 88                         DEY			; y--
   693  0b0d 100f                       BPL +
   694  0b0f 38                 	SEC			; C=1 not always true (SBC above)
   695  0b10 a5a5                       LDA gaddr		; y overflow
   696  0b12 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   697  0b14 85a5                       STA gaddr
   698  0b16 a5a6                       LDA gaddr+1
   699  0b18 e901               	SBC #1
   700  0b1a 85a6                       STA gaddr+1
   701  0b1c a007               	LDY #7			; wrap around
   702                          
   703  0b1e ca                 +	DEX			; x--
   704  0b1f 100d                       BPL +++
   705  0b21 a207                       LDX #7			; wrap around
   706  0b23 38                 	SEC
   707  0b24 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   708  0b26 e908                       SBC #8
   709  0b28 85a5                       STA gaddr
   710  0b2a b002                       BCS +++
   711  0b2c c6a6                       DEC gaddr+1
   712                          +++
   713  0b2e c6a3               	DEC cl			; c--
   714  0b30 d0c0                       BNE loop_xleft_yup
   715  0b32 c6a4                       DEC ch			; adjusted high which allows this
   716  0b34 d0bc                       BNE loop_xleft_yup
   717                          
   718  0b36 4c580a                     JMP gexit
   719                          
   720                          
   721                          
   722                          ;-----------------------------------------------------------------
   723                          
   724                          ; line x left, y down, dx > dy (case 3)
   725                          
   726                          line_down_flat
   727  0b39 20770a                     JSR position		; x,y
   728  0b3c a5a3               	LDA cl			; counter adjustment for
   729  0b3e f002               	BEQ +			; prepare for dec-dec-counting
   730  0b40 e6a4               	INC ch
   731                          +
   732                          loop_xleft_ydown
   733  0b42 20ed03                     JSR gchange		; pixel
   734                          
   735  0b45 18                         CLC			; k += dy
   736  0b46 a595                       LDA kl
   737  0b48 65a9                       ADC dy
   738  0b4a 8595                       STA kl
   739  0b4c 9021                       BCC +			; k < 0
   740  0b4e e696                       INC kh
   741  0b50 301d               	BMI +			; k < 0
   742                          
   743  0b52 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   744  0b54 8595                       STA kl
   745  0b56 a596                       LDA kh
   746  0b58 e5a7                       SBC dxh		
   747  0b5a 8596                       STA kh
   748                          
   749  0b5c c8                         INY			; y++
   750  0b5d c008                       CPY #8
   751  0b5f d00e                       BNE +
   752                          	; C=1
   753  0b61 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   754  0b63 693f                       ADC #$40-1		; C already set by CPY
   755  0b65 85a5                       STA gaddr
   756  0b67 a5a6                       LDA gaddr+1
   757  0b69 6901               	ADC #1
   758  0b6b 85a6                       STA gaddr+1
   759  0b6d a000                       LDY #0			; wrap around
   760                          
   761  0b6f ca                 +	DEX			; x--
   762  0b70 100d                       BPL +++
   763  0b72 a207                       LDX #7			; wrap around
   764  0b74 38                 	SEC
   765  0b75 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   766  0b77 e908                       SBC #8
   767  0b79 85a5                       STA gaddr
   768  0b7b b002                       BCS +++
   769  0b7d c6a6                       DEC gaddr+1
   770                          +++
   771  0b7f c6a3               	DEC cl			; c--
   772  0b81 d0bf               	BNE loop_xleft_ydown
   773  0b83 c6a4               	DEC ch			; adjusted high which allows this
   774  0b85 d0bb                       BNE loop_xleft_ydown
   775                          
   776  0b87 4c580a                     JMP gexit
   777                          
   778                          
   779                          ;-----------------------------------------------------------------
   780                          
   781                          ; line y down, x right, dx < dy (case 4)
   782                          
   783                          line_down_steep
   784  0b8a 20770a                     JSR position		; x,y
   785                          loop_ydown_xleft
   786  0b8d 20ed03                     JSR gchange		; pixel
   787                          
   788  0b90 18                         CLC			; k += dx
   789  0b91 a595                       LDA kl
   790  0b93 65ab                       ADC dxl			; dxh is 0, because dx < dy
   791  0b95 8595                       STA kl
   792  0b97 9014                       BCC +			; k >= 0 ->
   793                          
   794  0b99 e5a9               	SBC dy			; k -= dy, C=1
   795  0b9b 8595                       STA kl
   796                          
   797  0b9d ca                  	DEX			; x--
   798  0b9e 100d                       BPL +
   799  0ba0 a207                       LDX #7			; wrap around
   800  0ba2 38                 	SEC
   801  0ba3 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   802  0ba5 e908                       SBC #8
   803  0ba7 85a5                       STA gaddr
   804  0ba9 b002                       BCS +
   805  0bab c6a6                       DEC gaddr+1
   806                          
   807  0bad c8                 +	INY			; y++
   808  0bae c008                       CPY #8			; y overflow?
   809  0bb0 d00e                       BNE +++
   810  0bb2 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   811  0bb4 693f                       ADC #$40-1		; C already set by CPY
   812  0bb6 85a5                       STA gaddr
   813  0bb8 a5a6                       LDA gaddr+1
   814  0bba 6901               	ADC #1
   815  0bbc 85a6                       STA gaddr+1
   816  0bbe a000                       LDY #0			; wrap around
   817                          
   818  0bc0 c6a3               +++	DEC cl			; c--
   819                          				; until c=0
   820  0bc2 d0c9                       BNE loop_ydown_xleft
   821  0bc4 4c580a                     JMP gexit
   822                          
   823                          
   824                          ;-----------------------------------------------------------------
   825                          
   826                          getcommaxy
   827  0bc7 20fdae                     JSR b_getcomma		; check ","
   828                          getxy
   829  0bca 208aad                     JSR b_getval		; get X coord. value
   830  0bcd 20f7b7                     JSR b_convint
   831  0bd0 c901                       CMP #>xmax
   832  0bd2 900c               	BCC gcxy_xok
   833  0bd4 f003                       BEQ ++			; X = $1xx
   834  0bd6 20390e                     JSR range_error
   835                          
   836  0bd9 c040               ++	CPY #<xmax		; check X low
   837  0bdb 9003                       BCC +
   838  0bdd 20390e                     JSR range_error
   839                          +
   840                          gcxy_xok
   841  0be0 84fb                       STY gpos		; temporary save X coord.
   842  0be2 85fc                       STA gpos+1
   843                          
   844  0be4 20f1b7                     JSR b_getcomma8bit
   845                          				; get Y coord. value
   846  0be7 e0c8                       CPX #ymax
   847  0be9 9003                       BCC +
   848  0beb 20390e                     JSR range_error
   849                          +
   850  0bee a4fb                       LDY gpos		; restory X coord.
   851  0bf0 a5fc                       LDA gpos+1
   852  0bf2 60                         RTS
   853                          
   854                          
   855                          ;-----------------------------------------------------------------
   856                          
   857                          para_hline_box
   858  0bf3 20ca0b                     JSR getxy		; get startpoint
   859  0bf6 86aa                       STX y
   860  0bf8 8e3e03                     STX savey		; save as cursor, too
   861  0bfb 859c                       STA xh
   862  0bfd 849b                       STY xl
   863  0bff 8d3d03             	STA savexh
   864  0c02 8c3c03             	STY savexl
   865  0c05 20fdae                     JSR b_getcomma		; get length
   866  0c08 208aad                     JSR b_getval
   867  0c0b 20f7b7                     JSR b_convint
   868                          				; calculate end point
   869  0c0e aa                         TAX			; save length high byte
   870  0c0f 98                         TYA			; length low byte
   871  0c10 18                         CLC
   872  0c11 659b                       ADC xl			; low xend = x+length
   873  0c13 859e                       STA xendl
   874  0c15 a8                 	TAY
   875  0c16 8a                         TXA			; high
   876  0c17 659c                       ADC xh			; high xend = x+length
   877  0c19 859f                       STA xendh
   878  0c1b aa                 	TAX
   879                          
   880  0c1c c901               	CMP #>xmax		; endpoint outside?
   881  0c1e 9005               	BCC +
   882  0c20 d003               	BNE +			; >$200 (512)
   883  0c22 98                 	TYA
   884  0c23 e940               	SBC #<xmax
   885  0c25 60                 +	RTS			; C=1 out of range, C=0 ok
   886                          
   887                          ;-----------------------------------------------------------------
   888                          
   889                          hline
   890  0c26 20f30b             	JSR para_hline_box
   891  0c29 9003               	BCC +
   892  0c2b 20390e             	JSR range_error
   893                          				; XXX xend=xmax-1 ?
   894                          +
   895  0c2e 8e3d03                     STX savexh
   896  0c31 8c3c03                     STY savexl		; also save as final cursor
   897                          
   898  0c34 a900               	LDA #0			; default thickness 0 (means 1 pixel)
   899  0c36 85a3               	STA ycount
   900  0c38 207900             	JSR chrgot		; last char. again
   901  0c3b f019               	BEQ +++			; command end? no optional param.
   902  0c3d 20f1b7             	JSR b_getcomma8bit
   903  0c40 8a                 	TXA			; optional 8-bit parameter
   904  0c41 85a3               	STA ycount		; hline thickness
   905  0c43 f011               	BEQ +++			; 0 means 1 pixel
   906  0c45 18                 	CLC
   907  0c46 65aa               	ADC y			; end position for y coord.
   908  0c48 b004               	BCS +			; > 255
   909  0c4a c9c8               	CMP #ymax
   910  0c4c 9008               	BCC +++
   911                          +				; C=1 from ADC or CMP before
   912  0c4e 20390e             	JSR range_error		; corrupts A
   913                          				; XXX ycount=ymax-y-1 ?
   914                          				; xend >= x
   915  0c51 b003               	BCS hl_noxswap		; always
   916                          
   917                          hline_start
   918  0c53 209c0a             	JSR swap_x_xend		; xend < x, entry from line
   919                          	
   920                          hl_noxswap
   921                          				; xend > x
   922                          +++
   923  0c56 e6a3               	INC ycount		; count to 0
   924  0c58 20600a                     JSR ginit		; map in graphic memory
   925                          
   926  0c5b 20770a                     JSR position		; graphic position x,y
   927                          
   928  0c5e a5a5               	LDA gaddr		; save position for vertical
   929  0c60 85fb               	STA sgaddr
   930  0c62 a5a6               	LDA gaddr+1
   931  0c64 85fc               	STA sgaddr+1
   932  0c66 86ab               	STX xsave
   933  0c68 84a9               	STY ysave
   934                          
   935  0c6a a59e                       LDA xendl
   936  0c6c 2907                       AND #%00000111
   937  0c6e 8596                       STA tmp2		; xend mod 8, mask index
   938  0c70 a59b                       LDA xl
   939  0c72 29f8                       AND #%11111000		; (xl div 8)*8
   940  0c74 8595                       STA tmpbits
   941  0c76 a59e                       LDA xendl		; xend unmasked
   942  0c78 38                         SEC
   943  0c79 e595                       SBC tmpbits		; finally: xend - (x div 8)*8 
   944  0c7b 8595                       STA tmpbits
   945  0c7d a59f                       LDA xendh
   946  0c7f e59c                       SBC xh
   947  0c81 4a                         LSR			; / 8 ->  0-39
   948  0c82 a595                       LDA tmpbits		; only 1 highest bit
   949  0c84 6a                         ROR			; and 3 lower bits
   950  0c85 4a                         LSR
   951  0c86 4a                         LSR
   952                                  			; 8-pixel-blocks count
   953  0c87 85a4               	STA hcount		; save for vertical extension
   954                           
   955                          hl_vertloop
   956  0c89 98                 	TYA			; calculate max. Y in 8x8 block
   957  0c8a 18                 	CLC
   958  0c8b 65a3               	ADC ycount
   959  0c8d c908               	CMP #8
   960  0c8f 9002               	BCC +
   961  0c91 a908               	LDA #8
   962  0c93 85a8               +	STA ylimit
   963                          
   964  0c95 bdaa09                     LDA maskleft,X		; starting mask
   965  0c98 8595               	STA tmpbits
   966  0c9a a6a4               	LDX hcount		; how many blocks
   967                          
   968                          hl_nextblock
   969  0c9c ca                         DEX
   970                          hl_islastblock
   971  0c9d 301d                       BMI hl_lastblock
   972                          				; leave loop if X<0
   973  0c9f a4a9               	LDY ysave
   974  0ca1 a595               -	LDA tmpbits		; mask
   975  0ca3 20f503             	JSR gmask		; first with left end mask
   976  0ca6 c8                 	INY			; vertical down
   977  0ca7 c4a8               	CPY ylimit		; in 8x8 box
   978  0ca9 d0f6               	BNE -
   979                          
   980  0cab 18                         CLC			; gaddr += 8 (one block to right)
   981  0cac a5a5                       LDA gaddr
   982  0cae 6908                       ADC #8
   983  0cb0 85a5                       STA gaddr
   984  0cb2 9002                       BCC +
   985  0cb4 e6a6                       INC gaddr+1
   986                          
   987  0cb6 a9ff               +	LDA #$FF		; following with full 8-pixel mask
   988  0cb8 8595               	STA tmpbits
   989  0cba d0e0               	BNE hl_nextblock	; always
   990                          
   991                          hl_lastblock
   992  0cbc a696                       LDX tmp2		; xend mask index
   993  0cbe 3db409                     AND maskright,X		; current mask combined with mask right end
   994  0cc1 8595               	STA tmpbits		; mask
   995  0cc3 a4a9               	LDY ysave		; start position in 8x8 block
   996  0cc5 a595               -	LDA tmpbits		; mask
   997  0cc7 20f503             	JSR gmask		; modify
   998  0cca c8                 	INY			; vertical down
   999  0ccb c6a3               	DEC ycount		; overall y counter
  1000  0ccd c4a8               	CPY ylimit
  1001  0ccf d0f4               	BNE -
  1002                          
  1003  0cd1 a5a3               	LDA ycount		; finished
  1004  0cd3 d003               	BNE +			; roll-over into 8x8 block below
  1005  0cd5 4c580a                     JMP gexit		; leave
  1006                          
  1007  0cd8 18                 +	CLC
  1008  0cd9 a5fb               	LDA sgaddr
  1009  0cdb 6940               	ADC #$40		; next 8-pixel row below
  1010  0cdd 85fb               	STA sgaddr		; + $140 (320)
  1011  0cdf 85a5               	STA gaddr
  1012  0ce1 a5fc               	LDA sgaddr+1
  1013  0ce3 6901               	ADC #$01
  1014  0ce5 85fc               	STA sgaddr+1
  1015  0ce7 85a6               	STA gaddr+1
  1016  0ce9 a6ab               	LDX xsave		; initial mask index
  1017  0ceb a000               	LDY #0			; start on top of 8x8
  1018  0ced 84a9               	STY ysave
  1019  0cef f098               	BEQ hl_vertloop
  1020                          ;-----------------------------------------------------------------
  1021                          
  1022                          vline
  1023  0cf1 20ca0b                     JSR getxy		; get startpoint
  1024  0cf4 859c                       STA xh
  1025  0cf6 8d3d03                     STA savexh		; save as cursor too
  1026  0cf9 849b                       STY xl
  1027  0cfb 8c3c03                     STY savexl
  1028  0cfe 8693                       STX yend		; initial point is endpoint
  1029                          
  1030  0d00 20f1b7                     JSR b_getcomma8bit
  1031                          				; get length
  1032  0d03 18                         CLC			; calculate end point
  1033  0d04 8a                         TXA			; length
  1034                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1035                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1036                          ;	STA tmpbits
  1037  0d05 6593                       ADC yend		; length + initial point is startpoint
  1038  0d07 b005               	BCS vline_iq		; > 255
  1039  0d09 c9c8                       CMP #ymax		; outside?
  1040  0d0b a8                 	TAY			; keep startpoint
  1041  0d0c 9003                       BCC +
  1042                          vline_iq
  1043  0d0e 20390e                     JSR range_error		; corrupts A
  1044                          				; XXX Y = ymax-1 ?
  1045  0d11 84aa               +	STY y			; startpoint
  1046  0d13 8c3e03             	STY savey		; set cursor y position
  1047  0d16 18                 	CLC
  1048  0d17 900e               	BCC +++			; skip following, because y, yend are already ordered
  1049                          
  1050                          vline_start			; entry point from line command (only)
  1051  0d19 a5aa               	LDA y			; order of y, yend is not defined
  1052  0d1b c593               	CMP yend
  1053  0d1d b008               	BCS vl_noyswap		; yend > y ->
  1054  0d1f a5aa               	LDA y			; swap y, yend
  1055  0d21 a693               	LDX yend
  1056  0d23 8593               	STA yend
  1057  0d25 86aa               	STX y
  1058                          vl_noyswap
  1059                          				; startpoint is below the endpoint
  1060  0d27 20600a             +++	JSR ginit		; map in graphic memory
  1061                          
  1062                          vl_start
  1063  0d2a 20770a                     JSR position		; graphic position x,y
  1064  0d2d bd7d09                     LDA bitmask,X
  1065  0d30 8596                       STA tmp2		; save mask
  1066                          ; DON'T-CHANGE: replace ...
  1067  0d32 38                         SEC
  1068  0d33 a5aa                       LDA y			; startpoint is greater!
  1069  0d35 e593                       SBC yend		; vertical length
  1070  0d37 aa                         TAX
  1071                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmpbits)
  1072                          ;		DO NOT USE: tmpbits does not exist if called via vline_start!
  1073                          ;	LDX tmpbits
  1074  0d38 e8                         INX			; +1 (exit on 0)
  1075  0d39 38                 	SEC			; for subtraction, never changed!
  1076                          vl_nextline
  1077  0d3a a596                       LDA tmp2
  1078  0d3c 20f503                     JSR gmask		; modify 
  1079  0d3f 88                         DEY			; go up
  1080  0d40 100e                       BPL +
  1081  0d42 a5a5                       LDA gaddr		; C=1
  1082  0d44 e940               	SBC #$40		; gaddr -= 320
  1083  0d46 85a5                       STA gaddr
  1084  0d48 a5a6                       LDA gaddr+1
  1085  0d4a e901                       SBC #$01
  1086  0d4c 85a6                       STA gaddr+1
  1087  0d4e a007                       LDY #7			; wrap y offset
  1088  0d50 ca                 +	DEX			; all vertical positions done?
  1089  0d51 d0e7                       BNE vl_nextline
  1090  0d53 4c580a                     JMP gexit		; leave
  1091                          
  1092                          
  1093                          ;-----------------------------------------------------------------
  1094                          
  1095                          line
  1096  0d56 20ca0b                     JSR getxy		; get startpoint
  1097  0d59 849b                       STY xl 
  1098  0d5b 859c                       STA xh
  1099  0d5d 86aa                       STX y
  1100                          
  1101  0d5f 20c70b                     JSR getcommaxy		; get endpoint
  1102                          line_start
  1103  0d62 8c3c03                     STY savexl		; save as cursor position too
  1104  0d65 849e                       STY xendl
  1105  0d67 8d3d03                     STA savexh
  1106  0d6a 859f                       STA xendh
  1107  0d6c 8e3e03                     STX savey
  1108  0d6f 8693                       STX yend
  1109                          
  1110  0d71 a000                       LDY #$00		; initialize to 0
  1111  0d73 84a8                       STY ydir
  1112  0d75 8495                       STY kl
  1113  0d77 8496                       STY kh
  1114                          
  1115  0d79 38                         SEC
  1116  0d7a a59b                       LDA xl			; calculate dx
  1117  0d7c e59e                       SBC xendl
  1118  0d7e 85ab                       STA dxl
  1119  0d80 a59c                       LDA xh
  1120  0d82 e59f                       SBC xendh
  1121  0d84 85a7                       STA dxh
  1122                          
  1123  0d86 b018                       BCS li_xend_left
  1124                          	; dx != 0
  1125                          				; negate dx:
  1126  0d88 98                         TYA			; Y=A=0
  1127  0d89 38                         SEC			; dx = 0 - dx
  1128  0d8a e5ab                       SBC dxl
  1129  0d8c 85ab                       STA dxl
  1130  0d8e 98                         TYA			; Y=A=0
  1131  0d8f e5a7                       SBC dxh
  1132  0d91 85a7                       STA dxh
  1133                          				; C=0 always, needed later
  1134  0d93 209c0a             	jsr swap_x_xend
  1135  0d96 a6aa                       LDX y			; swap y
  1136  0d98 a493                       LDY yend
  1137  0d9a 8693                       STX yend
  1138  0d9c 84aa                       STY y
  1139                          
  1140  0d9e 9007                       BCC li_x_different
  1141                          				; C=0 always (from negation before)
  1142                          
  1143                          li_xend_left
  1144                                  			; A already contains dxh
  1145  0da0 05ab                       ORA dxl			; dx = 0?
  1146  0da2 d003                       BNE li_x_different
  1147  0da4 4c190d                     JMP vline_start		; vertical line case
  1148                          
  1149                          li_x_different
  1150  0da7 38                         SEC			; calculate dy
  1151  0da8 a593                       LDA yend
  1152  0daa e5aa                       SBC y
  1153  0dac b006                       BCS li_y_right		; yend >= y?
  1154  0dae 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1155  0db0 6901                       ADC #$01		; C=0
  1156  0db2 85a8                       STA ydir		; always not 0: flag y goes up
  1157                          
  1158                          li_y_right
  1159  0db4 85a9                       STA dy
  1160  0db6 d007                       BNE +
  1161  0db8 a900               	LDA #0			; line thickness = 1
  1162  0dba 85a3               	STA ycount
  1163  0dbc 4c530c                     JMP hline_start		; horizontal line case
  1164                          +
  1165                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1166                          
  1167  0dbf a5a7                       LDA dxh			; dx > dy
  1168  0dc1 d01c                       BNE line_flat		; yes -> flat
  1169  0dc3 a5a9                       LDA dy			; no -> steep
  1170  0dc5 aa                         TAX
  1171  0dc6 c5ab                       CMP dxl
  1172  0dc8 9015                       BCC line_flat
  1173                          
  1174                          line_steep
  1175  0dca e8                         INX	
  1176  0dcb 86a3                       STX cl			; c = dy+1
  1177  0dcd 4a                         LSR			; dy/2
  1178  0dce 49ff               	EOR #$FF		; one's complement
  1179  0dd0 8595                       STA kl			; k = -dy/2 -1
  1180                          
  1181  0dd2 20600a                     JSR ginit		; map in graphic memory
  1182                          
  1183  0dd5 a5a8                       LDA ydir
  1184  0dd7 d003                       BNE +
  1185  0dd9 4c8a0b                     JMP line_down_steep	; y down, steep
  1186  0ddc 4cad0a             +	JMP line_up_steep	; y up, steep
  1187                          
  1188                          line_flat
  1189  0ddf a5a7                       LDA dxh
  1190  0de1 a8                         TAY
  1191  0de2 a6ab                       LDX dxl
  1192  0de4 e8                         INX
  1193  0de5 d001                       BNE +
  1194  0de7 c8                         INY
  1195  0de8 86a3               +	STX cl			; c = dx+1
  1196  0dea 84a4                       STY ch
  1197                          
  1198  0dec 4a                         LSR			; dx/2 high
  1199  0ded 49ff               	EOR #$FF		; one's complement
  1200  0def 8596                       STA kh
  1201  0df1 a5ab                       LDA dxl
  1202  0df3 6a                         ROR			; dx/2 low
  1203  0df4 49ff               	EOR #$FF		; one's complement
  1204  0df6 8595                       STA kl			; k = -dx/2 - 1
  1205                          
  1206  0df8 20600a                     JSR ginit		; map in graphic memory
  1207                          
  1208  0dfb a5a8                       LDA ydir	
  1209  0dfd d003                       BNE +
  1210  0dff 4c390b                     JMP line_down_flat	; y down, flat
  1211  0e02 4ce90a             +	JMP line_up_flat	; y up, flat
  1212                          
  1213                          ;-----------------------------------------------------------------
  1214                          
  1215                          plot
  1216  0e05 20ca0b                     JSR getxy		; get parameter
  1217  0e08 859c                       STA xh			; save x/y
  1218  0e0a 849b                       STY xl
  1219  0e0c 86aa                       STX y
  1220  0e0e 8d3d03                     STA savexh		; and store as cursor
  1221  0e11 8c3c03                     STY savexl
  1222  0e14 8e3e03                     STX savey
  1223                          
  1224                          plot_start
  1225  0e17 20770a                     JSR position		; calculate graphical address
  1226                          
  1227  0e1a a501                       LDA prozport
  1228  0e1c 29fd                       AND #%11111101		; Kernal ROM disable
  1229  0e1e 78                         SEI			
  1230  0e1f 8501                       STA prozport
  1231                          
  1232  0e21 20ed03                     JSR gchange		; change graphical data
  1233                          
  1234  0e24 a501                       LDA prozport
  1235  0e26 0902                       ORA #%00000010		; kernal ROM enable
  1236  0e28 8501                       STA prozport
  1237  0e2a 58                         CLI
  1238  0e2b 60                         RTS
  1239                          
  1240                          ;-----------------------------------------------------------------
  1241                          
  1242                          move
  1243  0e2c 20ca0b                     JSR getxy		; get parameter
  1244  0e2f 8d3d03                     STA savexh		; just save as cursor
  1245  0e32 8c3c03                     STY savexl
  1246  0e35 8e3e03                     STX savey
  1247  0e38 60                         RTS
  1248                          
  1249                          
  1250                          ;-----------------------------------------------------------------
  1251                          
  1252                          ; never touches X, Y, C-flag
  1253                          ; on exit: A corrupted, Z=0
  1254                          
  1255                          range_error
  1256  0e39 ad3f03             	LDA savemo
  1257  0e3c 29f0               	AND #$F0
  1258  0e3e d003               	BNE +
  1259                          				; error mode 3: abort command (silent)
  1260  0e40 68                 	PLA			; cleanup JSR
  1261  0e41 68                 	PLA			; highbyte of return address >0
  1262                          
  1263  0e42 60                 -	RTS			; error mode 5: back to command
  1264                          				; to handle value correction
  1265                          				; Z=0
  1266  0e43 2920               +	AND #$20		; mode 5?
  1267  0e45 d0fb               	BNE -			; exit with Z=0
  1268  0e47 68                 	PLA			; error mode 4: terminate with error
  1269  0e48 68                 	PLA			; cleanup JSR
  1270                          setmode_error
  1271  0e49 4c48b2             	JMP b_illquant		; throw error message
  1272                          
  1273                          ;-----------------------------------------------------------------
  1274                          
  1275                          setmode
  1276  0e4c 209eb7                     JSR b_get8bit
  1277  0e4f e003                       CPX #3
  1278  0e51 9017                       BCC +			; less then 3, modification mode
  1279  0e53 e006               	CPX #6
  1280  0e55 b0f2               	BCS setmode_error	; out of range
  1281                          				; error mode
  1282  0e57 8a                 	TXA
  1283  0e58 e902               	SBC #2			; C=0, therefore -3
  1284  0e5a 0a                 	ASL			; 0-2 -> 16,32 or 48
  1285  0e5b 0a                 	ASL			; shift to upper nibble
  1286  0e5c 0a                 	ASL
  1287  0e5d 0a                 	ASL
  1288                          				; put A's bit 4-7 into savemo
  1289  0e5e 4d3f03             	EOR savemo		; ********
  1290  0e61 29f0               	AND #%11110000		; ****0000
  1291  0e63 4d3f03             	EOR savemo		; AAAAmmmm
  1292  0e66 8d3f03             	STA savemo		; 
  1293  0e69 60                 	RTS
  1294                          
  1295  0e6a 8a                 +	TXA
  1296  0e6b 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1297  0e6e 290f               	AND #%00001111
  1298  0e70 4d3f03             	EOR savemo
  1299  0e73 8d3f03             	STA savemo
  1300                          setmode_enter
  1301  0e76 e001               	CPX #$01
  1302  0e78 b01a                       BCS set_or_toggle
  1303                          
  1304                          modereset
  1305  0e7a a909                       LDA #>(nbitmask)
  1306  0e7c 8df103                     STA gchange_op+2
  1307  0e7f a985                       LDA #<(nbitmask)
  1308  0e81 8df003                     STA gchange_op+1
  1309  0e84 a93d                       LDA #$3D		; opcode AND abs,X
  1310  0e86 8def03                     STA gchange_op
  1311  0e89 a931                       LDA #$31		; opcode AND (zp),Y
  1312  0e8b 8df703                     STA gmask_op
  1313  0e8e a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1314  0e90 8df603                     STA gmask_flip+1
  1315  0e93 60                         RTS
  1316                          
  1317                          set_or_toggle
  1318  0e94 d01a                       BNE modetoggle
  1319                          modeset
  1320  0e96 a909                       LDA #>(bitmask)
  1321  0e98 8df103                     STA gchange_op+2
  1322  0e9b a97d                       LDA #<(bitmask)
  1323  0e9d 8df003                     STA gchange_op+1
  1324  0ea0 a91d                       LDA #$1D		; opcode OR abs,X
  1325  0ea2 8def03                     STA gchange_op
  1326  0ea5 a911                       LDA #$11		; opcode OR (zp),Y
  1327  0ea7 8df703                     STA gmask_op
  1328  0eaa a900                       LDA #$00		; mask, EOR #$00, not inverting
  1329  0eac 8df603                     STA gmask_flip+1
  1330  0eaf 60                         RTS
  1331                          
  1332                          modetoggle
  1333  0eb0 a909                       LDA #>(bitmask)
  1334  0eb2 8df103                     STA gchange_op+2
  1335  0eb5 a97d                       LDA #<(bitmask)
  1336  0eb7 8df003                     STA gchange_op+1
  1337  0eba a95d                       LDA #$5D		; opcode EOR abs,X
  1338  0ebc 8def03                     STA gchange_op
  1339  0ebf a951                       LDA #$51		; opcode EOR (zp),Y
  1340  0ec1 8df703                     STA gmask_op
  1341  0ec4 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1342  0ec6 8df603                     STA gmask_flip+1
  1343  0ec9 60                         RTS
  1344                          
  1345                          
  1346                          ;-----------------------------------------------------------------
  1347                          ; get current x cursor position
  1348                          
  1349                          getposx
  1350  0eca ac3c03             	LDY savexl
  1351  0ecd ad3d03             	LDA savexh
  1352  0ed0 2091b3             	JSR b_word2fac
  1353  0ed3 4c7300             	JMP chrget		; last position of expression (function name)
  1354                          
  1355                          ;-----------------------------------------------------------------
  1356                          ; get current y cursor position
  1357                          
  1358                          getposy
  1359  0ed6 ac3e03             	LDY savey
  1360  0ed9 20a2b3             	JSR b_byte2fac
  1361  0edc 4c7300             	JMP chrget		; last position of expression (function name)
  1362                          
  1363                          ;-----------------------------------------------------------------
  1364                          
  1365                          ; get pixel (check if pixel set)
  1366                          ; not used
  1367                          
  1368                          get
  1369  0edf 207300             	JSR chrget		; advance past function name
  1370  0ee2 20faae             	JSR b_chkparl		; "("?
  1371  0ee5 20ca0b                     JSR getxy		; get X,Y values
  1372  0ee8 859c                       STA xh
  1373  0eea 849b                       STY xl
  1374  0eec 86aa                       STX y
  1375  0eee 207900             	JSR chrgot
  1376  0ef1 20f7ae             	JSR b_chkparr		; ")"?
  1377                          	
  1378                          
  1379  0ef4 20770a                     JSR position		; calculate graphic address/position
  1380                          
  1381  0ef7 a501                       LDA prozport
  1382  0ef9 29fd               	AND #%11111101		; Kernal ROM disable
  1383  0efb 78                         SEI
  1384  0efc 8501                       STA prozport
  1385                          
  1386  0efe b1a5                       LDA (gaddr),Y
  1387  0f00 3d7d09                     AND bitmask,X		; mask position
  1388  0f03 a8                         TAY
  1389  0f04 a501                       LDA prozport
  1390  0f06 0902               	ORA #%00000010		; kernal ROM enable
  1391  0f08 8501                       STA prozport
  1392  0f0a 58                         CLI
  1393  0f0b 98                 	TYA
  1394  0f0c f002               	BEQ +
  1395  0f0e a001               	LDY #1			; <> 0 -> always return 1
  1396  0f10 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1397                          
  1398                          ;-----------------------------------------------------------------
  1399                          
  1400                          relto_cont
  1401                          				; continue
  1402  0f13 207300             	JSR chrget		; skip TO token
  1403                          relto
  1404  0f16 208aad                     JSR b_getval		; get X offset (+/-)
  1405  0f19 a561               	LDA facexp		; FAC exponent
  1406  0f1b c990               	CMP #$90		; more than 16 bit
  1407  0f1d b031               	BCS relto_error		; illegal quantity
  1408  0f1f 209bbc                     JSR b_fac2int		; to signed integer
  1409                          
  1410  0f22 18                         CLC
  1411  0f23 a565                       LDA facintl
  1412  0f25 6d3c03                     ADC savexl
  1413  0f28 859e                       STA xendl
  1414  0f2a a564                       LDA facinth
  1415  0f2c 6d3d03                     ADC savexh
  1416  0f2f 859f                       STA xendh		; xend = savex+facint
  1417                          
  1418  0f31 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1419  0f34 208aad                     JSR b_getval
  1420  0f37 a561                       LDA facexp		; FAC exponent
  1421  0f39 c990                       CMP #$90		; more than 16 bit
  1422  0f3b b013                       BCS relto_error		; illegal quantity
  1423  0f3d 209bbc                     JSR b_fac2int		; to signed integer
  1424  0f40 18                         CLC
  1425  0f41 a565                       LDA facintl
  1426  0f43 6d3e03                     ADC savey
  1427  0f46 8593                       STA yend		; yend = savey+facint
  1428                          
  1429  0f48 a59f                       LDA xendh		; check end coord. x
  1430  0f4a c901                       CMP #>xmax
  1431  0f4c 900e                       BCC rt_xok
  1432  0f4e f003                       BEQ +
  1433                          relto_error
  1434  0f50 20390e                     JSR range_error
  1435  0f53 a59e               +	LDA xendl
  1436  0f55 c940                       CMP #<xmax
  1437  0f57 9003                       BCC +
  1438  0f59 20390e                     JSR range_error
  1439                          +
  1440                          rt_xok
  1441  0f5c a593                       LDA yend		; check end coord. y
  1442  0f5e c9c8                       CMP #ymax
  1443  0f60 9003                       BCC +
  1444  0f62 20390e                     JSR range_error
  1445                          +
  1446  0f65 ad3c03                     LDA savexl
  1447  0f68 859b                       STA xl
  1448  0f6a ad3d03                     LDA savexh
  1449  0f6d 859c                       STA xh
  1450  0f6f ad3e03                     LDA savey
  1451  0f72 85aa                       STA y
  1452  0f74 a49e                       LDY xendl
  1453  0f76 a59f                       LDA xendh
  1454  0f78 a693                       LDX yend		; xend/yend = cursor + x/y
  1455                          
  1456  0f7a 20620d                     JSR line_start		; draw line x/y to xend/yend
  1457                          
  1458  0f7d 207900             	JSR chrgot
  1459  0f80 d001               	BNE +
  1460  0f82 60                 	RTS
  1461  0f83 c9a4               +	CMP #t_to		; TO keyword?
  1462  0f85 f08c               	BEQ relto_cont
  1463  0f87 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1464                          
  1465                          ;-----------------------------------------------------------------
  1466                          
  1467                          char
  1468  0f8a 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1469  0f8d e028                       CPX #40	
  1470  0f8f 9003                       BCC +
  1471                          char_error
  1472  0f91 4c48b2                     JMP b_illquant
  1473  0f94 86fb               +	STX gpos		; save x coord.
  1474  0f96 20f1b7                     JSR b_getcomma8bit
  1475                          				; get char. position y 0-24
  1476  0f99 e019                       CPX #25
  1477  0f9b b0f4                       BCS char_error
  1478  0f9d 86fc                       STX gpos+1		; save y coord.
  1479                          
  1480  0f9f 20fdae                     JSR b_getcomma		; get string
  1481  0fa2 209ead                     JSR b_getexpr
  1482  0fa5 20a3b6                     JSR b_stringval		 ; string address in str
  1483  0fa8 48                         PHA			; string length
  1484  0fa9 a6fc                       LDX gpos+1		; y coord. for char. position
  1485  0fab 8a                         TXA
  1486  0fac 2903                       AND #$03		; mask 2 bits
  1487  0fae a8                         TAY			; table index
  1488  0faf a900                       LDA #$00
  1489  0fb1 85fc                       STA gpos+1		; x high
  1490  0fb3 a5fb                       LDA gpos		; saved x: multiply by 8
  1491  0fb5 0a                         ASL
  1492  0fb6 0a                         ASL
  1493  0fb7 0a                         ASL
  1494  0fb8 26fc                       ROL gpos+1		; overflow to high byte
  1495  0fba 798d09                     ADC ytabl,Y
  1496  0fbd 85a5                       STA gaddr
  1497  0fbf a5fc                       LDA gpos+1		; x high
  1498  0fc1 7d9109                     ADC ytabh,X
  1499  0fc4 85a6                       STA gaddr+1
  1500  0fc6 68                         PLA			; string length
  1501  0fc7 a000                       LDY #$00		; string index
  1502  0fc9 aa                         TAX			; length
  1503  0fca e8                         INX			; prepare as counter
  1504                          char_loop
  1505  0fcb ca                         DEX
  1506  0fcc f008                       BEQ char_exit
  1507  0fce b122                       LDA (str),Y		; read string
  1508  0fd0 20d70f                     JSR char_display
  1509  0fd3 c8                         INY
  1510  0fd4 d0f5                       BNE char_loop
  1511                          char_exit
  1512  0fd6 60                         RTS
  1513                          
  1514                          char_display
  1515  0fd7 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1516  0fd9 8a                         TXA			; save register X+Y
  1517  0fda 48                         PHA
  1518  0fdb 98                         TYA
  1519  0fdc 48                         PHA
  1520  0fdd a5d7                       LDA z_tmp		; get saved character
  1521  0fdf 3012                       BMI char_inverse
  1522                          
  1523                          char_normal
  1524  0fe1 c920                       CMP #$20		; control character?
  1525  0fe3 9054                       BCC char_disp_leave
  1526  0fe5 c960                       CMP #$60
  1527  0fe7 9004                       BCC +
  1528  0fe9 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1529  0feb d014                       BNE char_hires
  1530  0fed 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1531  0fef d010               	BNE char_hires
  1532  0ff1 f00e               	BEQ char_hires
  1533                          
  1534                          char_inverse
  1535  0ff3 297f                       AND #%01111111		; mask bit 7
  1536  0ff5 c97f                       CMP #%01111111		; was 255? (pi)
  1537  0ff7 d002                       BNE +
  1538  0ff9 a95e                       LDA #$5E		; screen code for pi
  1539  0ffb c920               +	CMP #$20		; control character?
  1540  0ffd 903a                       BCC char_disp_leave
  1541                          				; yes, skip
  1542  0fff 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1543                          				; $C0-$FF -> $40-$7F
  1544                          				; OPT: BNE char_hires
  1545                          				; OPT: char_normal
  1546                          char_hires
  1547  1001 a6c7                       LDX z_reverseflag
  1548  1003 f002                       BEQ +
  1549  1005 0980                       ORA #%10000000		; invert char.
  1550  1007 aa                 +	TAX			; save char. for later
  1551  1008 a501                       LDA prozport		; save prozport state
  1552  100a 48                 	PHA
  1553  100b a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1554  100d 78                         SEI
  1555  100e 8501                       STA prozport		; char. rom base = $D000
  1556  1010 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1557  1012 85fc                       STA gpos+1		; 
  1558  1014 8a                         TXA			; char. code
  1559  1015 0a                         ASL			; *8
  1560  1016 26fc                       ROL gpos+1
  1561  1018 0a                         ASL
  1562  1019 26fc                       ROL gpos+1
  1563  101b 0a                         ASL
  1564  101c 26fc                       ROL gpos+1
  1565  101e 85fb                       STA gpos		; addr. in char. rom for char.
  1566                          
  1567  1020 a007                       LDY #$07		; 8 hires lines
  1568                          char_line
  1569  1022 b1fb                       LDA (gpos),Y		; read character line
  1570  1024 20f503                     JSR gmask		; write to hires screen
  1571  1027 88                         DEY
  1572  1028 10f8                       BPL char_line
  1573                          
  1574  102a 68                 	PLA
  1575  102b 8501                       STA prozport
  1576  102d 58                         CLI
  1577                          
  1578  102e 18                         CLC			; step char position to left
  1579  102f a5a5                       LDA gaddr		; ( +8 )
  1580  1031 6908                       ADC #$08
  1581  1033 85a5                       STA gaddr
  1582  1035 9002                       BCC +
  1583  1037 e6a6                       INC gaddr+1
  1584                          +
  1585                          char_disp_leave
  1586  1039 68                 	PLA			; pass written character back
  1587  103a a8                         TAY			; restore saved registers
  1588  103b 68                         PLA
  1589  103c aa                         TAX
  1590  103d 60                 -       RTS
  1591                          
  1592                          
  1593                          ;-----------------------------------------------------------------
  1594                          
  1595                          to_cont
  1596                          				; continue
  1597  103e 207300             	JSR chrget		; skip TO token
  1598                          to
  1599  1041 ad3c03                     LDA savexl
  1600  1044 859b                       STA xl
  1601  1046 ad3d03                     LDA savexh
  1602  1049 859c                       STA xh
  1603  104b ad3e03                     LDA savey
  1604  104e 85aa                       STA y
  1605  1050 20ca0b                     JSR getxy
  1606  1053 20620d                     JSR line_start
  1607  1056 207900             	JSR chrgot
  1608  1059 f0e2               	BEQ -
  1609  105b c9a4               	CMP #t_to		; TO keyword?
  1610  105d f0df               	BEQ to_cont
  1611  105f 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1612                          
  1613                          ;-----------------------------------------------------------------
  1614                          
  1615                          box
  1616  1062 20f30b                     JSR para_hline_box
  1617  1065 9003               	BCC +
  1618  1067 20390e             	JSR range_error
  1619                          				; XXX xend=xmax-1 ?
  1620                          +
  1621  106a 20f1b7             	JSR b_getcomma8bit
  1622  106d 8a                 	TXA			; optional 8-bit parameter
  1623                          				; height
  1624  106e f00c               	BEQ +++			; 0 means 1, box is just a line
  1625  1070 18                 	CLC
  1626  1071 65aa               	ADC y			; end position for y coord.
  1627  1073 b004               	BCS +			; > 255
  1628  1075 c9c8               	CMP #ymax
  1629  1077 9003               	BCC +++
  1630                          +				; C=1 from ADC or CMP before
  1631  1079 20390e             	JSR range_error		; corrupts A
  1632                          				; XXX ycount=ymax-y-1 ?
  1633                          				; xend >= x
  1634  107c 48                 +++	PHA			; yend
  1635  107d a900               	LDA #0
  1636  107f 85a3               	STA ycount		; line thickness 1
  1637  1081 20560c             	JSR hl_noxswap		; upper horizontal line
  1638                          
  1639                          				; right vertical line
  1640  1084 68                 	PLA			; if 0, heigth is 1
  1641  1085 d001               	BNE +			; no 
  1642  1087 60                 	RTS			; exit, if box is degenerated (line)
  1643  1088 a6aa               +	LDX y			; start point at higher values
  1644  108a 85aa               	STA y
  1645  108c 8693               	STX yend
  1646  108e a59e               	LDA xendl
  1647  1090 859b               	STA xl
  1648  1092 a59f               	LDA xendh
  1649  1094 859c               	STA xh
  1650  1096 20270d             	JSR vl_noyswap		; xend,yend -> xend,y
  1651                          				; lower horizontal line
  1652  1099 ad3c03             	LDA savexl
  1653  109c 859b               	STA xl
  1654  109e ad3d03             	LDA savexh
  1655  10a1 859c               	STA xh			; xend already set
  1656  10a3 20560c             	JSR hl_noxswap		; x,yend -> xend,yend
  1657                          				; left vertical line
  1658  10a6 4c270d             	JMP vl_noyswap		; x,y -> x,xend
  1659                          
  1660                          ;-----------------------------------------------------------------
  1661                          
  1662                          fill
  1663  10a9 20ca0b             	JSR getxy
  1664  10ac 859c               	STA xh			; save x/y
  1665  10ae 849b               	STY xl
  1666  10b0 86aa               	STX y
  1667  10b2 8d3d03             	STA savexh		; and store as cursor
  1668  10b5 8c3c03             	STY savexl
  1669  10b8 8e3e03             	STX savey
  1670                                  
  1671  10bb a531                       LDA basaryend		; initialize fill stack pointer
  1672  10bd 38                 	SEC
  1673  10be e903               	SBC #fesize		; one element below
  1674  10c0 85fd               	STA fstack		; use space between basic arrays
  1675  10c2 a532               	LDA basaryend+1		; and string heap bottom
  1676  10c4 e900               	SBC #0			; take borrow
  1677  10c6 85fe               	STA fstack+1
  1678                          
  1679  10c8 20770a             	JSR position		; graphic position in (gaddr)+Y, bit X
  1680  10cb bd7d09             	LDA bitmask,X		; start pixel
  1681  10ce 85a3               	STA tmpmask		; initial single pixel mask
  1682                          
  1683  10d0 a59c               	LDA xh			; setup 8x8 block index (x8)
  1684  10d2 4a                 	LSR			; high bit into C
  1685  10d3 a59b               	LDA xl
  1686  10d5 6a                 	ROR			; take high bit
  1687  10d6 4a                 	LSR
  1688  10d7 4a                 	LSR			; finally divide by 8
  1689  10d8 85a7               	STA x8			; = index of 8x8 block in bitmap
  1690                          
  1691                          	; set fmode (from mode)
  1692  10da ad3f03             	LDA savemo
  1693  10dd 2901               	AND #1			; mode = 0 -> invertmask: $FF
  1694  10df 38                 	SEC			; mode = 1 -> invertmask: $00
  1695  10e0 e901               	SBC #1			; mode = 2 -> same as mode=0
  1696  10e2 85a8               	STA fmode		; mode set or reset
  1697                          
  1698  10e4 84a9               	STY ysave		; save y, will be destroyed
  1699  10e6 205812             	JSR push_to_stack	; place dummy on stack (data ignored)
  1700  10e9 a4a9               	LDY ysave
  1701  10eb 20600a             	JSR ginit		; map in bitmap memory
  1702  10ee 4cf211             	JMP try_stack		; process current position and 
  1703                          				; pull from stack, comes back to f_start
  1704                          				; if pixel is already set, it never returns.
  1705                          				; graphic data in tmpbits
  1706                          
  1707                          f_start				; start fill in the mid of a line ...
  1708  10f1 a900               	LDA #0			; initialize continuation flag
  1709  10f3 8596               	STA fcont		; for line above und below
  1710                          
  1711                          	; tmpmask will be extended to left and right to the borders
  1712                          
  1713                          	; set bits outside mask to 1
  1714  10f5 a5a3               	LDA tmpmask		; 00011100
  1715  10f7 49ff               	EOR #$ff		; 11100011
  1716  10f9 0595               	ORA tmpbits		; 00101010 merge with graphic pixel data
  1717                          				; 11101011 pixel outside tmpmask now set! 
  1718  10fb a2ff               	LDX #$ff		; pixel gap search: first one from left
  1719  10fd e8                 -	INX
  1720  10fe 0a                 	ASL			; counting from left
  1721  10ff b0fc               	BCS -			; loop if pixel is set
  1722                          				; bit number of the leftmost unset pixel in X
  1723  1101 a595               	LDA tmpbits		; 01000010 graphic pixel data
  1724                                                          ; extent bitmask to the right
  1725  1103 86ab               	STX xsave		; needed again later
  1726  1105 3daa09             	AND maskleft,X		; 0000S111 clear left from starting point, to
  1727                          				; 00000010 -> X=6
  1728  1108 208b12             	JSR bitposright		; find the first set bit to right (border)
  1729  110b bdb309             	LDA maskright0,X	; get a mask from the right border to left
  1730  110e 85a3               	STA tmpmask		; 1111S100
  1731                          
  1732  1110 a6ab               	LDX xsave		; starting position
  1733  1112 a595               	LDA tmpbits		; 01000010 graphic pixel data
  1734  1114 3db409             	AND maskright,X		; 11111000 clear right from starting point
  1735  1117 f015               	BEQ stepleft8		; open to left, continue left
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
  1752  1119 a200               	LDX #0
  1753  111b c900               	CMP #0			; special case (no bit set at all)
  1754  111d f004               	BEQ +
  1755  111f e8                 -	INX
  1756  1120 0a                 	ASL			; shift to left
  1757  1121 d0fc               	BNE -			; until byte is empty
  1758                          +
  1759  1123 bdaa09             	LDA maskleft0,X		; get a mask from the left border to right
  1760                          				; 0011S111
  1761  1126 25a3               	AND tmpmask		; intersect with right mask 1111S100
  1762  1128 85a3               	STA tmpmask		; and store it for later 0011S100
  1763  112a f01f               	BEQ next_block		; empty mask immediate continue to right
  1764  112c d045               	BNE to_right		; start to walk and fill to the right border
  1765                          
  1766                          stepleft8
  1767  112e a5a7               	LDA x8 			; 8x8 block position
  1768  1130 f041               	BEQ to_right		; =0, hit left screen border
  1769  1132 c6a7               	DEC x8			; count step 8x8 block to left
  1770  1134 a9ff               	LDA #$ff
  1771  1136 85a3               	STA tmpmask		; initial mask full pixel line
  1772                          
  1773  1138 38                 	SEC 			; graphic address to to next pixel line/block
  1774  1139 a5a5               	LDA gaddr
  1775  113b e908               	SBC #8
  1776  113d b002               	BCS +
  1777  113f c6a6               	DEC gaddr+1
  1778  1141 85a5               +	STA gaddr
  1779                          
  1780                          	; y left unchanged
  1781  1143 b1a5               	LDA (gaddr),Y		; real graphic pixel data from bitmap
  1782  1145 45a8               	EOR fmode		; set/reset mode
  1783  1147 f0e5               	BEQ stepleft8		; step block left if empty
  1784  1149 d0ce               	BNE leftcont		; find left border
  1785                          	
  1786                          next_block
  1787  114b e6a7               	INC x8			; step right a block
  1788  114d a5a7               	LDA x8
  1789  114f c928               	CMP #40			; beyond last horizontal block?
  1790  1151 b077               	BCS process_stack	; done if right screen border
  1791                          	; C = 0
  1792  1153 a5a5               	LDA gaddr		; advance to block right
  1793  1155 6908               	ADC #8			; gaddr = gaddr + 8
  1794  1157 85a5               	STA gaddr
  1795  1159 9002               	BCC +
  1796  115b e6a6               	INC gaddr+1
  1797  115d a9ff               +	LDA #$ff		; asume "all pixels" mask
  1798  115f 85a3               	STA tmpmask
  1799  1161 b1a5               	LDA (gaddr),Y		; pixel data
  1800  1163 45a8               	EOR fmode		; set/reset mode
  1801  1165 f00c               	BEQ to_right		; empty -> finally to to_right
  1802  1167 208b12             	JSR bitposright	        ; search right border
  1803  116a bdb309             	LDA maskright0,X	; mask out the right part
  1804  116d 25a3               	AND tmpmask		; shorten mask accordingly
  1805  116f 85a3               	STA tmpmask
  1806  1171 f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1807                          				; leading to 0 mask (fill_check wont't
  1808                          				; handle this special case)
  1809                          
  1810                          				; continue to fill to right ...
  1811                          to_right			; fill loop towards right border
  1812  1173 a5a3               	LDA tmpmask		; fill mask
  1813                          				; assert:    (bitmap & tempmask) == 0
  1814                          				;         || (bitmap & tempmask) == tempmask
  1815  1175 51a5               	EOR (gaddr),Y		; set/reset to fill
  1816  1177 91a5               	STA (gaddr),Y		; into bitmap - the actual fill action!
  1817                          	
  1818                          check_above
  1819  1179 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1820                          				; c = 0!
  1821  117b 84a9               	STY ysave		; to be restored later
  1822  117d a5a5               	LDA gaddr		; current graphic position
  1823  117f a6a6               	LDX gaddr+1
  1824  1181 88                 	DEY			; line above
  1825  1182 100f               	BPL +			; leaving 8x8 block?
  1826                          	; c=0 (asl fcont)
  1827  1184 e93f               	SBC #$40-1		; block above:
  1828  1186 85fb               	STA caddr		; caddr = gaddr - $140
  1829  1188 8a                 	TXA
  1830  1189 e901               	SBC #$01
  1831  118b aa                 	TAX
  1832  118c c9e0               	CMP #>gram		; still graphic ram?
  1833  118e 900a               	BCC skip_above
  1834  1190 a007               	LDY #7			; last line in block in new block
  1835  1192 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1836  1193 85fb               +	STA caddr		; still in same block
  1837  1195 86fc               ++	STX caddr+1		; shared store
  1838  1197 202712             	JSR fill_check
  1839                          skip_above
  1840                          
  1841                          check_below
  1842  119a 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1843  119c a5a5               	LDA gaddr		; current graphic position
  1844  119e a6a6               	LDX gaddr+1
  1845  11a0 a4a9               	LDY ysave		; restore original y position
  1846  11a2 c8                 	INY			; line below
  1847  11a3 c008               	CPY #8			; crossing 8x8 block?
  1848  11a5 9014               	BCC +			; less then 8
  1849                          	; c=1 (cpy)
  1850  11a7 693f               	ADC #$40-1		; block below: accu has gaddr
  1851  11a9 85fb               	STA caddr		; caddr = gaddr + $140
  1852  11ab a8                 	TAY			; for compare later
  1853  11ac 8a                 	TXA			; gaddr high
  1854  11ad 6901               	ADC #$01
  1855  11af aa                 	TAX
  1856  11b0 b010               	BCS skip_below		; > $10000  -> skip
  1857  11b2 c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1858  11b4 e9ff               	SBC #>(gram+8000)
  1859  11b6 b00a               	BCS skip_below		; greater, so skip
  1860  11b8 a000               	LDY #0			; first line in block
  1861  11ba 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1862  11bb 85fb               +	STA caddr		; transfer unchanged
  1863  11bd 86fc               ++	STX caddr+1		; shared store
  1864  11bf 202712             	JSR fill_check
  1865                          skip_below
  1866                          
  1867  11c2 a4a9               	LDY ysave		; restore original y position
  1868  11c4 a5a3               	LDA tmpmask		; mask:
  1869  11c6 2901               	AND #%00000001		; open to right, continue?
  1870  11c8 d081               	BNE next_block		; to next block if open
  1871                          ; long branch version
  1872                          ;	BEQ process_stack	; not open, finished
  1873                          ;	JMP next_block		; to next block if open
  1874                          
  1875                          process_stack
  1876  11ca a5fd               	LDA fstack		; stack empty?
  1877  11cc c531               	CMP basaryend
  1878  11ce a5fe               	LDA fstack+1
  1879  11d0 e532               	SBC basaryend+1
  1880  11d2 b003               	BCS +			; fstack >= basaryend -> not empty
  1881  11d4 4c580a             	JMP gexit		; empty, we are finished
  1882                          
  1883                          	; top of stack: fetched multiple times until mask is completly filled!
  1884  11d7 a002               +	LDY #fesize-1		; element's last component
  1885                          !ifndef opt_space {
  1886                          	LDA (fstack),Y
  1887                          	STA x8			; 8x8 block position
  1888                          	DEY
  1889                          }
  1890  11d9 b1fd               	LDA (fstack),Y
  1891  11db 85a3               	STA tmpmask		; pixel mask
  1892  11dd 88                 	DEY
  1893  11de b1fd               	LDA (fstack),Y
  1894  11e0 85a6               	STA gaddr+1		; graphic addr high byte
  1895  11e2 88                 	DEY
  1896  11e3 b1fd               	LDA (fstack),Y		; graphic addr low byte combined with y-line
  1897  11e5 aa                 	TAX			; needed twice
  1898  11e6 29f8               	AND #%11111000		; split off address
  1899  11e8 85a5               	STA gaddr
  1900                          !ifdef opt_space {
  1901  11ea 0904               	ORA #%00000100		; end bit marker (if 0 all bits are shifted)
  1902  11ec 85a7               	STA x8			; low byte without least significant 3 bits
  1903                          				; x8 temporary reused. Calculated later ...
  1904                          }
  1905  11ee 8a                 	TXA
  1906  11ef 2907               	AND #%00000111		; split off y-line
  1907  11f1 a8                 	TAY
  1908                          try_stack
  1909  11f2 b1a5               	LDA (gaddr),Y		; get pixels
  1910  11f4 45a8               	EOR fmode		; according to set/reset
  1911  11f6 8595               	STA tmpbits		; keep it for later
  1912  11f8 25a3               	AND tmpmask		; focus on masked pixels
  1913  11fa 08                 	PHP			; save Z flag
  1914  11fb f004               	BEQ pop_stack		; all bits unset, remove from stack, because
  1915                          				; it could be filled in one step!
  1916  11fd c5a3               	CMP tmpmask		; all gaps filled?
  1917  11ff d00f               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1918                          	; all gaps filled, next on stack 
  1919                          pop_stack
  1920  1201 38                 	SEC	
  1921  1202 a5fd               	LDA fstack		; remove entry from stack
  1922  1204 e903               	SBC #fesize		; entry size
  1923  1206 85fd               	STA fstack
  1924  1208 b002               	BCS +
  1925  120a c6fe               	DEC fstack+1
  1926  120c 28                 +	PLP			; all bits to fill empty?
  1927  120d d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1928                          				; all bits unset,
  1929  120f 24                 	!by $24			; = bit $ll, skip next statement (1 byte)
  1930                          				; stack already cleaned up
  1931  1210 28                 +++	PLP			; notstack cleanup
  1932                          
  1933                          !ifdef opt_space {
  1934                          	; Calculate the 8x8 block index from the the graphic address.
  1935                          	; Delayed, only if popped position is not already filled ...
  1936                          	; ((addr & 0x1fff) >> 3) % 40
  1937                          	; Takes 4 iterations. Register X, Y left untouched, 
  1938                          	; x8 contains gaddr low and has bit 2 set as end marker, bit 0, 1 is cleared.
  1939                          	; (312/8) % 40  -> 39
  1940                          	; 1 00111.000 : 101000
  1941  1211 a5a6               	LDA gaddr+1		; divident high byte, mask out upper 3 bits
  1942  1213 291f               	AND #$1f		; range 0 to 1f3f
  1943  1215 06a7               	ASL x8			; $1f always < 40
  1944  1217 2a                 -	ROL			; shift into high byte, carry from low byte
  1945  1218 c928               	CMP #40			; modulo 40
  1946  121a 9002               	BCC +			; dividend less divisor
  1947  121c e928               	SBC #40			; greater or equal divisor, c=1
  1948                          				; nothing done to keep the quotient
  1949  121e 06a7               +	ASL x8			; shift low byte divident
  1950  1220 d0f5               	BNE -			; if end-marker bit shifted out -> 0
  1951  1222 85a7               	STA x8			; modulo in accu, stored to final location
  1952                          }
  1953                          
  1954  1224 4cf110             	JMP f_start		; long (to far away) jump to fill line start
  1955                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1956                          
  1957                          
  1958                          ; Check upper or lower fill path
  1959                          ;		destroys x
  1960                          
  1961                          fill_check
  1962  1227 b1fb               	LDA (caddr),Y
  1963  1229 45a8               	EOR fmode		; pixel data
  1964  122b aa                 	TAX			; save for later
  1965  122c 25a3               	AND tmpmask		; mask to fill
  1966  122e f015               	BEQ fc_cleared		; all masked pixels cleared?
  1967  1230 c5a3               	CMP tmpmask		; check for gaps
  1968  1232 f056               	BEQ fc_exit		; all gaps filled, finished
  1969                          				; if not so, some pixels still set
  1970  1234 a5a3               	LDA tmpmask
  1971                          fc_checkstart			; no continuation, init flag based on
  1972                          				; rightmost pixel:
  1973  1236 4a                 	LSR			; mask bit 0 to carry
  1974  1237 9019               	BCC fc_nocont		; maskbit empty?
  1975  1239 8a                 	TXA			; pixel data
  1976  123a 4a                 	LSR			; pixel bit 0 to carry
  1977  123b b015               	BCS fc_nocont		; bit 0 set
  1978                          				; -> mask is 1 and pixel 0
  1979                          fc_cont
  1980  123d a596               	LDA fcont		; set flag for continuation
  1981  123f 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1982  1241 8596               	STA fcont
  1983  1243 d013               	BNE push_to_stack	; always non zero
  1984                          
  1985                          fc_cleared
  1986  1245 a5a3               	LDA tmpmask		; pixel & mask -> 0
  1987                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1988                          				; the caller asserts that this never happens
  1989  1247 c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1990  1249 d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1991                          				; 8 pixel line empty
  1992  124b a596               	LDA fcont		; continued gap?
  1993  124d 2902               	AND #%00000010		; check bit 2
  1994  124f f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1995  1251 60                 	RTS			; gap continued and already on stack, leave
  1996                          
  1997                          fc_nocont
  1998  1252 a596               	LDA fcont		; clear continuation flag
  1999  1254 29fd               	AND #%11111101		; clear bit 2
  2000  1256 8596               	STA fcont
  2001                          
  2002                          push_to_stack
  2003  1258 18                 	CLC			; fstack points to top of stack
  2004  1259 a5fd               	LDA fstack		; to next free stack element
  2005  125b 6903               	ADC #fesize		; entry size
  2006  125d 85fd               	STA fstack
  2007  125f 9002               	BCC +
  2008  1261 e6fe               	INC fstack+1
  2009                          +
  2010  1263 a534               	LDA strbot+1		; check stack space
  2011  1265 c5fe               	CMP fstack+1
  2012  1267 b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  2013                          				; strbot MSB < fstack MSB
  2014                          out_of_memory			
  2015  1269 20580a             	JSR gexit
  2016  126c a210               	LDX #$10		; out of memory error
  2017  126e 6c0003             	JMP (v_baserr)		; basic error handler
  2018  1271 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  2019  1273 a5fd               	LDA fstack		; MSB equal, check LSB
  2020  1275 c533               	CMP strbot
  2021  1277 b0f0               	BCS out_of_memory	; fstack collides with string heap!
  2022                          
  2023                          fc_put
  2024  1279 98                 	TYA			; y-line (value 0-7) merged with
  2025  127a 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  2026  127c a000               	LDY #0			; stack structure index, on next free element
  2027  127e 91fd               	STA (fstack),Y
  2028  1280 c8                 	INY
  2029  1281 a5fc               	LDA caddr+1
  2030  1283 91fd               	STA (fstack),Y		; graphic address high
  2031  1285 c8                 	INY
  2032  1286 a5a3               	LDA tmpmask
  2033  1288 91fd               	STA (fstack),Y
  2034                          !ifndef opt_space {
  2035                          	INY
  2036                          	LDA x8			; 8x8 block position
  2037                          	STA (fstack),Y
  2038                          }
  2039                          	
  2040  128a 60                 fc_exit	RTS
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
  2067  128b a208               	LDX #8
  2068  128d c900               	CMP #0			; special case (no bit set at all)
  2069  128f f004               	BEQ +
  2070  1291 ca                 -	DEX
  2071  1292 4a                 	LSR			; shift to right
  2072  1293 d0fc               	BNE -			; until byte is empty
  2073  1295 60                 +	RTS
  2074                          
  2075                          ;-----------------------------------------------------------------
  2076                          
  2077                          unnew
  2078                          
  2079  1296 a52b               	LDA bassta
  2080  1298 8522               	STA str
  2081  129a a52c               	LDA bassta+1
  2082  129c 8523               	STA str+1
  2083  129e a001               	LDY #1
  2084  12a0 98                 	TYA
  2085  12a1 9122               	STA (str),y		; != 0
  2086                          
  2087  12a3 2033a5             	JSR b_rechain		; starting from bassta
  2088                          				; result in (str)
  2089  12a6 18                 	CLC			; str+1 -> new basic end
  2090  12a7 a423               	LDY str+1
  2091  12a9 a522               	LDA str
  2092  12ab 6902               	ADC #2
  2093  12ad 852d               	STA basend
  2094  12af 9001               	BCC +
  2095  12b1 c8                 	INY
  2096  12b2 842e               +	STY basend+1
  2097  12b4 4c60a6             	JMP b_clr		; perform CLR
  2098                          
  2099                          
  2100                          ;-----------------------------------------------------------------
  2101                          graext_end

; ******** Source: ge-run.asm
    45                          
    46                          
