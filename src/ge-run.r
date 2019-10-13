
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
    31  0822 a244               	ldx #<graext_end	; setup basic
    32  0824 a012               	ldy #>graext_end
    33  0826 18                 	clc			; set if C=0
    34  0827 209cff             	jsr $ff9c		; KERNAL: system RAM bottom
    35  082a 862b               	stx $2b			; BASIC text start
    36  082c 842c               	sty $2c
    37  082e 2016e4             	jsr $e416		; setup BASIC text start
    38  0831 203e08             	jsr init		; init extension (place hook)
    39  0834 a950               	lda #<author		; message ...
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
   198  083e ad0803                     LDA v_bascmd	; check if hooks are already 
   199  0841 ae0903                     LDX v_bascmd+1	; in place 
   200  0844 c9c9               	CMP #<(parse)
   201  0846 d004               	BNE +
   202  0848 e008               	CPX #>(parse)
   203  084a f052               	BEQ ++		; already hooked
   204                          
   205  084c 8d3403             +       STA savevpars	; save old vector
   206  084f 8e3503             	STX savevpars+1
   207  0852 a9c9               	LDA #<(parse)	; basic interpreter parser hook
   208  0854 8d0803                     STA v_bascmd	; for commands
   209  0857 a908                       LDA #>(parse)
   210  0859 8d0903                     STA v_bascmd+1
   211                          
   212  085c ad0a03                     LDA v_basexp	; basic interpreter parser hook
   213  085f 8d3a03             	STA savevexp	; for expressions
   214  0862 a9fd                       LDA #<(express) ; with save of old pointer
   215  0864 8d0a03                     STA v_basexp
   216  0867 ad0b03                     LDA v_basexp+1
   217  086a 8d3b03             	STA savevexp+1
   218  086d a908                       LDA #>(express)
   219  086f 8d0b03                     STA v_basexp+1
   220                          
   221  0872 ad2803                     LDA v_basstp
   222  0875 8d3803             	STA savevstp
   223  0878 a9b4                       LDA #<(stop)	; basic interpreter stop hook
   224  087a 8d2803                     STA v_basstp
   225  087d ad2903                     LDA v_basstp+1
   226  0880 8d3903             	STA savevstp+1
   227  0883 a908                       LDA #>(stop)
   228  0885 8d2903                     STA v_basstp+1
   229                          
   230  0888 ad0003                     LDA v_baserr
   231  088b 8d3603             	STA saveverr
   232  088e a9ae                       LDA #<(error)	; basic interpreter error hook
   233  0890 8d0003                     STA v_baserr
   234  0893 ad0103                     LDA v_baserr+1
   235  0896 8d3703             	STA saveverr+1
   236  0899 a908                       LDA #>(error)
   237  089b 8d0103                     STA v_baserr+1
   238                          
   239  089e a200               ++	LDX #0		; set graphic cursor to (0,0)
   240  08a0 8e3c03             	STX savexl
   241  08a3 8e3d03             	STX savexh
   242  08a6 8e3e03             	STX savey
   243  08a9 e8                 	INX
   244  08aa 8e3f03             	STX savemo	; set mode 1
   245  08ad 60                         RTS
   246                          
   247                          error	
   248                          	; reg A may destroyed
   249  08ae 20c009             	JSR gra_off		; uses only reg A
   250  08b1 6c3603             	JMP (saveverr)		; to original vector
   251                          
   252                          stop	
   253                          	; reg A may destroyed
   254  08b4 a591               	LDA $91			; Scan code
   255  08b6 c97f               	CMP #$7F		; STOP key?
   256  08b8 d003               	BNE nostop
   257  08ba 20c009             	JSR gra_off		; uses only reg A
   258                          nostop
   259  08bd 6c3803             	JMP (savevstp)		; to original vector
   260                          
   261                          
   262                          ;-----------------------------------------------------------------
   263                          
   264                          ; undo chrget
   265                          
   266                          undo_chrget
   267  08c0 a57a               	LDA txtptr		; decrement text pointer by 1
   268  08c2 d002               	BNE +
   269  08c4 c67b               	DEC txtptr+1
   270  08c6 c67a               +	DEC txtptr
   271  08c8 60                 	RTS
   272                          
   273                          ;-----------------------------------------------------------------
   274                          
   275                          ; start parsing an extension command ...
   276                          
   277                          parse
   278  08c9 207300                     JSR chrget			; next char.
   279  08cc c926                       CMP #'&'			; command prefix
   280  08ce f006                       BEQ newcmd
   281  08d0 20c008             	JSR undo_chrget
   282  08d3 6c3403             	JMP (savevpars)
   283                          newcmd
   284  08d6 207300                     JSR chrget			; command character
   285                          
   286  08d9 a00d                       LDY #(cmdsend-cmds)		; map character to
   287                          					; command address ...
   288                          checknextcmd
   289  08db 88                         DEY
   290  08dc f01c               	BEQ parse_error
   291  08de d92b09                     CMP cmds,Y
   292  08e1 d0f8                       BNE checknextcmd		; try next
   293  08e3 88                         DEY				; found
   294  08e4 98                         TYA
   295  08e5 0a                         ASL				; *2
   296  08e6 a8                         TAY
   297                          !ifndef command_rts_tyle {
   298                          	!set co=0			; command offset in jump table
   299  08e7 b93909                     LDA cmdaddr+1,Y                 ; high byte from table
   300  08ea 8556                       STA ijmp+1
   301  08ec b93809                     LDA cmdaddr,Y                   ; low byte from table
   302  08ef 8555                       STA ijmp
   303  08f1 207300                     JSR chrget			; read next byte in basic text
   304  08f4 205400                     JSR ijmp-1                      ; go to command by JMP (addr)
   305  08f7 4caea7                     JMP b_interpreter		; continue parsing
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
   320  08fa 4c08af                     JMP b_syntaxerror		; throw error (unknown command)
   321                          
   322                          ;-----------------------------------------------------------------
   323                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   324                          express
   325  08fd a900               	LDA #0
   326  08ff 850d               	STA type	
   327  0901 207300             	JSR chrget
   328  0904 b003               	BCS exp_nonumber
   329  0906 4cf3bc             	JMP b_str2fac
   330                          exp_nonumber
   331  0909 c926                       CMP #'&'			; command prefix
   332  090b f006                       BEQ newfunc
   333  090d 20c008             	JSR undo_chrget
   334  0910 6c3a03             	JMP (savevexp)			; original routine	
   335                          ;	JMP b_execexpr
   336                          newfunc
   337  0913 207300             	JSR chrget
   338  0916 c95a               	CMP #'Z'
   339  0918 d003               	BNE +
   340  091a 4cce0e             	JMP get
   341  091d c958               +	CMP #'X'
   342  091f d003               	BNE +
   343  0921 4cb90e             	JMP getposx
   344  0924 c959               +	CMP #'Y'
   345  0926 d0d2               	BNE parse_error
   346  0928 4cc50e             	JMP getposy
   347                          
   348                          ;-----------------------------------------------------------------
   349                          
   350                          ; the most commonly used command placed at the end ...
   351                          
   352  092b 2055464743534d52...cmds	!text " UFGCSMRTVHLP"		; first char. is a dummy
   353                          cmdsend
   354                          
   355                          cmdaddr
   356  0938 23123210b909690f...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co,relto-co
   357  0946 1d10e40cf00b490d...        !word to-co,vline-co,hline-co,line-co,plot-co
   358                          
   359  0950 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  095a 312e3239            !text "1.29" 

; ******** Source: graext-core.asm
   361  095e 20313938362c3230...	!text " 1986,2019 JOHANN@KLASEK.AT",0
   362                          
   363                          bitmask
   364  097a 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   365                          nbitmask
   366  0982 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   367                          ytabl
   368  098a 004080c0           	!byte $00,$40,$80,$c0
   369                          ytabh
   370  098e e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   371  0992 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   372  0996 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   373  099a eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   374  099e f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   375  09a2 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   376  09a6 fe                 	!byte gramp+$1e
   377                          
   378                          ; for horiz. line
   379                          
   380                          maskleft0
   381                          maskleft
   382  09a7 ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   383  09af 00                 	!byte $00
   384                          
   385                          maskright0
   386  09b0 00                 	!byte $00
   387                          maskright
   388  09b1 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   389                          
   390                          ;-----------------------------------------------------------------
   391                          
   392                          graphic
   393  09b9 209eb7                     JSR b_get8bit
   394  09bc e000                       CPX #$00
   395  09be d013                       BNE gra_other
   396                          gra0			; &G 0
   397                          gra_off
   398  09c0 a9c7                       LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   399  09c2 8d00dd                     STA cia_pra
   400  09c5 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   401                          			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   402                          			; char addr $1000/4096 = char. ROM
   403  09c7 8d18d0                     STA vic_mcr	; VIC memory control
   404  09ca ad11d0                     LDA vic_cr	; VIC control register
   405  09cd 29df                       AND #%11011111	; Hires mode off
   406  09cf 8d11d0                     STA vic_cr
   407  09d2 60                         RTS
   408                          
   409                          gra_other
   410  09d3 e001                       CPX #$01
   411  09d5 f00f               	BEQ gra1
   412  09d7 e002               	CPX #$02
   413  09d9 f00e                       BEQ gra2
   414  09db e004               	CPX #$04
   415  09dd f043                       BEQ gra_clear	; &G 4 (erase only, leave mode)
   416  09df e003               	CPX #$03	; &G 3 (graphic on)
   417  09e1 f029               	BEQ gra_on
   418  09e3 4c48b2                     JMP b_illquant	; parameter illegal
   419                          	
   420                          gra1			; &G 1
   421  09e6 20220a             	JSR gra_clear
   422                          
   423                          gra2
   424  09e9 20f1b7                     JSR b_getcomma8bit
   425  09ec 8a                         TXA		; foreground color
   426  09ed 0a                         ASL		; upper nibble
   427  09ee 0a                         ASL
   428  09ef 0a                         ASL
   429  09f0 0a                         ASL
   430  09f1 85fd                       STA gcol
   431  09f3 20f1b7                     JSR b_getcomma8bit
   432  09f6 8a                         TXA		; background color
   433  09f7 290f                       AND #$0F
   434  09f9 05fd                       ORA gcol
   435  09fb a000                       LDY #$00
   436                          cram_loop
   437  09fd 9900cc                     STA cram,Y	; fill color RAM
   438  0a00 9900cd                     STA cram+$100,Y
   439  0a03 9900ce                     STA cram+$200,Y
   440  0a06 99e8ce                     STA cram+$300-24,Y
   441  0a09 c8                         INY
   442  0a0a d0f1                       BNE cram_loop
   443                          
   444                          gra_on
   445  0a0c 20410a             	JSR gra_setupcode
   446                          
   447  0a0f a9c4                       LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   448  0a11 8d00dd                     STA cia_pra
   449  0a14 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   450  0a16 8d18d0                     STA vic_mcr	; VIC memory control
   451  0a19 ad11d0                     LDA vic_cr	; VIC control register
   452  0a1c 0920                       ORA #%00100000	; Bit 5 = 1: Hires on
   453  0a1e 8d11d0                     STA vic_cr
   454  0a21 60                         RTS
   455                          
   456                          gra_clear
   457  0a22 a220                       LDX #$20	; Pages (8 KByte)
   458  0a24 a9e0                       LDA #>gram
   459  0a26 85fc                       STA gpos+1
   460  0a28 a000                       LDY #$00
   461  0a2a 84fb                       STY gpos
   462  0a2c 98                         TYA
   463                          gra_fill
   464  0a2d 91fb                       STA (gpos),Y	; Loop unroll
   465  0a2f c8                         INY
   466  0a30 91fb                       STA (gpos),Y
   467  0a32 c8                         INY
   468  0a33 91fb                       STA (gpos),Y
   469  0a35 c8                         INY
   470  0a36 91fb                       STA (gpos),Y
   471  0a38 c8                         INY
   472  0a39 d0f2                       BNE gra_fill
   473  0a3b e6fc                       INC gpos+1
   474  0a3d ca                         DEX
   475  0a3e d0ed                       BNE gra_fill
   476  0a40 60                 	RTS
   477                          
   478                          gra_setupcode
   479  0a41 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   480                          gra_copycode
   481  0a43 bd640a             	LDA gromcode-1,X
   482  0a46 9dec03             	STA gramcode-1,X
   483  0a49 ca                 	DEX
   484  0a4a d0f7               	BNE gra_copycode
   485  0a4c ad3f03             	LDA savemo
   486  0a4f 290f               	AND #$0F
   487  0a51 aa                 	TAX
   488  0a52 4c650e             	JMP setmode_enter	; re-apply mode to routines
   489                          				; implicit RTS
   490                          
   491                          ;-----------------------------------------------------------------
   492                          
   493                          gexit
   494  0a55 a501                       LDA prozport
   495  0a57 0902                       ORA #%00000010	; kernal ROM enable
   496  0a59 8501                       STA prozport
   497  0a5b 58                         CLI		; allow interrupts
   498  0a5c 60                         RTS
   499                          
   500                          ;-----------------------------------------------------------------
   501                          
   502                          ginit
   503  0a5d a501                       LDA prozport
   504  0a5f 29fd                       AND #%11111101	; Kernal ROM disable
   505  0a61 78                         SEI		; disable interrupts
   506  0a62 8501                       STA prozport
   507  0a64 60                         RTS
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
   527  0a65 b1a5                       LDA (gaddr),Y
   528                          gchange_op
   529  0a67 1d7a09                     ORA bitmask,X
   530  0a6a 91a5                       STA (gaddr),Y
   531                          !ifdef ltc {
   532                          	LDA #mc_sim		; vollständige ROM-Simulation
   533                          	STA memconf		; wieder schnelles RAM ab $C000
   534                          }
   535  0a6c 60                         RTS
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
   547  0a6d 4900                       EOR #$00
   548                          gmask_op
   549  0a6f 11a5                       ORA (gaddr),Y
   550  0a71 91a5                       STA (gaddr),Y
   551                          !ifdef ltc {
   552                          	LDA #mc_sim		; vollständige ROM-Simulation
   553                          	STA memconf		; wieder schnelles RAM ab $C000
   554                          }
   555  0a73 60                         RTS
   556                          
   557                          }
   558                          
   559                          gromcode_end
   560                          
   561                          ;-----------------------------------------------------------------
   562                          
   563                          position
   564  0a74 a5aa                       LDA y
   565  0a76 4a                         LSR
   566  0a77 4a                         LSR
   567  0a78 4a                         LSR		; y/8
   568  0a79 a8                         TAY
   569  0a7a 2903                       AND #%00000011	; (y/8) mod 4
   570  0a7c aa                         TAX
   571  0a7d a59b                       LDA xl		; x low
   572  0a7f 29f8                       AND #%11111000	; clear bit 2-0
   573  0a81 18                         CLC
   574  0a82 7d8a09                     ADC ytabl,X	; addr low: y base + x part
   575  0a85 85a5                       STA gaddr
   576  0a87 a59c                       LDA xh		; addr high: x part
   577  0a89 798e09                     ADC ytabh,Y	; 	+ y base
   578  0a8c 85a6                       STA gaddr+1
   579  0a8e a5aa                       LDA y		; vertical offset
   580  0a90 2907                       AND #%00000111	; y mod 8
   581  0a92 a8                         TAY
   582  0a93 a59b                       LDA xl
   583  0a95 2907                       AND #%00000111	; x mod 8
   584  0a97 aa                         TAX		; horizonal offset
   585  0a98 60                         RTS		; (bitmask)
   586                          
   587                          
   588                          ;-----------------------------------------------------------------
   589                          
   590                          ; swap tupel xl,xh <-> xendl,xendh
   591                          
   592                          swap_x_xend
   593  0a99 a69e                       LDX xendl	; swap x, xend
   594  0a9b a49b                       LDY xl
   595  0a9d 869b                       STX xl
   596  0a9f 849e                       STY xendl
   597                          
   598  0aa1 a69f                       LDX xendh
   599  0aa3 a49c                       LDY xh
   600  0aa5 849f                       STY xendh
   601  0aa7 869c                       STX xh
   602  0aa9 60                 	RTS
   603                          
   604                          
   605                          ;-----------------------------------------------------------------
   606                          
   607                          ; line y up, x left, dx < dy (case 1)
   608                          
   609                          line_up_steep
   610  0aaa 20740a                     JSR position	; x,y
   611                          loop_yup_xleft
   612  0aad 20ed03                     JSR gchange	; pixel
   613                          
   614  0ab0 18                         CLC		; k += dx
   615  0ab1 a595                       LDA kl
   616  0ab3 65ab                       ADC dxl		; dxh is 0, because dx < dy
   617  0ab5 8595                       STA kl
   618  0ab7 9014                       BCC +		; k >= 0 ->
   619                          
   620  0ab9 e5a9               ++	SBC dy		; k -= dy (C=1)
   621  0abb 8595                       STA kl
   622                          
   623  0abd ca                  	DEX		; x--
   624  0abe 100d                       BPL +
   625  0ac0 a207                       LDX #7		; wrap around
   626  0ac2 38                 	SEC
   627  0ac3 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   628  0ac5 e908                       SBC #8
   629  0ac7 85a5                       STA gaddr
   630  0ac9 b002                       BCS +
   631  0acb c6a6                       DEC gaddr+1
   632                          
   633  0acd 88                 +	DEY		; y--
   634  0ace 100f                       BPL +++
   635  0ad0 38                         SEC		; y overflow
   636  0ad1 a5a5                       LDA gaddr
   637  0ad3 e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   638  0ad5 85a5                       STA gaddr
   639  0ad7 a5a6                       LDA gaddr+1
   640  0ad9 e901               	SBC #1
   641  0adb 85a6                       STA gaddr+1
   642  0add a007                       LDY #7		; wrap around
   643                          
   644  0adf c6a3               +++	DEC cl		; until c=0
   645  0ae1 d0ca                       BNE loop_yup_xleft
   646  0ae3 4c550a                     JMP gexit
   647                          
   648                          
   649                          ;-----------------------------------------------------------------
   650                          
   651                          ; line x left, y up, dx > dy (case 2)
   652                          
   653                          line_up_flat
   654  0ae6 20740a                     JSR position	; x,y
   655  0ae9 a5a3               	LDA cl		; counter adjustment for
   656  0aeb f002               	BEQ +		; prepare for dec-dec-counting
   657  0aed e6a4               	INC ch
   658                          +
   659                          loop_xleft_yup
   660  0aef 20ed03                     JSR gchange	; pixel
   661                          
   662  0af2 18                         CLC		; k += dy
   663  0af3 a595                       LDA kl
   664  0af5 65a9                       ADC dy
   665  0af7 8595                       STA kl
   666  0af9 9020                       BCC +		; k < 0
   667  0afb e696                       INC kh
   668  0afd 301c               	BMI +		; k < 0
   669                          
   670  0aff e5ab                       SBC dxl		; k -= dx (A = kl, C=1)
   671  0b01 8595                       STA kl
   672  0b03 a596                       LDA kh
   673  0b05 e5a7                       SBC dxh		
   674  0b07 8596                       STA kh
   675                          
   676  0b09 88                         DEY		; y--
   677  0b0a 100f                       BPL +
   678  0b0c 38                 	SEC		; C=1 not always true (SBC above)
   679  0b0d a5a5                       LDA gaddr	; y overflow
   680  0b0f e940                       SBC #$40	; y-8: gaddr -= 40*8 ($140)
   681  0b11 85a5                       STA gaddr
   682  0b13 a5a6                       LDA gaddr+1
   683  0b15 e901               	SBC #1
   684  0b17 85a6                       STA gaddr+1
   685  0b19 a007               	LDY #7		; wrap around
   686                          
   687  0b1b ca                 +	DEX		; x--
   688  0b1c 100d                       BPL +++
   689  0b1e a207                       LDX #7		; wrap around
   690  0b20 38                 	SEC
   691  0b21 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   692  0b23 e908                       SBC #8
   693  0b25 85a5                       STA gaddr
   694  0b27 b002                       BCS +++
   695  0b29 c6a6                       DEC gaddr+1
   696                          +++
   697  0b2b c6a3               	DEC cl		; c--
   698  0b2d d0c0                       BNE loop_xleft_yup
   699  0b2f c6a4                       DEC ch		; adjusted high which allows this
   700  0b31 d0bc                       BNE loop_xleft_yup
   701                          
   702  0b33 4c550a                     JMP gexit
   703                          
   704                          
   705                          
   706                          ;-----------------------------------------------------------------
   707                          
   708                          ; line x left, y down, dx > dy (case 3)
   709                          
   710                          line_down_flat
   711  0b36 20740a                     JSR position	; x,y
   712  0b39 a5a3               	LDA cl		; counter adjustment for
   713  0b3b f002               	BEQ +		; prepare for dec-dec-counting
   714  0b3d e6a4               	INC ch
   715                          +
   716                          loop_xleft_ydown
   717  0b3f 20ed03                     JSR gchange	; pixel
   718                          
   719  0b42 18                         CLC		; k += dy
   720  0b43 a595                       LDA kl
   721  0b45 65a9                       ADC dy
   722  0b47 8595                       STA kl
   723  0b49 9021                       BCC +		; k < 0
   724  0b4b e696                       INC kh
   725  0b4d 301d               	BMI +		; k < 0
   726                          
   727  0b4f e5ab                       SBC dxl		; k -= dx (A = kl, C=1)
   728  0b51 8595                       STA kl
   729  0b53 a596                       LDA kh
   730  0b55 e5a7                       SBC dxh		
   731  0b57 8596                       STA kh
   732                          
   733  0b59 c8                         INY		; y++
   734  0b5a c008                       CPY #8
   735  0b5c d00e                       BNE +
   736                          	; C=1
   737  0b5e a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   738  0b60 693f                       ADC #$40-1	; C already set by CPY
   739  0b62 85a5                       STA gaddr
   740  0b64 a5a6                       LDA gaddr+1
   741  0b66 6901               	ADC #1
   742  0b68 85a6                       STA gaddr+1
   743  0b6a a000                       LDY #0		; wrap around
   744                          
   745  0b6c ca                 +	DEX		; x--
   746  0b6d 100d                       BPL +++
   747  0b6f a207                       LDX #7		; wrap around
   748  0b71 38                 	SEC
   749  0b72 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   750  0b74 e908                       SBC #8
   751  0b76 85a5                       STA gaddr
   752  0b78 b002                       BCS +++
   753  0b7a c6a6                       DEC gaddr+1
   754                          +++
   755  0b7c c6a3               	DEC cl		; c--
   756  0b7e d0bf               	BNE loop_xleft_ydown
   757  0b80 c6a4               	DEC ch		; adjusted high which allows this
   758  0b82 d0bb                       BNE loop_xleft_ydown
   759                          
   760  0b84 4c550a                     JMP gexit
   761                          
   762                          
   763                          ;-----------------------------------------------------------------
   764                          
   765                          ; line y down, x right, dx < dy (case 4)
   766                          
   767                          line_down_steep
   768  0b87 20740a                     JSR position	; x,y
   769                          loop_ydown_xleft
   770  0b8a 20ed03                     JSR gchange	; pixel
   771                          
   772  0b8d 18                         CLC		; k += dx
   773  0b8e a595                       LDA kl
   774  0b90 65ab                       ADC dxl		; dxh is 0, because dx < dy
   775  0b92 8595                       STA kl
   776  0b94 9014                       BCC +		; k >= 0 ->
   777                          
   778  0b96 e5a9               	SBC dy		; k -= dy, C=1
   779  0b98 8595                       STA kl
   780                          
   781  0b9a ca                  	DEX		; x--
   782  0b9b 100d                       BPL +
   783  0b9d a207                       LDX #7		; wrap around
   784  0b9f 38                 	SEC
   785  0ba0 a5a5                       LDA gaddr	; x-8: gaddr -= 8
   786  0ba2 e908                       SBC #8
   787  0ba4 85a5                       STA gaddr
   788  0ba6 b002                       BCS +
   789  0ba8 c6a6                       DEC gaddr+1
   790                          
   791  0baa c8                 +	INY		; y++
   792  0bab c008                       CPY #8		; y overflow?
   793  0bad d00e                       BNE +++
   794  0baf a5a5                       LDA gaddr	; y+8: gaddr += 40*8 ($140)
   795  0bb1 693f                       ADC #$40-1	; C already set by CPY
   796  0bb3 85a5                       STA gaddr
   797  0bb5 a5a6                       LDA gaddr+1
   798  0bb7 6901               	ADC #1
   799  0bb9 85a6                       STA gaddr+1
   800  0bbb a000                       LDY #0		; wrap around
   801                          
   802  0bbd c6a3               +++	DEC cl		; c--
   803                          			; until c=0
   804  0bbf d0c9                       BNE loop_ydown_xleft
   805  0bc1 4c550a                     JMP gexit
   806                          
   807                          
   808                          ;-----------------------------------------------------------------
   809                          
   810                          getcommaxy
   811  0bc4 20fdae                     JSR b_getcomma	; check ","
   812                          getxy
   813  0bc7 208aad                     JSR b_getval	; get X coord. value
   814  0bca 20f7b7                     JSR b_convint
   815  0bcd c901                       CMP #>xmax
   816  0bcf 900c               	BCC gcxy_xok
   817  0bd1 f003                       BEQ ++		; X = $1xx
   818  0bd3 202c0e                     JSR range_error
   819                          
   820  0bd6 c040               ++	CPY #<xmax	; check X low
   821  0bd8 9003                       BCC +
   822  0bda 202c0e                     JSR range_error
   823                          +
   824                          gcxy_xok
   825  0bdd 84fb                       STY gpos	; temporary save X coord.
   826  0bdf 85fc                       STA gpos+1
   827                          
   828  0be1 20f1b7                     JSR b_getcomma8bit
   829                          			; get Y coord. value
   830  0be4 e0c8                       CPX #ymax
   831  0be6 9003                       BCC +
   832  0be8 202c0e                     JSR range_error
   833                          +
   834  0beb a4fb                       LDY gpos	; restory X coord.
   835  0bed a5fc                       LDA gpos+1
   836  0bef 60                         RTS
   837                          
   838                          
   839                          ;-----------------------------------------------------------------
   840                          
   841                          hline
   842  0bf0 20c70b                     JSR getxy	; get startpoint
   843  0bf3 86aa                       STX y
   844  0bf5 8e3e03                     STX savey	; save as cursor, too
   845  0bf8 859c                       STA xh
   846  0bfa 849b                       STY xl
   847  0bfc 20fdae                     JSR b_getcomma	; get length
   848  0bff 208aad                     JSR b_getval
   849  0c02 20f7b7                     JSR b_convint
   850                          			; calculate end point
   851  0c05 aa                         TAX		; save length high byte
   852  0c06 98                         TYA		; length low byte
   853  0c07 18                         CLC
   854  0c08 659b                       ADC xl		; low xend = x+length
   855  0c0a 859e                       STA xendl
   856  0c0c a8                 	TAY
   857  0c0d 8a                         TXA		; high
   858  0c0e 659c                       ADC xh		; high xend = x+length
   859  0c10 859f                       STA xendh
   860  0c12 aa                 	TAX
   861                          
   862  0c13 c901               	CMP #>xmax	; endpoint outside?
   863  0c15 900a               	BCC +
   864  0c17 d005               	BNE ++		; >=$200
   865  0c19 98                 	TYA
   866  0c1a e940               	SBC #<xmax
   867  0c1c 9003               	BCC +
   868  0c1e 202c0e             ++	JSR range_error
   869                          			; XXX xend=xmax-1 ?
   870                          +
   871  0c21 8e3d03                     STX savexh
   872  0c24 8c3c03                     STY savexl	; also save as final cursor
   873                          
   874  0c27 a900               	LDA #0		; default thickness 0 (means 1 pixel)
   875  0c29 85a3               	STA ycount
   876  0c2b 207900             	JSR chrgot	; last char. again
   877  0c2e f019               	BEQ +++		; command end? no optional param.
   878  0c30 20f1b7             	JSR b_getcomma8bit
   879  0c33 8a                 	TXA		; optional 8-bit parameter
   880  0c34 85a3               	STA ycount	; hline thickness
   881  0c36 f011               	BEQ +++		; 0 means 1 pixel
   882  0c38 18                 	CLC
   883  0c39 65aa               	ADC y		; end position for y coord.
   884  0c3b b004               	BCS +		; > 255
   885  0c3d c9c8               	CMP #ymax
   886  0c3f 9008               	BCC +++
   887                          +			; C=1 from ADC or CMP before
   888  0c41 202c0e             	JSR range_error	; corrupts A
   889                          			; XXX ycount=ymax-y-1 ?
   890                          			; xend >= x
   891  0c44 b003               	BCS hl_noxswap	; always
   892                          
   893                          hline_start
   894  0c46 20990a             	JSR swap_x_xend	; xend < x, entry from line
   895                          	
   896                          hl_noxswap
   897                          			; xend > x
   898                          +++
   899  0c49 e6a3               	INC ycount	; count to 0
   900  0c4b 205d0a                     JSR ginit	; map in graphic memory
   901                          
   902  0c4e 20740a                     JSR position	; graphic position x,y
   903                          
   904  0c51 a5a5               	LDA gaddr	; save position for vertical
   905  0c53 85fb               	STA sgaddr
   906  0c55 a5a6               	LDA gaddr+1
   907  0c57 85fc               	STA sgaddr+1
   908  0c59 86ab               	STX xsave
   909  0c5b 84a9               	STY ysave
   910                          
   911  0c5d a59e                       LDA xendl
   912  0c5f 2907                       AND #%00000111
   913  0c61 8596                       STA tmp2	; xend mod 8, mask index
   914  0c63 a59b                       LDA xl
   915  0c65 29f8                       AND #%11111000	; (xl div 8)*8
   916  0c67 8595                       STA tmp1
   917  0c69 a59e                       LDA xendl	; xend unmasked
   918  0c6b 38                         SEC
   919  0c6c e595                       SBC tmp1	; finally: xend - (x div 8)*8 
   920  0c6e 8595                       STA tmp1
   921  0c70 a59f                       LDA xendh
   922  0c72 e59c                       SBC xh
   923  0c74 4a                         LSR		; / 8 ->  0-39
   924  0c75 a595                       LDA tmp1	; only 1 highest bit
   925  0c77 6a                         ROR		; and 3 lower bits
   926  0c78 4a                         LSR
   927  0c79 4a                         LSR
   928                                  		; 8-pixel-blocks count
   929  0c7a 85a4               	STA hcount	; save for vertical extension
   930                           
   931                          hl_vertloop
   932  0c7c 98                 	TYA		; calculate max. Y in 8x8 block
   933  0c7d 18                 	CLC
   934  0c7e 65a3               	ADC ycount
   935  0c80 c908               	CMP #8
   936  0c82 9002               	BCC +
   937  0c84 a908               	LDA #8
   938  0c86 85a8               +	STA ylimit
   939                          
   940  0c88 bda709                     LDA maskleft,X	; starting mask
   941  0c8b 8595               	STA tmp1
   942  0c8d a6a4               	LDX hcount	; how many blocks
   943                          
   944                          hl_nextblock
   945  0c8f ca                         DEX
   946                          hl_islastblock
   947  0c90 301d                       BMI hl_lastblock
   948                          			; leave loop if X<0
   949  0c92 a4a9               	LDY ysave
   950  0c94 a595               -	LDA tmp1	; mask
   951  0c96 20f503             	JSR gmask	; first with left end mask
   952  0c99 c8                 	INY		; vertical down
   953  0c9a c4a8               	CPY ylimit	; in 8x8 box
   954  0c9c d0f6               	BNE -
   955                          
   956  0c9e 18                         CLC		; gaddr += 8 (one block to right)
   957  0c9f a5a5                       LDA gaddr
   958  0ca1 6908                       ADC #8
   959  0ca3 85a5                       STA gaddr
   960  0ca5 9002                       BCC +
   961  0ca7 e6a6                       INC gaddr+1
   962                          
   963  0ca9 a9ff               +	LDA #$FF	; following with full 8-pixel mask
   964  0cab 8595               	STA tmp1
   965  0cad d0e0               	BNE hl_nextblock	; always
   966                          
   967                          hl_lastblock
   968  0caf a696                       LDX tmp2	; xend mask index
   969  0cb1 3db109                     AND maskright,X ; A has current maskt combine with mask right end
   970  0cb4 8595               	STA tmp1	; mask
   971  0cb6 a4a9               	LDY ysave	; start position in 8x8 block
   972  0cb8 a595               -	LDA tmp1	; mask
   973  0cba 20f503             	JSR gmask	; modify
   974  0cbd c8                 	INY		; vertical down
   975  0cbe c6a3               	DEC ycount	; overall y counter
   976  0cc0 c4a8               	CPY ylimit
   977  0cc2 d0f4               	BNE -
   978                          
   979  0cc4 a5a3               	LDA ycount	; finished
   980  0cc6 d003               	BNE +		; roll-over into 8x8 block below
   981  0cc8 4c550a                     JMP gexit	; leave
   982                          
   983  0ccb 18                 +	CLC
   984  0ccc a5fb               	LDA sgaddr
   985  0cce 6940               	ADC #$40	; next 8-pixel row below
   986  0cd0 85fb               	STA sgaddr	; + $140 (320)
   987  0cd2 85a5               	STA gaddr
   988  0cd4 a5fc               	LDA sgaddr+1
   989  0cd6 6901               	ADC #$01
   990  0cd8 85fc               	STA sgaddr+1
   991  0cda 85a6               	STA gaddr+1
   992  0cdc a6ab               	LDX xsave	; initial mask index
   993  0cde a000               	LDY #0		; start on top of 8x8
   994  0ce0 84a9               	STY ysave
   995  0ce2 f098               	BEQ hl_vertloop
   996                          ;-----------------------------------------------------------------
   997                          
   998                          vline
   999  0ce4 20c70b                     JSR getxy	; get startpoint
  1000  0ce7 859c                       STA xh
  1001  0ce9 8d3d03                     STA savexh	; save as cursor too
  1002  0cec 849b                       STY xl
  1003  0cee 8c3c03                     STY savexl
  1004  0cf1 8693                       STX yend	; initial point is endpoint
  1005                          
  1006  0cf3 20f1b7                     JSR b_getcomma8bit
  1007                          			; get length
  1008  0cf6 18                         CLC		; calculate end point
  1009  0cf7 8a                         TXA		; length
  1010                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1011                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1012                          ;	STA tmp1
  1013  0cf8 6593                       ADC yend	; length + initial point is startpoint
  1014  0cfa b005               	BCS vline_iq	; > 255
  1015  0cfc c9c8                       CMP #ymax	; outside?
  1016  0cfe a8                 	TAY		; keep startpoint
  1017  0cff 9003                       BCC +
  1018                          vline_iq
  1019  0d01 202c0e                     JSR range_error ; corrupts A
  1020                          			; XXX Y = ymax-1 ?
  1021  0d04 84aa               +	STY y		; startpoint
  1022  0d06 8c3e03             	STY savey	; set cursor y position
  1023  0d09 18                 	CLC
  1024  0d0a 900e               	BCC +++		; skip following, because y, yend are already ordered
  1025                          
  1026                          vline_start		; entry point from line command (only)
  1027  0d0c a5aa               	LDA y		; order of y, yend is not defined
  1028  0d0e c593               	CMP yend
  1029  0d10 b008               	BCS vl_noyswap	; yend > y ->
  1030  0d12 a5aa               	LDA y		; swap y, yend
  1031  0d14 a693               	LDX yend
  1032  0d16 8593               	STA yend
  1033  0d18 86aa               	STX y
  1034                          vl_noyswap
  1035                          			; startpoint is below the endpoint
  1036  0d1a 205d0a             +++	JSR ginit	; map in graphic memory
  1037                          
  1038                          vl_start
  1039  0d1d 20740a                     JSR position	; graphic position x,y
  1040  0d20 bd7a09                     LDA bitmask,X
  1041  0d23 8596                       STA tmp2	; save mask
  1042                          ; DON'T-CHANGE: replace ...
  1043  0d25 38                         SEC
  1044  0d26 a5aa                       LDA y		; startpoint is greater!
  1045  0d28 e593                       SBC yend	; vertical length
  1046  0d2a aa                         TAX
  1047                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1048                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1049                          ;	LDX tmp1
  1050  0d2b e8                         INX		; +1 (exit on 0)
  1051  0d2c 38                 	SEC		; for subtraction, never changed!
  1052                          vl_nextline
  1053  0d2d a596                       LDA tmp2
  1054  0d2f 20f503                     JSR gmask	; modify 
  1055  0d32 88                         DEY		; go up
  1056  0d33 100e                       BPL +
  1057  0d35 a5a5                       LDA gaddr	; C=1
  1058  0d37 e940               	SBC #$40	; gaddr -= 320
  1059  0d39 85a5                       STA gaddr
  1060  0d3b a5a6                       LDA gaddr+1
  1061  0d3d e901                       SBC #$01
  1062  0d3f 85a6                       STA gaddr+1
  1063  0d41 a007                       LDY #7		; wrap y offset
  1064  0d43 ca                 +	DEX		; all vertical positions done?
  1065  0d44 d0e7                       BNE vl_nextline
  1066  0d46 4c550a                     JMP gexit	; leave
  1067                          
  1068                          
  1069                          ;-----------------------------------------------------------------
  1070                          
  1071                          line
  1072  0d49 20c70b                     JSR getxy	; get startpoint
  1073  0d4c 849b                       STY xl 
  1074  0d4e 859c                       STA xh
  1075  0d50 86aa                       STX y
  1076                          
  1077  0d52 20c40b                     JSR getcommaxy	; get endpoint
  1078                          line_start
  1079  0d55 8c3c03                     STY savexl	; save as cursor position too
  1080  0d58 849e                       STY xendl
  1081  0d5a 8d3d03                     STA savexh
  1082  0d5d 859f                       STA xendh
  1083  0d5f 8e3e03                     STX savey
  1084  0d62 8693                       STX yend
  1085                          
  1086  0d64 a000                       LDY #$00	; initialize to 0
  1087  0d66 84a8                       STY ydir
  1088  0d68 8495                       STY kl
  1089  0d6a 8496                       STY kh
  1090                          
  1091  0d6c 38                         SEC
  1092  0d6d a59b                       LDA xl		; calculate dx
  1093  0d6f e59e                       SBC xendl
  1094  0d71 85ab                       STA dxl
  1095  0d73 a59c                       LDA xh
  1096  0d75 e59f                       SBC xendh
  1097  0d77 85a7                       STA dxh
  1098                          
  1099  0d79 b018                       BCS li_xend_left
  1100                          	; dx != 0
  1101                          			; negate dx:
  1102  0d7b 98                         TYA		; Y=A=0
  1103  0d7c 38                         SEC		; dx = 0 - dx
  1104  0d7d e5ab                       SBC dxl
  1105  0d7f 85ab                       STA dxl
  1106  0d81 98                         TYA		; Y=A=0
  1107  0d82 e5a7                       SBC dxh
  1108  0d84 85a7                       STA dxh
  1109                          			; C=0 always, needed later
  1110  0d86 20990a             	jsr swap_x_xend
  1111  0d89 a6aa                       LDX y		; swap y
  1112  0d8b a493                       LDY yend
  1113  0d8d 8693                       STX yend
  1114  0d8f 84aa                       STY y
  1115                          
  1116  0d91 9007                       BCC li_x_different
  1117                          			; C=0 always (from negation before)
  1118                          
  1119                          li_xend_left
  1120                                  		; A already contains dxh
  1121  0d93 05ab                       ORA dxl		; dx = 0?
  1122  0d95 d003                       BNE li_x_different
  1123  0d97 4c0c0d                     JMP vline_start	; vertical line case
  1124                          
  1125                          li_x_different
  1126  0d9a 38                         SEC		; calculate dy
  1127  0d9b a593                       LDA yend
  1128  0d9d e5aa                       SBC y
  1129  0d9f b006                       BCS li_y_right	; yend >= y?
  1130  0da1 49ff                       EOR #$FF	; no, negate dy (two's complement)
  1131  0da3 6901                       ADC #$01	; C=0
  1132  0da5 85a8                       STA ydir	; always not 0: flag y goes up
  1133                          
  1134                          li_y_right
  1135  0da7 85a9                       STA dy
  1136  0da9 d007                       BNE +
  1137  0dab a900               	LDA #0		; line thickness = 1
  1138  0dad 85a3               	STA ycount
  1139  0daf 4c460c                     JMP hline_start	; horizontal line case
  1140                          +
  1141                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1142                          
  1143  0db2 a5a7                       LDA dxh		; dx > dy
  1144  0db4 d01c                       BNE line_flat	; yes -> flat
  1145  0db6 a5a9                       LDA dy		; no -> steep
  1146  0db8 aa                         TAX
  1147  0db9 c5ab                       CMP dxl
  1148  0dbb 9015                       BCC line_flat
  1149                          
  1150                          line_steep
  1151  0dbd e8                         INX	
  1152  0dbe 86a3                       STX cl		; c = dy+1
  1153  0dc0 4a                         LSR		; dy/2
  1154  0dc1 49ff               	EOR #$FF	; one's complement
  1155  0dc3 8595                       STA kl		; k = -dy/2 -1
  1156                          
  1157  0dc5 205d0a                     JSR ginit	; map in graphic memory
  1158                          
  1159  0dc8 a5a8                       LDA ydir
  1160  0dca d003                       BNE +
  1161  0dcc 4c870b                     JMP line_down_steep	; y down, steep
  1162  0dcf 4caa0a             +	JMP line_up_steep	; y up, steep
  1163                          
  1164                          line_flat
  1165  0dd2 a5a7                       LDA dxh
  1166  0dd4 a8                         TAY
  1167  0dd5 a6ab                       LDX dxl
  1168  0dd7 e8                         INX
  1169  0dd8 d001                       BNE +
  1170  0dda c8                         INY
  1171  0ddb 86a3               +	STX cl		; c = dx+1
  1172  0ddd 84a4                       STY ch
  1173                          
  1174  0ddf 4a                         LSR		; dx/2 high
  1175  0de0 49ff               	EOR #$FF	; one's complement
  1176  0de2 8596                       STA kh
  1177  0de4 a5ab                       LDA dxl
  1178  0de6 6a                         ROR		; dx/2 low
  1179  0de7 49ff               	EOR #$FF	; one's complement
  1180  0de9 8595                       STA kl		; k = -dx/2 - 1
  1181                          
  1182  0deb 205d0a                     JSR ginit	; map in graphic memory
  1183                          
  1184  0dee a5a8                       LDA ydir	
  1185  0df0 d003                       BNE +
  1186  0df2 4c360b                     JMP line_down_flat	; y down, flat
  1187  0df5 4ce60a             +	JMP line_up_flat	; y up, flat
  1188                          
  1189                          ;-----------------------------------------------------------------
  1190                          
  1191                          plot
  1192  0df8 20c70b                     JSR getxy	; get parameter
  1193  0dfb 859c                       STA xh		; save x/y
  1194  0dfd 849b                       STY xl
  1195  0dff 86aa                       STX y
  1196  0e01 8d3d03                     STA savexh	; and store as cursor
  1197  0e04 8c3c03                     STY savexl
  1198  0e07 8e3e03                     STX savey
  1199                          
  1200                          plot_start
  1201  0e0a 20740a                     JSR position	; calculate graphical address
  1202                          
  1203  0e0d a501                       LDA prozport
  1204  0e0f 29fd                       AND #%11111101	; Kernal ROM disable
  1205  0e11 78                         SEI			
  1206  0e12 8501                       STA prozport
  1207                          
  1208  0e14 20ed03                     JSR gchange	; change graphical data
  1209                          
  1210  0e17 a501                       LDA prozport
  1211  0e19 0902                       ORA #%00000010	; kernal ROM enable
  1212  0e1b 8501                       STA prozport
  1213  0e1d 58                         CLI
  1214  0e1e 60                         RTS
  1215                          
  1216                          ;-----------------------------------------------------------------
  1217                          
  1218                          move
  1219  0e1f 20c70b                     JSR getxy	; get parameter
  1220  0e22 8d3d03                     STA savexh	; just save as cursor
  1221  0e25 8c3c03                     STY savexl
  1222  0e28 8e3e03                     STX savey
  1223  0e2b 60                         RTS
  1224                          
  1225                          
  1226                          ;-----------------------------------------------------------------
  1227                          
  1228                          ; never touches X, Y, C-flag
  1229                          ; on exit: A corrupted, Z=0
  1230                          
  1231                          range_error
  1232  0e2c ad3f03             	LDA savemo
  1233  0e2f 29f0               	AND #$F0
  1234  0e31 d003               	BNE +
  1235  0e33 68                 	PLA			; cleanup JSR
  1236  0e34 68                 	PLA			; highbyte of return address >0
  1237  0e35 60                 -	RTS			; error mode 0: do nothing, back to caller before
  1238                          				; error mode 2: cut value: control back
  1239                          				; to handle value correction
  1240                          				; Z=0
  1241  0e36 2920               +	AND #$20
  1242  0e38 d0fb               	BNE -			; Z=0
  1243  0e3a 68                 	PLA			; cleanup JSR
  1244  0e3b 68                 	PLA
  1245                          setmode_error
  1246  0e3c 4c48b2             	JMP b_illquant		; error mode 1: throw error message
  1247                          
  1248                          ;-----------------------------------------------------------------
  1249                          
  1250                          setmode
  1251  0e3f 209eb7                     JSR b_get8bit
  1252  0e42 e003                       CPX #3
  1253  0e44 9013                       BCC +			; less then 3, modification mode
  1254  0e46 e006               	CPX #6
  1255  0e48 b0f2               	BCS setmode_error	; out of range
  1256                          				; error mode
  1257  0e4a 8a                 	TXA
  1258  0e4b 690d               	ADC #13			; C=0, therefore -3
  1259                          				; 3-5 -> 16-18
  1260                          				; put A's bit 4-7 into savemo
  1261  0e4d 4d3f03             	EOR savemo		; ********
  1262  0e50 29f0               	AND #%11110000		; ****0000
  1263  0e52 4d3f03             	EOR savemo		; AAAAmmmm
  1264  0e55 8d3f03             	STA savemo		; 
  1265  0e58 60                 	RTS
  1266                          
  1267  0e59 8a                 +	TXA
  1268  0e5a 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1269  0e5d 290f               	AND #%00001111
  1270  0e5f 4d3f03             	EOR savemo
  1271  0e62 8d3f03             	STA savemo
  1272                          setmode_enter
  1273  0e65 e001               	CPX #$01
  1274  0e67 b01a                       BCS set_or_toggle
  1275                          
  1276                          modereset
  1277  0e69 a909                       LDA #>(nbitmask)
  1278  0e6b 8df103                     STA gchange_op+2
  1279  0e6e a982                       LDA #<(nbitmask)
  1280  0e70 8df003                     STA gchange_op+1
  1281  0e73 a93d                       LDA #$3D		; opcode AND abs,X
  1282  0e75 8def03                     STA gchange_op
  1283  0e78 a931                       LDA #$31		; opcode AND (zp),Y
  1284  0e7a 8df703                     STA gmask_op
  1285  0e7d a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1286  0e7f 8df603                     STA gmask_flip+1
  1287  0e82 60                         RTS
  1288                          
  1289                          set_or_toggle
  1290  0e83 d01a                       BNE modetoggle
  1291                          modeset
  1292  0e85 a909                       LDA #>(bitmask)
  1293  0e87 8df103                     STA gchange_op+2
  1294  0e8a a97a                       LDA #<(bitmask)
  1295  0e8c 8df003                     STA gchange_op+1
  1296  0e8f a91d                       LDA #$1D		; opcode OR abs,X
  1297  0e91 8def03                     STA gchange_op
  1298  0e94 a911                       LDA #$11		; opcode OR (zp),Y
  1299  0e96 8df703                     STA gmask_op
  1300  0e99 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1301  0e9b 8df603                     STA gmask_flip+1
  1302  0e9e 60                         RTS
  1303                          
  1304                          modetoggle
  1305  0e9f a909                       LDA #>(bitmask)
  1306  0ea1 8df103                     STA gchange_op+2
  1307  0ea4 a97a                       LDA #<(bitmask)
  1308  0ea6 8df003                     STA gchange_op+1
  1309  0ea9 a95d                       LDA #$5D		; opcode EOR abs,X
  1310  0eab 8def03                     STA gchange_op
  1311  0eae a951                       LDA #$51		; opcode EOR (zp),Y
  1312  0eb0 8df703                     STA gmask_op
  1313  0eb3 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1314  0eb5 8df603                     STA gmask_flip+1
  1315  0eb8 60                         RTS
  1316                          
  1317                          
  1318                          ;-----------------------------------------------------------------
  1319                          ; get current x cursor position
  1320                          
  1321                          getposx
  1322  0eb9 ac3c03             	LDY savexl
  1323  0ebc ad3d03             	LDA savexh
  1324  0ebf 2091b3             	JSR b_word2fac
  1325  0ec2 4c7300             	JMP chrget	; last position of expression (function name)
  1326                          
  1327                          ;-----------------------------------------------------------------
  1328                          ; get current y cursor position
  1329                          
  1330                          getposy
  1331  0ec5 ac3e03             	LDY savey
  1332  0ec8 20a2b3             	JSR b_byte2fac
  1333  0ecb 4c7300             	JMP chrget	; last position of expression (function name)
  1334                          
  1335                          ;-----------------------------------------------------------------
  1336                          
  1337                          ; get pixel (check if pixel set)
  1338                          ; not used
  1339                          
  1340                          get
  1341  0ece 207300             	JSR chrget	; advance past function name
  1342  0ed1 20faae             	JSR b_chkparl	; "("?
  1343  0ed4 20c70b                     JSR getxy	; get X,Y values
  1344  0ed7 859c                       STA xh
  1345  0ed9 849b                       STY xl
  1346  0edb 86aa                       STX y
  1347  0edd 207900             	JSR chrgot
  1348  0ee0 20f7ae             	JSR b_chkparr	; ")"?
  1349                          	
  1350                          
  1351  0ee3 20740a                     JSR position	; calculate graphic address/position
  1352                          
  1353  0ee6 a501                       LDA prozport
  1354  0ee8 29fd               	AND #%11111101	; Kernal ROM disable
  1355  0eea 78                         SEI
  1356  0eeb 8501                       STA prozport
  1357                          
  1358  0eed b1a5                       LDA (gaddr),Y
  1359  0eef 3d7a09                     AND bitmask,X	; mask position
  1360  0ef2 a8                         TAY
  1361  0ef3 a501                       LDA prozport
  1362  0ef5 0902               	ORA #%00000010	; kernal ROM enable
  1363  0ef7 8501                       STA prozport
  1364  0ef9 58                         CLI
  1365  0efa 98                 	TYA
  1366  0efb f002               	BEQ +
  1367  0efd a001               	LDY #1		; <> 0 -> always return 1
  1368  0eff 4ca2b3             +	JMP b_byte2fac	; still on expr.'s last character
  1369                          
  1370                          ;-----------------------------------------------------------------
  1371                          
  1372                          relto
  1373  0f02 208aad                     JSR b_getval	; get X offset (+/-)
  1374  0f05 a561               	LDA facexp	; FAC exponent
  1375  0f07 c990               	CMP #$90	; more than 16 bit
  1376  0f09 b031               	BCS relto_error	; illegal quantity
  1377  0f0b 209bbc                     JSR b_fac2int	; to signed integer
  1378                          
  1379  0f0e 18                         CLC
  1380  0f0f a565                       LDA facintl
  1381  0f11 6d3c03                     ADC savexl
  1382  0f14 859e                       STA xendl
  1383  0f16 a564                       LDA facinth
  1384  0f18 6d3d03                     ADC savexh
  1385  0f1b 859f                       STA xendh	; xend = savex+facint
  1386                          
  1387  0f1d 20fdae                     JSR b_getcomma	; get Y offset (+/-)
  1388  0f20 208aad                     JSR b_getval
  1389  0f23 a561                       LDA facexp	; FAC exponent
  1390  0f25 c990                       CMP #$90	; more than 16 bit
  1391  0f27 b013                       BCS relto_error	; illegal quantity
  1392  0f29 209bbc                     JSR b_fac2int	; to signed integer
  1393  0f2c 18                         CLC
  1394  0f2d a565                       LDA facintl
  1395  0f2f 6d3e03                     ADC savey
  1396  0f32 8593                       STA yend	; yend = savey+facint
  1397                          
  1398  0f34 a59f                       LDA xendh	; check end coord. x
  1399  0f36 c901                       CMP #>xmax
  1400  0f38 900e                       BCC rt_xok
  1401  0f3a f003                       BEQ +
  1402                          relto_error
  1403  0f3c 202c0e                     JSR range_error
  1404  0f3f a59e               +	LDA xendl
  1405  0f41 c940                       CMP #<xmax
  1406  0f43 9003                       BCC +
  1407  0f45 202c0e                     JSR range_error
  1408                          +
  1409                          rt_xok
  1410  0f48 a593                       LDA yend	; check end coord. y
  1411  0f4a c9c8                       CMP #ymax
  1412  0f4c 9003                       BCC +
  1413  0f4e 202c0e                     JSR range_error
  1414                          +
  1415  0f51 ad3c03                     LDA savexl
  1416  0f54 859b                       STA xl
  1417  0f56 ad3d03                     LDA savexh
  1418  0f59 859c                       STA xh
  1419  0f5b ad3e03                     LDA savey
  1420  0f5e 85aa                       STA y
  1421  0f60 a49e                       LDY xendl
  1422  0f62 a59f                       LDA xendh
  1423  0f64 a693                       LDX yend	; xend/yend = cursor + x/y
  1424                          
  1425  0f66 4c550d                     JMP line_start	; draw line x/y to xend/yend
  1426                          
  1427                          
  1428                          ;-----------------------------------------------------------------
  1429                          
  1430                          char
  1431  0f69 209eb7                     JSR b_get8bit	; get char. position x 0-39
  1432  0f6c e028                       CPX #40	
  1433  0f6e 9003                       BCC +
  1434                          char_error
  1435  0f70 4c48b2                     JMP b_illquant
  1436  0f73 86fb               +	STX gpos	; save x coord.
  1437  0f75 20f1b7                     JSR b_getcomma8bit
  1438                          			; get char. position y 0-24
  1439  0f78 e019                       CPX #25
  1440  0f7a b0f4                       BCS char_error
  1441  0f7c 86fc                       STX gpos+1	; save y coord.
  1442                          
  1443  0f7e 20fdae                     JSR b_getcomma	; get string
  1444  0f81 209ead                     JSR b_getexpr
  1445  0f84 20a3b6                     JSR b_stringval ; string address in str
  1446  0f87 48                         PHA		; string length
  1447  0f88 a6fc                       LDX gpos+1	; y coord. for char. position
  1448  0f8a 8a                         TXA
  1449  0f8b 2903                       AND #$03	; mask 2 bits
  1450  0f8d a8                         TAY		; table index
  1451  0f8e a900                       LDA #$00
  1452  0f90 85fc                       STA gpos+1	; x high
  1453  0f92 a5fb                       LDA gpos	; saved x: multiply by 8
  1454  0f94 0a                         ASL
  1455  0f95 0a                         ASL
  1456  0f96 0a                         ASL
  1457  0f97 26fc                       ROL gpos+1	; overflow to high byte
  1458  0f99 798a09                     ADC ytabl,Y
  1459  0f9c 85a5                       STA gaddr
  1460  0f9e a5fc                       LDA gpos+1	; x high
  1461  0fa0 7d8e09                     ADC ytabh,X
  1462  0fa3 85a6                       STA gaddr+1
  1463  0fa5 68                         PLA		; string length
  1464  0fa6 a000                       LDY #$00	; string index
  1465  0fa8 aa                         TAX		; length
  1466  0fa9 e8                         INX		; prepare as counter
  1467                          char_loop
  1468  0faa ca                         DEX
  1469  0fab f008                       BEQ char_exit
  1470  0fad b122                       LDA (str),Y	; read string
  1471  0faf 20b60f                     JSR char_display
  1472  0fb2 c8                         INY
  1473  0fb3 d0f5                       BNE char_loop
  1474                          char_exit
  1475  0fb5 60                         RTS
  1476                          
  1477                          char_display
  1478  0fb6 85d7                       STA z_tmp	; character (lastkey, temporary reused)
  1479  0fb8 8a                         TXA		; save register X+Y
  1480  0fb9 48                         PHA
  1481  0fba 98                         TYA
  1482  0fbb 48                         PHA
  1483  0fbc a5d7                       LDA z_tmp	; get saved character
  1484  0fbe 3012                       BMI char_inverse
  1485                          
  1486                          char_normal
  1487  0fc0 c920                       CMP #$20	; control character?
  1488  0fc2 9054                       BCC char_disp_leave
  1489  0fc4 c960                       CMP #$60
  1490  0fc6 9004                       BCC +
  1491  0fc8 29df                       AND #%11011111	; $60-$7F -> $40-$5F
  1492  0fca d014                       BNE char_hires
  1493  0fcc 293f               +	AND #%00111111  ; $40-$5F -> $00-$1F
  1494  0fce d010               	BNE char_hires
  1495  0fd0 f00e               	BEQ char_hires
  1496                          
  1497                          char_inverse
  1498  0fd2 297f                       AND #%01111111	; mask bit 7
  1499  0fd4 c97f                       CMP #%01111111	; was 255? (pi)
  1500  0fd6 d002                       BNE +
  1501  0fd8 a95e                       LDA #$5E	; screen code for pi
  1502  0fda c920               +	CMP #$20	; control character?
  1503  0fdc 903a                       BCC char_disp_leave
  1504                          			; yes, skip
  1505  0fde 0940                       ORA #%01000000	; $A0-$BF -> $60-$7F
  1506                          			; $C0-$FF -> $40-$7F
  1507                          			; OPT: BNE char_hires
  1508                          			; OPT: char_normal
  1509                          char_hires
  1510  0fe0 a6c7                       LDX z_reverseflag
  1511  0fe2 f002                       BEQ +
  1512  0fe4 0980                       ORA #%10000000	; invert char.
  1513  0fe6 aa                 +	TAX		; save char. for later
  1514  0fe7 a501                       LDA prozport	; save prozport state
  1515  0fe9 48                 	PHA
  1516  0fea a921                       LDA #%00100001	; char. rom, no basic and kernal rom
  1517  0fec 78                         SEI
  1518  0fed 8501                       STA prozport	; char. rom base = $D000
  1519  0fef a91a                       LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
  1520  0ff1 85fc                       STA gpos+1	; 
  1521  0ff3 8a                         TXA		; char. code
  1522  0ff4 0a                         ASL		; *8
  1523  0ff5 26fc                       ROL gpos+1
  1524  0ff7 0a                         ASL
  1525  0ff8 26fc                       ROL gpos+1
  1526  0ffa 0a                         ASL
  1527  0ffb 26fc                       ROL gpos+1
  1528  0ffd 85fb                       STA gpos	; addr. in char. rom for char.
  1529                          
  1530  0fff a007                       LDY #$07	; 8 hires lines
  1531                          char_line
  1532  1001 b1fb                       LDA (gpos),Y	; read character line
  1533  1003 20f503                     JSR gmask	; write to hires screen
  1534  1006 88                         DEY
  1535  1007 10f8                       BPL char_line
  1536                          
  1537  1009 68                 	PLA
  1538  100a 8501                       STA prozport
  1539  100c 58                         CLI
  1540                          
  1541  100d 18                         CLC		; step char position to left
  1542  100e a5a5                       LDA gaddr	; ( +8 )
  1543  1010 6908                       ADC #$08
  1544  1012 85a5                       STA gaddr
  1545  1014 9002                       BCC +
  1546  1016 e6a6                       INC gaddr+1
  1547                          +
  1548                          char_disp_leave
  1549  1018 68                 	PLA		; pass written character back
  1550  1019 a8                         TAY		; restore saved registers
  1551  101a 68                         PLA
  1552  101b aa                         TAX
  1553  101c 60                         RTS
  1554                          
  1555                          
  1556                          ;-----------------------------------------------------------------
  1557                          
  1558                          to
  1559  101d ad3c03                     LDA savexl
  1560  1020 859b                       STA xl
  1561  1022 ad3d03                     LDA savexh
  1562  1025 859c                       STA xh
  1563  1027 ad3e03                     LDA savey
  1564  102a 85aa                       STA y
  1565  102c 20c70b                     JSR getxy
  1566  102f 4c550d                     JMP line_start
  1567                          
  1568                          ;-----------------------------------------------------------------
  1569                          
  1570                          fill
  1571  1032 20c70b                     JSR getxy
  1572  1035 859c                       STA xh			; save x/y
  1573  1037 849b                       STY xl
  1574  1039 86aa                       STX y
  1575  103b 8d3d03                     STA savexh		; and store as cursor
  1576  103e 8c3c03                     STY savexl
  1577  1041 8e3e03                     STX savey
  1578                                  
  1579  1044 a531                       LDA basaryend		; initialize fill stack pointer
  1580  1046 38                 	SEC
  1581  1047 e904               	SBC #4			; one element below
  1582  1049 85fd                       STA fstack		; use space between basic arrays
  1583  104b a532                       LDA basaryend+1		; and string heap bottom
  1584  104d e900               	SBC #0			; take borrow
  1585  104f 85fe                       STA fstack+1
  1586                          
  1587  1051 20740a             	JSR position		; graphic position in (gaddr)+Y, bit X
  1588                          
  1589  1054 a59c               	LDA xh			; setup 8x8 block index (x8)
  1590  1056 4a                 	LSR			; high bit into C
  1591  1057 a59b               	LDA xl
  1592  1059 2a                 	ROL			; take high bit
  1593  105a 4a                 	LSR
  1594  105b 4a                 	LSR			; finally divide by 8
  1595  105c 85a7               	STA x8			; = index of 8x8 block in bitmap
  1596                          
  1597                          	; set fmode (from mode)
  1598  105e ad3f03             	LDA savemo
  1599  1061 2903               	AND #3
  1600  1063 aa                 	TAX
  1601  1064 ca                 	DEX
  1602  1065 3003               	BMI +			; mode = 0 -> invertmask: $FF
  1603  1067 f001               	BEQ +			; mode = 1 -> invertmask: $00
  1604  1069 ca                 	DEX			; mode = 2 -> ? (same as mode=0)
  1605  106a 86a8               +	STX fmode		; mode set or reset
  1606                          
  1607  106c 205d0a             	JSR ginit		; map in bitmap memory
  1608                          
  1609  106f b1a5               	LDA (gaddr),y		; graphic position in Y (in index in 8x8 block)
  1610  1071 45a8               	EOR fmode
  1611  1073 8595               	STA tmp1		; bitmap, for later usage
  1612                          
  1613  1075 3d7a09             	AND bitmask,x		; test start pixel
  1614  1078 f003               	BEQ +			; not set
  1615                          f_exit
  1616  107a 4c550a             	JMP gexit		; leave if start pixel is already set
  1617                          +
  1618                          f_start				; the start: in mid of a line to fill ...
  1619  107d a900               	LDA #0
  1620  107f 8596               	STA fcont		; initialize continuation flag for line above und below
  1621                          
  1622  1081 a595               	LDA tmp1		; graphic pixel data
  1623                          				; extent bitmask to the right
  1624  1083 86ab               	STX xsave
  1625  1085 3da709             	AND maskleft,x		; mask out left part, bits right from starting point remain
  1626  1088 201812             	JSR bitposr		; find the first set bit from start to right (border)
  1627  108b bdb009             	LDA maskright0,x	; get a mask from the right border to left
  1628  108e 85a3               	STA tmpmask		
  1629                          
  1630                          leftcont
  1631  1090 a595               	LDA tmp1		; graphic pixel data
  1632  1092 a6ab               	LDX xsave
  1633                          leftcont_a
  1634  1094 3db109             	AND maskright,x		; mask out right part, bits left from starting point remain
  1635  1097 f00e               	BEQ stepleft8		; no left border in this pixel line
  1636  1099 200c12             	JSR bitposl		; find the first set bit from start to left (border)
  1637  109c bda709             	LDA maskleft0,x		; get a mask from the left border to right
  1638  109f 25a3               	AND tmpmask		; intersect masks
  1639  10a1 85a3               	STA tmpmask		; and store it for later
  1640  10a3 f021               	BEQ next_block		; empty mask immediate continue to right
  1641  10a5 d047               	BNE to_right		; start to walk and fill to the right border
  1642                          
  1643                          stepleft8
  1644  10a7 a5a7               	LDA x8 			; 8x8 block position
  1645  10a9 f043               	BEQ to_right		; =0, hit screen border
  1646  10ab c6a7               	DEC x8			; count step 8x8 block to left
  1647  10ad a9ff               	LDA #$ff
  1648  10af 85a3               	STA tmpmask		; initial mask full pixel line
  1649                          
  1650  10b1 38                 	SEC 			; graphic address to to next pixel line/block
  1651  10b2 a5a5               	LDA gaddr
  1652  10b4 e908               	SBC #8
  1653  10b6 b002               	BCS +
  1654  10b8 c6a6               	DEC gaddr+1
  1655  10ba 85a5               +	STA gaddr
  1656                          
  1657                          	; y left unchanged
  1658  10bc b1a5               	LDA (gaddr),y		; real graphic pixel data from bitmap
  1659  10be 45a8               	EOR fmode		; set/reset mode
  1660  10c0 8595               	STA tmp1		; graphic pixel data
  1661  10c2 a207               	LDX #7			; start bit 0 (index 7, rightmost)
  1662  10c4 d0ce               	BNE leftcont_a		; loop to left border search
  1663                          	
  1664                          next_block
  1665  10c6 e6a7               	INC x8			; step right a block
  1666  10c8 a5a7               	LDA x8
  1667  10ca c928               	CMP #40			; beyond last horizontal block?
  1668  10cc b077               	BCS process_stack	; done if right screen border
  1669                          	; C = 0
  1670  10ce a5a5               	LDA gaddr		; advance to block right
  1671  10d0 6908               	ADC #8			; gaddr = gaddr + 8
  1672  10d2 85a5               	STA gaddr
  1673  10d4 9002               	BCC +
  1674  10d6 e6a6               	INC gaddr+1
  1675  10d8 a9ff               +	LDA #$ff		; asume "all pixels" mask
  1676  10da 85a3               	STA tmpmask
  1677  10dc b1a5               	LDA (gaddr),y		; pixel data
  1678  10de 45a8               	EOR fmode		; set/reset mode
  1679  10e0 f00c               	BEQ to_right		; empty -> finally to to_right
  1680  10e2 201812             	JSR bitposr		; search right border
  1681  10e5 bdb009             	LDA maskright0,x	; mask out the right part
  1682  10e8 25a3               	AND tmpmask		; shorten mask accordingly
  1683  10ea 85a3               	STA tmpmask
  1684  10ec f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1685                          				; leading to 0 mask (fill_check wont't
  1686                          				; handle this special case)
  1687                          
  1688                          				; continue to fill to right ...
  1689                          to_right			; fill loop towards right border
  1690  10ee a5a3               	LDA tmpmask		; fill mask
  1691                          				; assert:    (bitmap & tempmask) == 0
  1692                          				;         || (bitmap & tempmask) == tempmask
  1693  10f0 51a5               	EOR (gaddr),y		; set/reset to fill
  1694  10f2 91a5               	STA (gaddr),y		; into bitmap - the actual fill action!
  1695                          	
  1696                          check_above
  1697  10f4 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1698                          				; c = 0!
  1699  10f6 84a9               	STY ysave		; to be restored later
  1700  10f8 a5a5               	LDA gaddr		; current graphic position
  1701  10fa a6a6               	LDX gaddr+1
  1702  10fc 88                 	DEY			; line above
  1703  10fd 100f               	BPL +			; leaving 8x8 block?
  1704                          	; c=0 (asl fcont)
  1705  10ff e93f               	SBC #$40-1		; block above:
  1706  1101 85fb               	STA caddr		; caddr = gaddr - $140
  1707  1103 8a                 	TXA
  1708  1104 e901               	SBC #$01
  1709  1106 aa                 	TAX
  1710  1107 c9e0               	CMP #>gram		; still graphic ram?
  1711  1109 900a               	BCC skip_above
  1712  110b a007               	LDY #7			; last line in block in new block
  1713  110d 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1714  110e 85fb               +	STA caddr		; still in same block
  1715  1110 86fc               ++	STX caddr+1		; shared store
  1716  1112 20a311             	JSR fill_check
  1717                          skip_above
  1718                          
  1719                          check_below
  1720  1115 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1721  1117 a5a5               	LDA gaddr		; current graphic position
  1722  1119 a6a6               	LDX gaddr+1
  1723  111b a4a9               	LDY ysave		; restore original y position
  1724  111d c8                 	INY			; line below
  1725  111e c008               	CPY #8			; crossing 8x8 block?
  1726  1120 9014               	BCC +			; less then 8
  1727                          	; c=1 (cpy)
  1728  1122 693f               	ADC #$40-1		; block below: accu has gaddr
  1729  1124 85fb               	STA caddr		; caddr = gaddr + $140
  1730  1126 a8                 	TAY			; for compare later
  1731  1127 8a                 	TXA			; gaddr high
  1732  1128 6901               	ADC #$01
  1733  112a aa                 	TAX
  1734  112b b010               	BCS skip_below		; > $10000  -> skip
  1735  112d c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1736  112f e9ff               	SBC #>(gram+8000)
  1737  1131 b00a               	BCS skip_below		; greater, so skip
  1738  1133 a000               	LDY #0			; first line in block
  1739  1135 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1740  1136 85fb               +	STA caddr		; transfer unchanged
  1741  1138 86fc               ++	STX caddr+1		; shared store
  1742  113a 20a311             	JSR fill_check
  1743                          skip_below
  1744                          
  1745  113d a4a9               	LDY ysave		; restore original y position
  1746  113f a5a3               	LDA tmpmask		; mask:
  1747  1141 2901               	AND #%00000001		; open to right, continue?
  1748  1143 d081               	BNE next_block		; to next block if open
  1749                          ; long branch version
  1750                          ;	BEQ process_stack	; not open, finished
  1751                          ;	JMP next_block		; to next block if open
  1752                          
  1753                          process_stack
  1754  1145 a5fd               	LDA fstack		; stack empty?
  1755  1147 c531               	CMP basaryend
  1756  1149 a5fe               	LDA fstack+1
  1757  114b e532               	SBC basaryend+1
  1758  114d b003               	BCS +			; fstack >= basaryend -> not empty
  1759  114f 4c550a             	JMP gexit		; empty, we are finished
  1760                          
  1761  1152 a003               +	LDY #4-1		; top of stack, element's last component
  1762  1154 b1fd               	LDA (fstack),y
  1763  1156 85a7               	STA x8			; 8x8 block position
  1764  1158 88                 	DEY
  1765  1159 b1fd               	LDA (fstack),y
  1766  115b 85a3               	STA tmpmask		; pixel mask
  1767  115d 88                 	DEY
  1768  115e b1fd               	LDA (fstack),y
  1769  1160 85a6               	STA gaddr+1		; graphic addr high byte
  1770  1162 88                 	DEY
  1771  1163 b1fd               	LDA (fstack),y		; graphic addr low byte combined with y-line
  1772  1165 aa                 	TAX			; needed twice
  1773  1166 29f8               	AND #%11111000		; split off address
  1774  1168 85a5               	STA gaddr
  1775  116a 8a                 	TXA
  1776  116b 2907               	AND #%00000111		; split off y-line
  1777  116d a8                 	TAY
  1778                          	
  1779  116e b1a5               	LDA (gaddr),y		; get pixels
  1780  1170 45a8               	EOR fmode		; according to set/reset
  1781  1172 aa                 	TAX			; keep it for later
  1782  1173 25a3               	AND tmpmask		; focus on masked pixels
  1783  1175 08                 	PHP			; save Z flag
  1784  1176 f004               	BEQ pop_stack		; all bits unset, remove from stack
  1785                          				; and fill it!
  1786  1178 c5a3               	CMP tmpmask		; all gaps filled?
  1787  117a d010               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1788                          	; all gaps filled, next on stack 
  1789                          pop_stack
  1790  117c 38                 	SEC	
  1791  117d a5fd               	LDA fstack		; remove entry from stack
  1792  117f e904               	SBC #4			; entry size
  1793  1181 85fd               	STA fstack
  1794  1183 b002               	BCS +
  1795  1185 c6fe               	DEC fstack+1
  1796  1187 28                 +	PLP
  1797  1188 d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1798                          				; all bits unset,
  1799  118a f001               	BEQ ++			; stack already cleaned up
  1800  118c 28                 +++	PLP			; stack cleanup
  1801                          
  1802                          	; set bits outside mask to 1
  1803  118d 8a                 ++	TXA			; bitmap
  1804                          				; 00100110	
  1805  118e 49ff               	EOR #$ff		; 11011001
  1806  1190 25a3               	AND tmpmask		; 00011100 -> 00011000
  1807  1192 49ff               	EOR #$ff		; 11100111
  1808                          				; pixel outside tmpmask now set!
  1809  1194 a2ff               	LDX #$ff		; pixel gap search: first one from left
  1810  1196 e8                 -	INX
  1811  1197 0a                 	ASL			; counting from left
  1812  1198 b0fc               	BCS -			; loop if pixel is set
  1813                          				; X has the bit number of the unset pixel
  1814  119a b1a5               	LDA (gaddr),y		; setup value for processing a new line
  1815  119c 45a8               	EOR fmode		; set/reset mode
  1816  119e 8595               	STA tmp1		; temporary bitmap pixels
  1817  11a0 4c7d10             	JMP f_start		; long (to far away) jump to fill line start
  1818                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1819                          
  1820                          
  1821                          ; Check upper or lower fill path
  1822                          ;		destroys x
  1823                          
  1824                          fill_check
  1825  11a3 b1fb               	LDA (caddr),y
  1826  11a5 45a8               	EOR fmode		; pixel data
  1827  11a7 aa                 	TAX			; save for later
  1828  11a8 25a3               	AND tmpmask		; mask to fill
  1829  11aa f015               	BEQ fc_cleared		; all masked pixels cleared?
  1830  11ac c5a3               	CMP tmpmask		; check for gaps
  1831  11ae f05b               	BEQ fc_exit		; all gaps filled, finished
  1832                          				; if not so, some pixels still set
  1833  11b0 a5a3               	LDA tmpmask
  1834                          fc_checkstart			; no continuation, init flag based on
  1835                          				; rightmost pixel:
  1836  11b2 4a                 	LSR			; mask bit 0 to carry
  1837  11b3 9019               	BCC fc_nocont		; maskbit empty?
  1838  11b5 8a                 	TXA			; pixel data
  1839  11b6 4a                 	LSR			; pixel bit 0 to carry
  1840  11b7 b015               	BCS fc_nocont		; bit 0 set
  1841                          				; -> mask is 1 and pixel 0
  1842                          fc_cont
  1843  11b9 a596               	LDA fcont		; set flag for continuation
  1844  11bb 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1845  11bd 8596               	STA fcont
  1846  11bf d013               	BNE push_to_stack	; always non zero
  1847                          
  1848                          fc_cleared
  1849  11c1 a5a3               	LDA tmpmask		; pixel & mask -> 0
  1850                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1851                          				; the caller asserts that this never happens
  1852  11c3 c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1853  11c5 d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1854                          				; 8 pixel line empty
  1855  11c7 a596               	LDA fcont		; continued gap?
  1856  11c9 2902               	AND #%00000010		; check bit 2
  1857  11cb f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1858  11cd 60                 	RTS			; gap continued and already on stack, leave
  1859                          
  1860                          fc_nocont
  1861  11ce a596               	LDA fcont		; clear continuation flag
  1862  11d0 29fd               	AND #%11111101		; clear bit 2
  1863  11d2 8596               	STA fcont
  1864                          
  1865                          push_to_stack
  1866  11d4 18                 	CLC			; fstack points to top of stack
  1867  11d5 a5fd               	LDA fstack		; to next free stack element
  1868  11d7 6904               	ADC #4			; entry size
  1869  11d9 85fd               	STA fstack
  1870  11db 9002               	BCC +
  1871  11dd e6fe               	INC fstack+1
  1872                          +
  1873  11df a534               	LDA strbot+1		; check stack space
  1874  11e1 c5fe               	CMP fstack+1
  1875  11e3 b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  1876                          				; strbot MSB < fstack MSB
  1877                          out_of_memory			
  1878  11e5 20550a             	JSR gexit
  1879  11e8 a210               	LDX #$10		; out of memory error
  1880  11ea 6c0003             	JMP (v_baserr)		; basic error handler
  1881  11ed d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  1882  11ef a5fd               	LDA fstack		; MSB equal, check LSB
  1883  11f1 c533               	CMP strbot
  1884  11f3 b0f0               	BCS out_of_memory	; fstack collides with string heap!
  1885                          
  1886                          fc_put
  1887  11f5 98                 	TYA			; y-line (value 0-7) merged with
  1888  11f6 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  1889  11f8 a000               	LDY #0			; stack structure index, on next free element
  1890  11fa 91fd               	STA (fstack),y
  1891  11fc c8                 	INY
  1892  11fd a5fc               	LDA caddr+1
  1893  11ff 91fd               	STA (fstack),y		; graphic address high
  1894  1201 c8                 	INY
  1895  1202 a5a3               	LDA tmpmask
  1896  1204 91fd               	STA (fstack),y
  1897  1206 c8                 	INY
  1898  1207 a5a7               	LDA x8			; 8x8 block position
  1899  1209 91fd               	STA (fstack),y
  1900                          	
  1901  120b 60                 fc_exit	RTS
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
  1926  120c a2ff               	LDX #$ff
  1927  120e c900               	CMP #0		; special case (no bit set at all)
  1928  1210 f004               	BEQ +
  1929  1212 e8                 -	INX
  1930  1213 0a                 	ASL		; shift to left
  1931  1214 d0fc               	BNE -		; until byte is empty
  1932  1216 e8                 +	INX
  1933  1217 60                 	RTS
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
  1956  1218 a208               	LDX #8
  1957  121a c900               	CMP #0		; special case (no bit set at all)
  1958  121c f004               	BEQ +
  1959  121e ca                 -	DEX
  1960  121f 4a                 	LSR		; shift to right
  1961  1220 d0fc               	BNE -		; until byte is empty
  1962  1222 60                 +	RTS
  1963                          
  1964                          ;-----------------------------------------------------------------
  1965                          
  1966                          unnew
  1967                          
  1968  1223 a52b               	LDA bassta
  1969  1225 8522               	STA str
  1970  1227 a52c               	LDA bassta+1
  1971  1229 8523               	STA str+1
  1972  122b a001               	LDY #1
  1973  122d 98                 	TYA
  1974  122e 9122               	STA (str),y		; != 0
  1975                          
  1976  1230 2033a5             	JSR b_rechain		; starting from bassta
  1977                          				; result in (str)
  1978  1233 18                 	CLC			; str+1 -> new basic end
  1979  1234 a423               	LDY str+1
  1980  1236 a522               	LDA str
  1981  1238 6902               	ADC #2
  1982  123a 852d               	STA basend
  1983  123c 9001               	BCC +
  1984  123e c8                 	INY
  1985  123f 842e               +	STY basend+1
  1986  1241 4c60a6             	JMP b_clr		; perform CLR
  1987                          
  1988                          
  1989                          ;-----------------------------------------------------------------
  1990                          graext_end

; ******** Source: ge-run.asm
    45                          
    46                          
