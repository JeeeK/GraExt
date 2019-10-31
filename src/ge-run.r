
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
    31  0822 a2bb               	ldx #<graext_end	; setup basic
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
   202  083e ad0803                     LDA v_bascmd		; check if hooks are already 
   203  0841 ae0903                     LDX v_bascmd+1		; in place 
   204  0844 c9c9               	CMP #<(parse)
   205  0846 d004               	BNE +
   206  0848 e008               	CPX #>(parse)
   207  084a f052               	BEQ ++			; already hooked
   208                          
   209  084c 8d3403             +       STA savevpars		; save old vector
   210  084f 8e3503             	STX savevpars+1
   211  0852 a9c9               	LDA #<(parse)		; basic interpreter parser hook
   212  0854 8d0803                     STA v_bascmd		; for commands
   213  0857 a908                       LDA #>(parse)
   214  0859 8d0903                     STA v_bascmd+1
   215                          
   216  085c ad0a03                     LDA v_basexp		; basic interpreter parser hook
   217  085f 8d3a03             	STA savevexp		; for expressions
   218  0862 a9fd                       LDA #<(express)		; with save of old pointer
   219  0864 8d0a03                     STA v_basexp
   220  0867 ad0b03                     LDA v_basexp+1
   221  086a 8d3b03             	STA savevexp+1
   222  086d a908                       LDA #>(express)
   223  086f 8d0b03                     STA v_basexp+1
   224                          
   225  0872 ad2803                     LDA v_basstp
   226  0875 8d3803             	STA savevstp
   227  0878 a9b4                       LDA #<(stop)		; basic interpreter stop hook
   228  087a 8d2803                     STA v_basstp
   229  087d ad2903                     LDA v_basstp+1
   230  0880 8d3903             	STA savevstp+1
   231  0883 a908                       LDA #>(stop)
   232  0885 8d2903                     STA v_basstp+1
   233                          
   234  0888 ad0003                     LDA v_baserr
   235  088b 8d3603             	STA saveverr
   236  088e a9ae                       LDA #<(error)		; basic interpreter error hook
   237  0890 8d0003                     STA v_baserr
   238  0893 ad0103                     LDA v_baserr+1
   239  0896 8d3703             	STA saveverr+1
   240  0899 a908                       LDA #>(error)
   241  089b 8d0103                     STA v_baserr+1
   242                          
   243  089e a200               ++	LDX #0			; set graphic cursor to (0,0)
   244  08a0 8e3c03             	STX savexl
   245  08a3 8e3d03             	STX savexh
   246  08a6 8e3e03             	STX savey
   247  08a9 e8                 	INX
   248  08aa 8e3f03             	STX savemo		; set mode 1
   249  08ad 60                         RTS
   250                          
   251                          error	
   252                          	; reg A may destroyed
   253  08ae 20c309             	JSR gra_off		; uses only reg A
   254  08b1 6c3603             	JMP (saveverr)		; to original vector
   255                          
   256                          stop	
   257                          	; reg A may destroyed
   258  08b4 a591               	LDA $91			; Scan code
   259  08b6 c97f               	CMP #$7F		; STOP key?
   260  08b8 d003               	BNE nostop
   261  08ba 20c309             	JSR gra_off		; uses only reg A
   262                          nostop
   263  08bd 6c3803             	JMP (savevstp)		; to original vector
   264                          
   265                          
   266                          ;-----------------------------------------------------------------
   267                          
   268                          ; undo chrget
   269                          
   270                          undo_chrget
   271  08c0 a57a               	LDA txtptr		; decrement text pointer by 1
   272  08c2 d002               	BNE +
   273  08c4 c67b               	DEC txtptr+1
   274  08c6 c67a               +	DEC txtptr
   275  08c8 60                 	RTS
   276                          
   277                          ;-----------------------------------------------------------------
   278                          
   279                          ; start parsing an extension command ...
   280                          
   281                          parse
   282  08c9 207300                     JSR chrget		; next char.
   283  08cc c926                       CMP #'&'		; command prefix
   284  08ce f006                       BEQ newcmd
   285  08d0 20c008             	JSR undo_chrget
   286  08d3 6c3403             	JMP (savevpars)
   287                          newcmd
   288  08d6 207300                     JSR chrget		; command character
   289                          
   290  08d9 a00e                       LDY #(cmdsend-cmds)	; map character to
   291                          				; command address ...
   292                          checknextcmd
   293  08db 88                         DEY
   294  08dc f01c               	BEQ parse_error
   295  08de d92b09                     CMP cmds,Y
   296  08e1 d0f8                       BNE checknextcmd	; try next
   297  08e3 88                         DEY			; found
   298  08e4 98                         TYA
   299  08e5 0a                         ASL			; *2
   300  08e6 a8                         TAY
   301                          !ifndef command_rts_tyle {
   302                          	!set co=0		; command offset in jump table
   303  08e7 b93a09                     LDA cmdaddr+1,Y		; high byte from table
   304  08ea 8556                       STA ijmp+1
   305  08ec b93909                     LDA cmdaddr,Y		; low byte from table
   306  08ef 8555                       STA ijmp
   307  08f1 207300                     JSR chrget		; read next byte in basic text
   308  08f4 205400                     JSR ijmp-1		; go to command by JMP (addr)
   309  08f7 4caea7                     JMP b_interpreter	; continue parsing
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
   324  08fa 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
   325                          
   326                          ;-----------------------------------------------------------------
   327                                  ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
   328                          express
   329  08fd a900               	LDA #0
   330  08ff 850d               	STA type	
   331  0901 207300             	JSR chrget
   332  0904 b003               	BCS exp_nonumber
   333  0906 4cf3bc             	JMP b_str2fac
   334                          exp_nonumber
   335  0909 c926                       CMP #'&'		; command prefix
   336  090b f006                       BEQ newfunc
   337  090d 20c008             	JSR undo_chrget
   338  0910 6c3a03             	JMP (savevexp)		; original routine	
   339                          ;	JMP b_execexpr
   340                          newfunc
   341  0913 207300             	JSR chrget
   342  0916 c95a               	CMP #'Z'
   343  0918 d003               	BNE +
   344  091a 4cdf0e             	JMP get
   345  091d c958               +	CMP #'X'
   346  091f d003               	BNE +
   347  0921 4cca0e             	JMP getposx
   348  0924 c959               +	CMP #'Y'
   349  0926 d0d2               	BNE parse_error
   350  0928 4cd60e             	JMP getposy
   351                          
   352                          ;-----------------------------------------------------------------
   353                          
   354                          ; the most commonly used command placed at the end ...
   355                          
   356  092b 2055464743534d42...cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
   357                          cmdsend
   358                          
   359                          cmdaddr
   360  0939 9a12a910bc098a0f...        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
   361  0945 6210160f4110f10c...        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co
   362                          
   363  0953 934752412d455854...author	!text 147,"GRA-EXT V"

; ******** Source: graext-core.asm, macro: version
     5                          
     6  095d 312e3331            !text "1.31" 

; ******** Source: graext-core.asm
   365  0961 20313938362c3230...	!text " 1986,2019 JOHANN@KLASEK.AT",0
   366                          
   367                          bitmask
   368  097d 8040201008040201   	!byte $80, $40, $20, $10, $08, $04, $02, $01
   369                          nbitmask
   370  0985 7fbfdfeff7fbfdfe   	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
   371                          ytabl
   372  098d 004080c0           	!byte $00,$40,$80,$c0
   373                          ytabh
   374  0991 e0e1e2e3           	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
   375  0995 e5e6e7e8           	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
   376  0999 eaebeced           	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
   377  099d eff0f1f2           	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
   378  09a1 f4f5f6f7           	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
   379  09a5 f9fafbfc           	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
   380  09a9 fe                 	!byte gramp+$1e
   381                          
   382                          ; for horiz. line
   383                          
   384                          maskleft0
   385                          maskleft
   386  09aa ff7f3f1f0f070301   	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
   387  09b2 00                 	!byte $00
   388                          
   389                          maskright0
   390  09b3 00                 	!byte $00
   391                          maskright
   392  09b4 80c0e0f0f8fcfeff   	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
   393                          
   394                          ;-----------------------------------------------------------------
   395                          
   396                          graphic
   397  09bc 209eb7                     JSR b_get8bit
   398  09bf e000                       CPX #$00
   399  09c1 d013                       BNE gra_other
   400                          gra0				; &G 0
   401                          gra_off
   402  09c3 a9c7                       LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
   403  09c5 8d00dd                     STA cia_pra
   404  09c8 a915                       LDA #((1 <<4) + (2 <<1) + 1)
   405                          				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
   406                          				; char addr $1000/4096 = char. ROM
   407  09ca 8d18d0                     STA vic_mcr		; VIC memory control
   408  09cd ad11d0                     LDA vic_cr		; VIC control register
   409  09d0 29df                       AND #%11011111		; Hires mode off
   410  09d2 8d11d0                     STA vic_cr
   411  09d5 60                         RTS
   412                          
   413                          gra_other
   414  09d6 e001                       CPX #$01
   415  09d8 f00f               	BEQ gra1
   416  09da e002               	CPX #$02
   417  09dc f00e                       BEQ gra2
   418  09de e004               	CPX #$04
   419  09e0 f043                       BEQ gra_clear		; &G 4 (erase only, leave mode)
   420  09e2 e003               	CPX #$03		; &G 3 (graphic on)
   421  09e4 f029               	BEQ gra_on
   422  09e6 4c48b2                     JMP b_illquant		; parameter illegal
   423                          	
   424                          gra1				; &G 1
   425  09e9 20250a             	JSR gra_clear
   426                          
   427                          gra2
   428  09ec 20f1b7                     JSR b_getcomma8bit
   429  09ef 8a                         TXA			; foreground color
   430  09f0 0a                         ASL			; upper nibble
   431  09f1 0a                         ASL
   432  09f2 0a                         ASL
   433  09f3 0a                         ASL
   434  09f4 85fd                       STA gcol
   435  09f6 20f1b7                     JSR b_getcomma8bit
   436  09f9 8a                         TXA			; background color
   437  09fa 290f                       AND #$0F
   438  09fc 05fd                       ORA gcol
   439  09fe a000                       LDY #$00
   440                          cram_loop
   441  0a00 9900cc                     STA cram,Y		; fill color RAM
   442  0a03 9900cd                     STA cram+$100,Y
   443  0a06 9900ce                     STA cram+$200,Y
   444  0a09 99e8ce                     STA cram+$300-24,Y
   445  0a0c c8                         INY
   446  0a0d d0f1                       BNE cram_loop
   447                          
   448                          gra_on
   449  0a0f 20440a             	JSR gra_setupcode
   450                          
   451  0a12 a9c4                       LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
   452  0a14 8d00dd                     STA cia_pra
   453  0a17 a938                       LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
   454  0a19 8d18d0                     STA vic_mcr		; VIC memory control
   455  0a1c ad11d0                     LDA vic_cr		; VIC control register
   456  0a1f 0920                       ORA #%00100000		; Bit 5 = 1: Hires on
   457  0a21 8d11d0                     STA vic_cr
   458  0a24 60                         RTS
   459                          
   460                          gra_clear
   461  0a25 a220                       LDX #$20		; Pages (8 KByte)
   462  0a27 a9e0                       LDA #>gram
   463  0a29 85fc                       STA gpos+1
   464  0a2b a000                       LDY #$00
   465  0a2d 84fb                       STY gpos
   466  0a2f 98                         TYA
   467                          gra_fill
   468  0a30 91fb                       STA (gpos),Y		; Loop unroll
   469  0a32 c8                         INY
   470  0a33 91fb                       STA (gpos),Y
   471  0a35 c8                         INY
   472  0a36 91fb                       STA (gpos),Y
   473  0a38 c8                         INY
   474  0a39 91fb                       STA (gpos),Y
   475  0a3b c8                         INY
   476  0a3c d0f2                       BNE gra_fill
   477  0a3e e6fc                       INC gpos+1
   478  0a40 ca                         DEX
   479  0a41 d0ed                       BNE gra_fill
   480  0a43 60                 	RTS
   481                          
   482                          gra_setupcode
   483  0a44 a20f               	LDX #(gromcode_end-gromcode) ; count of bytes
   484                          gra_copycode
   485  0a46 bd670a             	LDA gromcode-1,X
   486  0a49 9dec03             	STA gramcode-1,X
   487  0a4c ca                 	DEX
   488  0a4d d0f7               	BNE gra_copycode
   489  0a4f ad3f03             	LDA savemo
   490  0a52 290f               	AND #$0F
   491  0a54 aa                 	TAX
   492  0a55 4c760e             	JMP setmode_enter	; re-apply mode to routines
   493                          				; implicit RTS
   494                          
   495                          ;-----------------------------------------------------------------
   496                          
   497                          gexit
   498  0a58 a501                       LDA prozport
   499  0a5a 0902                       ORA #%00000010		; kernal ROM enable
   500  0a5c 8501                       STA prozport
   501  0a5e 58                         CLI			; allow interrupts
   502  0a5f 60                         RTS
   503                          
   504                          ;-----------------------------------------------------------------
   505                          
   506                          ginit
   507  0a60 a501                       LDA prozport
   508  0a62 29fd                       AND #%11111101		; Kernal ROM disable
   509  0a64 78                         SEI			; disable interrupts
   510  0a65 8501                       STA prozport
   511  0a67 60                         RTS
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
   531  0a68 b1a5                       LDA (gaddr),Y
   532                          gchange_op
   533  0a6a 1d7d09                     ORA bitmask,X
   534  0a6d 91a5                       STA (gaddr),Y
   535                          !ifdef ltc {
   536                          	LDA #mc_sim		; vollständige ROM-Simulation
   537                          	STA memconf		; wieder schnelles RAM ab $C000
   538                          }
   539  0a6f 60                         RTS
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
   551  0a70 4900                       EOR #$00
   552                          gmask_op
   553  0a72 11a5                       ORA (gaddr),Y
   554  0a74 91a5                       STA (gaddr),Y
   555                          !ifdef ltc {
   556                          	LDA #mc_sim		; vollständige ROM-Simulation
   557                          	STA memconf		; wieder schnelles RAM ab $C000
   558                          }
   559  0a76 60                         RTS
   560                          
   561                          }
   562                          
   563                          gromcode_end
   564                          
   565                          ;-----------------------------------------------------------------
   566                          
   567                          position
   568  0a77 a5aa                       LDA y
   569  0a79 4a                         LSR
   570  0a7a 4a                         LSR
   571  0a7b 4a                         LSR			; y/8
   572  0a7c a8                         TAY
   573  0a7d 2903                       AND #%00000011		; (y/8) mod 4
   574  0a7f aa                         TAX
   575  0a80 a59b                       LDA xl			; x low
   576  0a82 29f8                       AND #%11111000		; clear bit 2-0
   577  0a84 18                         CLC
   578  0a85 7d8d09                     ADC ytabl,X		; addr low: y base + x part
   579  0a88 85a5                       STA gaddr
   580  0a8a a59c                       LDA xh			; addr high: x part
   581  0a8c 799109                     ADC ytabh,Y		; 	+ y base
   582  0a8f 85a6                       STA gaddr+1
   583  0a91 a5aa                       LDA y			; vertical offset
   584  0a93 2907                       AND #%00000111		; y mod 8
   585  0a95 a8                         TAY
   586  0a96 a59b                       LDA xl
   587  0a98 2907                       AND #%00000111		; x mod 8
   588  0a9a aa                         TAX			; horizonal offset
   589  0a9b 60                         RTS			; (bitmask)
   590                          
   591                          
   592                          ;-----------------------------------------------------------------
   593                          
   594                          ; swap tupel xl,xh <-> xendl,xendh
   595                          
   596                          swap_x_xend
   597  0a9c a69e                       LDX xendl		; swap x, xend
   598  0a9e a49b                       LDY xl
   599  0aa0 869b                       STX xl
   600  0aa2 849e                       STY xendl
   601                          
   602  0aa4 a69f                       LDX xendh
   603  0aa6 a49c                       LDY xh
   604  0aa8 849f                       STY xendh
   605  0aaa 869c                       STX xh
   606  0aac 60                 	RTS
   607                          
   608                          
   609                          ;-----------------------------------------------------------------
   610                          
   611                          ; line y up, x left, dx < dy (case 1)
   612                          
   613                          line_up_steep
   614  0aad 20770a                     JSR position		; x,y
   615                          loop_yup_xleft
   616  0ab0 20ed03                     JSR gchange		; pixel
   617                          
   618  0ab3 18                         CLC			; k += dx
   619  0ab4 a595                       LDA kl
   620  0ab6 65ab                       ADC dxl			; dxh is 0, because dx < dy
   621  0ab8 8595                       STA kl
   622  0aba 9014                       BCC +			; k >= 0 ->
   623                          
   624  0abc e5a9               ++	SBC dy			; k -= dy (C=1)
   625  0abe 8595                       STA kl
   626                          
   627  0ac0 ca                  	DEX			; x--
   628  0ac1 100d                       BPL +
   629  0ac3 a207                       LDX #7			; wrap around
   630  0ac5 38                 	SEC
   631  0ac6 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   632  0ac8 e908                       SBC #8
   633  0aca 85a5                       STA gaddr
   634  0acc b002                       BCS +
   635  0ace c6a6                       DEC gaddr+1
   636                          
   637  0ad0 88                 +	DEY			; y--
   638  0ad1 100f                       BPL +++
   639  0ad3 38                         SEC			; y overflow
   640  0ad4 a5a5                       LDA gaddr
   641  0ad6 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   642  0ad8 85a5                       STA gaddr
   643  0ada a5a6                       LDA gaddr+1
   644  0adc e901               	SBC #1
   645  0ade 85a6                       STA gaddr+1
   646  0ae0 a007                       LDY #7			; wrap around
   647                          
   648  0ae2 c6a3               +++	DEC cl			; until c=0
   649  0ae4 d0ca                       BNE loop_yup_xleft
   650  0ae6 4c580a                     JMP gexit
   651                          
   652                          
   653                          ;-----------------------------------------------------------------
   654                          
   655                          ; line x left, y up, dx > dy (case 2)
   656                          
   657                          line_up_flat
   658  0ae9 20770a                     JSR position		; x,y
   659  0aec a5a3               	LDA cl			; counter adjustment for
   660  0aee f002               	BEQ +			; prepare for dec-dec-counting
   661  0af0 e6a4               	INC ch
   662                          +
   663                          loop_xleft_yup
   664  0af2 20ed03                     JSR gchange		; pixel
   665                          
   666  0af5 18                         CLC			; k += dy
   667  0af6 a595                       LDA kl
   668  0af8 65a9                       ADC dy
   669  0afa 8595                       STA kl
   670  0afc 9020                       BCC +			; k < 0
   671  0afe e696                       INC kh
   672  0b00 301c               	BMI +			; k < 0
   673                          
   674  0b02 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   675  0b04 8595                       STA kl
   676  0b06 a596                       LDA kh
   677  0b08 e5a7                       SBC dxh		
   678  0b0a 8596                       STA kh
   679                          
   680  0b0c 88                         DEY			; y--
   681  0b0d 100f                       BPL +
   682  0b0f 38                 	SEC			; C=1 not always true (SBC above)
   683  0b10 a5a5                       LDA gaddr		; y overflow
   684  0b12 e940                       SBC #$40		; y-8: gaddr -= 40*8 ($140)
   685  0b14 85a5                       STA gaddr
   686  0b16 a5a6                       LDA gaddr+1
   687  0b18 e901               	SBC #1
   688  0b1a 85a6                       STA gaddr+1
   689  0b1c a007               	LDY #7			; wrap around
   690                          
   691  0b1e ca                 +	DEX			; x--
   692  0b1f 100d                       BPL +++
   693  0b21 a207                       LDX #7			; wrap around
   694  0b23 38                 	SEC
   695  0b24 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   696  0b26 e908                       SBC #8
   697  0b28 85a5                       STA gaddr
   698  0b2a b002                       BCS +++
   699  0b2c c6a6                       DEC gaddr+1
   700                          +++
   701  0b2e c6a3               	DEC cl			; c--
   702  0b30 d0c0                       BNE loop_xleft_yup
   703  0b32 c6a4                       DEC ch			; adjusted high which allows this
   704  0b34 d0bc                       BNE loop_xleft_yup
   705                          
   706  0b36 4c580a                     JMP gexit
   707                          
   708                          
   709                          
   710                          ;-----------------------------------------------------------------
   711                          
   712                          ; line x left, y down, dx > dy (case 3)
   713                          
   714                          line_down_flat
   715  0b39 20770a                     JSR position		; x,y
   716  0b3c a5a3               	LDA cl			; counter adjustment for
   717  0b3e f002               	BEQ +			; prepare for dec-dec-counting
   718  0b40 e6a4               	INC ch
   719                          +
   720                          loop_xleft_ydown
   721  0b42 20ed03                     JSR gchange		; pixel
   722                          
   723  0b45 18                         CLC			; k += dy
   724  0b46 a595                       LDA kl
   725  0b48 65a9                       ADC dy
   726  0b4a 8595                       STA kl
   727  0b4c 9021                       BCC +			; k < 0
   728  0b4e e696                       INC kh
   729  0b50 301d               	BMI +			; k < 0
   730                          
   731  0b52 e5ab                       SBC dxl			; k -= dx (A = kl, C=1)
   732  0b54 8595                       STA kl
   733  0b56 a596                       LDA kh
   734  0b58 e5a7                       SBC dxh		
   735  0b5a 8596                       STA kh
   736                          
   737  0b5c c8                         INY			; y++
   738  0b5d c008                       CPY #8
   739  0b5f d00e                       BNE +
   740                          	; C=1
   741  0b61 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   742  0b63 693f                       ADC #$40-1		; C already set by CPY
   743  0b65 85a5                       STA gaddr
   744  0b67 a5a6                       LDA gaddr+1
   745  0b69 6901               	ADC #1
   746  0b6b 85a6                       STA gaddr+1
   747  0b6d a000                       LDY #0			; wrap around
   748                          
   749  0b6f ca                 +	DEX			; x--
   750  0b70 100d                       BPL +++
   751  0b72 a207                       LDX #7			; wrap around
   752  0b74 38                 	SEC
   753  0b75 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   754  0b77 e908                       SBC #8
   755  0b79 85a5                       STA gaddr
   756  0b7b b002                       BCS +++
   757  0b7d c6a6                       DEC gaddr+1
   758                          +++
   759  0b7f c6a3               	DEC cl			; c--
   760  0b81 d0bf               	BNE loop_xleft_ydown
   761  0b83 c6a4               	DEC ch			; adjusted high which allows this
   762  0b85 d0bb                       BNE loop_xleft_ydown
   763                          
   764  0b87 4c580a                     JMP gexit
   765                          
   766                          
   767                          ;-----------------------------------------------------------------
   768                          
   769                          ; line y down, x right, dx < dy (case 4)
   770                          
   771                          line_down_steep
   772  0b8a 20770a                     JSR position		; x,y
   773                          loop_ydown_xleft
   774  0b8d 20ed03                     JSR gchange		; pixel
   775                          
   776  0b90 18                         CLC			; k += dx
   777  0b91 a595                       LDA kl
   778  0b93 65ab                       ADC dxl			; dxh is 0, because dx < dy
   779  0b95 8595                       STA kl
   780  0b97 9014                       BCC +			; k >= 0 ->
   781                          
   782  0b99 e5a9               	SBC dy			; k -= dy, C=1
   783  0b9b 8595                       STA kl
   784                          
   785  0b9d ca                  	DEX			; x--
   786  0b9e 100d                       BPL +
   787  0ba0 a207                       LDX #7			; wrap around
   788  0ba2 38                 	SEC
   789  0ba3 a5a5                       LDA gaddr		; x-8: gaddr -= 8
   790  0ba5 e908                       SBC #8
   791  0ba7 85a5                       STA gaddr
   792  0ba9 b002                       BCS +
   793  0bab c6a6                       DEC gaddr+1
   794                          
   795  0bad c8                 +	INY			; y++
   796  0bae c008                       CPY #8			; y overflow?
   797  0bb0 d00e                       BNE +++
   798  0bb2 a5a5                       LDA gaddr		; y+8: gaddr += 40*8 ($140)
   799  0bb4 693f                       ADC #$40-1		; C already set by CPY
   800  0bb6 85a5                       STA gaddr
   801  0bb8 a5a6                       LDA gaddr+1
   802  0bba 6901               	ADC #1
   803  0bbc 85a6                       STA gaddr+1
   804  0bbe a000                       LDY #0			; wrap around
   805                          
   806  0bc0 c6a3               +++	DEC cl			; c--
   807                          				; until c=0
   808  0bc2 d0c9                       BNE loop_ydown_xleft
   809  0bc4 4c580a                     JMP gexit
   810                          
   811                          
   812                          ;-----------------------------------------------------------------
   813                          
   814                          getcommaxy
   815  0bc7 20fdae                     JSR b_getcomma		; check ","
   816                          getxy
   817  0bca 208aad                     JSR b_getval		; get X coord. value
   818  0bcd 20f7b7                     JSR b_convint
   819  0bd0 c901                       CMP #>xmax
   820  0bd2 900c               	BCC gcxy_xok
   821  0bd4 f003                       BEQ ++			; X = $1xx
   822  0bd6 20390e                     JSR range_error
   823                          
   824  0bd9 c040               ++	CPY #<xmax		; check X low
   825  0bdb 9003                       BCC +
   826  0bdd 20390e                     JSR range_error
   827                          +
   828                          gcxy_xok
   829  0be0 84fb                       STY gpos		; temporary save X coord.
   830  0be2 85fc                       STA gpos+1
   831                          
   832  0be4 20f1b7                     JSR b_getcomma8bit
   833                          				; get Y coord. value
   834  0be7 e0c8                       CPX #ymax
   835  0be9 9003                       BCC +
   836  0beb 20390e                     JSR range_error
   837                          +
   838  0bee a4fb                       LDY gpos		; restory X coord.
   839  0bf0 a5fc                       LDA gpos+1
   840  0bf2 60                         RTS
   841                          
   842                          
   843                          ;-----------------------------------------------------------------
   844                          
   845                          para_hline_box
   846  0bf3 20ca0b                     JSR getxy		; get startpoint
   847  0bf6 86aa                       STX y
   848  0bf8 8e3e03                     STX savey		; save as cursor, too
   849  0bfb 859c                       STA xh
   850  0bfd 849b                       STY xl
   851  0bff 8d3d03             	STA savexh
   852  0c02 8c3c03             	STY savexl
   853  0c05 20fdae                     JSR b_getcomma		; get length
   854  0c08 208aad                     JSR b_getval
   855  0c0b 20f7b7                     JSR b_convint
   856                          				; calculate end point
   857  0c0e aa                         TAX			; save length high byte
   858  0c0f 98                         TYA			; length low byte
   859  0c10 18                         CLC
   860  0c11 659b                       ADC xl			; low xend = x+length
   861  0c13 859e                       STA xendl
   862  0c15 a8                 	TAY
   863  0c16 8a                         TXA			; high
   864  0c17 659c                       ADC xh			; high xend = x+length
   865  0c19 859f                       STA xendh
   866  0c1b aa                 	TAX
   867                          
   868  0c1c c901               	CMP #>xmax		; endpoint outside?
   869  0c1e 9005               	BCC +
   870  0c20 d003               	BNE +			; >$200 (512)
   871  0c22 98                 	TYA
   872  0c23 e940               	SBC #<xmax
   873  0c25 60                 +	RTS			; C=1 out of range, C=0 ok
   874                          
   875                          ;-----------------------------------------------------------------
   876                          
   877                          hline
   878  0c26 20f30b             	JSR para_hline_box
   879  0c29 9003               	BCC +
   880  0c2b 20390e             	JSR range_error
   881                          				; XXX xend=xmax-1 ?
   882                          +
   883  0c2e 8e3d03                     STX savexh
   884  0c31 8c3c03                     STY savexl		; also save as final cursor
   885                          
   886  0c34 a900               	LDA #0			; default thickness 0 (means 1 pixel)
   887  0c36 85a3               	STA ycount
   888  0c38 207900             	JSR chrgot		; last char. again
   889  0c3b f019               	BEQ +++			; command end? no optional param.
   890  0c3d 20f1b7             	JSR b_getcomma8bit
   891  0c40 8a                 	TXA			; optional 8-bit parameter
   892  0c41 85a3               	STA ycount		; hline thickness
   893  0c43 f011               	BEQ +++			; 0 means 1 pixel
   894  0c45 18                 	CLC
   895  0c46 65aa               	ADC y			; end position for y coord.
   896  0c48 b004               	BCS +			; > 255
   897  0c4a c9c8               	CMP #ymax
   898  0c4c 9008               	BCC +++
   899                          +				; C=1 from ADC or CMP before
   900  0c4e 20390e             	JSR range_error		; corrupts A
   901                          				; XXX ycount=ymax-y-1 ?
   902                          				; xend >= x
   903  0c51 b003               	BCS hl_noxswap		; always
   904                          
   905                          hline_start
   906  0c53 209c0a             	JSR swap_x_xend		; xend < x, entry from line
   907                          	
   908                          hl_noxswap
   909                          				; xend > x
   910                          +++
   911  0c56 e6a3               	INC ycount		; count to 0
   912  0c58 20600a                     JSR ginit		; map in graphic memory
   913                          
   914  0c5b 20770a                     JSR position		; graphic position x,y
   915                          
   916  0c5e a5a5               	LDA gaddr		; save position for vertical
   917  0c60 85fb               	STA sgaddr
   918  0c62 a5a6               	LDA gaddr+1
   919  0c64 85fc               	STA sgaddr+1
   920  0c66 86ab               	STX xsave
   921  0c68 84a9               	STY ysave
   922                          
   923  0c6a a59e                       LDA xendl
   924  0c6c 2907                       AND #%00000111
   925  0c6e 8596                       STA tmp2		; xend mod 8, mask index
   926  0c70 a59b                       LDA xl
   927  0c72 29f8                       AND #%11111000		; (xl div 8)*8
   928  0c74 8595                       STA tmp1
   929  0c76 a59e                       LDA xendl		; xend unmasked
   930  0c78 38                         SEC
   931  0c79 e595                       SBC tmp1		; finally: xend - (x div 8)*8 
   932  0c7b 8595                       STA tmp1
   933  0c7d a59f                       LDA xendh
   934  0c7f e59c                       SBC xh
   935  0c81 4a                         LSR			; / 8 ->  0-39
   936  0c82 a595                       LDA tmp1		; only 1 highest bit
   937  0c84 6a                         ROR			; and 3 lower bits
   938  0c85 4a                         LSR
   939  0c86 4a                         LSR
   940                                  			; 8-pixel-blocks count
   941  0c87 85a4               	STA hcount		; save for vertical extension
   942                           
   943                          hl_vertloop
   944  0c89 98                 	TYA			; calculate max. Y in 8x8 block
   945  0c8a 18                 	CLC
   946  0c8b 65a3               	ADC ycount
   947  0c8d c908               	CMP #8
   948  0c8f 9002               	BCC +
   949  0c91 a908               	LDA #8
   950  0c93 85a8               +	STA ylimit
   951                          
   952  0c95 bdaa09                     LDA maskleft,X		; starting mask
   953  0c98 8595               	STA tmp1
   954  0c9a a6a4               	LDX hcount		; how many blocks
   955                          
   956                          hl_nextblock
   957  0c9c ca                         DEX
   958                          hl_islastblock
   959  0c9d 301d                       BMI hl_lastblock
   960                          				; leave loop if X<0
   961  0c9f a4a9               	LDY ysave
   962  0ca1 a595               -	LDA tmp1		; mask
   963  0ca3 20f503             	JSR gmask		; first with left end mask
   964  0ca6 c8                 	INY			; vertical down
   965  0ca7 c4a8               	CPY ylimit		; in 8x8 box
   966  0ca9 d0f6               	BNE -
   967                          
   968  0cab 18                         CLC			; gaddr += 8 (one block to right)
   969  0cac a5a5                       LDA gaddr
   970  0cae 6908                       ADC #8
   971  0cb0 85a5                       STA gaddr
   972  0cb2 9002                       BCC +
   973  0cb4 e6a6                       INC gaddr+1
   974                          
   975  0cb6 a9ff               +	LDA #$FF		; following with full 8-pixel mask
   976  0cb8 8595               	STA tmp1
   977  0cba d0e0               	BNE hl_nextblock	; always
   978                          
   979                          hl_lastblock
   980  0cbc a696                       LDX tmp2		; xend mask index
   981  0cbe 3db409                     AND maskright,X		; A has current maskt combine with mask right end
   982  0cc1 8595               	STA tmp1		; mask
   983  0cc3 a4a9               	LDY ysave		; start position in 8x8 block
   984  0cc5 a595               -	LDA tmp1		; mask
   985  0cc7 20f503             	JSR gmask		; modify
   986  0cca c8                 	INY			; vertical down
   987  0ccb c6a3               	DEC ycount		; overall y counter
   988  0ccd c4a8               	CPY ylimit
   989  0ccf d0f4               	BNE -
   990                          
   991  0cd1 a5a3               	LDA ycount		; finished
   992  0cd3 d003               	BNE +			; roll-over into 8x8 block below
   993  0cd5 4c580a                     JMP gexit		; leave
   994                          
   995  0cd8 18                 +	CLC
   996  0cd9 a5fb               	LDA sgaddr
   997  0cdb 6940               	ADC #$40		; next 8-pixel row below
   998  0cdd 85fb               	STA sgaddr		; + $140 (320)
   999  0cdf 85a5               	STA gaddr
  1000  0ce1 a5fc               	LDA sgaddr+1
  1001  0ce3 6901               	ADC #$01
  1002  0ce5 85fc               	STA sgaddr+1
  1003  0ce7 85a6               	STA gaddr+1
  1004  0ce9 a6ab               	LDX xsave		; initial mask index
  1005  0ceb a000               	LDY #0			; start on top of 8x8
  1006  0ced 84a9               	STY ysave
  1007  0cef f098               	BEQ hl_vertloop
  1008                          ;-----------------------------------------------------------------
  1009                          
  1010                          vline
  1011  0cf1 20ca0b                     JSR getxy		; get startpoint
  1012  0cf4 859c                       STA xh
  1013  0cf6 8d3d03                     STA savexh		; save as cursor too
  1014  0cf9 849b                       STY xl
  1015  0cfb 8c3c03                     STY savexl
  1016  0cfe 8693                       STX yend		; initial point is endpoint
  1017                          
  1018  0d00 20f1b7                     JSR b_getcomma8bit
  1019                          				; get length
  1020  0d03 18                         CLC			; calculate end point
  1021  0d04 8a                         TXA			; length
  1022                          ; DON'T-CHANGE: how long to go vertically (needed later)
  1023                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1024                          ;	STA tmp1
  1025  0d05 6593                       ADC yend		; length + initial point is startpoint
  1026  0d07 b005               	BCS vline_iq		; > 255
  1027  0d09 c9c8                       CMP #ymax		; outside?
  1028  0d0b a8                 	TAY			; keep startpoint
  1029  0d0c 9003                       BCC +
  1030                          vline_iq
  1031  0d0e 20390e                     JSR range_error		; corrupts A
  1032                          				; XXX Y = ymax-1 ?
  1033  0d11 84aa               +	STY y			; startpoint
  1034  0d13 8c3e03             	STY savey		; set cursor y position
  1035  0d16 18                 	CLC
  1036  0d17 900e               	BCC +++			; skip following, because y, yend are already ordered
  1037                          
  1038                          vline_start			; entry point from line command (only)
  1039  0d19 a5aa               	LDA y			; order of y, yend is not defined
  1040  0d1b c593               	CMP yend
  1041  0d1d b008               	BCS vl_noyswap		; yend > y ->
  1042  0d1f a5aa               	LDA y			; swap y, yend
  1043  0d21 a693               	LDX yend
  1044  0d23 8593               	STA yend
  1045  0d25 86aa               	STX y
  1046                          vl_noyswap
  1047                          				; startpoint is below the endpoint
  1048  0d27 20600a             +++	JSR ginit		; map in graphic memory
  1049                          
  1050                          vl_start
  1051  0d2a 20770a                     JSR position		; graphic position x,y
  1052  0d2d bd7d09                     LDA bitmask,X
  1053  0d30 8596                       STA tmp2		; save mask
  1054                          ; DON'T-CHANGE: replace ...
  1055  0d32 38                         SEC
  1056  0d33 a5aa                       LDA y			; startpoint is greater!
  1057  0d35 e593                       SBC yend		; vertical length
  1058  0d37 aa                         TAX
  1059                          ; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
  1060                          ;		DO NOT USE: tmp1 does not exist if called via vline_start!
  1061                          ;	LDX tmp1
  1062  0d38 e8                         INX			; +1 (exit on 0)
  1063  0d39 38                 	SEC			; for subtraction, never changed!
  1064                          vl_nextline
  1065  0d3a a596                       LDA tmp2
  1066  0d3c 20f503                     JSR gmask		; modify 
  1067  0d3f 88                         DEY			; go up
  1068  0d40 100e                       BPL +
  1069  0d42 a5a5                       LDA gaddr		; C=1
  1070  0d44 e940               	SBC #$40		; gaddr -= 320
  1071  0d46 85a5                       STA gaddr
  1072  0d48 a5a6                       LDA gaddr+1
  1073  0d4a e901                       SBC #$01
  1074  0d4c 85a6                       STA gaddr+1
  1075  0d4e a007                       LDY #7			; wrap y offset
  1076  0d50 ca                 +	DEX			; all vertical positions done?
  1077  0d51 d0e7                       BNE vl_nextline
  1078  0d53 4c580a                     JMP gexit		; leave
  1079                          
  1080                          
  1081                          ;-----------------------------------------------------------------
  1082                          
  1083                          line
  1084  0d56 20ca0b                     JSR getxy		; get startpoint
  1085  0d59 849b                       STY xl 
  1086  0d5b 859c                       STA xh
  1087  0d5d 86aa                       STX y
  1088                          
  1089  0d5f 20c70b                     JSR getcommaxy		; get endpoint
  1090                          line_start
  1091  0d62 8c3c03                     STY savexl		; save as cursor position too
  1092  0d65 849e                       STY xendl
  1093  0d67 8d3d03                     STA savexh
  1094  0d6a 859f                       STA xendh
  1095  0d6c 8e3e03                     STX savey
  1096  0d6f 8693                       STX yend
  1097                          
  1098  0d71 a000                       LDY #$00		; initialize to 0
  1099  0d73 84a8                       STY ydir
  1100  0d75 8495                       STY kl
  1101  0d77 8496                       STY kh
  1102                          
  1103  0d79 38                         SEC
  1104  0d7a a59b                       LDA xl			; calculate dx
  1105  0d7c e59e                       SBC xendl
  1106  0d7e 85ab                       STA dxl
  1107  0d80 a59c                       LDA xh
  1108  0d82 e59f                       SBC xendh
  1109  0d84 85a7                       STA dxh
  1110                          
  1111  0d86 b018                       BCS li_xend_left
  1112                          	; dx != 0
  1113                          				; negate dx:
  1114  0d88 98                         TYA			; Y=A=0
  1115  0d89 38                         SEC			; dx = 0 - dx
  1116  0d8a e5ab                       SBC dxl
  1117  0d8c 85ab                       STA dxl
  1118  0d8e 98                         TYA			; Y=A=0
  1119  0d8f e5a7                       SBC dxh
  1120  0d91 85a7                       STA dxh
  1121                          				; C=0 always, needed later
  1122  0d93 209c0a             	jsr swap_x_xend
  1123  0d96 a6aa                       LDX y			; swap y
  1124  0d98 a493                       LDY yend
  1125  0d9a 8693                       STX yend
  1126  0d9c 84aa                       STY y
  1127                          
  1128  0d9e 9007                       BCC li_x_different
  1129                          				; C=0 always (from negation before)
  1130                          
  1131                          li_xend_left
  1132                                  			; A already contains dxh
  1133  0da0 05ab                       ORA dxl			; dx = 0?
  1134  0da2 d003                       BNE li_x_different
  1135  0da4 4c190d                     JMP vline_start		; vertical line case
  1136                          
  1137                          li_x_different
  1138  0da7 38                         SEC			; calculate dy
  1139  0da8 a593                       LDA yend
  1140  0daa e5aa                       SBC y
  1141  0dac b006                       BCS li_y_right		; yend >= y?
  1142  0dae 49ff                       EOR #$FF		; no, negate dy (two's complement)
  1143  0db0 6901                       ADC #$01		; C=0
  1144  0db2 85a8                       STA ydir		; always not 0: flag y goes up
  1145                          
  1146                          li_y_right
  1147  0db4 85a9                       STA dy
  1148  0db6 d007                       BNE +
  1149  0db8 a900               	LDA #0			; line thickness = 1
  1150  0dba 85a3               	STA ycount
  1151  0dbc 4c530c                     JMP hline_start		; horizontal line case
  1152                          +
  1153                          	; dx and dy is *always* !=0, otherwise hline or vline got called.
  1154                          
  1155  0dbf a5a7                       LDA dxh			; dx > dy
  1156  0dc1 d01c                       BNE line_flat		; yes -> flat
  1157  0dc3 a5a9                       LDA dy			; no -> steep
  1158  0dc5 aa                         TAX
  1159  0dc6 c5ab                       CMP dxl
  1160  0dc8 9015                       BCC line_flat
  1161                          
  1162                          line_steep
  1163  0dca e8                         INX	
  1164  0dcb 86a3                       STX cl			; c = dy+1
  1165  0dcd 4a                         LSR			; dy/2
  1166  0dce 49ff               	EOR #$FF		; one's complement
  1167  0dd0 8595                       STA kl			; k = -dy/2 -1
  1168                          
  1169  0dd2 20600a                     JSR ginit		; map in graphic memory
  1170                          
  1171  0dd5 a5a8                       LDA ydir
  1172  0dd7 d003                       BNE +
  1173  0dd9 4c8a0b                     JMP line_down_steep	; y down, steep
  1174  0ddc 4cad0a             +	JMP line_up_steep	; y up, steep
  1175                          
  1176                          line_flat
  1177  0ddf a5a7                       LDA dxh
  1178  0de1 a8                         TAY
  1179  0de2 a6ab                       LDX dxl
  1180  0de4 e8                         INX
  1181  0de5 d001                       BNE +
  1182  0de7 c8                         INY
  1183  0de8 86a3               +	STX cl			; c = dx+1
  1184  0dea 84a4                       STY ch
  1185                          
  1186  0dec 4a                         LSR			; dx/2 high
  1187  0ded 49ff               	EOR #$FF		; one's complement
  1188  0def 8596                       STA kh
  1189  0df1 a5ab                       LDA dxl
  1190  0df3 6a                         ROR			; dx/2 low
  1191  0df4 49ff               	EOR #$FF		; one's complement
  1192  0df6 8595                       STA kl			; k = -dx/2 - 1
  1193                          
  1194  0df8 20600a                     JSR ginit		; map in graphic memory
  1195                          
  1196  0dfb a5a8                       LDA ydir	
  1197  0dfd d003                       BNE +
  1198  0dff 4c390b                     JMP line_down_flat	; y down, flat
  1199  0e02 4ce90a             +	JMP line_up_flat	; y up, flat
  1200                          
  1201                          ;-----------------------------------------------------------------
  1202                          
  1203                          plot
  1204  0e05 20ca0b                     JSR getxy		; get parameter
  1205  0e08 859c                       STA xh			; save x/y
  1206  0e0a 849b                       STY xl
  1207  0e0c 86aa                       STX y
  1208  0e0e 8d3d03                     STA savexh		; and store as cursor
  1209  0e11 8c3c03                     STY savexl
  1210  0e14 8e3e03                     STX savey
  1211                          
  1212                          plot_start
  1213  0e17 20770a                     JSR position		; calculate graphical address
  1214                          
  1215  0e1a a501                       LDA prozport
  1216  0e1c 29fd                       AND #%11111101		; Kernal ROM disable
  1217  0e1e 78                         SEI			
  1218  0e1f 8501                       STA prozport
  1219                          
  1220  0e21 20ed03                     JSR gchange		; change graphical data
  1221                          
  1222  0e24 a501                       LDA prozport
  1223  0e26 0902                       ORA #%00000010		; kernal ROM enable
  1224  0e28 8501                       STA prozport
  1225  0e2a 58                         CLI
  1226  0e2b 60                         RTS
  1227                          
  1228                          ;-----------------------------------------------------------------
  1229                          
  1230                          move
  1231  0e2c 20ca0b                     JSR getxy		; get parameter
  1232  0e2f 8d3d03                     STA savexh		; just save as cursor
  1233  0e32 8c3c03                     STY savexl
  1234  0e35 8e3e03                     STX savey
  1235  0e38 60                         RTS
  1236                          
  1237                          
  1238                          ;-----------------------------------------------------------------
  1239                          
  1240                          ; never touches X, Y, C-flag
  1241                          ; on exit: A corrupted, Z=0
  1242                          
  1243                          range_error
  1244  0e39 ad3f03             	LDA savemo
  1245  0e3c 29f0               	AND #$F0
  1246  0e3e d003               	BNE +
  1247                          				; error mode 3: abort command (silent)
  1248  0e40 68                 	PLA			; cleanup JSR
  1249  0e41 68                 	PLA			; highbyte of return address >0
  1250                          
  1251  0e42 60                 -	RTS			; error mode 5: back to command
  1252                          				; to handle value correction
  1253                          				; Z=0
  1254  0e43 2920               +	AND #$20		; mode 5?
  1255  0e45 d0fb               	BNE -			; exit with Z=0
  1256  0e47 68                 	PLA			; error mode 4: terminate with error
  1257  0e48 68                 	PLA			; cleanup JSR
  1258                          setmode_error
  1259  0e49 4c48b2             	JMP b_illquant		; throw error message
  1260                          
  1261                          ;-----------------------------------------------------------------
  1262                          
  1263                          setmode
  1264  0e4c 209eb7                     JSR b_get8bit
  1265  0e4f e003                       CPX #3
  1266  0e51 9017                       BCC +			; less then 3, modification mode
  1267  0e53 e006               	CPX #6
  1268  0e55 b0f2               	BCS setmode_error	; out of range
  1269                          				; error mode
  1270  0e57 8a                 	TXA
  1271  0e58 e902               	SBC #2			; C=0, therefore -3
  1272  0e5a 0a                 	ASL			; 0-2 -> 16,32 or 48
  1273  0e5b 0a                 	ASL			; shift to upper nibble
  1274  0e5c 0a                 	ASL
  1275  0e5d 0a                 	ASL
  1276                          				; put A's bit 4-7 into savemo
  1277  0e5e 4d3f03             	EOR savemo		; ********
  1278  0e61 29f0               	AND #%11110000		; ****0000
  1279  0e63 4d3f03             	EOR savemo		; AAAAmmmm
  1280  0e66 8d3f03             	STA savemo		; 
  1281  0e69 60                 	RTS
  1282                          
  1283  0e6a 8a                 +	TXA
  1284  0e6b 4d3f03             	EOR savemo		; put A's bit 0-3 into savemo
  1285  0e6e 290f               	AND #%00001111
  1286  0e70 4d3f03             	EOR savemo
  1287  0e73 8d3f03             	STA savemo
  1288                          setmode_enter
  1289  0e76 e001               	CPX #$01
  1290  0e78 b01a                       BCS set_or_toggle
  1291                          
  1292                          modereset
  1293  0e7a a909                       LDA #>(nbitmask)
  1294  0e7c 8df103                     STA gchange_op+2
  1295  0e7f a985                       LDA #<(nbitmask)
  1296  0e81 8df003                     STA gchange_op+1
  1297  0e84 a93d                       LDA #$3D		; opcode AND abs,X
  1298  0e86 8def03                     STA gchange_op
  1299  0e89 a931                       LDA #$31		; opcode AND (zp),Y
  1300  0e8b 8df703                     STA gmask_op
  1301  0e8e a9ff                       LDA #$FF		; mask, EOR $#FF, inverting
  1302  0e90 8df603                     STA gmask_flip+1
  1303  0e93 60                         RTS
  1304                          
  1305                          set_or_toggle
  1306  0e94 d01a                       BNE modetoggle
  1307                          modeset
  1308  0e96 a909                       LDA #>(bitmask)
  1309  0e98 8df103                     STA gchange_op+2
  1310  0e9b a97d                       LDA #<(bitmask)
  1311  0e9d 8df003                     STA gchange_op+1
  1312  0ea0 a91d                       LDA #$1D		; opcode OR abs,X
  1313  0ea2 8def03                     STA gchange_op
  1314  0ea5 a911                       LDA #$11		; opcode OR (zp),Y
  1315  0ea7 8df703                     STA gmask_op
  1316  0eaa a900                       LDA #$00		; mask, EOR #$00, not inverting
  1317  0eac 8df603                     STA gmask_flip+1
  1318  0eaf 60                         RTS
  1319                          
  1320                          modetoggle
  1321  0eb0 a909                       LDA #>(bitmask)
  1322  0eb2 8df103                     STA gchange_op+2
  1323  0eb5 a97d                       LDA #<(bitmask)
  1324  0eb7 8df003                     STA gchange_op+1
  1325  0eba a95d                       LDA #$5D		; opcode EOR abs,X
  1326  0ebc 8def03                     STA gchange_op
  1327  0ebf a951                       LDA #$51		; opcode EOR (zp),Y
  1328  0ec1 8df703                     STA gmask_op
  1329  0ec4 a900                       LDA #$00		; mask, EOR #$00, not inverting
  1330  0ec6 8df603                     STA gmask_flip+1
  1331  0ec9 60                         RTS
  1332                          
  1333                          
  1334                          ;-----------------------------------------------------------------
  1335                          ; get current x cursor position
  1336                          
  1337                          getposx
  1338  0eca ac3c03             	LDY savexl
  1339  0ecd ad3d03             	LDA savexh
  1340  0ed0 2091b3             	JSR b_word2fac
  1341  0ed3 4c7300             	JMP chrget		; last position of expression (function name)
  1342                          
  1343                          ;-----------------------------------------------------------------
  1344                          ; get current y cursor position
  1345                          
  1346                          getposy
  1347  0ed6 ac3e03             	LDY savey
  1348  0ed9 20a2b3             	JSR b_byte2fac
  1349  0edc 4c7300             	JMP chrget		; last position of expression (function name)
  1350                          
  1351                          ;-----------------------------------------------------------------
  1352                          
  1353                          ; get pixel (check if pixel set)
  1354                          ; not used
  1355                          
  1356                          get
  1357  0edf 207300             	JSR chrget		; advance past function name
  1358  0ee2 20faae             	JSR b_chkparl		; "("?
  1359  0ee5 20ca0b                     JSR getxy		; get X,Y values
  1360  0ee8 859c                       STA xh
  1361  0eea 849b                       STY xl
  1362  0eec 86aa                       STX y
  1363  0eee 207900             	JSR chrgot
  1364  0ef1 20f7ae             	JSR b_chkparr		; ")"?
  1365                          	
  1366                          
  1367  0ef4 20770a                     JSR position		; calculate graphic address/position
  1368                          
  1369  0ef7 a501                       LDA prozport
  1370  0ef9 29fd               	AND #%11111101		; Kernal ROM disable
  1371  0efb 78                         SEI
  1372  0efc 8501                       STA prozport
  1373                          
  1374  0efe b1a5                       LDA (gaddr),Y
  1375  0f00 3d7d09                     AND bitmask,X		; mask position
  1376  0f03 a8                         TAY
  1377  0f04 a501                       LDA prozport
  1378  0f06 0902               	ORA #%00000010		; kernal ROM enable
  1379  0f08 8501                       STA prozport
  1380  0f0a 58                         CLI
  1381  0f0b 98                 	TYA
  1382  0f0c f002               	BEQ +
  1383  0f0e a001               	LDY #1			; <> 0 -> always return 1
  1384  0f10 4ca2b3             +	JMP b_byte2fac		; still on expr.'s last character
  1385                          
  1386                          ;-----------------------------------------------------------------
  1387                          
  1388                          relto_cont
  1389                          				; continue
  1390  0f13 207300             	JSR chrget		; skip TO token
  1391                          relto
  1392  0f16 208aad                     JSR b_getval		; get X offset (+/-)
  1393  0f19 a561               	LDA facexp		; FAC exponent
  1394  0f1b c990               	CMP #$90		; more than 16 bit
  1395  0f1d b031               	BCS relto_error		; illegal quantity
  1396  0f1f 209bbc                     JSR b_fac2int		; to signed integer
  1397                          
  1398  0f22 18                         CLC
  1399  0f23 a565                       LDA facintl
  1400  0f25 6d3c03                     ADC savexl
  1401  0f28 859e                       STA xendl
  1402  0f2a a564                       LDA facinth
  1403  0f2c 6d3d03                     ADC savexh
  1404  0f2f 859f                       STA xendh		; xend = savex+facint
  1405                          
  1406  0f31 20fdae                     JSR b_getcomma		; get Y offset (+/-)
  1407  0f34 208aad                     JSR b_getval
  1408  0f37 a561                       LDA facexp		; FAC exponent
  1409  0f39 c990                       CMP #$90		; more than 16 bit
  1410  0f3b b013                       BCS relto_error		; illegal quantity
  1411  0f3d 209bbc                     JSR b_fac2int		; to signed integer
  1412  0f40 18                         CLC
  1413  0f41 a565                       LDA facintl
  1414  0f43 6d3e03                     ADC savey
  1415  0f46 8593                       STA yend		; yend = savey+facint
  1416                          
  1417  0f48 a59f                       LDA xendh		; check end coord. x
  1418  0f4a c901                       CMP #>xmax
  1419  0f4c 900e                       BCC rt_xok
  1420  0f4e f003                       BEQ +
  1421                          relto_error
  1422  0f50 20390e                     JSR range_error
  1423  0f53 a59e               +	LDA xendl
  1424  0f55 c940                       CMP #<xmax
  1425  0f57 9003                       BCC +
  1426  0f59 20390e                     JSR range_error
  1427                          +
  1428                          rt_xok
  1429  0f5c a593                       LDA yend		; check end coord. y
  1430  0f5e c9c8                       CMP #ymax
  1431  0f60 9003                       BCC +
  1432  0f62 20390e                     JSR range_error
  1433                          +
  1434  0f65 ad3c03                     LDA savexl
  1435  0f68 859b                       STA xl
  1436  0f6a ad3d03                     LDA savexh
  1437  0f6d 859c                       STA xh
  1438  0f6f ad3e03                     LDA savey
  1439  0f72 85aa                       STA y
  1440  0f74 a49e                       LDY xendl
  1441  0f76 a59f                       LDA xendh
  1442  0f78 a693                       LDX yend		; xend/yend = cursor + x/y
  1443                          
  1444  0f7a 20620d                     JSR line_start		; draw line x/y to xend/yend
  1445                          
  1446  0f7d 207900             	JSR chrgot
  1447  0f80 d001               	BNE +
  1448  0f82 60                 	RTS
  1449  0f83 c9a4               +	CMP #t_to		; TO keyword?
  1450  0f85 f08c               	BEQ relto_cont
  1451  0f87 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1452                          
  1453                          ;-----------------------------------------------------------------
  1454                          
  1455                          char
  1456  0f8a 209eb7                     JSR b_get8bit		; get char. position x 0-39
  1457  0f8d e028                       CPX #40	
  1458  0f8f 9003                       BCC +
  1459                          char_error
  1460  0f91 4c48b2                     JMP b_illquant
  1461  0f94 86fb               +	STX gpos		; save x coord.
  1462  0f96 20f1b7                     JSR b_getcomma8bit
  1463                          				; get char. position y 0-24
  1464  0f99 e019                       CPX #25
  1465  0f9b b0f4                       BCS char_error
  1466  0f9d 86fc                       STX gpos+1		; save y coord.
  1467                          
  1468  0f9f 20fdae                     JSR b_getcomma		; get string
  1469  0fa2 209ead                     JSR b_getexpr
  1470  0fa5 20a3b6                     JSR b_stringval		 ; string address in str
  1471  0fa8 48                         PHA			; string length
  1472  0fa9 a6fc                       LDX gpos+1		; y coord. for char. position
  1473  0fab 8a                         TXA
  1474  0fac 2903                       AND #$03		; mask 2 bits
  1475  0fae a8                         TAY			; table index
  1476  0faf a900                       LDA #$00
  1477  0fb1 85fc                       STA gpos+1		; x high
  1478  0fb3 a5fb                       LDA gpos		; saved x: multiply by 8
  1479  0fb5 0a                         ASL
  1480  0fb6 0a                         ASL
  1481  0fb7 0a                         ASL
  1482  0fb8 26fc                       ROL gpos+1		; overflow to high byte
  1483  0fba 798d09                     ADC ytabl,Y
  1484  0fbd 85a5                       STA gaddr
  1485  0fbf a5fc                       LDA gpos+1		; x high
  1486  0fc1 7d9109                     ADC ytabh,X
  1487  0fc4 85a6                       STA gaddr+1
  1488  0fc6 68                         PLA			; string length
  1489  0fc7 a000                       LDY #$00		; string index
  1490  0fc9 aa                         TAX			; length
  1491  0fca e8                         INX			; prepare as counter
  1492                          char_loop
  1493  0fcb ca                         DEX
  1494  0fcc f008                       BEQ char_exit
  1495  0fce b122                       LDA (str),Y		; read string
  1496  0fd0 20d70f                     JSR char_display
  1497  0fd3 c8                         INY
  1498  0fd4 d0f5                       BNE char_loop
  1499                          char_exit
  1500  0fd6 60                         RTS
  1501                          
  1502                          char_display
  1503  0fd7 85d7                       STA z_tmp		; character (lastkey, temporary reused)
  1504  0fd9 8a                         TXA			; save register X+Y
  1505  0fda 48                         PHA
  1506  0fdb 98                         TYA
  1507  0fdc 48                         PHA
  1508  0fdd a5d7                       LDA z_tmp		; get saved character
  1509  0fdf 3012                       BMI char_inverse
  1510                          
  1511                          char_normal
  1512  0fe1 c920                       CMP #$20		; control character?
  1513  0fe3 9054                       BCC char_disp_leave
  1514  0fe5 c960                       CMP #$60
  1515  0fe7 9004                       BCC +
  1516  0fe9 29df                       AND #%11011111		; $60-$7F -> $40-$5F
  1517  0feb d014                       BNE char_hires
  1518  0fed 293f               +	AND #%00111111		; $40-$5F -> $00-$1F
  1519  0fef d010               	BNE char_hires
  1520  0ff1 f00e               	BEQ char_hires
  1521                          
  1522                          char_inverse
  1523  0ff3 297f                       AND #%01111111		; mask bit 7
  1524  0ff5 c97f                       CMP #%01111111		; was 255? (pi)
  1525  0ff7 d002                       BNE +
  1526  0ff9 a95e                       LDA #$5E		; screen code for pi
  1527  0ffb c920               +	CMP #$20		; control character?
  1528  0ffd 903a                       BCC char_disp_leave
  1529                          				; yes, skip
  1530  0fff 0940                       ORA #%01000000		; $A0-$BF -> $60-$7F
  1531                          				; $C0-$FF -> $40-$7F
  1532                          				; OPT: BNE char_hires
  1533                          				; OPT: char_normal
  1534                          char_hires
  1535  1001 a6c7                       LDX z_reverseflag
  1536  1003 f002                       BEQ +
  1537  1005 0980                       ORA #%10000000		; invert char.
  1538  1007 aa                 +	TAX			; save char. for later
  1539  1008 a501                       LDA prozport		; save prozport state
  1540  100a 48                 	PHA
  1541  100b a921                       LDA #%00100001		; char. rom, no basic and kernal rom
  1542  100d 78                         SEI
  1543  100e 8501                       STA prozport		; char. rom base = $D000
  1544  1010 a91a                       LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
  1545  1012 85fc                       STA gpos+1		; 
  1546  1014 8a                         TXA			; char. code
  1547  1015 0a                         ASL			; *8
  1548  1016 26fc                       ROL gpos+1
  1549  1018 0a                         ASL
  1550  1019 26fc                       ROL gpos+1
  1551  101b 0a                         ASL
  1552  101c 26fc                       ROL gpos+1
  1553  101e 85fb                       STA gpos		; addr. in char. rom for char.
  1554                          
  1555  1020 a007                       LDY #$07		; 8 hires lines
  1556                          char_line
  1557  1022 b1fb                       LDA (gpos),Y		; read character line
  1558  1024 20f503                     JSR gmask		; write to hires screen
  1559  1027 88                         DEY
  1560  1028 10f8                       BPL char_line
  1561                          
  1562  102a 68                 	PLA
  1563  102b 8501                       STA prozport
  1564  102d 58                         CLI
  1565                          
  1566  102e 18                         CLC			; step char position to left
  1567  102f a5a5                       LDA gaddr		; ( +8 )
  1568  1031 6908                       ADC #$08
  1569  1033 85a5                       STA gaddr
  1570  1035 9002                       BCC +
  1571  1037 e6a6                       INC gaddr+1
  1572                          +
  1573                          char_disp_leave
  1574  1039 68                 	PLA			; pass written character back
  1575  103a a8                         TAY			; restore saved registers
  1576  103b 68                         PLA
  1577  103c aa                         TAX
  1578  103d 60                 -       RTS
  1579                          
  1580                          
  1581                          ;-----------------------------------------------------------------
  1582                          
  1583                          to_cont
  1584                          				; continue
  1585  103e 207300             	JSR chrget		; skip TO token
  1586                          to
  1587  1041 ad3c03                     LDA savexl
  1588  1044 859b                       STA xl
  1589  1046 ad3d03                     LDA savexh
  1590  1049 859c                       STA xh
  1591  104b ad3e03                     LDA savey
  1592  104e 85aa                       STA y
  1593  1050 20ca0b                     JSR getxy
  1594  1053 20620d                     JSR line_start
  1595  1056 207900             	JSR chrgot
  1596  1059 f0e2               	BEQ -
  1597  105b c9a4               	CMP #t_to		; TO keyword?
  1598  105d f0df               	BEQ to_cont
  1599  105f 4c08af                     JMP b_syntaxerror	; throw error (unknown command)
  1600                          
  1601                          ;-----------------------------------------------------------------
  1602                          
  1603                          box
  1604  1062 20f30b                     JSR para_hline_box
  1605  1065 9003               	BCC +
  1606  1067 20390e             	JSR range_error
  1607                          				; XXX xend=xmax-1 ?
  1608                          +
  1609  106a 20f1b7             	JSR b_getcomma8bit
  1610  106d 8a                 	TXA			; optional 8-bit parameter
  1611                          				; height
  1612  106e f00c               	BEQ +++			; 0 means 1, box is just a line
  1613  1070 18                 	CLC
  1614  1071 65aa               	ADC y			; end position for y coord.
  1615  1073 b004               	BCS +			; > 255
  1616  1075 c9c8               	CMP #ymax
  1617  1077 9003               	BCC +++
  1618                          +				; C=1 from ADC or CMP before
  1619  1079 20390e             	JSR range_error		; corrupts A
  1620                          				; XXX ycount=ymax-y-1 ?
  1621                          				; xend >= x
  1622  107c 48                 +++	PHA			; yend
  1623  107d a900               	LDA #0
  1624  107f 85a3               	STA ycount		; line thickness 1
  1625  1081 20560c             	JSR hl_noxswap		; upper horizontal line
  1626                          
  1627                          				; right vertical line
  1628  1084 68                 	PLA			; if 0, heigth is 1
  1629  1085 d001               	BNE +			; no 
  1630  1087 60                 	RTS			; exit, if box is degenerated (line)
  1631  1088 a6aa               +	LDX y			; start point at higher values
  1632  108a 85aa               	STA y
  1633  108c 8693               	STX yend
  1634  108e a59e               	LDA xendl
  1635  1090 859b               	STA xl
  1636  1092 a59f               	LDA xendh
  1637  1094 859c               	STA xh
  1638  1096 20270d             	JSR vl_noyswap		; xend,yend -> xend,y
  1639                          				; lower horizontal line
  1640  1099 ad3c03             	LDA savexl
  1641  109c 859b               	STA xl
  1642  109e ad3d03             	LDA savexh
  1643  10a1 859c               	STA xh			; xend already set
  1644  10a3 20560c             	JSR hl_noxswap		; x,yend -> xend,yend
  1645                          				; left vertical line
  1646  10a6 4c270d             	JMP vl_noyswap		; x,y -> x,xend
  1647                          
  1648                          ;-----------------------------------------------------------------
  1649                          
  1650                          fill
  1651  10a9 20ca0b                     JSR getxy
  1652  10ac 859c                       STA xh			; save x/y
  1653  10ae 849b                       STY xl
  1654  10b0 86aa                       STX y
  1655  10b2 8d3d03                     STA savexh		; and store as cursor
  1656  10b5 8c3c03                     STY savexl
  1657  10b8 8e3e03                     STX savey
  1658                                  
  1659  10bb a531                       LDA basaryend		; initialize fill stack pointer
  1660  10bd 38                 	SEC
  1661  10be e904               	SBC #4			; one element below
  1662  10c0 85fd                       STA fstack		; use space between basic arrays
  1663  10c2 a532                       LDA basaryend+1		; and string heap bottom
  1664  10c4 e900               	SBC #0			; take borrow
  1665  10c6 85fe                       STA fstack+1
  1666                          
  1667  10c8 20770a             	JSR position		; graphic position in (gaddr)+Y, bit X
  1668                          
  1669  10cb a59c               	LDA xh			; setup 8x8 block index (x8)
  1670  10cd 4a                 	LSR			; high bit into C
  1671  10ce a59b               	LDA xl
  1672  10d0 2a                 	ROL			; take high bit
  1673  10d1 4a                 	LSR
  1674  10d2 4a                 	LSR			; finally divide by 8
  1675  10d3 85a7               	STA x8			; = index of 8x8 block in bitmap
  1676                          
  1677                          	; set fmode (from mode)
  1678  10d5 ad3f03             	LDA savemo
  1679  10d8 2903               	AND #3
  1680  10da aa                 	TAX
  1681  10db ca                 	DEX
  1682  10dc 3003               	BMI +			; mode = 0 -> invertmask: $FF
  1683  10de f001               	BEQ +			; mode = 1 -> invertmask: $00
  1684  10e0 ca                 	DEX			; mode = 2 -> ? (same as mode=0)
  1685  10e1 86a8               +	STX fmode		; mode set or reset
  1686                          
  1687  10e3 20600a             	JSR ginit		; map in bitmap memory
  1688                          
  1689  10e6 b1a5               	LDA (gaddr),y		; graphic position in Y (in index in 8x8 block)
  1690  10e8 45a8               	EOR fmode
  1691  10ea 8595               	STA tmp1		; bitmap, for later usage
  1692                          
  1693  10ec 3d7d09             	AND bitmask,x		; test start pixel
  1694  10ef f003               	BEQ +			; not set
  1695                          f_exit
  1696  10f1 4c580a             	JMP gexit		; leave if start pixel is already set
  1697                          +
  1698                          f_start				; the start: in mid of a line to fill ...
  1699  10f4 a900               	LDA #0
  1700  10f6 8596               	STA fcont		; initialize continuation flag for line above und below
  1701                          
  1702  10f8 a595               	LDA tmp1		; graphic pixel data
  1703                          				; extent bitmask to the right
  1704  10fa 86ab               	STX xsave
  1705  10fc 3daa09             	AND maskleft,x		; mask out left part, bits right from starting point remain
  1706  10ff 208f12             	JSR bitposr		; find the first set bit from start to right (border)
  1707  1102 bdb309             	LDA maskright0,x	; get a mask from the right border to left
  1708  1105 85a3               	STA tmpmask		
  1709                          
  1710                          leftcont
  1711  1107 a595               	LDA tmp1		; graphic pixel data
  1712  1109 a6ab               	LDX xsave
  1713                          leftcont_a
  1714  110b 3db409             	AND maskright,x		; mask out right part, bits left from starting point remain
  1715  110e f00e               	BEQ stepleft8		; no left border in this pixel line
  1716  1110 208312             	JSR bitposl		; find the first set bit from start to left (border)
  1717  1113 bdaa09             	LDA maskleft0,x		; get a mask from the left border to right
  1718  1116 25a3               	AND tmpmask		; intersect masks
  1719  1118 85a3               	STA tmpmask		; and store it for later
  1720  111a f021               	BEQ next_block		; empty mask immediate continue to right
  1721  111c d047               	BNE to_right		; start to walk and fill to the right border
  1722                          
  1723                          stepleft8
  1724  111e a5a7               	LDA x8 			; 8x8 block position
  1725  1120 f043               	BEQ to_right		; =0, hit screen border
  1726  1122 c6a7               	DEC x8			; count step 8x8 block to left
  1727  1124 a9ff               	LDA #$ff
  1728  1126 85a3               	STA tmpmask		; initial mask full pixel line
  1729                          
  1730  1128 38                 	SEC 			; graphic address to to next pixel line/block
  1731  1129 a5a5               	LDA gaddr
  1732  112b e908               	SBC #8
  1733  112d b002               	BCS +
  1734  112f c6a6               	DEC gaddr+1
  1735  1131 85a5               +	STA gaddr
  1736                          
  1737                          	; y left unchanged
  1738  1133 b1a5               	LDA (gaddr),y		; real graphic pixel data from bitmap
  1739  1135 45a8               	EOR fmode		; set/reset mode
  1740  1137 8595               	STA tmp1		; graphic pixel data
  1741  1139 a207               	LDX #7			; start bit 0 (index 7, rightmost)
  1742  113b d0ce               	BNE leftcont_a		; loop to left border search
  1743                          	
  1744                          next_block
  1745  113d e6a7               	INC x8			; step right a block
  1746  113f a5a7               	LDA x8
  1747  1141 c928               	CMP #40			; beyond last horizontal block?
  1748  1143 b077               	BCS process_stack	; done if right screen border
  1749                          	; C = 0
  1750  1145 a5a5               	LDA gaddr		; advance to block right
  1751  1147 6908               	ADC #8			; gaddr = gaddr + 8
  1752  1149 85a5               	STA gaddr
  1753  114b 9002               	BCC +
  1754  114d e6a6               	INC gaddr+1
  1755  114f a9ff               +	LDA #$ff		; asume "all pixels" mask
  1756  1151 85a3               	STA tmpmask
  1757  1153 b1a5               	LDA (gaddr),y		; pixel data
  1758  1155 45a8               	EOR fmode		; set/reset mode
  1759  1157 f00c               	BEQ to_right		; empty -> finally to to_right
  1760  1159 208f12             	JSR bitposr		; search right border
  1761  115c bdb309             	LDA maskright0,x	; mask out the right part
  1762  115f 25a3               	AND tmpmask		; shorten mask accordingly
  1763  1161 85a3               	STA tmpmask
  1764  1163 f057               	BEQ process_stack	; done if bit 7 (leftmost) is set
  1765                          				; leading to 0 mask (fill_check wont't
  1766                          				; handle this special case)
  1767                          
  1768                          				; continue to fill to right ...
  1769                          to_right			; fill loop towards right border
  1770  1165 a5a3               	LDA tmpmask		; fill mask
  1771                          				; assert:    (bitmap & tempmask) == 0
  1772                          				;         || (bitmap & tempmask) == tempmask
  1773  1167 51a5               	EOR (gaddr),y		; set/reset to fill
  1774  1169 91a5               	STA (gaddr),y		; into bitmap - the actual fill action!
  1775                          	
  1776                          check_above
  1777  116b 0696               	ASL fcont		; bit 0 to bit 1 position to check (above)
  1778                          				; c = 0!
  1779  116d 84a9               	STY ysave		; to be restored later
  1780  116f a5a5               	LDA gaddr		; current graphic position
  1781  1171 a6a6               	LDX gaddr+1
  1782  1173 88                 	DEY			; line above
  1783  1174 100f               	BPL +			; leaving 8x8 block?
  1784                          	; c=0 (asl fcont)
  1785  1176 e93f               	SBC #$40-1		; block above:
  1786  1178 85fb               	STA caddr		; caddr = gaddr - $140
  1787  117a 8a                 	TXA
  1788  117b e901               	SBC #$01
  1789  117d aa                 	TAX
  1790  117e c9e0               	CMP #>gram		; still graphic ram?
  1791  1180 900a               	BCC skip_above
  1792  1182 a007               	LDY #7			; last line in block in new block
  1793  1184 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1794  1185 85fb               +	STA caddr		; still in same block
  1795  1187 86fc               ++	STX caddr+1		; shared store
  1796  1189 201a12             	JSR fill_check
  1797                          skip_above
  1798                          
  1799                          check_below
  1800  118c 4696               	LSR fcont		; bit 2 back to bit 1 position to check (below)
  1801  118e a5a5               	LDA gaddr		; current graphic position
  1802  1190 a6a6               	LDX gaddr+1
  1803  1192 a4a9               	LDY ysave		; restore original y position
  1804  1194 c8                 	INY			; line below
  1805  1195 c008               	CPY #8			; crossing 8x8 block?
  1806  1197 9014               	BCC +			; less then 8
  1807                          	; c=1 (cpy)
  1808  1199 693f               	ADC #$40-1		; block below: accu has gaddr
  1809  119b 85fb               	STA caddr		; caddr = gaddr + $140
  1810  119d a8                 	TAY			; for compare later
  1811  119e 8a                 	TXA			; gaddr high
  1812  119f 6901               	ADC #$01
  1813  11a1 aa                 	TAX
  1814  11a2 b010               	BCS skip_below		; > $10000  -> skip
  1815  11a4 c040               	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
  1816  11a6 e9ff               	SBC #>(gram+8000)
  1817  11a8 b00a               	BCS skip_below		; greater, so skip
  1818  11aa a000               	LDY #0			; first line in block
  1819  11ac 2c                 	!by $2c			; = bit $hhll, skip next statement (2 bytes)
  1820  11ad 85fb               +	STA caddr		; transfer unchanged
  1821  11af 86fc               ++	STX caddr+1		; shared store
  1822  11b1 201a12             	JSR fill_check
  1823                          skip_below
  1824                          
  1825  11b4 a4a9               	LDY ysave		; restore original y position
  1826  11b6 a5a3               	LDA tmpmask		; mask:
  1827  11b8 2901               	AND #%00000001		; open to right, continue?
  1828  11ba d081               	BNE next_block		; to next block if open
  1829                          ; long branch version
  1830                          ;	BEQ process_stack	; not open, finished
  1831                          ;	JMP next_block		; to next block if open
  1832                          
  1833                          process_stack
  1834  11bc a5fd               	LDA fstack		; stack empty?
  1835  11be c531               	CMP basaryend
  1836  11c0 a5fe               	LDA fstack+1
  1837  11c2 e532               	SBC basaryend+1
  1838  11c4 b003               	BCS +			; fstack >= basaryend -> not empty
  1839  11c6 4c580a             	JMP gexit		; empty, we are finished
  1840                          
  1841  11c9 a003               +	LDY #4-1		; top of stack, element's last component
  1842  11cb b1fd               	LDA (fstack),y
  1843  11cd 85a7               	STA x8			; 8x8 block position
  1844  11cf 88                 	DEY
  1845  11d0 b1fd               	LDA (fstack),y
  1846  11d2 85a3               	STA tmpmask		; pixel mask
  1847  11d4 88                 	DEY
  1848  11d5 b1fd               	LDA (fstack),y
  1849  11d7 85a6               	STA gaddr+1		; graphic addr high byte
  1850  11d9 88                 	DEY
  1851  11da b1fd               	LDA (fstack),y		; graphic addr low byte combined with y-line
  1852  11dc aa                 	TAX			; needed twice
  1853  11dd 29f8               	AND #%11111000		; split off address
  1854  11df 85a5               	STA gaddr
  1855  11e1 8a                 	TXA
  1856  11e2 2907               	AND #%00000111		; split off y-line
  1857  11e4 a8                 	TAY
  1858                          	
  1859  11e5 b1a5               	LDA (gaddr),y		; get pixels
  1860  11e7 45a8               	EOR fmode		; according to set/reset
  1861  11e9 aa                 	TAX			; keep it for later
  1862  11ea 25a3               	AND tmpmask		; focus on masked pixels
  1863  11ec 08                 	PHP			; save Z flag
  1864  11ed f004               	BEQ pop_stack		; all bits unset, remove from stack
  1865                          				; and fill it!
  1866  11ef c5a3               	CMP tmpmask		; all gaps filled?
  1867  11f1 d010               	BNE +++			; still some gaps (splitted pixels), leave on stack
  1868                          	; all gaps filled, next on stack 
  1869                          pop_stack
  1870  11f3 38                 	SEC	
  1871  11f4 a5fd               	LDA fstack		; remove entry from stack
  1872  11f6 e904               	SBC #4			; entry size
  1873  11f8 85fd               	STA fstack
  1874  11fa b002               	BCS +
  1875  11fc c6fe               	DEC fstack+1
  1876  11fe 28                 +	PLP
  1877  11ff d0bb               	BNE process_stack	; all masked bits are set, next stack element
  1878                          				; all bits unset,
  1879  1201 f001               	BEQ ++			; stack already cleaned up
  1880  1203 28                 +++	PLP			; stack cleanup
  1881                          
  1882                          	; set bits outside mask to 1
  1883  1204 8a                 ++	TXA			; bitmap
  1884                          				; 00100110	
  1885  1205 49ff               	EOR #$ff		; 11011001
  1886  1207 25a3               	AND tmpmask		; 00011100 -> 00011000
  1887  1209 49ff               	EOR #$ff		; 11100111
  1888                          				; pixel outside tmpmask now set!
  1889  120b a2ff               	LDX #$ff		; pixel gap search: first one from left
  1890  120d e8                 -	INX
  1891  120e 0a                 	ASL			; counting from left
  1892  120f b0fc               	BCS -			; loop if pixel is set
  1893                          				; X has the bit number of the unset pixel
  1894  1211 b1a5               	LDA (gaddr),y		; setup value for processing a new line
  1895  1213 45a8               	EOR fmode		; set/reset mode
  1896  1215 8595               	STA tmp1		; temporary bitmap pixels
  1897  1217 4cf410             	JMP f_start		; long (to far away) jump to fill line start
  1898                          ;	BCC f_start		; not used: short variant, always (C=0 from above)
  1899                          
  1900                          
  1901                          ; Check upper or lower fill path
  1902                          ;		destroys x
  1903                          
  1904                          fill_check
  1905  121a b1fb               	LDA (caddr),y
  1906  121c 45a8               	EOR fmode		; pixel data
  1907  121e aa                 	TAX			; save for later
  1908  121f 25a3               	AND tmpmask		; mask to fill
  1909  1221 f015               	BEQ fc_cleared		; all masked pixels cleared?
  1910  1223 c5a3               	CMP tmpmask		; check for gaps
  1911  1225 f05b               	BEQ fc_exit		; all gaps filled, finished
  1912                          				; if not so, some pixels still set
  1913  1227 a5a3               	LDA tmpmask
  1914                          fc_checkstart			; no continuation, init flag based on
  1915                          				; rightmost pixel:
  1916  1229 4a                 	LSR			; mask bit 0 to carry
  1917  122a 9019               	BCC fc_nocont		; maskbit empty?
  1918  122c 8a                 	TXA			; pixel data
  1919  122d 4a                 	LSR			; pixel bit 0 to carry
  1920  122e b015               	BCS fc_nocont		; bit 0 set
  1921                          				; -> mask is 1 and pixel 0
  1922                          fc_cont
  1923  1230 a596               	LDA fcont		; set flag for continuation
  1924  1232 0902               	ORA #%00000010		; mark in bit 1, store it, make a push
  1925  1234 8596               	STA fcont
  1926  1236 d013               	BNE push_to_stack	; always non zero
  1927                          
  1928                          fc_cleared
  1929  1238 a5a3               	LDA tmpmask		; pixel & mask -> 0
  1930                          ;	BEQ fc_exit		; but if mask=0 we are done (never push!)
  1931                          				; the caller asserts that this never happens
  1932  123a c9ff               	CMP #$ff		; full pixel line mask and all pixels cleared
  1933  123c d0eb               	BNE fc_checkstart	; maybe a continuation ...
  1934                          				; 8 pixel line empty
  1935  123e a596               	LDA fcont		; continued gap?
  1936  1240 2902               	AND #%00000010		; check bit 2
  1937  1242 f0ec               	BEQ fc_cont		; new gap, start it and push on stack
  1938  1244 60                 	RTS			; gap continued and already on stack, leave
  1939                          
  1940                          fc_nocont
  1941  1245 a596               	LDA fcont		; clear continuation flag
  1942  1247 29fd               	AND #%11111101		; clear bit 2
  1943  1249 8596               	STA fcont
  1944                          
  1945                          push_to_stack
  1946  124b 18                 	CLC			; fstack points to top of stack
  1947  124c a5fd               	LDA fstack		; to next free stack element
  1948  124e 6904               	ADC #4			; entry size
  1949  1250 85fd               	STA fstack
  1950  1252 9002               	BCC +
  1951  1254 e6fe               	INC fstack+1
  1952                          +
  1953  1256 a534               	LDA strbot+1		; check stack space
  1954  1258 c5fe               	CMP fstack+1
  1955  125a b008               	BCS ++			; strbot MSB >= fstack MSB, need more to check
  1956                          				; strbot MSB < fstack MSB
  1957                          out_of_memory			
  1958  125c 20580a             	JSR gexit
  1959  125f a210               	LDX #$10		; out of memory error
  1960  1261 6c0003             	JMP (v_baserr)		; basic error handler
  1961  1264 d006               ++	BNE fc_put		; <> -> (strbot > fstack)
  1962  1266 a5fd               	LDA fstack		; MSB equal, check LSB
  1963  1268 c533               	CMP strbot
  1964  126a b0f0               	BCS out_of_memory	; fstack collides with string heap!
  1965                          
  1966                          fc_put
  1967  126c 98                 	TYA			; y-line (value 0-7) merged with
  1968  126d 05fb               	ORA caddr		; graphic address low (bit 0-2 always empty)
  1969  126f a000               	LDY #0			; stack structure index, on next free element
  1970  1271 91fd               	STA (fstack),y
  1971  1273 c8                 	INY
  1972  1274 a5fc               	LDA caddr+1
  1973  1276 91fd               	STA (fstack),y		; graphic address high
  1974  1278 c8                 	INY
  1975  1279 a5a3               	LDA tmpmask
  1976  127b 91fd               	STA (fstack),y
  1977  127d c8                 	INY
  1978  127e a5a7               	LDA x8			; 8x8 block position
  1979  1280 91fd               	STA (fstack),y
  1980                          	
  1981  1282 60                 fc_exit	RTS
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
  2006  1283 a2ff               	LDX #$ff
  2007  1285 c900               	CMP #0		; special case (no bit set at all)
  2008  1287 f004               	BEQ +
  2009  1289 e8                 -	INX
  2010  128a 0a                 	ASL		; shift to left
  2011  128b d0fc               	BNE -		; until byte is empty
  2012  128d e8                 +	INX
  2013  128e 60                 	RTS
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
  2036  128f a208               	LDX #8
  2037  1291 c900               	CMP #0			; special case (no bit set at all)
  2038  1293 f004               	BEQ +
  2039  1295 ca                 -	DEX
  2040  1296 4a                 	LSR			; shift to right
  2041  1297 d0fc               	BNE -			; until byte is empty
  2042  1299 60                 +	RTS
  2043                          
  2044                          ;-----------------------------------------------------------------
  2045                          
  2046                          unnew
  2047                          
  2048  129a a52b               	LDA bassta
  2049  129c 8522               	STA str
  2050  129e a52c               	LDA bassta+1
  2051  12a0 8523               	STA str+1
  2052  12a2 a001               	LDY #1
  2053  12a4 98                 	TYA
  2054  12a5 9122               	STA (str),y		; != 0
  2055                          
  2056  12a7 2033a5             	JSR b_rechain		; starting from bassta
  2057                          				; result in (str)
  2058  12aa 18                 	CLC			; str+1 -> new basic end
  2059  12ab a423               	LDY str+1
  2060  12ad a522               	LDA str
  2061  12af 6902               	ADC #2
  2062  12b1 852d               	STA basend
  2063  12b3 9001               	BCC +
  2064  12b5 c8                 	INY
  2065  12b6 842e               +	STY basend+1
  2066  12b8 4c60a6             	JMP b_clr		; perform CLR
  2067                          
  2068                          
  2069                          ;-----------------------------------------------------------------
  2070                          graext_end

; ******** Source: ge-run.asm
    45                          
    46                          
