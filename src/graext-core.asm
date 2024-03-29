;  **** gra-ext core ****
;
; 2015-10-05 johann e. klasek, johann at klasek at
;
!macro version {
	!text "1.34" ; current version
}
; revisions:
;	2022-04-24 v 1.34
;	2022-03-27 v 1.33
;	2020-05-03 v 1.32
;	2019-10-30 v 1.31
;	2019-10-24 v 1.30
;	2019-10-10 v 1.29
;	2016-09-10 v 1.28
;	2016-07-13 v 1.27
;	2016-07-09 v 1.26
;	2016-06-21 v 1.25
;	2016-06-16 v 1.24
;	2016-05-29 v 1.23
;	2016-05-20 v 1.22
;	2016-05-16 v 1.21
;	2016-02-23 v 1.20
;	2016-01-15 v 1.19
;	1992-12-28 v 1.18
;	1986-03-24 v 1.17
;	1985       v 0.00 - 1.16
;
; the initial development is based on the implemention
; done in a Forth environment written with a common 
; 6502 forth assembler.
; later, the code has been pulled out from there, relocated and 
; enriched with some glue code to finally form the first 
; basic extension.

; command dispatcher style JMP/RTS
;	(if defined)
;command_rts_style=1

; error handling 
;	(if defined)
;no_error=1

; optimize for space (at runtime)
;opt_space=1


; basic interpreter registers, addresses and entry points

type	= $0d
str     = $22		; string address
bassta	= $2b		; basic start pointer
basend	= $2d		; basic end pointer
basaryend	= $31		; basic end of array +1
strbot	= $33		; bottom of string heap 
ijmp    = $55		; address of JMP (addr)
chrget  = $73		; basic charget routine
chrgot  = $79		; basic last char got (charget routine)
txtptr	= $7A		; basic text pointer
facintl = $65		; integer result from b_fac2int
facinth = $64
facexp  = $61		; fac exponent, after b_getval

z_reverseflag = $C7	; character routine
z_lastkey = $D7		; original use case, unused here
z_tmp = z_lastkey	; temporary reused for character routine

v_baserr = $0300	; vector error routine
v_basstp = $0328	; vector error routine
v_bascmd = $0308	; vector interpreter parsing
v_basexp = $030a	; vector evaluate expression

basic_rom = $A000	; start of BASIC ROM

b_clr = $A660		; CLR command
b_interpreter = $A7AE	; interpreter loop
b_execstatement = $A7E7	; process statement (after chrget) - not used
b_execexpr =$AE92	; process expression - not used
b_getcomma = $AEFD	; read comma from basic text
b_illquant = $B248	; error "illegal quantity"
b_syntaxerror = $AF08	; error "syntax"
b_get8bit = $B79E	; read 8 bit numeric value from
			; basic text
b_getcomma8bit = $B7F1	; read comma and 8 bit numeric value
			; from basic text
b_getval = $AD8A	; read numeric value from basic text
b_getexpr = $AD9E	; read expression from basic text
b_byte2fac =$B3A2	; convert Y to FAC (unsigned 8 bit)
b_word2fac =$B391	; convert Y/Y to FAC (signed 16 bit)
b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
b_fac2int = $BC9B	; convert FAC to integer
b_stringval = $B6A3	; take epression as string $22/$23 (str)
b_rechain = $A533	; rechain basic lines
b_str2fac = $BCF3	; convert string in FAC (expression handling)
b_chkparl = $AEFA 	; check '('
b_chkparr = $AEF7 	; check ')'

t_to = $A4		; keyword TO token

; hardware registers and values

prozport = $01		; processor port
memrom = %00110111	; basic+kernal rom
membas = %00110110	; basic ram+kernal rom
memram = %00110101	; basic+kernal ram

vic_cr	= $D011		; VIC control register
vic_mcr	= $D018		; VIC memory control register
cia_pra	= $DD00		; CIA 2 port register A

cram	= $CC00		; start of color ram

gram	= $e000		; start of graphic bitmap ram
gramp	= gram >> 8	; start page of bitmap

; constants 

xmax	= 320		; max x dimension
ymax	= 200		; max y dimension

!ifdef opt_space {
fesize	= 3		; Fill stack entry size without block position
} else {
fesize	= 4		; Fill stack entry size with block position
}

; zeropage variables

x	= $9B		; start coordinate x, low+high
xl	= x
xh	= x+1
y	= $AA		; start coordinate y

xendl	= $9E		; end coordinate x, low+high
xendh	= $9F
yend	= $93		; end coordinate y

kl	= $95		; gradient for lines, low+high
kh	= kl+1
tmpbits	= kl		; temp. var. (hline, vline, fill context)
tmp2	= kh		; temp. var. (hline, vline context)
fcont	= kh		; fill continuation flags (bit 1,0 for above, below)

dxl	= $AB		; x delta, low+high
xsave	= dxl		; x register saved (hline, fill context)
dxh	= $A7
x8	= dxh		; 8x8 block index: (xh/xl) : 8 (fill context)

dy	= $A9		; y delta
ysave	= dy		; y saved (hline context, fill context)

ydir	= $A8		; y direction: 0 | !=0 ... down | up
ylimit	= ydir		; y limit in a 8x8 block (hline context)
fmode   = ydir		; mode mask: 0 | $FF (fill context)

cl	= $A3		; dot count, low+high
ch	= $A4
ycount	= cl		; y count overall (hline context)
hcount	= ch		; horizontal blocks (hline context)
tmpmask	= cl		; temp. mask (fill context)

gaddr	= $A5		; graphic address

gpos	= $FB		; in graphic position
sgaddr	= gpos		; saved gaddr (hline context)
caddr	= gpos		; check gaddr (fill context)

gcol	= $FD		; graphic color, in "graphic on" context only
fstack = gcol	; fill stack pointer (fill context)

; static ram areas

savevpars = $0334	; original v_bascmd
saveverr = savevpars+2	; original v_baserr
savevstp = saveverr+2	; original v_basstp
savevexp = savevstp+2	; original v_basexp
savexl	= savevexp+2	; the graphic cursor: x low 
savexh	= savexl+1	; the graphic cursor: x high
savey	= savexh+1	; the graphic cursor: y
savemo	= savey+1	; the graphic mode
saveend = savemo+1	; byte after save area

			; real place for gchange and gmask routines,
!ifdef ltc {
gramcode = $03ed - 26	; 15 bytes + 4*6+2
} else {
gramcode = $03ed	; 15 bytes
}

; LTC64 specifics

!ifdef ltc {

!cpu 65816

bank4+3 = $040000
rombank+3 = $010000	; c't

; c't-Karte-Kontrollregister

memconf = bank4 or 1
mc_off  = $80		; CPU 816 ausschalten
mc_slow = $40		; CPU 1 MHz
mc_epr  = $20		; EPROM in Bank0
mc_sim  = $10		; ROM-Simulation Bit

}



;
; initialize extension

init
        LDA v_bascmd		; check if hooks are already 
        LDX v_bascmd+1		; in place 
	CMP #<(parse)
	BNE +
	CPX #>(parse)
	BEQ ++			; already hooked

+       STA savevpars		; save old vector
	STX savevpars+1
	LDA #<(parse)		; basic interpreter parser hook
        STA v_bascmd		; for commands
        LDA #>(parse)
        STA v_bascmd+1

        LDA v_basexp		; basic interpreter parser hook
	STA savevexp		; for expressions
        LDA #<(express)		; with save of old pointer
        STA v_basexp
        LDA v_basexp+1
	STA savevexp+1
        LDA #>(express)
        STA v_basexp+1

        LDA v_basstp
	STA savevstp
        LDA #<(stop)		; basic interpreter stop hook
        STA v_basstp
        LDA v_basstp+1
	STA savevstp+1
        LDA #>(stop)
        STA v_basstp+1

        LDA v_baserr
	STA saveverr
        LDA #<(error)		; basic interpreter error hook
        STA v_baserr
        LDA v_baserr+1
	STA saveverr+1
        LDA #>(error)
        STA v_baserr+1

++	LDX #0			; set graphic cursor to (0,0)
	STX savexl
	STX savexh
	STX savey
	INX
	STX savemo		; set mode 1
        RTS

error	
	; reg A may destroyed
	JSR gra_off		; uses only reg A
	JMP (saveverr)		; to original vector

stop	
	; reg A may destroyed
	LDA $91			; Scan code
	CMP #$7F		; STOP key?
	BNE nostop
	JSR gra_off		; uses only reg A
nostop
	JMP (savevstp)		; to original vector


;-----------------------------------------------------------------

; undo chrget

undo_chrget
	LDA txtptr		; decrement text pointer by 1
	BNE +
	DEC txtptr+1
+	DEC txtptr
	RTS

;-----------------------------------------------------------------

; start parsing an extension command ...

parse
        JSR chrget		; next char.
        CMP #'&'		; command prefix
        BEQ newcmd
	JSR undo_chrget
	JMP (savevpars)
newcmd
        JSR chrget		; command character

        LDY #(cmdsend-cmds)	; map character to
				; command address ...
checknextcmd
        DEY
	BEQ parse_error
        CMP cmds,Y
        BNE checknextcmd	; try next
        DEY			; found
        TYA
        ASL			; *2
        TAY
!ifndef command_rts_tyle {
	!set co=0		; command offset in jump table
        LDA cmdaddr+1,Y		; high byte from table
        STA ijmp+1
        LDA cmdaddr,Y		; low byte from table
        STA ijmp
        JSR chrget		; read next byte in basic text
        JSR ijmp-1		; go to command by JMP (addr)
        JMP b_interpreter	; continue parsing
} else {
	!set co=1		; command offset in jump table
	LDA #>(b_interpreter-1)	; return to interpreter
	PHA
	LDA #<(b_interpreter-1)
	PHA				
        LDA cmdaddr+1,Y		; command address (RTS style)
        PHA			; high byte on stack
        LDA cmdaddr,Y		; command address (RTS style)
        PHA			; low byte on stack
        JMP chrget		; read next byte in basic text 
				; and RTS to command
}
parse_error
        JMP b_syntaxerror	; throw error (unknown command)

;-----------------------------------------------------------------
        ; see http://unusedino.de/ec64/technical/aay/c64/romae83.htm
express
	LDA #0
	STA type	
	JSR chrget
	BCS exp_nonumber
	JMP b_str2fac
exp_nonumber
        CMP #'&'		; command prefix
        BEQ newfunc
	JSR undo_chrget
	JMP (savevexp)		; original routine	
;	JMP b_execexpr
newfunc
	JSR chrget
	CMP #'Z'
	BNE +
	JMP get
+	CMP #'X'
	BNE +
	JMP getposx
+	CMP #'Y'
	BNE parse_error
	JMP getposy

;-----------------------------------------------------------------

; the most commonly used command placed at the end ...

cmds	!text " UFGCSMBRTVHLP"		; first char. is a dummy
cmdsend

cmdaddr
        !word unnew-co,fill-co,graphic-co,char-co,setmode-co,move-co
        !word box-co,relto-co,to-co,vline-co,hline-co,line-co,plot-co

author	!text 147,"GRA-EXT V"
	+version
	!text " 1986,2022 JOHANN@KLASEK.AT",0

bitmask
	!byte $80, $40, $20, $10, $08, $04, $02, $01
nbitmask
	!byte $7f, $bf, $df, $ef, $f7, $fb, $fd, $fe
ytabl
	!byte $00,$40,$80,$c0
ytabh
	!byte gramp+$00,gramp+$01,gramp+$02,gramp+$03
	!byte gramp+$05,gramp+$06,gramp+$07,gramp+$08
	!byte gramp+$0a,gramp+$0b,gramp+$0c,gramp+$0d
	!byte gramp+$0f,gramp+$10,gramp+$11,gramp+$12
	!byte gramp+$14,gramp+$15,gramp+$16,gramp+$17
	!byte gramp+$19,gramp+$1a,gramp+$1b,gramp+$1c
	!byte gramp+$1e

; for horiz. line

maskleft0
maskleft
	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
	!byte $00

maskright0
	!byte $00
maskright
	!byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff

;-----------------------------------------------------------------

graphic
        JSR b_get8bit
        CPX #$00
        BNE gra_other
gra0				; &G 0
gra_off
        LDA #$C7		; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
        STA cia_pra
        LDA #((1 <<4) + (2 <<1) + 1)
				; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
				; char addr $1000/4096 = char. ROM
        STA vic_mcr		; VIC memory control
        LDA vic_cr		; VIC control register
        AND #%11011111		; Hires mode off
        STA vic_cr
        RTS

gra_other
        CPX #$01
	BEQ gra1
	CPX #$02
        BEQ gra2
	CPX #$04
        BEQ gra_clear		; &G 4 (erase only, leave mode)
	CPX #$03		; &G 3 (graphic on)
	BEQ gra_on
        JMP b_illquant		; parameter illegal
	
gra1				; &G 1
	JSR gra_clear

gra2
        JSR b_getcomma8bit
        TXA			; foreground color
        ASL			; upper nibble
        ASL
        ASL
        ASL
        STA gcol
        JSR b_getcomma8bit
        TXA			; background color
        AND #$0F
        ORA gcol
        LDY #$00
cram_loop
        STA cram,Y		; fill color RAM
        STA cram+$100,Y
        STA cram+$200,Y
        STA cram+$300-24,Y
        INY
        BNE cram_loop

gra_on
	JSR gra_setupcode

        LDA #$C4		; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
        STA cia_pra
        LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
        STA vic_mcr		; VIC memory control
        LDA vic_cr		; VIC control register
        ORA #%00100000		; Bit 5 = 1: Hires on
        STA vic_cr
        RTS

gra_clear
        LDX #$20		; Pages (8 KByte)
        LDA #>gram
        STA gpos+1
        LDY #$00
        STY gpos
        TYA
gra_fill
        STA (gpos),Y		; Loop unroll
        INY
        STA (gpos),Y
        INY
        STA (gpos),Y
        INY
        STA (gpos),Y
        INY
        BNE gra_fill
        INC gpos+1
        DEX
        BNE gra_fill
	RTS

gra_setupcode
	LDX #(gromcode_end-gromcode) ; count of bytes
gra_copycode
	LDA gromcode-1,X
	STA gramcode-1,X
	DEX
	BNE gra_copycode
	LDA savemo
	AND #$0F
	TAX
	JMP setmode_enter	; re-apply mode to routines
				; implicit RTS

;-----------------------------------------------------------------

gexit
        LDA prozport
        ORA #%00000010		; kernal ROM enable
        STA prozport
        CLI			; allow interrupts
        RTS

;-----------------------------------------------------------------

ginit
        LDA prozport
        AND #%11111101		; Kernal ROM disable
        SEI			; disable interrupts
        STA prozport
        RTS
				; on exit Z=0

;-----------------------------------------------------------------

; These are selfmodified routines, which has to placed into RAM
; (on every graphic "on")
; Code gromcode to gromcode_end-1 is relocated to gramcode

gromcode

!pseudopc gramcode {

; change a graphic location

gchange
!ifdef ltc {
	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
	STA memconf		; damit internes RAM gelesen werden kann!
}
        LDA (gaddr),Y
gchange_op
        ORA bitmask,X
        STA (gaddr),Y
!ifdef ltc {
	LDA #mc_sim		; vollständige ROM-Simulation
	STA memconf		; wieder schnelles RAM ab $C000
}
        RTS

; mask a graphic location 

gmask
!ifdef ltc {
	XBA
	LDA #mc_epr		; Basic/Kernal-ROM-Simulation
	STA memconf		; damit internes RAM gelesen werden kann!
	XBA
}
gmask_flip
        EOR #$00
gmask_op
        ORA (gaddr),Y
        STA (gaddr),Y
!ifdef ltc {
	LDA #mc_sim		; vollständige ROM-Simulation
	STA memconf		; wieder schnelles RAM ab $C000
}
        RTS

}

gromcode_end

;-----------------------------------------------------------------

position
        LDA y
        LSR
        LSR
        LSR			; y/8
        TAY
        AND #%00000011		; (y/8) mod 4
        TAX
        LDA xl			; x low
        AND #%11111000		; clear bit 2-0
        CLC
        ADC ytabl,X		; addr low: y base + x part
        STA gaddr
        LDA xh			; addr high: x part
        ADC ytabh,Y		; 	+ y base
        STA gaddr+1
        LDA y			; vertical offset
        AND #%00000111		; y mod 8
        TAY
        LDA xl
        AND #%00000111		; x mod 8
        TAX			; horizonal offset
        RTS			; (bitmask)


;-----------------------------------------------------------------

; swap tupel xl,xh <-> xendl,xendh

swap_x_xend
        LDX xendl		; swap x, xend
        LDY xl
        STX xl
        STY xendl

        LDX xendh
        LDY xh
        STY xendh
        STX xh
	RTS


;-----------------------------------------------------------------

; line y up, x left, dx < dy (case 1)

line_up_steep
        JSR position		; x,y
loop_yup_xleft
        JSR gchange		; pixel

        CLC			; k += dx
        LDA kl
        ADC dxl			; dxh is 0, because dx < dy
        STA kl
        BCC +			; k >= 0 ->

++	SBC dy			; k -= dy (C=1)
        STA kl

 	DEX			; x--
        BPL +
        LDX #7			; wrap around
	SEC
        LDA gaddr		; x-8: gaddr -= 8
        SBC #8
        STA gaddr
        BCS +
        DEC gaddr+1

+	DEY			; y--
        BPL +++
        SEC			; y overflow
        LDA gaddr
        SBC #$40		; y-8: gaddr -= 40*8 ($140)
        STA gaddr
        LDA gaddr+1
	SBC #1
        STA gaddr+1
        LDY #7			; wrap around

+++	DEC cl			; until c=0
        BNE loop_yup_xleft
        JMP gexit


;-----------------------------------------------------------------

; line x left, y up, dx > dy (case 2)

line_up_flat
        JSR position		; x,y
	LDA cl			; counter adjustment for
	BEQ +			; prepare for dec-dec-counting
	INC ch
+
loop_xleft_yup
        JSR gchange		; pixel

        CLC			; k += dy
        LDA kl
        ADC dy
        STA kl
        BCC +			; k < 0
        INC kh
	BMI +			; k < 0

        SBC dxl			; k -= dx (A = kl, C=1)
        STA kl
        LDA kh
        SBC dxh		
        STA kh

        DEY			; y--
        BPL +
	SEC			; C=1 not always true (SBC above)
        LDA gaddr		; y overflow
        SBC #$40		; y-8: gaddr -= 40*8 ($140)
        STA gaddr
        LDA gaddr+1
	SBC #1
        STA gaddr+1
	LDY #7			; wrap around

+	DEX			; x--
        BPL +++
        LDX #7			; wrap around
	SEC
        LDA gaddr		; x-8: gaddr -= 8
        SBC #8
        STA gaddr
        BCS +++
        DEC gaddr+1
+++
	DEC cl			; c--
        BNE loop_xleft_yup
        DEC ch			; adjusted high which allows this
        BNE loop_xleft_yup

        JMP gexit



;-----------------------------------------------------------------

; line x left, y down, dx > dy (case 3)

line_down_flat
        JSR position		; x,y
	LDA cl			; counter adjustment for
	BEQ +			; prepare for dec-dec-counting
	INC ch
+
loop_xleft_ydown
        JSR gchange		; pixel

        CLC			; k += dy
        LDA kl
        ADC dy
        STA kl
        BCC +			; k < 0
        INC kh
	BMI +			; k < 0

        SBC dxl			; k -= dx (A = kl, C=1)
        STA kl
        LDA kh
        SBC dxh		
        STA kh

        INY			; y++
        CPY #8
        BNE +
	; C=1
        LDA gaddr		; y+8: gaddr += 40*8 ($140)
        ADC #$40-1		; C already set by CPY
        STA gaddr
        LDA gaddr+1
	ADC #1
        STA gaddr+1
        LDY #0			; wrap around

+	DEX			; x--
        BPL +++
        LDX #7			; wrap around
	SEC
        LDA gaddr		; x-8: gaddr -= 8
        SBC #8
        STA gaddr
        BCS +++
        DEC gaddr+1
+++
	DEC cl			; c--
	BNE loop_xleft_ydown
	DEC ch			; adjusted high which allows this
        BNE loop_xleft_ydown

        JMP gexit


;-----------------------------------------------------------------

; line y down, x right, dx < dy (case 4)

line_down_steep
        JSR position		; x,y
loop_ydown_xleft
        JSR gchange		; pixel

        CLC			; k += dx
        LDA kl
        ADC dxl			; dxh is 0, because dx < dy
        STA kl
        BCC +			; k >= 0 ->

	SBC dy			; k -= dy, C=1
        STA kl

 	DEX			; x--
        BPL +
        LDX #7			; wrap around
	SEC
        LDA gaddr		; x-8: gaddr -= 8
        SBC #8
        STA gaddr
        BCS +
        DEC gaddr+1

+	INY			; y++
        CPY #8			; y overflow?
        BNE +++
        LDA gaddr		; y+8: gaddr += 40*8 ($140)
        ADC #$40-1		; C already set by CPY
        STA gaddr
        LDA gaddr+1
	ADC #1
        STA gaddr+1
        LDY #0			; wrap around

+++	DEC cl			; c--
				; until c=0
        BNE loop_ydown_xleft
        JMP gexit


;-----------------------------------------------------------------

getcommaxy
        JSR b_getcomma		; check ","
getxy
        JSR b_getval		; get X coord. value
        JSR b_convint
        CMP #>xmax
	BCC gcxy_xok
        BEQ ++			; X = $1xx
        JSR range_error

++	CPY #<xmax		; check X low
        BCC +
        JSR range_error
+
gcxy_xok
        STY gpos		; temporary save X coord.
        STA gpos+1

        JSR b_getcomma8bit
				; get Y coord. value
        CPX #ymax
        BCC +
        JSR range_error
+
        LDY gpos		; restory X coord.
        LDA gpos+1
        RTS


;-----------------------------------------------------------------

para_hline_box
        JSR getxy		; get startpoint
        STX y
        STX savey		; save as cursor, too
        STA xh
        STY xl
	STA savexh
	STY savexl
        JSR b_getcomma		; get length
        JSR b_getval
        JSR b_convint
				; calculate end point
        TAX			; save length high byte
        TYA			; length low byte
        CLC
        ADC xl			; low xend = x+length
        STA xendl
	TAY
        TXA			; high
        ADC xh			; high xend = x+length
        STA xendh
	TAX

	CMP #>xmax		; endpoint outside?
	BCC +
	BNE +			; >$200 (512)
	TYA
	SBC #<xmax
+	RTS			; C=1 out of range, C=0 ok

;-----------------------------------------------------------------

hline
	JSR para_hline_box
	BCC +
	JSR range_error
				; XXX xend=xmax-1 ?
+
        STX savexh
        STY savexl		; also save as final cursor

	LDA #0			; default thickness 0 (means 1 pixel)
	STA ycount
	JSR chrgot		; last char. again
	BEQ +++			; command end? no optional param.
	JSR b_getcomma8bit
	TXA			; optional 8-bit parameter
	STA ycount		; hline thickness
	BEQ +++			; 0 means 1 pixel
	CLC
	ADC y			; end position for y coord.
	BCS +			; > 255
	CMP #ymax
	BCC +++
+				; C=1 from ADC or CMP before
	JSR range_error		; corrupts A
				; XXX ycount=ymax-y-1 ?
				; xend >= x
	BCS hl_noxswap		; always

hline_start
	JSR swap_x_xend		; xend < x, entry from line
	
hl_noxswap
				; xend > x
+++
	INC ycount		; count to 0
        JSR ginit		; map in graphic memory

        JSR position		; graphic position x,y

	LDA gaddr		; save position for vertical
	STA sgaddr
	LDA gaddr+1
	STA sgaddr+1
	STX xsave
	STY ysave

        LDA xendl
        AND #%00000111
        STA tmp2		; xend mod 8, mask index
        LDA xl
        AND #%11111000		; (xl div 8)*8
        STA tmpbits
        LDA xendl		; xend unmasked
        SEC
        SBC tmpbits		; finally: xend - (x div 8)*8 
        STA tmpbits
        LDA xendh
        SBC xh
        LSR			; / 8 ->  0-39
        LDA tmpbits		; only 1 highest bit
        ROR			; and 3 lower bits
        LSR
        LSR
        			; 8-pixel-blocks count
	STA hcount		; save for vertical extension
 
hl_vertloop
	TYA			; calculate max. Y in 8x8 block
	CLC
	ADC ycount
	CMP #8
	BCC +
	LDA #8
+	STA ylimit

        LDA maskleft,X		; starting mask
	STA tmpbits
	LDX hcount		; how many blocks

hl_nextblock
        DEX
hl_islastblock
        BMI hl_lastblock
				; leave loop if X<0
	LDY ysave
-	LDA tmpbits		; mask
	JSR gmask		; first with left end mask
	INY			; vertical down
	CPY ylimit		; in 8x8 box
	BNE -

        CLC			; gaddr += 8 (one block to right)
        LDA gaddr
        ADC #8
        STA gaddr
        BCC +
        INC gaddr+1

+	LDA #$FF		; following with full 8-pixel mask
	STA tmpbits
	BNE hl_nextblock	; always

hl_lastblock
        LDX tmp2		; xend mask index
        AND maskright,X		; current mask combined with mask right end
	STA tmpbits		; mask
	LDY ysave		; start position in 8x8 block
-	LDA tmpbits		; mask
	JSR gmask		; modify
	INY			; vertical down
	DEC ycount		; overall y counter
	CPY ylimit
	BNE -

	LDA ycount		; finished
	BNE +			; roll-over into 8x8 block below
        JMP gexit		; leave

+	CLC
	LDA sgaddr
	ADC #$40		; next 8-pixel row below
	STA sgaddr		; + $140 (320)
	STA gaddr
	LDA sgaddr+1
	ADC #$01
	STA sgaddr+1
	STA gaddr+1
	LDX xsave		; initial mask index
	LDY #0			; start on top of 8x8
	STY ysave
	BEQ hl_vertloop
;-----------------------------------------------------------------

vline
        JSR getxy		; get startpoint
        STA xh
        STA savexh		; save as cursor too
        STY xl
        STY savexl
        STX yend		; initial point is endpoint

        JSR b_getcomma8bit
				; get length
        CLC			; calculate end point
        TXA			; length
; DON'T-CHANGE: how long to go vertically (needed later)
;		DO NOT USE: tmpbits does not exist if called via vline_start!
;	STA tmpbits
        ADC yend		; length + initial point is startpoint
	BCS vline_iq		; > 255
        CMP #ymax		; outside?
	TAY			; keep startpoint
        BCC +
vline_iq
        JSR range_error		; corrupts A
				; XXX Y = ymax-1 ?
+	STY y			; startpoint
	STY savey		; set cursor y position
	CLC
	BCC +++			; skip following, because y, yend are already ordered

vline_start			; entry point from line command (only)
	LDA y			; order of y, yend is not defined
	CMP yend
	BCS vl_noyswap		; yend > y ->
	LDA y			; swap y, yend
	LDX yend
	STA yend
	STX y
vl_noyswap
				; startpoint is below the endpoint
+++	JSR ginit		; map in graphic memory

vl_start
        JSR position		; graphic position x,y
        LDA bitmask,X
        STA tmp2		; save mask
; DON'T-CHANGE: replace ...
        SEC
        LDA y			; startpoint is greater!
        SBC yend		; vertical length
        TAX
; DON'T-CHANGE: replacy by ... (already as parameter, from tmpbits)
;		DO NOT USE: tmpbits does not exist if called via vline_start!
;	LDX tmpbits
        INX			; +1 (exit on 0)
	SEC			; for subtraction, never changed!
vl_nextline
        LDA tmp2
        JSR gmask		; modify 
        DEY			; go up
        BPL +
        LDA gaddr		; C=1
	SBC #$40		; gaddr -= 320
        STA gaddr
        LDA gaddr+1
        SBC #$01
        STA gaddr+1
        LDY #7			; wrap y offset
+	DEX			; all vertical positions done?
        BNE vl_nextline
        JMP gexit		; leave


;-----------------------------------------------------------------

line
        JSR getxy		; get startpoint
        STY xl 
        STA xh
        STX y

        JSR getcommaxy		; get endpoint
line_start
        STY savexl		; save as cursor position too
        STY xendl
        STA savexh
        STA xendh
        STX savey
        STX yend

        LDY #$00		; initialize to 0
        STY ydir
        STY kl
        STY kh

        SEC
        LDA xl			; calculate dx
        SBC xendl
        STA dxl
        LDA xh
        SBC xendh
        STA dxh

        BCS li_xend_left
	; dx != 0
				; negate dx:
        TYA			; Y=A=0
        SEC			; dx = 0 - dx
        SBC dxl
        STA dxl
        TYA			; Y=A=0
        SBC dxh
        STA dxh
				; C=0 always, needed later
	jsr swap_x_xend
        LDX y			; swap y
        LDY yend
        STX yend
        STY y

        BCC li_x_different
				; C=0 always (from negation before)

li_xend_left
        			; A already contains dxh
        ORA dxl			; dx = 0?
        BNE li_x_different
        JMP vline_start		; vertical line case

li_x_different
        SEC			; calculate dy
        LDA yend
        SBC y
        BCS li_y_right		; yend >= y?
        EOR #$FF		; no, negate dy (two's complement)
        ADC #$01		; C=0
        STA ydir		; always not 0: flag y goes up

li_y_right
        STA dy
        BNE +
	LDA #0			; line thickness = 1
	STA ycount
        JMP hline_start		; horizontal line case
+
	; dx and dy is *always* !=0, otherwise hline or vline got called.

        LDA dxh			; dx > dy
        BNE line_flat		; yes -> flat
        LDA dy			; no -> steep
        TAX
        CMP dxl
        BCC line_flat

line_steep
        INX	
        STX cl			; c = dy+1
        LSR			; dy/2
	EOR #$FF		; one's complement
        STA kl			; k = -dy/2 -1

        JSR ginit		; map in graphic memory

        LDA ydir
        BNE +
        JMP line_down_steep	; y down, steep
+	JMP line_up_steep	; y up, steep

line_flat
        LDA dxh
        TAY
        LDX dxl
        INX
        BNE +
        INY
+	STX cl			; c = dx+1
        STY ch

        LSR			; dx/2 high
	EOR #$FF		; one's complement
        STA kh
        LDA dxl
        ROR			; dx/2 low
	EOR #$FF		; one's complement
        STA kl			; k = -dx/2 - 1

        JSR ginit		; map in graphic memory

        LDA ydir	
        BNE +
        JMP line_down_flat	; y down, flat
+	JMP line_up_flat	; y up, flat

;-----------------------------------------------------------------

plot
        JSR getxy		; get parameter
        STA xh			; save x/y
        STY xl
        STX y
        STA savexh		; and store as cursor
        STY savexl
        STX savey

plot_start
        JSR position		; calculate graphical address

        LDA prozport
        AND #%11111101		; Kernal ROM disable
        SEI			
        STA prozport

        JSR gchange		; change graphical data

        LDA prozport
        ORA #%00000010		; kernal ROM enable
        STA prozport
        CLI
        RTS

;-----------------------------------------------------------------

move
        JSR getxy		; get parameter
        STA savexh		; just save as cursor
        STY savexl
        STX savey
        RTS


;-----------------------------------------------------------------

; never touches X, Y, C-flag
; on exit: A corrupted, Z=0

range_error
	LDA savemo
	AND #$F0
	BNE +
				; error mode 3: abort command (silent)
	PLA			; cleanup JSR
	PLA			; highbyte of return address >0

-	RTS			; error mode 5: back to command
				; to handle value correction
				; Z=0
+	AND #$20		; mode 5?
	BNE -			; exit with Z=0
	PLA			; error mode 4: terminate with error
	PLA			; cleanup JSR
setmode_error
	JMP b_illquant		; throw error message

;-----------------------------------------------------------------

setmode
        JSR b_get8bit
        CPX #3
        BCC +			; less then 3, modification mode
	CPX #6
	BCS setmode_error	; out of range
				; error mode
	TXA
	SBC #2			; C=0, therefore -3
	ASL			; 0-2 -> 16,32 or 48
	ASL			; shift to upper nibble
	ASL
	ASL
				; put A's bit 4-7 into savemo
	EOR savemo		; ********
	AND #%11110000		; ****0000
	EOR savemo		; AAAAmmmm
	STA savemo		; 
	RTS

+	TXA
	EOR savemo		; put A's bit 0-3 into savemo
	AND #%00001111
	EOR savemo
	STA savemo
setmode_enter
	CPX #$01
        BCS set_or_toggle

modereset
        LDA #>(nbitmask)
        STA gchange_op+2
        LDA #<(nbitmask)
        STA gchange_op+1
        LDA #$3D		; opcode AND abs,X
        STA gchange_op
        LDA #$31		; opcode AND (zp),Y
        STA gmask_op
        LDA #$FF		; mask, EOR $#FF, inverting
        STA gmask_flip+1
        RTS

set_or_toggle
        BNE modetoggle
modeset
        LDA #>(bitmask)
        STA gchange_op+2
        LDA #<(bitmask)
        STA gchange_op+1
        LDA #$1D		; opcode OR abs,X
        STA gchange_op
        LDA #$11		; opcode OR (zp),Y
        STA gmask_op
        LDA #$00		; mask, EOR #$00, not inverting
        STA gmask_flip+1
        RTS

modetoggle
        LDA #>(bitmask)
        STA gchange_op+2
        LDA #<(bitmask)
        STA gchange_op+1
        LDA #$5D		; opcode EOR abs,X
        STA gchange_op
        LDA #$51		; opcode EOR (zp),Y
        STA gmask_op
        LDA #$00		; mask, EOR #$00, not inverting
        STA gmask_flip+1
        RTS


;-----------------------------------------------------------------
; get current x cursor position

getposx
	LDY savexl
	LDA savexh
	JSR b_word2fac
	JMP chrget		; last position of expression (function name)

;-----------------------------------------------------------------
; get current y cursor position

getposy
	LDY savey
	JSR b_byte2fac
	JMP chrget		; last position of expression (function name)

;-----------------------------------------------------------------

; get pixel (check if pixel set)
; not used

get
	JSR chrget		; advance past function name
	JSR b_chkparl		; "("?
        JSR getxy		; get X,Y values
        STA xh
        STY xl
        STX y
	JSR chrgot
	JSR b_chkparr		; ")"?
	

        JSR position		; calculate graphic address/position

        LDA prozport
	AND #%11111101		; Kernal ROM disable
        SEI
        STA prozport

        LDA (gaddr),Y
        AND bitmask,X		; mask position
        TAY
        LDA prozport
	ORA #%00000010		; kernal ROM enable
        STA prozport
        CLI
	TYA
	BEQ +
	LDY #1			; <> 0 -> always return 1
+	JMP b_byte2fac		; still on expr.'s last character

;-----------------------------------------------------------------

relto_cont
				; continue
	JSR chrget		; skip TO token
relto
        JSR b_getval		; get X offset (+/-)
	LDA facexp		; FAC exponent
	CMP #$90		; more than 16 bit
	BCS relto_error		; illegal quantity
        JSR b_fac2int		; to signed integer

        CLC
        LDA facintl
        ADC savexl
        STA xendl
        LDA facinth
        ADC savexh
        STA xendh		; xend = savex+facint

        JSR b_getcomma		; get Y offset (+/-)
        JSR b_getval
        LDA facexp		; FAC exponent
        CMP #$90		; more than 16 bit
        BCS relto_error		; illegal quantity
        JSR b_fac2int		; to signed integer
        CLC
        LDA facintl
        ADC savey
        STA yend		; yend = savey+facint

        LDA xendh		; check end coord. x
        CMP #>xmax
        BCC rt_xok
        BEQ +
relto_error
        JSR range_error
+	LDA xendl
        CMP #<xmax
        BCC +
        JSR range_error
+
rt_xok
        LDA yend		; check end coord. y
        CMP #ymax
        BCC +
        JSR range_error
+
        LDA savexl
        STA xl
        LDA savexh
        STA xh
        LDA savey
        STA y
        LDY xendl
        LDA xendh
        LDX yend		; xend/yend = cursor + x/y

        JSR line_start		; draw line x/y to xend/yend

	JSR chrgot
	BNE +
	RTS
+	CMP #t_to		; TO keyword?
	BEQ relto_cont
        JMP b_syntaxerror	; throw error (unknown command)

;-----------------------------------------------------------------

char
        JSR b_get8bit		; get char. position x 0-39
        CPX #40	
        BCC +
char_error
        JMP b_illquant
+	STX gpos		; save x coord.
        JSR b_getcomma8bit
				; get char. position y 0-24
        CPX #25
        BCS char_error
        STX gpos+1		; save y coord.

        JSR b_getcomma		; get string
        JSR b_getexpr
        JSR b_stringval		 ; string address in str
        PHA			; string length
        LDX gpos+1		; y coord. for char. position
        TXA
        AND #$03		; mask 2 bits
        TAY			; table index
        LDA #$00
        STA gpos+1		; x high
        LDA gpos		; saved x: multiply by 8
        ASL
        ASL
        ASL
        ROL gpos+1		; overflow to high byte
        ADC ytabl,Y
        STA gaddr
        LDA gpos+1		; x high
        ADC ytabh,X
        STA gaddr+1
        PLA			; string length
        LDY #$00		; string index
        TAX			; length
        INX			; prepare as counter
char_loop
        DEX
        BEQ char_exit
        LDA (str),Y		; read string
        JSR char_display
        INY
        BNE char_loop
char_exit
        RTS

char_display
        STA z_tmp		; character (lastkey, temporary reused)
        TXA			; save register X+Y
        PHA
        TYA
        PHA
        LDA z_tmp		; get saved character
        BMI char_inverse

char_normal
        CMP #$20		; control character?
        BCC char_disp_leave
        CMP #$60
        BCC +
        AND #%11011111		; $60-$7F -> $40-$5F
        BNE char_hires
+	AND #%00111111		; $40-$5F -> $00-$1F
	BNE char_hires
	BEQ char_hires

char_inverse
        AND #%01111111		; mask bit 7
        CMP #%01111111		; was 255? (pi)
        BNE +
        LDA #$5E		; screen code for pi
+	CMP #$20		; control character?
        BCC char_disp_leave
				; yes, skip
        ORA #%01000000		; $A0-$BF -> $60-$7F
				; $C0-$FF -> $40-$7F
				; OPT: BNE char_hires
				; OPT: char_normal
char_hires
        LDX z_reverseflag
        BEQ +
        ORA #%10000000		; invert char.
+	TAX			; save char. for later
        LDA prozport		; save prozport state
	PHA
        LDA #%00100001		; char. rom, no basic and kernal rom
        SEI
        STA prozport		; char. rom base = $D000
        LDA #($D0 >> 3)		; $D0/8   1101 0000 -> 0001 1010
        STA gpos+1		; 
        TXA			; char. code
        ASL			; *8
        ROL gpos+1
        ASL
        ROL gpos+1
        ASL
        ROL gpos+1
        STA gpos		; addr. in char. rom for char.

        LDY #$07		; 8 hires lines
char_line
        LDA (gpos),Y		; read character line
        JSR gmask		; write to hires screen
        DEY
        BPL char_line

	PLA
        STA prozport
        CLI

        CLC			; step char position to left
        LDA gaddr		; ( +8 )
        ADC #$08
        STA gaddr
        BCC +
        INC gaddr+1
+
char_disp_leave
	PLA			; pass written character back
        TAY			; restore saved registers
        PLA
        TAX
-       RTS


;-----------------------------------------------------------------

to_cont
				; continue
	JSR chrget		; skip TO token
to
        LDA savexl
        STA xl
        LDA savexh
        STA xh
        LDA savey
        STA y
        JSR getxy
        JSR line_start
	JSR chrgot
	BEQ -
	CMP #t_to		; TO keyword?
	BEQ to_cont
        JMP b_syntaxerror	; throw error (unknown command)

;-----------------------------------------------------------------

box
        JSR para_hline_box
	BCC +
	JSR range_error
				; XXX xend=xmax-1 ?
+
	JSR b_getcomma8bit
	TXA			; optional 8-bit parameter
				; height
	BEQ +++			; 0 means 1, box is just a line
	CLC
	ADC y			; end position for y coord.
	BCS +			; > 255
	CMP #ymax
	BCC +++
+				; C=1 from ADC or CMP before
	JSR range_error		; corrupts A
				; XXX ycount=ymax-y-1 ?
				; xend >= x
+++	PHA			; yend
	LDA #0
	STA ycount		; line thickness 1
	JSR hl_noxswap		; upper horizontal line

				; right vertical line
	PLA			; if 0, heigth is 1
	BNE +			; no 
	RTS			; exit, if box is degenerated (line)
+	LDX y			; start point at higher values
	STA y
	STX yend
	LDA xendl
	STA xl
	LDA xendh
	STA xh
	JSR vl_noyswap		; xend,yend -> xend,y
				; lower horizontal line
	LDA savexl
	STA xl
	LDA savexh
	STA xh			; xend already set
	JSR hl_noxswap		; x,yend -> xend,yend
				; left vertical line
	JMP vl_noyswap		; x,y -> x,xend

;-----------------------------------------------------------------

fill
	JSR getxy
	STA xh			; save x/y
	STY xl
	STX y
	STA savexh		; and store as cursor
	STY savexl
	STX savey
        
        LDA basaryend		; initialize fill stack pointer
	SEC
	SBC #fesize		; one element below
	STA fstack		; use space between basic arrays
	LDA basaryend+1		; and string heap bottom
	SBC #0			; take borrow
	STA fstack+1

	JSR position		; graphic position in (gaddr)+Y, bit X
	LDA bitmask,X		; start pixel
	STA tmpmask		; initial single pixel mask

	LDA xh			; setup 8x8 block index (x8)
	LSR			; high bit into C
	LDA xl
	ROR			; take high bit
	LSR
	LSR			; finally divide by 8
	STA x8			; = index of 8x8 block in bitmap

	JSR ginit		; map in bitmap memory

	; set fmode (from mode)
	LDA savemo
	AND #1			; mode = 0 -> invertmask: $FF
	SEC			; mode = 1 -> invertmask: $00
	SBC #1			; mode = 2 -> same as mode=0
	STA fmode		; mode set or reset

        ; test start pixel
        EOR (gaddr),Y           ; bitmap according to mode
        STA tmpbits             ; mask bits
        BIT tmpmask             ; check single bit, and preserve A
        BEQ +			; not set, bit position already in X
        JMP gexit		; set, we are finished early

f_line				; start fill in the mid of a line ...

	; Get the index of the first leftmost unset pixel inside tmpmask.
	; Just the single leftmost gap is filled, others are processed later
	; from the element left on the stack.
	; Normally comming from process_stack.

	; set bits outside mask to 1
	LDA tmpmask		; 00011100
	EOR #$ff		; 11100011
	ORA tmpbits		; 00101010 merge with graphic pixel data
				; 11101011 pixel outside tmpmask now set! 
	LDX #$ff		; pixel gap search: first one from left
-	INX
	ASL			; counting from left
	BCS -			; loop if pixel is set
				; bit number of the leftmost unset pixel in X

	; in: pixels in tmpbits, bit index in X
+	LDA #0			; initialize continuation flag
	STA fcont		; for line above und below
	LDA tmpbits		; 01000010 graphic pixel data
	AND maskright,X		; 1111X000 clear right from starting point
	BNE left_border		; 01000000 left border remains if any
	; open to left, continue left
	LDA #$ff		; no left border, next block to left
	STA tmpmask		; initial mask full pixel line
stepleft8
	LDX x8 			; 8x8 block position
	BEQ left_end		; hit left screen border, X=0 -> tmpmask=$FF!
	DEC x8			; count step 8x8 block to left

	SEC 			; graphic address to to next pixel line/block
	LDA gaddr
	SBC #8			; graphic address -8 -> next block left
	BCS +
	DEC gaddr+1		; carry to high byte 
+	STA gaddr

	; y left unchanged
	LDA (gaddr),Y		; real graphic pixel data from bitmap
	EOR fmode		; set/reset mode
	BEQ stepleft8		; step block left if empty (no border)

left_border

	; Find first set bit from start to left (border)
	; Get the pixel position of the first set pixel from the right.

	; bit 76543210  -> index 12345678
	; input    index   maskleft0
	; 00000000 -> 0 -> $FF 
	; 10000000 -> 1 -> $7F
	; X1000000 -> 2 -> $3F
	; XX100000 -> 3 -> $1F
	; XXX10000 -> 4 -> $0F
	; XXXX1000 -> 5 -> $07
	; XXXXX100 -> 6 -> $03
	; XXXXXX10 -> 7 -> $01
	; XXXXXXX1 -> 8 -> $00
	; Speed consideration: for results from X 0 to 4 it is faster than
	; a table-driven approach.
	; A is never 0!
	LDX #0
-	INX
	ASL			; shift to left
	BNE -			; until byte is empty

left_end
	LDA maskleft0,X		; get a mask from the left border to right
				; 00X11111
	BNE right_start		; start to walk and fill towards the right border
				; empty mask immediate continue to right
stepright8
	INC x8			; step right a block
	LDA x8
	CMP #40			; beyond last horizontal block?
	BCS process_stack	; done if right screen border
	; C = 0
	LDA gaddr		; advance to block right
	ADC #8			; gaddr = gaddr + 8
	BCC +
	INC gaddr+1		; carry to high byte
+	STA gaddr
	LDA #$ff		; force "all pixels" mask, because intial
				; mask might be a partial one
right_start
	STA tmpmask		; 00111111 store all/distinct mask
	LDA (gaddr),Y		; 01000010 pixel data
	EOR fmode		; set/reset mode
	AND tmpmask		; 00000010 mask out left border and beyond
	BEQ fill_to_right	; empty -> finally start to fill

	; Get the pixel position of the first set pixel from the left, to
	; find the right border:

	; bit 76543210  -> index 01234567  -> index
	; input    index   maskright0
	; 00000000 -> 8 -> $FF
	; 00000001 -> 7 -> $FE
	; 0000001X -> 6 -> $FC
	; 000001XX -> 5 -> $F8
	; 00001XXX -> 4 -> $F0
	; 0001XXXX -> 3 -> $E0
	; 001XXXXX -> 2 -> $C0
	; 01XXXXXX -> 1 -> $80
	; 1XXXXXXX -> 0 -> $00
	; Speed consideration: for results of X from 4 to 8 it is faster than
	; a table-driven approach.
	; A is never 0!
	LDX #8
-	DEX
	LSR			; shift to right
	BNE -			; until byte is empty

	; search right border
	LDA maskright0,X	; 11111X00 mask out the right part
	AND tmpmask		; 00111111 intersect with mask from left
				; 00111100
	BEQ process_stack	; done if bit 7 (leftmost) is set
				; leading to 0 mask (fill_check wont't
				; handle this special case)
	STA tmpmask		; 00111100 save intersected masks from left and right
				; continue to fill to right ...
fill_to_right			; fill loop towards right border
	LDA tmpmask		; fill mask
				; assert:    (bitmap & tempmask) == 0
				;         || (bitmap & tempmask) == tempmask
	EOR (gaddr),Y		; set/reset to fill
	STA (gaddr),Y		; into bitmap - the actual fill action!
	
check_above
	ASL fcont		; bit 0 to bit 1 position to check (above)
				; c = 0!
	STY ysave		; to be restored later
	LDA gaddr		; current graphic position
	STA caddr		; check position
	LDX gaddr+1
	DEY			; line above
	BPL +			; leaving 8x8 block?
	; c=0 (asl fcont)
	SBC #$40-1		; block above:
	STA caddr		; caddr = gaddr - $140

	DEX			; subtract high byte
	BCS ++			; borrow from low byte
	DEX			; subtract borrow
++	CPX #>gram		; still graphic ram?

	BCC skip_above
	LDY #7			; last line in block in new block
+	STX caddr+1		; shared store
	JSR fill_check
skip_above

check_below
	LSR fcont		; bit 2 back to bit 1 position to check (below)
	LDA gaddr		; current graphic position
	STA caddr		; check position
	LDX gaddr+1
	LDY ysave		; restore original y position
	INY			; line below
	CPY #8			; crossing 8x8 block?
	BCC +			; less then 8
	; c=1 (cpy)
	ADC #$40-1		; block below: accu has gaddr
	STA caddr		; caddr = gaddr + $140

;	INX
;	BCC ++
;	INX			; add carry
;++	BEQ skip_below		; skip to $100xx, out of range
;	CPX #>(gram+8000)
;	BCC +++			; below GRAM end
;	BNE skip_below		; high byte above, out of range
;	CMP #<(gram+8000)	; low byte check if in last page of GRAM
	; 12 T if GRAM not in last page!

	TAY			; for compare later
	TXA			; gaddr high
	ADC #$01
	TAX
	BCS skip_below		; > $10000  -> skip
	CPY #<(gram+8000)	; > gram end: $e000(=gram) + $2000 ?
	SBC #>(gram+8000)
	; 16 T
	BCS skip_below		; greater, so skip
+++	LDY #0			; first line in block
+	STX caddr+1		; shared store
	JSR fill_check
skip_below

	LDY ysave		; restore original y position
	LSR tmpmask
				; bit 0 to carry, open to right, continue?
	BCS stepright8		; to next block if open
; long branch version
;	BCC +			; not open, finished
;	JMP stepright8		; to next block if open
;+


; get next location to fill from stack (if any)
;	in: fstack, fmode
; 	out: fstack, gaddr, tmpmask, tmpbits, x8, Y
; 	destroys: A

process_stack
	LDA fstack		; stack empty?
	CMP basaryend
	LDA fstack+1
	SBC basaryend+1
	BCS +			; fstack >= basaryend -> not empty
	JMP gexit		; empty, we are finished

	; top of stack: fetched multiple times until mask is completly filled!
+	LDY #fesize-1		; element's last component
!ifndef opt_space {
	LDA (fstack),Y
	STA x8			; 8x8 block position
	DEY
}
	LDA (fstack),Y
	STA tmpmask		; pixel mask
	DEY
	LDA (fstack),Y
	STA gaddr+1		; graphic addr high byte
	DEY
	LDA (fstack),Y		; graphic addr low byte combined with y-line
	TAX			; needed twice
	AND #%11111000		; split off address
	STA gaddr
!ifdef opt_space {
	ORA #%00000100		; end bit marker (if 0 all bits are shifted)
	STA x8			; low byte without least significant 3 bits
				; x8 temporary reused. Calculated later ...
}
	TXA
	AND #%00000111		; split off y-line
	TAY

	LDA (gaddr),Y		; get pixels
	EOR fmode		; according to set/reset
	STA tmpbits		; keep it for later
	AND tmpmask		; focus on masked pixels
	PHP			; save Z flag
	BEQ pop_stack		; all bits unset, remove from stack, because
				; it could be filled in one step!
	CMP tmpmask		; all gaps filled?
	BNE +++			; still some gaps (splitted pixels), leave on stack
	; all gaps filled, next on stack 
pop_stack
	SEC	
	LDA fstack		; remove entry from stack
	SBC #fesize		; entry size
	STA fstack
	BCS +
	DEC fstack+1
+	PLP			; all bits to fill empty?
	BNE process_stack	; all masked bits are set, next stack element
				; all bits unset,
	!by $24			; = bit $ll, skip next statement (1 byte)
				; stack already cleaned up
+++	PLP			; notstack cleanup

!ifdef opt_space {
	; Calculate the 8x8 block index from the the graphic address.
	; Delayed, only if popped position is not already filled ...
	; ((addr & 0x1fff) >> 3) % 40
	; Takes 4 iterations. Register X, Y left untouched, 
	; x8 contains gaddr low and has bit 2 set as end marker, bit 0, 1 is cleared.
	; (312/8) % 40  -> 39
	; 1 00111.000 : 101000
	LDA gaddr+1		; divident high byte, mask out upper 3 bits
	AND #$1f		; range 0 to 1f3f
	ASL x8			; $1f always < 40
-	ROL			; shift into high byte, carry from low byte
	CMP #40			; modulo 40
	BCC +			; dividend less divisor
	SBC #40			; greater or equal divisor, c=1
				; nothing done to keep the quotient
+	ASL x8			; shift low byte divident
	BNE -			; if end-marker bit shifted out -> 0
	STA x8			; modulo in accu, stored to final location
}

	JMP f_line		; long (to far away) jump to fill line start


; Check upper or lower fill path
;	in: caddr, fmode, tmpmask, fcont, fstack(, x8)
;	out: fcont, fstack
;	destroys: A,X,Y

fill_check
	LDA (caddr),Y
	EOR fmode		; pixel data
	TAX			; save for later
	AND tmpmask		; mask to fill
	BEQ fc_cleared		; all masked pixels cleared?
	CMP tmpmask		; check for gaps
	BEQ fc_exit		; all gaps filled, finished
				; if not so, some pixels still set
	LDA tmpmask
fc_checkstart			; no continuation, init flag based on
				; rightmost pixel:
	LSR			; mask bit 0 to carry
	BCC fc_nocont		; maskbit empty?
	TXA			; pixel data
	LSR			; pixel bit 0 to carry
	BCS fc_nocont		; bit 0 set
				; -> mask is 1 and pixel 0
fc_cont
	LDA fcont		; set flag for continuation
	ORA #%00000010		; mark in bit 1, store it, make a push
	BNE push_to_stack	; always non zero

fc_cleared
	LDA tmpmask		; pixel & mask -> 0
;	BEQ fc_exit		; but if mask=0 we are done (never push!)
				; the caller asserts that this never happens
	CMP #$ff		; full pixel line mask and all pixels cleared
	BNE fc_checkstart	; maybe a continuation ...
				; 8 pixel line empty
	LDA fcont		; continued gap?
	AND #%00000010		; check bit 2
	BEQ fc_cont		; new gap, start it and push on stack
fc_exit	RTS			; gap continued and already on stack, leave

fc_nocont
	LDA fcont		; clear continuation flag
	AND #%11111101		; clear bit 2

push_to_stack
	STA fcont
	CLC			; fstack points to top of stack
	LDA fstack		; to next free stack element
	ADC #fesize		; entry size
	STA fstack
	BCC +
	INC fstack+1
+
	LDA strbot+1		; check stack space
	CMP fstack+1
	BCS ++			; strbot MSB >= fstack MSB, need more to check
				; strbot MSB < fstack MSB
out_of_memory			
	JSR gexit
	LDX #$10		; out of memory error
	JMP (v_baserr)		; basic error handler
++	BNE fc_put		; <> -> (strbot > fstack)
	LDA fstack		; MSB equal, check LSB
	CMP strbot
	BCS out_of_memory	; fstack collides with string heap!

fc_put
	TYA			; y-line (value 0-7) merged with
	ORA caddr		; graphic address low (bit 0-2 always empty)
	LDY #0			; stack structure index, on next free element
	STA (fstack),Y
	INY
	LDA caddr+1
	STA (fstack),Y		; graphic address high
	INY
	LDA tmpmask
	STA (fstack),Y
!ifndef opt_space {
	INY
	LDA x8			; 8x8 block position
	STA (fstack),Y
}
	
	RTS
	





;-----------------------------------------------------------------

unnew

	LDA bassta
	STA str
	LDA bassta+1
	STA str+1
	LDY #1
	TYA
	STA (str),y		; != 0

	JSR b_rechain		; starting from bassta
				; result in (str)
	CLC			; str+1 -> new basic end
	LDY str+1
	LDA str
	ADC #2
	STA basend
	BCC +
	INY
+	STY basend+1
	JMP b_clr		; perform CLR


;-----------------------------------------------------------------
graext_end
