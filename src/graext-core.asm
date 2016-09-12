;  **** gra-ext core ****
;
; 2015-10-05 johann e. klasek, johann at klasek at
;
; revisions:
;	2016-05-20 v 1.22
;	2016-05-16 v 1.21
;	2016-02-23 v 1.20
;	2016-01-15 v 1.19
;	1992-12-28 v 1.18
;	1986-03-24 v 1.17
;	1985       v 0.00 - 1.16
;
; the original source has been lost.
; development has based on the implemention
; done on a forth-64 written with its forth assembler.
; the code has been pulled out from there and enriched
; with some glue code to get a basic extension.

; command dispatcher style JMP/RTS
;	(if defined)
;command_rts_style=1

; error handling 
;	(if defined)
;no_error=1

; basic interpreter registers, addresses and entry points

str     = $22		; string address
ijmp    = $55		; address of JMP (addr)
chrget  = $73		; basic charget routine
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

b_interpreter =$A7AE	; interpreter loop
b_execstatement =$A7E7	; process statement
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
b_convint = $B7F7	; convert FAC to unsigned integer, return Y/A and $14/$15
b_fac2int = $BC9B	; convert FAC to integer
b_stringval = $B6A3	; take epression as string $22/$23 (str)

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

tmp1	= $95		; =kl, temp. var. in context horiz. lines
tmp2	= $96		; =kh, temp. var. in context horiz. lines

dxl	= $AB		; x delta, low+high
dxh	= $A7

dy	= $A9		; y delta

ydir	= $A8		; y direction: 0 | !=0 ... down | up

cl	= $A3		; dot count, low+high
ch	= $A4

gaddr	= $A5		; graphic address

gpos	= $FB		; in graphic position

gcol	= $FD		; graphic color, in "graphic on" context only


; static ram areas

savexl	= $0334		; the graphic cursor: x low 
savexh	= savexl+1	; the graphic cursor: x high
savey	= savexh+1	; the graphic cursor: y
savemo	= savey+1	; the graphic mode
saveverr = savemo+1	; original v_baserr
savevstp = saveverr+2	; original v_basstp

gramcode = $03ed	; real place for gchange and gmask routines,
			; they take 15 bytes

;
; initialize extension

init
        LDA #<(parse)	; basic interpreter parser hook
        STA v_bascmd
        LDA #>(parse)
        STA v_bascmd+1

        LDA v_basstp
	STA savevstp
        LDA #<(stop)	; basic interpreter stop hook
        STA v_basstp
        LDA v_basstp+1
	STA savevstp+1
        LDA #>(stop)
        STA v_basstp+1

        LDA v_baserr
	STA saveverr
        LDA #<(error)	; basic interpreter error hook
        STA v_baserr
        LDA v_baserr+1
	STA saveverr+1
        LDA #>(error)
        STA v_baserr+1

	LDX #0		; set graphic cursor to (0,0)
	STX savexl
	STX savexh
	STX savey
	INX
	STX savemo	; set mode 1
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

; start parsing an extension command ...

parse
        JSR chrget			; next char.
	PHP
        CMP #'&'			; command prefix
        BEQ newcmd
        PLP
        JMP b_execstatement
newcmd
	PLP
        JSR chrget			; command character

        LDY #(cmdsend-cmds)		; map character to
					; command address ...
checknextcmd
        DEY
	BEQ parse_error
        CMP cmds,Y
        BNE checknextcmd		; try next
        DEY				; found
        TYA
        ASL				; *2
        TAY
!ifndef command_rts_tyle {
	!set co=0			; command offset in jump table
        LDA cmdaddr+1,Y                 ; high byte from table
        STA ijmp+1
        LDA cmdaddr,Y                   ; low byte from table
        STA ijmp
        JSR chrget			; read next byte in basic text
        JSR ijmp-1                      ; go to command by JMP (addr)
        JMP b_interpreter		; continue parsing
} else {
	!set co=1			; command offset in jump table
	LDA #>(b_interpreter-1)		; return to interpreter
	PHA
	LDA #<(b_interpreter-1)
	PHA				
        LDA cmdaddr+1,Y			; command address (RTS style)
        PHA				; high byte on stack
        LDA cmdaddr,Y			; command address (RTS style)
        PHA				; low byte on stack
        JMP chrget			; read next byte in basic text 
					; and RTS to command
}
parse_error
        JMP b_syntaxerror		; throw error (unknown command)

;-----------------------------------------------------------------

; the most commonly used command placed at the end ...

cmds	!text " GCSMRTVHLP"		; first char. is a dummy
cmdsend

cmdaddr
        !word graphic-co,char-co,setmode-co,move-co,relto-co
        !word to-co,vline-co,hline-co,line-co,plot-co

author	!text 147,"GRA-EXT V1.22 1986,2016 JOHANN@KLASEK.AT",0

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

maskleft !byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01

maskright !byte $80,$c0,$e0,$f0,$f8,$fc,$fe,$ff


;-----------------------------------------------------------------

graphic
        JSR b_get8bit
        CPX #$00
        BNE gra_other
gra0			; &G 0
gra_off
        LDA #$C7	; Bit 1,0: %11, 3: Bank 0: $0000-$3FFF, 0-16383 (Standard)
        STA cia_pra
        LDA #((1 <<4) + (2 <<1) + 1)
			; Screen addr=VIC_bank+$400*1, char addr= $800*2, 1
			; char addr $1000/4096 = char. ROM
        STA vic_mcr	; VIC memory control
        LDA vic_cr	; VIC control register
        AND #%11011111	; Hires mode off
        STA vic_cr
        RTS

gra_other
        CPX #$01
	BEQ gra1
	CPX #$02
        BEQ gra2
	CPX #$04
        BEQ gra_clear	; &G 4 (erase only, leave mode)
	CPX #$03	; &G 3 (graphic on)
	BEQ gra_on
        JMP b_illquant	; parameter illegal
	
gra1			; &G 1
	JSR gra_clear

gra2
        JSR b_getcomma8bit
        TXA		; foreground color
        ASL		; upper nibble
        ASL
        ASL
        ASL
        STA gcol
        JSR b_getcomma8bit
        TXA		; background color
        AND #$0F
        ORA gcol
        LDY #$00
cram_loop
        STA cram,Y	; fill color RAM
        STA cram+$100,Y
        STA cram+$200,Y
        STA cram+$300-24,Y
        INY
        BNE cram_loop

gra_on
	JSR gra_setupcode

        LDA #$C4	; Bit 1,0: %00, 0: Bank 3: $C000-$FFFF, 49152-65535
        STA cia_pra
        LDA #((3 << 4) + %1000)	; cram: VIC_bank+$400*3, Hires upper half
        STA vic_mcr	; VIC memory control
        LDA vic_cr	; VIC control register
        ORA #%00100000	; Bit 5 = 1: Hires on
        STA vic_cr
        RTS

gra_clear
        LDX #$20	; Pages (8 KByte)
        LDA #>gram
        STA gpos+1
        LDY #$00
        STY gpos
        TYA
gra_fill
        STA (gpos),Y	; Loop unroll
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
        ORA #%00000010	; kernal ROM enable
        STA prozport
        CLI		; allow interrupts
        RTS

;-----------------------------------------------------------------

ginit
        LDA prozport
        AND #%11111101	; Kernal ROM disable
        SEI		; disable interrupts
        STA prozport
        RTS

;-----------------------------------------------------------------

; These are selfmodified routines, which has to placed into RAM
; (on every graphic "on")
; Code gromcode to gromcode_end-1 is relocated to gramcode

gromcode

!pseudopc gramcode {

; change a graphic location

gchange
        LDA (gaddr),Y
gchange_op
        ORA bitmask,X
        STA (gaddr),Y
        RTS

; mask a graphic location 

gmask
gmask_flip
        EOR #$00
gmask_op
        ORA (gaddr),Y
        STA (gaddr),Y
        RTS

}

gromcode_end

;-----------------------------------------------------------------

position
        LDA y
        LSR
        LSR
        LSR		; y/8
        TAY
        AND #%00000011	; (y/8) mod 4
        TAX
        LDA xl		; x low
        AND #%11111000	; clear bit 2-0
        CLC
        ADC ytabl,X	; addr low: y base + x part
        STA gaddr
        LDA xh		; addr high: x part
        ADC ytabh,Y	; 	+ y base
        STA gaddr+1
        LDA y		; vertical offset
        AND #%00000111	; y mod 8
        TAY
        LDA xl
        AND #%00000111	; x mod 8
        TAX		; horizonal offset
        RTS		; (bitmask)


;-----------------------------------------------------------------

; line y up, x right, dx < dy (case 1)

line_up_steep
        JSR position	; x,y
loop_yup_xright
        JSR gchange	; pixel

        CLC		; k += dx
        LDA kl
        ADC dxl		; dxh is 0, because dx < dy
        STA kl
        BCS ++		; k > 255

        CMP dy
        BCC +		; k >= dy ->

++	SBC dy		; k -= dy
        STA kl

        INX		; x++
        CPX #8
        BNE +
	; C=1
        LDX #0		; x overflow, wrap around
        LDA gaddr	; x+8: gaddr += 8
        ADC #8-1	; C already set by CPX
        STA gaddr
        BCC +
        INC gaddr+1

+	DEY		; y--
        BPL +++
        SEC		; y overflow
        LDA gaddr
        SBC #$40	; y-8: gaddr -= 40*8 ($140)
        STA gaddr
        LDA gaddr+1
	SBC #1
        STA gaddr+1
        LDY #7		; wrap around

+++	DEC cl		; until c=0
        BNE loop_yup_xright
        JMP gexit


;-----------------------------------------------------------------

; line x right, y up, dx > dy (case 2)

line_up_flat
        JSR position	; x,y
	LDA cl		; counter adjustment for
	BEQ +		; dec-dec-counting
	INC ch
+
loop_xright_yup
        JSR gchange	; pixel

        CLC		; k += dy
        LDA kl
        ADC dy
        STA kl
        BCC ++
        INC kh

++	CMP dxl		; k > dx?
        LDA kh
        SBC dxh
        BCC +

        STA kh		; k -= dx
        LDA kl
        SBC dxl
        STA kl

        DEY		; y--
        BPL +
	SEC		; C=1 not always true (SBC above)
        LDA gaddr	; y overflow
        SBC #$40	; y-8: gaddr -= 40*8 ($140)
        STA gaddr
        LDA gaddr+1
	SBC #1
        STA gaddr+1
	LDY #7		; wrap around

+	INX		; x++
        CPX #8		; x overflow?
        BNE ++
	; C=1
        LDX #0		; wrap around
        LDA gaddr	; x+8: gaddr += 8
        ADC #8-1	; C already set by CPX
        STA gaddr
        BCC ++
        INC gaddr+1
++
	DEC cl		; c--
        BNE loop_xright_yup
        DEC ch		; adjusted high which allows this
        BNE loop_xright_yup

        JMP gexit



;-----------------------------------------------------------------

; line x right, y down, dx > dy (case 3)

line_down_flat
        JSR position	; x,y
	LDA cl		; counter adjustment for
	BEQ +		; dec-dec-counting
	INC ch
+
loop_xright_ydown
        JSR gchange	; pixel

        CLC		; k += dy
        LDA kl
        ADC dy
        STA kl
        BCC ++
        INC kh

++	CMP dxl		; k > dx
        LDA kh
        SBC dxh		; k -= dx
        BCC +

        STA kh
        LDA kl
        SBC dxl
        STA kl

        INY		; y++
        CPY #8
        BNE +
	; C=1
        LDA gaddr	; y+8: gaddr += 40*8 ($140)
        ADC #$40-1	; C already set by CPY
        STA gaddr
        LDA gaddr+1
	ADC #1
        STA gaddr+1
        LDY #0		; wrap around

+	INX		; x++
        CPX #8		; x overflow ?
        BNE +++
	; C=1
        LDX #$00	; wrap around
        LDA gaddr	; gaddr += 8
        ADC #$08-1	; C always set by CPX
        STA gaddr
        BCC +++
        INC gaddr+1
+++
	DEC cl		; c--
        BNE loop_xright_ydown
        DEC ch		; adjusted high which allows this
        BNE loop_xright_ydown

        JMP gexit


;-----------------------------------------------------------------

; line y down, x right, dx < dy (case 4)

line_down_steep
        JSR position	; x,y
loop_ydown_xright
        JSR gchange	; pixel

        CLC		; k += dx
        LDA kl
        ADC dxl		; dxh is 0, because dx < dy
        STA kl
        BCS ++
        CMP dy		; k > dy?
        BCC +
++	SBC dy		; k -= dy
        STA kl

        INX		; x++
        CPX #8
        BNE +		; x overflow?
        LDX #0		; wrap around
        LDA gaddr	; x+9: gaddr += 8
        ADC #8-1	; C already set by CPX
        STA gaddr
        BCC +
        INC gaddr+1

+	INY		; y++
        CPY #8		; y overflow?
        BNE +++
        LDA gaddr	; y+8: gaddr += 40*8 ($140)
        ADC #$40-1	; C already set by CPY
        STA gaddr
        LDA gaddr+1
	ADC #1
        STA gaddr+1
        LDY #0		; wrap around

+++	DEC cl		; c--
			; until c=0
        BNE loop_ydown_xright
        JMP gexit


;-----------------------------------------------------------------

getcommaxy
        JSR b_getcomma	; check ","
getxy
        JSR b_getval	; get X coord. value
        JSR b_convint
        CMP #>xmax
	BCC gcxy_xok
        BEQ +		; X = $1xx
error_iq
        JSR range_error
+	CPY #<xmax	; check X low
        BCS error_iq	; X to big
gcxy_xok
        STY gpos	; temporary save X coord.
        STA gpos+1

        JSR b_getcomma8bit
			; get Y coord. value
        CPX #ymax
        BCS error_iq	; Y to big

        LDY gpos	; restory X coord.
        LDA gpos+1
        RTS


;-----------------------------------------------------------------

hline
        JSR getxy	; get startpoint
        STX y
        STX savey	; save as cursor, too
        STA xh
        STY xl
        JSR b_getcomma	; get length
        JSR b_getval
        JSR b_convint

        CMP #>xmax
        BCC +		; X < 256
        BNE error_iq
        CPY #<xmax
        BCS error_iq
+
			; calculate end point
        TAX		; save length high byte
        TYA		; length low byte
        CLC
        ADC xl		; low xend = x+length
        STA xendl
	TAY
        TXA		; high
        ADC xh		; high xend = x+length
        STA xendh
	TAX

	CMP #>xmax	; endpoint outside?
	BCC +
	TYA
	SBC #<xmax
	BCS error_iq
+
        STX savexh
        STY savexl	; also save as cursor

        JSR ginit	; map in graphic memory

hline_start
        LDA xendl
        CMP xl
        LDA xendh
        SBC xh
        BCS hl_noxswap	; xend < x ->

        LDX xendl	; swap x, xend
        LDA xl
        STX xl
        STA xendl

        LDX xendh
        LDY xh
        STY xendh
        STX xh
        JMP hl_start	; x != xend

hl_noxswap
        LDA xendl
        CMP xl
        BNE hl_start
        LDA xendh
        CMP xh
        BNE hl_start	; x = xend ->
	JMP plot_start	; single point
;	JMP gexit	; no point

hl_start
        JSR position	; graphic position x,y
        LDA maskleft,X
        PHA		; save left end mask
        LDA xendl
        AND #%00000111
        STA tmp2	; xend mod 8, mask index
        LDA xl
        AND #%11111000	; (xl div 8)*8
        STA tmp1
        LDA xendl	; xend unmasked
        SEC
        SBC tmp1	; finally: xend - (x div 8)*8 
        STA tmp1
        LDA xendh
        SBC xh
        LSR		; / 8 ->  0-39
        LDA tmp1	; only 1 highest bit
        ROR		; and 3 lower bits
        LSR
        LSR
        TAX		; 8-pixel-blocks count
        PLA		; left end x mask

hl_nextblock
        DEX
hl_islastblock
        BMI hl_lastblock
			; leave loop if X<0
        JSR gmask	; first with left end mask
        CLC		; gaddr += 8
        LDA gaddr
        ADC #8
        STA gaddr
        BCC +
        INC gaddr+1
+	LDA #$FF	; following with full 8-pixel mask
	BNE hl_nextblock	; always

hl_lastblock
        LDX tmp2	; xend mask index
        AND maskright,X ; mask right end
        JSR gmask	; modify
        JMP gexit	; leave


;-----------------------------------------------------------------

vline
        JSR getxy	; get startpoint
        STA xh
        STA savexh	; save as cursor too
        STY xl
        STY savexl
        STX y

        JSR b_getcomma8bit
			; get length
        CLC		; calculate end point
        TXA		; length
; DON'T-CHANGE: how long to go vertically (needed later)
;		DO NOT USE: tmp1 does not exist if called via vline_start!
;	STA tmp1
        ADC y		; length + y
        CMP #ymax
        BCC +
vline_iq
        JSR range_error
+	STA yend	; endpoint
	CMP #ymax	; outside?
	BCS vline_iq

	STA savey	; set cursor y position

        JSR ginit	; map in graphic memory

vline_start
        LDA yend
        CMP y
        BCS vl_noyswap	; yend < y ->
        LDA y		; swap y, yend
        LDX yend
        STA yend
        STX y
	BEQ vl_start	; always (with next branch)
	; fall through if yend is
vl_noyswap
        BNE vl_start	; y = yend ->
	JMP plot_start	; single point
;	JMP gexit	; no point

vl_start
        JSR position	; graphic position x,y
        LDA bitmask,X
        STA tmp2	; save mask
; DON'T-CHANGE: replace ...
        SEC
        LDA yend
        SBC y		; vertical length
        TAX
; DON'T-CHANGE: replacy by ... (already as parameter, from tmp1)
;		DO NOT USE: tmp1 does not exist if called via vline_start!
;	LDX tmp1
        INX		; +1 (exit on 0)
vl_nextline
        LDA tmp2
        JSR gmask	; modify 
        INY		; go down
        CPY #8		; 8-line wrap
        BNE +
        LDA gaddr	; gaddr += 320
	ADC #$40-1	; compensate for C = 1
        STA gaddr
        LDA gaddr+1
        ADC #$01
        STA gaddr+1
        LDY #0		; wrap y offset
+	DEX		; all vertical positions done?
        BNE vl_nextline
        JMP gexit	; leave


;-----------------------------------------------------------------

line
        JSR getxy	; get startpoint
        STY xl 
        STA xh
        STX y

        JSR getcommaxy	; get endpoint
line_start
        STY savexl	; save as cursor position too
        STY xendl
        STA savexh
        STA xendh
        STX savey
        STX yend

        JSR ginit	; map in graphic memory

        LDY #$00	; initialize to 0
        STY ydir
        STY kl
        STY kh

        SEC
        LDA xendl	; calculate dx
        SBC xl
        STA dxl
        LDA xendh
        SBC xh
        STA dxh

        BCS li_xend_right
	; dx != 0
        TYA		; negate dx
        SEC		; dx = 0 - dx
        SBC dxl
        STA dxl
        TYA
        SBC dxh
        STA dxh
			; C=0 always, needed later
        LDX xl		; swap x low
        LDY xendl
        STX xendl
        STY xl

        LDX xh		; swap x high
        LDY xendh
        STX xendh
        STY xh

        LDX y		; swap y
        LDY yend
        STX yend
        STY y

        BCC li_x_different
			; C=0 always (from negation before)

li_xend_right
        LDA dxl		; dx = 0?
        ORA dxh
        BNE li_x_different
        JMP vline_start	; vertical line case

li_x_different
        SEC		; calculate dy
        LDA yend
        SBC y
        BCS li_y_right
        EOR #$FF	; negate dy (two's complement)
        ADC #$01	; C=0
        STA ydir	; flag y goes up

li_y_right
        STA dy
        BNE +
        JMP hline_start	; horizontal line case
+
	; dx and dy is *always* !=0, otherwise hline or vline got called.

        LDA dxh		; dx > dy
        BNE line_flat	; yes -> flat
        LDA dy		; no -> steep
        TAX
        CMP dxl
        BCC line_flat

line_steep
        INX	
        STX cl		; c = dy+1
        LSR		; k = dy/2
        STA kl
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
+	STX cl		; c = dx+1
        STY ch

        LSR		; k = dx/2
        STA kh
        LDA dxl
        ROR		; dx/2
        STA kl
        LDA ydir	
        BNE +
        JMP line_down_flat	; y down, flat
+	JMP line_up_flat	; y up, flat

;-----------------------------------------------------------------

plot
        JSR getxy	; get parameter
        STA xh		; save x/y
        STY xl
        STX y
        STA savexh	; and store as cursor
        STY savexl
        STX savey

plot_start
        JSR position	; calculate graphical address

        LDA prozport
        AND #%11111101	; Kernal ROM disable
        SEI			
        STA prozport

        JSR gchange	; change graphical data

        LDA prozport
        ORA #%00000010	; kernal ROM enable
        STA prozport
        CLI
        RTS

;-----------------------------------------------------------------

move
        JSR getxy	; get parameter
        STA savexh	; just save as cursor
        STY savexl
        STX savey
        RTS


;-----------------------------------------------------------------

range_error
	LDA savemo
	AND #$F0
	BNE +
	PLA			; cleanup JSR
	PLA
-	RTS			; error mode 0: do nothing, back to caller before
				; error mode 2: cut value: control back
				; to handle value correction
+	AND #$20
	BNE -
	PLA			; cleanup JSR
	PLA
setmode_error
	JMP b_illquant		; error mode 1: throw error message

;-----------------------------------------------------------------

setmode
        JSR b_get8bit
        CPX #3
        BCC +			; less then 3, modification mode
	CPX #6
	BCS setmode_error	; out of range
				; error mode
	ADC #13			; C=0, therefore -3
				; 3-5 -> 16-18
				; put A's bit 4-7 into savemo
	EOR savemo		; ********
	AND #$F0		; ****0000
	EOR savemo		; AAAAmmmm
	STA savemo		; 
	RTS

+	TXA
	EOR savemo		; put A's bit 0-3 into savemo
	AND #$0F
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
        LDA #$3D		; AND abs,X
        STA gchange_op
        LDA #$31		; AND (zp),Y
        STA gmask_op
        LDA #$FF		; EOR $#FF, invertieren
        STA gmask_flip+1
        RTS

set_or_toggle
        BNE modetoggle
modeset
        LDA #>(bitmask)
        STA gchange_op+2
        LDA #<(bitmask)
        STA gchange_op+1
        LDA #$1D		; OR abs,X
        STA gchange_op
        LDA #$11		; OR (zp),Y
        STA gmask_op
        LDA #$00		; EOR #$00, nicht invertieren
        STA gmask_flip+1
        RTS

modetoggle
        LDA #>(bitmask)
        STA gchange_op+2
        LDA #<(bitmask)
        STA gchange_op+1
        LDA #$5D		; EOR abs,X
        STA gchange_op
        LDA #$51		; EOR (zp),Y
        STA gmask_op
        LDA #$00		; EOR #$00, nicht invertieren
        STA gmask_flip+1
        RTS


;-----------------------------------------------------------------

; get pixel (check if pixel set)
; not used

get
        JSR getcommaxy
        STA xh
        STY xl
        STX y

        JSR position

        LDA prozport
	AND #%11111101	; Kernal ROM disable
        SEI
        STA prozport

        LDA (gaddr),Y
        AND bitmask,X
        TAY
        LDA prozport
	ORA #%00000010	; kernal ROM enable
        STA prozport
        CLI
        JMP b_byte2fac


;-----------------------------------------------------------------

relto
        JSR b_getval	; get X offset (+/-)
	LDA facexp	; FAC exponent
	CMP #$90	; more than 16 bit
	BCS relto_error	; illegal quantity
        JSR b_fac2int	; to signed integer

        CLC
        LDA facintl
        ADC savexl
        STA xendl
        LDA facinth
        ADC savexh
        STA xendh	; xend = savex+facint

        JSR b_getcomma	; get Y offset (+/-)
        JSR b_getval
        LDA facexp	; FAC exponent
        CMP #$90	; more than 16 bit
        BCS relto_error	; illegal quantity
        JSR b_fac2int	; to signed integer
        CLC
        LDA facintl
        ADC savey
        STA yend	; yend = savey+facint

        LDA xendh	; check end coord. x
        CMP #>xmax
        BCC rt_xok
        BEQ +
relto_error
        JSR range_error
+	LDA xendl
        CMP #<xmax
        BCS relto_error
rt_xok
        LDA yend	; check end coord. y
        CMP #ymax
        BCS relto_error

        LDA savexl
        STA xl
        LDA savexh
        STA xh
        LDA savey
        STA y
        LDY xendl
        LDA xendh
        LDX yend	; xend/yend = cursor + x/y

        JMP line_start	; draw line x/y to xend/yend


;-----------------------------------------------------------------

char
        JSR b_get8bit	; get char. position x 0-39
        CPX #40	
        BCC +
char_error
        JMP b_illquant
+	STX gpos	; save x coord.
        JSR b_getcomma8bit
			; get char. position y 0-24
        CPX #25
        BCS char_error
        STX gpos+1	; save y coord.

        JSR b_getcomma	; get string
        JSR b_getexpr
        JSR b_stringval ; string address in str
        PHA		; string length
        LDX gpos+1	; y coord. for char. position
        TXA
        AND #$03	; mask 2 bits
        TAY		; table index
        LDA #$00
        STA gpos+1	; x high
        LDA gpos	; saved x: multiply by 8
        ASL
        ASL
        ASL
        ROL gpos+1	; overflow to high byte
        ADC ytabl,Y
        STA gaddr
        LDA gpos+1	; x high
        ADC ytabh,X
        STA gaddr+1
        PLA		; string length
        LDY #$00	; string index
        TAX		; length
        INX		; prepare as counter
char_loop
        DEX
        BEQ char_exit
        LDA (str),Y	; read string
        JSR char_display
        INY
        BNE char_loop
char_exit
        RTS

char_display
        STA z_tmp	; character (lastkey, temporary reused)
        TXA		; save register X+Y
        PHA
        TYA
        PHA
        LDA z_tmp	; get saved character
        BPL char_normal

char_inverse
        AND #%01111111	; mask bit 7
        CMP #%01111111	; was 255? (pi)
        BNE +
        LDA #$5E	; screen code for pi
+	CMP #$20	; control character?
        BCC char_disp_leave
			; yes, skip
        ORA #%01000000	; $A0-$BF -> $60-$7F
			; $C0-$FF -> $40-$7F
			; OPT: BNE char_hires
			; OPT: char_normal
char_hires
        LDX z_reverseflag
        BEQ +
        ORA #%10000000	; invert char.
+	PHA		; save char. for later
        SEI
        LDA #$21	; char. rom, no basic and kernal rom
        STA prozport	; char. rom base = $D000
        LDA #($D0 >> 3)	; $D0/8   1101 0000 -> 0001 1010
        STA gpos+1	; 
        PLA		; char. code
        ASL		; *8
        ROL gpos+1
        ASL
        ROL gpos+1
        ASL
        ROL gpos+1
        STA gpos	; addr. in char. rom for char.

        LDY #$07	; 8 hires lines
char_line
        LDA (gpos),Y	; read character line
        JSR gmask	; write to hires screen
        DEY
        BPL char_line

        LDA #$36	; no char. rom, basic ram, kernal rom
        STA prozport
        CLI

        CLC		; step char position to left
        LDA gaddr	; ( +8 )
        ADC #$08
        STA gaddr
        BCC +
        INC gaddr+1
+
char_disp_leave
	PLA		; pass written character back
        TAY		; restore saved registers
        PLA
        TAX
        RTS

char_normal
        CMP #$20	; control character?
        BCC char_disp_leave
        CMP #$60
        BCC +
        AND #%11011111	; $60-$7F -> $40-$5F
        BNE ++
+	AND #%00111111  ; $40-$5F -> $00-$1F
++	JMP char_hires	; 		OPT: Bxx


;-----------------------------------------------------------------

to
        LDA savexl
        STA xl
        LDA savexh
        STA xh
        LDA savey
        STA y
        JSR getxy
        JMP line_start

;-----------------------------------------------------------------
graext_end
