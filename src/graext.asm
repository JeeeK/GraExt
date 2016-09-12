!to "graext.o",cbm	

;  **** gra-ext module ****
;
; 2016-05-18 johann e. klasek, johann at klasek at
;
; revisions:
;	2016-06-16 v 1.24
;
;
; Usage: SYS49152
;

*= $c000

	bit $FFFF
*=*-2				; back 2 byte (reset bit-argument)
magic
	!scr "jk"		; magic

        lda #<(parse)		; check if basic interpreter parser hook
        cmp v_bascmd		; does already exist
	bne start
        lda #>(parse)
        cmp v_bascmd+1
        bne start
	rts			; hook already in place, no start message

start
        jsr init                ; init extension (place hook)
	ldx $3a			; direct mode flag
	inx
	beq interactive
	rts			; simply return in running program
interactive
        lda #<author            ; message ...
        ldy #>author
        jsr $ab1e               ; output string 

        jmp $e386               ; BASIC warm start

!source "graext-core.asm"


