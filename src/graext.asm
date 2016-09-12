!to "graext.o",cbm	

;  **** gra-ext module ****
;
; 2016-05-18 johann e. klasek, johann at klasek at
;
; revisions:
;	2016-05-18 v 1.21
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
        lda #<author            ; message ...
        ldy #>author
        jsr $ab1e               ; output string 
        jmp $e386               ; BASIC warm start

!source "graext-core.asm"


