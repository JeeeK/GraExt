!to "ge-run.prg",cbm	

;  **** gra-ext loader ****
;
; 2016-05-18 johann e. klasek, johann at klasek at
;
; revisions:
;	2016-05-18 v 1.21
;
;
; Usage: RUN
;

; loader for BASIC

*= $0801
basic_start
;       2013 sys2061
	!by <EOP,>EOP,<(2013),>(2013),$9E
	!tx "2061"
	!by 0 		; End of Line
EOP	!by 0, 0	; Basic-Programmende

loader
!if loader != 2061 {
	!error "Loader-Adresse stimmt nicht mit SYS-Adresse überein!"
}

	ldx #<graext_end	; setup basic
	ldy #>graext_end
	clc			; set if C=0
	jsr $ff9c		; KERNAL: system RAM bottom
	stx $2b			; BASIC text start
	sty $2c
	jsr $e416		; setup BASIC text start
	jsr init		; init extension (place hook)
	lda #<author		; message ...
	ldy #>author
	jsr $e441		; output string and perform BASIC NEW (set remaining pointers)
	jmp $e386		; BASIC warm start

!source "graext-core.asm"


