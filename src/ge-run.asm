!to "ge-run.prg",cbm	

;  **** gra-ext loader ****
;
; 2016-05-18 johann e. klasek, johann at klasek at
;
; revisions:
;	2019-10-12 v 1.29
;
;
; Usage: RUN
;

; loader for BASIC

*= $0801
basic_start
;       2019 sys....:REM by ....
        !by <EOP,>EOP,<(2019),>(2019),$9E ; Link-Adresse, Zeilennummer, SYS-Token
        !by '0' + loader % 10000 / 1000   ; Ziffern für SYS-Argument
        !by '0' + loader %  1000 /  100
        !by '0' + loader %   100 /   10
        !by '0' + loader %    10
        ; ":rem "-Teil
        !pet $3a, $8f, " graext by j.e.e.k."
        !by 0                             ; BASIC "End of Line"
EOP     !wo 0                             ; BASIC-Programmende

loader

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


