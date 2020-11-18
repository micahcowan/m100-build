; determine the value of PC
; immediately after CALL,
; and save it to (HL)
call 1D         CD 1D 00
DEC SP          3B
DEC SP          3B
POP DE          D1
LD (HL),E       73
INC HL          23
LD (HL),D       72
RET             C9
