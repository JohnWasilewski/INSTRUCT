C     ASCTYP.INC
C     ASCII character type declarations
C     jw 09-10-01
C     jw 23-11-03 last rev

      Character PADDNG, NUL$,   BEL$,   BS$,    TAB$,
     +          LF$,    CR$,    EOF$,   SPC$,   ESC$,
     +          Box1TL, Box1TR, Box1BL, Box1BR,
     +          Box1H,  Box1V, 
     +          Tee1T,  Tee1B,  Tee1L,  Tee1R

C
C     Logical value to test whether the compiler has managed 
C     to populate the above ASC values in Block Data LUINIT
C
      LOGICAL   ASCset

C     asctyp.inc
