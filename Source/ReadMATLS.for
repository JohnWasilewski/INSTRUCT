      SUBROUTINE ReadMATLS(LU,NextLine)
C     ---------------------------------
C     jw / 30-10-85  draft
C     jw / 03-03-12  last revised

C     Always obtain an input filename from the user.
C     Open it if possible.
C     Try to read all data contained in it.
C     Close it.

      USE PARAMS
      USE MATERIALS

      CHARACTER NextLine*80, I2CHAR*8
      INTEGER Imat
      REAL Gamma,Emod,Gmod
      CHARACTER Des*31

      LOGICAL GoodLine

      GoodMat=.TRUE.
      MatlLine=0

C     Enter loop repeatedly reading the next line in the file
      DO
         MatlLine=MatlLine+1
         GoodLine=.TRUE.
         IF(MatlLine.NE.1) THEN  !1st line already read by FoundIN()
             NextLine=''
C            Read next line from input file
             READ(LU,'(A)') NextLine

C            If the line is blank, no further materials to read
             IF(LEN_TRIM(NextLine).EQ.0) EXIT
         END IF
         READ(NextLine,*,END=201) Imat,Gamma,Emod,Gmod,Des
         GO TO 202
201          CONTINUE ! READ ERR
             IF(Imat.NE.0 .AND. Emod.NE.0.0) THEN
                 Des=''
                 GO TO 202 ! No ERR, continue
             ELSE
             END IF
202      CONTINUE !No READ ERR
         IF(LEN_TRIM(Des).GT.0)
     +   Des = NextLine(Index(NextLine,TRIM(Des)):LEN(NextLine))

         IF (Imat.LE.0 .OR. Imat.GT.NMATS) THEN
            CALL SCRmsg
     +      ('**Error: Material properties discarded for '//
     +      'invalid material <'//TRIM(I2CHAR(Imat,Idum))//
     +      '>',.FALSE.)
            IF (Imat.NE.0) GoodLine = .FALSE.
         END IF

         IF (Gamma.LE.0.0 .OR. EMod.LE.0.0 .OR. GMod.LE.0.0) THEN
            CALL SCRmsg('**Error in properties entered for '//
     +      'material number '//TRIM(I2CHAR(Imat,Idum)),.FALSE.)
            IF (Gamma.NE.0.0 .AND. EMod.NE.0.0 .AND. GMod.NE.0.0)
     +      GoodLine = .FALSE.
         END IF

         IF(GoodLine) THEN
C           Accept the line of data read from the file
C           and copy it to array storage
            Matls(IMat)%Gamma = Gamma
            Matls(IMat)%Desc  = Des
            Matls(IMat)%Emod  = EMod
            Matls(IMat)%Gmod  = GMod
         ELSE
            GoodMat=.FALSE.
         END IF

      END DO

      DO Imat = 1,NMATS
         IF
     +   (Matls(IMat)%Gamma.LE.0.0 .OR.
     +    Matls(IMat)%Emod.LE.0.0 .OR.
     +    Matls(IMat)%Gmod.LE.0.0) THEN
            CALL SCRmsg('**Bad properties entered for '//
     +     'material number'//TRIM(I2CHAR(Imat,Idum)),.FALSE.)
            GoodMat = .FALSE.
         END IF
      END DO
      RETURN
      END

