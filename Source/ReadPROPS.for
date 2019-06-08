      SUBROUTINE ReadPROPS(LU,NextLine)
C     ---------------------------------
C     jw / 30-10-85  draft
C     jw / 03-03-12  last revised

C     Always obtain an input filename from the user.
C     Open it if possible.
C     Try to read all data contained in it.
C     Close it.

      USE LUnits
      USE PARAMS
      USE PROPERTIES

      CHARACTER NextLine*80,I2CHAR*8
      INTEGER Matl,Iprp, PrpLine
      REAL AreaX,AreaY,InrtZ
      CHARACTER Des*31

      LOGICAL GoodLine

      GoodPrp=.TRUE.
      PrpLine=0

C     Enter loop repeatedly reading the next line in the file
C     (except the first 'NextLine', which is already read by FoundIN() )
      DO
         PrpLine=PrpLine+1
         GoodLine=.TRUE.

         IF(PrpLine.NE.1) THEN  !NextLine not already read by FoundIN()
             NextLine=''
C            Read next line from input file
             READ(LU,'(A)') NextLine

C            If the line is blank, no further member types to read
             IF(LEN_TRIM(NextLine).EQ.0) EXIT
         END IF
         READ(NextLine,*,END=201) IPrp,Matl,AreaX,AreaY,InrtZ,Des
         GO TO 202
201          CONTINUE ! READ ERR
             IF(Matl.LT.0)    Matl=0
             IF(isNaN(AreaX).OR.AreaX.LT.0.0) AreaX=0.0
             IF(isNaN(AreaY).OR.AreaY.LT.0.0) AreaY=0.0
             IF(isNaN(InrtZ).OR.InrtZ.LT.0.0) InrtZ=0.0
             Des=''

202      CONTINUE !No READ ERR
         IF(LEN_TRIM(Des).GT.0)
     +   Des = TRIM(Des)//REPEAT(' ',24-LEN(TRIM(DES)))

         IF (IPrp.LE.0 .OR. IPrp.GT.NMTYP) THEN
            CALL SCRmsg('**Error: Properties discarded for '//
     +      'invalid member type <'//TRIM(I2CHAR(IPrp,iDum))//'>',
     +      .FALSE.)
            IF (IPrp.NE.0) GoodLine = .FALSE.
         END IF

         IF (AreaX.LE.0.0 .OR. AreaY.LE.0.0 .OR. InrtZ.LE.0.0) THEN
            CALL SCRmsg('**Error in properties entered for '//
     +      'member type '//TRIM(I2CHAR(IPrp,Idum)),.FALSE.)
            GoodLine = .FALSE.
         END IF

         IF(GoodLine) THEN
C           Accept the line of data read from the file
C           and copy it to array storage
            PROPS(IPrp)%Matl=Matl
            PROPS(IPrp)%Desc=Des
            PROPS(IPrp)%AreaX=AreaX
            PROPS(IPrp)%AreaY=AreaY
            PROPS(IPrp)%InrtZ=InrtZ
         ELSE
            GoodPrp=.FALSE.
         END IF

      END DO

      DO Iprp = 1,NMTYP
         IF (PROPS(IPrp)%AreaX.LE.0.0 .OR.
     +       PROPS(IPrp)%AreaY.LE.0.0 .OR.
     +       PROPS(IPrp)%InrtZ.LE.0.0) THEN
            CALL SCRmsg('**Error in properties entered for '//
     +      'member type'//TRIM(I2CHAR(Iprp,Idum)),.FALSE.)
            GoodPrp = .FALSE.
         END IF
      END DO
      RETURN
      END

