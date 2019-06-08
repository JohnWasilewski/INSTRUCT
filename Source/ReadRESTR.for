      SUBROUTINE ReadRESTR(LU,NextLine)
C     ---------------------------------
C     jw / 30-10-85  draft
C     jw / 19-08-12  last revised

C     Read rigid RESTRAINTS from LU

      USE PARAMS
      USE Restraints

      CHARACTER NextLine*80,String*6,I2CHAR*8
      INTEGER RestrLine

C     Initialise
      DO iDoF = 1,NDOFG
         KFIXD(IDOF) = 0
      END DO

C     DO iPRES = 1,NPRES
C        PRESC(iPRES) =0.0
C     END DO

C     Count any rigid restraints which have already been set
      iPRES=0
      DO iNode=1,NNODE
         DO iDoF=1,3
            IF(KFIXD(IDOFN(ABS(iNode),iDoF)) .EQ. 1) iPRES=iPRES+1
         END DO ! iDoF
      END DO ! iNode

C     Set GoodRES to .TRUE. pending any checks to the contrary
      GoodRes=.TRUE.
      RestrLine=0

C     Enter loop repeatedly reading the next line in the file
      DO
         RestrLine=RestrLine+1
         IF(RestrLine.NE.1) THEN  !1st line already read by FoundIN()
             NextLine=''
C            Read next line from input file
             READ(LU,'(A)') NextLine

C            If the line is blank, no further restraints to read
             IF(LEN_TRIM(NextLine).EQ.0) EXIT
         END IF

C        Read the restraint node number, and the trailing string
C        containing restrained directions, X, Y and/or Z
         READ(NextLine,*) iNode,String

C        Initialise GoodRES to .FALSE.
         GoodRES = .FALSE.
C        It will be se to .TRUE. if a valid restraint
C        is found in the data line just read

C        Is it a valid node number?
         IF (iNode.LE.0 .OR. iNode.GT.NNODE) THEN

C           No, it isn't a valid node number, so ignore it
            CALL BailOUT('ERROR:|Restraints discarded for '//
     +         'invalid node '//TRIM(I2CHAR(iNode,iDum))//'.$',
     +         'OPTIONS' ,'YES')
            CYCLE

         ELSE

C           Yes, it is a valid node number, so search String
C           for restrained directions, X, Y and/or Z

            CALL GetRESTR(iNode,String)

C           If, having now checked iDoF=1,3, .GoodRES. still
C           not found, then issue an error message.
C
            IF(.NOT.GoodRES)
     +         CALL BailOUT('ERROR:|Restrained node '//
     +         TRIM(I2CHAR(iNode,iDum))//' has no recognised '//
     +         'X, Y and/or Z restraint direction.$','OPTIONS' ,'YES')

         END IF !Whether this is a valid node number
      END DO !Repeatedly reading the next line in the file

      RETURN
      END

