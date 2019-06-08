      SUBROUTINE ReadSPRI(LU,NextLine)
C     --------------------------------
C     jw / 16-09-12  draft
C     jw / 16-09-12  last revised

C     READ SPRINGS from LU

      USE dislin
      USE LUnits
      USE WINSTUFF
      USE FILES
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints

      CHARACTER   NextLine*80,I2CHAR*8,iDir*1
      CHARACTER*2 ADoF(3)
      REAL        KSpri

      INTEGER     SprLine

      LOGICAL     GoodLine

      PARAMETER (ADoF = (/'Xx','Yy','Zz'/))

      GoodNod=.TRUE.
      iSpri=0
      GoodLine=.TRUE.
      SprLine=0
C     Enter loop repeatedly reading the next line in the file
      DO WHILE(iSpri.LT.NSpri)
         SprLine=SprLine+1
         IF(SprLine.NE.1) THEN  !1st line already read by FoundIN()
             NextLine=''
C            Read next line from input file
             READ(LU,'(A)') NextLine

C            If the line is blank, no further springs to read
             IF(LEN_TRIM(NextLine).EQ.0) EXIT
         END IF

         READ(NextLine,*) iNode,iDir,KSpri

         IF (iNode.LE.0 .OR. iNode.GT.NNODE) THEN
            CALL BailOUT('ERROR:|'//
     +      'Spring discarded for invalid node '//
     +      TRIM(I2CHAR(iNode,iDum))//'.$',
     +    'OPTIONS' ,'YES')
            GoodLine = .FALSE.
         END IF

         iDoF=(INDEX(ADoF(1)//ADoF(2)//ADoF(3),iDir)+1)/2
         IF(iDoF.EQ.0) THEN
            CALL BailOUT('ERROR:|'//
     +         'Spring direction not recognised as X, Y or Z at node '//
     +         TRIM(I2CHAR(iNode,iDum))//'.',
     +         'OPTIONS' ,'YES')
               GoodLine = .FALSE.
         END IF

         IF(GoodLine) THEN
C           Spring seems OK so far. Check not too many.
            iSpri=iSpri+1

            IF(iSpri.GT.NSPRI) THEN
               CALL BailOUT('ERROR:|'//
     +         'Too many springs:|'//
     +         'Spring on node '//
     +         TRIM(I2CHAR(iNode,iDum))//
     +         '('//ADoF(iDoF)(1:1)//') ignored.$',
     +         'OPTIONS' ,'YES')
               GoodLine = .FALSE.
            END IF

         END IF

         IF(GoodLine) THEN
C           Accept the line of data read from the file
C           and copy it to array storage
            SPRING(iSpri)%Node  = iNode
            SPRING(iSpri)%DoF   = iDoF
            SPRING(iSpri)%Stiff = KSpri

         ELSE
            GoodNod=.FALSE.
         END IF
      END DO
      RETURN
      END