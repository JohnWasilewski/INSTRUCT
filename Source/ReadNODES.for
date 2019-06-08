      SUBROUTINE ReadNODES(LU,NextLine)
C     ---------------------------------
C     jw / 16-09-12  draft
C     jw / 16-09-12  last revised

C     READ NODES from LU

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

      CHARACTER   NextLine*80,I2CHAR*8,String*48
      CHARACTER*2 ADoF(3)
      REAL        Gamma,Emod,Gmod, KSpr(3)

      LOGICAL     GoodLine

      PARAMETER (ADoF = (/'Xx','Yy','Zz'/))

C     Count how many rigid restraints have already been set
      iPRES=0
      DO iNode=1,NNODE
         DO iDoF=1,3
            IF(KFIXD(IDOFN(ABS(iNode),iDoF)) .EQ. 1) iPRES=iPRES+1
         END DO ! iDoF
      END DO ! iNode

      GoodNod=.TRUE.
      NodLine=0

C     Enter loop repeatedly reading the next line in the file
      DO
         NodLine=NodLine+1
         GoodLine=.TRUE.
         IF(NodLine.NE.1) THEN  !1st line already read by FoundIN()
             NextLine=''
C            Read next line from input file
             READ(LU,'(A)') NextLine

C            If the line is blank, no further nodes to read
             IF(LEN_TRIM(NextLine).EQ.0) EXIT
         END IF
         READ(NextLine,*) iNode,X,Y
         IF (iNode.LE.0 .OR. iNode.GT.NNODE) THEN
            CALL SCRmsg('**Error: Coordinates discarded for '//
     +      'invalid node <'//TRIM(I2CHAR(iNode,iDum))//'>',.FALSE.)
            GoodLine = .FALSE.
         END IF

         IF(GoodLine) THEN
C           Accept the line of data read from the file
C           and copy it to array storage
            Node(1,iNode)=X
            Node(2,iNode)=Y

C           Is there a rigid-restraints string after the X, Y coords?
            iRes=SCAN(NextLine,ADoF(1)//ADoF(2)//ADoF(3))
            IF(iRes.GT.0) THEN
                String=NextLine(iRes:MAX(iRes+5,LEN_TRIM(NextLine)))
                CALL GetRESTR(iNode,String)
            END IF

C           Are there any sprung-restraints after the X, Y coords
C           and/or rigid restraints?
            jSpr=MAX(INDEX(NextLine,'K='),
     +      INDEX(NextLine,'K ='),
     +      INDEX(NextLine,'k='),
     +      INDEX(NextLine,'k ='))
            IF(jSpr.NE.0) THEN !One or more springs added to iNode

C              Which DoFs have the spring(s)?
               String=NextLine(jSpr:LEN_TRIM(NextLine))
               i0=INDEX(String,'=')+1
               i1=LEN_TRIM(String)
               READ(String(i0:i1),*) (KSpr(i),i=1,3)
               DO iDoF=1,3
                  IF(KSpr(iDoF).NE.0.0) THEN
C                    Data for iNode puts a spring on iDoF

C                    Check not rigidly restrained
                     IF(KFIXD(IDOFN(iNode,iDoF)) .EQ. 1) THEN
                        CALL BailOUT('ERROR:|'//
     +                  'Springs not possible on rigid restraints.|'
     +                  //'Node '//TRIM(I2CHAR(iNode,iDum))
     +                  //'('//ADoF(iDoF)(1:1)//') '
     +                  //'is rigidly restrained, so this|'
     +                  //'spring will be ignored.$','OPTIONS','YES')
                        GOODLINE = .FALSE.
                     END IF

C                    Is there already a spring here?
                     IF(GOODLINE) THEN
                        DO iS=1,NSPRI
                           IF (SPRING(iS)%Node.EQ.iNode
     +                     .AND.SPRING(iS)%DoF.EQ.iDoF) THEN
                              CALL BailOUT('ERROR:|'//
     +                        'A spring has already been entered|'//
     +                        'at Node '//TRIM(I2CHAR(iNode,iDum))//
     +                        '('//ADoF(iDoF)(1:1)//'), so this|'
     +                        //'spring will be ignored.$'
     +                        ,'OPTIONS' ,'YES')
                              GOODLINE = .FALSE.
                           END IF
                        END DO
                     END IF

C                    Too many springs?
                     IF(GOODLINE) THEN
                        IF (iSpri+1.GT.NSPRI) THEN
                           CALL BailOUT('ERROR:|'//
     +                     'Too many springs:|'//
     +                     'Spring on node '//
     +                     TRIM(I2CHAR(iNode,iDum))//
     +                     '('//ADoF(iDoF)(1:1)//') ignored.$',
     +                     'OPTIONS' ,'YES')
                           GoodLine = .FALSE.
                        END IF
                     END IF

                     IF(GOODLINE) THEN
C                       Accept the spring read from the file
C                       and copy it to array storage
                        iSpri=iSpri+1
                        SPRING(iSpri)%Node  = iNode
                        SPRING(iSpri)%DoF   = iDoF
                        SPRING(iSpri)%Stiff = KSpr(iDoF)
                     END IF
                  END IF
               END DO
            END IF

         ELSE
            GoodNod=.FALSE.
         END IF
      END DO
      
C     Flag all member stiffnesses as unknown
      DO iMem = 1,NMEMB
         MEMB(iMem)%StiffsStored = .FALSE.
      END DO !iMem
      
      RETURN
      END