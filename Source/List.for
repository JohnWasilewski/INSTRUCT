
      SUBROUTINE ListOUT
C     ------------------
C     jw / 29-01-12  draft
C     jw / 29-01-12  last revised

      USE LUnits
      USE Files
      USE RTF

      IF(F%Named) THEN !File name is reported as KNOWN
          IF(F%O%Open) THEN !Output file is reported as OPEN
              IF(F%O%LU.GT.0) THEN
                  CONTINUE
              ELSE !No valid file unit
                  CALL BailOUT
     +            ('ERROR|Output file '//TRIM(F%Name)//'.rtf reported'//
     +            '|as OPEN but it has no valid file unit.|$',
     +            'OPTIONS','YES')
C                  Reaching here means user opted to continue
                   CALL fSaveAs
              END IF !(F%O%LU.GT.0)'invalid file unit' code
C             F%O%LU Output file file unit was OK
          ELSE
              !Output file reported as KNOWN so should be OPEN but isn't
              CALL BailOUT
     +        ('ERROR|Output file '//TRIM(F%Name)//'.rtf is known so'//
     +        '|it should be OPEN but reported not open.$|',
     +        'OPTIONS','YES')
C             Reaching here means user opted to continue
              CALL fOPEN
          END IF !(F%O%Open)
      END IF !(F%Named)

      IF(.NOT.F%InpDataPosted) THEN
          CALL LIST(F%O%LU)
          F%InpDataPosted = .TRUE.
          CALL LIST(0)
          FLUSH(F%O%LU)
          WRITE(F%O%LU, '(''}'')')
          FLUSH(F%O%LU)
      END IF !(.NOT.F%InpDataPosted)

      RETURN
      END




      SUBROUTINE List(LU)
C     -------------------
C     jw / 00-00-01 draft
C     jw / 22-08-12 last rev

C     List all input data to either a text window or LU
C     If (LU.EQ.0) then use CALL SWGTXT to list to the text window.
C     If (LU.GT.0) then list to Fortran logical unit LU

      USE dislin
      USE Config
      USE RTF
      USE FILES
      USE WINSTUFF
      USE M
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING


      REAL      Fx,Fy,Mz
      CHARACTER I2CHAR*16, R2CHAR*16, Axes*1, LTyp*1, TDLshp*1
      CHARACTER AxesW*1, LTypW*8, aFx*11, aFy*11, aMz*11
      CHARACTER*1 Rel1,Rel2,Rel3,Rel4,Rel5,Rel6

      CHARACTER TxtLN*80, Anum*7, RelI*3,
     +          aIF*60, NodRes*3, SprRes(3)*9,
     +          ADoF*3, LCNam*30, Dashes*9, OutpCOMB*90

      LOGICAL   MoreLC, MoreLod, NewPg

      PARAMETER (ADoF   = ('XYZ'))
      PARAMETER (Dashes = (' -  -  - '))

      IF(SOLVED .AND. LU.EQ.0) RETURN

      IF(LU.NE.0) THEN
          CALL nPAGE(PRLIPP-6,NewPg)
          CALL DrawRTF
      END IF
C     IF(LU.NE.0) CALL DrawRTF

C     IF(LU.EQ.0 .AND. ShowsSCR) CALL WinSCR('HIDE')
      IF(LU.EQ.0) THEN
          CALL WinSCR('SHOW')

C         List headings
          CALL SWGFNT(MonB%Styl,MonB%Size)
          CALL SWGTXT(idSCR,'Program '//PROG8//'  '//
     +                  'vers.'  //VERS4//': '//PDAT9)
          CALL SWGFNT(MonS%Styl,MonS%Size)
C         CALL SWGTXT(idSCR,'vers. '  //VERS//': '//PDAT)
          CALL SWGTXT(idSCR,'Copyright '//CPRT17(1:14)//AUTH16)
          CALL SWGFNT(MonN%Styl,MonB%Size)
          CALL SWGFNT('Courier New',10)
          CALL SWGTXT(idSCR,REPEAT('-',81))
          CALL SWGTXT(idSCR,TITLE1)
          CALL SWGTXT(idSCR,TITLE2)
          CALL SWGTXT(idSCR,TITLE3)
          CALL SWGTXT(idSCR,' ')
          CALL SWGFNT('Arial',8)
          CALL SWGTXT(idSCR,'Engineer: '//ENG)
          CALL SWGTXT(idSCR,'Date: '//UDat)
          CALL SWGTXT(idSCR,REPEAT('-',81))
      END IF !(LU.EQ.0)

C     LIST PARAMS
C     -----------
      CALL SWGTXT(idSCR,'SIZE of structure')
      ANUM=I2CHAR(MAX(NMATS,NMTYP,NNODE,NMEMB,NPRES,NSPRI,NLCAS),iL1)

      ANUM=I2CHAR(NMATS,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' material types')

      ANUM=I2CHAR(NMTYP,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' member types')

      ANUM=I2CHAR(NNODE,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' nodes')

      ANUM=I2CHAR(NMEMB,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' members')

      ANUM=I2CHAR(NPRES,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' rigid support restraints')

      ANUM=I2CHAR(NSPRI,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' springs')

      ANUM=I2CHAR(NLCAS,IL2)//REPEAT(' ',7-IL2)
      CALL SWGTXT(idSCR,ANUM(1:IL1)//' load cases')

      CALL SWGTXT(idSCR,REPEAT('-',81))



C     LIST MATERIALS
C     --------------
C     IF(GoodMAT) THEN
C     IF(.TRUE.) THEN
      iTAB=(PRLINW-75)/2
      iMAT = 1

101   IF(LU.EQ.0) THEN !                            CAPTIONS --> CONSOLE
          WRITE(TxtLN,'(''MATERIALS'',26X,
     +                  ''Density   E-modulus   G-modulus'')')
          CALL SWGTXT(idSCR,TRIM(TxtLN))

      ELSE             !                            CAPTIONS --> OUTFILE
          CALL nPAGE(MIN(2*PRLIPP/7,NMATS+4),NewPg)
          WRITE(LU,'(A)') CR$//Bold$//
     +    REPEAT(' ',iTAB)//'MATERIALS'//
     +    REPEAT(' ',26)//'Density   E-modulus   G-modulus'//
     +    unBold$//CR$
          ILINE=ILINE+2
      END IF !(LU.EQ.0)

      DO IMat=iMAT,NMATS
          WRITE(TxtLN,'(I4,'': '',A24,F12.3,2E12.3)')
     +        IMat,
     +        Matls(IMat)%Desc,
     +        Matls(IMat)%Gamma,
     +        Matls(IMat)%Emod,
     +        Matls(IMat)%Gmod
          IF(LU.EQ.0) THEN !                         LISTING --> CONSOLE
              CALL SWGTXT(idSCR,TRIM(TxtLN))
          ELSE             !                         LISTING --> OUTFILE
              CALL nPAGE(1,NewPg)
                 IF(NewPg) THEN
                    NewPg = .FALSE.
                    GO TO 101
                 END IF
              WRITE(LU,'(A)') REPEAT(' ',iTAB)//TRIM(TxtLN)//CR$
              ILINE=ILINE+1
          END IF !(LU.EQ.0)
      END DO !IMat=iMAT,NMATS

      IF(LU.EQ.0)CALL SWGTXT(idSCR,REPEAT('-',81))
C     END IF !(GoodMAT)

C     LIST MEMBER TYPES/PROPERTIES
C     ----------------------------
C     IF(GoodPRP) THEN
C     IF(.TRUE.) THEN
C     iTAB=(PRLINW-75)/2
      iPrp = 1

201   IF(LU.EQ.0) THEN !                            CAPTIONS --> CONSOLE
          WRITE(TxtLN,'(''MEMBER TYPES'',19X,''Matl'',
     +                    7X,''Ax'',9X,''Ay'',10X,''Iz'')')
          CALL SWGTXT(idSCR,TRIM(TxtLN))
      ELSE             !                            CAPTIONS --> OUTFILE
          CALL nPAGE(MIN(2*PRLIPP/7,NMTYP+4),NewPg)
          WRITE(LU,'(A)') CR$//CR$//CR$//Bold$//
     +    REPEAT(' ',iTAB)//'MEMBER TYPES'//
     +    REPEAT(' ',19)//'Matl'//
     +    REPEAT(' ',7)//'Ax'//
     +    REPEAT(' ',9)//'Ay'//
     +    REPEAT(' ',10)//'Iz'//
     +    unBold$//CR$
          ILINE=ILINE+4
      END IF

      DO IPrp=iPrp,NMTYP
          WRITE(TxtLN,'(I4,'': '',A24,I4,F12.3,2E12.3)')
     +         IPrp,
     +         PROPS(IPrp)%Desc,
     +         PROPS(IPrp)%Matl,
     +         PROPS(IPrp)%AreaX,
     +         PROPS(IPrp)%AreaY,
     +         PROPS(IPrp)%InrtZ
          IF(LU.EQ.0) THEN !                         LISTING --> CONSOLE
              CALL SWGTXT(idSCR,TRIM(TxtLN))
          ELSE             !                         LISTING --> OUTFILE
              CALL nPAGE(1,NewPg)
                 IF(NewPg) THEN
                    NewPg = .FALSE.
                    GO TO 201
                 END IF
              WRITE(LU,'(A)') REPEAT(' ',iTAB)//TRIM(TxtLN)//CR$
              ILINE=ILINE+1
          END IF
      END DO

      IF(LU.EQ.0)CALL SWGTXT(idSCR,REPEAT('-',81))
C     END IF !(GoodPP)


C     LIST NODE COORDINATES
C     ---------------------
C     IF(GoodNOD) THEN
C     IF(.TRUE.) THEN
C     iTAB=(PRLINW-75)/2
      iNODE = 1

301   IF(LU.EQ.0) THEN !                            CAPTIONS --> CONSOLE
          WRITE(TxtLN,'(''NODES'',10X,''X'',10X,''Y'',6X,
     +    ''Rigid-supports   Spring-X   Spring-Y   Spring-Z'')')
          CALL SWGTXT(idSCR,TRIM(TxtLN))
      ELSE             !                            CAPTIONS --> OUTFILE
          CALL nPAGE(MIN(2*PRLIPP/7,NNODE+4),NewPg)
          WRITE(LU,'(A)') CR$//CR$//CR$//Bold$//
     +    REPEAT(' ',iTAB)//'NODES'//
     +    REPEAT(' ',10)//'X'//
     +    REPEAT(' ',10)//'Y'//Small$//
     +    REPEAT(' ',7)//'Rigid-supports'//
     +    REPEAT(' ',3)//'Spring-X'//
     +    REPEAT(' ',7)//'Spring-Y'//
     +    REPEAT(' ',6)//'Spring-Z'//unSmall$//
     +    unBold$//CR$
          ILINE=ILINE+4
      END IF !(LU.EQ.0)

      DO iNode=iNODE,NNODE
         NodRes = ''
         SprRes = ''

         DO iDoF=1,3
            IF( (RCODEd .AND. KFIXD(IDOFN(INODE,iDoF)).LT.0)
     +           .OR.
     +          (.NOT.(RCODEd) .AND. (KFIXD(IDOFN(INODE,iDoF)).EQ.1)) )
     +      THEN
               NodRes=TRIM(NodRes)//ADOF(iDoF:iDoF)
            ELSE
               NodRes=TRIM(NodRes)//' '
            END IF !RCODEd,KFIXD
            WRITE(SprRes(iDoF),'(9X)')
         END DO !iDoF=1,3

         SprRes = REPEAT('-',9)

         DO iSpr=1,NSPRI
            IF(SPRING(iSpr)%Node.EQ.iNode) THEN
               IF(SPRING(iSpr)%DoF.EQ.1)
     +         WRITE(SprRes(1),'(E9.3E1)') SPRING(iSpr)%Stiff
               IF(SPRING(iSpr)%DoF.EQ.2)
     +         WRITE(SprRes(2),'(E9.3E1)') SPRING(iSpr)%Stiff
               IF(SPRING(iSpr)%DoF.EQ.3)
     +         WRITE(SprRes(3),'(E9.3E1)') SPRING(iSpr)%Stiff
            END IF !(SPRING(iSpr)%Node.EQ.iNode)
         END DO !iSpr=1,NSPRI

         WRITE(TxtLN,'(I4,'':  '',2F11.3, 7X,A3,3X,3(2X,A9))')
     +   INode, NODE(1,INode), NODE(2,INode), NodRes, (SprRes(i),i=1,3)

         IF(LU.EQ.0) THEN !                          LISTING --> CONSOLE
             CALL SWGTXT
     +           (idSCR,TRIM(TxtLN(1:36)//TxtLN(35:43)//TxtLN(40:74)))
         ELSE             !                          LISTING --> OUTFILE
             CALL nPAGE(1,NewPg)
             IF(NewPg) THEN
                NewPg = .FALSE.
                GO TO 301
             END IF !(NewPg)
             WRITE(LU,'(A)') REPEAT(' ',iTAB)//TRIM(TxtLN)//CR$
             ILINE=ILINE+1
         END IF !(LU.EQ.0)

      END DO !Inode

      IF(LU.EQ.0) CALL SWGTXT(idSCR,REPEAT('-',81))
C     END IF !(GoodNOD)


C     LIST MEMBER CONNECTIONS
C     -----------------------
C     IF(GoodMEM) THEN
C     IF(.TRUE.) THEN
C     iTAB=(PRLINW-75)/2
      iMem = 1

401   IF(LU.EQ.0) THEN !                            CAPTIONS --> CONSOLE
          WRITE(TxtLN,'(''MEMBERS'',25X,
     +      ''Node1 [rel]'', 4X,
     +      ''TYPE'',4X,
     +      ''[rel] Node2'')')
          CALL SWGTXT(idSCR,TRIM(TxtLN))
      ELSE             !                            CAPTIONS --> OUTFILE
          CALL nPAGE(MIN(2*PRLIPP/7,NMEMB+4),NewPg)
          WRITE(LU,'(A)') CR$//CR$//CR$//Bold$//
     +    REPEAT(' ',iTAB)//'MEMBERS'//
     +    REPEAT(' ',25)//'Node1 [rel]'//
     +    REPEAT(' ',4)//'TYPE'//
     +    REPEAT(' ',4)//'[rel] Node2'//
     +    unBold$//CR$
          ILINE=ILINE+4
      END IF !(LU.EQ.0)

      DO IMem=iMem,NMEMB
        Rel1=aIF(MEMB(IMem)%Released(1),'x$','$')
        Rel2=aIF(MEMB(IMem)%Released(2),'y$','$')
        Rel3=aIF(MEMB(IMem)%Released(3),'z$','$')
        Rel4=aIF(MEMB(IMem)%Released(4),'x$','$')
        Rel5=aIF(MEMB(IMem)%Released(5),'y$','$')
        Rel6=aIF(MEMB(IMem)%Released(6),'z$','$')

         WRITE ( TxtLN,
     +          '(I4,'': '',A24,
     +            I6,3X,A3,
     +            I8,6X,
     +            A3,3X,A6)' )
     +          IMem,MEMB(IMem)%Desc(1:24),
     +          MEMB(IMem)%Node1,
     +          ADJUSTL(TRIM(Rel1)//TRIM(Rel2)//TRIM(Rel3)//'   '),
     +          MEMB(IMem)%MTyp,
     +          ADJUSTR(TRIM(Rel4)//TRIM(Rel5)//TRIM(Rel6)),
     +          ADJUSTL(I2CHAR(MEMB(IMem)%Node2,idum))
         IF(LU.EQ.0) THEN !                          LISTING --> CONSOLE
             CALL SWGTXT(idSCR,TRIM(TxtLN))
         ELSE             !                          LISTING --> OUTFILE
             CALL nPAGE(1,NewPg)
             IF(NewPg) THEN
                NewPg = .FALSE.
                GO TO 401
             END IF !(NewPg)
             WRITE(LU,'(A)') REPEAT(' ',iTAB)//TRIM(TxtLN)//CR$
             ILINE=ILINE+1
         END IF !(LU.EQ.0)

      END DO !IMem=iMem,NMEMB

      IF(LU.EQ.0)CALL SWGTXT(idSCR,REPEAT('-',81))
C     END IF !(GoodMEM)




C     LIST LOADS
C     ----------
C     IF(GoodLOD) THEN
      IF(NLCAS.GT.0) THEN
        IF(LU.NE.0) THEN
            CALL nPAGE(PRLIPP,NewPg)
            iTAB=(PRLINW-81)/2
        END IF !(LU.NE.0)
        iLCas = 1
        MoreLC = .TRUE.

        LCNam = LCName(iLCas)

C       LOADCASE LOOP
        DO WHILE(MoreLC)
            iLod = 1
            MoreLOD = .TRUE.
501         IF(LU.EQ.0) THEN !                      CAPTIONS --> CONSOLE
C               LC NAME
                CALL SWGTXT(idSCR,'')
                CALL SWGTXT(idSCR,'LOAD CASE '//
     +            TRIM(I2CHAR(iLCas,idum))//
     +            ': '//TRIM(LCName(iLCas)))

                CALL SWGTXT(idSCR,
     +          'NodeNo. -or-   Description          Axes Load       '//
     +          ' Actions -or- displacements')

                CALL SWGTXT(idSCR,
     +          'Memb.proprtn   of the load          G/L  type      '//
     +          'Fx/wX/dX   Fy/wY/dY   Mz/mZ/rZ')

                CALL SWGTXT(idSCR,
     +          '-------------|---------------------|----|-------|'//
     +          '----------|----------|----------')
            ELSE           !                        CAPTIONS --> OUTFILE
                DO KOUNT=1,PRLIPP
                  IF(ABS(LOADCASE(ILCas,iLod+KOUNT-1)%Pos).EQ.0.0) EXIT
                END DO !KOUNT=1,PRLIPP
                CALL nPAGE(MIN(2*PRLIPP/7,KOUNT+7),NewPg)
                WRITE(LU,'(A)')
     +          CR$//CR$//CR$//
     +          Bold$//
     +          REPEAT(' ',iTAB)//
     +          'LOAD CASE '//TRIM(I2CHAR(iLCas,idum))//
     +          ': '//TRIM(LCName(iLCas))//CR$//
     +          unBold$//
     +          REPEAT(' ',iTAB)//
     +          'NODE[or]     Description          Load  Load       '//
     +          'Actions -or- displacements'//CR$//
     +          REPEAT(' ',iTAB)//'MEMB.'//Small$//
     +          'proportn'//unSmall$//
     +          '  of the load'//REPEAT(' ',10)//'axes  type     '//
     +          'F'//SubScrip$//'x'//unSubScrip$//
     +          '/w'//SubScrip$//'X'//unSubScrip$//
     +          '/d'//SubScrip$//'X'//unSubScrip$//'   '//
     +          'F'//SubScrip$//'y'//unSubScrip$//
     +          '/w'//SubScrip$//'Y'//unSubScrip$//
     +          '/d'//SubScrip$//'Y'//unSubScrip$//'   '//
     +          'M'//SubScrip$//'z'//unSubScrip$//
     +          '/m'//SubScrip$//'Z'//unSubScrip$//
     +          '/r'//SubScrip$//'Z'//unSubScrip$//'   '//CR$//
     +          REPEAT(' ',iTAB)//
     +          '-----------|---------------------|----|--------|'//
     +          '----------|----------|----------'//
     +          CR$

                ILINE=ILINE+7
            END IF !(LU.EQ.0)

            Pos = LOADCASE(ILCas,iLod)%Pos

C           LOAD LOOP
            DO WHILE(MoreLOD)

                 IF(ABS(Pos-NINT(Pos)).LT. 2*TINY(Pos) ) THEN
                    WRITE(TxtLN,'(I4)') NINT(POS)
                 ELSE
                    TxtLN=AdjustL(R2CHAR(POS,idum,'F10.4 '))
                    DO ii = LEN(TxtLN),INDEX(TxtLN,'.'),-1
                       IF(SCAN(TxtLN(ii:ii),' 0').NE.0) EXIT
                       IF(TxtLN(ii:ii).EQ.'0') THEN
                          TxtLN(ii:ii)=' '
                       END IF
                    END DO
                    TxtLn = REPEAT(' ',5-(INDEX(TxtLn,'.')))//TxtLn
                 END IF

                 nSPC  = MAX(14-LEN_TRIM(TxtLN),0)
                 TxtLN = TRIM(TxtLN)//
     +                   REPEAT(' ',nSPC)//LOADCASE(iLCas,iLod)%Desc

                 IF(.NOT.
     +           ((TxtLN(1:1).EQ.'-')
     +           .AND.
     _           (LOADCASE(ILCas,iLod)%Fx .EQ. 0.0)
     +           .AND.
     _           (LOADCASE(ILCas,iLod)%Fy .EQ. 0.0)
     +           .AND.
     _           (LOADCASE(ILCas,iLod)%Mz .EQ. 0.0)) ) THEN

                    Axes   = LOADCASE(ILCas,iLod)%Axes
                    LTyp   = LOADCASE(ILCas,iLod)%LTyp
                    TDLshp = LOADCASE(ILCas,iLod)%TDLshp

                    IF(SCAN(Axes,'Gg').GT.0) AxesW='G'
                    IF(SCAN(Axes,'Ll').GT.0) AxesW='L'

                    IF(SCAN(LTyp,'Ss').GT.0) LTypW='S)ettlmt'
                    IF(SCAN(LTyp,'Nn').GT.0) LTypW='N)ode Ld'
                    IF(SCAN(LTyp,'Uu').GT.0) LTypW='U)DL'
                    IF(SCAN(LTyp,'Tt').GT.0) THEN
                      WRITE(TxtLN(5:5),'(A)') TDLshp
                      LTypW='T)DL'//':'//TDLshp
                    END IF
                    IF(SCAN(LTyp,'Dd').GT.0) LTypW='D)ensity'
                    IF(SCAN(LTyp,'Pp').GT.0) LTypW='P)ointLd'

                    Fx = LOADCASE(ILCas,iLod)%Fx
                    Fy = LOADCASE(ILCas,iLod)%Fy
                    Mz = LOADCASE(ILCas,iLod)%Mz

                    IF(ABS(Fx).GT.2.0*TINY(Fx)) THEN
                       IF(ABS(Fx).LT.1.0E06) THEN
                          Write(aFx,10) Fx
                       ELSE
                          Write(aFx,11) Fx
                       END IF
                    ELSE
                       aFx=REPEAT(' ',11)
                    END IF

                    IF(ABS(Fy).GT.2.0*TINY(Fy)) THEN
                       IF(ABS(Fy).LT.1.0E06) THEN
                          Write(aFy,10) Fy
                       ELSE
                          Write(aFy,11) Fy
                       END IF
                    ELSE
                       aFy=REPEAT(' ',11)
                    END IF

                    IF(ABS(Mz).GT.2.0*TINY(Mz)) THEN
                       IF(ABS(Mz).LT.1.0E06) THEN
                          Write(aMz,10) Mz
                       ELSE
                          Write(aMz,11) Mz
                       END IF
                    ELSE
                       aMz=REPEAT(' ',11)
                    END IF

10                  FORMAT(F11.3)
11                  FORMAT(ES11.3)

                    nSPC  = MAX(35-LEN_TRIM(TxtLN),0)

                    TxtLN = TRIM(TxtLN)//REPEAT(' ',nSPC)//
     +              AxesW//'   '//LTypW//aFx//aFy//aMz

                    IF(LU.EQ.0) THEN !               LISTING --> CONSOLE
                       CALL SWGTXT
     +                      (idSCR,TRIM(TxtLN(1:35)//'  '//
     +                       TxtLN(36:36)//TxtLN(37:48)//TxtLN(50:80)))
                    ELSE             !               LISTING --> OUTFILE
                        IF(ABS(LOADCASE(ILCas,iLod+1)%Pos).GT.0)
     +                  CALL nPAGE(1,NewPg)
                        IF(NewPg) THEN
                            NewPg = .FALSE.
                            GO TO 501
                        END IF
                        WRITE(LU,'(A)') REPEAT(' ',iTAB)//
     +                        TRIM(TxtLN)//CR$
                        ILINE=ILINE+1
                    END IF
                 END IF

                 iLod=iLod+1
                 Pos = LOADCASE(ILCas,iLod)%Pos
                 IF(abs(Pos).GT.TINY(Pos)) CYCLE

C                Pos=0 means there are no more loads in this LC
                 MoreLod = .FALSE.

            END DO !while MoreLod
C           End of LOAD loop

            IF(LU.EQ.0)CALL SWGTXT(idSCR,REPEAT('-',81))

            iLCas=iLCas+1
            IF(iLCas.LE.NLCAS) CYCLE
C           IF(LEN_TRIM(LCName(iLCas)).GT.0) CYCLE

C           Len_Trim=0 means there are no more load cases
            MoreLC=.FALSE.

        END DO !WHILE(MoreLC)
C       End of Loadcase loop

        CALL SWGTXT(idSCR,' ')
      END IF !(NLCAS.GT.0)
C     END IF !GoodLOD




C     LIST LOAD COMBINATIONS
C     ----------------------
C     IF(GoodCOMB) THEN
      IF(NCOMB.GT.0 .AND. LU.EQ.0) THEN
        !No need to list the specified combination at this point
        !in the output to the results file because they are defined
        !in the heading for each of the combination results tables

        DO iCOMB=1,NCOMB
            !LD COMBS CAPTIONS--> CONSOLE
C           LComb NAME
            CALL SWGTXT(idSCR,'')
            CALL SWGTXT(idSCR,'LOAD COMBINATION '//
     +        TRIM(I2CHAR(iCOMB,idum))//
     +        ': '//TRIM(CombName(iCOMB)))

            CALL SWGTXT(idSCR,'  Load case'//REPEAT(' ',27)//'Factor')

            CALL SWGTXT(idSCR,
     +      REPEAT('-',35)//'|----------')

            DO ILCASC=1,NLCASC
                LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C               LCName=LCName(LCNum)
                Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                IF(LCNum.EQ.0) EXIT

                WRITE(OutpCOMB,1015)LCNum, LCName(LCNum), Factr
1015            FORMAT(I3, ': ', A30, F9.3)
                CALL SWGTXT(idSCR,TRIM(OutpCOMB))    !<- CONSOLE COMBlist
            END DO !ILCASC=1,NLCASC

        END DO !iCOMB=1,NCOMB
      END IF !(NCOMB.GT.0 .AND. LU.EQ.0)

      CALL SWGTXT(idSCR,' ')
      CALL SWGTXT(idSCR,' ')
C     END IF !GoodCOMB


C     IF(LU.EQ.0) CALL DrawSCR

      RETURN
      END
