	  SUBROUTINE rLOADS
C     -----------------
C     jw / 25-05-12  draft
C     jw / 30-05-12  last mod

C     Read loads from screen forms
C     ----------------------------

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING

      CALL HidePANES
      CALL GhostMenus

      iLCas = 0
      SOLVED = .FALSE.
      CALL WinLOD('SHOW')

      RETURN
      END





      SUBROUTINE WinLOD(ACTN)
C     -----------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

C     Window layout for Load input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      EXTERNAL  ChgLoadCase,
     +          CheckLOADS,
     +          CloseLod,
     +          MoreLoads,
     +          Help_Loading,
     +          Help_LoadTypes

      REAL      LCNameWidths(2),  ColWidths(7)

      CHARACTER ACTN*4, LCList*1300

      DATA      LCNameWidths
     +         /-17.,  -83. /
C               Label LCName

      DATA      ColWidths
     +         /-17.,  -17.,  -3.,  -31.,  -10., -10., -10. /
C                POS   DESC   AXES  LTYP    FX,   FY,   MZ

C     PANES

C     +---------------------------------+
C     | +---------------+ +-----------+ |
C     | |               | |           | |
C     | |               | |     3     | |
C     | |       1       | |           | |
C     | |               | +-----------+ |
C     | |               | +-----------+ |
C     | |               | |     4     | |
C     | +---------------+ +-----------+ |
C     | +-----------------------------+ |
C     | |              2              | |
C     | +-----------------------------+ |
C     +---------------------------------+

      LCList = ''

      IF(.NOT.ALLOCATED(LOADCASE)) THEN
         NLCAS = MAX(NLCAS,2) !No of load cases
         NLOAD = MAX(NLOAD,5) !No of load entries allowed for in each LC
C        NLOAD=(NMEMB+NNODE)*2!Default no. of load entries per loadcase
         CALL ALLOC2
         iLCas = 1
      END IF

10    SELECT CASE(ACTN)

C-----------------------------------------------------------------------
         CASE('SHOW')
C-----------------------------------------------------------------------
          CALL WGAPP(MenuHG,'Loading',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_Loading)

          CALL WGAPP(MenuHG,'Load Types',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_LoadTypes)

         ShowsLOD = .TRUE.

C           Make Loads data entry fields
C-----------------------------------------------------------------------
C           Use Pane 4 for the load case title and selector
               CALL SWGPOS(Pan4L,Pan4T)
               CALL WGBAS(Win1,'HORI',idLCSelPAN)
C-----------------------------------------------------------------------
C           Divide pane 4 into two columns
C           Column 1
               CALL SWGWTH(-30)
               CALL WGBAS(idLCSelPAN,'VERT',idLCSelPAN1) !LC title

C           Column 2
               CALL SWGWTH(-9)
               CALL WGBAS(idLCSelPAN,'VERT',idLCSelPAN2) !LC selector
C-----------------------------------------------------------------------
C           One-cell table for the load case name in pane 4, column 1
               CALL SWGRAY(LCNameWidths,2,'TABLE')
               CALL SWGOPT('ROWS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idLCSelPAN1,1,2,idLCNameTbl)
               CALL SWGTBS(idLCNameTbl,'LC name',1,0,'VALUE')
C-----------------------------------------------------------------------
C           Loadcase no. selector button in pane 4, column  2
            WRITE(LCList, 100) (jLCas, jLCas=1,NLCAS)
100         FORMAT (100('Load case',I3,'|'))

            CALL WGDLIS(idLCSelPAN2,LCList,iLCas,idLCas)
            CALL SWGCBK(idLCas, ChgLoadCase)
C-----------------------------------------------------------------------
C           Use Pane 2 for the loads pane and main action buttons
            CALL SWGPOS(Pan2L,Pan2T)
            CALL WGBAS(Win1,'HORI',idLodPAN)
C-----------------------------------------------------------------------
C           Divide pane 2 into two columns
C           Column 1 (loads table)
            CALL SWGWTH(-83)
            CALL WGBAS(idLodPAN,'VERT',idLodPAN1) !Data

C           Column 2 (actions buttons)
            CALL SWGWTH(-7)
            CALL WGBAS(idLodPAN,'VERT',idLodPAN2) !Buttons
C-----------------------------------------------------------------------
C           LOADS input table in the load pane, column 1
            CALL SWGFNT('Arial',NINT(Float(Pan1W)/42.0))
            CALL SWGRAY(ColWidths,7,'TABLE')
            CALL SWGOPT('COLUMNS', 'HEADER')
            CALL SWGOPT('ON', 'EDIT')
            CALL SWGJUS('LEFT', 'TABLE')
            CALL WGTBL(idLodPAN1,MAX(1,NLOAD),7,idLodTbl)
            CALL SWGTBS(idLodTbl,
     +           ' Node or Member[.propn / i / j / m ]',    0,1,'VALUE')
            CALL SWGTBS(idLodTbl,' Load description',0,2,'VALUE')
            CALL SWGTBS(idLodTbl,' G/L',             0,3,'VALUE')
            CALL SWGTBS (idLodTbl,
     +           ' N(ode Ld, P(ointLd, U(DL, T(DL, D(ensity S(ettlemt',
     +                                               0,4,'VALUE')
            CALL SWGTBS(idLodTbl,' F(x) or dx',      0,5,'VALUE')
            CALL SWGTBS(idLodTbl,' F(y) or dy',      0,6,'VALUE')
            CALL SWGTBS(idLodTbl,' M(z) or dz',      0,7,'VALUE')

            CALL ShowLOADS
C-----------------------------------------------------------------------
C           BUTTONS in the load pane, column 2

            CALL SWGFNT('Arial',NINT(Float(Pan1W)/38.0))

            CALL WGPBUT(idLodPAN2,'Accept',idOK)
            CALL SWGCBK(idOK, CheckLOADS)

            CALL WGPBUT(idLodPAN2,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseLod)

            CALL WGPBUT(idLodPAN2,'More entries',idMoreLoads)
            CALL SWGCBK(idMoreLoads, MoreLoads)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsLOD = .FALSE.
         IF(id0.GT.0) CALL SWGATT(id0,'INVISIBLE','STATUS')
         IF(id1.GT.0) CALL SWGATT(id1,'INVISIBLE','STATUS')
         CALL SWGATT(idLodPAN,   'INVISIBLE','STATUS')
         CALL SWGATT(idLodPAN1,  'INVISIBLE','STATUS')
C        CALL SWGATT(idLodPAN1,  'INACTIVE','STATUS')
C        CALL SWGATT(idLodTbl,   'INACTIVE','STATUS')
         CALL SWGATT(idLodPAN2,  'INVISIBLE','STATUS')
         CALL SWGATT(idLCas,     'INVISIBLE','STATUS')
         CALL SWGATT(idOK,       'INVISIBLE','STATUS')
C        CALL SWGATT(idCANCEL,   'INVISIBLE','STATUS')
C        CALL SWGATT(idMoreLoads,'INVISIBLE','STATUS')

      END SELECT !(ACTN)

      IF(ChangedLC) GO TO 10

      RETURN
      END





      SUBROUTINE MoreLOADS
C     ---------------------
C     jw / 22-01-14  draft
C     jw / 22-01-14  last revised

      USE DISLIN
      USE PARAMS
      USE LOADING
      USE WinStuff

      CHARACTER I2CHAR*16

      CALL Alloc2Plus('NLOAD',MIN(1+NLOAD/4,8))
      CALL DWGMSG('Increasing load table length to '//
     +            TRIM(I2CHAR(NLOAD,iDUM))//' rows.')
      CALL SWGATT(idLodTbl, 'INVISIBLE','STATUS')

      CALL rLOADS

      RETURN
      END






      SUBROUTINE CheckLOADS
C     ---------------------
C     jw / 29-01-12  draft
C     jw / 29-01-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      REAL*4       Pos,Fx,Fy,Mz
      CHARACTER*1  AXES,LTYP,TDLshp
      CHARACTER    NMT*6
      CHARACTER*16 R2CHAR
      LOGICAL      LoadDataGen  ! set when load entry begins with a
                                ! hyphen to signify last Node or member
                                ! in a load data generation sequence

      CHARACTER I2CHAR*8

C     IF(ILCAS.EQ.NLCAS) CALL StorLOADS
      GoodLOD = .TRUE.
      IF(NLCAS.NE.0       !because some loads have been read
     +      .AND.
     +  idLCNameTbl.NE.0) !because user has already used load entry menu
     +THEN
          CALL StorLOADS
      ELSE
          MORE = .TRUE.
          ILCAS=ILCAS-1
          IF(ILCAS.LT.0) THEN
             GoodLOD = .FALSE.
             CALL SENDOK
          ENDIF
          RETURN
      END IF !(NLCAS.NE.0 .AND. idLCNameTbl.NE.0)

      GoodLOD = .TRUE.

         DO iLCas=1,NLCAS
            DO iLod = 1,NLOAD
               LoadDataGen = .FALSE.
               Pos  = (LOADCASE(iLCas,iLod)%Pos) !NM.Propn poss with '-'
               IF(INT(POS).EQ.0) THEN
                  IF(iLCas.EQ.1 .AND. iLod.EQ.1) GoodLOD = .FALSE.
                  EXIT
               END IF

               If(Pos.LT.0.0) LoadDataGen = .TRUE.
               Pos   = ABS(Pos)               !NM.Propn
               NM    = INT(Pos)               !Node or member number
               Propn = Pos - FLOAT(NM)

               Axes = LOADCASE(iLCas,iLod)%Axes
               LTyp = LOADCASE(iLCas,iLod)%LTyp
               IF(SCAN(LTyp,'Tt').GT.0)
     +         TDLshp = LOADCASE(iLCas,iLod)%TDLshp
               Fx   = LOADCASE(iLCas,iLod)%Fx
               Fy   = LOADCASE(iLCas,iLod)%Fy
               Mz   = LOADCASE(iLCas,iLod)%Mz

               IF(SCAN(LTyp,'NnSs').GT.0) THEN
                  NMT = 'node  '
               ELSE IF(SCAN(LTyp,'PpUuDdTt').GT.0) THEN
                  NMT = 'member'
               ELSE
                  CALL BailOUT('ERROR|Load on node or member '//
     +            TRIM(I2CHAR(INT(Pos),idum))//' '//
     +            'has unrecognisable load type.||'//
     +            'Valid load types are:|'//
     +            '  N = Node load;|'//
     +            '  P = Point load on a member;|'//
     +            '        (put position along member in member no.)|'//
     +            '  S = Support settlmt (any prescribed displacmt);|'//
     +            '  U = Uniformly distributed load;|'//
     +            '  T = Triangularly distributed load;|'//
     +            '        (also needs ''i'', ''m'' or ''j'' straight'//
     +            ' after the memb.no.)|'//
     +            '  D = Density (auto-self weight).$',
     +            'OPTIONS','YES')
                  GoodLOD = .FALSE.
               END IF

               IF(SCAN(Axes,'GgLl').EQ.0) THEN
                  CALL BailOUT('ERROR|Load on '//TRIM(NMT)//' '//
     +            TRIM(I2CHAR(INT(Pos),idum))//' '//
     +            'has unrecognisable axes.|Use G or L.$',
     +            'OPTIONS','YES')
                  GoodLOD = .FALSE.
               END IF

               IF(NMT.EQ.'node  ' .AND. Propn.NE.0.0)
     +         THEN
                  CALL BailOUT('ERROR|Node load on '//TRIM(NMT)//' '//
     +            TRIM(I2CHAR(NM,idum))//' '//
     +            'cannot have a member proportion.|'//
     +            'Enter the node number as '//
     +             TRIM(I2CHAR(NM,idum))//' instead of '//
     +             TRIM(I2CHAR(NM,idum))//'.'//
     +             R2CHAR(Propn,nDum,'F4.3  ')//
     +            ' or use load type P.$',
     +            'OPTIONS','YES')
                  GoodLOD = .FALSE.
               END IF

               IF(NMT.EQ.'member' .AND. SCAN(LTyp,'PpUuDdTt').EQ.0)
     +         THEN
                  CALL BailOUT('ERROR|Load on '//TRIM(NMT)//' '//
     +            TRIM(I2CHAR(INT(Pos),idum))//' '//
     +            'has unrecognisable load type.||'//
     +            'For MEMBER loads, use:|'//
     +            '  P = Point load on a member;|'//
     +            '        (put position along member in member no.)|'//
     +            '  U = Uniformly distributed load;|'//
     +            '  T = Triangularly distributed load;|'//
     +            '        (also needs ''i'', ''m'' or ''j'' straight'//
     +            ' after the memb.no.)|'//
     +            '  D = Density (auto-self weight).$',
     +            'OPTIONS','YES')
                  GoodLOD = .FALSE.
               END IF

               IF(SCAN(LTyp,'Tt').GT.0 .AND.
     +            SCAN(TDLshp,'IiLlJjRrMm ').EQ.0)
     +         THEN
                  CALL BailOUT('ERROR|'//
     +            'Triangularly distributed load on member '//
     +            TRIM(I2CHAR(INT(Pos),idum))//' '//
     +            'has unrecognisable load orientation.||'//
     +            'For ''TDL'' loads, put one of the following '//
     +            'immediately after the member number:|'//
     +            '  i    : Triangle apex at member ''i-end'';|'//
     +            '  m : Triangle apex at member mid-point;|'//
     +            '  j    : Triangle apex at member ''j-end''.$',
     +            'OPTIONS','YES')
                  GoodLOD = .FALSE.
               END IF

               IF((LTyp.NE.'D' .AND. LTyp.NE.'d') .AND.
     +         (ABS(Fx)+ABS(Fy)+ABS(Mz).EQ.0.0) ) THEN
                  CALL BailOUT('ERROR|'//TRIM(NMT)//' '//
     +            TRIM(I2CHAR(INT(Pos),idum))//' '//
     +            'has no load applied to it.$',
     +            'OPTIONS','YES')
                  GoodLOD = .FALSE.
               END IF
            END DO !iLod
         END DO !iLCas
C     END DO

      IF(.NOT.GoodLOD) THEN
C        RETURN leaving the error messages still displayed and the
C        PROPERTIES data entry fields on screen for further data
         CALL SCRmsg(REPEAT('-',80),.FALSE.)
         RETURN
      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
C        ILCAS=ILCAS-1
C        IF(ILCAS.EQ.0) GOODLOD = .FALSE.

         CALL SENDOK

      END IF

      RETURN
      END




      SUBROUTINE ShowLOADS
C     --------------------
C     jw / 29-01-12  draft
C     jw / 30-05-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      REAL*4 Pos,Fx,Fy,Mz
      CHARACTER APos*10

      IF(iLCas.LT.1) iLCas=1

C     LOAD CASE NUMBER
C     Copy current LC number into the on-screen table
      CALL SWGLIS(idLCas,iLCas)

C     LOAD CASE NAME
C     Copy stored LC title into the on-screen table
      CALL SWGTBS(idLCNameTbl,LCName(iLCas),1,1,'VALUE')

C     LOAD CASE DATA
C     Copy stored values into the on-screen table
      DO iLod=1,NLOAD
         Pos = LOADCASE(iLCas,iLod)%Pos
         IF(Pos.NE.0.0) THEN
            DECI=ABS(Pos-INT(Pos))
            IF(DECI.LT.0.000001) DECI=0.0
            IF(DECI.NE.0.0) THEN
                WRITE(APos,'(F10.3)') Pos
            ELSE
                WRITE(Apos,'(I8)') INT(Pos)
                APos=TRIM(ADJUSTL(APos))//LOADCASE(iLCas,iLod)%TDLshp
            END IF !(DECI.NE.0.0)
            CALL SWGTBS
     +      (idLodTbl,APos,iLod,1,'VALUE')
         ELSE
            CALL SWGTBS
     +      (idLodTbl,'',iLod,1,'VALUE')
         END IF

         CALL SWGTBS
     +   (idLodTbl,LOADCASE(iLCas,iLod)%Desc,  iLod,2,'VALUE')


         CALL SWGTBS
     +   (idLodTbl,LOADCASE(iLCas,iLod)%Axes,  iLod,3,'VALUE')


         CALL SWGTBS
     +   (idLodTbl,LOADCASE(iLCas,iLod)%LTyp,  iLod,4,'VALUE')


         Fx = LOADCASE(iLCas,iLod)%Fx
         IF(Fx.NE.0.0) THEN
            CALL SWGTBF
     +      (idLodTbl,Fx, -2,iLod,5,'VALUE')
         ELSE
            CALL SWGTBS
     +      (idLodTbl,'',iLod,5,'VALUE')
         END IF

         Fy = LOADCASE(iLCas,iLod)%Fy
         IF(Fy.NE.0.0) THEN
            CALL SWGTBF
     +      (idLodTbl,Fy, -2,iLod,6,'VALUE')
         ELSE
            CALL SWGTBS
     +      (idLodTbl,'',iLod,6,'VALUE')
         END IF

         Mz = LOADCASE(iLCas,iLod)%Mz
         IF(Mz.NE.0.0) THEN
            CALL SWGTBF
     +      (idLodTbl,Mz, -2,iLod,7,'VALUE')
         ELSE
            CALL SWGTBS
     +      (idLodTbl,'',iLod,7,'VALUE')
         END IF

      END DO

      RETURN
      END




      SUBROUTINE StorLOADS
C     --------------------
C     jw / 29-01-12  draft
C     jw / 30-05-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      REAL*4       Pos,Fx,Fy,Mz
      CHARACTER*1  AXES,LTYP, TDLshp
      CHARACTER*10 APos
      CHARACTER*30 LoadCaseName
      CHARACTER*21 Desc

C     Read load case name from the 1-cell data-entry table
      CALL GWGTBS(idLCNameTbl,1,1,LoadCaseName)

C     and store it in memory array
      LCName(iLCas) = LoadCaseName

      DO iLod = 1,NLOAD

C        Read iLod from the on-screen data-entry table and store it
         Pos  = 0
         TDLshp = ' '
         CALL GWGTBS(idLodTbl,iLod,1,APos)
         APos=ADJUSTL(Apos)
         LPos = LEN_TRIM(APos)
         IF(LPos.EQ.0) EXIT
         jTDLshp=SCAN(APos,'IiLlJjRrMm')
         IF(jTDLshp.EQ.0) THEN
              APos=ADJUSTR(APos)
              READ(APos,'(F10.0)') Pos
         ELSE
              TDLshp = APos(jTDLshp:jTDLshp)
              READ(APos(1:jTDLshp-1),*) Pos
         END IF !(jTDLshp.EQ.0)
         LOADCASE(iLCas,iLod)%Pos = Pos

         Desc  = REPEAT(' ',21)
         CALL GWGTBS(idLodTbl,iLod,2,Desc(1:21))
         DESC=ADJUSTL(Desc)
         DESC=TRIM((Desc))
         LOADCASE(iLCas,iLod)%Desc = Desc

         Axes  = ' '
         CALL GWGTBS(idLodTbl,iLod,3,Axes)
         LOADCASE(iLCas,iLod)%Axes = Axes

         LTyp  = ' '
         CALL GWGTBS(idLodTbl,iLod,4,LTyp)
         LOADCASE(iLCas,iLod)%LTyp = LTyp

         IF(LTyp.EQ.'T' .OR. LTyp.EQ.'t') THEN
             IF(SCAN(TDLshp,'IiLl').GT.0) THEN
                 LOADCASE(iLCas,iLod)%TDLshp = 'i'
             ELSE IF(SCAN(TDLshp,'JjRr').GT.0) THEN
                 LOADCASE(iLCas,iLod)%TDLshp = 'j'
             ELSE
                 LOADCASE(iLCas,iLod)%TDLshp = 'm'
             END IF !(SCAN(TDLshp,'IiLlJjRr').GT.0)
         END IF !(LTyp.EQ.'T' .OR. LTyp.EQ.'t')
         Fx = 0.0
         CALL GWGTBF(idLodTbl,iLod,5,Fx)
         LOADCASE(iLCas,iLod)%Fx = Fx

         Fy = 0.0
         CALL GWGTBF(idLodTbl,iLod,6,Fy)
         LOADCASE(iLCas,iLod)%Fy = Fy

         Mz = 0.0
         CALL GWGTBF(idLodTbl,iLod,7,Mz)
         LOADCASE(iLCas,iLod)%Mz = Mz

      END DO

      RETURN
      END





      SUBROUTINE CloseLOD
C     -------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

      USE DISLIN
      USE WinSTUFF
      CALL WinLOD('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END




      SUBROUTINE ChgLoadCase
C     ----------------------
C     jw / 27-05-12  draft
C     jw / 29-05-12  last revised

C     On entry to this subroutine, the selected pull-down LC no. has
C     been changed by the user but not yet read by the program.
C     iLCas still contains the LC no. for the on-screen LC name, and
C     iLCas still contains the LC no. for the on-screen LC data.
C
C     BEFORE READING the selected pulldown LC no.:
C      - Read and store the currently-displayed LCName for iLCas
C      - Read and store the currently-displayed LC data for iLCas
C
C     Then read the selected pull-down LC no.
C     Make iLCas the changed LC
C
C     AFTER READING the selected pulldown LC no.:
C      - Display on-screen the stored LC name for the changed iLCas
C      - Display on-screen the stored LC data for the changed iLCas

      USE DISLIN
      USE PARAMS
      USE LOADING

      iL = iLCas
      NL = NLCAS

      CALL GWGLIS(idLCas,LCnum)    !Read the selected pulldown LC no.
      IF(LCnum.GT.NLCAS) THEN
        CALL ALLOC2PLUS('NLCAS',1) !add 1 to NLCAS
        IF(NLCAS.GT.0) NLCAS = NLCAS-1
        iLCas=iL                   !  Because ReadLOADS will have done a
        NLCAS=NLCAS+1              !  re-count when called by Alloc2Plus
      END IF

      CALL StorLOADS !With iLCas still containing the previous LC no.

      iLCas=LCnum    !Change iLcas to the selected LC no.
      CALL ShowLOADS !Whatever (if any) loads alreadt entered in LCNum

      RETURN
      END
