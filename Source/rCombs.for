      SUBROUTINE rCOMBS
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

      iCOMB = 0
      SOLVED = .FALSE.
      CALL WinCOMB('SHOW')
      RETURN
      END





      SUBROUTINE WinCOMB(ACTN)
C     ------------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

C     Window layout for Load input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      EXTERNAL  ChgCOMB,
     +          CheckCOMBS,
     +          CloseComb,
     +          MoreCombs,
     +          Help_LoadCombs

      REAL      CombNamWidths(2),  ColWidths(3)

      CHARACTER ACTN*4, CombList*1300

      DATA      CombNamWidths
     +         /-20.,  -80. /
C               Label CombNam

      DATA      ColWidths
     +         /-12.,  -69.,  -19. /
C               LComb   LdDESC   LFactor

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

      CombList = ''

      IF(.NOT.ALLOCATED(LOADCOMB)) THEN
         NCOMB  = MAX(NCOMB,2) !No of load COMBs
         NLCasC = MAX(NLCasC,5)!No of loadcases allowed for in each COMB
         CALL ALLOC3
         iCOMB = 1
      END IF

10    SELECT CASE(ACTN)

C-----------------------------------------------------------------------
         CASE('SHOW')
C-----------------------------------------------------------------------
         CALL WGAPP(MenuHG,'Load combinations',MenuHGN)
         CALL SWGCBK(MenuHGN, Help_LoadCombs)

         ShowsCOMB = .TRUE.

C           Make Combs data entry fields
C-----------------------------------------------------------------------
C           Use Pane 4 for the load combination title and selector
               CALL SWGPOS(Pan4L,Pan4T)
               CALL WGBAS(Win1,'HORI',idCombSelPAN)
C-----------------------------------------------------------------------
C           Divide pane 4 into two columns
C           Column 1
               CALL SWGWTH(-30)
               CALL WGBAS(idCombSelPAN,'VERT',idCombSelPAN1)!Comb title

C           Column 2
               CALL SWGWTH(-9)
               CALL WGBAS(idCombSelPAN,'VERT',idCombSelPAN2)!Comb select
C-----------------------------------------------------------------------
C           One-cell table for the load comb name in pane 4, column 1
               CALL SWGRAY(CombNamWidths,2,'TABLE')
               CALL SWGOPT('ROWS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idCombSelPAN1,1,2,idCombNamTbl)
               CALL SWGTBS(idCombNamTbl,'Name:',1,0,'VALUE')
C-----------------------------------------------------------------------
C           Load comb. no. selector button in pane 4, column  2
            WRITE(CombList, 100) (jLComb, jLComb=1,NCOMB)
100         FORMAT (100('Combination',I3,'|'))

            CALL WGDLIS(idCombSelPAN2,CombList,iCOMB,idComb)
            CALL SWGCBK(idComb, ChgCOMB)
C-----------------------------------------------------------------------
C           Use Pane 2 for the loads pane and main action buttons
            CALL SWGPOS(Pan2L,Pan2T)
            CALL WGBAS(Win1,'HORI',idCombPAN)
C-----------------------------------------------------------------------
C           Divide pane 2 into two columns
C           Column 1 (load cases table)
            CALL SWGWTH(-83)
            CALL WGBAS(idCombPAN,'VERT',idCombPAN1) !Data

C           Column 2 (actions buttons)
            CALL SWGWTH(-7)
            CALL WGBAS(idCombPAN,'VERT',idCombPAN2) !Buttons
C-----------------------------------------------------------------------
C           LOADCASES input table in combinations pane 2, column 1
            CALL SWGRAY(ColWidths,3,'TABLE')
            CALL SWGOPT('COLUMNS', 'HEADER')
            CALL SWGOPT('ON', 'EDIT')
            CALL SWGJUS('LEFT', 'TABLE')
            CALL WGTBL(idCombPAN1,MAX(1,NLCASC),3,idCombTbl)
            CALL SWGTBS(idCombTbl,' Load case',       0,1,'VALUE')
            CALL SWGTBS(idCombTbl,
     +                 ' Load description (don''t enter anything here)',
     +                  0,2,'VALUE')
            CALL SWGTBS(idCombTbl,' Load factor',     0,3,'VALUE')

            CALL ShowCOMBS
C-----------------------------------------------------------------------
C           BUTTONS in the combinations pane, column 2
            CALL WGPBUT(idCombPAN2,'Accept',idOK)
            CALL SWGCBK(idOK, CheckCOMBS)

            CALL WGPBUT(idCombPAN2,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseComb)

            CALL WGPBUT(idCombPAN2,'More entries',idMoreCombs)
            CALL SWGCBK(idMoreCombs, MoreCombs)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsCOMB = .FALSE.
         IF(id0.GT.0) CALL SWGATT(id0,'INVISIBLE','STATUS')
         IF(id1.GT.0) CALL SWGATT(id1,'INVISIBLE','STATUS')
         CALL SWGATT(idCombPAN,       'INVISIBLE','STATUS')
         CALL SWGATT(idCombPAN1,      'INVISIBLE','STATUS')
C        CALL SWGATT(idCombPAN1,      'INACTIVE','STATUS')
C        CALL SWGATT(idCombTbl,       'INACTIVE','STATUS')
         CALL SWGATT(idCombPAN2,      'INVISIBLE','STATUS')
         CALL SWGATT(idComb,          'INVISIBLE','STATUS')
         CALL SWGATT(idOK,            'INVISIBLE','STATUS')
C        CALL SWGATT(idCANCEL,        'INVISIBLE','STATUS')
C        CALL SWGATT(idMoreCombs,     'INVISIBLE','STATUS')

      END SELECT !(ACTN)

      IF(ChangedComb) GO TO 10

      RETURN
      END





      SUBROUTINE MoreCOMBS
C     --------------------
C     jw / 22-01-14  draft
C     jw / 22-01-14  last revised

      USE DISLIN
      USE PARAMS
      USE LOADING
      USE WinStuff


      EXTERNAL  ChgCOMB,
     +          CheckCOMBS,
     +          CloseComb

      CHARACTER I2CHAR*16

      CALL Alloc3Plus('NCOMB',MIN(1+NCOMB/4,8))
      CALL DWGMSG('Increasing load combination table length to '//
     +            TRIM(I2CHAR(NCOMB,iDUM))//' rows.')
      CALL SWGATT(idCombTbl, 'INVISIBLE','STATUS')

      CALL rCOMBS

      RETURN
      END






      SUBROUTINE CheckCOMBS
C     ---------------------
C     jw / 29-01-12  draft
C     jw / 29-01-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      LOGICAL LoadDataGen  ! set when load entry begins with a
                           ! hyphen to signify last Node or member
                           ! in a load data generation sequence
      CHARACTER I2CHAR*8

      iiCOMB = iCOMB
C     IF(iCOMB.EQ.NCOMB) CALL StorCOMBS
      GOODCOMB = .TRUE.
      IF(NCOMB.NE.0       !because some combs have been read
     +      .AND.
     +  idCombNamTbl.NE.0)!because user has already used comb entry menu
     +THEN
          CALL StorCOMBS
      ELSE
          MORE = .TRUE.
          iCOMB=iCOMB-1
          IF(iCOMB.LT.0) THEN
             GOODCOMB = .FALSE.
             CALL SENDOK
          ENDIF
          RETURN
      END IF !(NCOMB.NE.0 .AND. idCombNamTbl.NE.0)

      GOODCOMB = .TRUE.

      DO iCOMB=1,NCOMB
          DO iLcasC = 1,NLCASC
              LoadDataGen = .FALSE.

              LcNum = LOADCOMB(iCOMB,iLcasC)%LcNum
              Factr = LOADCOMB(iCOMB,iLcasC)%Factr

              IF(LcNum.LT.0.OR.LcNum.GT.NLCAS) THEN
                  CALL BailOUT('ERROR|In load combination '//
     +            TRIM(I2CHAR(INT(iCOMB),idum))//', load case '//
     +            TRIM(I2CHAR(INT(LcNum),idum))//
     +            ' is an unknown load case number.$',
     +            'OPTIONS','YES')
                  GOODCOMB = .FALSE.
              END IF

              IF(LcNum.NE.0 .AND. Factr.EQ.0.0) THEN
                  CALL BailOUT('WARNING|In load combination '//
     +            TRIM(I2CHAR(INT(iCOMB),idum))//', load case '//
     +            TRIM(I2CHAR(INT(LcNum),idum))//' '//
     +            'has no multiplying factor.$',
     +            'OPTIONS','YES')
                  GOODCOMB = .FALSE.
              END IF

          END DO !NLCASC
      END DO !NCOMB

      IF(.NOT.GOODCOMB) THEN
C        RETURN leaving the error messages still displayed and the
C        PROPERTIES data entry fields on screen for further data
         iCOMB = iiCOMB
         CALL SCRmsg(REPEAT('-',80),.FALSE.)
         RETURN
      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
C        iCOMB=iCOMB-1
C        IF(iCOMB.EQ.0) GOODCOMB = .FALSE.
         CALL SENDOK

      END IF

      RETURN
      END




      SUBROUTINE ShowCOMBS
C     --------------------
C     jw / 29-01-12  draft
C     jw / 30-05-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

!Debug**** When Alloc3Plus extends the COMBS table,
!Debug****  the COMB added at the bottom of
!Debug****  Pane 4 Column 2 has no COMB number;


      IF(iCOMB.LT.1) iCOMB=1

C     LOAD COMB NUMBER
C     Copy current COMB number into the on-screen table
      CALL SWGLIS(idComb,iCOMB)

C     LOAD COMB NAME
C     Copy stored COMB title into the on-screen table
      CALL SWGTBS(idCombNamTbl,CombName(iCOMB),1,1,'VALUE')

C     LOAD COMB DATA
C     Clear the on-screen table
      DO iLCase=1,NLCASC
         DO I=1,3
           CALL SWGTBS(idCombTbl,'',iLCase,I,'VALUE')
         END DO !I=1,3
      END DO !iLCase=1,NLCASC

C     Copy stored values into the on-screen table
      DO iLCase=1,NLCASC
         LCNum = LOADCOMB(iCOMB,iLCase)%LCNum
         Factr = LOADCOMB(iCOMB,iLCase)%Factr

         IF(LCNum.NE.0) THEN
            CALL SWGTBI(idCombTbl,100,iLCase,1,'VALUE')
            CALL SWGTBF(idCombTbl,Factr, -2, iLCase,3,'VALUE')
            CALL SWGTBS(idCombTbl,LCName(LCNum),iLCase,2,'VALUE')
            CALL SWGTBI(idCombTbl,LCNum,iLCase,1,'VALUE')
         END IF

      END DO

      RETURN
      END




      SUBROUTINE StorCOMBS
C     --------------------
C     jw / 29-01-12  draft
C     jw / 30-05-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE LOADING

      CHARACTER*30 LoadCombName

C     Read load case name from the 1-cell data-entry table..
      CALL GWGTBS(idCombNamTbl,1,1,LoadCombName)

C     ..and store it in memory array
      CombName(iCOMB) = LoadCombName

      DO iLCase = 1,NLCASC
         LOADCOMB(iCOMB,iLCase)%LCNum = 0
         LOADCOMB(iCOMB,iLCase)%Factr = 0

C        Read iLCase from the on-screen data-entry table and store it
         LCNum  = 0
         Factr  = 0

         CALL GWGTBI(idCombTbl,iLCase,1,LCNum)
         CALL GWGTBF(idCombTbl,iLCase,3,Factr)

         IF(LCNum.GT.0) THEN
             LOADCOMB(iCOMB,iLCase)%LCNum = LCNum
             LOADCOMB(iCOMB,iLCase)%Factr = Factr
         END IF !(LCNum.GT.0)

      END DO !NLCASC

      RETURN
      END





      SUBROUTINE CloseCOMB
C     --------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

      USE DISLIN
      USE WinSTUFF
      CALL WinCOMB('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END




      SUBROUTINE ChgCOMB
C     ------------------
C     jw / 27-05-12  draft
C     jw / 29-05-12  last revised

C     On entry to this subroutine, the selected pull-down LC no. has
C     been changed by the user but not yet read by the program.
C     iCOMB still contains the LC no. for the on-screen LC name, and
C     iCOMB still contains the LC no. for the on-screen LC data.
C
C     BEFORE READING the selected pulldown LC no.:
C      - Read and store the currently-displayed CombNam for iCOMB
C      - Read and store the currently-displayed Comb data for iCOMB
C
C     Then read the selected pull-down LC no.
C     Make iCOMB the changed LC
C
C     AFTER READING the selected pulldown LC no.:
C      - Display on-screen the stored LC name for the changed iCOMB
C      - Display on-screen the stored LC data for the changed iCOMB

      USE DISLIN
      USE PARAMS
      USE LOADING

      CALL StorCOMBS !With iCOMB still containing the previous LC no.

      iL = iCOMB
      NL = NCOMB

      CALL GWGLIS(idComb,LCombNum)   !Read the selected pulldown LC no.
      IF(LCombNum.GT.NCOMB) THEN
        CALL ALLOC3PLUS('NCOMB',LCombNum-NCOMB+1)!increase NCOMB
        IF(NCOMB.GT.0) NCOMB = NCOMB-1
        iCOMB=iL                  !  Because ReadLOADS will have done a
        NCOMB=NCOMB+1             !  re-count when called by Alloc3Plus
      END IF

      iCOMB=LCombNum !Change iCOMB to the selected COMB no.
      CALL ShowCOMBS

      RETURN
      END
