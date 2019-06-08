      SUBROUTINE RMATLS
C     -----------------
C     jw / 21-02-87  draft
C     jw / 12-02-12  last mod

C     Read materials
C     --------------
C     Creates a data entry table in a matls pane of window WIN1.
C     Calls StorMATLS to read keyboard data entered into the matls table

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS

      CALL HidePANES
      CALL GhostMenus

C     GoodMAT = .FALSE.

      SOLVED = .FALSE.
      CALL WinMAT('SHOW')

      RETURN
      END




      SUBROUTINE WinMAT(ACTN)
C     -----------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

C     Window layout for Materials input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE MATERIALS

      EXTERNAL  StorMATLS,
     +          CloseMat,
     +          Listout

      REAL      ColWidths(5)
      INTEGER   IMAT
      CHARACTER ACTN*4

      DATA      ColWidths
     +         /-12.,  -36.,        -14.,   -19.,  -19. /
C               Matl   Description  Density  Emod  Gmod

      SELECT CASE(ACTN)

C-----------------------------------------------------------------------
         CASE('SHOW')
C-----------------------------------------------------------------------
         ShowsMAT = .TRUE.

C           Make MATerials data entry fields
C-----------------------------------------------------------------------
C           Matls pane
               CALL SWGPOS(Pan2L,Pan2T)
               CALL WGBAS(Win1,'HORI',idMatPAN)
C-----------------------------------------------------------------------
C           Divide matls pane into two vertical stacks
               CALL SWGWTH(-83)
               CALL WGBAS(idMatPAN,'VERT',idMatPAN1)

               CALL SWGWTH(-7)
               CALL WGBAS(idMatPAN,'VERT',idMatPAN2)
C-----------------------------------------------------------------------
C           Matls input table
               CALL SWGRAY(ColWidths,5,'TABLE')
               CALL SWGOPT('COLUMNS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idMatPAN1,MAX(1,NMATS),5,idMatTbl)
               CALL SWGTBS(idMatTbl,'Material type',0,1,'VALUE')
               CALL SWGTBS(idMatTbl,'Description',0,2,'VALUE')
               CALL SWGTBS(idMatTbl,'Density',0,3,'VALUE')
               CALL SWGTBS(idMatTbl,'E-modulus',0,4,'VALUE')
               CALL SWGTBS(idMatTbl,'G-modulus',0,5,'VALUE')

C           Copy stored values into the on-screen table
            DO Imat=1,NMATS
               CALL SWGTBI(idMatTbl,Imat,Imat,1,'VALUE')
               CALL SWGTBS(idMatTbl,Matls(IMat)%Desc,Imat,2,'VALUE')
               CALL SWGTBF(idMatTbl,Matls(IMat)%Gamma,-2,Imat,3,'VALUE')
               CALL SWGTBF(idMatTbl,Matls(IMat)%Emod, -2,Imat,4,'VALUE')
               CALL SWGTBF(idMatTbl,Matls(IMat)%Gmod, -2,Imat,5,'VALUE')
            END DO
C-----------------------------------------------------------------------
C           Action buttons in stack col 2
            CALL WGPBUT(idMatPAN2,'Accept',idOK)
            CALL SWGCBK(idOK, StorMATLS)

            CALL WGPBUT(idMatPAN2,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseMAT)

            CALL WGPBUT(idMatPAN2,'List input',idList)
            CALL SWGCBK(idList, Listout)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsMAT = .FALSE.
         IF(id0.GT.0) CALL SWGATT(id0,'INACTIVE','STATUS')
         IF(id1.GT.0) CALL SWGATT(id1,'INACTIVE','STATUS')
         CALL SWGATT(idMatPAN, 'INACTIVE','STATUS')
         CALL SWGATT(idMatPAN1,'INACTIVE','STATUS')
         CALL SWGATT(idMatPAN2,'INACTIVE','STATUS')
         CALL SWGATT(idMatTbl, 'INACTIVE','STATUS')
         CALL SWGATT(idOK,     'INACTIVE','STATUS')
         CALL SWGATT(idCANCEL, 'INACTIVE','STATUS')
         CALL SWGATT(idLIST,   'INACTIVE','STATUS')

C        CALL SWGATT(id0,      'INVISIBLE','STATUS')
C        CALL SWGATT(id1,      'INVISIBLE','STATUS')
C        CALL SWGATT(idMatPAN, 'INVISIBLE','STATUS')
C        CALL SWGATT(idMatPAN1,'INVISIBLE','STATUS')
C        CALL SWGATT(idMatPAN2,'INVISIBLE','STATUS')
C        CALL SWGATT(idMatTbl, 'INVISIBLE','STATUS')
C        CALL SWGATT(idOK,     'INVISIBLE','STATUS')
C        CALL SWGATT(idCANCEL, 'INVISIBLE','STATUS')
C        CALL SWGATT(idLIST,   'INVISIBLE','STATUS')

      END SELECT !(ACTN)

      RETURN
      END




      SUBROUTINE StorMATLS
C     --------------------
C     jw / 29-01-12  draft
C     jw / 29-01-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE MATERIALS

      REAL*4    Gamma, Emod, Gmod
      CHARACTER Desc*24

      CHARACTER I2CHAR*8

      GoodMAT = .TRUE.

      DO Itab = 1,NMATS

         Imat  = 0
         Desc  = Repeat(' ',24)
         Gamma = 0.0
         EMod  = 0.0
         GMod  = 0.0

C        Read line Itab from the on-screen data-entry table
         CALL GWGTBI(idMatTbl,Itab,1,Imat)
         CALL GWGTBS(idMatTbl,Itab,2,Desc)
         CALL GWGTBF(idMatTbl,Itab,3,Gamma)
         CALL GWGTBF(idMatTbl,Itab,4,EMod)
         CALL GWGTBF(idMatTbl,Itab,5,GMod)

         IF (Imat.LE.0 .OR. Imat.GT.NMATS) THEN
            CALL SCRmsg('**Error: Material properties discarded for '//
     +      'invalid material <'//TRIM(I2CHAR(Imat,Idum))//'>',GoodMAT)
            GoodMAT = .FALSE.

         ELSE

            IF (ABS(Gamma+EMod+GMod).GT.0.0) THEN
               IF( Gamma.NE.Matls(IMat)%Gamma
     +           .AND. EMod.NE.Matls(IMat)%Emod
     +           .AND. GMod.NE.Matls(IMat)%Gmod )
     +         CALL SCRmsg('Overwriting existing material '//
     +         TRIM(I2CHAR(Imat,Idum))//' with new properties',GoodMAT)
            END IF

C           Accept the line of data read from the on-screen table
C           and copy it to array storage
            Matls(IMat)%Gamma  = Gamma
            Matls(IMat)%Desc   = TRIM(ADJUSTL(Desc(1:24)))
            Matls(IMat)%Emod   = EMod
            Matls(IMat)%Gmod   = GMod

            IF (Gamma.LE.0.0 .OR. EMod.LE.0.0 .OR. GMod.LE.0.0) THEN
               CALL SCRmsg('**Error in properties entered for '//
     +         'material number'//TRIM(I2CHAR(Imat,Idum)),GoodMAT)
               GoodMAT = .FALSE.
            END IF
         END IF
      END DO
      SOLVED = .FALSE.

      IF(.NOT.GoodMAT) THEN
C        RETURN leaving the error messages still displayed and the
C        MATERIALS data entry fields on screen for further data
         CALL SCRmsg(REPEAT('-',80),(.FALSE.))
         RETURN
      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK
      END IF

      RETURN
      END




      SUBROUTINE CloseMAT
C     -------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

      USE WinSTUFF

      CALL WinMAT('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END