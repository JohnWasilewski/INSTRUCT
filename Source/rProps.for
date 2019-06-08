	   SUBROUTINE rPROPS
C     -----------------
C     jw / 21-02-87  draft
C     jw / 12-02-12  last mod

C     Read member properties
C     ----------------------
C     Creates a data entry table in a member PROPS pane of window WIN1.
C     Calls StorPROPS to read keyboard data entered into the PROPS table

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

C     GoodPRP = .FALSE.

      SOLVED = .FALSE.
      CALL WinPRP('SHOW')

      RETURN
      END





      SUBROUTINE WinPRP(ACTN)
C     -----------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

C     Window layout for Materials input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE PROPERTIES

      EXTERNAL  StorPROPS,
     +          ClosePrp,
     +          Listout

      REAL      ColWidths(6)

      INTEGER   IPrp

      CHARACTER ACTN*4

      DATA      ColWidths
     +         /-11.,   -37.,       -10.,     -14.,   -14.,   -14. /
C                Prp  Description  Material  Density  AreaY  InrtZ

      SELECT CASE(ACTN)

C-----------------------------------------------------------------------
         CASE('SHOW')
C-----------------------------------------------------------------------
         ShowsPRP = .TRUE.

C           Make member type PRoPerties data entry fields
C-----------------------------------------------------------------------
C           PROPS pane
               CALL SWGPOS(Pan2L,Pan2T)
               CALL WGBAS(Win1,'HORI',idPrpPAN)
C-----------------------------------------------------------------------
C           Divide PROPS pane into two vertical stacks
               CALL SWGWTH(-83)
               CALL WGBAS(idPrpPAN,'VERT',idPrpPAN1) !Data

               CALL SWGWTH(-7)
               CALL WGBAS(idPrpPAN,'VERT',idPrpPAN2) !Buttons
C-----------------------------------------------------------------------
C           PROPS input table
               CALL SWGRAY(ColWidths,6,'TABLE')
               CALL SWGOPT('COLUMNS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idPrpPAN1,MAX(1,NMTYP),6,idPrpTbl)
               CALL SWGTBS(idPrpTbl,' Member type',0,1,'VALUE')
               CALL SWGTBS(idPrpTbl,' Description',0,2,'VALUE')
               CALL SWGTBS(idPrpTbl,' Material',   0,3,'VALUE')
               CALL SWGTBS(idPrpTbl,' Area(X)',    0,4,'VALUE')
               CALL SWGTBS(idPrpTbl,' Area(Y)',    0,5,'VALUE')
               CALL SWGTBS(idPrpTbl,' Inertia(Z)', 0,6,'VALUE')

C           Copy stored values into the on-screen table
            DO IPrp=1,NMTYP
               CALL SWGTBI(idPrpTbl,IPrp,IPrp,1,'VALUE')
               CALL SWGTBS(idPrpTbl,PROPS(IPrp)%Desc,IPrp,2,'VALUE')
               CALL SWGTBI(idPrpTbl,PROPS(IPrp)%Matl,IPrp,3,'VALUE')
               CALL SWGTBF(idPrpTbl,PROPS(IPrp)%AreaX,-2,IPrp,4,'VALUE')
               CALL SWGTBF(idPrpTbl,PROPS(IPrp)%AreaY,-2,IPrp,5,'VALUE')
               CALL SWGTBF(idPrpTbl,PROPS(IPrp)%InrtZ,-2,IPrp,6,'VALUE')
            END DO
C-----------------------------------------------------------------------
C           Action buttons in stack col 2
            CALL WGPBUT(idPrpPAN2,'Accept',idOK)
            CALL SWGCBK(idOK, StorPROPS)

            CALL WGPBUT(idPrpPAN2,'Cancel',idCancel)
            CALL SWGCBK(idCancel, ClosePrp)

            CALL WGPBUT(idPrpPAN2,'List input',idList)
            CALL SWGCBK(idList, Listout)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsPRP = .FALSE.
         IF(id0.GT.0) CALL SWGATT(id0,'INACTIVE','STATUS')
         IF(id1.GT.0) CALL SWGATT(id1,'INACTIVE','STATUS')
         CALL SWGATT(idPrpPAN, 'INACTIVE','STATUS')
         CALL SWGATT(idPrpPAN1,'INACTIVE','STATUS')
         CALL SWGATT(idPrpPAN2,'INACTIVE','STATUS')
         CALL SWGATT(idPrpTbl, 'INACTIVE','STATUS')
         CALL SWGATT(idOK,     'INACTIVE','STATUS')
         CALL SWGATT(idCANCEL, 'INACTIVE','STATUS')
         CALL SWGATT(idLIST,   'INACTIVE','STATUS')

C        CALL SWGATT(id0,      'INVISIBLE','STATUS')
C        CALL SWGATT(id1,      'INVISIBLE','STATUS')
C        CALL SWGATT(idPrpPAN, 'INVISIBLE','STATUS')
C        CALL SWGATT(idPrpPAN1,'INVISIBLE','STATUS')
C        CALL SWGATT(idPrpPAN2,'INVISIBLE','STATUS')
C        CALL SWGATT(idPrpTbl, 'INVISIBLE','STATUS')
C        CALL SWGATT(idOK,     'INVISIBLE','STATUS')
C        CALL SWGATT(idCANCEL, 'INVISIBLE','STATUS')
C        CALL SWGATT(idLIST,   'INVISIBLE','STATUS')

      END SELECT !(ACTN)

      RETURN
      END





      SUBROUTINE StorPROPS
C     --------------------
C     jw / 29-01-12  draft
C     jw / 29-01-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE PROPERTIES

      INTEGER   Matl,Matl_new
      REAL*4    AreaX,Ax,AxExtg,AreaX_new
      REAL*4    AreaY,Ay,AyExtg,AreaY_new
      REAL*4    InrtZ,Iz,IzExtg,InrtZ_new
      CHARACTER Desc*24, Desc_new*24

      CHARACTER I2CHAR*8

      GoodPRP = .TRUE.

      DO Itab = 1,NMTYP

C        Read line Itab from the on-screen data-entry table

         Iprp  = 0
         CALL GWGTBI(idPrpTbl,Itab,1,Iprp)

         Desc  = REPEAT(' ',24)
         CALL GWGTBS(idPrpTbl,Itab,2,Desc(1:24))
         DESC=ADJUSTL(Desc)
         DESC=TRIM((Desc))

         Matl  = 0
         CALL GWGTBI(idPrpTbl,Itab,3,Matl)

         AreaX = 0.0
         CALL GWGTBF(idPrpTbl,Itab,4,AreaX)
         Ax = ABS(AreaX)
         AxExtg= PROPS(IPrp)%AreaX

         AreaY = 0.0
         CALL GWGTBF(idPrpTbl,Itab,5,AreaY)
         Ay = ABS(AreaY)
         AyExtg= PROPS(IPrp)%AreaY

         InrtZ = 0.0
         CALL GWGTBF(idPrpTbl,Itab,6,InrtZ)
         Iz = ABS(InrtZ)
         IzExtg= PROPS(IPrp)%InrtZ

         IF (IPrp.LE.0 .OR. IPrp.GT.NMTYP) THEN
            CALL SCRmsg('**Error: Properties discarded for '//
     +      'invalid member type <'//TRIM(I2CHAR(IPrp,idum))//'>',
     +      .FALSE.)
            GoodPRP = .FALSE.

         ELSE

            Matl_new = Matl
            AreaX_new = AreaX
            AreaY_new = AreaY
            InrtZ_new = InrtZ
            Desc_new = TRIM(ADJUSTL(Desc))

            IF (Ax+Ay+Iz.GT.0.0) THEN
               IF( Ax.NE.AxExtg .AND. Ay.NE.AyExtg .AND. Iz.NE.IzExtg)
     +         THEN
                  PROPS(IPrp)%Matl  = 0
                  PROPS(IPrp)%AreaX = 0.0
                  PROPS(IPrp)%AreaY = 0.0
                  PROPS(IPrp)%InrtZ = 0.0
                  PROPS(IPrp)%Desc  = REPEAT(' ',24)

C                 When copying InrtZ directly into PROPS(IPrp)%InrtZ,
C                 the statement below was overwriting PROPS(IPrp)%InrtZ
                  CALL SCRmsg
     +            ('Overwriting existing member type '//
     +            TRIM(I2CHAR(IPrp,idum))//' with new properties',
     +            .FALSE.)

               END IF
            END IF

C           Accept the line of data read from the on-screen table
C           and copy it to array storage

            PROPS(IPrp)%Matl  = Matl_new
            PROPS(IPrp)%AreaX = AreaX_new
            PROPS(IPrp)%AreaY = AreaY_new
            PROPS(IPrp)%InrtZ = InrtZ_new
            PROPS(IPrp)%Desc  = Desc_new

            IF (AreaX.LE.0.0 .OR. AreaY.LE.0.0 .OR. InrtZ.LE.0.0) THEN

               CALL SCRmsg('**Error in properties entered for '//
     +         'member type '//TRIM(I2CHAR(IPrp,idum)),.FALSE.)

               GoodPrp = .FALSE.
            END IF
         END IF
      END DO

      IF(.NOT.GoodPRP) THEN
C        RETURN leaving the error messages still displayed and the
C        PROPERTIES data entry fields on screen for further data
         CALL SCRmsg(REPEAT('-',80),.FALSE.)
         RETURN
      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK

      END IF

      RETURN
      END





      SUBROUTINE ClosePRP
C     -------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

      USE WinSTUFF

      CALL WinPRP('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END