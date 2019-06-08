      SUBROUTINE rTITLES
C     ------------------
C     jw / 30-10-85  draft
C     jw / 06-02-12  last revised

C     Creates data entry fields in a titles pane of window WIN1.
C     Calls StorTIT to read keyboard data entered into the titles fields

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES

      CALL HidePANES
      CALL GhostMenus

      CALL WinTITL('SHOW')

      RETURN
      END




      SUBROUTINE WinTITL(ACTN)
C     ------------------------
C     jw / 30-10-85  draft
C     jw / 06-02-12  last revised

C     Window layout for Titles input

      USE dislin
      USE WINSTUFF
      USE TITLES

      EXTERNAL  StorTIT,
     +          CloseTIT,
     +          Listout

      CHARACTER ACTN*4, Blanks*24

      Blanks = REPEAT(' ',24)

      SELECT CASE(ACTN)
C-----------------------------------------------------------------------
         CASE ('SHOW')
C-----------------------------------------------------------------------
         ShowsTIT = .TRUE.

C           Make TITles data entry fields
C-----------------------------------------------------------------------
C           Title pane
            CALL SWGPOS(Pan2L,Pan2T)
            CALL WGBAS(Win1,'HORI',idTitPAN)
C-----------------------------------------------------------------------
C           Divide title pane into three vertical stacks
C           (only the first and last will be used)
            CALL SWGWTH(-22)
            CALL WGBAS(idTitPAN,'VERT',idTitPAN1)

            CALL SWGWTH(-58)
            CALL WGBAS(idTitPAN,'VERT',idTitPAN2)

            CALL SWGWTH(-8)
            CALL WGBAS(idTitPAN,'VERT',idTitPAN3)
C-----------------------------------------------------------------------
C           Prompt, messages and data entry fields in the LH stack
            CALL SWGWTH(-79)
            CALL WGLAB(idTitPAN1,Blanks//'TITLES',id0)
            CALL WGLTXT(idTitPAN1,'Line 1',TITLE1,70,idTL1)
            CALL WGLTXT(idTitPAN1,'Line 2',TITLE2,70,idTL2)
            CALL WGLTXT(idTitPAN1,'Line 3',TITLE3,70,idTL3)
            CALL WGLTXT(idTitPAN1,'Engineer',ENG,60,idENG)

            CALL SWGFOC(idTL1)
C-----------------------------------------------------------------------
C           Action buttons in the RH stack

C           CALL WGLAB(idTitPAN3,'',id1)
            CALL WGPBUT(idTitPAN3,'Accept',idOK)
            CALL SWGCBK(idOK, StorTIT)

            CALL WGPBUT(idTitPAN3,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseTIT)

            CALL WGPBUT(idTitPAN3,'List input',idList)
            CALL SWGCBK(idList, Listout)

C-----------------------------------------------------------------------
      CASE ('HIDE')
C-----------------------------------------------------------------------

         IF(id0.GT.0) CALL SWGATT(id0,'INACTIVE','STATUS')
         CALL SWGATT(idTL1,   'INACTIVE','STATUS')
         CALL SWGATT(idTL2,   'INACTIVE','STATUS')
         CALL SWGATT(idTL3,   'INACTIVE','STATUS')
         CALL SWGATT(idENG,   'INACTIVE','STATUS')
         CALL SWGATT(idOK,    'INACTIVE','STATUS')
         CALL SWGATT(idCANCEL,'INACTIVE','STATUS')
         CALL SWGATT(idLIST,  'INACTIVE','STATUS')

C        CALL SWGATT(id0,     'INVISIBLE','STATUS')
C        CALL SWGATT(idTL1,   'INVISIBLE','STATUS')
C        CALL SWGATT(idTL2,   'INVISIBLE','STATUS')
C        CALL SWGATT(idTL3,   'INVISIBLE','STATUS')
C        CALL SWGATT(idENG,   'INVISIBLE','STATUS')
C        CALL SWGATT(idOK,    'INVISIBLE','STATUS')
C        CALL SWGATT(idCANCEL,'INVISIBLE','STATUS')
C        CALL SWGATT(idLIST,  'INVISIBLE','STATUS')

         ShowsTIT = .FALSE.

      END SELECT !(ACTN)


      RETURN
      END




      SUBROUTINE StorTIT
C     ------------------
C     jw / 29-01-12  draft
C     jw / 29-01-12  last revised

      USE dislin
      USE WINSTUFF
      USE TITLES

      CALL GWGTXT(idTL1,TITLE1)
      CALL GWGTXT(idTL2,TITLE2)
      CALL GWGTXT(idTL3,TITLE3)
      CALL GWGTXT(idENG,ENG)

      IF(LEN_TRIM(Title1)+LEN_TRIM(Title2)+LEN_TRIM(Title3).NE.0)
     +    GoodTIT = .TRUE.

      CALL WinTITL('HIDE')
      SOLVED = .FALSE.

      IF(.NOT.GoodTIT) THEN
C     RETURN leaving the error messages still displayed and the
C     MATERIALS data entry fields on screen for further data
         CALL SCRmsg(REPEAT('-',80),(.FALSE.))
         RETURN
      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK
      END IF


C     CALL WinTITL('HIDE')

C     CALL LIST(0)
C     RETURN
      END





      SUBROUTINE CloseTIT
C     ------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

      USE WinSTUFF

      CALL WinTITL('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END

