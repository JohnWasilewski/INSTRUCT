      SUBROUTINE WinSCR(ACTN)
C     -----------------------
C     jw / 07-02-12  draft
C     jw / 09-02-12  last revised

C     Open or close the output window.

      USE dislin
      USE WINSTUFF

      CHARACTER*4 ACTN

C     CALL SWGFNT(MonN%Styl,MonB%Size)
      CALL SWGFNT('Courier New',NINT(Float(Pan1W)/39.1))

C     Screen output pane

      IF(.NOT.MadeSCR) THEN
         CALL SWGCLR (1.0, 1.0, 1.0, 'BACK') ! R,G,B Background colour
         CALL SWGWIN(Pan1L,Pan1T,Pan1W,Pan1H)
         NDispLines=40
         NScroLines=999999
         CALL WGSTXT(Win1,NDispLines,NScroLines,idSCR)
      END IF !(MadeSCR)

      SELECT CASE(ACTN)

      CASE('SHOW')
         CALL SWGATT(idSCR,'ACTIVE','STATUS')
         ShowsSCR = .TRUE.

      CASE('HIDE')
         CALL SWGATT(idSCR,'INACTIVE','STATUS')
         CALL SWGATT(idSCR,'INVISIBLE','STATUS')
         ShowsSCR = .FALSE.
      END SELECT !(ACTN)

      RETURN
      END




      SUBROUTINE SCRmsg(msg,CLS)
C     --------------------------
C     jw / 01-02-12  draft
C     jw / 19-03-12  last revised

C     Display a message on the OUT pane.
C     If(CLS) then clear the pane first.

      USE dislin
      USE WINSTUFF

      LOGICAL, INTENT(IN), OPTIONAL :: CLS
      CHARACTER(*), INTENT(IN)      :: msg

C     IF(PRESENT(CLS)) THEN
C        IF(CLS .AND. ShowsSCR) CALL WinSCR('HIDE')
C     END IF

      IF(.NOT.ShowsSCR) CALL WinSCR('SHOW')
      CALL SWGTXT(idSCR, msg(1:LEN_TRIM(msg)))

      RETURN
      END




      SUBROUTINE HidePanes
C     --------------------
C     jw / 30-03-12    draft
C     jw / 30-03-12  last mod

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

      IF(ShowsTIT) CALL WinTITL('HIDE')
      IF(ShowsPAR) CALL WinPAR('HIDE')
      IF(ShowsMAT) CALL WinMAT('HIDE')
      IF(ShowsPRP) CALL WinPRP('HIDE')
      IF(ShowsNOD) CALL WinNOD('HIDE')
      IF(ShowsMEM) CALL WinMEM('HIDE')
      IF(ShowsRES) CALL WinRES('HIDE')
      IF(ShowsLOD) CALL WinLOD('HIDE')
      IF(ShowsLOD) CALL WinCOMB('HIDE')

C     CALL SWGFNT(PrpN%Styl,PrpN%Size)
      CALL SWGFNT(PrpN%Styl,Pan2W/70)

      RETURN
      END
