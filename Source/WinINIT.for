      Subroutine WinINIT
C     ------------------

      USE WinStuff
      USE TITLES

C-----------------------------------------------------------------------
      CALL SWGOPT('TOP','DIALOG') !Keep all dialog boxes on top

C     When LIST Pane 1 displays 46 chars wide, Pan1W is 483 pixels

C     WINDOW 1 LAYOUT
C     Define main window and menus
      CALL SWGCLR (0.94, 0.93, 0.91, 'BACK') ! R,G,B Background colour
      CALL SWGTIT
     +    (PROG8//' ver.'//VERS4//' : '//ADJUSTL(PDAT9)//' '
     +     //'Copyright '//CPRT17(1:13)//' '//ADJUSTL(AUTH16)//'      ')
      CALL SWGOPT('OK','CLOSE')

C     Screen size
      CALL GETSCR(NWPIX,NHPIX) ! Current screensize
C     Write(6,'('' Screen size'',I5,'' x'',I5/)') NWPIX,NHPIX

C     Program window
      Win1L = NINT(1.0*NWPIX/100)         !Margin left
      Win1W = NINT(98.0*NWPIX/100)        !No of pixels wide
      Win1T = NINT(2.0*NHPIX/100)         !Margin top
      Win1H = NINT(96.0*NHPIX/100)        !No of pixels high

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

C     Pane 1
      Pan1L = NINT(0.8*Win1W/100)         !Margin left
      Pan1W = NINT(48.1*Win1W/100)        !No of pixels wide
      Pan1T = NINT(0.8*Win1H/100)         !Margin top
      Pan1H = NINT(66.7*Win1H/100)        !No of pixels high

C     Pane 2
      Pan2L = Pan1L                       !Margin left
      Pan2W = Win1W-2*Pan2L               !No of pixels wide
      Pan2T = Pan1T+Pan1H                 !Margin top
      Pan2H = Win1H-Pan2T                 !No of pixels high

C     Pane 3
      Pan3L = PAN1L+PAN1W+Pan1L           !Margin left
      Pan3W = Win1W-PAN3L-3*PAN1L         !No of pixels wide
      Pan3T = PAN1T                       !Margin top
      Pan3H = NINT(PAN1H * 0.94) - PAN3T  !No of pixels high
      Pan3R = Pan3L + Pan3W
      Pan3B = Pan3T + Pan3H

C     Pane 4
      Pan4L = PAN3L                       !Margin left
      Pan4W = PAN3W                       !No of pixels wide
      Pan4T = PAN3T+PAN3H+PAN3T           !Margin top
      Pan4H = PAN1H - PAN3H               !No of pixels high

      RETURN
      END
