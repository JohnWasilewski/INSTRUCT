      SUBROUTINE DrawSCR
C     ------------------
C     jw / 06-02-12  draft
C     jw / 23-02-14  last revised

C     Diagram pane

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
      USE DRW

      CHARACTER ADoF*3, NodRes*3, Framing*3
      REAL      Lx, Ly, THKTH, Thickness, Length, Average

      REAL      BarLen, BarLen2,
     +          GapWid, GapWid2,
     +          GapLen, GapLen2,
     +          Parallel, Orthognl,
     +          cosA, sinA

      LOGICAL   Rel(6), REL1, REL2,
     +          Rx1,Ry1,Rz1, Rx2,Ry2,Rz2

      EQUIVALENCE (Rx1, Rel(1)),
     +            (Ry1, Rel(2)),
     +            (Rz1, Rel(3)),
     +            (Rx2, Rel(4)),
     +            (Ry2, Rel(5)),
     +            (Rz2, Rel(6))

      PARAMETER (ADoF   = ('XYZ'))

      LinThkH = 2155 !Display panel height in units of line thickness

      AR = FLOAT(Pan3W)/FLOAT(Pan3H) !Aspect Ratio (1.4026)
C                 749          534

      SymM = 0.05    !Restraint symbol width multiplier on frame size.
                     !Changes later to the restraint symbol width.

C     Line thicknesses for restraint symbols
      LiVthn =  4 !Very thin line thickness
      LinThn = 10 !Thin line thickness
      LinMed = 24 !Medium line thickness
      LinThk = 38 !Thick line thickness
      LinRng = 20 !Ring line thickness

C     ---------------------
C     Open the diagram pane
C     ---------------------
C     Use Pane 3 for this purpose

      CALL SWGATT(idDRW,'ACTIVE','STATUS')
      CALL REAWGT

      CALL setxid(idDRW,'WIDGET')
      CALL metafl('CONS')   !Graphic page goes to screen

      MinX=HUGE(1.1)
      MaxX=TINY(1.1)
      MinY=MinX
      MaxY=MaxX

      DO iNOD=1,NNODE
         MinX=MIN(MinX,NODE(1,iNOD)) ! frame units, eg metres ('m')
         MaxX=MAX(MaxX,NODE(1,iNOD)) ! m
         MinY=MIN(MinY,NODE(2,iNOD)) ! m
         MaxY=MAX(MaxY,NODE(2,iNOD)) ! m
      END DO

      FrL  = MinX
      FrL  = FLOAT(INT(FrL))

      FrR  = MaxX
      FrR  = FLOAT(NINT(FrR))

      FrB  = MinY
      FrB  = FLOAT(INT(FrB))

      FrT  = MaxY
      FrT  = FLOAT(NINT(FrT))

      FrW  = FrR - FrL ! m
      FrH  = ABS(MaxY-MinY) ! m

      Fmx  = MAX(FrR-FrL,FrT-FrB)
      SymM = SymM*Fmx



      CALL disini

C     Initial settings

      CALL UNITS('POINTS')

      CALL NOCLIP

      CALL errmod('ALL', 'OFF')
      CALL erase  !Clear the graphics screen

      CALL HEIGHT(30)
      CALL COLOR('BLUE')
      CALL LINWID(7)
      CALL CHASPC(-0.02)
      CALL TXTJUS('MIDDLE')
      CALL TXTJUS('LEFT')
      CALL MESSAG('UNDEFLECTED STRUCTURE',10,25)
      CALL TXTJUS('CENT')

      CALL intax  !Integer axes labels

      CALL SETGRF('LABELS','LABELS','NONE','NONE')
      CALL FRAME(0)
      CALL AXCLRS(INDRGB(0.7, 0.0, 0.0),'ALL','XY')


C     -------------
C     DRAW THE AXES
C     -------------

      CALL GRAF( 0.0,                           ! X-axis: lower limit
     +           Fmx,                           !         upper limit
     +           0.0,                           !         1st label
     +           FLOAT(NINT(Fmx/10.0)),         !         label steps

     +           FrB,                           ! Y-axis: lower limit
     +           FrB+Fmx/AR,                    !         upper limit
     +           FrB,                           !         1st label
     +           FLOAT(NINT(1.1*Fmx/AR/10.0)) ) !         label steps

C     Available screen area to display the structure
      DispW  = 0.9 * Pan3W * FrW/Fmx  !  Frame width in pixels
      DispH  = 0.9 * Pan3H * FrH/Fmx  !  Frame height in pixels
      DispArea  = DispW * DispH       !  Displayed frame area in pixels

C     Conversion factor, plot units to pixels
      PperPx = 500.0/FLOAT((NXPIXL(500,0)-NXPIXL(0,0))) !PlotU/pixel
      PperPy = 500.0/FLOAT((NYPIXL(0,500)-NYPIXL(0,0))) !PlotU/pixel

C     Conversion factor, plot units to user units
      PperMx = ABS(500.0/(XINVRS(500)-XINVRS(0)))       ! PlotU/m
      PperMy = ABS(500.0/(YINVRS(500)-YINVRS(0)))       ! PlotU/m
      PUperM = (PperMx+PperMy)*0.5                      ! PlotU/m


C     -------
C     MEMBERS
C     -------

      CALL LNCAP('CUT')

C     ------------------
C     Member thicknesses
C     ------------------

C     Thinnest and thickest member types
      Thinnest = THKTH(1)
      Thickest = Thinnest
      IF(NMTYP.GT.1) then
          DO IPrp=2,NMTYP
             Thk = THKTH(IPrp)
             Thinnest=MIN(Thinnest,Thk)
             Thickest=MAX(Thickest,Thk)
          END DO
      END IF

C     Member thickness should now be plotted as estimated thickness
C     but not less than 3.

      DO iMemb=1,NMEMB

C         -----------------------
C         Draw the current member
C         -----------------------
          iNod1 = MEMB(iMemb)%Node1
          iNod2 = MEMB(iMemb)%Node2
          iPrp = MEMB(iMemb)%MTyp
          REL  = MEMB(iMemb)%Released
          x1 = NODE(1,iNod1)-FrL
          y1 = NODE(2,iNod1)
          x2 = NODE(1,iNod2)-FrL
          y2 = NODE(2,iNod2)
          Length = SQRT((x2-x1)**2+(y2-y1)**2)
          Thickness = MAX(PUperM*THKTH(iPrp),3.)
          CALL LINWID(NINT(Thickness))
          CALL COLOR('GRAY')
          CALL RLINE(x1,y1,x2,y2) ! Within the CALL GRAF range

C         -----------------------
C         Write the member number
C         -----------------------
          CALL HEIGHT(10)
C         NL = NLNUMB(FLOAT(iMemb),0)
          NumSiz = NINT(PUperM*Min(Length/8.0,2.25*Thickness))
          MNHGHT= MAX(MIN(NumSiz,40),22)
          CALL HEIGHT(MNHGHT)
          CALL COLOR('RED')
          CALL LINWID(7)

          CALL RLNUMB (FLOAT(iMemb), -1, 
     +                (x1+x2)/2.0,
     +                (y1+y2)/2.0   )

C         -----------------------
C         Any member-end releases
C         -----------------------
          PrpnAlong = 0.015*FLOAT(MNHGHT)/Length 
          PINdia = 20.0 ! Minimum
          
          IF(Rx1.OR.Ry1.OR.Rz1) THEN
              REL1=.TRUE.
          ELSE
              REL1=.FALSE.
          END IF !(Rx1.OR.Ry1.OR.Rz1)

          IF(Rx2.OR.Ry2.OR.Rz2) THEN
              REL2=.TRUE.
          ELSE
              REL2=.FALSE.
          END IF !(Rx2.OR.Ry2.OR.Rz2)

          IF(REL1.OR.REL2) THEN
              BarLen = 0.12*Length       !Restraint symbol length
              GapWid = Max(0.033*Length,0.5*Thickness/PUperM) !Lines gap
              BarLen = MAX(BarLen,1.5*GapWid)
              GapLen = 0.3*BarLen        !Gap in member

              GapWid2 = GapWid*0.5       !Half the gap between lines
              GapLen2 = GapLen*0.5       !Half the gap
              BarLen2 = BarLen*0.5       !Half restraint symbol length

              Parallel = (y2-y1)/(x2-x1) !Member gradient
              Orthognl = -1.0/Parallel   !Gradient perpendiclr to member

              sinA=(y2-y1)/Length        !cos(line gradient)
              cosA=(x2-x1)/Length        !sin(line gradient)

          END IF !(REL1.OR.REL2)

          IF(REL1) THEN
C             Release symbol position
              Xrel = x1+PrpnAlong*(x2-x1)
              Yrel = y1+PrpnAlong*(y2-y1)

C             Gap in member
              dX = ABS(GapLen2*cosA)
              IF(x2.LT.x1) dX = -dX
              dY = ABS(GapLen2*sinA)
              IF(y2.LT.y1) dY = -dY

C             CALL COLOR('WHITE')
              CALL SETRGB (1.0, 1.0, 1.0)

              CALL LINWID(NINT(1.2*Thickness))
              CALL RLINE(Xrel-dX, Yrel-dY, Xrel+dX, Yrel+dY)

C             CALL COLOR('BLACK')
              CALL SETRGB (0.0, 0.0, 0.0)

              IF(ShowThknss) THEN
                  CALL LINWID(LinThn)
                  Stretch = 1.25
              ELSE
                  CALL LINWID(LiVThn)
                  Stretch = 1.0
              END IF !(ShowThknss)

              IF(Rx1) THEN
C                 Parallel-lines symbol parallel to member
                  Stretch=1.2*Stretch
                  BarX1 = Xrel-Stretch*BarLen2*cosA
                  BarX2 = Xrel+Stretch*BarLen2*cosA
                  BarY1 = Yrel-Stretch*BarLen2*sinA
                  BarY2 = Yrel+Stretch*BarLen2*sinA
                  dX = 1.3*GapWid2*sinA
                  dY = 1.3*GapWid2*cosA
                  CALL RLINE(BarX1+dX, BarY1-dY, BarX2+dX, BarY2-dY)
                  CALL RLINE(BarX1-dX, BarY1+dY, BarX2-dX, BarY2+dY)
              END IF !(Rx1)

              IF(Ry1) THEN
C                 Parallel-lines symbol orthogonal to member
                  Stretch=0.7*Stretch
                  BarX1 = Xrel-Stretch*BarLen2*sinA
                  BarX2 = Xrel+Stretch*BarLen2*sinA
                  BarY1 = Yrel+Stretch*BarLen2*cosA
                  BarY2 = Yrel-Stretch*BarLen2*cosA
                  dX = 0.66*GapWid2*cosA
                  dY = 0.66*GapWid2*sinA
                  CALL RLINE(BarX1-dX, BarY1-dY, BarX2-dX, BarY2-dY)
                  CALL RLINE(BarX1+dX, BarY1+dY, BarX2+dX, BarY2+dY)
              END IF !(Ry1)

              IF(Rz1) THEN
C                 Circular ring hinge symbol
                  Nsiz = MAX(Thickness-1.0,PINdia)
C                 Nsiz = NINT(0.9*GapWid*PUperM)
C                 Nsiz = NINT(MAX(1.3*GapWid,0.05*LENGTH)*PUperM)
                  CALL HEIGHT(Nsiz)
                  CALL LINWID(3)
                  CALL TXTJUS('CENT')
                  CALL TXTJUS('MIDDLE')
C                 CALL TXTBGD(0)
                  CALL SETRGB (0.0, 0.0, 0.0)
                  CALL COLOR('RED')
                  CALL RLMESS('O',Xrel,Yrel)
C                CALL CIRCLE(NINT(Xrel*PUperM),NINT(Yrel*PUperM),Nziz/2)
C                 CALL TXTBGD(-1)
              END IF !(Rz1)
          END IF !(REL1)

          IF(REL2) THEN
C             Release symbol position
              Xrel = x2-PrpnAlong*(x2-x1)
              Yrel = y2-PrpnAlong*(y2-y1)

C             Gap in member
              dX = ABS(GapLen2*cosA)
              IF(x2.LT.x1) dX = -dX
              dY = ABS(GapLen2*sinA)
              IF(y2.LT.y1) dY = -dY

C             CALL COLOR('WHITE')
              CALL SETRGB (1.0, 1.0, 1.0)

              CALL LINWID(NINT(1.2*Thickness))
              CALL RLINE(Xrel-dX, Yrel-dY, Xrel+dX, Yrel+dY)

C             CALL COLOR('BLACK')
              CALL SETRGB (0.0, 0.0, 0.0)

              IF(ShowThknss) THEN
                  CALL LINWID(LinThn)
                  Stretch = 1.25
              ELSE
                  CALL LINWID(LiVThn)
                  Stretch = 1.0
              END IF !(ShowThknss)

              IF(Rx2) THEN
C                 Parallel-lines symbol parallel to member
                  Stretch=1.2*Stretch
                  BarX1 = Xrel-Stretch*BarLen2*cosA
                  BarX2 = Xrel+Stretch*BarLen2*cosA
                  BarY1 = Yrel-Stretch*BarLen2*sinA
                  BarY2 = Yrel+Stretch*BarLen2*sinA
                  dX = 1.3*GapWid2*sinA
                  dY = 1.3*GapWid2*cosA
                  CALL RLINE(BarX1+dX, BarY1-dY, BarX2+dX, BarY2-dY)
                  CALL RLINE(BarX1-dX, BarY1+dY, BarX2-dX, BarY2+dY)
              END IF !(Rx2)

              IF(Ry2) THEN
C                 Parallel-lines symbol orthogonal to member
                  Stretch=0.7*Stretch
                  BarX1 = Xrel-Stretch*BarLen2*sinA
                  BarX2 = Xrel+Stretch*BarLen2*sinA
                  BarY1 = Yrel+Stretch*BarLen2*cosA
                  BarY2 = Yrel-Stretch*BarLen2*cosA
                  dX = 0.66*GapWid2*cosA
                  dY = 0.66*GapWid2*sinA
                  CALL RLINE(BarX1-dX, BarY1-dY, BarX2-dX, BarY2-dY)
                  CALL RLINE(BarX1+dX, BarY1+dY, BarX2+dX, BarY2+dY)
              END IF !(Ry2)

              IF(Rz2) THEN
C                 Circular ring hinge symbol
                  Nsiz = MAX(Thickness-1.0,PINdia)
C                 Nsiz = NINT(0.9*GapWid*PUperM)
C                 Nsiz = NINT(MAX(1.3*GapWid,0.05*LENGTH)*PUperM)
                  CALL HEIGHT(Nsiz)
                  CALL LINWID(3)
                  CALL TXTJUS('CENT')
                  CALL TXTJUS('MIDDLE')
C                 CALL TXTBGD(0)
                  CALL SETRGB (0.0, 0.0, 0.0)
                  CALL COLOR('RED')
                  CALL RLMESS('O',Xrel,Yrel)
C                CALL CIRCLE(NINT(Xrel*PUperM),NINT(Yrel*PUperM),Nziz/2)
C                 CALL TXTBGD(-1)
              END IF !(Rz2)
          END IF !(REL2)

      END DO !iMemb=1,NMEMB


C     -----
C     NODES
C     -----

      DO iNode = 1, NNODE

C         Examine members framing into this node
C         Determine frame direction away from this node
C         Find shortest and avge length

          Average = 0.0
          N = 0
          Shortest = HUGE(1.1)
          CountH  = 0
          CountV  = 0
          CountHH = 0
          CountVV = 0

          DO iMemb = 1,NMEMB
              iNod1=MEMB(iMemb)%Node1
              iNod2=MEMB(iMemb)%Node2
              If(iNod1.EQ.iNode.OR.iNod2.EQ.iNode) THEN
                  IF(iNod1.EQ.iNode) THEN
                      iEND = iNod1
                      jEND = iNod2
                  ELSE
                      iEND = iNod2
                      jEND = iNod1
                  END IF
                  Xi = NODE(1,iEND)
                  Yi = NODE(2,iEND)
                  Xj = NODE(1,jEND)
                  Yj = NODE(2,jEND)

                  Dx   = Xj-Xi
                  Dy   = Yj-Yi
                  Dsum = ABS(Dx) + ABS(Dy)

                  CountH  = CountH + Dx/Dsum
                  CountHH = CountHH + ABS(CountH)
                  CountV  = CountV + Dy/Dsum
                  CountVV = CountVV + ABS(CountV)

                  Length = SQRT(Dx**2+Dy**2)
                  Average = (Average*FLOAT(N) + Length)/FLOAT(N+1)
                  N=N+1
                  IF(Length.LT.Shortest) Shortest=Length
              END IF
          END DO

          IF(CountHH.GT.CountVV) THEN
C             Framing from iNODE is horizontal or generally horizontal
              IF(CountH.GE.0) THEN
C                 Framing from iNODE is to the RIGHT
                  Framing = 'Rgt'
              ELSE
C                 Framing from iNODE is to the LEFT
                  Framing = 'Lft'
              END IF
          ELSE
C             Framing from iNODE is vertical or generally vertical
              IF(CountV.GE.0) THEN
C                 Framing from iNODE is UPwards or generally UPwards
                  Framing = 'Upw'
              ELSE
C                 Framing from iNODE is DOWNards or generally DOWNards
                  Framing = 'Dnw'
              END IF
          END IF

C         -----------
C         Node number
C         -----------

C         Label position
          Lx = NODE(1,iNode)-FrL
          Ly = NODE(2,iNode)

C         Label size, colour, pen thickness
          NumSiz = NINT(PUperM*(Shortest+3.0*Average)/40.0)
          CALL HEIGHT(MAX(MIN(NumSiz,46),22))
          CALL COLOR('BLUE')
          CALL LINWID(LiVthn)

          CALL RLNUMB(FLOAT(iNode), -1, Lx, Ly)

C         ---------------
C         Node restraints
C         ---------------

C         Dark grey
          CALL SETRGB(0.29, 0.29, 0.29)

          Adj = SymM*0.6

C         Which DoF, if any, are restrained?
          NodRes = ''
          DO iDoF=1,3
              IF( (RCODEd .AND. KFIXD(IDOFN(INODE,iDoF)).LT.0) .OR.
     +          (.NOT.(RCODEd) .AND. (KFIXD(IDOFN(INODE,iDoF)).EQ.1)) )
     +        THEN
                 NodRes=TRIM(NodRes)//ADOF(iDoF:iDoF)
              ELSE
                 NodRes=TRIM(NodRes)//' '
              END IF
          END DO !iDoF=1,3

C         If restrained in ANY DoF...
          IF(LEN_TRIM(NodRes).GT.0) THEN

              IF(Framing.EQ.'Upw' .OR.  Framing.EQ.'Dnw') THEN

C                 Now check which particular DoF need restraint symbols
                  IF(INDEX(NodRes,'Y').GT.0) THEN
C                     Restrained: Y
C                     Medium horiz line barring displmt-Y
                      CALL LINWID(LinMed)
                      CALL RLINE( Lx-Adj, Ly,
     +                            Lx+Adj, Ly )

C                     Also restrained: X ?

                      IF(INDEX(NodRes,'X').GT.0) THEN
C                         Restrained: Y and X
C                         Heavy horiz line barring displmt-XY
                          CALL LINWID(LinThk)
                          CALL RLINE( Lx-Adj, Ly-SymM*0.1,
     +                                Lx+Adj, Ly-SymM*0.1 )
                      ELSE
C                         Restrained: Y but no X
C                         2nd thin horiz line releasing displmt-X
                          CALL LINWID(10)
                          IF(Framing.EQ.'Upw') THEN
                              CALL RLINE( Lx-Adj, Ly-SymM*0.25,
     +                                    Lx+Adj, Ly-SymM*0.25 )
                          ELSE !(Framing.EQ.'Dnw')
                              CALL RLINE( Lx-Adj, Ly+SymM*0.25,
     +                                    Lx+Adj, Ly+SymM*0.25 )
                          END IF !(Framing.EQ.'Upw')

                      END IF !INDEX(NodRes,'X').GT.0)
                  END IF !INDEX(NodRes,'Y').GT.0)

              ELSE
C                 (Framing.EQ.'Rgt' .OR.  Framing.EQ.'Lft')

C                 Now check which particular DoF need restraint symbols
                  IF(INDEX(NodRes,'X').GT.0) THEN
C                     Restrained: X
C                     Medium vert line barring displmt-X
                      CALL LINWID(LinMed)
                      CALL RLINE( Lx, Ly+Adj,
     +                            Lx, Ly-Adj )

C                     Also restrained: Y ?

                      IF(INDEX(NodRes,'Y').GT.0) THEN
C                         Restrained: X and Y
C                         Heavy vert line barring displmt-XY
                          CALL LINWID(LinThk)
                          CALL RLINE( Lx-SymM*0.1, Ly+Adj,
     +                                Lx-SymM*0.1, Ly-Adj )
                      ELSE
C                         Restrained: X but no Y
C                         2nd thin vert line releasing displmt-X
                          CALL LINWID(10)
                          IF(Framing.EQ.'Rgt') THEN
                              CALL RLINE( Lx-SymM*0.255, Ly+Adj,
     +                                    Lx-SymM*0.255, Ly-Adj )
                          ELSE !(Framing.EQ.'Lft')
                              CALL RLINE( Lx+SymM*0.255, Ly+Adj,
     +                                    Lx+SymM*0.255, Ly-Adj )
                          END IF !(Framing.EQ.'Rgt')

                      END IF !INDEX(NodRes,'Y').GT.0)
                  END IF !INDEX(NodRes,'X').GT.0)

              END IF !(Framing.EQ.'Upw' .OR.  Framing.EQ.'Dnw')

C             Also restrained: Z ?

              IF(INDEX(NodRes,'Z').GT.0) THEN
C             Restrained: Y and maybe X, and Z
C             No Z release needed
                  CONTINUE
              ELSE
C                 Restrained: Y and maybe X, but no Z
C                 Z release required
C                 Ring, releasing displmt-Z
                  Nsiz = 10
                  CALL HEIGHT(Nsiz)
                  CALL LINWID(LinRng)
                  CALL TXTJUS('BOTTOM')
                  CALL TXTBGD(0)
                  CALL RLMESS('O',Lx,Ly)
                  CALL TXTBGD(-1)
              END IF !INDEX(NodRes,'Z').GT.0)

          END IF !LEN_TRIM(NodRes).GT.0

      END DO

      CALL DISFIN()
      MadeDRW = .TRUE.

      RETURN
      END






      SUBROUTINE DrawRTF
C     ------------------
C     jw / 06-02-12  draft
C     jw / 14-02-14  last revised


C      0,297 PML                                    PMR  210,297
C           +----+--------------------------------+-----+
C        PMT|    |                                |     |PMT
C           |    |                                |     |
C           +----+--------------------------------+-----+
C        TLT|    |FBL                          FBR|     |TLT
C           +----+---+------------------------+---+-----+
C           | FBT|   |                        |   |FBT  |
C           |    +---+------------------------+---+     |
C           |    |   |Frame              Frame|   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |                        |   |     |
C           |    |   |Frame              Frame|   |     |
C           |    +---+------------------------+---+     |
C           |    |   |                        |   |     |
C           | FBB|   |                        |   |FBB  |
C           +----+---+------------------------+---+-----+
C        TLB|    |FBL                          FBR|     |TLB
C           +----+--------------------------------+-----+
C        PMB|    |                                |     |PMB
C           |    |                                |     |
C           +----+--------------------------------+-----+
C        0,0  PML                                   PMR  0,210
C
C
C      Main layout variable names      |     Suffixes (see mRTF.for)
C      --------------------------      |     --------
C      PMT: Page print margin, Top     |     mm  Page units, millimetres
C      PMB: Page print margin, Btm     |     FU  Frame units
C      PML: Page print margin, Left    |     TW  Twips
C      PMR: Page print margin, Right   |     EM  EMUs
C
C      TLT: Text lines space, Top
C
C      FBT: Frame border, Top
C      FBB: Frame border, Btm
C      FBL: Frame border, Left
C      FBR: Frame border, Right

C      Constants set up by MODULE RTF
C      ------------------------------
C      REAL TWperMM
C      REAL PTperMM
C      REAL MMperPT
C      REAL PTperTW
C      INTEGER TWperPT
C      REAL TWperCHAR
C      REAL TWperLINE

C      REAL MMperCHAR
C      REAL MMperLINE


C      Constants set up by subroutine RTFini
C      -------------------------------------
C      PPGWIDTW = INT(TWperMM*PPGWID)
C      PPGHGTTW = INT(TWperMM*PPGHGT)
C      PRMLHSTW = INT(TWperMM*PRMLHS)
C      PRMRHSTW = INT(TWperMM*PRMRHS)
C      PRMTOPTW = INT(TWperMM*PRMTOP)
C      PRMBTMTW = INT(TWperMM*PRMBTM)

C      PPGWIDTW : paperw
C      PPGHGTTW : paperh
C      PRMLHSTW : margl
C      PRMRHSTW : margr
C      PRMTOPTW : margt
C      PRMBTMTW : margb

      USE dislin
      USE WinSTUFF
      USE Config
      USE Files
      USE LUnits
      USE RTF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING
      USE DRW

      CHARACTER ADoF*3, NodRes*3, ThkthEM*10
      CHARACTER I2CHAR*16

      CHARACTER Framing*3

      REAL      ThkthFU, LenFU, AvgLenFU
      REAL      VthnLinMM, ThinLinMM, MedmLinMM, ThckLinMM,
     +          VthnLinEM, ThinLinEM, MedmLinEM, ThckLinEM

      LOGICAL   Rel(6), REL1, REL2,
     +          Rx1,Ry1,Rz1, Rx2,Ry2,Rz2

      EQUIVALENCE (Rx1, Rel(1)),
     +            (Ry1, Rel(2)),
     +            (Rz1, Rel(3)),
     +            (Rx2, Rel(4)),
     +            (Ry2, Rel(5)),
     +            (Rz2, Rel(6))

      PARAMETER (ADoF   = ('XYZ'))

C     Config.for
C     ----------
C     INTEGER, PARAMETER ::
C    +  PPGWID = 210, !outfile page width, mm
C    +  PPGHGT = 297, !outfile page height, mm
C    +  PRLINW =  83, !outfile characters per line
C    +  PRLIPP =  65, !outfile lines per page
C    +  PRMLHS =  19, !outfile L margin, mm
C    +  PRMRHS =  15, !outfile R margin, mm
C    +  PRMTOP =  16, !outfile T margin, mm
C    +  PRMBTM =  16  !outfile B margin, mm

      REAL    PMTmm  !Page print margin, top,  mm page units
C     REAL    PMTfu  !Page print margin, top,  fu frame units
      INTEGER PMTtw  !Page print margin, top,  Twips

      REAL    PMBmm  !Page print margin, btm,  mm page units
C     REAL    PMBfu  !Page print margin, btm,  fu frame units
C     INTEGER PMBtw  !Page print margin, btm,  Twips

      REAL    PMLmm  !Page print margin, lft,  mm page units
C     REAL    PMLfu  !Page print margin, lft,  fu frame units
      INTEGER PMLtw  !Page print margin, lft,  Twips

      REAL    PMRmm  !Page print margin, rgt,  mm page units
C     REAL    PMRfu  !Page print margin, rgt,  fu frame units
C     INTEGER PMRtw  !Page print margin, rgt,  Twips

      REAL    FBTmm  !Frame border, top,  mm page units
C     REAL    FBTfu  !Frame border, top,  fu frame units
      INTEGER FBTtw  !Frame border, top,  Twips

      REAL    FBBmm  !Frame border, btm,  mm page units
C     REAL    FBBfu  !Frame border, btm,  fu frame units
      INTEGER FBBtw  !Frame border, btm,  Twips

      REAL    FBLmm  !Frame border, lft,  mm page units
C     REAL    FBLfu  !Frame border, lft,  fu frame units
      INTEGER FBLtw  !Frame border, lft,  Twips

      REAL    FBRmm  !Frame border, rgt,  mm page units
C     REAL    FBRfu  !Frame border, rgt,  fu frame units
      INTEGER FBRtw  !Frame border, rgt,  Twips

      INTEGER TLTli  !Text lines space, top, lines
      REAL    TLTmm  !Text lines space, top, mm page units
C     REAL    TLTfu  !Text lines space, top, fu frame units
      INTEGER TLTtw  !Text lines space, top, Twips

      INTEGER TLBli  !Text lines space, btm, lines
      REAL    TLBmm  !Text lines space, btm, mm page units
C     REAL    TLBfu  !Text lines space, btm, fu frame units
      INTEGER TLBtw  !Text lines space, btm, Twips

      REAL    FrWmm  !Frame width in mm
      REAL    FrHmm  !Frame height in mm

      INTEGER FrWtw  !Frame width, Twips
      INTEGER FrHtw  !Frame height, Twips

      REAL    FrWfu  !Frame width, fu frame units
      REAL    FrHfu  !Frame height, fu frame units

      REAL BarLenTW
      REAL GapWidTW
      REAL GapLenTW

      REAL BarLen2TW

      REAL BarX1TW
      REAL BarX2TW
      REAL BarY1TW
      REAL BarY2TW

      REAL XrelTW
      REAL YrelTW

      REAL Stretch

C     Subroutine RTFini will have evaluated MMperLINE and TWperLINE:
C     MMperLINE = Float((PPGHGT-PRMTOP-PRMBTM)/PRLIPP)
C     TWperLINE = MMperLINE * TWperMM


      IF(.NOT. (GoodNOD .OR. GoodMEM)) RETURN

      SymM = 0.03    !Restraint symbol width multiplier on frame size.
                     !Changes later to the restraint symbol width.


C     Don't print members any thinner, or font sizes for node and member
C     number labels get any smaller, than these:
      ThinstLinMM = 0.1  !Thinnest line used for a member in mm
      ThinstLinTW = ThinstLinMM*TWperMM  !Thinnest line in mm
      LablTinyPt  = 5    !Smallest font size in points.
      LablTinyTW  = NINT(FLOAT(LablTinyPt)*MMperPT*TWperMM)

C     Frame size in frame's own real-world units
         MinX = NODE(1,1)
         MaxX = NODE(1,1)
         MinY = NODE(2,1)
         MaxY = NODE(2,1)

      DO iNOD=2,NNODE
         MinX = MIN(MinX,NODE(1,iNOD))      ! frame units, eg metres ('m')
         MaxX = MAX(MaxX,NODE(1,iNOD))      ! frame units
         MinY = MIN(MinY,NODE(2,iNOD))      ! frame units
         MaxY = MAX(MaxY,NODE(2,iNOD))      ! frame units
      END DO

      FrLfu   = FLOAT(INT(MinX))            ! frame units
      FrRfu   = FLOAT(NINT(MaxX))           ! frame units
      FrBfu   = FLOAT(INT(MinY))            ! frame units
      FrTfu   = FLOAT(NINT(MaxY))           ! frame units

      FrWfu   = ABS(FrRfu - FrLfu)          ! frame units
      FrHfu   = ABS(FrTfu - FrBfu)          ! frame units

      FrMfu   = MAX(FrRfu-FrLfu,FrTfu-FrBfu)! frame units

C     Page print margins
      PMLmm   = FLOAT(PRMLHS)
      PMLTW   = TWperMM * PMLmm

      PMRmm   = FLOAT(PRMRHS)

      PMTmm   = FLOAT(PRMTOP)
      PMTTW   = TWperMM * PMTmm

      PMBmm   = FLOAT(PRMBTM)

C     Frame borders
      FBLmm   = 5.0
      FBLTW   = TWperMM * FBLmm

      FBRmm   = 5.0
      FBRTW   = TWperMM * FBRmm

      FBTmm   = 10.0
      FBTTW   = TWperMM * FBTmm

      FBBmm   = 1.5
      FBBTW   = TWperMM * FBBmm

C     Text lines spaces above and below
      TLTli   = 5
      TLBli   = 1
      TLTmm   = FLOAT(TLTli) * MMperLINE
      TLBmm   = FLOAT(TLBli) * MMperLINE

      TLTTW   = TWperMM * TLTmm
      TLBTW   = TWperMM * TLBmm

      PgWmm   = FLOAT(PPGWID)
      PgHmm   = FLOAT(PPGHGT)

      PgWTW   = PgWmm * TWperMM
      PgHTW   = PgHmm * TWperMM

      FrWmm   = PgWmm -(PMLmm+PMRmm)-(FBLmm+FBRmm)
      FrHmm   = PgHmm -(PMTmm+PMBmm)-(FBTmm+FBBmm)-(TLTmm+TLBmm)

      ScaW    = FrWmm/FrWfu          !mm per Frame Unit
      ScaH    = FrHmm/FrHfu          !mm per Frame Unit
      MMperFU = MIN(ScaW,ScaH)       !mm per Frame Unit
      TWperFU = MMperFU * TWperMM    !TW per Frame Unit

      SymLtw  = SymM*FrMfu*TWperFU

      FrWTW   = FrWfu * TWperFU
      FrHTW   = FrHfu * TWperFU

      PlArTW  = FrWTW * FrHTW        !Frame area in TW squared

      FrMLTW = FBLTW
C         (not PMLTW+FBLTW because RTFini sets all page margins)
      FrMTTW = TLTTW+FBTTW
C         (not PMTTW+TLTTW+FBTTW because RTFini sets all page margins)
      FrMBTW = TLBTW

C     ------------------------------------------
C     Leave lines blank as space for the drawing
C     ------------------------------------------
C     LinesForDrg = INT((FrMTTW+FrHTW+TLBTW)/TWperLine)+1
C     WRITE(F%O%LU,'(A)') REPEAT(LF$,LinesForDrg)
C     iLINE = iLINE + LinesForDrg

C     Draw the drawing
C     ----------------
      WRITE(F%O%LU,'(A)') DrgInit

C     Blank box to push text down
C     ---------------------------
      WRITE(F%O%LU,'(A)') BoxNoLine//
     +  ShapeL//'0'//
     +  ShapeR//TRIM(I2CHAR(NINT(PgWTW),iDUM))//
     +  ShapeT//TRIM(I2CHAR(TLTTW,iDUM))//
     +  ShapeB//TRIM(I2CHAR(NINT(FrMTTW+FrHTW+FrMBTW),iDUM))//
     +  ShapeOK

      iLINE = iLINE + INT(FLOAT(TLTTW+FrHTW+TLBTW)/TWperLINE)+1

C     -------
C     MEMBERS
C     -------

C     ------------------
C     Member thicknesses
C     ------------------

C     Thinnest and thickest member types
      ThinstFU = THKTH(1)
      ThikstFU = ThinstFU
      IF(NMTYP.GT.1) then
          DO IPrp=2,NMTYP
             ThkFU = Thkth(IPrp)
             ThinstFU=MIN(ThinstFU,ThkFU)
             ThikstFU=MAX(ThikstFU,ThkFU)
          END DO
      END IF

      LablSmallestTW=HUGE(1)

      DO iMemb=1,NMEMB
C         -----------------------
C         Draw the current member
C         -----------------------
C         Member position
          iNod1=MEMB(iMemb)%Node1
          iNod2 = MEMB(iMemb)%Node2
          iPrp = MEMB(iMemb)%MTyp
          REL  = MEMB(iMemb)%Released
          x1 = NODE(1,iNod1)
          y1 = NODE(2,iNod1)
          x2 = NODE(1,iNod2)
          y2 = NODE(2,iNod2)
          LenFU = SQRT((x2-x1)**2+(y2-y1)**2)
          LinL = NINT(FrMLTW+(x1-FrLfu)*TWperFU)
          LinR = NINT(FrMLTW+(x2-FrLfu)*TWperFU)
C         IF(LinL.GT.LinR) THEN
C             LinTmp=LinL
C             LinL=LinR
C             LinR=LinTmp
C         END IF
          LinT = NINT(FrMTtw+FrHtw-(y1-FrBfu)*TWperFU)
          LinB = NINT(FrMTtw+FrHtw-(y2-FrBfu)*TWperFU)
C         IF(LinT.GT.LinB) THEN
C             LinTmp=LinT
C             LinT=LinB
C             LinB=LinTmp
C         END IF
          ThkthFU = THKTH(iPrp)
          ThkthTW = NINT(MAX(ThkthFU*TWperFU,ThinstLinTW))
          WRITE(ThkthEM,'(I10)') NINT(ThkthTW*FLOAT(EMperTW))

C         Draw the member
          WRITE(F%O%LU,'(A)') StrLine//
     +      LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +      LineGrey//
     +      ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +      ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +      ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +      ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +      ShapeOK

C         -----------------------
C         Write the member number
C         -----------------------

C         Label size
          LablSiz1TW = NINT(SQRT(FLOAT(FrWtw*FrHtw)/
     +                 FLOAT(NMEMB+NNODE))/7.0)
          LablSiz2TW = NINT(TWperFU*LenFU/8.5)
          LablSiz3TW = NINT(ThkthTW*2.0)
          LablSizTW  =
     +    MAX(MIN(LablSiz1TW,LablSiz2TW,LablSiz3TW),LablTinyTW)
          LablSizPT  = 2*LablSizTW/TWperPT   !(2* because RTF halves it)

          LablSmallestTW = MIN(LablSmallestTW,LablSizTW)

C         Label position
          LinCx=NINT(FLOAT(LinL+LinR)/2.0)                 !TW
          LinCy=NINT(FLOAT(LinT+LinB)/2.0)                 !TW

          LabL=LinCx-LablSizTW*1.0 - ThkthTW
          LabR=LinCx+LablSizTW*3.0
          LabT=LinCy-NINT(0.5*FLOAT(LablSizTW))
          LabB=LinCy+NINT(2.0*FLOAT(LablSizTW))

C         Write the label
          WRITE(F%O%LU,'(A)')
     +      '{'//TxtBox//
     +       ArNarrB$//
     +       DarkBlue$//
     +      '\fs'//TRIM(I2CHAR(LablSizPT,iDUM))//' '//
     +      '\qc '//
     +       TRIM(I2CHAR(iMEMB,iDUM))//
     +       ShapeL//TRIM(I2CHAR(LabL,iDUM))//
     +       ShapeT//TRIM(I2CHAR(LabT,iDUM))//
     +       ShapeR//TRIM(I2CHAR(LabR,iDUM))//
     +       ShapeB//TRIM(I2CHAR(LabB,iDUM))//
     +       ShapeOK//'}'

C         -----------------------
C         Any member-end releases
C         -----------------------
          PrpnAlong = 0.015*FLOAT(MNHGHT)/Length 
          PINdia = 20.0 ! Minimum
          
          IF(Rx1.OR.Ry1.OR.Rz1) THEN
              REL1=.TRUE.
          ELSE
              REL1=.FALSE.
          END IF !(Rx1.OR.Ry1.OR.Rz1)

          IF(Rx2.OR.Ry2.OR.Rz2) THEN
              REL2=.TRUE.
          ELSE
              REL2=.FALSE.
          END IF !(Rx2.OR.Ry2.OR.Rz2)

          IF(REL1.OR.REL2) THEN
              BarLenFU = 0.1*LenFU         !Restraint symbol length
              GapWidFU = Max(0.02*LenFU,0.3*ThkthFU) !Lines gap
              BarLenFU = MAX(BarLenFU,1.5*GapWidFU)
              GapLenFU = 0.3*BarLenFU      !Gap in member

              BarLenTW = BarLenFU * TWperFU
              GapWidTW = MAX(MIN(GapWidFU*TWperFU,48.0),31.0)
              GapLenTW = GapLenFU * TWperFU

              GapWid2FU = GapWidFU * 0.5     !Half the gap between lines
              GapLen2FU = GapLenFU * 0.5     !Half the gap
              BarLen2FU = BarLenFU * 0.5     !Half restraint symb length

              GapWid2TW = GapWidTW * 0.5
              GapLen2TW = GapLenTW * 0.5
              BarLen2TW = BarLenTW * 0.5

              Parallel = (y2-y1)/(x2-x1) !Member gradient
              Orthognl = -1.0/Parallel   !Gradient perpendiclr to member

              sinA=(y2-y1)/LenFU         !cos(line gradient)
              cosA=(x2-x1)/LenFU         !sin(line gradient)


          END IF !(REL1.OR.REL2)

          IF(REL1) THEN
C             Release symbol position
              XrelTW = NINT(LinL + PrpnAlong*(FLOAT(LinR)-FLOAT(LinL)))
              YrelTW = NINT(LinT + PrpnAlong*(FLOAT(LinB)-FLOAT(LinT)))

C             Gap in member
              dX = GapWidTW*FIF(Ry1,0.4,1.0)
              dY = dX
              dX = dX*cosA
              dY = dY*sinA

              ThkthTW = NINT(MAX(ThkthFU*TWperFU,1.2*ThinstLinTW))
              WRITE(ThkthEM,'(I10)') NINT(1.2*ThkthTW*FLOAT(EMperTW))

              WRITE(F%O%LU,'(A)') StrLine//
     +          LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +          LineWhite//
     +          ShapeL//TRIM(I2CHAR(NINT(XrelTW-dX),iDUM))//
     +          ShapeT//TRIM(I2CHAR(NINT(YrelTW+dY),iDUM))//
     +          ShapeR//TRIM(I2CHAR(NINT(XrelTW+dX),iDUM))//
     +          ShapeB//TRIM(I2CHAR(NINT(YrelTW-dY),iDUM))//
     +          ShapeOK

              IF(ShowThknss) THEN
C                 CALL LINWID(LinThn)
                  Stretch = 1.25
              WRITE(ThkthEM,'(I10)') NINT(2.5*ThinstLinTW)
              ELSE
C                 CALL LINWID(LiVThn)
                  Stretch = 1.0
              WRITE(ThkthEM,'(I10)') NINT(ThinstLinTW)
              END IF !(ShowThknss)
C
              IF(Rx1) THEN

C                 Parallel-lines symbol parallel to member
                  Stretch=0.9*Stretch
                  BarX1TW = XrelTW-Stretch*BarLen2TW*cosA
                  BarX2TW = XrelTW+Stretch*BarLen2TW*cosA
                  BarY1TW = YrelTW+Stretch*BarLen2TW*sinA
                  BarY2TW = YrelTW-Stretch*BarLen2TW*sinA

                  dX = 1.3*FIF(ShowThknss,1.9,0.9)*GapWid2TW
                  dY = dX
                  dX = dX*sinA
                  dY = dY*cosA

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW-dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW-dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW-dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW-dY),iDUM))//
     +              ShapeOK

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW+dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW+dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW+dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW+dY),iDUM))//
     +              ShapeOK
              END IF !(Rx1)

              IF(Ry1) THEN
C                 Parallel-lines symbol orthogonal to member
                  Stretch=0.8*Stretch
                  BarX1TW = XrelTW-Stretch*BarLen2TW*sinA
                  BarX2TW = XrelTW+Stretch*BarLen2TW*sinA
                  BarY1TW = YrelTW-Stretch*BarLen2TW*cosA
                  BarY2TW = YrelTW+Stretch*BarLen2TW*cosA
                  dX = 0.66*GapWid2TW*cosA
                  dY = 0.66*GapWid2TW*sinA

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW-dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW+dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW-dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW+dY),iDUM))//
     +              ShapeOK

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW+dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW-dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW+dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW-dY),iDUM))//
     +              ShapeOK
              END IF !(Ry1)

              IF(Rz1) THEN
C                 Circular ring hinge symbol
                  dX=0.7*GapWidTW
                  dY=dX
                  WRITE(F%O%LU,'(A)') Donut//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(XrelTW-dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(YrelTW-dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(XrelTW+dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(YrelTW+dY),iDUM))//
     +              ShapeOK

              END IF !(Rz1)
          END IF !(REL1)

          IF(REL2) THEN

C             Release symbol position
              XrelTW = NINT(LinR - PrpnAlong*(FLOAT(LinR)-FLOAT(LinL)))
              YrelTW = NINT(LinB - PrpnAlong*(FLOAT(LinB)-FLOAT(LinT)))

C             Gap in member
              dX = GapWidTW*FIF(Ry2,0.66,1.0)
              dY = dX
              dX = dX*cosA
              dY = dY*sinA

              ThkthTW = NINT(MAX(ThkthFU*TWperFU,2.5*ThinstLinTW))
              WRITE(ThkthEM,'(I10)') NINT(1.2*ThkthTW*FLOAT(EMperTW))

              WRITE(F%O%LU,'(A)') StrLine//
     +          LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +          LineWhite//
     +          ShapeL//TRIM(I2CHAR(NINT(XrelTW-dX),iDUM))//
     +          ShapeT//TRIM(I2CHAR(NINT(YrelTW+dY),iDUM))//
     +          ShapeR//TRIM(I2CHAR(NINT(XrelTW+dX),iDUM))//
     +          ShapeB//TRIM(I2CHAR(NINT(YrelTW-dY),iDUM))//
     +          ShapeOK

              IF(ShowThknss) THEN
C                 CALL LINWID(LinThn)
                  Stretch = 1.25
              WRITE(ThkthEM,'(I10)') NINT(2.5*ThinstLinTW)
              ELSE
C                 CALL LINWID(LiVThn)
                  Stretch = 1.0
              WRITE(ThkthEM,'(I10)') NINT(ThinstLinTW)
              END IF !(ShowThknss)
C
              IF(Rx2) THEN

C                 Parallel-lines symbol parallel to member
                  Stretch=0.9*Stretch
                  BarX1TW = XrelTW-Stretch*BarLen2TW*cosA
                  BarX2TW = XrelTW+Stretch*BarLen2TW*cosA
                  BarY1TW = YrelTW+Stretch*BarLen2TW*sinA
                  BarY2TW = YrelTW-Stretch*BarLen2TW*sinA

                  dX = 1.3*FIF(ShowThknss,1.9,0.9)*GapWid2TW
                  dY = dX
                  dX = dX*sinA
                  dY = dY*cosA

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW-dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW-dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW-dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW-dY),iDUM))//
     +              ShapeOK

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW+dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW+dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW+dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW+dY),iDUM))//
     +              ShapeOK
              END IF !(Rx2)

              IF(Ry2) THEN
C                 Parallel-lines symbol orthogonal to member
                  Stretch=0.8*Stretch
                  BarX1TW = XrelTW-Stretch*BarLen2TW*sinA
                  BarX2TW = XrelTW+Stretch*BarLen2TW*sinA
                  BarY1TW = YrelTW-Stretch*BarLen2TW*cosA
                  BarY2TW = YrelTW+Stretch*BarLen2TW*cosA
                  dX = 0.66*GapWid2TW*cosA
                  dY = 0.66*GapWid2TW*sinA

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW-dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW+dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW-dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW+dY),iDUM))//
     +              ShapeOK

                  WRITE(F%O%LU,'(A)') StrLine//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(BarX1TW+dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(BarY1TW-dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(BarX2TW+dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(BarY2TW-dY),iDUM))//
     +              ShapeOK
              END IF !(Ry2)

              IF(Rz2) THEN
C                 Circular ring hinge symbol
                  dX=0.7*GapWidTW
                  dY=dX
                  WRITE(F%O%LU,'(A)') Donut//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(NINT(XrelTW-dX),iDUM))//
     +              ShapeT//TRIM(I2CHAR(NINT(YrelTW-dY),iDUM))//
     +              ShapeR//TRIM(I2CHAR(NINT(XrelTW+dX),iDUM))//
     +              ShapeB//TRIM(I2CHAR(NINT(YrelTW+dY),iDUM))//
     +              ShapeOK

              END IF !(Rz2)
          END IF !(REL2)

      END DO !iMemb=1,NMEMB


C     -----
C     NODES
C     -----

      DO iNode = 1, NNODE

C         ---------------------
C         Write the node number
C         ---------------------

C         Label size

C         Examine members framing into this node

          AvgLenFU = 0.0
          N = 0
          ShortestFU = HUGE(1.1)
          CountH  = 0
          CountV  = 0
          CountHH = 0
          CountVV = 0

          DO iMemb = 1,NMEMB
              iNod1=MEMB(iMemb)%Node1
              iNod2=MEMB(iMemb)%Node2
              If(iNod1.EQ.iNode.OR.iNod2.EQ.iNode) THEN
                  IF(iNod1.EQ.iNode) THEN
                      iEND = iNod1
                      jEND = iNod2
                  ELSE
                      iEND = iNod2
                      jEND = iNod1
                  END IF
                  Xi = NODE(1,iEND)
                  Yi = NODE(2,iEND)
                  Xj = NODE(1,jEND)
                  Yj = NODE(2,jEND)

                  Dx   = Xj-Xi
                  Dy   = Yj-Yi
                  Dsum = ABS(Dx) + ABS(Dy)

                  CountH  = CountH + Dx/Dsum
                  CountHH = CountHH + ABS(CountH)
                  CountV  = CountV + Dy/Dsum
                  CountVV = CountVV + ABS(CountV)

                  LenFU = SQRT(Dx**2+Dy**2)
                  AvgLenFU = (AvgLenFU*FLOAT(N) + LenFU)/FLOAT(N+1)
                  N=N+1
                  IF(LenFU.LT.ShortestFU) ShortestFU=LenFU
              END IF
          END DO

          IF(CountHH.GT.CountVV) THEN
C             Framing from iNODE is horizontal or generally horizontal
              IF(CountH.GE.0) THEN
C                 Framing from iNODE is to the RIGHT
                  Framing = 'Rgt'
              ELSE
C                 Framing from iNODE is to the LEFT
                  Framing = 'Lft'
              END IF
          ELSE
C             Framing from iNODE is vertical or generally vertical
              IF(CountV.GE.0) THEN
C                 Framing from iNODE is UPwards or generally UPwards
                  Framing = 'Upw'
              ELSE
C                 Framing from iNODE is DOWNards or generally DOWNards
                  Framing = 'Dnw'
              END IF
          END IF

          LablSizTW =
     +    MAX(NINT(TWperFU*(ShortestFU+3.0*AvgLenFU)/52.0),LablTinyTW)
          LablSizPT  = 2*LablSizTW/TWperPT   !(2x because RTF halves it)

C         Label position
          Lx=NINT(FrMLTW+(NODE(1,iNode)-FrLfu)*TWperFU)          !TW
          Ly=NINT(FrMTtw+FrHtw-(NODE(2,iNode)-FrBfu)*TWperFU)    !TW

          LabL=Lx-LablSizTW*1.9
          LabR=Lx+LablSizTW*3.0
          LabT=Ly-NINT(0.5*FLOAT(LablSizTW))
          LabB=Ly+NINT(2.0*FLOAT(LablSizTW))


C         Write the label
          WRITE(F%O%LU,'(A)')
     +      '{'//TxtBox//
     +       ArNarrB$//
     +       Red$//
     +      '\fs'//TRIM(I2CHAR(LablSizPT,iDUM))//' '//
     +      '\qc '//
     +       TRIM(I2CHAR(iNode,iDUM))//
     +       ShapeL//TRIM(I2CHAR(LabL,iDUM))//
     +       ShapeT//TRIM(I2CHAR(LabT,iDUM))//
     +       ShapeR//TRIM(I2CHAR(LabR,iDUM))//
     +       ShapeB//TRIM(I2CHAR(LabB,iDUM))//
     +       ShapeOK//'}'


C         ---------------
C         Node restraints
C         ---------------

          VThnLinMM = 1.0
          VthnLinEM = VThnLinMM * EMperMM

          ThinLinMM = 1.0
          ThinLinEM = ThinLinMM * EMperMM

          MedmLinMM = 2.0
          MedmLinEM = MedmLinMM * EMperMM

          ThckLinMM = 3.3
          ThckLinEM = ThckLinMM * EMperMM

C         Which DoF, if any, are restrained?
          NodRes = ''
          DO iDoF=1,3
             IF( (RCODEd .AND. KFIXD(IDOFN(INODE,iDoF)).LT.0) .OR.
     +           (.NOT.(RCODEd) .AND. (KFIXD(IDOFN(INODE,iDoF)).EQ.1)) )
     +       THEN
                NodRes=TRIM(NodRes)//ADOF(iDoF:iDoF)
             ELSE
                NodRes=TRIM(NodRes)//' '
             END IF
          END DO !iDoF=1,3

C         If restrained in ANY DoF...
          IF(LEN_TRIM(NodRes).GT.0) THEN

             IF(Framing.EQ.'Upw' .OR.  Framing.EQ.'Dnw') THEN

                 IF(Framing.EQ.'Upw') THEN
                     AdjLtw = SymLtw*0.8
                     AdjRtw = SymLtw*0.7
                 ELSE !(Framing.EQ.'Dnw')
                     AdjLtw = SymLtw*0.7
                     AdjRtw = SymLtw*0.8
                 END IF

C                Check which particular DoF need restraint symbols
                 IF(INDEX(NodRes,'Y').GT.0) THEN
C                   Restrained: Y
C                   Medium thknss horiz line barring displmt-Y
                    LinL=NINT(Lx - AdjLtw)  !Twips
                    LinR=NINT(Lx + AdjRtw)  !Twips
                    LinT=Ly                     !Twips
                    LinB=Ly                     !Twips
                    WRITE(ThkthEM,'(I10)') NINT(MedmLinEM)
                    WRITE(F%O%LU,'(A)') StrLine//
     +                LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +                LineGrey//
     +                ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +                ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +                ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +                ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +                ShapeOK

C                   Also restrained: X ?

                    IF(INDEX(NodRes,'X').GT.0) THEN
C                        Restrained: Y and X
C                        Thick horiz line barring displmt-XY
                         LinL=NINT(Lx - AdjLtw)  !Twips
                         LinR=NINT(Lx + AdjRtw)  !Twips
                         LinT=Ly                 !Twips
                         LinB=Ly                 !Twips
                         WRITE(ThkthEM,'(I10)') NINT(ThckLinEM)
                         WRITE(F%O%LU,'(A)') StrLine//
     +                     LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +                     LineGrey//
     +                     ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +                     ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +                     ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +                     ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +                     ShapeOK

                    ELSE
C                        Restrained: Y but no X
C                        2nd, thin, horiz line releasing displmt-X
                         LinL=NINT(Lx - AdjLtw) !Twips
                         LinR=NINT(Lx + AdjRtw) !Twips
                         IF(Framing.EQ.'Upw') THEN
                             LinT =
     +                       Ly+((MedmLinMM+2.0*ThinLinMM)*0.5)*TWperMM
                         ELSE !(Framing.EQ.'Dnw')                 !Twips
                             LinT =
     +                       Ly-((MedmLinMM+2.0*ThinLinMM)*0.5)*TWperMM
                         END IF                                   !Twips
                         LinB=LinT !Twips
                         WRITE(ThkthEM,'(I10)') NINT(ThinLinEM)
                         WRITE(F%O%LU,'(A)') StrLine//
     +                     LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +                     LineGrey//
     +                     ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +                     ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +                     ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +                     ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +                     ShapeOK

                    END IF !INDEX(NodRes,'X').GT.0)

                 END IF !INDEX(NodRes,'Y').GT.0)

             ELSE
C                (Framing.EQ.'Rgt' .OR.  Framing.EQ.'Lft')

                 IF(Framing.EQ.'Rgt') THEN
                     AdjTtw = SymLtw*0.7
                     AdjBtw = SymLtw*0.8
                 ELSE !(Framing.EQ.'Lft')
                     AdjTtw = SymLtw*0.8
                     AdjBtw = SymLtw*0.7
                 END IF

C                Check which particular DoF need restraint symbols
                 IF(INDEX(NodRes,'X').GT.0) THEN
C                   Restrained: X
C                   Medium thknss vert line barring displmt-X
                    LinT=NINT(Ly - AdjTtw)  !Twips
                    LinB=NINT(Ly + AdjBtw)  !Twips
                    LinL=Lx                 !Twips
                    LinR=Lx                 !Twips
                    WRITE(ThkthEM,'(I10)') NINT(MedmLinEM)
                    WRITE(F%O%LU,'(A)') StrLine//
     +                LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +                LineGrey//
     +                ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +                ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +                ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +                ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +                ShapeOK

C                   Also restrained: Y ?

                    IF(INDEX(NodRes,'Y').GT.0) THEN
C                        Restrained: X and Y
C                        Thick vert line barring displmt-XY
                         LinT=NINT(Ly - AdjTtw)  !Twips
                         LinB=NINT(Ly + AdjBtw)  !Twips
                         LinL=Lx                 !Twips
                         LinR=Lx                 !Twips
                         WRITE(ThkthEM,'(I10)') NINT(ThckLinEM)
                         WRITE(F%O%LU,'(A)') StrLine//
     +                     LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +                     LineGrey//
     +                     ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +                     ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +                     ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +                     ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +                     ShapeOK

                    ELSE
C                        Restrained: X but no Y
C                        2nd, thin, vert line releasing displmt-Y
                         LinT=NINT(Ly - AdjTtw) !Twips
                         LinB=NINT(Ly + AdjBtw) !Twips
                         IF(Framing.EQ.'Rgt') THEN
                             LinL =
     +                       Lx-((MedmLinMM+2.0*ThinLinMM)*0.5)*TWperMM
                         ELSE !(Framing.EQ.'Lft')                 !Twips
                             LinL =
     +                       Lx+((MedmLinMM+2.0*ThinLinMM)*0.5)*TWperMM
                         END IF                                   !Twips
                         LinR=LinL !Twips
                         WRITE(ThkthEM,'(I10)') NINT(ThinLinEM)
                         WRITE(F%O%LU,'(A)') StrLine//
     +                     LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +                     LineGrey//
     +                     ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +                     ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +                     ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +                     ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +                     ShapeOK

                    END IF !INDEX(NodRes,'Y').GT.0)

                 END IF !INDEX(NodRes,'X').GT.0)

             END IF !(Framing.EQ.'Upw' .OR.  Framing.EQ.'Dnw')

C            Also restrained: Z ?

             IF(INDEX(NodRes,'Z').GT.0) THEN
C            Restrained: Y and maybe X, and Z
C            No Z release needed
                  CONTINUE
             ELSE
C                 Restrained: Y and maybe X, but no Z
C                 Z release required
C                 Circle, releasing displmt-Z
                  LinL=NINT(Lx - SymLtw*0.21)  !Twips
                  LinR=NINT(Lx + SymLtw*0.21)  !Twips
                  LinT=Ly - SymLtw*0.22        !Twips
                  LinB=Ly + SymLtw*0.20        !Twips
                  WRITE(ThkthEM,'(I10)') NINT(ThinLinEM)
                  WRITE(F%O%LU,'(A)') Ellipse//
     +              LineThk//TRIM(ADJUSTL(ThkthEM))//ShapeOK//
     +              ShapeL//TRIM(I2CHAR(LinL,iDUM))//
     +              ShapeT//TRIM(I2CHAR(LinT,iDUM))//
     +              ShapeR//TRIM(I2CHAR(LinR,iDUM))//
     +              ShapeB//TRIM(I2CHAR(LinB,iDUM))//
     +              ShapeOK

             END IF !INDEX(NodRes,'Z').GT.0)

          END IF !LEN_TRIM(NodRes).GT.0

      END DO

      WRITE(F%O%LU,'(A)') CR$//DrgOK
      MadeDRW = .TRUE.

      RETURN
      END




      FUNCTION THKTH(Iprp)
C     --------------------
C     Returns an indication of the member 'thickness' (section depth).
C
      USE  PROPERTIES
      USE  PARAMS
      USE  DRW
      REAL THKTH, AreaX, InrtZ

      IF(ShowThknss) THEN
          InrtZ = PROPS(IPrp)%InrtZ
          AreaX = PROPS(IPrp)%AreaX

C         Calculate D from I/A assuming a solid section
          Dsolid = SQRT(12.0*InrtZ/AreaX)

C         Now see what A should be using the value of D just obtained,
C         if the section is solid.

C         B is unlikely to be less than 0.2D so assume that it is so.
          Bsolid = 0.2*Dsolid

C         Now deduce what A must be if it is a solid section.
          Asolid = Bsolid * Dsolid

          IF(AreaX.GE.Asolid) THEN
              THKTH = Dsolid
          ELSE
C             Area too small for a solid section, so assume flanged.
C             Assume Bflngd=0.333*Dflngd
C             Assume t=0.04*Bflgd
C             Assume T=0.095*Dflgd
C             Thus, approximately:
              Dflngd = SQRT(12.0*AreaX)
              THKTH = Dflngd
          END IF

      ELSE
          THKTH = TINY(1.1)

      END IF !(ShowMemThk)

      RETURN
      END


      SUBROUTINE SetDrawLin
C     ---------------------
C     jw / 12-02-14  draft
C     jw / 12-02-14  last revised

C     Set diagram to show thin lines only

      USE dislin
      USE WINSTUFF
      USE FILES
      USE DRW

      ShowThknss = .FALSE.
      MORE = .TRUE.
      F%InpDataPosted = .FALSE.
      CALL SENDOK
      RETURN

      END


      SUBROUTINE SetDrawThk
C     ---------------------
C     jw / 12-02-14  draft
C     jw / 12-02-14  last revised

C     Set diagram to show estimated member thicknesses (section depths)

      USE dislin
      USE WINSTUFF
      USE FILES
      USE DRW

      ShowThknss = .TRUE.
      MORE = .TRUE.
      F%InpDataPosted = .FALSE.
      CALL DrawScr
      RETURN

      END



