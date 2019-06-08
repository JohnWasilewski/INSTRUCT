      SUBROUTINE ROTATE(SENSE, Lx,Ly,L, FX,FY,MZ)
C     --------------------------------------------
C     jw /  05-03-87  draft
C     jw /  08-08-07  last amended

C     Transform F(X),F(Y),M(Z) from local member axes to global structure
C     axes or vice-versa depending upon the value of SENSE.
C
C     Local member axes lie at an angle A to global structure axes
C     where A = arctan(Ly/Lx), the positive direction being clockwise
C     about Z or anticlockwise about a lower left X-Y origin.
C
C     When SENSE = -1
C     - - - - - - - -
C     Transform Fx,Fy,Mz from local MEMBER axes to global STRUCTURE axes
C     (analogous to negative rotation back to structure axes):
C
C     [ FX ]                       [ Fx ]
C     [ FY ]  =   [ ROTMAT ]  *    [ Fy ]
C     [ MZ ]                       [ Mz ]
C
C
C     When SENSE = +1
C     - - - - - - - -
C     Transform FX,FY,FZ from global STRUCTURE axes to local MEMBER axes
C     (analogous to positive rotation up from structure axes):
C
C     [ Fx ]              Transpose      [ FX ]
C     [ Fy ]  =  [ ROTMAT ]          *   [ FY ]
C     [ Mz ]                             [ MZ ]
C
C
C     Where..
C                         [ COSA  -SINA    0  ]
C       [ ROTMAT ]   =    [ SINA   COSA    0  ]
C                         [   0      0     1  ]
C
C
      INTEGER      SENSE
      REAL*8       Lx,Ly,L, SINA,COSA, FX,FY,MZ, FX1

C     CHARACTER*16 DUMMY

C     CALL DISP(0,'**ROTATE',8,1)

C     Next two 'workaround' statements plus 3 statements below are
C     needed to overcome an unexplained problem with G77 compiler
C           Lxx=Lx
C           Lyy=Ly

      SINA = Ly/L
      COSA = Lx/L

      FX1  = FX
      FX   = FX*COSA + FY*SENSE*SINA

C     If the five 'workaround' statements are removed then, with the G77
C     compiler, the next statement also sets the value of Ly to zero
      FY   = FX1*(-SENSE)*SINA + FY*COSA

C     Three more 'workaround' statements (see above)
C           Lx=Lxx
C           Ly=Lyy
C           WRITE(DUMMY,'(F8.3,F8.3)' ) Ly, Lyy

C     MZ remains unchanged
C     MZ   = MZ
      RETURN
      END
