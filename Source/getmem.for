      SUBROUTINE GETMEM(iMEMB,NODE1,NODE2,
     +           AX,AY,IZ,Lx,Ly,L,GAMMA,EMOD,GMOD,Rel)
C     ------------------------------------------------
C     jw / 21-02-86  draft
C     jw / 14-09-12  last amended

C     Returns properties and nodal connections of member IMEMB,
C     with nodes in ascending order if SORTED is specified as .TRUE.

      USE Members
      USE Materials
      USE Properties
      USE Nodes

      LOGICAL   Rel(6)
      INTEGER   iMEMB, NODE1,NODE2, IMTYP,IMATL

      REAL*4    AX, AY, IZ, EMOD, GMOD

      REAL*8    Lx, Ly, L

C     CALL DISP(0,'**GETMEM',8,1)

C     CALL Debug1('GetMem  ',1)


      iMTYP = MEMB(iMEMB)%MTyp
      iMATL = PROPS(iMTyp)%Matl
      AX    = PROPS(iMTyp)%AreaX
      AY    = PROPS(iMTyp)%AreaY
      IZ    = PROPS(iMTyp)%InrtZ
      NODE1 = MEMB(iMEMB)%Node1
      NODE2 = MEMB(iMEMB)%Node2
      X1    = NODE(1,NODE1)
      X2    = NODE(1,NODE2)
      Lx    = X2-X1
      Y1    = NODE(2,NODE1)
      Y2    = NODE(2,NODE2)
      Ly    = Y2-Y1
      L     = SQRT(Lx*Lx+Ly*Ly)

      GAMMA = MATLS(iMatl)%Gamma
      EMOD  = MATLS(iMatl)%Emod
      GMOD  = MATLS(iMatl)%Gmod
      Rel   = MEMB(iMEMB)%Released

      RETURN
      END
