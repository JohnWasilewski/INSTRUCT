      SUBROUTINE STIFF
C     ----------------
C     jw / 25-05-86  draft
C     jw / 13-05-01  last corrected

C     Setup reduced stiffness matrix RSTIF for the whole structure.
C     Storage follows the 'skyline' scheme within the symmetrical
C     half band, using vector JDIAG to point to diagonal elements.
C
C     More details are given by comments in subroutine SKYDIA which
C     sets up JDIAG.
C
C     SkyDIA explains the packing scheme to store all stiffness
C     coefficients lying below the 'skyline' profile in a 1-D array


      USE M
      USE Params
      USE Materials
      USE Properties
      USE Nodes
      USE Members
      USE Restraints

      LOGICAL  GLOBAL
      INTEGER  IMEMB, NODE1, NODE2

C     REAL*4   AX,AY,IZ, EMOD,GMOD
      REAL*8   TERMS(21), DX,DY,DL

      DATA     GLOBAL /.TRUE. /

C     Initialise stiffnesses array
      RSTIF = 0.0
      RSTIF(1)=0.0

      DO iSTIF=1,JDND
         RSTIF(iSTIF)=0.0
      END DO

C     Set up stiffnesses array, member by member

      DO  IMEMB = 1,NMEMB
         CALL MEMSTF(IMEMB,GLOBAL,NODE1,NODE2,TERMS,DX,DY,DL)
         CALL ADDMEM(TERMS,NODE1,NODE2)
      END DO


      IF(NSPRI.GT.0)
     +   CALL ADDSPR

      RETURN
      END
