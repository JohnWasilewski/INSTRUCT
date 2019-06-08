      SUBROUTINE ADDSPR
C     -----------------
C     jw / 29-05-86  draft
C     jw /  1-02-87  last corrected

C     Add all spring stiffnesses to global structure stiffnesses.

      USE Params
      USE Restraints
      USE M

      DO 1000 ISPRI = 1,NSPRI
C        IRFN = IDOFR(LOSPR(ISPRI))
C        IRFN = IDOFR(3*SPRING(ISPRI)%NODE-1+SPRING(ISPRI)%DoF)
         IRFN = IDOFR(IDOFN(SPRING(ISPRI)%NODE,SPRING(ISPRI)%DoF))
 1000    IF(IRFN.NE.0)
     +     RSTIF(JDIAG(IRFN)) = RSTIF(JDIAG(IRFN)) + SPRING(ISPRI)%Stiff

      RETURN
      END
