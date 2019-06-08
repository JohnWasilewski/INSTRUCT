      SUBROUTINE MEMREL(FIXITY,FullFix,Rel,L)
C     ---------------------------------------
C     jw / 27-11-11  first draft
C     jw / 05/12-11  last rev.

C     -----------------------------------
C     Release member ends where specified
C     -----------------------------------

      LOGICAL     Rel(6)
      INTEGER     ir
      REAL*8, INTENT(INOUT):: FIXITY(6)
      REAL*8, INTENT(IN)   :: FullFix(6),L

      IF(Rel(1)) THEN
C        FIXITY(1) 0 [See DO loop below]
         FIXITY(4) = FIXITY(4) + FullFix(1)
         END IF !(Rel(1))

      IF(Rel(2)) THEN
C        FIXITY(2) 0 [See DO loop below]
         FIXITY(3) = FIXITY(3) - FullFix(2)*L/2.0
         FIXITY(5) = FIXITY(5) + FullFix(2)
         FIXITY(6) = FIXITY(6) - FullFix(2)*L/2.0
         END IF !(Rel(2))

      IF(Rel(3)) THEN
         FIXITY(2) = FIXITY(2) + 1.5*FullFix(3)/L
C        FIXITY(3) 0 [See below: DO ir=1,6]
         FIXITY(5) = FIXITY(5) + 1.5*FullFix(3)/L
         FIXITY(6) = FIXITY(6) + FullFix(3)/2.0
         END IF !(Rel(3))

      IF(Rel(4)) THEN
         FIXITY(1) = FIXITY(1) + FullFix(4)
C        FIXITY(4) 0 [See DO loop below]
         END IF !(Rel(4))

      IF(Rel(5)) THEN
         FIXITY(2) = FIXITY(2) + FullFix(5)
         FIXITY(3) = FIXITY(3) + FullFix(5)*L/2.0
C        FIXITY(5) 0 [See DO loop below]
         FIXITY(6) = FIXITY(6) + FullFix(5)*L/2.0
         END IF !(Rel(5))

      IF(Rel(6)) THEN
         FIXITY(2) = FIXITY(2) - 1.5*FullFix(6)/L
         FIXITY(3) = FIXITY(3) - FullFix(6)/2.0
         FIXITY(5) = FIXITY(5) + 1.5*FullFix(6)/L
C        FIXITY(6) 0 [See DO loop below]
         END IF !(Rel(6))

      DO ir=1,6
         IF(REL(ir)) FIXITY(ir) = 0.0
         END DO !ir=1,6

      RETURN
      END
      