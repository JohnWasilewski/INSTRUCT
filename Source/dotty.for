      FUNCTION DOTTY(A,B, IB1, KOUNT)
C     -------------------------------
C     jw / 01-01-87  draft
C     jw / 22-05-04  rewritten
C     Vector dot product algorithm from Prof.Taylor, modified
C     to allow B to be either an unreduced or a reduced array.
C
C     IB1 serves both as a flag to show whether, when (IB1.NE.0),
C     array B needs to be reduced, and as a pointer to where the
C     vector required for the dot product begins in array B.
C
C     If IB1 =  0  then array B is already reduced and
C                  the vector begins at B(1)
C
C     If IB1 <> 0  then array B is unreduced and the reduced
C                  vector begins at B(IDOFG(IB1)) and
C                  all terms (I) of the the vector must be
C                  fetched using IDOFG(I)

      USE PARAMS
      INTEGER  IB1, KOUNT
      REAL*8   DOTTY, A(1), B(1)
      DOTTY = 0.0
C
      DO 100 I=1,KOUNT
         IA=I
         IB=I
         IF(IB1.NE.0) IB = IDOFG(IB1-1+I)
 100    DOTTY = DOTTY + A(IA)*B(IB)

      RETURN
      END
