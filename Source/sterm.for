      FUNCTION STERM(TERMS,IPOS,JPOS)
C     -------------------------------
C     jw / 11-05-87  draft
C     jw / 08-01-04  last rev
C
C     Returns the (IPOS,JPOS)th term of the 6x6 square matrix of
C     member stiffnesses.
C
C     Note that the symmetrical lower triangle of member stiffness
C     terms is not in fact stored.  The 2-D upper triangular array of
C     member stiffnesses is calculated and stored by subroutine MEMSTF
C     as a 1-D array according to the following storage convention:
C
C     (  1   2   3   4   5   6  )
C     (      7   8   9  10  11  )
C     (         12  13  14  15  )
C     (             16  17  18  )
C     (                 19  20  )
C     (                     21  )
C
C     Formula for the
C     position of the
C     (i,j)th term in
C     the 1-D array =    ( (12-i)*(i-1)/2 + j )
C
C
C     For more information, see subroutine MEMSTF.

      REAL*8       STERM, TERMS(21)
      INTEGER      IPOS,JPOS

      IF (JPOS.GE.IPOS) THEN
          STERM=TERMS((12-IPOS)*(IPOS-1)/2 + JPOS)
      ELSE
          STERM=TERMS((12-JPOS)*(JPOS-1)/2 + IPOS)
          END IF

      RETURN
      END
