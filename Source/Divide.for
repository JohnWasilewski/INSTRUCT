      FUNCTION DIVIDE(V1,V2)
C     -----------------------
C     jw / 18-11-08  draft
C     jw / 12-09-12  last revised

C     Division without divide-by-zero overflow

	   REAL*8 DIVIDE

C     Zero
      IF (V1.EQ.0.0) DIVIDE=0.0

C     Divide by zero
      DIVIDE=HUGE(1.1)

C     Normal division
      IF (V2.NE.0.0) DIVIDE=V1/V2

      RETURN
      END
