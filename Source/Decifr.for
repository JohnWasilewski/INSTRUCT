      FUNCTION DECIFR(CODED,NDECI,IDECI,RDECI,NEGFLG)
C     -----------------------------------------------


C     jw / 08-03-87 draft
C     jw / 09-02-09 re-written
C        /          last rev

C     Parse the components of two numbers coded into one and
C     report if negative sign.
C
C     CODED = A string variable containing a real number representing
C     two numbers coded into one with the decimal point as delimiter
C
C     E.G. :
C     '15.050' may mean either (15 and 0.050) or (15 and 50),
C     both possibilities being returned in (DECIFR and RDECI)
C     and in (DECIFR and IDECI) respectively.
C
C     NDECI=The number of decimal places in CODED.
C
C     DECIFR=The principal integer value that is returned
C
C     IDECI =The secondary integer value that is returned, if any.
C     RDECI =The decimal fraction, if any (same digits as IDECI).
C     NOTE: If CODED has no whole number part then DECIFR obtains its
C     principal integer value from the decimal fraction, leaving zero
C     in the latter
C     EXAMPLES:
C     - - - - -
C     DECIFR('15.050',3,IDECI,RDECI,NEGFLG)
C     -->  15(15.050, 3,   50, .050,.FALSE.)
C
C     DECIFR('-15.050',3,IDECI,RDECI,NEGFLG)
C     -->  15(-15.050, 3,   50, .050,.TRUE.)
C
C     DECIFR('.015',3,IDECI,RDECI,NEGFLG)
C     -->  15(.015, 3,    0, .000,.FALSE.)

      CHARACTER LTRIM*80, FINDCHR
      CHARACTER CODED*20, A*1
      INTEGER   DECIFR,NDECI,IDECI
      REAL*8    RDECI
      LOGICAL   NEGFLG

      DATA TOL /1.0E-9/

      CODED=LTRIM(CODED)

      NEGFLG=.FALSE.
      LENCOD=LENSTR(CODED)
      IDOT=INDEX(CODED,',')
      IF(IDOT.GT.0) LENCOD=IDOT-1

      IF(CODED(1:1).EQ.'-') THEN
         NEGFLG=.TRUE.
         CODED(1:LENCOD-1)=CODED(2:LENCOD)
         CODED(LENCOD:LENCOD)=' '
         CODED =TRIM(CODED)
         LENCOD=LENCOD-1
      END IF

      READ(CODED,*) RDECI
      DECIFR=INT(RDECI)
      RDECI=RDECI-REAL(DECIFR)

      IDOT=0
      NDECI=0
      IDECI=0

      A=FINDCHR(CODED,IDOT,'.,',0)

      IF(A .EQ. '.') THEN
         NDECI=LENCOD-IDOT
         DO 14 II=1,NDECI
            IDECI=10*IDECI

            READ(CODED(IDOT+II:IDOT+II),*) IDIGIT
14          IDECI=IDECI+IDIGIT
      END IF

      RETURN

      END
