      MODULE M
C     --------
C     Memory allocations for the analysis

      LOGICAL MemRsrvd
      
C
C     INTEGER JDIAG(IDOFR)
C     - - - - - - - - - - -
      INTEGER, ALLOCATABLE :: JDIAG(:)

C
C     REAL RSTIF([JDIAG(IDOFR)])
C     - - - - - - - - - - - - - -
C     NBYTES = NBYTES + JDIAG(NDOFR)*8
      REAL*8,  ALLOCATABLE :: RSTIF(:)

C
C     REAL ACTNS([ILCAS*IMEMB*6])
C     - - - - - - - - - - - - - -
C     NBYTES = NBYTES + NLCAS*NMEMB*6*8
      REAL*8,  ALLOCATABLE :: ACTNS(:,:,:)

C
C     REAL FORCR([ILCAS*(iPRES)])
C     - - - - - - - - - - - - - -
C     NBYTES = NBYTES + NLCAS*NPRES*4
      REAL*8,  ALLOCATABLE :: FORCR(:,:)

C
C     REAL VECTR([IDOFG*ILCAS])
C     - - - - - - - - - - - - - -
C     NBYTES = NBYTES + NLCAS*NDOFG * 8
      REAL*8,  ALLOCATABLE :: VECTR(:,:)

C
C     REAL EQLIB([ILCAS*IDOFG])
C     - - - - - - - - - - - - - -
C     NBYTES = NBYTES + NLCAS*NDOFG * 8
      REAL*8,  ALLOCATABLE :: EQLIB(:,:)

C
C     REAL EQLIR([2*ILCAS])
C     - - - - - - - - - - - - -
C     NBYTES = NBYTES + 2*8*NLCAS
      REAL*8,  ALLOCATABLE :: EQLIR(:,:)

      END MODULE M
