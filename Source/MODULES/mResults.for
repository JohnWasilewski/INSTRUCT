      MODULE Results
C     --------------

      REAL*8, ALLOCATABLE :: DeflRSLT(:,:,:)   !(NLCAS,NNODE,3)
      REAL*8, ALLOCATABLE :: MembRSLT(:,:,:)   !(NLCAS,NMENB,6)

      Type :: ReacRSLTS
         INTEGER  :: iDoF
         INTEGER  :: iNod
         REAL*8   :: RSLT
      END TYPE

      TYPE(ReacRSLTS), ALLOCATABLE :: Reac(:,:)   !(NLCAS,NPRES+NSPRI)

      END MODULE Results