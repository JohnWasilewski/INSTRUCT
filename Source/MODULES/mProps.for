      MODULE PROPERTIES
C     -----------------
C     Member types
C     jw 19-03-12 last amended

      LOGICAL  ShowsPRP, GoodPRP

      INTEGER  idPrpPAN,
     +            idPrpPAN1,
     +            idPrpPAN2,
     +            idPrpTbl

C     REAL PROPS([Matl, AX,AY,IZ, Desc*24],IMTYP)
      Type :: PROPERTY
         INTEGER        :: Matl
         REAL           :: AreaX, AreaY, InrtZ
         CHARACTER(24)  :: Desc
      END TYPE

      TYPE(PROPERTY), ALLOCATABLE :: PROPS(:)

      END MODULE PROPERTIES