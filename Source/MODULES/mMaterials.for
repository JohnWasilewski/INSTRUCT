      MODULE MATERIALS
C     ----------------
C     jw 17-02-12 draft
C     jw 18-02-12 last amended

      LOGICAL  ShowsMAT, GoodMAT

      INTEGER  idMatPAN,
     +            idMatPAN1,
     +            idMatPAN2,
     +            idMatTbl

      Type :: MATERIAL
         REAL           ::   Gamma,Emod,Gmod
         CHARACTER(24)  ::   Desc
      END TYPE

      TYPE(MATERIAL), ALLOCATABLE :: MATLS(:)

      END MODULE MATERIALS