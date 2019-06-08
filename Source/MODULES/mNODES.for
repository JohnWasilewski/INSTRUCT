      MODULE NODES
C     ------------
C     jw 19-03-12 last amended

      LOGICAL  ShowsNOD, GoodNOD

      INTEGER  idNodPAN,
     +            idNodPAN1,
     +            idNodPAN2,
     +            idNodTbl

C     REAL NODE ( [XCOORD,YCOORD], INODE )
      REAL*4,  ALLOCATABLE :: NODE(:,:)

      END MODULE NODES 