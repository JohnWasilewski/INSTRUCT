      MODULE MEMBERS
C     --------------
C     jw 06-04-12 last amended

      LOGICAL  ShowsMEM, GoodMEM

      INTEGER  idMemPAN,
     +            idMemPAN1,
     +            idMemPAN2,
     +            idMemTbl


C     INTEGER MEMB
C    +( [INODE1,INODE2,MTYP,Released(6),
C    +   StiffsStored, MStif(21%Local, MStif(21)%Global,
C    +   Desc*24], IMEMB )
      Type :: MEMBER
         INTEGER           :: NODE1, NODE2
         INTEGER           :: MTyp
         LOGICAL           :: Released(6)
         LOGICAL           :: StiffsStored
         REAL*8            :: LStiff(21)
         REAL*8            :: GStiff(21)
         CHARACTER(24)     :: Desc
      END TYPE

      TYPE(MEMBER), ALLOCATABLE :: MEMB(:)

C     Member data addressable as (eg):
C         MEMB(iMemb)%Node1
C         MEMB(iMemb)%Node2
C         MEMB(iMemb)%MTyp
C         MEMB(iMemb)%Released(iDoF)
C         MEMB(iMemb)%StiffsStored
C         MEMB(iMemb)%GStiff(17)
C         MEMB(iMemb)%LStiff(21)

      END MODULE MEMBERS