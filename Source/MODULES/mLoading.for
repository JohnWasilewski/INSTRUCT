      MODULE LOADING
C     --------------
C     jw 25-05-12 draft
C     jw 25-05-12 last amended

      LOGICAL  ShowsLOD, GoodLOD

      INTEGER  idLodPAN,
     +            idLodPAN1,
     +            idLodPAN2

      INTEGER  idLCSelPAN,
     +            idLCSelPAN1,
     +            idLCSelPAN2

      INTEGER  idLodTbl,
     +         iLCas, SelectLCas,PrevLCas,
     +         idLCas,
     +         idLCNameTbl


      LOGICAL  ShowsCOMB,GoodCOMB

      INTEGER  idCombPAN,
     +            idCombPAN1,
     +            idCombPAN2

      INTEGER  idCombSelPAN,
     +            idCombSelPAN1,
     +            idCombSelPAN2

      INTEGER  idCombTbl,
     +         iCOMB, SelectLComb,PrevLComb,
     +         idComb,
     +         idCombNamTbl

      INTEGER  idMore

C     INTEGER  NLOAD  !Declared in module PARAMS (file: mPARAMS)
C     INTEGER  NLCAS  !Declared in module PARAMS (file: mPARAMS)
C     INTEGER  NCOMB  !Declared in module PARAMS (file: mPARAMS)
C     INTEGER  NLCASC !Declared in module PARAMS (file: mPARAMS)

      LOGICAL  ChangedLC, ChangedComb

      CHARACTER*48, ALLOCATABLE :: LCName(:), CombName(:)

C     Input definitions for POS,AXES,LTYP,FX,FY,MZ,DESC:
C     ------------------------------------------------------------------
C     Load         POS               AXES   LTYP   FX,FY,MZ   DESC
C     Condition   (F7.3)/(I7)        (A1)   (A1)   (3F8.3)    (21A1)
C     ------------------------------------------------------------------
C     NODE         Node               'G'   'N'   Fx,Fy,Mz   Description
C     point        no.               Only        (forces)   in words
C     load
C     ------------------------------------------------------------------
C     POINT        Member.Proprtn     'G'   'P'   Fx,Fy,Mz   Description
C     load on      MMM.ppp where       or        (forces)   in words
C     a member     MMM=memb.no.       'L'
C                  ppp=propn
C                  along member
C     ------------------------------------------------------------------
C     UDL on a     Member             'G'   'U'   fx,fy,mz   Description
C     member       Read as INT data    or        (intens-   in words
C                  value giving       'L'          ities)
C                  member no.
C                 (propn ignored)
C     ------------------------------------------------------------------
C     TDL on a     Member no. with    'G'   'U'   fx,fy,mz   Description
C     member       a 'L','R', or 'M'   or        (intens-   in words
C                  after it for a     'L'          ities)
C                  left, iight or
C                  isosceles triangle
C                  distributed load
C                  Default, if left
C                  blank, is 'M'
C     ------------------------------------------------------------------
C     DeadLoad     Member             'G'   'D'   fx,fy,mz   Description
C     of a         Read as INT data              (material   in words
C     member       value giving                  densities
C                  member no.                    in reqrd
C                 (propn ignored)                directions)
C     ------------------------------------------------------------------
C     SETTLEment   Node               'G'   'S'   dX,dY,@Z   Description
C     of a                            only                   in words
C     support
C    (prescribed
C     displacemt)
C     ------------------------------------------------------------------

      Type :: LOADS
         REAL             ::   Pos
         CHARACTER(36)    ::   Desc
         CHARACTER(1)     ::   Axes, LTyp, TDLshp
         REAL             ::   Fx,Fy,Mz
      END TYPE

      TYPE(LOADS),   ALLOCATABLE :: LOADCASE(:,:)

      Type :: COMBS
         INTEGER          ::   LCnum
         REAL             ::   Factr
      END TYPE

      TYPE(COMBS),   ALLOCATABLE :: LOADCOMB(:,:)

      END MODULE Loading