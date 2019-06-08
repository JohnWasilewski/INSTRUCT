      SUBROUTINE ADDMEM(STIFS,NODE1,NODE2)
C     ------------------------------------
C     jw / 12-03-86  draft
C     jw / 07-09-12  last amended

C     Add the upper triangle of member stiffness STIFS into the upper
C     triangle of the reduced global structure stiffness matrix array.

      USE PARAMS
      USE Restraints
      !INTEGER, ALLOCATABLE  :: KFIXD(:)
      !REAL*8, ALLOCATABLE   :: PRESC(:,:)
      !TYPE(SprungSUPP), ALLOCATABLE :: SPRING(:)
      !   Type :: SprungSUPP
      !   INTEGER  :: NODE
      !   INTEGER  :: DoF
      !   REAL*4   :: Stiff
      !END TYPE

      USE M
      !INTEGER, ALLOCATABLE :: JDIAG(:)
      !REAL*8,  ALLOCATABLE :: RSTIF(:)
      !REAL*8,  ALLOCATABLE :: ACTNS(:,:,:)
      !REAL*4,  ALLOCATABLE :: FORCR(:,:)
      !REAL*4,  ALLOCATABLE :: SREAC(:,:)
      !REAL*8,  ALLOCATABLE :: REACT(:,:)
      !REAL*8,  ALLOCATABLE :: VECTR(:,:)
      !REAL*8,  ALLOCATABLE :: EQLIB(:,:)
      !REAL*8,  ALLOCATABLE :: EQLIR(:,:)

      ! In Subroutine ALLOC4: ALLOCATE (RSTIF(JDIAG(NDOFR)))

      INTEGER  NODE1,NODE2,NDIFF, INOD,JNOD, IDOF,JDOF,JDOF1
      REAL*8   STIFS(21)

      NDIFF = NODE2-NODE1
      IMTERM = 0

      DO 1000 INOD = NODE1,NODE2,NDIFF
         DO 1000 IDOF = 1,3
            DO 1000 JNOD = INOD,NODE2,NDIFF
               JDOF1=1
               IF(JNOD.EQ.INOD) JDOF1=IDOF

               DO 1000 JDOF = JDOF1,3

C                 Member stiffness term
                  IMTERM = IMTERM+1

C                 Structure stiffness term
                  IF(JNOD.GE.INOD) THEN
                     ISTERM = KPOS(INOD,IDOF, JNOD,JDOF)
                  ELSE
                     ISTERM = KPOS(JNOD,JDOF, INOD,IDOF)
                  END IF

C                 Add member to structure except where restrained
C                 CALL Debug1('AddMem  ',1)
                  IRFN=IDOFR(IDOFN(INOD,IDOF))
C                 CALL Debug1('AddMem  ',2)
                  IF(IRFN.NE.0) THEN
C                    CALL Debug2(ISTERM,IMTERM,STIFS)
                     RSTIF(ISTERM) = RSTIF(ISTERM) + STIFS(IMTERM)
                  END IF
C                 CALL Debug1('AddMem  ',3)

 1000          CONTINUE

      RETURN
      END
