      MODULE RESTRAINTS
C     -----------------
C     jw 17-04-12 last amended

C     Two arrays are used to store the restraints codes:-
C       - KFIXD(NDOFG)
C       - PRESC(NPRES).
C
C     They are used in the following way:
C
C     Condition              Meaning
C     ------------------       -------
C     KFIXD(IDOFG) = nn   -->  IDOFG is unrestrained but there
C     (nn.GE.0)                are nn lower-numbered restraints.
C
C     KFIXD(IDOFG) = nn   -->  IDOFG is restrained by a prescribed
C     (nn.LT.0)                displacement and it is the (nn)th
C                              such prescribed-displacement in
C                              the sequence DOF = 1...IDOFG
C                              The values of the prescribed displa-
C                              cements are stored in PRESC(nn).
C
C     PRESC(IPRES), set initially to zero,
C     stores the prescribed displacements.
C
C     There is more information about this in subroutine RCODE.


      LOGICAL  MadeRES, ShowsRes, GoodRES, GoodSPR,
     +         RCODEd

      INTEGER  idResPAN,
     +            idResPAN1,
     +            idResPAN2,
     +            idResPAN3,
     +            idResTbl

      INTEGER  idSprPAN,
     +            idSprPAN1,
     +            idSprPAN2,
     +            idSprPAN3,
     +            idSprTbl

      INTEGER iPRES !needed so that the number of rigid restraints can
                    !be accumulated from both the restraints data input
                    !and/or any restraints entered with node coordinates


C     RIGID RESTRAINTS & REACTIONS
C     ----------------------------
C     INTEGER*4 KFIXD(NDOFG) ! Restraint codes for all nodes, all DoF
      INTEGER, ALLOCATABLE  :: KFIXD(:)

C     REAL*8 PRESC(NLCAS,NPRES) ! Any prescribed displacemts for all DoF
      REAL*8, ALLOCATABLE   :: PRESC(:,:)
C
C     REAL REACT([ILCAS*iPRES])
      REAL*8,  ALLOCATABLE :: REACT(:,:)


C     SPRING RESTRAINTS & REACTIONS
C     -----------------------------

      Type :: SprungSUPP
         INTEGER  :: NODE
         INTEGER  :: DoF
         REAL*4   :: Stiff
      END TYPE

      TYPE(SprungSUPP), ALLOCATABLE :: SPRING(:)

      INTEGER iSpri !needed here so that ReadNODES can see how many
                    !springs accumulated when RWinput called ReadSPRI
C
C     REAL SREAC([ILCAS*(ISPRI)])
      REAL*8,  ALLOCATABLE :: SREAC(:,:)

      END MODULE Restraints