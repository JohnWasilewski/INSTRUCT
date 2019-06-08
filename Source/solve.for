      SUBROUTINE SOLVE (DCOMP,BAKSUB)
C     -------------------------------
C     jw /  01-01-87  draft
C     jw /  18-12-03  rewritten
C     jw /  14-05-04  last rev
C
C     Active column profile solver
C     RSTIF =U(t)*D*U factorisation and/or backsubstitution of
C     symmetric positive definite equations RSTIF*DISP = VECTR
C
C     SkyDIA explains the packing scheme to store all stiffness
C     coefficients lying below the 'skyline' profile in a 1-D array.
C
C     Original code by Prof. R L Taylor in Zienkiewicz(1977).
C     NB nearly identical code is found in SUBROUTINE SKY_SOLVE_SYM,
C     in the Fortran library SKYLINE_LIB by J. E. Akin, for which
C     Akin claims (seemingly wrongly) copyright 1999.
C
C     Adapted from the Taylor original by J M Wasilewski.
C     Harmonised with calling program data structure.
C     Altered and extended to provide:
C
C      - Code to permit a group of load case VECTRs to be solved
C        together in a single pass through the BAKSUB section.
C
C      - Solution with a reduced stiffness array from which rows and
C        columns corresponding to zero-displacement restraints have
C        been removed but with UNreduced right hand side VECTRs.
C        Reduced DoF are related to global DoF by the
C        reduction vector, KFIXD.

C     Legend
C     ------
C      NLCAS = Number of loadcases
C
C      DCOMP = Logical: if true, perform factorisation RSTIF=U(t)*D*U
C
C     BAKSUB = Logical: if true, perform forward-reduction of VECTR
C              and back-substitution.
C
C      NDOFR = Nett ('reduced') number of DoF (= NDOFG-NPRES)
C              in the reduced K-matrix RSTIF
C              (= number of equations).
C
C      JDIAG = Pointer array - offsets from start of RSTIF to
C              diagonal pivot elements.
C
C      KFIXD = Restraints code vector - a vector of reductions
C              giving difference between unreduced DoF (all DoF nos)
C              and reduced DoF (with restraints stripped out);
C              a negative value reduction indicates an actual
C              restraint;  more explanation in subroutine RCODE.
C
C      VECTR = Array of UNreduced RHS load vectors for all load
C              cases, stored in 1-dimensional form, load case by
C              load case.  Holds displacements on exit.
C
C      RSTIF = Upper triangular skyline portion only of the reduced
C              (restraints removed) stiffness matrix for the whole
C              structure, stored in column-order. Holds D & U on exit.
C
C
C      NB   the term "reduced" has two distinct meanings -
C      REDUCED STIFFNESSES means stiffness matrix with all restrained
C      rows and columns stripped out; REDUCED EQUATIONS means equations
C      partially or fully solved.

C     IMPLICIT NONE

      USE Params
      USE Restraints
C-/-------------------------------------------------------------debug-\-
      USE WINSTUFF
      USE LUnits
      USE Files
C-/-------------------------------------------------------------debug-/-
      USE M
      USE Results

      LOGICAL  DCOMP, BAKSUB

      INTEGER  I, ID,IE,IH,IR,IS, J,JD,JH,JR,L
      INTEGER  ILCAS

      REAL*8   DOTTY, D, ENRGY


      IF(ALLOCATED(DeflRSLT)) DEALLOCATE(DeflRSLT)
      ALLOCATE (DeflRSLT(NLCas,NNODE,3))
      DeflRSLT = 0.0

      IF(ALLOCATED(MembRSLT)) DEALLOCATE(MembRSLT)
      ALLOCATE (MembRSLT(NLCas,NMemb,6))
      MembRSLT = 0.0

      IF(ALLOCATED(Reac)) DEALLOCATE(Reac)
      ALLOCATE (Reac(NLCas,NPRES+NSPRI))
      Reac%iDoF = 0
      Reac%iNod = 0
      Reac%RSLT = 0.0

C     Factorise RSTIF into Ut*D*U, reduce RHS
C     - - - - - - - - - - - - - - - - - - - -

      JR = 0

      DO 600 J = 1,NDOFR

         JD = JDIAG(J)
         JH = JD-JR
         IS = J-JH+2

         IF(JH-2) 600,300,100
100      IF(.NOT.DCOMP) GOTO 500

C        DCOMP SECTION BEGINS
C        - - - - - - - - - - -
C        CALL DISP(1,' **Factorisation ',17,0)
         IE = J-1
         L  = JR+2
         ID = JDIAG(IS-1)

C        Reduce all equations excluding diagonal terms
C        - - - - - - - - - - - - - - - - - - - - - - -

         DO 200 I = IS,IE

            IR = ID
            ID = JDIAG(I)
            IH = MIN0(ID-IR-1,I-IS+1)

            IF(IH.GT.0) RSTIF(L) =
     +         RSTIF(L) - DOTTY(RSTIF(L-IH),RSTIF(ID-IH),0,IH)

200       L = L+1

C        Reduce diagonal terms
C        - - - - - - - - - - -

300      IF ( .NOT. DCOMP ) GO TO 500
         IR = JR+1
         IE = JD-1
         L  = J-JD
         DO I = IR,IE

            ID= JDIAG(L+I)
            IF(RSTIF(ID).EQ.0.0)  THEN
               CALL BailOUT
     +         ('ERROR|Zero diagonal in structure stiffnesses.$',
     +         'Giveup','No')
            ELSE
               D = RSTIF(I)
               RSTIF(I) = RSTIF(I) / RSTIF(ID)
               RSTIF(JD) = RSTIF(JD) - D * RSTIF(I)
            END IF

         END DO

500      IF(BAKSUB) THEN

C           REDUCE RHS.
C           BAKSUB SECTION BEGINS
C           - - - - - - - - - - -

            DO 510 ILCAS = 1,NLCAS

510            VECTR(IDOFG(J),ILCAS) = VECTR(IDOFG(J),ILCAS)
     +               - DOTTY(RSTIF(JR+1),VECTR(1,ILCAS),IS-1,JH-1)
         END IF

600      JR = JD

      IF(.NOT.BAKSUB)  RETURN


      DO 1200 ILCAS = 1,NLCAS

C        Divide by diagonal pivots
C        - - - - - - - - - - - - -

         ENRGY = 0.0

         DO 700 I = 1,NDOFR
            ID = JDIAG(I)
            IDOF = IDOFG(I)
            D  = VECTR(IDOF,ILCAS)
            IF(RSTIF(ID).NE.0.0)
     +      VECTR(IDOF,ILCAS) = VECTR(IDOF,ILCAS)/RSTIF(ID)
700         ENRGY = ENRGY + D*VECTR(IDOF,ILCAS)

         IF (ENRGY .LT. 0.0) THEN
            CALL ERROR(50,' structure has negative energy')
         ELSE
C           Write(LUS,'(''  - equation strain energy ='',G8.3)')
C    +      SQRT(ENRGY)
         END IF

C        Backsubstitute
C        - - - - - - - -
C
C        CALL DISP(0,'  - backsubstitute',18,0)
         J = NDOFR
         JD = JDIAG(J)


800      D = VECTR(IDOFG(J),ILCAS)
         J = J-1
         IF(J.LE.0) GO TO 1200
         JR = JDIAG(J)

         IF(JD-JR.GT.1) THEN
            IS = J - JD+JR + 2
            L = JR - IS + 1
            DO 900 I = IS, J
               IDOF=IDOFG(I)
900            VECTR(IDOF,ILCAS) = VECTR(IDOF,ILCAS) - RSTIF(I + L) * D
         END IF

         JD = JR
         GO TO 800

1200     CONTINUE
      RETURN
      END

