      SUBROUTINE RCODE
C     ----------------
C     jw / 23-10-85  1st draft
C     jw / 01-09-12  last amended.
C
C     Subroutine to set up restraints code vector in IFPRE()
C
C     How the restraints code vector system works
C    (explanation of restraints/prescribed displacements storage):
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     1.  All restraints are stored as prescribed displacements, that
C         may have either zero or non-zero values.
C
C     2.  They are input initially by subroutine ReadRESTR as
C         restrained DoF that are flagged as restraints by means of
C         negative integers stored in the KFIXD array.
C
C     3.  Further input by subroutine PRESCR then accepts
C         displacements, if any, only at negative-flagged DoFs.
C         Default prescribed displacement values are all zero,
C         meaning a support that does not move.
C
C     4.  The stiffness matrix can be REDUCED, by removing or
C         ignoring rows and columns that correspond to restraints.
C         IDOFG refers to original unreduced equations.
C         IDOFR refers to the reduced equations.
C
C     5.  IDOFR is calculated as a function of IDOFG by subtracting
C         from it the number of restraints that come before it in
C         ascending IDOFG sequence.  This number-of-restraints is
C         stored in a vector of restraints codes.
C
C     6.  Two arrays are used to store the restraints codes.
C         The array are as follows and the meaning of the codes is
C         as explained in 7. :-
C             - KFIXD(NDOFG)
C             - PRESC(NPRES)
C
C     7.    Condition              Meaning
C         ------------------       -------
C         KFIXD(IDOFG) = nn   -->  IDOFG is unrestrained but there
C         (nn.GE.0)                are nn lower-numbered restraints.
C
C         KFIXD(IDOFG) = nn   -->  IDOFG is restrained by a prescribed
C         (nn.LT.0)                displacement and it is the (nn)th
C                                  such prescribed-displacement in
C                                  the sequence DOF = 1...IDOFG
C                                  The values of the prescribed displa-
C                                  cements are stored in PRESC(nn).
C
C     Notes on the actions of this subroutine
C     - - - - - - - - - - - - - - - - - - - -
C     On entry to RCODE, KFIXD has already been initialised by
C     subroutine RESTRD to contain all zeros except at the prescribed
C     restraints (supports) DoF, each of which contains the figure 1.
C     PRESC contains all zeros because any actual non-zero values of
C     the prescribed displacements have not yet been read in.
C
C     RCODE must then:
C     (i)   Store a running total of restraints at each restraint
C           position in KFIXD in place of the figure 1;  these will
C           be pointers to the corresponding PRESC values (if any)
C           that will be input later by means of a call from
C           subroutine LOADS;
C     (ii)  Change the signs of KFIXD pointer values to negative,
C           as an indication that they are restraint pointers;
C     (iii) Populate the remaining zero values in KFIXD with ascending
C           positive values equal in each case to the most
C           recently-passed restraint number in the ascending sequence.

      USE DISLIN
      USE PARAMS
      USE Restraints
      INTEGER IFIX, IDOF
      CHARACTER I2CHAR*16

      IF(RCODEd) CALL RdeCODE
C     If the accumulated count of rigid restraints actually entered, by
C     calls to GetRESTR from both ReadRESTR and/or ReadNODES, is not as
C     large as NPRES, warn the user and optionally continue with NPRES
C     adjusted to the lower number.

      IF(iPRES.LT.NPRES) THEN
C        DO a re-count, in case, whilst the nodes table and/or
C        restraints table(s) was/were being edited, the user has deleted
C        any restraints

         iPRES = 0
         DO iDOF=1,NDOFG
            iPRES=iPRES+KFIXD(iDOF)
         END DO !iDOF=1,NDOFG

C        Now re-check
         IF(iPRES.LT.NPRES) THEN
             CALL BailOut
     +          ('Warning: '//TRIM(I2CHAR(NPRES,iDum))//' rigid '//
     +          'restraints were specified but only '//
     +          TRIM(I2CHAR(iPRES,iDum))//' have been entered.$',
     +          'OPTIONS' ,'YES')
             NPRES = iPRES
             NDOFR = NDOFG-NPRES
         END IF !(iPRES.LT.NPRES) re-check after the re-count
      END IF !(iPRES.LT.NPRES) original check

      IFIX = 0

      DO 100 IDOF=1,NDOFG
         IF(KFIXD(IDOF).NE.0) GO TO 10

C        Not restrained
C        - - - - - - - -
         KFIXD(IDOF) = IFIX
         GO TO 100
C
C        Restrained
C        - - - - - -
10       IFIX=IFIX+1

C        Change the sign of the restraint pointer so that its negative
C        value will disinguish it from values representing the numbers
C        of previous restraints, that positive-signed restraint pointers
C        signify at unrestrained DoF.
C
         KFIXD(IDOF) = -IFIX

C        Next IDOF value please...
100      CONTINUE

      RCODEd = .TRUE.

      RETURN
      END





      SUBROUTINE RdeCODE
C     ------------------
C     jw / 14-07-13  1st draft
C     jw / 14-07-13  last edited.
C
      USE PARAMS
      USE Restraints
      INTEGER iDoF

         DO iDoF=1,NNODE*3
            IF(KFIXD(iDoF).LT.0) THEN
               KFIXD(iDoF)=1
            ELSE
               KFIXD(iDoF)=0
            END IF !(KFIXD(iDoF).LT.0)
         END DO !iDoF=1,NNODE*3

      RCODEd = .FALSE.

      RETURN
      END
