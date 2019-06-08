      SUBROUTINE SKYDIA
C     -----------------
C     jw / 08-01-86  1st draft
C     jw / 07-08-04  last amended.
C
C     This subroutine is a necessary preprocessor for the equation-
C     solving subroutine, SOLVE, which is a skyline (profile) solver.
C     The call to SKYDIA must in fact be made before setting up the
C     stiffness matrix array, so that stiffness element offsets can
C     be found during setup operations.
C
C     First   Calculate the 'skyline' profile of column heights above
C     -----   the diagonal of the reduced stiffness matrix.
C
C     Second  Add up and store in JDIAG a vector of offsets to the
C     ------  diagonal elements, from the start of the 1-D array, in
C             column-order, of the reduced stiffness matrix.
C
C     Example ( 1  2  .  .  .  .  . )   COLHT = ( 1 )   JDIAG = (  1 )
C     ------- ( 2  3  4  6  .  .  . )           ( 2 )           (  3 )
C             ( .  4  5  7  . 11  . )           ( 2 )           (  5 )
C             ( .  6  7  8  9 12  . )           ( 3 )           (  8 )
C             ( .  .  .  9 10 13 15 )           ( 2 )           ( 10 )
C             ( .  . 11 12 13 14 16 )           ( 4 )           ( 14 )
C             ( .  .  .  . 15 16 17 )           ( 3 )           ( 17 )
C
C     Note    Because JDIAG is made from running subtotals of COLHT it
C     ----    could occupy the same memory storage.  To avoid possible
C             problems if the compiler should object to an EQUIVALENCE
C             statement that acts on one of the subroutine arguments
C             (i.e. JDIAG), the varible name, 'COLHT', will not be used
C             but 'JDIAG' will be used synonymously for both JDIAG and
C             COLHT.
C
C     Note    JDIAG is calculated on the basis of only upper triangular
C     ----    non-zero stiffness elements being stored.

      USE PARAMS
      USE MEMBERS
      USE Restraints
      USE M


      INTEGER   END1, END2

      INTEGER   IX, IY, IZ,
     +          NODE1, NODE2, LONOD, INOD, NDIFF

      INTEGER   IDOF, JDOFR, LODOFR, ICOLHT

      DATA      END1, END2  /1, 2/
      DATA      IX, IY, IZ  /1, 2, 3/

C
C     Initialise JDIAG vector
C

      JDIAG = 0
C
C     Look at each member in turn
C
      DO 100 IMEMB=1,NMEMB
C
C      ..and examine each end of the member
         NODE1 = MEMB(IMEMB)%NODE1
         NODE2 = MEMB(IMEMB)%NODE2

         IF(NODE2.GT.NODE1)
     +   THEN
C           Nodes already in the required ascending order
            GO TO 12
         ELSE
C           Nodes are in reverse order, so swap them
            ITEMP=NODE1
            NODE1=NODE2
            NODE2=ITEMP
C           Nodes are now in ascending order
         END IF
C
C        Now loop through all of the unreduced DoF at both ends of the
C        member to find the lowest-numbered DoF that is not restrained
C
12       NDIFF = NODE2-NODE1
         DO 20 INOD=NODE1,NODE2,NDIFF

            DO 20 IDOF=IX,IZ
               LODOFR = IDOFR(IDOFN(INOD,IDOF))
C
C              Jump out of this double loop if a restraint indicator
C              (zero value) was not returned by IDOFR
20             IF(LODOFR.GT.0) GO TO 50

C
C        If this part of the program is reached then all six DoF in the
C        member must have been restrained and no entries are required
C        in the JDIAG array for this member, so skip to the next member
         GO TO 100

C
C        The lowest-numbered unrestrained DoF has been found
50       LONOD = INOD

C
C        Starting with the lowest-numbered DoF just found, calculate
C        stiffness matrix column heights for this and each one of any
C        other unrestrained DoF, at either end of the current member.
C        For each column-height calculated at the current DoF, update
C        the last-stored column-height if the new value exceeds it.
C
         ICOLHT=0
         DO 99 INOD = LONOD,NODE2,NDIFF

            DO 99 IDOF=IX,IZ

               JDOFR = IDOFR(IDOFN(INOD,IDOF))
C
C              This time, skip the rest of this double loop if a
C              restraint indicator (zero value) WAS returned by IDOFR
               IF(JDOFR.EQ.0) GO TO 99

               ICOLHT = JDOFR-LODOFR+1
               JDIAG(JDOFR)= MAX(JDIAG(JDOFR),ICOLHT)

C              Next DoF at this node-end of the member, please;
C              If no more DoF at this node, then next end please...
99             CONTINUE

C        If no more DoF at either end, then next member please..
100      CONTINUE

C
C     Accumulate column-heights to give JDIAG values
C
      DO 200 IDOF=2,NDOFR
200      JDIAG(IDOF) = JDIAG(IDOF-1) + JDIAG(IDOF)

      JDND = JDIAG(NDOFR)

C\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\debug
C     write(F%O%LU,101) JDND
C101   FORMAT('\par SKYDIA: RSTIF has',I5,' stiffness coefficients\par ')
C
C     CALL Debug1('SkyDia  ',1)
C//////////////////////////////////////////////////////////////////debug
      RETURN
      END
