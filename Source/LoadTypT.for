      SUBROUTINE LoadTypT ( ILCAS, IMEMB, PROPN, AXES, TDLshp, Rel,
     +                      Lx,Ly,L, FX,FY,MZ, RELEASED,
     +                      FIXITY, * )
C     -----------------------------------------------------------------
C     jw / 15-11-11  Extracted from Subroutine LOADS
C     jw / 15-11-11  last rev.

C     ------------------------------------------
C     Member TDL (Triangularly DIstributed Load)
C     ------------------------------------------

C     FOR One NODE LOAD entry:
C      - Print out the load data input.
C      - Calculate and store the fixity actions.
C      - Don't add the reversed fixity actions to the load vector, VECTR
C        (this is done by the calling subroutine).
C      - Do not preserve load input data on exit from this subroutine.
C
C     Input definitions for POS,AXES,LTYP,FX,FY,MZ,DESC:
C     ------------------------------------------------------------------
C     Load         POS               AXES   LTYP   FX,FY,MZ   DESC
C     Condition   (F7.3)/(I7)        (A1)   (A1)   (3F8.3)    (21A1)
C     ------------------------------------------------------------------
C     TDL on a     Member no. with    'G'   'U'   fx,fy,mz   Description
C     member       a 'L','R', or 'M'   or        (intens-   in words
C                  after it for a     'L'          ities)
C                  left, iight or
C                  isosceles triangle
C                  distributed load.
C                  Default, if left
C                  blank, is 'M'
C     ------------------------------------------------------------------

C     Note that signs are easier to follow if it is remembered that
C     ACTNS are actions applied internally BY THE NODES to the members
C     whereas VECTR contains forces applied externally TO THE NODES,
C     either directly on the nodes or indirectly via the members.


      LOGICAL     Released, Rel(6)

      CHARACTER*1 AXES, TDLshp, I2CHAR*16


      REAL*8      PROPN,
     +            Fx,Fy,MZ,
     +            Lx,Ly,L,L2, FIXITY(6)


C        Check for illogical loading
C        ---------------------------
         IF(PROPN.NE.0)
     +      CALL BailOUT('PROPN not valid with TDL, '//
     +      'as entered for member '//I2Char(IMEMB,iDum)//
     +      ', in loadcase '//I2Char(ILCAS,iDum)//'$',
     +      ' ',' ')


C        Transform member loads to equiv. forces in local axes
C        -----------------------------------------------------
         IF(AXES.EQ.'G') CALL ROTATE(1,Lx,Ly,L,FX,FY,MZ)

C        --------------------------------------
C        Member fixity actions for member UDL :
C        --------------------------------------
C        First, calculate the fixity
C        actions for a TDL on a member
C        that is fully fixed at both ends,
C        then deduct the necessary amounts
C        to allow for any member-end releases
C        --------------------------------------

C        FULLY FIXED CONDITION

         L2=L*L

         IF(TDLshp.EQ.'i') THEN
             FIXITY(1) = -FX*L/3.0
             FIXITY(2) = -FY*L/3.0 + MZ/2.0
             FIXITY(3) = -FY*L2/20.0
             FIXITY(4) = -FX*L/6.0
             FIXITY(5) = -FY*L/6.0 - MZ/2.0
             FIXITY(6) =  FY*L2/30.0
         ELSE IF(TDLshp.EQ.'m') THEN
             FIXITY(1) = -FX*L/4.0
             FIXITY(2) = -FY*L/4.0 + MZ/2.0
             FIXITY(3) = -FY*L2/19.2
             FIXITY(4) = -FX*L/4.0
             FIXITY(5) = -FY*L/4.0 - MZ/2.0
             FIXITY(6) =  FY*L2/19.2
         ELSE IF(TDLshp.EQ.'j') THEN
             FIXITY(1) = -FX*L/6.0
             FIXITY(2) = -FY*L/6.0 + MZ/2.0
             FIXITY(3) = -FY*L2/30.0
             FIXITY(4) = -FX*L/3.0
             FIXITY(5) = -FY*L/3.0 - MZ/2.0
             FIXITY(6) =  FY*L2/20.0
         ELSE
C            Something wrong - error trap needed
         END IF

C        ALLOW FOR ANY MEMBER-END RELEASES
         IF (Released) CALL MemRel(FIXITY,FIXITY,Rel,L)

C        Member fixity actions will be added to
C        ACTNS & VECTR on returning to the calling routine, LOADS

      RETURN
201   RETURN 1 !after error report

      END
