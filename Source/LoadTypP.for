      SUBROUTINE LoadTypP ( PROPN, FX,FY,MZ, AXES,
     +                      Lx,Ly,L, Rel, RELEASED, FIXITY )
C     ------------------------------------------------------
C     jw / 15-11-11  Extracted from Subroutine LOADS
C     jw / 15-11-11  last rev.

C     -----------------
C     Member POINT load
C     -----------------

C     FOR One NODE LOAD entry:
C      - Calculate and store the fixity actions.
C      - Don't add the reversed fixity actions to the load vector, VECTR
C        (this is done by the calling subroutine).
C      - Do not preserve load input data on exit from this subroutine.
C
C     Input definitions for POS,AXES,LTYP,FX,FY,MZ,DESC:
C     -----------------------------------------------------------------
C     Load         POS              AXES  LTYP   FX,FY,MZ   DESC
C     Condition   (F7.3)/(I7)       (A1)  (A1)   (3F8.3)    (16A1)
C     -----------------------------------------------------------------
C     POINT        Member.Proprtn    'G'   'P'   Fx,Fy,Mz   Description
C     load on      MMM.ppp where      or         (forces)   in words
C     a member     MMM=memb.no.      'L'
C                  ppp=propn
C                  along member
C     -----------------------------------------------------------------

C     Note that signs are easier to follow if it is remembered that
C     ACTNS are actions applied internally BY THE NODES to the members
C     whereas VECTR contains forces applied externally TO THE NODES,
C     either directly on the nodes or indirectly via the members.

      LOGICAL     Released, Rel(6)

      CHARACTER*1 AXES

      REAL*8      PROPN,
     +            FX,FY,MZ,
     +            Lx,Ly,L,L2,L3,A,A2,B,B2, FIXITY(6)

C        Transform member loads to equiv. forces in local axes
C        -----------------------------------------------------
         IF(AXES.EQ.'G') CALL ROTATE(1,Lx,Ly,L,FX,FY,MZ)

C        ---------------------------------------------
C        Member fixity actions for member point load :
C        ---------------------------------------------
C        First, calculate the fixity
C        actions for a UDL on a member
C        that is fully fixed at both ends,
C        then deduct the necessary amounts
C        to allow for any member-end releases
C        --------------------------------------

C        FULLY FIXED CONDITION

         A=PROPN*L
         A2=A*A
         B=L-A
         B2=B*B
         L2=L*L
         L3=L*L2

C..      Member fixity actions for member point load
C        -------------------------------------------

C..      Fixity actions (X-axis)
C        -----------------------

         FIXITY(1)=-FX*B/L
         FIXITY(4)=-FX*A/L

C..      Fixity actions (Y-axis)
C        -----------------------
         FIXITY(2)=-FY*(3.0*A+B)*B2/L3 + 6.0*MZ*A*B/L3
         FIXITY(5)=-FY*(A+3.0*B)*A2/L3 - 6.0*MZ*A*B/L3

C..      Moment fixity actions (Z-axis)
C        ------------------------------
         FIXITY(3)=-FY*A*B2/L2 + MZ*(2.0*A-B)*B/L2
         FIXITY(6)= FY*A2*B/L2 + MZ*(2.0*B-A)*A/L2


C        ALLOW FOR ANY MEMBER-END RELEASES
         IF (Released) CALL MemRel(FIXITY,FIXITY,Rel,L)

C        Member fixity actions will be added to
C        ACTNS & VECTR on returning to the calling routine, FORCE

      RETURN

      END
