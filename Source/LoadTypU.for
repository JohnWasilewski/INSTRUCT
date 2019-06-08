      SUBROUTINE LoadTypU ( ILCAS, IMEMB, PROPN, AXES,LTYP,Rel,
     +                      AX, Lx,Ly,L, FX,FY,MZ, GAMMA, RELEASED,
     +                      FIXITY, * )
C     ---------------------------------------------------------------
C     jw / 15-11-11  Extracted from Subroutine LOADS
C     jw / 15-11-11  last rev.

C     --------------------
C     Member UDL or DEADLD
C     --------------------

C     FOR One NODE LOAD entry:
C      - Print out the load data input.
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
C     UDL on a     Member            'G'   'U'   fx,fy,mz   Description
C     member       Read as INT data   or         (intens-   in words
C                  value giving      'L'          ities)
C                  member no.
C                 (propn ignored)
C     -----------------------------------------------------------------
C     DeadLoad     Member            'G'   'D'   fx,fy,mz   Description
C     of a         Read as INT data              (material   in words
C     member       value giving                  densities
C                  member no.                    in reqrd
C                 (propn ignored)                directions)
C     -----------------------------------------------------------------

C     Note that signs are easier to follow if it is remembered that
C     ACTNS are actions applied internally BY THE NODES to the members
C     whereas VECTR contains forces applied externally TO THE NODES,
C     either directly on the nodes or indirectly via the members.


      LOGICAL     Released, Rel(6)

      CHARACTER*1 AXES, LTYP, I2CHAR*16

      REAL*4      Ax

      REAL*8      PROPN,
     +            Fx,FdensX, Fy,FdensY, MZ,
     +            Lx,Ly,L,L2, FIXITY(6)


C        Check for illogical loading
C        ---------------------------
         IF(PROPN.NE.0)
     +      CALL BailOUT('PROPN not valid with UDL, '//
     +      'as entered for member '//I2Char(IMEMB,iDum)//
     +      ', in loadcase '//I2Char(ILCAS,iDum)//'$',
     +      ' ',' ')

         IF(LTYP.EQ.'D') THEN

            IF(AXES.EQ.'L')
     +         CALL BailOUT('Density entered on local member axes '//
     +         'on member '//I2Char(IMEMB,iDum)//
     +         ', instead of GLOBAL structure axes.'//
     +         ', in loadcase '//I2Char(ILCAS,iDum)//'$',
     +         ' ',' ')

            IF(AXES.EQ.'G' .AND. FX.NE.0.0)
     +         CALL BailOUT('Dead load does not normally act '//
     +         'in the FX direction, as entered '//
     +         'on member '//I2Char(IMEMB,iDum)//
     +         ', in loadcase '//I2Char(ILCAS,iDum)//'$',
     +         ' ',' ')

            IF(AXES.EQ.'G' .AND. FX.EQ.0.0 .AND. FY.GT.0.0)
     +         CALL BailOUT('Axes usually orientated with minus-Y '//
     +         'as the vertically downwards direction, and not as '//
     +         'on member '//I2Char(IMEMB,iDum)//
     +         ', in loadcase '//I2Char(ILCAS,iDum)//'$',
     +         ' ',' ')

            IF(MZ.NE.0.0)
     +         CALL BailOUT('Density not possible on the Z axis, '//
     +         'as entered for member '//I2Char(IMEMB,iDum)//
     +         ', in loadcase '//I2Char(ILCAS,iDum)//'$',
     +         ' ',' ')

            IF(FX.EQ.0.0 .AND. FY.EQ.0.0) FY=(-GAMMA)
         END IF

C        Convert densities to UDLs if necessary
C        --------------------------------------
         IF(LTYP.EQ.'D') THEN
            FdensX=Fx
            FdensY=Fy
            FX=Ax*FdensX
            FY=Ax*FdensY
         ELSE
            FX=Fx
            FY=Fy
         END IF


C        Transform member loads to equiv. forces in local axes
C        -----------------------------------------------------
         IF(AXES.EQ.'G') CALL ROTATE(1,Lx,Ly,L,FX,FY,MZ)

C        --------------------------------------
C        Member fixity actions for member UDL :
C        --------------------------------------
C        First, calculate the fixity
C        actions for a UDL on a member
C        that is fully fixed at both ends,
C        then deduct the necessary amounts
C        to allow for any member-end releases
C        --------------------------------------

C        FULLY FIXED CONDITION

         L2=L*L

         FIXITY(1) = -FX*L/2.0
         FIXITY(2) = -FY*L/2.0 + MZ
         FIXITY(3) = -FY*L2/12.0
         FIXITY(4) = -FX*L/2.0
         FIXITY(5) = -FY*L/2.0 - MZ
         FIXITY(6) =  FY*L2/12.0

C        ALLOW FOR ANY MEMBER-END RELEASES
         IF (Released) CALL MemRel(FIXITY,FIXITY,Rel,L)

C        Member fixity actions will be added to
C        ACTNS & VECTR on returning to the calling routine, LOADS

C        Revert UDLs to densities if necessary
C        -------------------------------------
         IF(LTYP.EQ.'D') THEN
            FX=FdensX
            FY=FdensY
         END IF

      RETURN
201   RETURN 1 !after error report

      END
