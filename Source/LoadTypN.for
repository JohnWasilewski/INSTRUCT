      SUBROUTINE LoadTypN ( ILCAS,INODE,FX,FY,MZ )
C     --------------------------------------------

C     jw / 02-11-11  Extracted from Subroutine LOADS
C     jw / 18-09-12  Last rev.

C     ---------
C     NODE load
C     ---------

C     FOR One NODE LOAD entry:
C      - Calculate and store the fixity actions.
C      - Sum the reversed fixity actions to the load vector, VECTR.
C      - Do not preserve load input data on exit from this subroutine.
C      - Do preserve prescribed displacement values because they will
C        be needed at results-printout time.
C
C     Input definitions for POS,AXES,LTYP,FX,FY,MZ,DESC:
C     -----------------------------------------------------------------
C     Load         POS              AXES  LTYP   FX,FY,MZ   DESC
C     Condition   (F7.3)/(I7)       (A1)  (A1)   (3F8.3)    (16A1)
C     -----------------------------------------------------------------
C     NODE         Node              'G'   'N'   Fx,Fy,Mz   Description
C     point        no.               Only        (forces)   in words
C     load
C     -----------------------------------------------------------------

C     Note that signs are easier to follow if it is remembered that
C     ACTNS are actions applied internally BY THE NODES to the members
C     whereas VECTR contains forces applied externally TO THE NODES,
C     either directly on the nodes or indirectly via the members.

      USE Params
      USE Restraints
      USE M

      INTEGER     INODE,ILCAS

      REAL*8      FX,FY,MZ

C        Add node loads directly to load vector
C        and to the equilibrium check vectors
C        --------------------------------------
         IDOFG = IDOFN(INODE,1)
         IF(KFIXD(IDOFG).GE.0) THEN
            VECTR(IDOFG,ILCAS)=VECTR(IDOFG,ILCAS)+FX
            EQLIB(ILCAS,IDOFG)=EQLIB(ILCAS,IDOFG)+FX
         ELSE
            IREAC=-KFIXD(IDOFG)
            FORCR(ILCAS,IREAC)=FORCR(ILCAS,IREAC)+FX
         END IF
         EQLIR(1,ILCAS)=EQLIR(1,ILCAS)+FX

         IDOFG = IDOFN(INODE,2)
         IF(KFIXD(IDOFG).GE.0) THEN
            VECTR(IDOFG,ILCAS)=VECTR(IDOFG,ILCAS)+FY
            EQLIB(ILCAS,IDOFG)=EQLIB(ILCAS,IDOFG)+FY
         ELSE
            IREAC=-KFIXD(IDOFG)
            FORCR(ILCAS,IREAC)=FORCR(ILCAS,IREAC)+FY
         END IF
         EQLIR(2,ILCAS)=EQLIR(2,ILCAS)+FY

         IDOFG = IDOFN(INODE,3)
         IF(KFIXD(IDOFG).GE.0) THEN
            VECTR(IDOFG,ILCAS)=VECTR(IDOFG,ILCAS)+MZ
            EQLIB(ILCAS,IDOFG)=EQLIB(ILCAS,IDOFG)+MZ
         ELSE
            IREAC=-KFIXD(IDOFG)
            FORCR(ILCAS,IREAC)=FORCR(ILCAS,IREAC)+MZ
         END IF

      RETURN

      END

