      SUBROUTINE ADDLOD(IMEMB,ILCAS,ENDS,FIXITY,LTYP)
C     -----------------------------------------------
C     jw / 23-03-87  draft
C     jw / 05-09-04  last corrected
C

C     Add fixity actions for a member load to the ACTNS array, then
C     add reversed-sign fixity actions to the load vector array, VECTR.

      USE Params
      USE Restraints
      USE M

      INTEGER     IMEMB, ENDS(2), IEND,  INODE, IDOF
      INTEGER     IVEC,    IFIXN

      CHARACTER*1 LTYP
      REAL*8  FIXITY(6)

C...  Add fixity actions to the ACTNS array.
      DO IFIXN=1,6
         ACTNS(ILCAS,IMEMB,IFIXN) =
     +   ACTNS(ILCAS,IMEMB,IFIXN) + FIXITY(IFIXN)

      END DO !IFIXN=1,6

C...  Add reversed fixity actions either to the VECTR array
C     or to the REACT a nd the equilib-check vector arrays.
      IFIXN=0
      DO 920 IEND=1,2
         INODE=ENDS(IEND)
         IVEC =IDOFN(INODE,1)-1
         IVECT=(ILCAS-1)*NDOFG+IVEC
         DO 920 IDOF=1,3
            IFIXN=IFIXN+1
            IVEC=IVEC+1
            IVECT=IVECT+1
            IF(KFIXD(IVEC).GE.0) THEN
               VECTR(IDOFN(INODE,IDOF),ILCAS)  =
     +          VECTR(IDOFN(INODE,IDOF),ILCAS) - FIXITY(IFIXN)
            ELSE
               IREAC=(ILCAS-1)*NPRES - KFIXD(IVEC)

               IF(LTYP.NE.'S')
     +         REACT(ILCAS,-KFIXD(IVEC)) =
     +         REACT(ILCAS,- KFIXD(IVEC)) + FIXITY(IFIXN)

               IF(LTYP.NE.'S')
     +         EQLIB(ILCAS,IDOFN(INODE,IDOF)) =
     +         EQLIB(ILCAS,IDOFN(INODE,IDOF)) + FIXITY(IFIXN)
            END IF

 920        CONTINUE

C...  Accumulate all Fx, Fy values for each loadcase in EQLIR
C     for equilibrium checks with total support reactions
      EQLIR(1,ILCAS)=EQLIR(1,ILCAS)-FIXITY(1)-FIXITY(4)
      EQLIR(2,ILCAS)=EQLIR(2,ILCAS)-FIXITY(2)-FIXITY(5)

      RETURN
      END SUBROUTINE