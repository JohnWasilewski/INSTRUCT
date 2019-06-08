      SUBROUTINE SPREAC
C     -----------------
C     jw / 09-09-04 draft
C     jw / 09-09-04 last rev.
C
C     Extract and store spring reactions SREAC((1..NLCAS),(1..NSPRI))
C     and add spring reactions to the nodal equilibrium checks vector

      USE PARAMS
      USE LUnits
      USE RESTRAINTS
      USE M


      INTEGER  ILCAS,INODE

      LOGICAL  FLEXR

C     CALL DISP(1,' * SPREAC',9,1)

      DO 4300 ILCAS=1,NLCAS
         ISPRI=0
         DO 4200 INODE=1,NNODE
C           See if there are any sprung restraints at this node
            DO 4050 IDOF=1,3
4050           IF(FLEXR(IDOFN(INODE,IDOF), SPRINK)) GOTO 4090

C           Loop completed means this node has no sprung DoF
            GOTO 4200

C           This node has one or more sprung DoF at INODE
4090        IOUT=IN3D(ILCAS,INODE,1,NNODE,3) -1 !(iLcas,iNode,1)
            DO 4150 IDOF=1,3
               IDOFG=IDOFN(INODE,IDOF)
               IF(FLEXR(IDOFG,SPRINK)) THEN
C              This IDOFG has a spring restraint
                  ISPRI=ISPRI+1
                  IR=IOUT+IDOF !(iLcas,iNode,iDoF)

C                 Store the spring reaction
                  IVEC1S=IN2D(ILCAS,ISPRI,NSPRI)
C                 SREAC(IVEC1S) = -VECTR(IR)*SPRINK
                  SREAC(ILCAS,ISPRI) =
     +            -VECTR(IDOFN(iNode,iDoF),iLcas)*SPRINK

C                 Add spring reaction to the equilib-check vector
                  IVEC1T=IN2D(ILCAS,IDOFG,NDOFG)
                  EQLIB(ILCAS,IDOFG) = EQLIB(ILCAS,IDOFG)
     +                               + SREAC(ILCAS,ISPRI)

                  END IF
4150           CONTINUE

C           Jump out of INODE loop if sprung restraints all done
4200        IF(ISPRI.GT.NSPRI) GOTO 4300

C        Next load case
4300     CONTINUE

      RETURN
      END
