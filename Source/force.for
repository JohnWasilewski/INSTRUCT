      SUBROUTINE FORCE
C     ----------------
C     jw / 06-06-87 draft
C     jw / 26-06-13 last amended
C
C..   Calc. displacement actions & add them to the fixity actions.
C     Also add up the reactions at prescribed displacement nodes,
C     overlaying these on the vector that now contains the displace-
C     ments in place of zeros or known prescribed displacements, at
C     the restrained DoF
C
C   1)
C     For each member..
C     - - - - - - - -
C     Recalculate member stiffness TERMS.  Note that these have all
C     been calculated at least once before (by a call from subroutine
C     STIFF during structure stiffnesses setup, and by a call from
C     subroutine LoadTypS if there are any none-zero displacement
C     values).  However, the stiffness TERMS were not preserved for
C     re-use, saving 168 bytes double precision storage per member,
C     at the expense of longer running time.
C
C   2)
C     For each loadcase..
C     - - - - - - - - -
C     (a) refer to array VECTR, where subroutine SOLVE has overwritten
C     the load vectors for all loadcases in the current range by the
C     corresponding global-structure-axes displacements;
C     (b) extract from VECTR the six nodal displacements, DISVEC, at
C     the end-nodes for the current member, for the current loadcase;
C     (c) calculate the [displacement actions] for the current member
C     as the product [DISVEC] * [stiffnesses];
C     (d) add the [displacement actions] to the fixity actions that
C     were stored, member-by-member, loadcase-by-loadcase, in array
C     ACTNS, at loadcase-input time;
C     (f) check for any restraints at either end of the current member
C     and add the reversed-sign [displacement reactions] to the fixity
C     reactions that were stored at loadcase-input time;
C     (g) transform the member-end actions from global structure
C     axes to the local member axes of the current member;
C
C   3)
C     Note that for restrained DoFs the values in VECTR should all be
C     zero or else equal to the prescribed displacements.
C

      USE Params
      USE Materials
      USE Properties
      USE Members
      USE Nodes
      USE Restraints
      USE M
C-/-------------------------------------------------------------debug-\-
C     USE WINSTUFF
C     USE LUnits
C     USE Files
C-\-------------------------------------------------------------debug-/-

      LOGICAL  GLOBAL,LOCAL,RSTRD
      INTEGER  ILCAS, NODE1,NODE2, IMEMB,
     +         IVEC1,IACT,
     +         DOFG,IDOFG0,IDOFG3,DDOFG

      REAL*8   STERM,TERMS(21),DISVEC(6),DISACT(6),DX,DY,DL

      DATA     GLOBAL, LOCAL /.TRUE., .FALSE. /

C..   Loop through all members
C-/-------------------------------------------------------------debug-\-
C     Write(LUS,'(''   DO 700 IMEMB=1,NMEMB'')')
C-\-------------------------------------------------------------debug-/-
      DO 700 IMEMB=1,NMEMB
C..      Get member properties and recalculate GLOBAL stiffness terms
         CALL MEMSTF(IMEMB,GLOBAL,NODE1,NODE2,TERMS,DX,DY,DL)

C        Pointers needed later
         NODIFF=NODE2-NODE1
         IDOFG0=IDOFN(NODE1,1)-1
         IDOFG3=IDOFN(NODE2,1)-1
         DDOFG =IDOFG3-IDOFG0

C..      Loop through all loadcases
         DO 700 ILCAS=1,NLCAS
            DISVEC = 0.0

C..         Extract GLOBAL displacements for the current loadcase at
C           the nodal DoF at each end of the current member
            IVEC1=0
C-/-------------------------------------------------------------debug-\-
C     Write(LUS,'(''      DO 20 DOFG=IDOFG0,IDOFG3,DDOFG'')')
C-\-------------------------------------------------------------debug-/-
            DO 20 DOFG=IDOFG0,IDOFG3,DDOFG
               DO 20 JDOF=1,3
                  IDOF=DOFG+JDOF ! iDoF[Node1(x,y,z) then Node2(x,y,z)]
                  IVEC1=IVEC1+1  ! 1,2,3,4,5,6
                  IF(.NOT. RSTRD(IDOF, KFIXD))
     +            DISVEC(IVEC1)=VECTR(IDOF,ILCAS)
20             CONTINUE

C..         Sum the GLOBAL displacement actions at IACT due to six JACTs
C-/-------------------------------------------------------------debug-\-
C     Write(LUS,'(''      DO 620 IACT=1,6'')')
C-\-------------------------------------------------------------debug-/-
            DO 620 IACT=1,6
               DISACT(IACT)=0.0
               DO 600 JACT=1,6
600               DISACT(IACT)=
     +            DISACT(IACT)+STERM(TERMS,IACT,JACT)*DISVEC(JACT)

C..            Add the GLOBAL displacement actions to the fixity actions
C              that were stored when the load vectors were set up
620            ACTNS(ILCAS,IMEMB,IACT) =
     +         ACTNS(ILCAS,IMEMB,IACT) + DISACT(IACT)

C..         Check for supports at either end.
C           Where found, :
C            - add the GLOBAL displacement actions to support reactions
C            - add the GLOBAL displcmt actns to the equilib-check vector
            IVEC1=1
            DO DOFG=IDOFG0,IDOFG3,DDOFG
               DO KDOF=1,3
                  IDOF=DOFG+KDOF
                  IF(RSTRD(IDOF,KFIXD)) THEN

C..                  Restraint(s)found among the three DoF at one end
C                    or the other of the member

                     DO JDOF=KDOF,3
                        IDOF=DOFG+JDOF
C..                     Make sure this DoF is one of the restraints
                        IF(.NOT.RSTRD(IDOF,KFIXD)) CYCLE

C..                     (A) Add GLOBAL displacement actions to the
C                       GLOBAL support reactions that were stored when
C                       load vectors were set up
                        IV1=IVEC1+JDOF-1
                        REACT(ILCAS,-KFIXD(IDOF)) =
     +                  REACT(ILCAS,-KFIXD(IDOF)) + DISACT(IV1)

C..                     (B) Add GLOBAL displacement reactions to the
C                       equilib-check vector
                        EQLIB(ILCAS,IDoF)=EQLIB(ILCAS,IDoF)+DISACT(IV1)
                     END DO !JDOF=KDOF,3

                     EXIT !from DO JDOF=1,3
                  END IF

               END DO !KDOF=1,3

               IVEC1=IVEC1+3

            END DO !DOFG=IDOFG0,IDOFG3,DDOFG

C..         Add final GLOAL member-end actions to the
C           equilib-check vector

            DO iDoF=1,3

               jDoFG = IDOFN(NODE1,iDoF)
               EQLIB(ILCAS,jDoFG) =
     +         EQLIB(ILCAS,jDoFG) - ACTNS(ILCAS,IMEMB,iDoF)

               jDoFG = IDOFN(NODE2,iDoF)
               EQLIB(ILCAS,jDoFG) =
     +         EQLIB(ILCAS,jDoFG) - ACTNS(ILCAS,IMEMB,iDoF+3)

            END DO

C..         Transform member-end actions to LOCAL member axes

            CALL ROTATE(1,DX,DY,DL,
     +                  ACTNS(ILCAS,IMEMB,1),
     +                  ACTNS(ILCAS,IMEMB,2),
     +                  ACTNS(ILCAS,IMEMB,3))
            CALL ROTATE(1,DX,DY,DL,
     +                  ACTNS(ILCAS,IMEMB,4),
     +                  ACTNS(ILCAS,IMEMB,5),
     +                  ACTNS(ILCAS,IMEMB,6))

700         CONTINUE

         RETURN
      END
