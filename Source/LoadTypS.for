      SUBROUTINE LoadTypS (ILCAS,INODE,AXES,DELTA,*)
C     ----------------------------------------------
C     jw / 15-11-11  Extracted from Subroutine LOADS
C     jw / 15-11-11  last rev.

C     -----------------------
C     Prescribed displacement
C     (SUPPORT SETTLEMENT)
C     -----------------------

C     FOR One NODE LOAD entry:
C      - Print out the load data input.
C      - Calculate and store the fixity actions.
C      - Don't add the reversed fixity actions to the load vector, VECTR
C        (this is done by the calling subroutine).
C      - Do not preserve load input data on exit from this subroutine.
C      - Do preserve prescribed displacement values because they will
C        be needed at results-printout time.
C
C     Input definitions for POS,AXES,LTYP,FX,FY,MZ,DESC:
C     -----------------------------------------------------------------
C     Load         POS              AXES  LTYP   FX,FY,MZ   DESC
C     Condition   (F7.3)/(I7)       (A1)  (A1)   (3F8.3)    (16A1)
C     -----------------------------------------------------------------
C     SETTLEment   Node              'G'   'S'   dX,dY,@Z   Description
C     of a                           only                   in words
C     support
C    (prescribed
C     displacemt)
C     -----------------------------------------------------------------

C     Note that signs are easier to follow if it is remembered that
C     ACTNS are actions applied internally BY THE NODES to the members
C     whereas VECTR contains forces applied externally TO THE NODES,
C     either directly on the nodes or indirectly via the members.


      USE Params
      USE Members
      USE Properties
      USE Nodes
      USE Materials
      USE Restraints
      USE M

      LOGICAL     GLOBAL, LOCAL, FREED, Released, Rel(6)

      CHARACTER*1 AXES

      CHARACTER   I2CHAR*16, XYZ*3

      INTEGER     INODE,ILCAS,I, NODE1,NODE2,
     +            ENDS(2), OFFSET

      REAL*8      DELTA(3),
     +            DISVEC(6),FIXITY(6), STERM, TERMS(21),
     +            dX,dY,rZ, Lx,Ly,L

      DATA        GLOBAL  /.TRUE.  /
      DATA        LOCAL   /.FALSE. /
      DATA        XYZ     /'xyz' /


      dX=DELTA(1)
      dY=DELTA(2)
      rZ=DELTA(3)


C     ** Workaround code to fix unexplained problem **
C     ** Without this apparently do-nothing code,   **
C     ** STERM appears to return incorrect TERMS    **
         do iact=1,6
            do jact=1,6
                 s=STERM(TERMS,IACT,JACT)
            end do
         end do


C     ---------------
C     Validity checks
C     ---------------
         IF(INODE.GT.NNODE) THEN
            CALL BailOut('The support settlement on node ['//
     +      I2Char(INODE,idum)//'] in loadcase '//I2Char(ILCAS,iDum)//
     +      ' has too large a node number$','OPTIONS','YES')
            GoodNOD = .FALSE.
            RETURN
         END IF !(INODE.GT.NNODE)
            
         IF(AXES.NE.'G') THEN
            CALL BailOut('The support settlement on node ['//
     +      I2Char(INODE,idum)//'] in loadcase '//I2Char(ILCAS,iDum)//
     +      ' has the wrong axes. They should be ''G''.$',
     +                                                 'OPTIONS','YES')
            GoodNOD = .FALSE.
            RETURN
         END IF !(AXES.NE.'G')

      DO 5 I=1,3
         IF(FREED(IDOFN(INODE,I),KFIXD) .AND. DELTA(I).NE.0.0) THEN
            CALL BailOUT('Support settlement cannot be specified '//
     +        'at node '//TRIM(I2Char(INODE,iDum))//', direction '//
     +         XYZ(I:I)//', in loadcase '//TRIM(I2Char(ILCAS,iDum))//
     +        ', because no rigid support has been specified there.$',
     +        'OPTIONS','YES')
            GoodNOD = .FALSE.
            RETURN
         END IF !(FREED(IDOFN(INODE,I),KFIXD) .AND. DELTA(I).NE.0.0)
  5   CONTINUE

C     -----------------------------
C     Store prescribed displacement
C     -----------------------------
      DO iDoF=1,3
          NthRes = -KFIXD(IDOFN(INODE,IDOF))
          IF(NthRes.GT.0) PRESC(ILCAS,NthRES) = Delta(iDoF)
      END DO

C     -----------------------------------------------------------
C     Compute prescribed displacement fixity actions
C     add the member fixity actions to the VECTR and ACTNS arrays
C     -----------------------------------------------------------

      DISVEC = 0.0

      DO iDoF=1,3
         IVECT = IDOFN(INODE,iDoF)
         VECTR(IVECT,ILCAS)= VECTR(IVECT,ILCAS)+ DELTA(iDoF)
      END DO

C     -----------------------------------------------
C..   Search through all members to find members that
C     frame into INODE
C     -----------------------------------------------
      DO 500 IMEMB=1,NMEMB
         IF(MEMB(IMEMB)%NODE1.NE.INODE)  GOTO 100
         IEND=1
         GOTO 101
100      IF(MEMB(IMEMB)%NODE2.NE.INODE)  GOTO 500
         IEND=2
C        GOTO 101

C        -----------------------------------------------------
C        Found one.
C        Now load the displacement vector with the prescribed
C        displacements for the correct end of the member
C        -----------------------------------------------------

C        ------------------------------------------
C        Get member props and LOCAL stiffness terms
C        ------------------------------------------
101      CALL MEMSTF(IMEMB,LOCAL,NODE1,NODE2,TERMS,Lx,Ly,L)

C        ------------------------------------------------------
C        Get .TRUE./.FALSE. member-end release codes, Rel(1..6)
C        ------------------------------------------------------
         Rel = MEMB(IMEMB)%Released

C        ----------------------------------------------------
C        Load the displacement vector with the prescribed
C        displacements for the correct end of the member
C        ----------------------------------------------------
C
         OFFSET=IEND*3-3

         DO 110 IVEC=1,3
            IVOFF=IVEC+OFFSET
110         DISVEC(IVOFF)=DELTA(IVEC)

C        ------------------------------------------------------------
C        Transform prescribed nodal displacement to LOCAL member axes
C        ------------------------------------------------------------

         CALL ROTATE
     +   (+1,Lx,Ly,L,DISVEC(1+OFFSET),DISVEC(2+OFFSET),DISVEC(3+OFFSET))

C        -------------------------------------
C..      Calculate member LOCAL fixity actions
C        -------------------------------------

         FIXITY = 0.0

         DO 200 IVEC=1,6
            DO 200 JVEC=1,6
200            FIXITY(IVEC) =
     +         FIXITY(IVEC) + STERM(TERMS,IVEC,JVEC)*DISVEC(JVEC)

C        ---------------------------------
C        ALLOW FOR ANY MEMBER-END RELEASES
C        ---------------------------------
         IF (Released) CALL MemRel(FIXITY,FIXITY,Rel,L)

C        ----------------------------
C        Revert to GLOBAL member axes
C        ----------------------------

         CALL ROTATE(-1,Lx,Ly,L,FIXITY(1),FIXITY(2),FIXITY(3))
         CALL ROTATE(-1,Lx,Ly,L,FIXITY(4),FIXITY(5),FIXITY(6))

C        ----------------------------------------------------------
C..      Add member GLOBAL fixity actions to VECTR and ACTNS arrays
C        ----------------------------------------------------------
         ENDS(1)=NODE1
         ENDS(2)=NODE2


         CALL ADDLOD(IMEMB,ILCAS,ENDS,FIXITY,ACTNS,
     +               REACT,VECTR,KFIXD,EQLIB,EQLIR,'S')

C        ------------------------------------
C        Any more members framing into INODE?
C        ------------------------------------
500      CONTINUE

      RETURN

      END 