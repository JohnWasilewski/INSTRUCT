C
      LOGICAL FUNCTION BuiltIN(iMemb,iDoF)
C     ------------------------------------
C     jw / 27-05-09  draft
C     jw / 14-09-12  last revised

C     Checks member continuity at a node.
C     Does this for the IDOFth out of six DoF for Memb IMEMB.
C     If continuous then returns BuitIN = .TRUE.
C     If released then returns BuitIN = .FALSE.

      USE Members

      INTEGER*4   iMemb, iDoF

      BuiltIN = .NOT. Memb(iMemb)%Released(iDoF)
      RETURN
      END



C
      FUNCTION IDOFR(IDOFG)
C     ---------------------
C     jw / 08-11-85  draft
C     jw / 25-04-12  last revised

C     Returns reduced DoF corresponding to an unreduced DoF
C     Returns a zero if IDOFG is restrained by a prescribed displcemnt.
C     For how fixity codes work in Array KFIXD() see subroutine RCODE().

      USE Restraints
      INTEGER IREDU, IDOFR, IDOFG

      IREDU = KFIXD(IDOFG)

C..   Restrained
      IF(IREDU.LT.0) THEN
          IDOFR = 0

C..   Unrestrained
      ELSE
          IDOFR = IDOFG-IREDU
      END IF

      RETURN
      END



C
      FUNCTION IDOFG(IDOFR)
C     ---------------------
C     jw /  6-10-87 draft
C     jw / 30-05-01 last rev.

C     Returns an unreduced DoF corresponding to a reduced DoF.
C     For how fixity codes work in Array KFIXD() see subroutine RCODE().

      USE Restraints
      INTEGER IREDU0,IREDU1, IDOFG,IDOFR

      IREDU0 = 0
      IDOFG  = IDOFR

10    IREDU1 = KFIXD(IDOFG)

      IF(IREDU1.EQ.IREDU0) RETURN

      IREDU0 = ABS(IREDU1)
      IDOFG  = IDOFR+IREDU0

      GOTO 10
      END



C
C     SUBROUTINE UNREDU(IDOFR, INOD,iDoF)
C     -----------------------------------
C     jw / 18-09-12 draft
C     jw / 18-09-12 last rev.

C     Returns the node no. and DoF corresponding to a reduced DoF.
C     For how fixities are coded in Array KFIXD() see subroutine RCODE()

C     USE Restraints
C     INTEGER IREDU0,IREDU1, IDOFG,IDOFR

C     IREDU0 = 0
C     IDOFG  = IDOFR

C10   INOD = 1+IDOFG/3
C     iDoF = IDOFG - 3*(INOD-1)

C     IREDU1 = KFIXD(INOD,iDoF)

C     IF(IREDU1.EQ.IREDU0) RETURN

C     IREDU0 = ABS(IREDU1)
C     IDOFG  = IDOFR+IREDU0

C     GOTO 10
C     END



C
C     FUNCTION iNODfor(IDOFG)
C     -----------------------
C     jw / 19-09-12 draft
C     jw / 19-09-12 last rev.

C     Returns the node number corresponding to an unreduced DoF.

C     INODfor = 1+IDOFG/3

C     RETURN
C     END



C
C     FUNCTION iDoFfor(IDOFG)
C     -----------------------
C     jw / 19-09-12 draft
C     jw / 19-09-12 last rev.

C     Returns the DoF corresponding to an unreduced DoF.

C     iDoFfor = IDOFG - 3*(iNODfor(IDOFG)-1)

C     RETURN
C     END





C
      LOGICAL FUNCTION FREED(IDOFG)
C     -----------------------------
C     jw / 28-09-04  draft
C     jw / 18-11-08  last corrected

C     Returns .TRUE. if IDOFG is an unrestrained DoF.
C     Returns .FALSE. if IDOFG is restrained.
C     For fixity codes in Array KFIXD() see subroutine RCODE().

      USE Restraints

C     LOGICAL    FREE
      LOGICAL    RSTRD
      INTEGER    IDOFG

C     FREE = .FALSE.
C     IF(KFIXD(IDOFG).GE.0) FREE = .TRUE.

      FREED = .NOT. RSTRD(IDOFG)

      RETURN
      END




C
      LOGICAL FUNCTION RSTRD(IDOFG)
C     -----------------------------
C     jw / 08-11-85  draft
C     jw / 25-04-12  last corrected

C     Returns .TRUE. if IDOFG is a rigidly restrained DoF.
C     Returns .FALSE. if IDOFG is unrestrained.
C     For fixity codes in Array KFIXD() see subroutine RCODE().

      USE Restraints

C     LOGICAL    RSTRD
      INTEGER    IDOFG

      RSTRD = .FALSE.

      RSTRD = (KFIXD(IDOFG).LT.0)

      RETURN
      END



C
      LOGICAL FUNCTION FLEXR(IDOFG,SPRST)
C     -----------------------------------
C     jw / 08-08-87
C     jw / 27-09-12  last rev.

C     Returns .TRUE. if IDOFG has a restraining spring attached,
C     extracting corresponding spring stiffness from array SPRNG
C     and placing it in SPRST.
C
C     Since springs were stored in random data-entry order, the only
C     way to check IDOFG for a spring is to compare IDOFG with all
C     LOSPR.  This should be pretty fast, however, in compiled machine
C     code that is doing only comparisons of integers, and no arithme-
C     tic except incrementing.

      USE Params
      USE Restraints

      INTEGER   IDOFG
      REAL      SPRST
C     LOGICAL   FLEXR

      FLEXR=.FALSE.
      SPRST=0.0

      DO ISPR=1,NSPRI
         IF(IDOFN(SPRING(ISPR)%Node,SPRING(ISPR)%DoF).EQ.IDOFG)
     +   GOTO 200
      END DO

C     None found
      RETURN

C     Found it
200   FLEXR=.TRUE.
      SPRST=SPRING(ISPR)%Stiff
      RETURN

      END


C
      FUNCTION IDOFN(INODE,IDOF)
C     --------------------------
C     jw / 30-12-85  draft
C     jw / 30-05-01  last rev.

C     Returns the global (unreduced) DoF no. corresponding
C     to a nodal dof (1..3)

      USE Restraints
      INTEGER  INODE,IDOF, IDOFN

      IDOFN=3*(INODE-1)+IDOF

      RETURN

      END


C
      FUNCTION KPOS(INOD,IDOF, JNOD,JDOF)
C     -----------------------------------
C     jw / 01-06-86  draft
C     jw / 30-05-01  last rev.

C     Returns 1-D offset position of a stiffness term in the
C     skyline-stored global reduced RSTIF stiffness array, giving
C     a zero if either of the contributing actions is restrained
C     by a prescribed displacement.

      USE Restraints
      USE M

      INTEGER   KPOS, INOD,IDOF, JNOD,JDOF
      INTEGER   IRFN,JRFN, IDOFR, IDOFN

      KPOS = 0
      IRFN=IDOFR(IDOFN(INOD,IDOF))

      IF(IRFN.EQ.0) RETURN

      JRFN=IDOFR(IDOFN(JNOD,JDOF))

      IF(JRFN.EQ.0) RETURN

      KPOS = JDIAG(JRFN)+IRFN-JRFN

      RETURN
      END


C
      FUNCTION KXY(JX,JY,JDIAG)
C     -------------------------
C     jw / 1-1-87
C     jw / 30-05-01  last rev.

C     Returns the 1-D offset in a skyline-stored RSTIF array of the
C     upper-triangular (JX,JY)'th stiffness term.
C

C     RSTIFF
C     example
C     -------
C                               j
C                               |
C              ( 1  2  .  .  .  .  . )        JDIAG = (  1 )
C              ( =  3  4  6  .  .  . )                (  3 )
C          i-- ( .  =  5  7  . 11  . ) -+-            (  5 )
C              ( .  =  =  8  9 12  . )  | j-i         (  8 )
C              ( .  .  .  = 10 13 15 )  |             ( 10 )
C          i-- ( .  .  =  =  = 14 16 ) -+-            ( 14 )
C              ( .  .  .  .  =  = 17 )                ( 17 )
C                               |
C                               j

      INTEGER   KXY, JX,JY, JDIAG(1)

      i=MIN(JX,JY)
      j=MAX(JX,JY)

      KXY = JDIAG(j)-(j-i)
      IF(j.EQ.1) RETURN
      IF(KXY.LE.JDIAG(j-1)) KXY=0
      RETURN
      END


C
      FUNCTION IN2D(IROW,ICOL,NCOL)
C      -----------------------------
C     jw / 1-1-87
C     jw / 30-05-01  last rev.

C     Returns the offset in a 1-D array of an element in the
C     2-D matrix which is stored in the array.

      INTEGER  IN2D, IROW, ICOL, NCOL

      IN2D = (IROW-1)*NCOL+ICOL

      RETURN
      END


C
      FUNCTION IN3D(IBLOCK,IROW,ICOL, NROW,NCOL)
C     ------------------------------------------
C     jw /  7-07-87  draft
C     jw / 22-06-04  last rev.

C     Returns the offset in a 1-D array of an element in the
C     3-D matrix which is stored in the array.

      INTEGER  IN3D, IBLOCK,IROW,ICOL, NROW,NCOL

      IN3D = (IBLOCK-1)*NROW*NCOL + (IROW-1)*NCOL+ICOL

      RETURN
      END




      REAL FUNCTION WHETHR(TEST,YES,ErmNO)
C     ------------------------------------
C     jw / 06-11-04
C     jw / 06-11-04  last rev.
      LOGICAL  TEST
      REAL     YES,ERMNO
      IF(TEST) THEN
        WHETHR=YES
      ELSE
        WHETHR=ErmNO
      END IF
      RETURN
      END



      INTEGER FUNCTION IFTHAT(TEST,IS,ISNT)
C     -------------------------------------
C     jw / 06-11-04
C     jw / 06-11-04  last rev.
      LOGICAL  TEST
      INTEGER  IS,ISNT
      IF(TEST) THEN
        IFTHAT=IS
      ELSE
        IFTHAT=ISNT
      END IF
      RETURN
      END



      SUBROUTINE PUTNUM(INTNUM,LAYOUT,NUMPOS)
C     ---------------------------------------
C     jw / 08-11-04
C     jw / 08-11-04  last rev.

C     Enter a number in the diagram layout

      CHARACTER ALFNUM*10, LAYOUT*(*), FINDCHR*1, I2CHAR*16
       INTEGER  INTNUM, NUMPOS, NCHAR

       ALFNUM = I2CHAR(INTNUM,NCHAR)
       NUMPOS = NUMPOS-NCHAR/2

       IF(FINDCHR(LAYOUT(NUMPOS:NUMPOS+NCHAR-1),INSET,'0123456789',0)
     +    .EQ.  ' ')
     +    LAYOUT(NUMPOS:NUMPOS+NCHAR-1) = ALFNUM(1:NCHAR)

       RETURN
       END


C-----------------------------------------------------------------------