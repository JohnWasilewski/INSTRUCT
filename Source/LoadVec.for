      SUBROUTINE LoadVEC
C     ------------------
C     jw / 03-03-87  draft
C     jw / 07-09-12  last amended

C     For each loadcase:-
C        For LOADS and for PRESCRIBED DISPLACEMENTS..
C          Store fixity actions;
C          Sum fixity actions at each node to form load vectors.
C
C     Return the number of loadcases used.

      USE LUnits
      USE Files
      USE RTF
      USE Params
      USE Materials
      USE Properties
      USE Nodes
      USE Members
      USE Restraints
      USE Loading
      USE M

      INTEGER     ILOD

      INTEGER     NM,IMEMB, ENDS(2),
     +            NINC, I2DUMY,
     +            PNM,PNOD1,PNOD2,LNM,LNOD1,LNOD2,CNOD1,CNOD2

      LOGICAL     IREPET,NEGFLG, NOSORT,SAYLEN,INTPL8,
     +            Released, Rel(6), MoreLoads, LErr

      CHARACTER*1 AXES,LTYP, TDLshp, BLANK1

      CHARACTER   DES24*24,
     +            NMT*6,
     +            BUFFER*80, AINCR*4

      CHARACTER   I2CHAR*16,BLANK8*8


      REAL*4      AX,AY,IZ,
     +            EMOD,GMOD

      REAL*8      DIVIDE, EXTENT, SLOP1, SLOP2,
     +            POS, PROPN, PPRPN, PRPNL, PFX,PFY,PMZ,
     +            FX,FY,MZ, DELTA(3),
     +            Lx,Ly,L, FIXITY(6),
     +            PNMX,PNMY,LFX,LFY,LMZ,LNMX,LNMY,
     +            FXP,FYP,MZP

      EQUIVALENCE (FX,DELTA(1)), (FY,DELTA(2)), (MZ,DELTA(3))

      DATA        NOSORT /.FALSE. /
      DATA        SAYLEN /.TRUE.  /
      DATA        BLANK1 /' '/
      DATA        BLANK8 /'        '/

      GoodLOD = .TRUE.

C    Keep adding loadcases until the maximum allowed by available memory
C    is reached or until user inputs no more and loop exits prematurely,
C    leaving a loadcase count on the loop counter.

C     Initialise arrays for all loadcases
C     NLCAS=NLCAS-1

      BUFFER = ''

      PRESC=0.0
      FORCR=0.0
      REACT=0.0

      ACTNS=0.0

      VECTR=0.0
      EQLIB=0.0

      EQLIR=0.0

      DO ILCAS=1,NLCAS
C..      Next load case

C        Get the load input data, store fixity actions and
C        form load vectors for loadcase NLCAS
C        ----------
C        INITIALISE
C        ----------
            LNMY=0.0
            PNMY=0.0
            LNMX=0.0
            PNMX=0.0

C        Initialise 'previous' load input values
C        ---------------------------------------
         POS = 0.0
         NM  = 0
         PNM = 0
         NDECI  = 3
         I2DUMY = 0
         PROPN  = 0.0
         PPRPN  = 0.0
         IREPET = .FALSE.
         INTPL8 = .FALSE.
         LErr   = .FALSE.

         FX  = 0.0
         FY  = 0.0
         MZ  = 0.0

         FXP = 0.0
         FYP = 0.0
         MZP = 0.0

         PFX = 0.0
         PFY = 0.0
         PMZ = 0.0

         iLod=0
         MoreLoads=.TRUE.
         DO WHILE(MoreLoads)
C-----------------------------------------------------------------------
C           PROCESS THE NEXT LINE OF LOAD INPUT DATA
C-----------------------------------------------------------------------
20          FX  = 0.0
            FY  = 0.0
            MZ  = 0.0
            LFX = 0.0
            LFY = 0.0
            LMZ = 0.0

            iLod=iLod+1

            POS = LOADCASE(ILCAS,iLod)%POS

C           Check for the end of the loadcase
C           ---------------------------------
            IF(POS.EQ.0) THEN
C              No more loads in this loadcase
               MoreLoads=.FALSE.
               CYCLE
               ELSE
C              This is another load entry for this loadcase
C              Retrieve the rest of this load entry and use it

C              MN     = Member number or node noumber
C              PROPN  = what proportion along (if it's a member load)
C              IREPET = whether this is a single load entry or the
C                       intended last NM of a REPEaTing series of
C                       loads starting from the previous entry

               IREPET = (POS.LT.0.0)   ! This ends a load sequence
               POS    = ABS(POS)       ! Node or Memb.Propn
               NM     = INT(POS)       ! node-or-member number,
               PROPN  = POS-FLOAT(NM)  ! proportion along a member
               NDECI  = LEN_TRIM(I2CHAR(PROPN,idum))-2 !No of decimls
               NMI    = INT(PROPN * 10.0**NDECI) ! Node/memb incremnt

C              Check again for the end of the loadcase
C              ---------------------------------------
               IF(NM.EQ.0) RETURN
C
C              ------------------------------------------------------
C              NM is not zero so this is another load entry.
C              Just a single load or a repeating series?
C              ------------------------------------------------------

               IF(.NOT.IREPET) THEN

C                 -----------------
C                 SINGLE LOAD ENTRY
C                 -----------------

C                 Set params to make just one pass
C                 through the data-generation loop
C                 --------------------------------
                  INCR = 1
                  NINC = 1
                  LNM  = NM

C                 Retrieve the load values
C                 --------------------
                  AXES   = LOADCASE(ILCAS,iLod)%Axes
                  LTYP   = LOADCASE(ILCAS,iLod)%LTyp
                  TDLshp = LOADCASE(ILCAS,iLod)%TDLshp
                  Fx     = LOADCASE(ILCAS,iLod)%Fx
                  Fy     = LOADCASE(ILCAS,iLod)%Fy
                  Mz     = LOADCASE(ILCAS,iLod)%Mz

                  GOTO 25

               ELSE
C                 ---------------------------
C                 REPEATING SERIES LOAD ENTRY
C                 ---------------------------

C                 Read details of the last load in the string of
C                 loads then set data-generation params to make the
C                 reqd no. of passes through the generate-loads loop
C                 ----------------------------------------------------

C                 NM     = either a node or a member
C
C                 PNM    = previous node or member
C                 PNOD1  = previous node1
C                 PNOD2  = previous node2
C                 PFX    = previous FX
C                 PFY    = previous FY
C                 PMZ    = previous MZ
C                 PNMX   = X-coord of prev.node or prev. member midpoint
C                 PNMY   = Y-coord of prev.node or prev. member midpoint
C
C                 CNM    = current node or member
C                 CNMX   = X-coord of this node or this member midpoint
C                 CNMY   = X-coord of this node or this member midpoint
C
C                 LNMI   = last Node-or-Member-dot-increment
C                 NMI    = Node-or-Member increment
C                 NINC   = number of increments
C                 EXTENT = Vector length distance between prev. and last
C
C                 LNM    = last Node-or-Member
C                 LNOD1  = last node1
C                 LNOD2  = last node2
C                 LFX    = last FX
C                 LFY    = last FY
C                 LMZ    = previous(last) MZ
C                 LNMX   = X-coord of last node or mid-last-member
C                 LNMY   = Y-coord of last node or mid-last-member


C                 Read the required 'last' values into increment
C                 variables (skip LNMI by reading LFX twice)
C                 ----------------------------------------------

C                 To prevent a read error, add a trail of 3 commas &
C                 zeros to the end of BUFFER in case not all three
C                 values, LFX, LFY and LMZ, have been entered.
                  DO IMORE=1,3
                     LB=LEN_TRIM(BUFFER)
                     IF (LB.LT.79) BUFFER=BUFFER(1:LB)//',0'
                  END DO

                  READ(BUFFER,*) LFX, LFX,LFY,LMZ

                  LNM =NM
                  INCR=NMI
                  NEGFLG=.TRUE.

                  IF(INCR.EQ.0) INCR=1

                  IF(LTYP.EQ.'N'.OR.LTYP.EQ.'P'.OR.LTYP.EQ.'S') THEN
                     NMT = 'node  '
                  END IF

                  IF(LTYP.EQ.'U' .OR. LTYP.EQ.'D') THEN
                     NMT = 'member'
                  END IF

                  IF(LTYP.EQ.'T' .OR. LTYP.EQ.'t') THEN
                     NMT = 'member'
                  END IF

                  IF( (LFX.NE.0.0 .OR. LFY.NE.0.0 .OR. LMZ.NE.0.0) .AND.
     +                (LFX.NE.PFX .OR. LFY.NE.PFY .OR. LMZ.NE.PMZ))
     +            THEN
C                    The load varies with distance between prev.
C                    and last (i.e.final) loads so interpolation
C                    constants are needed.
C                    Find previous/last load centre coords
C                    -------------------------------------------
                     IF(NMT .EQ. 'node  ') THEN
C                       Previous node
                        PNOD1 = 0
                        PNOD2 = 0
                        PNMX  = NODE(1,PNM)
                        PNMY  = NODE(2,PNM)
C                       Last node
                        LNOD1 = 0
                        LNOD2 = 0
                        LNMX  = NODE(1,LNM)
                        LNMY  = NODE(2,LNM)
                     END IF

                     IF(NMT .EQ. 'member') THEN
C                       Previous member
                        PNOD1 = MEMB(PNM)%Node1
                        PNOD2 = MEMB(PNM)%Node2
                        PNMX = 0.5*(NODE(1,PNOD1)+NODE(1,PNOD2))
                        PNMY = 0.5*(NODE(2,PNOD1)+NODE(2,PNOD2))
C                       Last member
                        LNOD1 = MEMB(LNM)%Node1
                        LNOD2 = MEMB(LNM)%Node2
                        LNMX  =0.5*(NODE(1,LNOD1)+NODE(1,LNOD2))
                        LNMY  =0.5*(NODE(2,LNOD1)+NODE(2,LNOD2))
                     END IF

C                    Distance between previous and last loads
                     EXTENT= SQRT((LNMX-PNMX)**2 + (LNMY-PNMY)**2)

                  ELSE
C                    Load remains constant so interpolation
C                    constants are unnecessary
C                    --------------------------------------
                     LFX=PFX
                     LFY=PFY
                     LMZ=PMZ

                  END IF

C                 Increments
                  NINC = IABS((LNM-PNM)/INCR)

                  IF(NM.LT.PNM) INCR=(-INCR)
                  IF(PNM+NINC*INCR .NE. LNM) THEN
                     AINCR=I2CHAR(INCR,NCHAR)

                     CALL ERROR
     +               (40,' Equal increments of '//AINCR(1:NCHAR)//
     +               ' will miss the last '//NMT(1:6),*20)
                  END IF

                  NM  = PNM+INCR
                  NINC = NINC-1

C                 Data generation ready to roll
                  GO TO 25

               END IF


C              ------------------------------------------------------
C              NOW ENTER THE LOADING DATA-GENERATION LOOP
C              (for both single lines of data AND data generation)

C              First, calculate the slope of the vector from
C              PNM to LNM, ready for the Loading-data-generation loop
C              ------------------------------------------------------
25             CONTINUE
               SLOP2 = ATAN(DIVIDE((LNMY-PNMY),(LNMX-PNMX)))


C              -----------------------
               DO NM = NM,LNM,INCR
C              -----------------------

                  IF(IREPET) THEN
                     PROPN = PPRPN
                     IF(INTPL8) THEN

C                       Calc Fx,Fy,Mz from the projected distance
C                       of the load increment's mid-point along
C                       the load extent vector.

C                       Find CURRENT load centre coords

                        IF(scan(LTyp,'NnPpSs').GT.0)
     +                  THEN
                           CNMX  = NODE(1,NM)
                           CNMY  = NODE(2,NM)
                        END IF

                        IF(scan(LTyp,'UuDd').GT.0)
     +                  THEN
                           CNOD1 = MEMB(NM)%Node1
                           CNOD2 = MEMB(NM)%Node2
                           CNMX  = 0.5*(NODE(1,CNOD1)+NODE(1,CNOD2))
                           CNMY  = 0.5*(NODE(2,CNOD1)+NODE(2,CNOD2))
                        END IF

C                       Slope of vector from PNM to CNM
                        SLOP1 = ATAN(DIVIDE((CNMY-PNMY),(CNMX-PNMX)))

C                       Distance from PNM to NM
                        PRPNL = (SQRT(
     +                  (CNMY-PNMY)**2+(CNMX-PNMX)**2)*COS(SLOP1-SLOP2))
     +                  /EXTENT
                        FX=PFX+PRPNL*(LFX-PFX)
                        FY=PFY+PRPNL*(LFY-PFY)
                        MZ=PMZ+PRPNL*(LMZ-PMZ)
                     ELSE
                        FX=PFX
                        FY=PFY
                        MZ=PMZ
                     END IF
                  END IF

C                 --------------------------
C                 Now CALL THE RELEVANT LTYP
C                 --------------------------

C                 ---------
C                 NODE LOAD
C                 ---------
                  IF(scan(LTYP,'Nn').GT.0) THEN
                     CALL LoadTypN(ILCAS,NM,FX,FY,MZ)
                     CYCLE

C                 ------------------
C                 SUPPORT SETTLEMENT
C                 ------------------
                  ELSE IF(scan(LTYP,'Ss').GT.0) THEN
                     CALL LoadTypS(ILCAS,NM,AXES,DELTA,*20)
                     CYCLE

C                 -----------
C                 MEMBER load
C                 -----------
                  ELSE IF(SCAN(LTyp,'PpUuDdTt').GT.0) THEN
                     IMEMB=NM
                     POS=IMEMB+PROPN

                     IF(IMEMB.GT.NMEMB) THEN
                         CALL BailOut('Member number ['//
     +                   I2Char(IMEMB,iDum)//'] is too large$',
     +                                          'OPTIONS','YES')
                         GoodLOD = .FALSE.
                         RETURN
                      END IF !(IMEMB.GT.NMEMB)


C..                  Get member details for the loaded member;
                     CALL GETMEM (iMEMB,ENDS(1),ENDS(2),
     +               AX,AY,IZ,Lx,Ly,L,GAMMA,EMOD,GMOD,Rel)

C                    Get .TRUE./.FALSE. member-end releases, Rel(1..6)
                     Released = .FALSE. !(if fully-fixed both ends)
                     DO IDof=1,6
                        Rel(iDoF)=MEMB(IMEMB)%Released(iDoF)
                        IF(Rel(iDoF)) RELEASED=.TRUE.
                     END DO

C                    -----------------
C                    MEMBER POINT LOAD
C                    -----------------
                     IF(scan(LTYP,'Pp').GT.0) THEN
                        CALL LoadTypP(PROPN, FX,FY,MZ, AXES,
     +                  Lx,Ly,L, Rel, RELEASED,FIXITY)
                        GOTO 900

C                    --------------------
C                    MEMBER UDL OR DEADLD
C                    --------------------
                     ELSE IF(scan(LTYP,'UuDd').GT.0) THEN
                        CALL LoadTypU
     +                  ( ILCAS, IMEMB, PROPN, AXES,LTYP,Rel,
     +                     AX, Lx,Ly,L, FX,FY,MZ, GAMMA, RELEASED,
     +                     FIXITY, *20)
                        GOTO 900

C                    -----------------------------------------
C                    MEMBER TDL (triangiular distributed load)
C                    -----------------------------------------
                     ELSE IF(scan(LTYP,'Tt').GT.0) THEN
                        CALL LoadTypT
     +                  ( ILCAS, IMEMB, PROPN, AXES, TDLshp,Rel,
     +                     Lx,Ly,L, FX,FY,MZ, RELEASED,
     +                     FIXITY, *20)
                        GOTO 900

                     END IF  ! LTYP.EQ.'P'/'U'/'D'/T)

C                 ELSE
                     CALL Bailout
     +               ('Unrecognised load type ['//LTYP//']$',
     +                                       'OPTIONS','YES')
                     GoodLOD = .FALSE.
                     RETURN

                  END IF ! All LTYP

 900              CONTINUE

C                 ------------------------
C                 REVERT to GLOBAL axes
C                 ------------------------
                  CALL ROTATE(-1,Lx,Ly,L,FIXITY(1),FIXITY(2),FIXITY(3))
                  CALL ROTATE(-1,Lx,Ly,L,FIXITY(4),FIXITY(5),FIXITY(6))

C                 -----------------------------
C                 ADD THE MEMBER FIXITY ACTIONS
C                 TO THE VECTR AND ACTNS ARRAYS
C                 -----------------------------
                  CALL ADDLOD(IMEMB,ILCAS,ENDS,FIXITY,LTYP)
C                 ------------------------------------------
C                 MAKE READY FOR READING THE NEXT LOAD ENTRY
C                 IN THE CURRENT LOADCASE
C                 ------------------------------------------
               END DO
               WRITE(DES24,'(24X)')
               IREPET = .FALSE.
               INTPL8 = .FALSE.
               PNM = NM-INCR
               PPRPN = PROPN
               PFX = FX
               PFY = FY
               PMZ = MZ
C              GOTO 20
            END IF
         END DO !WHILE(MoreLoads)

      END DO

      RETURN
      END
