      SUBROUTINE rRESTR
C     -----------------
C     jw / 20-12-85  draft
C     jw / 12-04-12  last corrected

C     Define the restraints (a restraint is a DoF having a prescribed
C     value for its displacement - usually, but not necessarily, zero).
C
C     Subroutine RCODE must be called later to set up special codes
C     that help the program keep track of unrestrained DoF.
C     Details of how this works are given in RCODE.
C
C     Note that the actual values of prescribed displacements PRESC
C     are not input here but are instead assigned default values of
C     zero.  Any non-zero values can be input later, during loadcase
C     input, which allows the user to vary prescribed displacements
C     at the same supports according to load conditions.

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints

      CALL HidePANES
      CALL GhostMenus

      SOLVED = .FALSE.
      CALL WinRES('SHOW')

      RETURN
      END




      SUBROUTINE WinRES(ACTN)
C     -----------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

C     Window layout for Materials input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE Restraints

      EXTERNAL  StorRES,
     +          CloseRES,
     +          Listout,
     +          Help_Supports

      REAL      ResColWidths(2), SprColWidths(3)
      INTEGER   IRes
      CHARACTER ACTN*4, String*3, ADoF*3
      LOGICAL   TooMany

      DATA      ResColWidths
     +         /-20.,     -80./
C               Node    RestrDir

      DATA      SprColWidths
     +         /-20.,     -35.,     -45./
C               Node    RestrDir   SprStif

      PARAMETER (ADoF = ('XYZ'))

      SELECT CASE(ACTN)

C-----------------------------------------------------------------------
         CASE('SHOW')
C-----------------------------------------------------------------------

         CALL WGAPP(MenuHG,'Supports',MenuHGN)
         CALL SWGCBK(MenuHGN, Help_Supports)

         ShowsRES = .TRUE.

C           Make REStraints data entry fields
C-----------------------------------------------------------------------
C           Restraints pane
               CALL SWGPOS(Pan2L,Pan2T)
               CALL WGBAS(Win1,'HORI',idResPAN)
C-----------------------------------------------------------------------
C           Divide restraints pane into three vertical stacks
               CALL SWGWTH(-35)
               CALL WGBAS(idResPAN,'VERT',idResPAN1)

               CALL SWGWTH(-47)
               CALL WGBAS(idResPAN,'VERT',idResPAN2)

               CALL SWGWTH(-8)
               CALL WGBAS(idResPAN,'VERT',idResPAN3)
C-----------------------------------------------------------------------
C           Node restraints input table
               CALL SWGRAY(ResColWidths,3,'TABLE')
               CALL SWGOPT('COLUMNS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idResPAN1,MAX(1,NPRES),2,idResTbl)
               CALL SWGTBS(idResTbl,'Node no.',0,1,'VALUE')
             CALL SWGTBS
     +            (idResTbl,
     +            'All rigid nodal restraint direction(s) on the node',
     +             0,2,'VALUE')

C           Copy stored values into the on-screen table
            iRow=0
            iRes=0
            TooMany=.FALSE.
            DO iNode=1,NNODE
               String = ''
               DO iDoF=1,3
                  IF(KFIXD(IDOFN(ABS(INODE),iDoF)).EQ.1) THEN
                     iRes=iRes+1
                     IF(iRes.LE.NPRES) THEN
                        String=Trim(String)//ADOF(iDoF:iDoF)
                     ELSE
                        CALL SCRmsg
     +                  ('**More restraints than specified.',.FALSE.)
                        TooMany=.TRUE.
                        EXIT
                     END IF
                  END IF

               END DO !iDoF
               IF(TooMany) EXIT
               IF(LEN_TRIM(String).GT.0) THEN
                  iRow=iRow+1
                  CALL SWGTBI(idResTbl,iNode,iRow,1,'VALUE')
                  CALL SWGTBS(idResTbl,Trim(String),iRow,2,'VALUE')
               END IF
            END DO !INode
C-----------------------------------------------------------------------
C           Node springs input table
               CALL SWGRAY(SprColWidths,3,'TABLE')
               CALL SWGOPT('COLUMNS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idResPAN2,MAX(1,NSPRI),3,idSprTbl)
               CALL SWGTBS(idSprTbl,'Node no.',0,1,'VALUE')
               CALL SWGTBS(idSprTbl,
     +             'One nodal spring direction ',0,2,'VALUE')
               CALL SWGTBS(idSprTbl,'Stiffness',0,3,'VALUE')

C           Copy stored values into the on-screen table
            DO ISpr=1,NSPRI
               CALL SWGTBI(idSprTbl,SPRING(ISpr)%Node,ISpr,1,'VALUE')
               IF(SPRING(ISpr)%DoF.EQ.1) THEN
                  String(1:1)='x'
               ELSE IF(SPRING(ISpr)%DoF.EQ.2) THEN
                  String(1:1)='y'
               ELSE IF(SPRING(ISpr)%DoF.EQ.3) THEN
                  String(1:1)='z'
               ELSE
                  String(1:1)=' '
               END IF
               CALL SWGTBS(idSprTbl,String(1:1),ISpr,2,'VALUE')
              CALL SWGTBF(idSprTbl,SPRING(ISpr)%Stiff,-2,ISpr,3,'VALUE')
            END DO
C-----------------------------------------------------------------------
C           Action buttons in stack col 3
            CALL WGPBUT(idResPAN3,'Accept',idOK)
            CALL SWGCBK(idOK, StorRES)

            CALL WGPBUT(idResPAN3,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseRES)

            CALL WGPBUT(idResPAN3,'List input',idList)
            CALL SWGCBK(idList, Listout)

            CALL WGLAB(idResPAN3,'',idLab)
            CALL WGLAB(idResPAN3,
     +      'See HELP for',idLab)
            CALL WGLAB(idResPAN3,
     +      'restr patterns',idLab)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsRES = .FALSE.
C           CALL SWGATT(id0,      'INACTIVE','STATUS')
C           CALL SWGATT(id1,      'INACTIVE','STATUS')
            CALL SWGATT(idResPAN, 'INACTIVE','STATUS')
            CALL SWGATT(idResPAN1,'INACTIVE','STATUS')
            CALL SWGATT(idResPAN2,'INACTIVE','STATUS')
            CALL SWGATT(idResPAN3,'INACTIVE','STATUS')
            CALL SWGATT(idResTbl, 'INACTIVE','STATUS')
            CALL SWGATT(idSprTbl, 'INACTIVE','STATUS')
            CALL SWGATT(idOK,     'INACTIVE','STATUS')
            CALL SWGATT(idCANCEL, 'INACTIVE','STATUS')
            CALL SWGATT(idLIST,   'INACTIVE','STATUS')

      END SELECT !(ACTN)

      RETURN
      END




      SUBROUTINE StorRES
C     ------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE Restraints

      CHARACTER I2CHAR*8, Res*5, DoFdat*1
      CHARACTER*2 ADoF(3)

      PARAMETER (ADoF = (/'Xx','Yy','Zz'/))


C     RIGID RESTRAINTS TABLE

C     Initialise
C     ----------
C     Set GoodRES to .TRUE. pending any checks to the contrary
      GoodRes=.TRUE.
      iPRES = 0
      KFIXD = 0

C    'Previous' node input values
      InodP = 0
      nINC  = 1

C     Now cycle through all rows in the on-screen
C     rigid restraints data-entry table
C    --------------------------------------------
      DO Itab = 1,NPRES

C        Read line Itab from the on-screen data-entry table
         CALL GWGTBF(idResTbl,Itab,1,rNod)
         CALL GWGTBS(idResTbl,Itab,2,Res)

         IF(rNod.GE.0.0) THEN

C           Single restrained-node entry
C           ----------------------------
            INC  = 1
            iNod = INT(rNod)
            LNod = iNod
            rInc = rNod-REAL(iNod)
            IF(rInc.NE.0.0) THEN
               CALL BailOUT
     +         ('ERROR|Use restrained-node increment decimal only '//
     +           'with hyphenated end-of-range node number.|'//
     +           'Restraints ignoded on Node '//
     +            TRIM(I2CHAR(iNOD,idum))//'.$', 'OPTIONS','YES')
               GoodRES = .FALSE.
            END IF

         ELSE

C           Data generation
C           ---------------
            rNod = -rNod
            LNod = INT(rNod)              !Last node in the sequence
            rInc = rNod-REAL(LNod)
            DO WHILE (rINC.NE.0.0 .AND. ABS(rInc).LT.1.0)
               rInc=10.0*rInc
            END DO
            INC = NINT(rInc)              !Node number increment
            IF(INC.EQ.0) INC=1
            nINC = IABS((LNod-iNodP)/INC) !Number of increments in all
            IF(LNod.LT.iNodP) INC=(-INC)
            iNod = iNodP+INC              !First node in the sequence

C           Check that the increments will hit the last-node target

            IF(iNodP+nINC*INC .NE. LNod) THEN
               CALL BailOUT
     +         ('ERROR|Data generation error after restrained node '//
     +         TRIM(I2CHAR(iNodP,idum))//'.|'//
     +         'Equal restrained-node increments of '
     +          //TRIM(I2CHAR(INC,idum))//
     +         ' will miss the last node.$', 'OPTIONS', 'YES')
                GoodRES = .FALSE.
            END IF
         END IF ! Single restrained-node or data generation

         IF((iNod.GT.NNODE)
     +       .OR.
     +      (iNod.EQ.0 .AND. .NOT.(LEN_TRIM(Res).EQ.0))) THEN
            CALL BailOUT
     +      ('ERROR|Restrained-node number must be between 1 and '//
     +      TRIM(I2CHAR(NNODE,idum))//'.|'//
     +      'Rigid supports discarded for '//
     +      'invalid node '//TRIM(I2CHAR(iNod,Idum))//'.$',
     +      'OPTIONS', 'YES')
            GoodRES = .FALSE.
            END IF

C        Now loop through all data-gen nodes and store the data
C        (which will be just one pass if datagen is not invoked)

         DO Inod=Inod,Lnod,INC
C..         CHECK AND ACCEPT THE LINE OF RIGID RESTRAINTS DATA
            DO iDoF=1,3
               IF(SCAN(Res,ADoF(iDoF)).GT.0) THEN

C                 Found a valid restraint direction, so check whether
C                 it is a new restraint or has been restrained
C                 already by a previous data-entry line
                  IF (KFIXD(IDOFN(ABS(iNod),iDoF)) .EQ. 0) THEN

C                    It is a new restraint, so increment the
C                    restraints total and check that the specified
C                    total has not been exceeded
                     iPRES = iPRES+1
                     IF(iPRES.LE.NPRES) THEN

C                       Permitted total has not been not exceeded.
C                       The line of data read from the file is
C                       therefore verified as a valid new restraint
C                       so accept it by storing a 1 in array vector,
C                       KFIXD() at position IDOFN((iNod),IDOF).
C                       This is to tell subroutine RCODE that this is
C                       a rigid restraint.

                        KFIXD(IDOFN(ABS(iNod),iDoF)) = 1

                     ELSE
C                       Permitted restraints total has been exceeded.
                        CALL BailOUT
     +                  ('ERROR|Too many restraints.|'//
     +                   'Restraint '//TRIM(I2CHAR(iNod,idum))//
     +                   ADoF(iDoF)(1:1)//' ignored.$',
     +                   'OPTIONS',
     +                   'YES')

                     END IF !Restraints total check to specfied limit

                  ELSE

C                    This is not a new restraint, it is a repeat of a
C                    restraint already entered, so issue a warning
C                    message. Do not increment the restraints total
C                    and there is no need to to store a 1 in array
C                    vector, KFIXD() at position IDOFN((iNod),IDOF)
C                    because there is already a 1 stored there.

                     CALL BailOUT
     +               ('WARNING|'//
     +                'Restraint '//TRIM(I2CHAR(iNod,idum))//
     +                 ADoF(iDoF)(1:1)//' entered more than once.$',
     +                'OPTIONS',
     +                'YES')
                  END IF !Whether this is a new restraint

               ELSE

C                 Restraint direction iDoF does not appear in Res.

C                 Check to see whether a restraint has been entered
C                 previously in direction iDoF.

                  IF(KFIXD(IDOFN(ABS(iNod),iDoF)) .EQ. 1) THEN

C                    If it has, then interpret the user's wishes
C                    in this data entry line according to the
C                    following rule:
C                     * If at least one of the other restraint
C                       direction HAS been specified then assume
C                       that the current data entry line is simply
C                       adding that/those other restraint(s), in
C                       which case, just cycle to the next iDoF.
C                     * If no other restraint direction(s) specified
C                       then assume the purpose of this data entry
C                       line is to remove the previously-entered
C                       restraint iDoF from on the selected node.

                     IF(SCAN(Res,ADoF(1)//ADoF(2)//ADoF(3)).GT.0)
     +               THEN
                        CYCLE
                     ELSE
                        KFIXD(IDOFN(ABS(iNod),iDoF)) = 0
                        iPRES = iPRES-1
                     END IF !Whether ANY valid restrd directn specifd

                  ELSE
C                    This node is not already restrained
C                    in direction iDoF
                     CYCLE

                  END IF !Whether already restrained in directn iDoF

               END IF !Whether a valid restraint direction entered
            END DO !iDoF=1,3
         END DO !Inod=Inod,Lnod,INC

C       'Previous' restraint input values
           !INODP = 0
           !ADOFP = '    '

      END DO

      IF(GoodRES) THEN

C        Set up new restraints codes
C        CALL RCODE


C    SPRING RESTRAINTS TABLE

C     Initialise
C     ----------
C     Set GoodRES to .TRUE. pending any checks to the contrary
      GoodSpr=.TRUE.
      DO iSpri = 1,NSPRI
         SPRING(iSpri)%Node  = 0
         SPRING(iSpri)%DoF   = 0
         SPRING(iSpri)%Stiff = 0.0
      END DO

C    'Previous' node input values
      InodP = 0
      nINC  = 1

C     Now cycle through all rows in the on-screen
C     springs data-entry table
C    --------------------------------------------
      iSpri = 0
      DO Itab = 1,NSPRI

C        Read line Itab from the on-screen data-entry table
         CALL GWGTBF(idSprTbl,Itab,1,rNod)
         CALL GWGTBS(idSprTbl,Itab,2,DoFdat)
         CALL GWGTBF(idSprTbl,Itab,3,SprKdat)

         IF(rNod.GE.0.0) THEN

C           Single sprung-node entry
C           ------------------------
            INC  = 1
            iNod = INT(rNod)
            LNod = iNod
            rInc = rNod-REAL(iNod)
            GoodSPR = .TRUE.

            IF(rInc.NE.0.0) THEN
               CALL BailOUT
     +         ('ERROR|Use sprung-node increment decimal only '//
     +          'with hyphenated end-of-range node number.|'//
     +          'Springs ignoded on Node '//
     +         TRIM(I2CHAR(iNOD,idum))//'.$','OPTIONS','YES')
               GoodSPR = .FALSE.
            END IF

         ELSE

C           Data generation
C           ---------------
            rNod = -rNod
            LNod = INT(rNod)              !Last node in the sequence
            rInc = rNod-REAL(LNod)
            DO WHILE (rINC.NE.0.0 .AND. ABS(rInc).LT.1.0)
               rInc=10.0*rInc
            END DO
            INC = NINT(rInc)              !Node number increment
            IF(INC.EQ.0) INC=1
            nINC = IABS((LNod-iNodP)/INC) !Number of increments in all
            IF(LNod.LT.iNodP) INC=(-INC)
            iNod = iNodP+INC              !First node in the sequence

C           Check that the increments will hit the last-node target

            IF(iNodP+nINC*INC .NE. LNod) THEN
               CALL BailOUT
     +         ('ERROR|Data generation error after sprung node '//
     +         TRIM(I2CHAR(iNodP,idum))//'.|'//
     +         'Equal sprung-node increments of '
     +          //TRIM(I2CHAR(INC,idum))//
     +          ' will miss the last node.$', 'OPTIONS','YES')
               GoodSPR = .FALSE.
            END IF
         END IF ! Single sprung-node or data generation

         IF((iNod.GT.NNODE) .OR.
     +   (iNod.EQ.0 .AND. .NOT.(LEN_TRIM(Res).EQ.0))) THEN
            CALL BailOUT
     +      ('ERROR|Sprung-node number must be between 1 and '//
     +      TRIM(I2CHAR(NNODE,idum))//'.|'//
     +      'Springs discarded for '//
     +      'invalid node '//TRIM(I2CHAR(iNod,Idum))//'.$',
     +      'OPTIONS',
     +      'YES')
            GoodSPR = .FALSE.
         END IF

C        Now loop through all data-gen nodes and store the data
C        (which will be just one pass if datagen is not invoked)

         DO Inod=Inod,Lnod,INC
C..         CHECK AND ACCEPT THE LINE OF SPRING RESTRAINTS DATA
C           Which DoF has the spring?
            Do iDoF=1,3
               IF(SCAN(ADoF(iDoF),DoFdat).NE.0) EXIT !Spring is on iDoF
            END DO

C           Check not rigidly restrained
            IF (KFIXD(IDOFN(iNod,iDoF)) .EQ. 1) THEN
               CALL BailOUT('ERROR:|'//
     +         'Springs not possible on rigid restraints.|'
     +         //'Node '//TRIM(I2CHAR(iNod,iDum))
     +         //'('//ADoF(iDoF)(1:1)//') '
     +         //'is rigidly restrained, so this|'
     +         //'spring will be ignored.$','OPTIONS' ,'YES')
               GoodSPR = .FALSE.
            END IF

C           Is there already a spring here?
            IF(GoodSPR) THEN
               DO iS=1,NSPRI
                  IF (SPRING(iS)%Node.EQ.iNod
     +            .AND.SPRING(iS)%DoF.EQ.iDoF) THEN
                     CALL BailOUT('ERROR:|'//
     +               'A spring has already been entered|'//
     +               'at Node '//TRIM(I2CHAR(iNod,iDum))//
     +               '('//ADoF(iDoF)(1:1)//'), so '
     +               //'this spring is ignored.$', 'OPTIONS' ,'YES')
                     GoodSPR = .FALSE.
                  END IF
               END DO
            END IF

C           Too many springs?
            IF(GoodSPR) THEN
               IF (iSpri+1.GT.NSPRI) THEN
                  CALL BailOUT('ERROR:|'//
     +            'Too many springs.|'//
     +            'Spring on node '//
     +            TRIM(I2CHAR(iNod,iDum))//
     +            '('//ADoF(iDoF)(1:1)//') ignored.$','OPTIONS' ,'YES')
                  GoodSPR = .FALSE.
               END IF
            END IF

            IF(GoodSPR) THEN
C              Accept the spring read from the file
C              and copy it to array storage
               iSpri=iSpri+1
               SPRING(iSpri)%Node  = iNod
               SPRING(iSpri)%DoF   = iDoF
               SPRING(iSpri)%Stiff = SprKdat
            END IF

         END DO

      END DO !Itab=1,NSPRI

C     Now call SENDOK to close Win1, allowing WinMENUS
C     to recycle Win1 with menus ghosted differently
      MORE = .TRUE.
      CALL SENDOK

      ELSE
C       (.NOT.GoodRes)
C        so:
C        RETURN leaving the error messages still displayed and the
C        MATERIALS data entry fields on screen for further data
         CALL SCRmsg(REPEAT('-',80),(.FALSE.))
         RETURN

      END IF

      RETURN
      END




      SUBROUTINE CloseRES
C     -------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

      USE WinSTUFF

      CALL WinRES('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END
