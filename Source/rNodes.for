      SUBROUTINE rNODES
C     -----------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last mod

C     Read node coordinates
C     ---------------------
C     Creates a data entry table in a nodes pane of window WIN1.
C     Calls StorNODES to read keyboard data entered into the nodes table

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE LUnits

      CALL HidePANES
      CALL GhostMenus

C     GoodNOD = .FALSE.

      SOLVED = .FALSE.
      CALL WinNOD('SHOW')

      RETURN
      END




      SUBROUTINE WinNOD(ACTN)
C     -----------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

C     Window layout for Materials input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE NODES
      USE Restraints

      EXTERNAL  StorNODES,
     +          CloseNOD,
     +          Listout,
     +          Help_Coords,
     +          Help_Supports

      REAL      ColWidths(7)
      INTEGER   Inod
      CHARACTER ACTN*4, NodRes*3, ADoF*3

      PARAMETER (ADoF = ('XYZ'))

      DATA      ColWidths
     +         /-8., -13., -20.,   -25.,      -11.,   -11.,   -10./
C               Node   X    Y  RigdSuppts   SprgKx SprgKy SprgKz

      SELECT CASE(ACTN)

C-----------------------------------------------------------------------
      CASE('SHOW')
C-----------------------------------------------------------------------

      CALL WGAPP(MenuHG,'Node coordinates',MenuHGN)
      CALL SWGCBK(MenuHGN, Help_Coords)

      CALL WGAPP(MenuHG,'Supports',MenuHGN)
      CALL SWGCBK(MenuHGN, Help_Supports)

         ShowsNOD = .TRUE.

C           Make NODes data entry fields
C-----------------------------------------------------------------------
C           Nodes pane
               CALL SWGPOS(Pan2L,Pan2T)
               CALL WGBAS(Win1,'HORI',idNodPAN)
C-----------------------------------------------------------------------
C           Divide nodes pane into two vertical stacks
               CALL SWGWTH(-83)
               CALL WGBAS(idNodPAN,'VERT',idNodPAN1)

               CALL SWGWTH(-7)
               CALL WGBAS(idNodPAN,'VERT',idNodPAN2)
C-----------------------------------------------------------------------
C           Node coordinates input table
               CALL SWGRAY(ColWidths,7,'TABLE')
               CALL SWGOPT('COLUMNS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idNodPAN1,MAX(1,NNODE),7,idNodTbl)
               CALL SWGTBS(idNodTbl,'Node',0,1,'VALUE')
               CALL SWGTBS(idNodTbl,'X-coordinate',0,2,'VALUE')
               CALL SWGTBS(idNodTbl,'Y-coordinate',0,3,'VALUE')

               CALL SWGTBS(idNodTbl,'Rigid Support directions X,Y,Z',
     +                                                      0,4,'VALUE')

               CALL SWGTBS(idNodTbl,'Spring-K(x)',0,5,'VALUE')
               CALL SWGTBS(idNodTbl,'Spring-K(y)',0,6,'VALUE')
               CALL SWGTBS(idNodTbl,'Spring-K(z)',0,7,'VALUE')

            DO Inod=1,NNODE
C              Copy stored node coordinates into the on-screen table
               CALL SWGTBI(idNodTbl,Inod,Inod,1,'VALUE')
               CALL SWGTBF(idNodTbl,Node(1,Inod),-2,Inod,2,'VALUE')
               CALL SWGTBF(idNodTbl,Node(2,Inod),-2,Inod,3,'VALUE')
               NodRes = ''

C              Copy stored rigid restraints into the on-screen table
               DO iDoF=1,3
                  IDOFG = IDOFN(ABS(Inod),iDoF)
                  IF(KFIXD(IDOFG).EQ.1) THEN
                     NodRes=Trim(NodRes)//ADOF(iDoF:iDoF)
C                    KFIXD(IDOFG) = 0
                  END IF
               END DO !iDoF
               CALL SWGTBS(idNodTbl,TRIM(NodRes),Inod,4,'VALUE')

C              Copy stored spring restraints into the on-screen table
               DO iSpr=1,NSPRI
                  IF(SPRING(ISpr)%Node.EQ.Inod) THEN
                     IF(SPRING(ISpr)%DoF.EQ.1) THEN
                        CALL SWGTBF
     +                  (idNodTbl,SPRING(ISpr)%Stiff,-2,Inod,5,'VALUE')
                     ELSE IF(SPRING(ISpr)%DoF.EQ.2) THEN
                        CALL SWGTBF
     +                  (idNodTbl,SPRING(ISpr)%Stiff,-2,Inod,6,'VALUE')
                     ELSE IF(SPRING(ISpr)%DoF.EQ.3) THEN
                        CALL SWGTBF
     +                  (idNodTbl,SPRING(ISpr)%Stiff,-2,Inod,7,'VALUE')
                     END IF
                  END IF
               END DO

            END DO

C-----------------------------------------------------------------------
C           Action buttons in stack col 2
            CALL WGPBUT(idNodPAN2,'Accept',idOK)
            CALL SWGCBK(idOK, StorNODES)

            CALL WGPBUT(idNodPAN2,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseNOD)

            CALL WGPBUT(idNodPAN2,'List input',idList)
            CALL SWGCBK(idList, Listout)

            CALL WGLAB(idNodPAN2,'',idLab)
            CALL WGLAB(idNodPAN2,
     +      'See HELP for',idLab)
            CALL WGLAB(idNodPAN2,
     +      'node patterns',idLab)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsNOD = .FALSE.
C        IF(id0.GT.0) CALL SWGATT(id0,'INACTIVE','STATUS')
C        IF(id1.GT.0) CALL SWGATT(id1,'INACTIVE','STATUS')
         CALL SWGATT(idNodPAN, 'INACTIVE','STATUS')
         CALL SWGATT(idNodPAN1,'INACTIVE','STATUS')
         CALL SWGATT(idNodPAN2,'INACTIVE','STATUS')
         CALL SWGATT(idNodTbl, 'INACTIVE','STATUS')
         CALL SWGATT(idOK,     'INACTIVE','STATUS')
         CALL SWGATT(idCANCEL, 'INACTIVE','STATUS')
         CALL SWGATT(idLIST,   'INACTIVE','STATUS')

      END SELECT !(ACTN)

      RETURN
      END




      SUBROUTINE StorNODES
C     --------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE NODES
      USE Restraints
      USE Members


      REAL      Spr(3)
      REAL*4    X, Y
      CHARACTER I2CHAR*8, ResXYZ*6, ADoF*3

      PARAMETER (ADoF = ('XYZ'))

C     Initialise
C     ----------
      GoodNOD = .TRUE.
      KFIXD = 0

C     'Previous' node input values
      InodP = 0
      nINC  = 1
      Xp    = 0.0
      Yp    = 0.0


C     Count restraints already set
C     ----------------------------
      iPRES=0
      iSpri=0
      DO iNod=1,NNODE
         DO iDof=1,3
            iPRES=iPRES+KFIXD(IDOFN(ABS(iNod),iDoF))
         END DO
      END DO

C     Cycle through all rows in the on-screen data-entry table
C     --------------------------------------------------------
      DO Itab = 1,NNODE

C        Read line Itab from the on-screen data-entry table
C
         CALL GWGTBF(idNodTbl,Itab,1,rNod)       !Node (or last node)
         CALL GWGTBF(idNodTbl,Itab,2,X)          !Coordinate-X
         CALL GWGTBF(idNodTbl,Itab,3,Y)          !Coordinate-Y
         CALL GWGTBS(idNodTbl,Itab,4,ResXYZ)     !Any rigid restraints
         DO i=1,3
            Spr(i)=0.0
            CALL GWGTBF(idNodTbl,Itab,4+i,Spr(i))!Any springs
         END DO

         IF(rNod.GE.0.0) THEN

C           Single node entry
C           -----------------
            Xp   = X
            Xinc = 0.0
            Yp   = Y
            Yinc = 0.0
            INC  = 1
            iNod = INT(rNod)
            LNod = iNod
            rInc = rNod-REAL(iNod)
            IF(rInc.NE.0.0) THEN
               CALL BailOut
     +         ('ERROR|Use node-increment decimal only with '//
     +          'hyphenated end-of-range node number.|'//
     +          'Node '//TRIM(I2CHAR(iNOD,idum))//' ignored.$',
     +          'OPTIONS' ,'YES')
               GoodNOD = .FALSE.
            END IF

         ELSE

C           Data generation
C           ---------------
            rNod = -rNod
            LNod = INT(rNod)             !Last node in the sequence
            rInc = rNod-REAL(LNod)
            DO WHILE (rINC.NE.0.0 .AND. ABS(rInc).LT.1.0)
               rInc=10.0*rInc
            END DO
            INC = NINT(rInc)              !Node number increment
            IF(INC.EQ.0) INC=1

C           Work out the coordinate increments

            nINC = IABS((LNod-iNodP)/INC) !Number of increments in all
            Xinc = (X-Xp)/nINC            !X increment
            Yinc = (Y-Yp)/nINC            !Y increment
            IF(LNod.LT.iNodP) INC=(-INC)
            iNod = iNodP+INC              !First node in the sequence
C           nINC = nINC - 1  !Number of increments after the first one

C           Check that the increments will hit the last-node target

            IF(iNodP+nINC*INC .NE. LNod) THEN
               CALL BailOut
     +         ('Data generation error after node '//
     +          TRIM(I2CHAR(iNodP,idum))//'.|'//
     +          'Equal node increments of '//TRIM(I2CHAR(INC,idum))//
     +          ' will miss the last node.$',
     +          'OPTIONS' ,'YES')
                GoodNOD = .FALSE.
            END IF
         END IF ! Single node or data generation

         IF((iNod.GT.NNODE)
     +       .OR.
     +      (iNod.EQ.0 .AND. .NOT.(X.EQ.0.0 .AND. Y.EQ.0.0))) THEN
            CALL BailOut
     +      ('Error: Node number must be between 1 and '//
     +      TRIM(I2CHAR(NNODE,idum))//'.|'//
     +      'Node coodinates discarded for '//
     +      'invalid node '//TRIM(I2CHAR(iNod,Idum))//'.$',
     +      'OPTIONS' ,'YES')
            GoodNOD = .FALSE.
            END IF

C        READING AND STORING THE DATA
C        Now loop through all data-gen nodes and store the coordinates
C        (which will be just one pass if datagen is not invoked)

         X=Xp
         Y=Yp
         DO Inod=Inod,Lnod,INC
            X = X+Xinc
            Y = Y+Yinc

C..         Accept the line of DATA

C           Coordinates of this node
            NODE(1,Inod) = X
            NODE(2,Inod) = Y

C           Any rigid restraints on this node
            CALL GetRESTR(iNod,ResXYZ)
C           IF(.NOT.GoodRES) GoodNOD=.FALSE.

C           Any springs on this node
            DO iDoF=1,3
               IF(Spr(iDoF).NE.0.0) THEN
                  iSpri=iSpri+1
                  IF(iSpri.LE.NSPRI) THEN
                     SPRING(iSpri)%Node  = iNod
                     SPRING(iSpri)%DoF   = iDoF
                     SPRING(iSpri)%Stiff = Spr(iDoF)
                  ELSE
                     CALL BailOUT
     +               ('ERROR:|Too many springs.|'//
     +               'Spring on node '//
     +               TRIM(I2CHAR(iNod,idum))//
     +               ADOF(iDoF:iDoF)//' ignored.$',
     +               'OPTIONS','YES')
                  END IF
               END IF
            END DO

C           Now flag all members framing into this node as having
C           unknown stiffnesses
            DO iMem = 1, NMEMB
               IF(MEMB(Imem)%Node1.EQ.iNod.OR.MEMB(Imem)%Node2.EQ.iNod)
     +         MEMB(iMem)%StiffsStored = .FALSE.
            END DO !iMem = 1, NMEMB

         END DO

C        Store 'previous' node input values
         InodP=Inod-INC
         Xp = X
         Yp = Y

      END DO

      IF(.NOT.GoodNOD) THEN
C     RETURN leaving the error messages still displayed and the
C     MATERIALS and any RESTRaints data entry fields on screen for
C     further data entry/editing/correction.
         CALL SCRmsg(REPEAT('-',80),(.FALSE.))

C     Before RETURNing, clear all the restraint codes from KFIXD()
C     because they are still going to be displayed and re-read from the
C     on-screen table
         KFIXD=0
         RETURN

      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK
      END IF

      RETURN
      END




      SUBROUTINE CloseNOD
C     -------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

      USE WinSTUFF

      CALL WinNOD('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END