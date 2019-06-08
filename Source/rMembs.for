      SUBROUTINE rMEMBS
C     -----------------
C     jw / 09-04-12    draft
C     jw / 09-04-12  last mod

C     Read member connections
C     -----------------------
C     Creates a data entry table in a members pane of window WIN1.
C     Calls StorMEMBS to read keyboard data entered into the membs table

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS

      CALL HidePANES
      CALL GhostMenus

C     GoodMEM = .FALSE.


      SOLVED = .FALSE.
      CALL WinMEM('SHOW')

      RETURN
      END




      SUBROUTINE WinMEM(ACTN)
C     -----------------------
C     jw / 09-04-12  draft
C     jw / 09-04-12  last revised

C     Window layout for Members input

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE MEMBERS

      EXTERNAL  StorMEMBS,
     +          CloseMEM,
     +          Listout,
     +          Help_MemberConnctns,
     +          Help_MemberEndReleases

      REAL      ColWidths(7)
      INTEGER   Imem
      CHARACTER ACTN*4, Rel*5

      DATA      ColWidths
     +   / -10., -9.,   -41.,      -7., -12.,        -7., -12. /
C          Memb  Type  Description  Node1 Releases    Node2 Releases


      SELECT CASE(ACTN)

C-----------------------------------------------------------------------
         CASE('SHOW')
C-----------------------------------------------------------------------
         CALL WGAPP(MenuHG,
     +   'Member types and connections',MenuHGN)
         CALL SWGCBK(MenuHGN, Help_MemberConnctns)

         CALL WGAPP(MenuHG,
     +   'Member-end releases',MenuHGN)
         CALL SWGCBK(MenuHGN, Help_MemberEndReleases)

         ShowsMEM = .TRUE.

C           Make MEMbers data entry fields
C-----------------------------------------------------------------------
C           Members pane
               CALL SWGPOS(Pan2L,Pan2T)
               CALL WGBAS(Win1,'HORI',idMemPAN)
C-----------------------------------------------------------------------
C           Divide members pane into two vertical stacks
               CALL SWGWTH(-83)
               CALL WGBAS(idMemPAN,'VERT',idMemPAN1)

               CALL SWGWTH(-7)
               CALL WGBAS(idMemPAN,'VERT',idMemPAN2)
C-----------------------------------------------------------------------
C           Member types input table
               CALL SWGRAY(ColWidths,9,'TABLE')
               CALL SWGOPT('COLUMNS', 'HEADER')
               CALL SWGOPT('ON', 'EDIT')
               CALL SWGJUS('LEFT', 'TABLE')
               CALL WGTBL(idMemPAN1,MAX(1,NMEMB),7,idMemTbl)
               CALL SWGTBS(idMemTbl,'Memb no.',0,1,'VALUE')
               CALL SWGTBS(idMemTbl,'Type',0,2,'VALUE')
               CALL SWGTBS(idMemTbl,'Description',0,3,'VALUE')
               CALL SWGTBS(idMemTbl,'Node1',0,4,'VALUE')
               CALL SWGTBS(idMemTbl,'Releases',0,5,'VALUE')
               CALL SWGTBS(idMemTbl,'Node2',0,6,'VALUE')
               CALL SWGTBS(idMemTbl,'Releases',0,7,'VALUE')

C           Copy stored values into the on-screen table
            DO Imem=1,NMEMB
               CALL SWGTBI(idMemTbl,IMem,IMem,1,'VALUE')
               CALL SWGTBI(idMemTbl,MEMB(IMem)%MTyp,IMem,2,'VALUE')
               CALL SWGTBS(idMemTbl,MEMB(IMem)%Desc,IMem,3,'VALUE')
               CALL SWGTBI(idMemTbl,MEMB(IMem)%Node1,IMem,4,'VALUE')
               CALL SWGTBI(idMemTbl,MEMB(IMem)%Node2,IMem,6,'VALUE')

               Rel=Repeat(' ',5)
               IF(MEMB(IMem)%Released(1)) Rel=Trim(Rel)//'X '
               IF(MEMB(IMem)%Released(2)) Rel=Trim(Rel)//'Y '
               IF(MEMB(IMem)%Released(3)) Rel=Trim(Rel)//'Z '
               CALL SWGTBS(idMemTbl,Trim(Rel),IMem,5,'VALUE')

               Rel=Repeat(' ',5)
               IF(MEMB(IMem)%Released(4)) Rel=Trim(Rel)//'X '
               IF(MEMB(IMem)%Released(5)) Rel=Trim(Rel)//'Y '
               IF(MEMB(IMem)%Released(6)) Rel=Trim(Rel)//'Z '
               CALL SWGTBS(idMemTbl,Trim(Rel),IMem,7,'VALUE')

            END DO
C-----------------------------------------------------------------------
C           Action buttons in stack col 2
            CALL WGPBUT(idMemPAN2,'Accept',idOK)
            CALL SWGCBK(idOK, StorMEMBS)

            CALL WGPBUT(idMemPAN2,'Cancel',idCancel)
            CALL SWGCBK(idCancel, CloseMem)

            CALL WGPBUT(idMemPAN2,'List input',idList)
            CALL SWGCBK(idList, Listout)

            CALL WGLAB(idMemPAN2,'',idLab)
            CALL WGLAB(idMemPAN2,
     +      'See HELP for',idLab)
            CALL WGLAB(idMemPAN2,
     +      'member sets',idLab)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         ShowsMEM = .FALSE.
         IF(id0.GT.0) CALL SWGATT(id0,'INACTIVE','STATUS')
         IF(id1.GT.0) CALL SWGATT(id1,'INACTIVE','STATUS')
         CALL SWGATT(idMemPAN, 'INACTIVE','STATUS')
         CALL SWGATT(idMemPAN1,'INACTIVE','STATUS')
         CALL SWGATT(idMemPAN2,'INACTIVE','STATUS')
         CALL SWGATT(idMemTbl, 'INACTIVE','STATUS')
         CALL SWGATT(idOK,     'INACTIVE','STATUS')
         CALL SWGATT(idCANCEL, 'INACTIVE','STATUS')
         CALL SWGATT(idLIST,   'INACTIVE','STATUS')

      END SELECT !(ACTN)

      RETURN
      END




      SUBROUTINE StorMEMBS
C     --------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE MEMBERS

      REAL*4    rMem
      INTEGER   mTyp, Node1, Node2
      CHARACTER I2CHAR*8, Des*24, DesP*24
      CHARACTER RelI*3, RelJ*3, RelIp*3, RelJp*3

C     Initialise
C     ----------
      GoodMEM = .TRUE.

C     'Previous' node input values
      ImemP = 0
      mINC  = 1
      mTypP = 0
      Node1P  = 0
      Node2P  = 0


C     Cycle through all rows in the on-screen data-entry table
C     --------------------------------------------------------
      DO Itab = 1,NMEMB

C        Read line Itab from the on-screen data-entry table
         CALL GWGTBF(idMemTbl,Itab,1,rMem)
         CALL GWGTBI(idMemTbl,Itab,2,mTyp)
         CALL GWGTBS(idMemTbl,Itab,3,Des)
         CALL GWGTBI(idMemTbl,Itab,4,Node1)
         CALL GWGTBS(idMemTbl,Itab,5,RelI)
         CALL GWGTBI(idMemTbl,Itab,6,Node2)
         CALL GWGTBS(idMemTbl,Itab,7,RelJ)

         IF(rMem.GE.0.0) THEN

C           Single member entry
C           -------------------
            iMem     = INT(rMem)
            LMem     = iMem
            mINC     = 1
            mTypP    = mTyp
            MTypINC  = 0
            Node1P   = Node1
            Node1INC = 0
            Node2P   = Node2
            Node2INC = 0

            rInc = rMem-REAL(iMem)
            IF(rInc.NE.0.0) THEN

               CALL BailOUT
     +         ('ERROR|Use member-increment decimal only with '//
     +            'hyphenated end-of-range member number.|'//
     +            'Member '//TRIM(I2CHAR(iMem,idum))//' ignored.$',
     +            'OPTIONS',
     +            'YES')

               GoodMEM = .FALSE.
            END IF
         ELSE

C           Data generation
C           ---------------
            Des    = DesP
            RelI   = RelIp
            RelJ   = RelJp

C           Member-number increments

            rMem = -rMem
            LMem = INT(rMem)                !Last member in the sequence
            rInc = rMem-REAL(LMem)
            DO WHILE (rINC.NE.0.0 .AND. ABS(rInc).LT.1.0)
               rInc=10.0*rInc
            END DO
            mINC = NINT(rInc)               !Member number increment
            IF(LMem.LT.iMemP) mINC=(-mINC)
            IF(mINC.EQ.0) mINC=1
            NumINC = IABS((LMem-iMemP)/mINC)!Number of increments in all

            IF(iMemP + NumINC*mINC .NE. LMem) THEN
               CALL BailOUT
     +         ('ERROR|Data generation error after member '//
     +          TRIM(I2CHAR(iMemP,idum))//'.|'//
     +         'Equal member increments of '//TRIM(I2CHAR(mINC,idum))//
     +         ' will miss the last member in the pattern.$',
     +         'OPTIONS','YES')
               GoodMEM = .FALSE.
            END IF

C           Node1 increments

            Node1INC = (Node1-Node1p)/NumINC        !Node1 increment
            IF(Node1p + NumINC*Node1INC.NE.Node1) THEN
               CALL BailOUT
     +         ('ERROR|Data generation error after member '//
     +          TRIM(I2CHAR(iMemP,idum))//'.|'//
     +         'Equal Node1 increments of '//
     +          TRIM(I2CHAR(Node1INC,idum))//
     +         ' will miss the last Node1.$',
     +         'OPTIONS','YES')
               GoodMEM = .FALSE.
            END IF

C           Node2 increments

            Node2INC = (Node2-Node2p)/NumINC        !Node2 increment
            IF(Node2p + NumINC*Node2INC.NE.Node2) THEN
               CALL BailOUT
     +         ('ERROR|Data generation error after member '//
     +          TRIM(I2CHAR(iMemP,idum))//'.|'//
     +         'Equal Node2 increments of '//
     +          TRIM(I2CHAR(Node2INC,idum))//
     +         ' will miss the last Node2.$',
     +         'OPTIONS','YES')
               GoodMEM = .FALSE.

            END IF

            iMem = iMemP+mINC !First node in the sequence
C           NumINC = NumINC - 1!Number of increments after the first one

         END IF ! Single node or data generation

         IF((iMem.GT.NMEMB)
     +       .OR.
     +      (iMem.EQ.0 .AND. .NOT.(Node1.EQ.0 .AND. Node2.EQ.0))) THEN
            CALL BailOUT
     +      ('ERROR| Member number must be between 1 and '//
     +      TRIM(I2CHAR(NMEMB,idum))//'.|'//
     +      'Node connections discarded for '//
     +      'invalid member '//TRIM(I2CHAR(iNod,Idum))//'.$',
     +       'OPTIONS','YES')
            GoodMEM = .FALSE.
         END IF

C        Now loop through all data-gen nodes and store the coordinates
C        (which will be just one pass if datagen is not invoked)

         Node1 = Node1P
         Node2 = Node2P
         mTyp  = mTypP

         DO Imem=Imem,Lmem,mINC
            mTyp  = mTypP+mTypINC
C           Des   = Des
            Node1 = Node1+Node1INC
            Node2 = Node2+Node2INC
C           RelI  = RelI
C           RelJ  = RelJ

C..         Accept the line of DATA
            MEMB(Imem)%MTyp  = mTyp
            MEMB(Imem)%Desc  = Des
            MEMB(Imem)%Node1 = Node1
            MEMB(Imem)%Node2 = Node2

            MEMB(Imem)%Released(1) =
     +      (INDEX(RelI(1:3),'X').NE.0 .OR. INDEX(RelI(1:3),'x').NE.0)

            MEMB(Imem)%Released(2) =
     +      (INDEX(RelI(1:3),'Y').NE.0 .OR. INDEX(RelI(1:3),'y').NE.0)

            MEMB(Imem)%Released(3) =
     +      (INDEX(RelI(1:3),'Z').NE.0 .OR. INDEX(RelI(1:3),'z').NE.0)

            MEMB(Imem)%Released(4) =
     +      (INDEX(RelJ(1:3),'X').NE.0 .OR. INDEX(RelJ(1:3),'x').NE.0)

            MEMB(Imem)%Released(5) =
     +      (INDEX(RelJ(1:3),'Y').NE.0 .OR. INDEX(RelJ(1:3),'y').NE.0)

            MEMB(Imem)%Released(6) =
     +      (INDEX(RelJ(1:3),'Z').NE.0 .OR. INDEX(RelJ(1:3),'z').NE.0)

C           Flag member stiffnesses as unknown
            MEMB(iMem)%StiffsStored = .FALSE.

         END DO

C        Store 'previous' node input values
         IMemP  = IMem-mINC
         mTypP  = mTyp
         DesP   = DEs
         Node1p = Node1
         RelIp  = RelI
         Node2p = Node2
         RelJp  = RelJ

      END DO

      IF(.NOT.GoodMEM) THEN
C     RETURN leaving the error messages still displayed and the
C     MATERIALS data entry fields on screen for further data
         RETURN
      ELSE
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK
      END IF

      RETURN
      END




      SUBROUTINE CloseMEM
C     -------------------
C     jw / 30-03-12  draft
C     jw / 30-03-12  last revised

      USE WinSTUFF

      CALL WinMEM('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END