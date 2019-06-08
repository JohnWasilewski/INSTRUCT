      SUBROUTINE rNEW
C     ---------------
C     jw / 30-10-85  draft
C     jw / 02-04-12  last revised

C     Read problem size parameters
C     ----------------------------
C     Initialises.
C     Calls rPARAMS, which calls WinPAR for a PARAMS data entry window.
C     WinPAR reads user data from the screen then calls StorPAR to SAVE.
C     StorPAR saves the new PARAMS then calls ALLOC1 to ALLOCATE new
C     data storage array sizes.

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING

      CALL WinSCR('HIDE')
      CALL HidePANES
      CALL GhostMenus

      NMATS = 0
      NMTYP = 0
      NNODE = 0
      NMEMB = 0
      NPRES = 0
      NSPRI = 0

      CALL EndAlloc

      GoodPAR = .FALSE.
      GoodMAT = .FALSE.
      GoodMEM = .FALSE.
      GoodPRP = .FALSE.
      GoodNOD = .FALSE.
      GoodRES = .FALSE.
      GoodLOD = .FALSE.

      SOLVED = .FALSE.
      CALL LIST(0)
      CALL rPARAMS

      RETURN
      END



      SUBROUTINE rPARAMS
C     ------------------
C     jw / 30-10-85  draft
C     jw / 15-02-14  last revised

C     Read problem size parameters
C     ----------------------------

C     CALLED BY
C     ---------
C        WinMenus, on user selection from pull-down menu

C     CALLS
C     -----
C        WinPAR, for a for a PARAMS data entry pane in window WIN1.

C     RESULTING ACTION
C     ----------------
C        WinPAR displays a PARAMS data entry form then calls StorPAR.
C        StorPAR reads form-filled PARAMS, saves the PARAMS, then
C        calls ALLOC1 to ALLOCATE data storage array sizes.

      USE dislin
      USE WINSTUFF
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE FILES

      CALL HidePANES
      CALL GhostMenus

C     IS AN EXISTING STRUCTURE BEING RE-SIZED HERE?
C     Immediate return if user has an existing structure open
C     and doesn't want to lose it
      isYES=0
      IF(GoodPAR) THEN
         CALL DWGBUT('WARNING|'//
     +              'Changing structure size settings erases|'//
     +              'memory, re-allocates all array storage to the|'//
     +              'new size, and then re-reads the currently-saved|'//
     +              'structure data from the last-saved input file.||'//
     +              'OK to continue?.',isYES)
         IF(isYES.EQ.1) THEN
            ReSizing = .TRUE.!(YES=1 NO=0) and the user selected [YES]
C           In case the structure hasn't been saved yet, SAVE it now
            CALL FSave
C           Now preserve the previous size params
            NMATSx=NMATS
            NMTYPx=NMTYP
            NNODEx=NNODE
            NMEMBx=NMEMB
            NPRESx=NPRES
            NSPRIx=NSPRI

         ELSE
             ReSizing = .FALSE.!(YES=1 NO=0) and the user selected [NO]
            RETURN
         END IF !(isYES.EQ.1)

      END IF !GoodPAR

C     Else (.NOT.GoodPAR) .or. (GoodPAR .and. ReSizing), which means,
C     Either : PARAMS not yet read
C         Or : PARAMS have been read but they are now being ReSized.

      CALL WinSCR('HIDE')

      IF(.NOT.ReSizing) THEN
          NMATS = 0
          NMTYP = 0
          NNODE = 0
          NMEMB = 0
          NPRES = 0
          NSPRI = 0
      END IF !(.NOT.ReSizing)

      GoodPAR = .FALSE.
      GoodMAT = .FALSE.
      GoodPRP = .FALSE.
      GoodNOD = .FALSE.

      CALL SWGFNT(PrpN%Styl,PrpN%Size)

      Call WinPAR('SHOW')
      RETURN

      END




      SUBROUTINE WinPAR(ACTN)
C     -----------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

C     Window layout for Params input

      USE dislin
      USE WINSTUFF
      USE PARAMS

      EXTERNAL  StorPAR,
     +          ClosePAR,
     +          Listout

      CHARACTER ACTN*4, Blanks*24
      CHARACTER D8*16

      CHARACTER*10
     +          ANMATS,
     +          ANMTYP,
     +          ANNODE,
     +          ANMEMB,
     +          ANPRES,
     +          ANSPRI

      Blanks = REPEAT(' ',24)
      D8 = REPEAT(' .',8)

      SELECT CASE(ACTN)

C-----------------------------------------------------------------------
      CASE ('SHOW')
C-----------------------------------------------------------------------

         ShowsPAR = .TRUE.

C           Make PARams data entry fields
C-----------------------------------------------------------------------C-----------------------------------------------------------------------
C           Params pane
            CALL SWGPOS(Pan2L,Pan2T) !upper LH corner
            CALL WGBAS(Win1,'HORI',idParPAN)
C-----------------------------------------------------------------------
C           Divide params pane into four vertical stacks
            CALL SWGWTH(-26)
            CALL WGBAS(idParPAN,'VERT',idParPAN1)

            CALL SWGWTH(-26)
            CALL WGBAS(idParPAN,'VERT',idParPAN2)

            CALL SWGWTH(-26)
            CALL WGBAS(idParPAN,'VERT',idParPAN3)

            CALL SWGWTH(-7)
            CALL WGBAS(idParPAN,'VERT',idParPAN4)
C-----------------------------------------------------------------------
C           Prompt and blank lines across LH+M+RH stacks
            CALL WGLAB(idParPAN1,'STRUCTURE SIZE PARAMETERS',id0)
            CALL WGLAB(idParPAN2,'',id1)
            CALL WGLAB(idParPAN3,'',id1)
            CALL WGLAB(idParPAN4,'',id1)
C-----------------------------------------------------------------------
C           Prompt, messages and data entry fields in stack col 1
            WRITE (ANMEMB,'(I10)') NMEMB
            CALL WGLTXT(idParPAN1,
     +     'Number of members'//D8//D8//D8,ANMEMB,20,idNMEMB)

            WRITE (ANMTYP,'(I10)') NMTYP
            CALL WGLTXT(idParPAN1,
     +     'Number of member types'//D8//D8,ANMTYP,20,idNMTYP)

            WRITE (ANMATS,'(I10)') NMATS
            CALL WGLTXT(idParPAN1,
     +     'Number of materials'//D8//D8//D8,ANMATS,20,idNMATS)
C-----------------------------------------------------------------------
C           Prompt, messages and data entry fields in stack col 2
            WRITE (ANNODE,'(I10)') NNODE
            CALL WGLTXT(idParPAN2,
     +     'Number of nodes'//D8//D8//D8,ANNODE,20,idNNODE)

            WRITE (ANPRES,'(I10)') NPRES
            CALL WGLTXT(idParPAN2,
     +     'Number of rigid support restraints'//D8,ANPRES,20,idNPRES)

            WRITE (ANSPRI,'(I10)') NSPRI
            CALL WGLTXT(idParPAN2,
     +     'Number of sprung support restraints'//D8,ANSPRI,20,idNSPRI)
C-----------------------------------------------------------------------
C           Action buttons in stack col 4
            CALL WGPBUT(idParPAN4,'Accept',idOK)
            CALL SWGCBK(idOK, StorPAR)

            CALL WGPBUT(idParPAN4,'Cancel',idCancel)
            CALL SWGCBK(idCancel, ClosePAR)

            CALL WGPBUT(idParPAN4,'List input',idList)
            CALL SWGCBK(idList, Listout)

C-----------------------------------------------------------------------
      CASE('HIDE')
C-----------------------------------------------------------------------
         IF(id0.GT.0) CALL SWGATT(id0,'INACTIVE','STATUS')
         CALL SWGATT(idNMTYP, 'INACTIVE','STATUS')
         CALL SWGATT(idNMATS, 'INACTIVE','STATUS')
         CALL SWGATT(idNNODE, 'INACTIVE','STATUS')
         CALL SWGATT(idNMEMB, 'INACTIVE','STATUS')
         CALL SWGATT(idNPRES, 'INACTIVE','STATUS')
         CALL SWGATT(idNSPRI, 'INACTIVE','STATUS')
         CALL SWGATT(idOK,    'INACTIVE','STATUS')
         CALL SWGATT(idCANCEL,'INACTIVE','STATUS')
         CALL SWGATT(idLIST,  'INACTIVE','STATUS')

         ShowsPAR = .FALSE.

      END SELECT !(ACTN)

      RETURN
      END




      SUBROUTINE StorPAR
C     ------------------
C     jw / 29-01-12  draft
C     jw / 15-02-14  last revised

C     Called by WinPAR after WinPAR displayed a PARAMS data entry pane.
C     StorPAR reads keyboard data entered into the params fields, saves
C     the PARAMS, and calls ALLOC1 to ALLOCATE data storage array sizes.

      USE dislin
      USE WINSTUFF
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE RESTRAINTS
      USE FILES
      USE LUnits

      CHARACTER*10 ANUM
      LOGICAL NoCmnt
      IF(ReSizing) CALL FSave ! Save all input data

C     Obtain latest structure size parameters from Text Widgets

C     MATERIALS
      CALL GWGTXT(idNMATS,ANUM)
      READ (ANUM,'(I10)') NMATS

C     MEMBER TYPES
      CALL GWGTXT(idNMTYP,ANUM)
      READ (ANUM,'(I10)') NMTYP

C     NODES
      CALL GWGTXT(idNNODE,ANUM)
      READ (ANUM,'(I10)') NNODE

C     MEMBERS
      CALL GWGTXT(idNMEMB,ANUM)
      READ (ANUM,'(I10)') NMEMB

C     PRESCRIBED DISPLACEMENTS
C     ('Support settlements')
      CALL GWGTXT(idNPRES,ANUM)
      READ (ANUM,'(I10)') NPRES

C     SPRINGS
      CALL GWGTXT(idNSPRI,ANUM)
      READ (ANUM,'(I10)') NSPRI

      IF(ReSizing) THEN
C         Save all input data
C         CALL FSave

C         Attempt to re-open the input file
C         ---------------------------------
          OPEN(UNIT=LUI,
     +         FILE=TRIM(TRIM(F%NAME)//'.'//TRIM(F%I%EXT)),
     +         STATUS='OLD',
     +         ERR=50,
     +         IOSTAT=IOCODE)
          CALL ReadInput(LUI)
          CLOSE(LUI)
      END IF


C     WARNINGS
C     --------
      NoCmnt = .TRUE.

      IF(NMATS.GT.8) THEN
         CALL SCRmsg('Warning: So many material types intended?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NMTYP.GT.8) THEN
         CALL SCRmsg('Warning: So many member types intended?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NNODE.LT.4) THEN
         CALL SCRmsg('Warning: Really so few nodes?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NMEMB.LT.3) THEN
         CALL SCRmsg('Warning: So few members intended?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NPRES.LT.3) THEN
         CALL SCRmsg('Warning: With so few rigid restraints, '//
     +              'spring(s) will also be necessary',NoCmnt)
         NoCmnt = .FALSE.
      END IF


C     ERRORS
C     ------
      GoodPAR = .TRUE.

      IF(NMATS.LT.1) THEN
         CALL SCRmsg('**Error: At least 1 material type is needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NMTYP.LT.1) THEN
         CALL SCRmsg('**Error: At least 1 member type is needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NNODE.LT.2) THEN
         CALL SCRmsg('**Error: At least 2 nodes are needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NMEMB.LT.1) THEN
         CALL SCRmsg('**Error: At least 1 member is needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NPRES.LT.0) THEN
         CALL SCRmsg('**Error: Negative no. of restraints not allowed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NPRES.GE.3*NNODE) THEN
         CALL SCRmsg('**Error: Too many restraints',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NPRES+NSPRI.LT.3) THEN
         CALL SCRmsg('**Error: More supports and/or springs essential',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NSPRI.LT.0) THEN
         CALL SCRmsg('**Error: Negative number of springs not allowed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NSPRI.GT.3*NNODE-NPRES) THEN
         CALL SCRmsg('**Error: Too many springs (more than total DoF)',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      CALL WinPAR('Hide')
      SOLVED = .FALSE.

      IF(.NOT.GoodPAR) THEN
C     RETURN leaving the error messages still displayed and the
C     PARAMS data entry fields on screen for further data
         CALL SCRmsg(REPEAT('-',80),(.FALSE.))
         RETURN

      ELSE
C        Call ALLOC1 to allocate data storage array sizes
         CALL ALLOC1

C        If rParams is being used to enter a new set of PARAMS then
C        re-read all other structure data from the input file
C        (subroutine ReadINPUT will skip reading the old PARAMS)

         IF(F%I%EXISTS.AND.ReSizing) THEN
C             Attempt to open existing file
C             -----------------------------
              OPEN(UNIT=LUI,
     +             FILE=TRIM(F%NAME)//'.'//TRIM(F%I%EXT),
     +             STATUS='OLD',
     +             IOSTAT=IOCODE)

C             File-open succeeded
C             -------------------
              LUISET     = .TRUE.
              F%I%OPEN   = .TRUE.
              F%I%LU     =  LUI

              CALL ReadINPUT(F%I%LU)

              CLOSE(LUI, STATUS='KEEP',IOSTAT=IOCODE)
              LUISET     = .FALSE.
              F%I%OPEN   = .FALSE.
              F%I%LU     =  0

C             Now initialise any extra lines of structure data

              IF (NMATSx.GT.NMATS) THEN
                  DO iMAT=NMATSx+1,NMATS
                        Matls(iMat)%Gamma = 0.0
                        Matls(iMat)%Desc  = ' '
                        Matls(iMat)%Emod  = 0.0
                        Matls(iMat)%Gmod  = 0.0
                  END DO ! iMAT
              END IF !(NMATSx.GT.NMATS)

              IF (NMTYPx.GT.NMTYP) THEN
                  DO iMTYP=NMTYPx+1,NMTYP
                        PROPS(iIMTYP)%Matl=0
                        PROPS(iMTYP)%Desc=' '
                        PROPS(iIMTYP)%AreaX=0.0
                        PROPS(iMTYP)%AreaY=0.0
                        PROPS(iIMTYP)%InrtZ=0.0
                  END DO ! iMTYP
              END IF !(NMTYPx.GT.NMTYP)

              IF (NNODEx.GT.NNODE) THEN
                  DO iNODE=NNODEx+1,NNODE
                        Node(1,iNode)=0.0
                        Node(2,iNode)=0.0
                  END DO ! INODE
              END IF !(NNODEx.GT.NNODE)

              IF (NMEMBx.GT.NMEMB) THEN
                  DO iMEMB=NMEMBx+1,NMEMB
                        MEMB(iMEMB)%Node1=0
                        MEMB(iMEMB)%Node2=0
                        MEMB(iMEMB)%MTyp=0
                        DO iDoF=1,6
                            MEMB(iMEMB)%Released(iDoF)=.FALSE.
                        END DO
                        MEMB(iMEMB)%StiffsStored=.FALSE.
                        DO iDoF=1,21
                            MEMB(iMEMB)%LStiff(iDoF)=0.0
                            MEMB(iMEMB)%GStiff(iDoF)=0.0
                        END DO
                        MEMB(iMEMB)%DESC=' '
                  END DO ! iMEMB
              END IF !(NMEMBx.GT.NMEMB)

C             IF (NPRESx.GT.NPRES) THEN
C                 DO iPRES=NPRESx+1,NPRES
C                 END DO ! iPRES
C             END IF !(NPRESx.GT.NPRES)

              IF (NSPRIx.GT.NSPRI) THEN
                  DO iSPRI=NSPRIx+1,NSPRI
                        SPRING(iSpri)%Node  = 0
                        SPRING(iSpri)%DoF   = 0
                        SPRING(iSpri)%Stiff = 0.0
                  END DO ! iSPRI
              END IF !(NSPRIx.GT.NSPRI)

              ReSizing = .FALSE.
         END IF

C        Now call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK
      END IF

      RETURN

C     Failed to open an input file
C     ----------------------------
50    CALL BAILOUT
     + ('ERROR trying to open file '//
     + TRIM(F%NAME)//'.'//TRIM(F%I%EXT)//' to re-read the input data '//
     + 'with new structure size settings.$','BailOut','NO')

      END





      SUBROUTINE ClosePAR
C     ------------------
C     jw / 06-02-12  draft
C     jw / 06-02-12  last revised

      USE WinSTUFF

      CALL WinPAR('HIDE')
      MORE = .TRUE.
      CALL SENDOK

      RETURN
      END
