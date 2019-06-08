      SUBROUTINE WinMenus
C     -------------------
C     jw / 21-01-12  draft
C     jw / 15-02-12  last mod

      USE dislin
      USE LUnits
      USE Config
      USE FILES
      USE WINSTUFF
      USE M
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING
      USE DRW

      EXTERNAL
     +    FNew,
     +    FOpen,
     +    FSave,
     +    FSaveAs,
     +    rTitles,
     +    rParams,
     +    rMatls,
     +    rProps,
     +    rNodes,
     +    rMembs,
     +    rRestr,
     +    rLoads,
     -    rCombs,
     +    List,
     +    ListOUT,
     +    DrawSCR,
     +    SetDRAWlin,
     +    SetDRAWthk,
     +    Struct,
     +    Fini,
     +    Exit,
     +    Help_Start,
     +    Help_New,
     +    Help_TitlandSize,
     +    Help_SizLim,
     +    Help_MtlsNods,
     +    Help_Membs,
     +    Help_StrucandLoads,
     +    Help_MatNodSup,
     +    Help_Supports,
     +    Help_MemberProps,
     +    Help_ReadyforConnctns,
     +    Help_MemberConnctns,
     +    Help_MemberEndReleases,
     +    Help_StrucTables,
     +    Help_LoadTypes,
     +    Help_ReadyToRun,
     +    Help_Output,
     +    Help_Baffled,
     +    HelpWhatsNew,
     +    HelpAbout

      CHARACTER Mu*12, Ma*12, I2CHAR*16

      CALL ClearAllData
      CALL RTFini

      SOLVED   = .FALSE.
      GoodPAR  = .FALSE.
      GoodLOD  = .FALSE.

100   MORE = .FALSE.

      IF(ReSizing) THEN
          GoodPAR  = .TRUE.
          GoodLOD  = .FALSE.
      END IF

      idSCR    = 0
      idTitPAN = 0
      idParPAN = 0
      idMatPAN = 0
      idPrpPAN = 0
      idNodPAN = 0
      idMemPAN = 0
      idResPAN = 0
      idSprPAN = 0
      idDRW    = 0
      idLodPAN = 0

      ShowsSCR = .FALSE.
      ShowsTIT = .FALSE.
      ShowsPAR = .FALSE.
      ShowsMAT = .FALSE.
      ShowsPRP = .FALSE.
      ShowsNOD = .FALSE.
      ShowsMEM = .FALSE.
      ShowsRES = .FALSE.
      ShowsDRW = .FALSE.
      ShowsLOD = .FALSE.

C     Suppress the Exit menu
      CALL SWGPOP('NOOK')
      CALL SWGPOP('NOQUIT')
      CALL SWGPOP('NOHELP')
      CALL SWGOPT('OK','CLOSE')


C     Program window
      CALL SWGPOS(Win1L,Win1T)
      CALL SWGSIZ(Win1W,Win1H)
      CALL SWGFNT(PrpN%Styl,PrpN%Size)
      CALL WGINI('FORM',Win1) !<initialise>


C     CALL WinScr('SHOW')
C     IF(.NOT.SOLVED) CALL LIST(0)


C     ---------
C     FILE MENU
C     ---------

      CALL WGPOP(Win1,'File', MenuF)
          CALL WGAPP(MenuF,'Open', MenuF_Open)
          CALL SWGCBK(MenuF_Open, FOpen)

          CALL WGAPP(MenuF,'New',MenuF_New)
          CALL SWGCBK(MenuF_New, FNew)

          CALL WGAPP(MenuF,'Save structure', MenuF_Save)
          CALL SWGCBK(MenuF_Save, FSave)

          CALL WGAPP
     +  (MenuF,'Save-As new input date & output.rtf files',MenuF_SaveAs)
          CALL SWGCBK(MenuF_SaveAs, FSaveAs)

          CALL WGAPP(MenuF,'Exit', MenuF_Exit)
          CALL SWGCBK(MenuF_Exit, Fini)


C     --------------
C     STRUCTURE MENU
C     --------------

      CALL WGPOP(Win1,'Structure',MenuS)
          CALL WGAPP(MenuS,'New structure',MenuS_New)
          CALL SWGCBK(MenuS_New, FNew)

          CALL WGAPP(MenuS,'Titles',MenuS_Titles)
          CALL SWGCBK(MenuS_Titles, rTitles)

          CALL WGAPP(MenuS,'Size',MenuS_Params)
          CALL SWGCBK(MenuS_Params, rParams)

          CALL WGAPP(MenuS,'Materials',MenuS_Matls)
          CALL SWGCBK(MenuS_Matls, rMatls)

          CALL WGAPP(MenuS,'Member properties',MenuS_Props)
          CALL SWGCBK(MenuS_Props, rProps)

          CALL WGAPP(MenuS,'Nodes',MenuS_Nodes)
          CALL SWGCBK(MenuS_Nodes, rNodes)

          CALL WGAPP(MenuS,'Member connections',MenuS_Membs)
          CALL SWGCBK(MenuS_Membs, rMembs)

          CALL WGAPP(MenuS,'Supports',MenuS_Restr)
          CALL SWGCBK(MenuS_Restr, rRestr)

          CALL WGAPP(MenuS,'Save structure', MenuS_Save)
          CALL SWGCBK(MenuS_Save, FSave)

          CALL WGAPP
     +    (MenuS,'Save-TO new input & output.rtf files',MenuS_SaveAs)
          CALL SWGCBK(MenuS_SaveAs, FSaveAs)


C        Diagram in pane 3
C        -----------------
          CALL SWGPOS(Pan3L,Pan3T)
          CALL SWGSIZ(Pan3W,Pan3H)

          CALL WGDRAW(Win1,idDRW)
          CALL scrmod('REVERSE')

          CALL SWGATT(idDRW,'INACTIVE','STATUS')

C     --------------
C     LOADS MENU
C     --------------

      CALL WGPOP(Win1,'Loading',MenuL)
          CALL WGAPP(MenuL,'Load cases',MenuL_Loads)
          CALL SWGCBK(MenuL_Loads, rLoads)

          CALL WGAPP(MenuL,'Load combinations',MenuL_Combs)
          CALL SWGCBK(MenuL_Combs, rCombs)



C     -------------
C     OUTPUT MENU
C     -------------

      CALL WGPOP(Win1,'Output',MenuO)

          CALL WGPOP(MenuO,'Show diagram as . . .',MenuO_DRW)

              CALL WGAPP(MenuO_DRW,'thin skeletal lines',MenuO_DRWlines)
              CALL SWGCBK(MenuO_DRWlines, SetDrawLin)

              CALL WGAPP
     +        (MenuO_DRW,'estimated member thicknesses',MenuO_DRWmemthk)
              CALL SWGCBK(MenuO_DRWmemthk, SetDrawThk)

          CALL WGAPP
     +   (MenuO,'Write input data only to the output file',MenuO_List)
          CALL SWGCBK(MenuO_List, ListOUT)

          CALL WGAPP(MenuO,'SOLVE then write input data + results '//
     +                     'to the output file',MenuO_Solve)
          CALL SWGCBK(MenuO_Solve, Struct)


C     --------------
C     HELP MENU
C     --------------

      CALL WGPOP(Win1,'HELP',MenuH)
          CALL WGPOP(MenuH,'GUIDANCE',MenuHG)
          CALL WGPOP(MenuH,'Program information',MenuHI)

          CALL WGAPP(MenuHI,'About',MenuHI_A)
          CALL SWGCBK(MenuHI_A, HelpAbout)
C
          CALL WGAPP(MenuHI,'What''s new',MenuHI_WN)
          CALL SWGCBK(MenuHI_WN, HelpWhatsNew)


C     ----------------------------------------
C     MENU GHOSTING and the HELP MENU
C     (depending on what data already entered)
C     ----------------------------------------

      IF(.NOT.F%Named) THEN
C         Prevent data entry until a structure filename has been chosen
C         and the input and output files have been opened.
          CALL SWGATT(MenuS_New,      'ACTIVE',   'STATUS')
C         CALL SWGATT(MenuF_New,      'INACTIVE', 'STATUS')
          CALL SWGATT(MenuF_Save,     'INACTIVE', 'STATUS')
          CALL SWGATT(MenuF_SaveAS,   'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Titles,   'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Params,   'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Matls,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Props,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Nodes,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Membs,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Restr,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Save,     'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_SaveAS,   'INACTIVE', 'STATUS')
          CALL SWGATT(MenuL_Loads,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuL_Combs,    'INACTIVE', 'STATUS')
C         CALL SWGATT(MenuO_List,     'INACTIVE', 'STATUS')
          CALL SWGATT(MenuO_DRWmemthk,'INACTIVE', 'STATUS')
          CALL SWGATT(MenuO_DRWlines, 'INACTIVE', 'STATUS')
          CALL SWGATT(MenuO_Solve,    'INACTIVE', 'STATUS')

          CALL WGAPP(MenuHG,'How to start',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_Start)

          CALL WGAPP(MenuHG,'Entering titles & structure size',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_TitlandSize)

          CALL WGAPP(MenuHG,'Problem size limit',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_SizLim)

          CALL WGAPP(MenuHG,'Baffled?',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_Baffled)


      ELSE IF(.NOT.GoodPAR) THEN
C     Parameters not yet entered
          CALL SWGATT(MenuS_Params,   'ACTIVE',   'STATUS')
          CALL SWGATT(MenuS_Matls,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Props,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Nodes,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Membs,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuS_Restr,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuL_Loads,    'INACTIVE', 'STATUS')
          CALL SWGATT(MenuL_Combs,    'INACTIVE', 'STATUS')
C         CALL SWGATT(MenuO_List,     'INACTIVE', 'STATUS')
          CALL SWGATT(MenuO_DRWmemthk,'INACTIVE', 'STATUS')
          CALL SWGATT(MenuO_DRWlines, 'INACTIVE', 'STATUS')
          CALL SWGATT(MenuO_Solve,    'INACTIVE', 'STATUS')

          CALL WGAPP(MenuHG,'Editing titles or structure size',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_TitlandSize)

          CALL WGAPP(MenuHG,'Problem size limit',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_SizLim)

          CALL WGAPP(MenuHG,'Baffled?',MenuHGN)
          CALL SWGCBK(MenuHGN, Help_Baffled)

      ELSE
C         GoodPAR
C         Parameters have been entered and therefore the dynamic arrays
C         have been allocated, so start to activate menus for the data
C         needing dynamic array storage.
C
C         Materials and Nodes data require only that the arrays have
C         been allocated and do not need any other data to precede them,
C         so show the materials and nodes menus.
C
C         Start by showing the user all data entered so far
C
          CALL List(0)

          CALL DrawSCR
          Mu = I2CHAR(MALLOC1,Nu)
          Ma = I2CHAR(MBYTES,Na)
C         CALL SWGTXT(idSCR,
C    +       'Structure storage allocated : '//Mu(1:Nu)//' bytes.')

C         CALL SWGATT(MenuF_New,    'ACTIVE',   'STATUS')
          CALL SWGATT(MenuF_Save,   'ACTIVE',   'STATUS')
          CALL SWGATT(MenuF_SaveAS, 'ACTIVE',   'STATUS')
          CALL SWGATT(MenuS_Params, 'ACTIVE',   'STATUS')
          CALL SWGATT(MenuS_Matls,  'ACTIVE',   'STATUS')
          CALL SWGATT(MenuS_Nodes,  'ACTIVE',   'STATUS')
          CALL SWGATT(MenuS_Restr,  'ACTIVE',   'STATUS')
C         CALL SWGATT(MenuO_List,   'ACTIVE',   'STATUS')

          IF(.NOT.GoodMAT) THEN
C         GoodPAR only:
C             Parameters have been entered but Materials have not, so
C             member prps and therefore members must remain ghosted
              CALL SWGATT(MenuS_Props, 'INACTIVE', 'STATUS')
              CALL SWGATT(MenuS_Membs, 'INACTIVE', 'STATUS')
              CALL SWGATT(MenuL_Loads, 'INACTIVE', 'STATUS')
              CALL SWGATT(MenuL_Combs, 'INACTIVE', 'STATUS')
              CALL SWGATT(MenuO_Solve, 'INACTIVE', 'STATUS')

              CALL WGAPP(MenuHG,
     +        'Editing titles or structure size',MenuHGN)
              CALL SWGCBK(MenuHGN, Help_TitlandSize)

              CALL WGAPP(MenuHG,
     +        'Entering Materials, Nodes and Supports',MenuHGN)
              CALL SWGCBK(MenuHGN, Help_MatNodSup)

              CALL WGAPP(MenuHG,
     +        'How supports are modelled',MenuHGN)
              CALL SWGCBK(MenuHGN, Help_Supports)

              CALL WGAPP(MenuHG,'Baffled?',MenuHGN)
              CALL SWGCBK(MenuHGN, Help_Baffled)

          ELSE
C             GoodPAR + GoodMAT  +   ?GoodPrp?
C             Parameters and materials have been entered so show also
C             the member props menu but leave the members menu
C             ghosted until after checking that member properties
C             have been entered
              CALL SWGATT(MenuS_Props, 'ACTIVE',   'STATUS')

              IF(.NOT.GoodPrp) THEN
C             GoodPAR, GoodMAT only:
C                 Member properties not yet entered and/or correct, so
C                 the members menu must remain ghosted
                  CALL SWGATT(MenuS_Membs, 'INACTIVE', 'STATUS')
                  CALL SWGATT(MenuO_Solve, 'INACTIVE', 'STATUS')

                  CALL WGAPP(MenuHG,
     +            'Editing titles or structure size',MenuHGN)
                  CALL SWGCBK(MenuHGN, Help_TitlandSize)

                  CALL WGAPP(MenuHG,
     +            'Editing Materials, Nodes and Supports',MenuHGN)
                  CALL SWGCBK(MenuHGN, Help_MatNodSup)

                  CALL WGAPP(MenuHG,
     +            'How supports are modelled',MenuHGN)
                  CALL SWGCBK(MenuHGN, Help_Supports)

                  CALL WGAPP(MenuHG,
     +            'Member properties are needed next',MenuHGN)
                  CALL SWGCBK(MenuHGN, Help_MemberProps)

                  CALL WGAPP(MenuHG,'Output/display options',MenuHGN)
                  CALL SWGCBK(MenuHGN, Help_Output)

                  CALL WGAPP(MenuHG,'Baffled?',MenuHGN)
                  CALL SWGCBK(MenuHGN, Help_Baffled)

              ELSE
C             GoodPAR + GoodMAT + GoodPrp:
C                 With member types entered, now allow member entry also
                  IF(.NOT. (GoodNOD.AND.GoodMEM)) THEN
                      CALL SWGATT(MenuS_Membs, 'ACTIVE',   'STATUS')
                      CALL SWGATT(MenuL_Loads, 'INACTIVE', 'STATUS')
                      CALL SWGATT(MenuL_Combs, 'INACTIVE', 'STATUS')
                      CALL SWGATT(MenuO_Solve, 'INACTIVE', 'STATUS')

                      CALL WGAPP(MenuHG,
     +                'Editing titles or structure size',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_TitlandSize)

                      CALL WGAPP(MenuHG,'Enter/edit the structure',
     +                           MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_StrucTables)

                      CALL WGAPP(MenuHG,
     +                'Editing Materials, Nodes and Supports',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MatNodSup)

                      CALL WGAPP(MenuHG,
     +                'How supports are modelled',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_Supports)

                      CALL WGAPP(MenuHG,
     +                'Editing member properties',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MemberProps)

                      CALL WGAPP(MenuHG,
     +                'Member connections can now be entered',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_ReadyforConnctns)

                      CALL WGAPP(MenuHG,
     +                'The member connections table',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MemberConnctns)

                      CALL WGAPP(MenuHG,
     +                'Member-end releases',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MemberEndReleases)

                     CALL WGAPP(MenuHG,'Output/display options',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_Output)

                      CALL WGAPP(MenuHG,'Baffled?',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_Baffled)

                  ELSE
C                 GoodPAR + GoodMAT + GoodPRP + GoodNOD + GoodMEM:
C                     With the whole structure entered, allow load entry
                      CALL SWGATT(MenuS_Membs,    'ACTIVE',  'STATUS')
                      CALL SWGATT(MenuL_Loads,    'ACTIVE',  'STATUS')
                      CALL SWGATT(MenuL_Combs,    'INACTIVE','STATUS')
                      CALL SWGATT(MenuO_DRWmemthk,'ACTIVE',  'STATUS')
                      CALL SWGATT(MenuO_DRWlines, 'ACTIVE',  'STATUS')

                      CALL WGAPP(MenuHG,
     +                'Editing titles or structure size',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_TitlandSize)

                      CALL WGAPP(MenuHG,
     +                 'Enter/edit geometry/loads',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_StrucandLoads)

                      CALL WGAPP(MenuHG,
     +                'Editing Materials, Nodes and Supports',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MatNodSup)

                      CALL WGAPP(MenuHG,
     +                'How supports are modelled',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_Supports)

                      CALL WGAPP(MenuHG,
     +                'Editing member properties',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MemberProps)

                      CALL WGAPP(MenuHG,
     +                'Editing member connections',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_ReadyforConnctns)

                      CALL WGAPP(MenuHG,
     +                'The member connections table',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MemberConnctns)

                      CALL WGAPP(MenuHG,
     +                'Member-end releases',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_MemberEndReleases)

                      CALL WGAPP(MenuHG,
     +                 'Available load types',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_LoadTypes)

                     CALL WGAPP(MenuHG,'Output/display options',MenuHGN)
                      CALL SWGCBK(MenuHGN, Help_Output)

                      IF(GoodLOD) THEN
                            CALL SWGATT(MenuL_Combs, 'ACTIVE', 'STATUS')
                            CALL SWGATT(MenuO_Solve, 'ACTIVE', 'STATUS')

                            CALL WGAPP(MenuHG,'Ready to run',MenuHGN)
                            CALL SWGCBK(MenuHGN, Help_ReadyToRun)
                      ELSE
                            CALL WGAPP(MenuHG,'Baffled?',MenuHGN)
                            CALL SWGCBK(MenuHGN, Help_Baffled)

                      END IF
                  END IF
              END IF
          END IF
      END IF !.NOT.GoodPAR

      IF(.NOT.SOLVED) THEN
          IF(.NOT. GoodPAR) CALL SPLASH
      END IF !(.NOT.SOLVED)

      CALL WGFIN !<activate>

      IF (MORE) GO TO 100  !Go around
C     GO TO 100  !Go around

      CALL CloseALL

      END



      SubRoutine FINI
      CALL SENDOK
      RETURN
      END



      SUBROUTINE GhostMenus
C     --------------------
C     jw / 03-04-12    draft
C     jw / 03-04-12  last mod

C     Ghost menus
C     -----------
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

      CALL SWGATT(MenuS_Params,   'INACTIVE','STATUS')
      CALL SWGATT(MenuS_Matls,    'INACTIVE','STATUS')
      CALL SWGATT(MenuS_Nodes,    'INACTIVE','STATUS')
      CALL SWGATT(MenuS_Props,    'INACTIVE','STATUS')
      CALL SWGATT(MenuS_Membs,    'INACTIVE','STATUS')
      CALL SWGATT(MenuS_Restr,    'INACTIVE','STATUS')
      CALL SWGATT(MenuL_Loads,    'INACTIVE','STATUS')
      CALL SWGATT(MenuL_Combs,    'INACTIVE','STATUS')
      CALL SWGATT(MenuO_DRWmemthk,'INACTIVE','STATUS')
      CALL SWGATT(MenuO_DRWlines, 'INACTIVE','STATUS')
      CALL SWGATT(MenuO_Solve,    'INACTIVE','STATUS')

      RETURN
      END
