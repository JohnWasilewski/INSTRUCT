      SUBROUTINE STRUCT
C     =================
C     jw / 01-06-86 draft
C     jw / 01-05-11 DISLIN added
C     jw / 01-05-11 last corrected

C     Controls flow through the program.  Dynamic storage was formerly
C     handled by pointers to open-ended array M in blank COMMON, with no
C     need to recompile MAIN or subroutines called by it when M storage
C     was altered.  Dynamic storage now uses the additions in F90/F95
C     for allocatable arrays.
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      USE dislin
      USE WINSTUFF
      USE LUnits
      USE Files
      USE RTF
      USE M
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING
      USE Files

      EXTERNAL Help_Finished

      LOGICAL  DECOMP, BAKSUB, FoundIN

C     Write all input data to the results file
C     CALL ListOUT

C     Save all input data in case the user has not done so
      CALL FSave

C     Initialise restraint codes for reduced storage
      kPRES=iPRES ! iPRES counted the restraints found in the input data
      CALL RCODE

C     Now reserve space for arrays, set up pointers
      IF(.NOT. MemRsrvd) THEN

C         Set up the Skyline storage diagonal pointers
          CALL SKYDIA

C         Input all data, setup pointers for skyline storage of
C         stiffnesses, setup restraints codes.
C         This must come after SkyDIA because it needs JDIAG(NFOFR).
          CALL ALLOC4
          MemRsrvd = .TRUE.
      END IF ! (.NOT. MemRsrvd)

C     Now process all loadcases at the same time.

C     First
C     -----
C   * Process all loads and prescribed support displacements,
C   * Calculate & store fixity actions,
C   * Sum fixity actions at the nodes to get the load vector (eqns RHS)
C   * Initialise nodal equilibrium checks vector..
      CALL LoadVec
      IF(.NOT.GoodLOD) THEN
          CALL SENDOK
          RETURN
      END IF !(.NOT.GoodLOD)

C     Set up the structure stiffness coefficients (eqns LHS)..
      CALL STIFF
C     This must come after SkyDIA because it needs JDIAG(NFOFR).

C     Second
C     ------
C   * Solve equations for unknown displacements,
C   * Calculate displacement actions and add them to the
C     fixity actions giving total member-end actions,
C   * Calculate reactions at restraints and sprung supports..

C     NLCAS = NLCAS-1

C..   Solve the equations by triangular decomposition to give the
C     unknown displacements which overlay the load vector storage
      DECOMP = .TRUE.
      BAKSUB = .TRUE.

      CALL SOLVE (DECOMP,BAKSUB)

C     Output the data
C     Call LIST(0)

C..   Output the displacements
      CALL OutputLCASES('DISP')

C..   Calc. displacement actions & add them to the fixity actions.
C     Also add up the reactions at prescribed displacement nodes,
C     overlaying these on the vector that now contains the displace-
C     ments, in place of zeros or known prescribed displacements at
C     the restrained DoF.

      CALL FORCE
C     (Calls ROTMAT DLOCAL KLOCAL DISACT)

C..   Calc. reactions on sprung nodes
      CALL SPREAC

C..   Output the member-end actions (forces & Moments)
      CALL OutputLCASES('FORC')

C..   Output the fixed and sprung reactions.
C     Sprung reactions have to be calculated from the displacements
C     and spring stiffnesses.  Also, it is necessary to do this
C     before calling OutputLCASES a 3rd time for the member-end actions
C     because spring reactions will need to have been added to FORCR()
C     before nodal equilibrium checks are calculated by the third call
C     to this subroutine.

      CALL OutputLCASES('REAC')
      CALL OutputLCOMBS

      iPRES=kPRES ! This is to restore the number of restraints counted
                  ! by iPRES during data input after iPRES has been
                  ! re-used for other purposes.

      CALL RdeCODE

      CALL WGAPP(MenuHG,'Examine the results',MenuHGN)
      CALL SWGCBK(MenuHGN, Help_Finished)

      WRITE(F%O%LU,'(A)') LF$//'}' !Completes the .RTF output
C     CALL FLUSH (F%O%LU)

      CALL MSGBOX('ANALYSIS COMPLETE||'//
     +
     + 'You can now scroll back through the input data and analysis '//
     + 'results. There should be a formatted, paged, MSWord-'//
     + 'readable copy of all of the above in file '//
     +  TRIM(F%Path)//TRIM(F%NAME)//'.'//TRIM(F%O%EXT)//
     + '.||'//
     +
     + 'Please check that all equilibrium errors are small '//
     + 'and check very carefully that all results make sense.')

C     Enable editing of all parts of the input data
      GoodPAR = .TRUE.
      GoodMAT = .TRUE.
      GoodPrp = .TRUE.
      GoodNOD = .TRUE.
      GoodMEM = .TRUE.
      GoodLOD = .TRUE.

      MORE = .TRUE.

      RETURN
      END
