      PROGRAM INSTRUCT
C     ================
C     jw / 01-06-86  draft
C     jw / 28-12-11  last rev
C     A program to analyse rigid-jointed plane frame skeletal
C     structures with in-plane loading.
C
C     This is a  self-contained program  in one executable run-file for
C     frame analysis.  All input data and all working storage remain in
C     memory throughout.  This makes for easier programming and faster
C     execution.

C     Contents                                    Page no.
C     --------                                    --------

C     i.   Flowchart through the program              2

C     ii.  Data and variable dictionary               3

C     iii. Dynamic storage pointers dictionary        4

C     iv.  The progam                                 5

C     v.   All subprograms                            6 onwards




C----------------------------------------------------------------------
C Flowchart through the program:
C----------------------------------------------------------------------
C INSTRUCT > WinINIT
C          > WinMENUS                          Main menus and CALLbacks
C                                              in WIndow, 'Win1'
C                > rTITLES
C                     > WinTITL(SHOW)          Nodes data entry table
C
C                         > StorTIT            Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > CloseTUTz           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > rPARAMS
C                     > WinPAR(SHOW)           Nodes data entry table
C
C                         > StorPAR           Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > ClosePAR           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > rMATLS
C                     > WinMAT(SHOW)           Nodes data entry table
C
C                         > StorMATLS          Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > CloseMAT           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > rPROPS
C                     > WinPRP(SHOW)           Nodes data entry table
C
C                         > StorPROPS          Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > ClosePRP           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > rNODES
C                     > WinNOD(SHOW)           Nodes data entry table
C
C                         > StorNODES          Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > CloseNOD           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > rMEMBS
C                     > WinMEM(SHOW)           Members data entry table
C
C                         > StorMEMBS          Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > CloseMEM           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > rRESTR
C                     > WinRES(SHOW)           Nodes data entry table
C
C                         > StorRES            Store onscreen table data
C                                              to ALLOCATed arrays
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > CloseRES           Cancel member changes and
C                                              hide data entry table
C
C                               > SendOK       Let WinMENUS recycle Win1
C                                              with new menu ghosting
C
C                         > List               List all input data
C
C                > FSave
C                > FSaveAs
C                > DrawSCR
C                > rLOADS
C                > STRUCT
C
C          > STRUCT                            Control/execute program
C----------------------------------------------------------------------
C   STRUCT > ALLOC4                   Allocate RSTIF, PRESC, ACTNS,
C                                              FORCR, SREAC, REACT,
C                                              VECTR, EQLIB, EQLIR

C          > RCODE                             Encodes KFIXD()
C          > SKYDIA
C          > STIFF  > MEMSTF > GETMEM          Memb. stiffnsses relative
C                                              to global structure axes
C          > LoadVEC
C          > SOLVE
C          > OUTPUT(1)
C          > FORCE
C          > SPREAC
C          > OUTPUT(2)
C          > OUTPUT(3)
C
C




C   STRUCT > DEFINE > rTITLES                  Input problem titles
C
C                   > rPARAMS                  Input problem parameters
C
C                   > ALLOC1                   Dynam storage pointers
C
C                   > rMATLS                   Input materials data
C                   > rPROPS                   Input member properties
C                   > rNODES                   Input nodal coordinates
C                   > rMEMBS                   Input nodal connections
C
C                   > RESTRD                   Input rigid supports
C                   > SPRUNG                   Input spring data
C
C                   > RCODE                    Set up restraints codes
C
C                   > SKYDIA                   Skyline storage pointers
C
C                   > ALLOC2                   More dynam strge pointrs
C
C          > DIAGRAM                           Character-plot diagram
C
C          > STIFF  > MEMSTF > GETMEM          Memb. stiffnsses relative
C                                              to global structure axes
C                   > ADDMEM                   Add to global struc stiff
C                   > ADDSPR                   Ditto for springs.
C
C          Repeat for each group of loadcases
C          (max. KLCAS loadcases per group)...
C
C          > LOADING > LOADS > DECIFR           Input load data
C                           > LoadTypN         Node load
C                           > LoadTypS         Support settlement
C                               > MEMSTF
C                                    > GETMEM
C                           > GETMEM           Memb props for memb loads
C                               > LoadTypU     Member UDL
C                                    > ROTATE  Transform member axes
C                                    > MemRel  Any member-end releases
C                               > LoadTypP     Member point load
C                                    > ROTATE  Transform member axes
C                                    > MemRel  Any member-end releases
C                           > ROTATE           Transform load axes
C                           > ADDLOD           Store fixity ACTNS, sum
C                                              load VECTRS
C
C          > EXECUT > SOLVE                    Solve globl displ eqatns
C
C                   > OUTPUT(1)                Output displacements
C
C                   > FORCE  > MEMSTF > GETMEM Member stiffneses
C                            > ROTATE          Calc displcmt actns
C                                              and add to fixity actns.
C                                              Sum reactions & transfm
C                                              to globl structure axes.
C                   > SPREAC                   Spring reactions
C
C                            > OUTPUT(2)       Output member-end
C                                              actions.
C
C                            > OUTPUT(3)       Output fixed/sprung DoF
C                                              reactions.
C

C----------------------------------------------------------------------
C  Data and variables dictionary
C----------------------------------------------------------------------

C  Parameters:-
C
C  NMATS               Number of materials
C  NMTYP               Number of member types
C  NNODE               Number of nodes
C  NMEMB               Number of members
C  NPRES               Number of prescribed displacements (ie supports)
C  NSPRI               Number of support springs
C  NDOFG               Gross number of DoF = 3*NNODE
C  NDOFR               Nett ('reduced') number of DoF ( = NDOFG-NPRES)
C                      in the reduced K-matrix
C  MLCAS               Loadcase limit in the available memory
C  NLCAS               Number of loadcases entered
C  KLCAS               Smaller number of loadcases processed in each
C                      pass through LOADING and EXECUT
C
C  MBYTES              Amount of dynamic storage provided.
C                      This must be the same as the DIMENSION
C                      of array M() in program INSTRUCT
C
C  NBYTES              Amount of dynamic storage actually needed
C                      and used by a problem



C  Arrays:-
C
C  I*4 M               Byte array reserved for dynamic storage allocatn
C  R*4 MATLS(3,NMATS)  Material props (E,G, Gamma) for NMATS materials
C  R*4 PROPS(3,NMTYP)  Properties of NMTYP member types
C  R*4 NODE(2,NNODE)   Node coordinates
C  I*4 MEMBS(4,NMEMB)  Member definitions and nodal connectivities
C                      MEMBS(1,IMEMB) = i node
C                      MEMBS(2,IMEMB) = j node
C                      MEMBS(3,IMEMB) = member type
C                      MEMBS(4,IMEMB) = 6-digit end-restraint codes
C  R*4 PRESC(NDOFG     Values of any prescribed displacemts for all DoF
C            * NLCAS)
C  I*4 LOSPR(NSPRI)    DoF locations of any springs
C  I*4 SPRNG(NSPRI)    Spring stiffnesses
C  I*4 JDIAG(NDOFR)    Pointers to diagonal cells in the skyline-storage
C                      of the global stiffness matrix RSTIF
C  I*4 KFIXD(NDOFG)    Restraint codes vector for all DoF
C  R*8 RSTIF(n)        Reduced global structure siffness coefficients
C                      ( (n) depends on the halfband skyline profile)
C
C  R*8 ACTNS(NACTN     Initially contains fixity actions.
C            * NLCAS)  Later, contains fixity actions + displcmt actions
C
C  R*4 FORCR(NPRES     Applied load actions on restrained DoF.
C            * NLCAS)  These are not included in the analysis because
C                      they go straight into the reactions but they need
C                      to be stored so that they can be added to the
C                      reactions by the output routine.
C
C  R*4 SREAC(NPRES     Reactions on sprung DoF.
C            * NLCAS)  These are calculated only at reaction OUTPUT
C                      time but they are needed both in the output
C                      tables and for the nodal equilibrium checks
C                      when calling OUTPUT() for the member actions
C                      tables, so they need to be stored
C
C  R*8 REACT(NPRES     Initially contains fixity reactions.
C            * NLCAS)  Later, fixity reactions + displcmt reactions
C
C  R*8 VECTR(NDOFG     Initially contains NLCAS load vectors.
C            * NLCAS)  Later, contains displcmts at unrestrained DoFs.
C                      (VECTR could also have been used to store
C                      (reactions at the restrained DoFs because)
C                      (the load vectors are no longer required )
C                      (once displacements have been obtained   )
C
C  R*8 EQLIB(NDOFG     NLCAS nodal equilibrium vectors.
C            * NLCAS)
C
C  R*8 EQLIR(2*NLCAS)  NLCAS Horizontal & vertical applied load totals.
C
C      LODID           Load location (node, member no.)
C                      and load type ( (-) = node, (+) = memb )
C
C      ALOAD           Applied load data
C
C                      N.B.  LODID and ALOAD currently not used


C  Pointers:-
C
C  JMATLS              Offset to the start of MATLS() in the M(1) array
C  JPROPS              Offset to the start of PROPS() in the M(1) array
C  JNODES              Offset to the start of NODES() in the M(1) array
C  JMEMBS              Offset to the start of MEMBS() in the M(1) array
C  JPRESC              Offset to the start of PRESC() in the M(1) array
C  JLOSPR              Offset to the start of LOSPR() in the M(1) array
C  JSPRNG              Offset to the start of SPRNG() in the M(1) array
C  JJDIAG              Offset to the start of JDIAG() in the M(1) array
C  JKFIXD              Offset to the start of KFIXD() in the M(1) array
C  JRSTIF              Offset to the start of RSTIF() in the M(1) array
C  JACTNS              Offset to the start of ACTNS() in the M(1) array
C  JVECTR              Offset to the start of VECTR() in the M(1) array
C
C  JLODID              Offset to the start of LODID() in the M(1) array
C  JALOAD              Offset to the start of ALOAD() in the M(1) array
C
C                      JLODID and JALOAD are currently not used
C------------------------------------------------------------------
C And begin (at last)...
C     PROGRAM INSTRUCT
C     Start of program code
C------------------------------------------------------------------

      USE DISLIN
      USE LUnits
      USE Config
      USE WINSTUFF
      USE Files
      USE TITLES
      USE Params

      CHARACTER DateToday*9, DateCompiled*11
C     CHARACTER*11 __DATE__

      EXTERNAL DisplayBanner,
     +         Read_Eng

      PROG8  = 'INSTRUCT'
      VERS4  = '1%27'

      DateCompiled = __DATE__  ! Invokes a macro in the C language
                               ! as long as compiler flag is set.
                               ! Date will be in the form, "Feb 15 2013.

      PDAT9  = DateCompiled(5:6)//' '//
     +         DateCompiled(1:3)//' '//
     +         DateCompiled(10:11)
      DESC24 = 'Plane frame analysis'
      DESC48 = 'Structural analysis of plane frames'
      DESC70 = 'Single self-contained executable program file  '//
     +         'Nothing to install.'
      AUTH16 = 'John Wasilewski'
      FRM32  = 'civil engineer, London, Oxford.'
      TEL1   = '0044-1865-454-170'
      TEL2   = '0044-7900-891-107'
      EML24  = 'John@Wasilewski.co.uk'
      CPRT17 = '(c) 2017 Jmw'

      VERSTR = '$VER: '//PROG8//' ver.'//VERS4//' ('//PDAT9//') '//
     +         'by '//AUTH16//CHAR(0)

      IPAGE  = 0
      ILINE  = 0
      CALL LUinit
C     CALL UNDLIN(LUS,'-')
C     CALL LF(LUS,21)

C     Title screen..
C     CALL BANNER(PROG8, VERS4, DESC24, CPRT17)

      UDat = DateToday()

C     Initialise the total number of stiffness coefficients.
C     This will have a non-zero value only after running SkyDIA
      JDND = 0  ! 'JDND' = JDIAG(NDOFR)

      CALL WinINIT
      CALL WinMENUS
      END
C-----------------------------------------------------------------------



