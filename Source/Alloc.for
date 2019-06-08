      SUBROUTINE ALLOC1
C     -----------------
C     jw / 04-03-86  1st draft started
C     jw / 05-02-87  1st draft completed
C     jw / 18-02-12  last rev.

C     Call history:
C
C         Input from Window pane data-entry form:
C         WinMenus > rPARAMS   > WinPAR    > StorPAR  > ALLOC1
C         Input from data file:
C         WinMenus > FOpen     > ReadInput > ALLOC1
C
C     Actions:
C         ALLOCATEs dynamic array sizes according to
C         the problem size and geometrical configuration.
C         See also SUBROUTINE ALLOC2.


      USE dislin
      USE WINSTUFF
      USE M
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints

      NACTN = 6*NMEMB
      NDOFG = 3*NNODE
      NDOFR = NDOFG-NPRES
   
C     Keep track
      NBYTES = 1

C     MATERIALS PROPS STORAGE
C     1. Allocate
C     REAL MATLS([GAMMA,EMOD,GMOD],IMATL)
C     CHARACTER*24 DESC
      IF(ALLOCATED(MATLS)) DEALLOCATE(MATLS)
      ALLOCATE (MATLS(NMATS))
C     2. Initialise
      MATLS%Gamma = 0.0
      MATLS%EMod  = 0.0
      MATLS%GMod  = 0.0
      MATLS%Desc  = REPEAT(' ',24)
C     3. Keep track
      NBYTES = NBYTES + 3*NMATS*4 + 24*NMATS

C     MEMBER PROPS STORAGE
C     1. Allocate
C     REAL PROPS([AX*4,IZ*4,AY*4, MATL*4, DESC*24],IMTYP)
C     CHARACTER*24 DESC
      IF(ALLOCATED(PROPS)) DEALLOCATE(PROPS)
      ALLOCATE (PROPS(NMTYP))
C     2. Initialise
      PROPS%Matl  = 0
      PROPS%AreaX = 0.0
      PROPS%AreaY = 0.0
      PROPS%InrtZ = 0.0
      PROPS%Desc = REPEAT(' ',24)
C     3. Keep track
      NBYTES = NBYTES + 3*NMTYP*4 + 24*NMTYP

C     NODE COORDS STORAGE
C     1. Allocate
C     REAL NODE([XCOOR,YCOORD],INODE)
      IF(ALLOCATED(NODE)) DEALLOCATE(NODE)
      ALLOCATE (NODE(2,NNODE))
C     2. Initialise
      NODE  = 0.0
C     3. Keep track
      NBYTES = NBYTES + 2*NNODE*4

C     MEMBER CONNECTIONS, TYPES, FIXITIES STORAGE
C     1. Allocate
C     INTEGER MEMBS([INODE1,INODE2,IMTYP,MEMFIX],IMEMB)
      IF(ALLOCATED(MEMB)) DEALLOCATE(MEMB)
      ALLOCATE (MEMB(NMEMB))
C     2. Initialise
      MEMB%Node1  = 0
      MEMB%Node2  = 0
      MEMB%MTyp   = 0
      DO iRel=1,6
          MEMB%Released(iRel) = .FALSE.
      END DO
      DO iStiff=1,21
          MEMB%LStiff(iStiff) = 0.0
          MEMB%GStiff(iStiff) = 0.0
      END DO
      MEMB%StiffsStored=.false.
      MEMB%Desc = ' '
C     3. Keep track
      NBYTES = NBYTES + 10*NMEMB*4 + 42*NMEMB*8 + 24*NMEMB*1

C     SUPPORTS STORAGE
C     1. Allocate
C     INTEGER KFIXD(IDOFG)
      IF(ALLOCATED(KFIXD)) DEALLOCATE(KFIXD)
      ALLOCATE (KFIXD(NNODE*3))
C     3. Keep track
      NBYTES = NBYTES + NNODE*3 * 4

C     SPRINGS STORAGE
C     1. Allocate
C     REAL SPRING(ISPRI)
      IF(ALLOCATED(SPRING)) DEALLOCATE(SPRING)
      ALLOCATE (SPRING(NSPRI))
C     2. Initialise
      SPRING%NODE  = 0
      SPRING%DoF   = 0
      SPRING%Stiff = 0.0
C     3. Keep track
      NBYTES = NBYTES + NSPRI*4

C     SKYLINE DIAGONALS ARRAY STORAGE
C     NB Only possible to allocate this memory after running SkyDIA
C     1. Allocate
C     INTEGER JDIAG(IDOFR)
      IF(ALLOCATED(JDIAG)) DEALLOCATE(JDIAG)
      ALLOCATE (JDIAG(NDOFR))
C     2. Keep track
      NBYTES = NBYTES + NDOFR*4

      MALLOC1 = NBYTES-1

C     See subroutine ALLOC2 for:
C        TYPE(LOADCASE) LOADCASE(NLCAS,NLOAD)

C     See subroutine ALLOC4 for:
C        REAL RSTIF([JDIAG(IDOFR)])
C        REAL ACTNS([ILCAS*IACTN])
C        REAL VECTR([ILCAS*IDOFG])

      RETURN
      END




      SUBROUTINE ALLOC2
C     -----------------
C     jw / 25-05-12  first drafted
C     jw / 05-06-12  last rev.
C
C     ALLOCATE dynamic array sizes for loads

      USE PARAMS
      USE LOADING

C     IF(NLOAD.LT.NNODE/2 .OR. NLOAD.GT.NNODE*NMEMB)
C    +   NLOAD=(NMEMB+NNODE)*2 !Default no. of load entries per loadcase

      IF(ALLOCATED(LOADCASE)) DEALLOCATE(LOADCASE)
      IF(ALLOCATED(LCName)) DEALLOCATE(LCName)

      ALLOCATE(LCName(NLCAS))
      ALLOCATE(LOADCASE(NLCAS,NLOAD))

      DO iLcas=1,NLCAS
         LCName(iLcas)=REPEAT(' ',30)
         DO iLoad=1,NLOAD
            LOADCASE(iLcas,iLoad)%Pos  = 0.0
            LOADCASE(iLcas,iLoad)%Desc = REPEAT(' ',16)
            LOADCASE(iLcas,iLoad)%Axes = ' '
            LOADCASE(iLcas,iLoad)%LTyp = ' '
            LOADCASE(iLcas,iLoad)%TDLshp = ' '
            LOADCASE(iLcas,iLoad)%Fx   = 0.0
            LOADCASE(iLcas,iLoad)%Fy   = 0.0
            LOADCASE(iLcas,iLoad)%Mz   = 0.0
         END DO
      END DO

      ILCAS = 0

      RETURN
      END




      SUBROUTINE ALLOC2PLUS(Which,HowMany)
C     ------------------------------------
C     Open a scratch file
C     Save all stored LCNames and LOADS to scratch file
C     DEALLOCATE LOADS and LCNames

C     ALLOCATES NLCAS and LCNames, or NLOADS, to larger sizes
C     If (Which.EQ.'NLCAS') then increase NLCAS and LCNames by Howmany
C     If (Which.EQ.'NLOAD') then increase NLOAD per loadcase by HowMany%

C     Retrieves stored LCName & LOADS from scratch file
C     Closes and deletes the scratch file

      USE DISLIN
      USE LUnits
      USE FILES
      USE PARAMS
      USE LOADING

      CHARACTER  TempFile*142, Which*5, FirstLine*80
      INTEGER    HowMany
      LOGICAL    EXISTS, INUSE

C     OPEN A TEMPORARY FILE
C     ---------------------
      TempFile = TRIM(F%Path)//'\in$truct.tmp'

C     Check whether the temp file already exists and delete if possible.
      INQUIRE(FILE=TempFile,EXIST=EXISTS,OPENED=INUSE, IOSTAT=IOCODE)
      GO TO 51

50    CALL BailOUT('ERROR:|'//
     +   'Unexplained error, when trying unsuccessfully to open the|'//
     +   'temp file needed whilst increasing memory for the large |'//
     +   'numbers of load entries.$',
     +   'NO-OPTION','NO')

51    IF(INUSE)THEN
         CALL BailOUT('ERROR:|'//
     +   'Temp file storage is needed whilst increasing memory for|'//
     +   'large numbers of load entries, but the temporary file,|'//
     +   'has accidentally been left open and canot be used.$',
     +   'NO-OPTION','NO')
      END IF

      IF(EXISTS .AND. .NOT.INUSE) THEN
         LU1=NextFreeLU()
C        CALL SYSTEM('ERASE '//TempFile//'/F')
         OPEN(UNIT=LU1,
     +        FILE=TempFile,
     +        STATUS='OLD',
     +        ERR=50,
     +        IOSTAT=IOCODE)
         CLOSE(LU1,STATUS='DELETE')
      END IF
      LU1SET=.FALSE.

C     Open the temp file.
      LU1=NextFreeLU()
      OPEN(UNIT=LU1,
     +        FILE=TempFile,
     +        STATUS='NEW',
     +        ERR=50,
     +        IOSTAT=IOCODE)
      LU1SET=.TRUE.

C     SAVE LOAD DATA
C     Write to the temp file all
C     load data entered to date
C     --------------------------
      CALL SaveLOADS(LU1)


C     CLOSE THE TEMP FILE
C     -------------------
      CLOSE(LU1)
      LU1SET=.FALSE.


C     INCREASE THE LOAD FILE STORAGE
C     ------------------------------
      IF(Which.EQ.'NLCAS' .OR. Which.EQ.'BOTH ')
     +NLCAS=NLCAS+HowMany
      IF(Which.EQ.'NLOAD' .OR. Which.EQ.'BOTH ')
     +NLOAD=NINT(FLOAT(NLOAD) * (HowMany))

      CALL ALLOC2

C     RE-OPEN THE TEMP FILE FOR READING
C     ---------------------------------
      LU1=NextFreeLU()
      OPEN(UNIT=LU1,
     +     FILE=TempFile,
     +     STATUS='OLD',
     +     ERR=50,
     +     IOSTAT=IOCODE)
      LU1SET=.TRUE.


C     RESTORE LOAD DATA
C     -----------------
      READ(LU1,'(A)') Firstline
      CALL ReadLOADS(LU1, Firstline)
      NLCAS = NLCAS+1 !(Added because ReadLOADS will have subtracted )
                      !(1 to correct for the end of the counting loop)


C     CLOSE AND DELETE TEMP FILE
C     Finished with the temporary file
C     --------------------------------
C     CLOSE(LU1)
      CLOSE(LU1, STATUS='DELETE')
      LU1SET=.FALSE.

      RETURN
      END




      SUBROUTINE ALLOC3
C     -----------------
C     jw / 25-05-12  first drafted
C     jw / 05-06-12  last rev.
C
C     ALLOCATE dynamic array sizes for load combinations

      USE PARAMS
      USE LOADING

C     IF(NLOAD.LT.NNODE/2 .OR. NLOAD.GT.NNODE*NMEMB)
C    +   NLOAD=(NMEMB+NNODE)*2 !Default no. of load entries per loadcase

      IF(ALLOCATED(LOADCOMB)) DEALLOCATE(LOADCOMB)
      IF(ALLOCATED(CombName)) DEALLOCATE(CombName)

      IF(NLCASC.LT.1) NLCASC=(NCOMB+NLCAS)/2 + 2

      ALLOCATE(CombName(NCOMB))
      ALLOCATE(LOADCOMB(NCOMB,NLCASC))

      DO iCOMB=1,NCOMB
         CombName(iCOMB)=(' ')
         DO iLcasC=1,NLCASC
            LOADCOMB(iCOMB,iLcasC)%LCnum  = 0
            LOADCOMB(iCOMB,iLcasC)%Factr = 0.0
         END DO
      END DO

      iCOMB = 0

      RETURN
      END




      SUBROUTINE ALLOC3PLUS(Which,HowMany)
C     ------------------------------------
C     Open a scratch file
C     Save all stored LCombNames and LOAD COMBINATIONS to scratch file
C     DEALLOCATE LOAD COMBS and LCombNames

C     ALLOCATES NCOMB and LCombNames, or NLCASC, to larger sizes
C     If (Which.EQ.'NCOMB'),  increase NCOMB and LCombNames by Howmany
C     If (Which.EQ.'NLCASC'), increase NLCASC per loadcomb by HowMany%

C     Retrieves stored LCombName & LOAD COMBS from scratch file
C     Closes and deletes the scratch file

      USE DISLIN
      USE LUnits
      USE FILES
      USE PARAMS
      USE LOADING

      CHARACTER  TempFile*142, Which*5, FirstLine*80
      INTEGER    HowMany
      LOGICAL    EXISTS, INUSE

C     OPEN A TEMPORARY FILE
C     ---------------------
      TempFile = TRIM(F%Path)//'\in$truct.tmp'

C     Check whether the temp file already exists and delete if possible.
      INQUIRE(FILE=TempFile,EXIST=EXISTS,OPENED=INUSE, IOSTAT=IOCODE)
      GO TO 51

50    CALL BailOUT('ERROR:|'//
     +   'Unexplained error, when trying unsuccessfully to open the|'//
     +   'temp file needed whilst increasing memory for the large |'//
     +   'numbers of load entries.$',
     +   'NO-OPTION','NO')

51    IF(INUSE)THEN
         CALL BailOUT('ERROR:|'//
     +   'Temp file storage is needed whilst increasing memory for|'//
     +   'large numbers of load entries, but the temporary file,|'//
     +   'has accidentally been left open and canot be used.$',
     +   'NO-OPTION','NO')
      END IF

      IF(EXISTS .AND. .NOT.INUSE) THEN
         LU1=NextFreeLU()
C        CALL SYSTEM('ERASE '//TempFile//'/F')
         OPEN(UNIT=LU1,
     +        FILE=TempFile,
     +        STATUS='OLD',
     +        ERR=50,
     +        IOSTAT=IOCODE)
         CLOSE(LU1,STATUS='DELETE')
      END IF
      LU1SET=.FALSE.

C     Open the temp file.
      LU1=NextFreeLU()
      OPEN(UNIT=LU1,
     +        FILE=TempFile,
     +        STATUS='NEW',
     +        ERR=50,
     +        IOSTAT=IOCODE)
      LU1SET=.TRUE.

C     SAVE LOAD DATA
C     Write to the temp file all
C     load data entered to date
C     --------------------------
      CALL SaveCOMBS(LU1)


C     CLOSE THE TEMP FILE
C     -------------------
      CLOSE(LU1)
      LU1SET=.FALSE.


C     INCREASE THE COMBINATION FILE STORAGE
C     -------------------------------------
      IF(Which.EQ.'NCOMB' .OR. Which.EQ.'BOTH ')
     +NCOMB=NCOMB+HowMany
      IF(Which.EQ.'NLCASC' .OR. Which.EQ.'BOTH ')
     +NLCASC=NINT(FLOAT(NLCASC) * (HowMany))

      CALL ALLOC3

C     RE-OPEN THE TEMP FILE FOR READING
C     ---------------------------------
      LU1=NextFreeLU()
      OPEN(UNIT=LU1,
     +     FILE=TempFile,
     +     STATUS='OLD',
     +     ERR=50,
     +     IOSTAT=IOCODE)
      LU1SET=.TRUE.


C     RESTORE COMB DATA
C     -----------------
      READ(LU1,'(A)') Firstline
      CALL ReadCOMBS(LU1, Firstline)
      NCOMB = NCOMB+1 !(Added because ReadCOMBS will have subtracted )
                      !(1 to correct for the end of the counting loop)


C     CLOSE AND DELETE TEMP FILE
C     Finished with the temporary file
C     --------------------------------
C     CLOSE(LU1)
      CLOSE(LU1, STATUS='DELETE')
      LU1SET=.FALSE.

      RETURN
      END




      SUBROUTINE ALLOC4
C     -----------------
C     jw / 09-02-87  1st draft
C     jw / 07-09-12  last rev.
C
C     Allocate the remaining pointers for dynamic array storage
C     according to the problem size and geometry.
C     See also SUBROUTINE ALLOC1.
C
C     These pointers could not be allocated in ALLOC1 because:
C      1. ALLOC1 had to be called earlier in order to allocate memory
C         to receive data input and/or generated by GEOM, BOUND, RCODE
C         and SKDIA.
C      2. Pointers for RSTIF not calculable until after calling SKYDIA.
C      3. Value of MLCAS depends upon how much space remains in M()
C         after RSTIF pointers have been allocated.
C      4. Pointers for PRESC, ACTNS, REACT and VECTR not calculable
C         until after MLCAS has been determined.

      USE Params
      USE Restraints
      USE M

C     REAL RSTIF([JDIAG(IDOFR)])
C     - - - - - - - - - - - - -
      IF(ALLOCATED(RSTIF)) DEALLOCATE(RSTIF)
      ALLOCATE (RSTIF(JDND))
      RSTIF=0.0
      NBYTES = NBYTES + JDND * 8

C     REAL PRESC([ILCAS*iPRES])
C     - - - - - - - - - - - - -
      IF(ALLOCATED(PRESC)) DEALLOCATE(PRESC)
      ALLOCATE (PRESC(NLCAS,NPRES))
      PRESC=0.0
      NBYTES = NBYTES + NLCAS*NPRES * 4

C     REAL ACTNS([ILCAS*IMEMB*6])
C     - - - - - - - - - - - - - -
      IF(ALLOCATED(ACTNS)) DEALLOCATE(ACTNS)
      ALLOCATE (ACTNS(NLCAS,NMEMB,6))
      ACTNS=0.0
      NBYTES = NBYTES + NLCAS*NMEMB*6 * 8
C
C     REAL FORCR([ILCAS*(iPRES)])
C     - - - - - - - - - - - - - -
      IF(ALLOCATED(FORCR)) DEALLOCATE(FORCR)
      ALLOCATE (FORCR(NLCAS,NPRES))
      FORCR=0.0
      NBYTES = NBYTES + NLCAS*NPRES * 4
C
C     REAL SREAC([ILCAS*(ISPRI)])
C     - - - - - - - - - - - - - -
      IF(ALLOCATED(SREAC)) DEALLOCATE(SREAC)
      ALLOCATE (SREAC(NLCAS,NSPRI))
      SREAC=0.0
      NBYTES = NBYTES + NLCAS*NSPRI * 4
C
C     REAL REACT([ILCAS*iPRES])
C     - - - - - - - - - - - - -
      IF(ALLOCATED(REACT)) DEALLOCATE(REACT)
      ALLOCATE (REACT(NLCAS,NPRES))
      REACT=0.0
      NBYTES = NBYTES + NLCAS*NPRES * 8

C     REAL VECTR([ILCAS*INODE*IDoF])
C     - - - - - - - - - - - - - - - -
      IF(ALLOCATED(VECTR)) DEALLOCATE(VECTR)
      ALLOCATE (VECTR(NDOFG,NLCAS))
      VECTR=0.0
      NBYTES = NBYTES + NLCAS*NDOFG * 8

C     REAL EQLIB([ILCAS*INODE*IDoF])
C     - - - - - - - - - - - - - - - -
      IF(ALLOCATED(EQLIB)) DEALLOCATE(EQLIB)
      ALLOCATE (EQLIB(NLCAS,NDOFG))
      EQLIB=0.0
      NBYTES = NBYTES + NLCAS*NDOFG * 8

C     REAL EQLIR([2*ILCAS])
C     - - - - - - - - - - - - -
      IF(ALLOCATED(EQLIR)) DEALLOCATE(EQLIR)
      ALLOCATE (EQLIR(2,NLCAS))
      EQLIR=0.0
      NBYTES = NBYTES + 2*8*NLCAS

      RETURN
      END
