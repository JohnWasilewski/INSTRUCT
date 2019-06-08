      SUBROUTINE FNew
C     --------------- 
C     jw / 25-07-12  1st draft started
C     jw / 25-07-12  last rev.

C     In any un-saved data then prompt to call FSave or FSaveAS if reqd.
C     CALL ClearAllData
C     CALL GetFileName 

C     If the input file exists already, then warn the user of this,
C     prompt for permission to save it as the new .BAK file and
C     open it as an empty file ready for new data

C     Open the new INPUT file as a NEW file for WRITing on LUn.
C     Leave it open, ready for writing to it.

C     If there's an existing OUTPUT file of the same name, prompt the
C     user for permission to delete it (mentioning that it can be
C     regenerated from the saved input file, delete it if permitted.

C     Open the new OUTPUT file on LUO.
C     Leave it open.

C     CALL rPARAMS ready to receive new frame/problem size parameters
C     Set ChkSUM = 0.0
C     RETURN

      USE Dislin
      USE WinSTUFF
      USE LUnits
      USE Files
      USE M
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING

      LOGICAL CHANGED
      CHARACTER DateToday*9

      INTEGER YN

C     In any un-saved data then prompt to call FSave or FSaveAS as reqd.
      IF(F%Named.AND.CHANGED()) THEN
          YN=1
          CALL DWGBUT
     +   ('You still have unSAVEd data.  SAVE it or lose it.|'//
     +    '  [YES] SAVE before continuing|'//
     +    '  [NO]  Continue without SAVing',YN)
          IF(YN.EQ.1) CALL fSave
      END IF

      CALL CloseALL
      CALL ClearAllData
      UDat = DateToday()

      NMATS = 0
      NMTYP = 0
      NNODE = 0
      NMEMB = 0
      NPRES = 0
      NSPRI = 0
      NLCAS = 0

      IPAGE  = 0
      ILINE  = 0
      CALL LUinit
      CALL WinInit

C     CALL DrawSCR
C     CALL WinSCR('HIDE')
      JDND = 0
      CALL FSave
      MORE = .TRUE.
      CALL SENDOK
      RETURN
      END





      SUBROUTINE FOpen
C     ----------------
C     jw / 25-07-12  1st draft started
C     jw / 25-08-12  last rev.

C     In any un-saved data then prompt to call FSave or FSaveAS if reqd.

C     CALL ClearAllData
C     CALL GetFileName

C     Open the named INPUT file as a READ file on LUI at:
C     TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext).
C     CALL ReadInput(LU)
C     Close LUI

C     Call the function CHANGED() to obtain a new ChkSUM

C     Open the new OUTPUT file as a WRITE file on LUO at:
C     TRIM(F%Path)//TRIM(F%Name)//'.rtf' .
C     Leave LUO open.
C     RETURN

      USE Dislin
      USE LUnits
      USE Files
      USE Params
      USE WinSTUFF
      USE Titles

      CHARACTER*132 InF, OuF, BkF
      CHARACTER*160 ErrPrmt
      CHARACTER DateToday*9, IOMSG*120, I2CHAR*16
      LOGICAL EXISTS, IsOPEN, DELETABLE, SavedIfWanted, CHANGED
      INTEGER YN

C     If any un-saved data then prompt to call FSave or FSaveAS as reqd.
      IF(F%Named.AND.CHANGED()) THEN
          CALL SWGFNT(PrpN%Styl,Pan2W/80)
          CALL SWGWTH(-54)
          ISel=3
          CALL DWGlis
     +   ('You still have unSAVEd data.  SAVE it or lose it.',
     +    'Save|Save-AS|Continue without SAVing|Go back|Quit', ISel)
          IF(ISel.EQ.1) THEN
              CALL fSave
          ELSE IF(ISel.EQ.2) THEN
              CALL fSaveAs
          ELSE IF(ISel.EQ.3)  THEN
              CONTINUE
          ELSE IF(ISel.EQ.4)  THEN
              RETURN
          ELSE
              CALL CloseALL ! (ISel.EQ.4)
          END IF
      END IF
      CALL CloseALL
      CALL ClearAllData
      UDat = DateToday()

      NMATS = 0
      NMTYP = 0
      NNODE = 0
      NMEMB = 0
      NPRES = 0
      NSPRI = 0
      NLCAS = 0

      IPAGE  = 0
      ILINE  = 0

      IF(F%I%OPEN) THEN
          CLOSE(LUI)
          LUISET     = .FALSE.
          F%I%OPEN   = .FALSE.
          F%I%LU     =  0
      END IF

      CALL LUinit

10    CALL GetFileName(.TRUE.)
C         InF = TRIM(F%NAME)//'.'//TRIM(F%I%EXT)
          InF = TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext)
          Li = LEN_TRIM(InF)
C     BAK FILE
          BkF = TRIM(F%NAME)//'.'//'bak'
          Lb = LEN_TRIM(BkF)
C     OUTPUT FILE
          OuF = TRIM(F%NAME)//'.rtf'
          Lo = LEN_TRIM(OuF)

C     INPUT FILE
C     Attempt to open existing file
C     -----------------------------
C     INQUIRE (FILE=TRIM(InF), OPENED=IsOPEN)
C     IF(IsOPEN) CLOSE (LUI, STATUS='KEEP')
      OPEN(UNIT=LUI,
     +     FILE=TRIM(InF),
     +     STATUS='OLD',
     +     ERR=50,
     +     IOSTAT=IOCODE,
     +     IOMSG=IOMSG)

C     File-open succeeded
C     -------------------
C     No ERR, set file-opened flag, read the file and close it
      F%Named    = .TRUE.
      F%I%EXISTS = .TRUE.
      LUISET     = .TRUE.
      F%I%OPEN   = .TRUE.
      F%I%LU     =  LUI

C     Attempt to create and open an empty Output file
C     -----------------------------------------------
      IF(.NOT.EXISTS(TRIM(F%NAME)//'.rtf')
     +   .OR. SavedIfWanted(LU,TRIM(F%NAME)//'.rtf'))
     +    OPEN(UNIT=LUO,
     +         FILE=TRIM(F%NAME)//'.rtf',
     +         STATUS='NEW',
     +         ERR=51,
     +         IOSTAT=IOCODE)

C     File-open succeeded
C     -------------------
C     No ERR, so say so and set the file-opened flag
      LUOSET=.TRUE.
      F%Named    = .TRUE.
      F%O%EXISTS = .TRUE.
      F%O%OPEN   = .TRUE.
      F%O%LU     =  LUO


C     Succesfully opened both an input file
C     and the corresponding output file
C     -------------------------------------
C     Read the input data and close the input file
C     Then initialise the output file
C
      CALL SCRmsg('Reading input data from file '//TRIM(InF)//'.',
     +             .FALSE.)

C     -------------------
      CALL ReadINput(LUI)
C     -------------------

      CLOSE(LUI, STATUS='KEEP',ERR=60,IOSTAT=IOCODE)
      LUISET     = .FALSE.
      F%I%OPEN   = .FALSE.
      F%I%LU     =  0
      CALL SCRmsg('Output file ready for results: '//
     +TRIM(F%NAME)//'.rtf',.FALSE.)

C     All done successfully 
C     ---------------------
C        Call SENDOK to close Win1, allowing WinMENUS
C        to recycle Win1 with menus ghosted differently
         MORE = .TRUE.
         CALL SENDOK
      RETURN


C     Failed to open an input file
C     ----------------------------
50    IF(.NOT.EXISTS(TRIM(InF))) THEN
          ErrPrmt='File '//TRIM(InF)//' not found.'
          CALL PERROR(ErrPrmt)
      ELSE
          ErrPrmt='Unable to open file '//TRIM(InF)//'.'
          CALL PERROR(ErrPrmt)
      END IF
      GO TO 90

C     Failed to open an output file
C     -----------------------------
51    ErrPrmt='Unable to open file '//TRIM(OuF)//'.'
      GO TO 90

C     Failed to close the input file
C     ----------------------------
60    ErrPrmt='Unable to close file '//TRIM(InF)//'.'
      CALL PERROR(ErrPrmt)
C     GO TO 90

C     Error recovery
C     --------------
90    IF(F%I%OPEN) THEN
          CALL ClearAllData
          CLOSE(LUI)
          LUISET     = .FALSE.
          F%I%OPEN   = .FALSE.
          F%I%LU     =  0
      END IF

      IF(F%O%OPEN) THEN
          CLOSE(LUO)
          LUISET     = .FALSE.
          F%I%OPEN   = .FALSE.
          F%I%LU     =  0
      END IF

      YN=1
      CALL DWGBUT
     +  ('ERROR: '//TRIM(ErrPrmt)//'|'//
     +  'IOSTAT error code '//TRIM(ADJUSTL(I2CHAR(IOCODE,iDum)))//'.|'//
     +   TRIM(IOMSG)//'.||'//
     +   'Retry opening an input file?|'//
     +   '  - YES to do so|'//
     +   '  - NO to cancel',YN)
         IF(YN.EQ.0) THEN
             RETURN
         ELSE
             GO TO 10
         END IF
      RETURN
      END





      SUBROUTINE FSave
C     ----------------
C     jw / 25-07-12  1st draft started
C     jw / 25-07-12  last rev.

      USE Dislin
      USE Files
      USE LUnits
      USE WinSTUFF
      USE RTF
      CHARACTER I2Char*16, ErrPrmt*96
      LOGICAL   EXISTS

          If(F%Named) THEN
              LU1=NextFreeLU()

C             If there's already a temp file, try to get rid of it
              iERR = 0
              IF(EXISTS(TRIM(F%NAME)//'.$$$')) THEN
                  CALL UNLINK(TRIM(F%NAME)//'.$$$',iERR)
                  IF(iERR.NE.0) GO TO 20
          END IF

C         Try to open F%NAME.$$$
10        OPEN(UNIT=LU1,
     +         FILE=TRIM(F%NAME)//'.$$$',
     +         STATUS='NEW',
     +         ERR=50,
     +         IOSTAT=IOCODE)

C         Write all input data to the temp file
          CALL WriteInput(LU1) 

C         Try to close the temp file
          CLOSE(LU1, STATUS='KEEP',ERR=60,IOSTAT=IOCODE)

C         Try to delete the existing saved file - if there is one
          iERR = 0
          IF(F%I%EXISTS) THEN
              CLOSE(F%I%LU, STATUS='DELETE',IOSTAT=IOCODE)
              CALL UNLINK(TRIM(F%NAME)//'.'//TRIM(F%I%EXT),iERR)
              IF(iERR.NE.0) GO TO 30
          END IF

C         Try to rename the temp file as the new input file
          iERR = 0
          CALL RENAME(TRIM(F%NAME)//'.$$$',
     +                TRIM(F%NAME)//'.'//F%I%EXT, iERR)
          IF(iERR.NE.0) GO TO 40
          F%I%EXISTS = .TRUE.

C         All done successfully
          RETURN

C         ERROR TRAPS
C         -----------

20        ErrPrmt=
     +    'FILE DELETION ERROR :'//TRIM(I2Char(iERR,iDum))//': '//
     +    'Can''t delete old temp file '//TRIM(F%NAME)//'.$$$'
          GO TO 90

30        ErrPrmt=
     +    'FILE DELETION ERROR :'//TRIM(I2Char(iERR,iDum))//': '//
     +    'Can''t delete prev. input file '//TRIM(F%NAME)//'.'//F%I%EXT
          GO TO 90

40        ErrPrmt=
     +    'FILE RENAME ERROR :'//TRIM(I2Char(iERR,iDum))//': '//
     +    'Can''t rename temp file as new input file '//
     +    TRIM(F%NAME)//'.'//F%I%EXT//'.'
          GO TO 90

50        ErrPrmt=
     +    'FILE OPENING ERROR :'//TRIM(I2Char(IOCODE,iDum))//': '//
     +    'Can''t open temp. file '//TRIM(F%NAME)//'.$$$'
          GO TO 90

60        ErrPrmt=
     +    'FILE CLOSURE ERROR :'//TRIM(I2Char(IOCODE,iDum))//': '//
     +    'Can''t close temp file '//TRIM(F%NAME)//'.$$$'
C         GO TO 90

90        CALL PERROR(ErrPrmt)
          ISel = 2
          CALL SWGWTH(-54)
          CALL SWGFNT(PrpN%Styl,Pan2W/80)
          CALL DWGLIS (TRIM(ErrPrmt), 'Retry|Cancel|Quit', ISel)

          IF(ISel.EQ.1)  THEN
              GO TO 10
          ELSE IF(ISel.EQ.2) THEN
              RETURN
          ELSE !(ISel.EQ.3)
              CALL CloseALL
          END IF

      ELSE
          CALL FSaveAs

      END IF

      RETURN
      END






      SUBROUTINE FSaveAS
C     ------------------
C     jw / 25-07-12  1st draft started
C     jw / 25-07-12  last rev.

C     If(F%OPEN), then close the input file on LUI
C     and set F%Named to .FALSE.

      USE Dislin
      USE Files
      USE WinSTUFF
      LOGICAL CHANGED, EXISTS, SavedIfWanted, DELETABLE
      CHARACTER*96 ErrPrmt

C     In any un-saved data then prompt to call FSave or FSaveAS as reqd.
      IF(F%Named.AND.CHANGED()) THEN
          ISel = 2
          CALL SWGWTH(-54)
          CALL SWGFNT(PrpN%Styl,Pan2W/80)
          CALL DWGlis
     +   ('You still have unSAVEd input data.  SAVE it or lose it.',
     +    'Save|Save-AS|Continue without SAVing|Go back|Quit', ISel)
          IF(ISel.EQ.1) THEN
              CALL fSave
          ELSE IF(ISel.EQ.2) THEN
              CONTINUE
          ELSE IF(ISel.EQ.3)  THEN
              RETURN
          ELSE
              CALL CloseALL ! (ISel.EQ.4)
          END IF
      END IF

10    F%Named = .FALSE.
      F%Name = ''
      F%I%EXT = ''
      F%I%Open = .FALSE.
      F%I%Exists = .FALSE.
      F%I%LU = 0

      IF(F%O%Open) THEN
          CLOSE(LUO)
          F%O%OPEN = .FALSE.
          F%O%Exists = .FALSE.
          F%O%LU = 0
      END IF

C     Continue with the Save-AS
      CALL GetFileName(.TRUE.)

C     Attempt to create and open an empty Output file
C     -----------------------------------------------
      IF(.NOT.EXISTS(TRIM(F%NAME)//'.rtf')
     +   .OR. SavedIfWanted(LU,TRIM(F%NAME)//'.rtf'))
     +    OPEN(UNIT=LUO,
     +         FILE=TRIM(F%NAME)//'.rtf',
     +         STATUS='NEW',
     +         ERR=51,
     +         IOSTAT=IOCODE)

C     File-open succeeded
C     -------------------
C     No ERR, so say so, set the file-opened flag
      F%Named    = .TRUE.
      F%O%EXISTS = .TRUE.
      F%O%OPEN   = .TRUE.
      F%O%LU     = LUO
      CALL SCRmsg('Output file ready for results: '//
     +TRIM(F%NAME)//'.rtf',.FALSE.)

      CALL fSave
      RETURN

C     Failed to open an output file
C     -----------------------------
51    ErrPrmt='Unable to open file '//TRIM(F%NAME)//'.rtf'//'.'
      CALL SWGWTH(-54)
      CALL SWGFNT(PrpN%Styl,Pan2W/80)
      CALL DWGLIS
     +   ('ERROR: '//ErrPrmt, 'Retry|Cancel|Quit', ISel)

      IF(ISel.EQ.1)  THEN
          GO TO 10
      ELSE IF(ISel.EQ.2) THEN
          RETURN
      ELSE !(ISel.EQ.3)
          CALL CloseALL
      END IF

      END






      SUBROUTINE GetFileName(ChangePath)
C     ----------------------------------
C     jw / 25-07-12  1st draft started
C     jw / 15-02-14  last rev.

C     On returning from this subroutine:

C     The INPUT file pathname should be stored in
C     TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext).

C     F%I%Exists should indicate whether the INPUT file already exists.
C     F%I%OPEN should indicate whether it is already open.

C     F%bakExists should indicate whether the a BAK file already exists.
C     (It should not be open).

C     F%O%Exists should indicate whether the OUTPUT file already exists.
C     F%O%OPEN should indicate whether it is already open.

      USE Dislin
      USE Files
      LOGICAL EXISTS, ChangePath

C     IF(F%Named) RETURN

C     Input filename not yet known
      F%Path = REPEAT(' ',128)
      F%Name = REPEAT(' ',96)
      F%I%Exists = .FALSE.
      F%I%OPEN = .FALSE.
      F%BAKexists = .FALSE.
      F%BAKdeletable = .FALSE.

      CALL SWGOPT('STANDARD','DIALOG') !Use STANDARD file dialog boxes
C     Prompt user for a data input Path\filename.ext
      F%Path = ' '
10    CALL DWGfil('Structure DATA filename',F%Path,'*.*')
      CALL SWGOPT('TOP','DIALOG') !Keep all other dialog boxes on top

C     Was a file actually selected or named by the user?
      IF(LEN(F%Path).EQ.0) THEN
C         No.
          CALL BailOut('No input file provided.$', 'OPTIONS', 'YES')
          RETURN
      END IF

      F%Named = .TRUE.

      CALL ParsePath(F%PATH,F%Name,F%I%Ext)
      IF(LEN_TRIM(F%I%Ext).EQ.0) F%I%Ext='ins'
      F%O%Ext = 'rtf'

C     Set the current directory to that of the chosen file
      IF(ChangePath) CALL CHDIR(F%Path,iDone)

C     INPUT FILE

C     Does the named INPUT file already exist?
      F%I%Exists=EXISTS(TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext))
      IF(F%I%Exists) THEN
C         Yes, it exists
C         Is it OPEN?
          INQUIRE
     +    (FILE=TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext),
     +    OPENED=F%I%OPEN)
C         Is there a 'bak' file?
          F%BAKexists = EXISTS(TRIM(F%Path)//TRIM(F%Name)//'.bak')
      END IF

C     OUTPUT FILE

      F%O%Exists = EXISTS(TRIM(F%Path)//TRIM(F%Name)//'.rtf')
      IF(F%I%Exists) THEN
C         Yes, it exists
C         Is it OPEN?
          INQUIRE
     +    (FILE=TRIM(F%Path)//TRIM(F%Name)//'.rtf',
     +    OPENED=F%O%OPEN)
      END IF

      RETURN
      END




      LOGICAL FUNCTION CHANGED()
C     --------------------------
C     jw / 27-07-12  1st draft started
C     jw / 27-07-12  last rev.

C     Add all numeric values stored in data input arrays
C     Compare with the previous total
C     CHANGED = .TRUE. if the sum has changed
C     CHANGED = .FALSE. if no change
C     Store the new total

      USE Files
      USE M
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING

      DOUBLE PRECISION NewSUM, DBLE

      NewSUM = DBLE(NMATS+NMTYP+NNODE+NMEMB+NPRES+NSPRI+NLCAS)

      DO iPrp=1,NMTYP
          NewSUM = NewSUM+DBLE(LEN_TRIM(PROPS(iPrp)%Desc))
          NewSUM = NewSUM+DBLE(PROPS(IPrp)%Matl)
     +               +DBLE(PROPS(IPrp)%AreaX
     +               +PROPS(IPrp)%AreaY
     +               +PROPS(IPrp)%InrtZ)
      END DO

      DO iNode=1,nNode
          NewSUM = NewSUM + DBLE( NODE(1,iNode) + NODE(2,iNode) )
          DO iDoF=1,3
              NewSUM = NewSUM + DBLE(KFIXD(IDOFN(ABS(iNode),iDoF)))
          END DO
      END DO

      DO iSpr=1,NSPRI
          NewSUM=NewSUM+DBLE(SPRING(ISpr)%Node)+DBLE(SPRING(ISpr)%Stiff)
      END DO

      DO IMem=1,NMEMB
          NewSUM = NewSUM + DBLE(MEMB(IMem)%Node1 + MEMB(IMem)%Node2)
          nREL = 0
          DO iREL=1,6
              IF(MEMB(IMem)%Released(iREL)) nREL = nREL + 1
          END DO
          NewSUM = NewSUM +DBLE(nREL)
      END DO

      IF(NLCAS.GT.0) THEN
        DO iLCas = 1,NLCAS
           NewSUM = NewSUM + DBLE(LEN_TRIM(LCName(iLCas)))
           jLoad = 0
           rLoad = 0.0
           DO iLOD=1,NLOAD
             rLoad = rLoad + LOADCASE(iLCas,iLod)%Pos
         IF(SCAN(LOADCASE(iLCas,iLod)%Axes,'Gg').GT.0) jLoad=jLoad + 111
         IF(SCAN(LOADCASE(iLCas,iLod)%Axes,'Ll').GT.0) jLoad=jLoad + 222
         IF(SCAN(LOADCASE(iLCas,iLod)%LTyp,'Ss').GT.0) jLoad=jLoad + 11
         IF(SCAN(LOADCASE(iLCas,iLod)%LTyp,'Nn').GT.0) jLoad=jLoad + 22
         IF(SCAN(LOADCASE(iLCas,iLod)%LTyp,'Uu').GT.0) jLoad=jLoad + 33
         IF(SCAN(LOADCASE(iLCas,iLod)%LTyp,'Dd').GT.0) jLoad=jLoad + 44
         IF(SCAN(LOADCASE(iLCas,iLod)%LTyp,'Pp').GT.0) jLoad=jLoad + 55
             rLoad = rLoad + LOADCASE(ILCas,iLod)%Fx
             rLoad = rLoad + LOADCASE(ILCas,iLod)%Fy
             rLoad = rLoad + LOADCASE(ILCas,iLod)%Mz
           END DO
           NewSUM = NewSUM + DBLE(jLoad) + DBLE(rLoad)
        END DO !iLCas = 1,NLCAS
      END IF !(NLCAS.GT.0)

      IF(NewSUM.NE.ChkSUM) THEN
          CHANGED = .TRUE.
          ChkSUM = NewSUM
      ELSE
          CHANGED = .FALSE.
      END IF

      RETURN
      END




      LOGICAL FUNCTION EXISTS(FilNAM)
C     -------------------------------
      CHARACTER*(*) FilNAM
      EXISTS = .FALSE.
      INQUIRE (FILE=FilNAM, EXIST=EXISTS)
      RETURN
      END




      LOGICAL FUNCTION OPENfile(FilNAM)
C     ---------------------------------
      CHARACTER*(*) FilNAM
      INTEGER*4 BUFF(13), iSTAT
C     OPENfile = .FALSE.
C     INQUIRE (FILE=FilNAM, OPENED=OPENfile)
      LUtest=NextFreeLU()

      CALL LSTAT (FilNAM,BUFF,iSTAT)

      OPEN(UNIT=LUtest,
     +     FILE=FilNAM,
     +     STATUS='NEW',
     +     ERR=20,
     +     IOSTAT=IOCODE)

      CLOSE (LUTest)
      OPENfile = .TRUE.
      RETURN

20    OPENfile = .FALSE.
      RETURN
      END




      FUNCTION FoundIN(LU,STRING,GotData,Line)
C     ----------------------------------------
C     Jmw 08-04-12  First draft
C     Jmw 08-04-12  Last altered

C     Search file LU for a STRING string.
C     LU point to an already-open file, ready to read.
C     If STRING is found in the file, return ready to read the next line

      CHARACTER STRING*(*), LINE*80
      LOGICAL FoundIN, GotData

      REWIND LU
      FoundIN = .TRUE.
      GotData = .TRUE.

      DO
         READ(LU,'(A80)',END=99) LINE
         IF(INDEX(LINE,STRING(1:Len_Trim(String))).NE.0) THEN
            READ(LU,'(A80)',END=97) LINE
            GO TO 98
97          GotData = .FALSE.
            RETURN
98          IF(LEN_TRIM(LINE).EQ.0) GotData = .FALSE.
            RETURN
         END IF
      END DO
      RETURN

C     End of file error
99    REWIND LU
      FoundIN = .FALSE.
      GotData = .FALSE.
      RETURN
      END




      LOGICAL FUNCTION DELETABLE(FilNAM)
C     ----------------------------------
C     jw / 11-08-12  1st draft
C     jw / 11-08-12  last rev.


      CHARACTER*(*) FilNAM
      CHARACTER Fext*3, Prompt*128

      LFN = LEN(FilNAM)
      Fext = FilNAM(LFN-2:LFN)
      Prompt = 'Is it OK to overwrite'

      IF(Fext.EQ.'bak' .OR. Fext.eq.'BAK') THEN
          Prompt = TRIM(Prompt)//
     +    ' the backup of your previous input file: '//FilNAM//' ?'
      ELSE IF(Fext.EQ.'rtf' .OR. Fext.eq.'RTF') THEN
          Prompt = TRIM(Prompt)//
     +    ' your previous output file '//FilNAM//' ?'
      END IF

      YN=1
      CALL DWGBUT(TRIM(Prompt),YN)

      IF(YN.EQ.1) THEN
          DELETABLE = .TRUE.
      ELSE
          DELETABLE = .FALSE.
      END IF !Whether YN to delete

      RETURN
      END




      LOGICAL FUNCTION INFtoBAK()
C     ---------------------------
C     jw / 25-07-12  1st draft started
C     jw / 25-07-12  last rev.
C
C     Attempt to save the current INPUT file as the latest BAK file
C     (overwriting the current BAK file if necessary).
C     Returns .TRUE. if successful.

      USE Files

      CHARACTER*96 InF, BkF, Temp$$
      LOGICAL EXISTS, DELETABLE

      InF = TRIM(F%NAME)//'.'//TRIM(F%I%EXT)
      BkF = TRIM(F%NAME)//'.'//'bak'
      Li  = LEN_TRIM(InF)
      Lb  = LEN_TRIM(BkF)

      iERR = 0
      INFtoBAK = .FALSE.

      IF (F%BAKexists.AND.DELETABLE(TRIM(F%Name)//'.BAK')) THEN
          CALL RENAME(TRIM(BkF), '$$temp', iERR)
          IF(iERR.NE.0) RETURN
          F%BAKexists = .FALSE.
      ELSE
          RETURN
      END IF

      IF(.NOT.F%BAKexists) THEN
          CALL RENAME(TRIM(InF), BkF, iERR)
          IF(iERR.NE.0) THEN
              IF(EXISTS('Temp$$')) CALL RENAME('Temp$$', BkF, iERR)
              F%BAKexists = .TRUE.
              RETURN
          END IF
          IF(EXISTS('Temp$$')) CALL UNLINK('Temp$$',iERR)
          F%BAKexists = .TRUE.
          INFtoBAK = .TRUE.
      END IF

      RETURN
      END




      LOGICAL FUNCTION SavedIfWanted(LU,FNAME)
C     ----------------------------------------
C     jw / 23-08-12  1st draft started
C     jw / 23-08-12  last rev.
C
C     Check whether an existing file is still wanted before re-using
C     the same filename, and prompt for a different name if it is

      USE DISLIN
      USE FILES
      CHARACTER*(*) FNAME
      CHARACTER*96  NewNAME
      CHARACTER EXT*3, fTyp*6, Last4
      INTEGER YN
      LOGICAL OP, OPENfile, EXISTS

      Character(26), Parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      Character(26), Parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

      SavedIfWanted=.FALSE.
      OP = OPENfile(FNAME)! Checks whether or not FNAME is already OPEN
      LFN = (LEN_TRIM(FNAME))
      EXT = FNAME(LFN-2:LFN)

      IF(EXT.EQ.'ins' .OR. EXT.EQ.'INS') THEN
          fTYP = 'Input'
          F%I%Open=OP
      ELSE IF(EXT.EQ.'bak' .OR. EXT.EQ.'BAK') THEN
          fTYP = 'Backup'
      ELSE IF(EXT.EQ.'rtf' .OR. EXT.EQ.'RTF') THEN
          fTYP = 'Output'
          F%O%Open=OP
      ELSE
          fTYP = ''
      END IF

      YN=0
      CALL DWGBUT('WARNING: Current '//TRIM(FTyp)//' file '//
     +             FNAME//' already exists and will '//
     +            'be overwritten by a new file of the same name.||'//
     +            'Is it OK to overwrite it?|'//
     +            '  [Y] to overwrite,|'//
     +            '  [N] to re-name your PREVIOUS '//FNAME//' file.',YN)

      IF(YN.EQ.1) THEN
C         Not needed so zap it
          IF(.NOT.OP) THEN
              LU=NextFreeLU()
              iERR=0
              OPEN(LU, FILE=FNAME, STATUS='OLD',
     +        ACTION='READ', ERR=2, IOSTAT=iERR)
          END IF

          iERR=0
          CLOSE(LU, STATUS='DELETE', ERR=2, IOSTAT=iERR)

2         IF(.NOT.EXISTS(FNAME) .AND. iERR.EQ.0) THEN !Deleted OK
              IF(TRIM(fTYP).EQ.'Input') THEN
                  F%I%OPEN   = .FALSE.
                  F%I%EXISTS = .FALSE.
                  F%I%LU     =  0
              ELSE IF(TRIM(fTYP).EQ.'Output') THEN
                  F%O%OPEN   = .FALSE.
                  F%O%EXISTS = .FALSE.
                  F%O%LU     =  0
              END IF
          ELSE!Error recovery
              CALL BAILOUT('ERROR: Unable to overwrite file '//FNAME//
     +        '.|No file selected.$',
     +        'OPTIONS', 'YES')
          END IF

          IF(TRIM(fTYP).EQ.'Input') THEN
              F%I%Open   = .FALSE.
              F%I%EXISTS = .FALSE.
              F%I%LU     =  0
          ELSE IF(TRIM(fTYP).EQ.'Output') THEN
              F%O%Open   = .FALSE.
              F%O%EXISTS = .FALSE.
              F%O%LU     =  0
          END IF
          SavedifWanted=.TRUE.
          RETURN
      ELSE
C         Try to rename it before returning
          IF(OP) CLOSE(LU, STATUS='KEEP')
C         Prompt user for a new Path\filename.ext
          NewNAME=FNAME
10        CALL DWGfil('Please ENTER (or select) another name for '//
     +    'renaming file '//FNAME, NewNAME, '*.rtf')
          IF(TRIM(fTYP).EQ.'Output') THEN
              jEXT=LEN_TRIM(NewNAME)-3
              Last4 = NewNAME(jEXT:LEN_TRIM(NewNAME))
              DO ij=1,4
                  ic = INDEX(CAP, Last4(ij:ij))
                  if(ic.GT.0) Last4(ij:ij) = LOW(ic:ic)
              END DO
              IF(Last4.NE.'.RTF') NewNAME = Trim(NewNAME)//'.RTF'
          END IF
          IF(EXISTS(TRIM(NewNAME))) THEN
C             Yes, file NewNAME does already exist.
C             Is file NewNAME already open?
              IF(OpenFILE(TRIM(NewNAME))) THEN
C                 NewNAME exists and it is already open
                  YN = 1
                  CALL DWGBUT('ERROR|File '//TRIM(NewNAME)//
     +             ' is in use.|'//
     +             'Try again?||'//
     +             '[YES] to try again.||'//
     +             '[NO]  to cancel without saving the new|'//
     +             '      '//FNAME//' file.',YN)
                  IF(YN.EQ.0) THEN
                      SavedIfWanted = .FALSE.
                      RETURN
                  ELSE
                      GO TO 10
                  END IF
              ELSE
C                 NewNAME exists but is not open
                  YN = 1
                  CALL DWGBUT('WARNING|File '//TRIM(NewNAME)//
     +             ' already exists.|'//
     +             'Is it OK to overwrite it with file '//FNAME//'?||'//
     +             '[YES] to overwrite it.||'//
     +             '[NO]  to re-name file '//FNAME//' '//
     +             'as another file instead.',YN)
                  IF(YN.EQ.0) THEN
                      GO TO 10
                  ELSE
                      CALL RENAME(FNAME, NewNAME, iERR)
                      IF(iERR.EQ.0) THEN
                          SavedIfWanted = .TRUE.
                          RETURN
                      ELSE
                          YN = 1
C                         Yes: OK to overwrite existing file NewNAME
C                         Delete extg file
17                        CALL UNLINK(NewNAME,iERR)
                          IF(iERR.EQ.0) GO TO 30
                          CALL BAILOUT
     +                    ('ERROR: Unable to overwrite file '//NewNAME//
     +                    '.|No file selected.$',
     +                    'OPTIONS', 'YES')
                          GO TO 17
30                        CALL RENAME(FNAME, NewNAME, iERR)
                          IF(iERR.EQ.0) THEN
                              SavedIfWanted = .TRUE.
                              RETURN
                          ELSE
                             YN = 1
                             CALL DWGBUT
     +                       ('ERROR|Unable to rename file '//
     +                       FNAME//' |'//
     +                       'as '//TRIM(NewNAME)//'.||'//
     +                       'Try again?||'//
     +                       '[YES] to try again.||'//
     +                       '[NO]  to cancel without saving the new|'//
     +                       '      '//FNAME//' file.',YN)
                             IF(YN.EQ.0) THEN
                                 SavedIfWanted = .FALSE.
                                 RETURN
                             ELSE
                                 GO TO 10
                             END IF
                          END IF

                          CALL DWGBUT('ERROR|Unable to rename file '//
     +                    FNAME//' |'//
     +                    'as '//TRIM(NewNAME)//'.||'//
     +                    'Try again?||'//
     +                    '[YES] to try again.||'//
     +                    '[NO]  to cancel without saving the new|'//
     +                    '      '//FNAME//' file.',YN)
                          IF(YN.EQ.0) THEN
                              SavedIfWanted = .FALSE.
                              RETURN
                          ELSE
                              GO TO 10
                          END IF
                      END IF
                  END IF

              END IF
          ELSE
C             No, file NewNAME does not yet exist.
              CALL RENAME(FNAME, NewNAME, iERR)
              IF(iERR.EQ.0) THEN
                  SavedIfWanted = .TRUE.
                  RETURN
              ELSE
                  YN = 1
                  CALL DWGBUT('ERROR|Unable to rename file '//
     +            FNAME//' |'//
     +            'as '//TRIM(NewNAME)//'.||'//
     +            'Try again?||'//
     +            '[YES] to try again.||'//
     +            '[NO]  to cancel without saving the new|'//
     +            '      '//FNAME//' file.',YN)
                  IF(YN.EQ.0) THEN
                      SavedIfWanted = .FALSE.
                      RETURN
                  ELSE
                      GO TO 10
                  END IF
              END IF
          END IF
          IF(EXISTS(FNAME))
     +    CALL BAILOUT('ERROR: |Unable to rename file '//FNAME//
     +    '|as '//TRIM(NewNAME)//'.||No file selected.$',
     +    'OPTIONS', 'YES')
      END IF

      RETURN
      END

