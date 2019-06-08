      SUBROUTINE ReadINput(LU)
C     ------------------------
C     jw / 30-10-85  draft
C     jw / 09-08-12  last revised

C     File  TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext) must already
C     be open on LU as a READ file before calling this subroutine.

C     READ all INPUT data from LU

      USE dislin
      USE LUnits
      USE WINSTUFF
      USE FILES
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE RESTRAINTS
      USE LOADING

      CHARACTER   TimeNow*5, FirstLine*80
      CHARACTER*2 ADoF(3)

      LOGICAL     FoundIN, GotData, CHANGED, GD

      PARAMETER (ADoF = (/'Xx','Yy','Zz'/))

C     ===================
C     READ all input data
C     ===================
C     CALL ClearAllData
      SOLVED = .FALSE.

C     TITLES
C     ------
      IF(FoundIN(LU,'TITLES',GotData,FirstLine)) THEN
          READ(FirstLine,'(A)') TITLE1
          READ(LU,'(A)') TITLE2,TITLE3
          READ(LU,'(A)') ENG
          IF(LEN_TRIM(TITLE1)+LEN_TRIM(TITLE2)+LEN_TRIM(TITLE3).GT.0)
     +    THEN
              GoodTIT = .TRUE.
          ELSE
              GoodTIT = .FALSE.
          END IF
      END IF !Found TITLES in the file


C     PARAMETERS
C     ----------
      IF(FoundIN(LU,'PARAMETERS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'PARAMS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'FRAME SIZE',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'PROBLEM SIZE',GotData,FirstLine)) THEN
          IF(GotData) THEN
              CALL ReadPARAMS(LU,FirstLine)
              CALL ALLOC1
          END IF !(GotData)
      END IF !Found PARAMETERS in the file


C     MATERIALS
C     ---------
      IF(FoundIN(LU,'MATERIALS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'MATERIAL PROPERTIES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'ELASTIC MODULI',GotData,FirstLine)) THEN
          IF(GotData) CALL ReadMATLS(LU,FirstLine)
      END IF !Found MATERIALS in the file


C     MEMBER PROPERTIES
C     -----------------
      IF(FoundIN(LU,'MEMBER PROPERTIES',GotData,FirstLine) .OR.
     + FoundIN(LU,'MEMBER TYPES',GotData,FirstLine) .OR.
     + FoundIN(LU,'SECTION PROPERTIES',GotData,FirstLine)) THEN
          IF(GotData) CALL ReadPROPS(LU,FirstLine)
      END IF !Found MEMBER PROPERTIES in the file


C     RIGID RESTRAINTS
C     ----------------
      IF(FoundIN(LU,'RESTRAINTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'SUPPORTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'RIGID RESTRAINTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'RIGID SUPPORTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'REACTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'FIXED REACTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'FIXITIES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'FIXED SUPPORTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'NODAL RESTRAINTS',GotData,FirstLine)) THEN
          IF(GotData) CALL ReadRESTR(LU,FirstLine)
      END IF !Found RIGID RESTRAINTS in the file


C     SPRING RESTRAINTS
C     -----------------
      IF(FoundIN(LU,'SPRINGS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'SPRING SUPPORTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'SPRUNG SUPPORTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'ELASTIC RESTRAINTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'SPRUNG JOINTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'SPRUNG REACTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'SPRING REACTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'ELASTIC REACTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'NODAL SPRINGS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'JOINT SPRINGS',GotData,FirstLine)) THEN
          IF(GotData) CALL ReadSPRI(LU,FirstLine)
      END IF



C     NODES
C     -----
      IF(FoundIN(LU,'NODES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'COORDINATES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'NODE COORDINATES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'COORDINATES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'JOINT COORDINATES',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'JOINTS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'NODE POINTS',GotData,FirstLine)) THEN
         IF(GotData) CALL ReadNODES(LU,FirstLine)
      END IF



C     MEMBERS & CONNECTIONS
C     ---------------------
      IF(FoundIN(LU,'MEMBERS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'CONNECTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'MEMBER CONNECTIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'MEMBER POSITIONS',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'CONNECTIVITY',GotData,FirstLine)
     + .OR.
     + FoundIN(LU,'MEMBER CONNECTIVITY')) THEN
         GD=GotData
         IF(GotData) CALL ReadMEMBS(LU,FirstLine)
      END IF



C     LOADS
C     -----
      IF(FoundIN(LU,'LOADS',GotData,FirstLine) .OR.
     + FoundIN(LU,'LOAD DATA',GotData,FirstLine) .OR.
     + FoundIN(LU,'LOADING',GotData,FirstLine)) THEN

C        Found a LOADS data heading in the input file
         IF(GotData) THEN
C           There is at least one line of data (the first loadcase name)
            CALL ReadLOADS(LU,FirstLine)
            CALL CheckLOADS

         ELSE
C           There's a LOADS data heading but there are no load data
            CALL MSGBOX('WARNING:|'//
     +                  'No LOADS found in the input data file.')
            MORE = .TRUE.
            GOODLOD = .FALSE.
            CALL SENDOK
         END IF !(GotData)

      END IF !Found a LOADS data heading in the input file



C     COMBINATIONS
C     ------------
      IF(FoundIN(LU,'LOAD COMBINATIONS',GotData,FirstLine) .OR.
     + FoundIN(LU,'COMBINATIONS',GotData,FirstLine) .OR.
     + FoundIN(LU,'LOAD CASE COMBINATIONS',GotData,FirstLine) .OR.
     + FoundIN(LU,'COMBINED LOAD CASES',GotData,FirstLine) .OR.
     + FoundIN(LU,'MULTIPLE LOAD CASES',GotData,FirstLine) .OR.
     + FoundIN(LU,'COMBINED LOADS',GotData,FirstLine)) THEN

C        Found a LOAD COMBINATIONS data heading in the input file
         IF(GotData) THEN
C           There is at least one line of data (the first loadcase name)
            CALL ReadCOMBS(LU,FirstLine)
            CALL CheckCOMBS

         ELSE
C           There's a LOAD COMBS data heading but there are no load data
            CALL MSGBOX('WARNING:|'//
     +        'No LOAD COMBINATIONS data found in the input data file.')
            MORE = .TRUE.
            GoodCOMB = .FALSE.
C           CALL SENDOK
         END IF !(GotData)

      END IF !Found a LOAD COMBINATIONS data heading in the input file


      CALL SCRmsg('Input data READ at '//TIMENOW(),.FALSE.)

C     Call function CHANGED() in order to obtain a value for ChkSUM
C     for reference when checking later to see if the data have changed
      IF(CHANGED()) CONTINUE
      RETURN

      END






      SUBROUTINE WriteINput(LU)
C     -------------------------
C     jw / 30-10-85  draft
C     jw / 06-02-12  last revised

C     File  TRIM(F%Path)//TRIM(F%Name)//'.'//TRIM(F%I%Ext)  must already
C     be open on LU as a WRITE file before calling this subroutine.

C     Save all input data from memory to this file then close it.
C     If save-input filename not known, then call SAVE-AS, then return.

      USE dislin
      USE LUnits
      USE WINSTUFF
      USE FILES
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING

      CHARACTER TIMENOW*5,
     +          ADoF*3, ARES*3,
     +          I2CHAR*16, ANUM*12, TxtLN*160, COMMAS*160,
     +          aIF*60

      PARAMETER (ADoF   = ('XYZ'))

C     ========================
C     SAVE all input data here
C     ========================

C     TITLES
C     ------
C     IF(GoodTIT) THEN
          WRITE(LU,'(//''TITLES''3(/A))') TITLE1, TITLE2, TITLE3
          WRITE(LU,'(A)') TRIM(ENG)
          WRITE(LU,'('''')')
C     END IF


C     PARAMETERS
C     ----------
C     IF(GoodPAR) THEN
          WRITE(LU,'(/''PARAMETERS'')')

          ANUM=I2CHAR(MAX(NMATS,NMTYP,NNODE,NMEMB,NPRES,NSPRI),iL1)

          ANUM=I2CHAR(NMATS,IL2)//REPEAT(' ',IL1-IL2)
          WRITE(LU,'(A)') ANUM(1:IL1)//' material types'

          ANUM=I2CHAR(NMTYP,IL2)//REPEAT(' ',IL1-IL2)
          WRITE(LU,'(A)') ANUM(1:IL1)//' member types'

          ANUM=I2CHAR(NNODE,IL2)//REPEAT(' ',IL1-IL2)
          WRITE(LU,'(A)') ANUM(1:IL1)//' nodes'

          ANUM=I2CHAR(NMEMB,IL2)//REPEAT(' ',IL1-IL2)
          WRITE(LU,'(A)') ANUM(1:IL1)//' members'

          ANUM=I2CHAR(NPRES,IL2)//REPEAT(' ',IL1-IL2)
          WRITE(LU,'(A)') ANUM(1:IL1)//' rigid support restraints'

          ANUM=I2CHAR(NSPRI,IL2)//REPEAT(' ',IL1-IL2)
          WRITE(LU,'(A)') ANUM(1:IL1)//' springs'

          WRITE(LU,'('''')')

C     END IF


C     MATERIALS
C     ---------
C     IF(GoodMAT) THEN
          WRITE(LU,'(/''MATERIALS'')')
          DO Imat=1,NMATS
              TxtLN=REPEAT(' ',160)
              Write(TxtLN,'(I12, 3('','',EN12.3),'','',A)')
     +        Imat,
     +        Matls(IMat)%Gamma,
     +        Matls(IMat)%Emod,
     +        Matls(IMat)%Gmod,
     +        Matls(IMat)%Desc
              TxtLN=COMMAS(TRIM(TxtLN))
              WRITE(LU,'(A)') TRIM(TxtLN)
          END DO
          WRITE(LU,'('''')')
C     END IF


C     MEMBER TYPES
C     ------------
C     IF(GoodPRP) THEN
          WRITE(LU,'(/''MEMBER TYPES'')')
          DO IPrp=1,NMTYP
              TxtLN=REPEAT(' ',160)
              Write(TxtLN,'(2(I12,'',''),3(EN12.3,'',''),A)')
     +        IPrp, PROPS(IPrp)%Matl,
     +        PROPS(IPrp)%AreaX, PROPS(IPrp)%AreaY, PROPS(IPrp)%InrtZ,
     +        PROPS(IPrp)%Desc
              TxtLN=COMMAS(TRIM(TxtLN))
              WRITE(LU,'(A)') TRIM(TxtLN)
          END DO
          WRITE(LU,'('''')')
C     END IF


C     NODES
C     -----
C     IF(GoodNOD) THEN
          WRITE(LU,'(/''NODES'')')
          DO INod=1,NNODE
              TxtLN=REPEAT(' ',160)
              WRITE(TxtLN,'(I6,2('','',F10.3))')
     +        INod,NODE(1,Inod),NODE(2,Inod)
              TxtLN=COMMAS(TRIM(TxtLN))
              WRITE(LU,'(A)') TRIM(TxtLN)
          END DO
          WRITE(LU,'('''')')
C     END IF


C     RIGID RESTRAINTS
C     ----------------
C     If RCODE has been called already, it will need to be
C     called again after reading new restraints data, so KFIXD
C     needs to be un-coded back to zeros and ones.
      IF(RCODEd) CALL RdeCODE

C     IF(GoodRES) THEN
         WRITE(LU,'(/''RIGID RESTRAINTS'')')
          iDoFG = 0
          DO INod=1,NNODE
              TxtLN=REPEAT(' ',160)
              ARES='   '
              DO iDoF=1,3
                  iDoFG = iDoFG+1
                  IF(KFIXD(iDoFG).EQ.1)
     +            ARES=TRIM(ARES)//ADoF(iDoF:iDoF)
              END DO !iDoF=1,3
              IF(LEN(TRIM(ARES)).GT.0) THEN
                 WRITE(TxtLN,'(I6,'','',A)') iNod, TRIM(ARES)
                 WRITE(LU,'(A)') ADJUSTL(TxtLN)
              END IF !(LEN(TRIM(ARES)).GT.0)
          END DO !INod=1,NNODE
          WRITE(LU,'('''')')
C     END IF


C     SPRINGS
C     -------
C     IF(GoodRES) THEN
      IF(NSPRI.GT.0) THEN
C     INTEGER Spring(iSpri)%NODE
C     INTEGER Spring(iSpri)%DoF
C     REAL*4  Spring(iSpri)%Stiff
         WRITE(LU,'(/''SPRINGS'')')
         DO iSpri=1,NSpri
            iNode=Spring(iSpri)%NODE
            iDoF=Spring(iSpri)%DoF
            SprStiff=Spring(iSpri)%Stiff
            IF(iNode.GT.0 .AND. iNode.LE.NNODE .AND.
     +         iDoF.GT.0  .AND. iDoF.LE.3 .AND.
     +         SprStiff.NE.0.0) THEN
               WRITE(LU,'(I4,A5,F12.4)')
     +         iNode, ', '//ADof(iDoF:iDoF)//', ', SprStiff
            ELSE
               CALL BAILOUT('Can''t write defective spring at Node '//
     +         TRIM(I2CHAR(Spring(iSpri)%NODE,iDUM))//', '//
     +         'DoF '//TRIM(I2CHAR(Spring(iSpri)%DoF,iDUM))//
     +         '.$','OPTIONS' ,'YES')
            END IF !(Spring(iSpri)%etc)
         END DO !(ISpri=1,NSpri)
      END IF !(NSPRI.GT.0)
C     END IF !(GoodRES)


C     MEMBERS
C     -------
C     IF(GoodMEM) THEN
          WRITE(LU,'(/''MEMBERS'')') 
          DO IMem=1,NMEMB
              TxtLN=REPEAT(' ',160)
              WRITE(TxtLN,'(4(I6,'',''))')
     +        IMem, MEMB(IMem)%Node1,MEMB(IMem)%Node2, MEMB(IMem)%MTyp
              TxtLN=COMMAS(TRIM(TxtLN))

              DO iDoF = 1,6
                  IF(.NOT.MEMB(IMem)%Released(iDoF)) CYCLE
                  !Found a release
                  DO iDr = 1,6
                     TxtLN = TRIM(TxtLN)//
     +               aIF(MEMB(IMem)%Released(iDr),'R$','-$')
                  END DO
                  TxtLN = TRIM(TxtLN)//','
                  EXIT
              END DO

              TxtLN=TRIM(TxtLN)//MEMB(IMem)%Desc

              WRITE(LU,'(A)') TRIM(TxtLN)
          END DO
          WRITE(LU,'('''')')
C     END IF


C     LOADS
C     -----
C     IF(GoodLOD) THEN
      IF(NLOAD.GE.1) THEN
          WRITE(LU,'(/''LOADS'')')
          CALL SaveLOADS(LU)
C-----
C         DO ILCas=1,NLCAS
C             WRITE(LU,'(A)') ADJUSTL(TRIM(LCName(iLCas)))
C             DO iLOD=1,NLOAD
C                 TxtLN=REPEAT(' ',160)
C
C                 IF(LOADCASE(iLCas,iLod)%Pos.EQ.0.0) THEN
C                     EXIT
C
C                 ELSE IF(LOADCASE(iLCas,iLod)%Pos.LT.0.0) THEN
C                     WRITE(TxtLN,'(F12.4)')
C    +                LOADCASE(iLCas,iLod)%Pos
C                 ELSE
C1000                  FORMAT(F10.4,',',2(A1,','),3(F14.6,','),A21)
C                     WRITE(TxtLN,1000)
C    +                  LOADCASE(iLCas,iLod)%Pos,
C    +                  LOADCASE(iLCas,iLod)%Axes,
C    +                  LOADCASE(iLCas,iLod)%LTyp,
C    +                  LOADCASE(iLCas,iLod)%Fx,
C    +                  LOADCASE(iLCas,iLod)%Fy,
C    +                  LOADCASE(iLCas,iLod)%Mz,
C    +                  TRIM(LOADCASE(iLCas,iLod)%Desc)//REPEAT(' ',21)
C                 END IF
C
C                 TxtLN=COMMAS(TRIM(TxtLN))
C
C                 WRITE(LU,'(A)') TRIM(TxtLN)
C             END DO ! (1 to NLOAD)
C             WRITE(LU,'('''')')
C         END DO !(1 to NLCas)
C         WRITE(LU,'('''')')
C-----
      END IF !(NLOAD.GE.1)
C     END IF


C     LOAD COMBINATIONS
C     -----------------
C     IF(GoodCOMB) THEN

      IF(NCOMB.GT.0) THEN
      IF(LOADCOMB(1,1)%LCNum.NE.0) THEN
          WRITE(LU,'(/''LOAD COMBINATIONS'')')
          CALL SaveCOMBS(LU)
C-----
C         DO ICOMB=1,NCOMB
C             WRITE(LU,'(A)') ADJUSTL(TRIM(LCName(LCNum)))
C             LCName=LCName(LCNum)
C             LCNum=LOADCOMB(iCOMB,iLCase)%LCNum
CC             LCName=LCName(LCNum)
C             Factr = LOADCOMB(iCOMB,iLCase)%Factr
C             DO iLOD=1,NLOAD
C                 TxtLN=REPEAT(' ',160)
C
C                 IF(LOADCASE(iLCas,iLod)%Pos.EQ.0.0) THEN
C                     EXIT
C
C                 ELSE IF(LOADCASE(iLCas,iLod)%Pos.LT.0.0) THEN
C                     WRITE(TxtLN,'(F12.4)')
C    +                LOADCASE(iLCas,iLod)%Pos
C                 ELSE
C1000                  FORMAT(F10.4,',',2(A1,','),3(F14.6,','),A21)
C                     WRITE(TxtLN,1000)
C    +                  LOADCASE(iLCas,iLod)%Pos,
C    +                  LOADCASE(iLCas,iLod)%Axes,
C    +                  LOADCASE(iLCas,iLod)%LTyp,
C    +                  LOADCASE(iLCas,iLod)%Fx,
C    +                  LOADCASE(iLCas,iLod)%Fy,
C    +                  LOADCASE(iLCas,iLod)%Mz,
C    +                  TRIM(LOADCASE(iLCas,iLod)%Desc)//REPEAT(' ',21)
C                 END IF
C
C                 TxtLN=COMMAS(TRIM(TxtLN))
C
C                 WRITE(LU,'(A)') TRIM(TxtLN)
C             END DO ! (1 to NLOAD)
C             WRITE(LU,'('''')')
C         END DO !(1 to NLCas)
C         WRITE(LU,'('''')')
C-----
      END IF !(LOADCOMB(1,1)%LCNum.NE.0)
      END IF !(NCOMB.GT.0)

C     END IF


C     CLOSE(LU)
C     LUSET=.FALSE.

      
      WRITE(LU,'(//)') !Two blank lines after all data
      
      CALL SCRmsg('Input data SAVEd at '//TIMENOW(),.FALSE.)

      RETURN


      END
