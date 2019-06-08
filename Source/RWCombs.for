      SUBROUTINE ReadCOMBS(LU,NextLine)
C     ---------------------------------
C     jw / 30-05-12  draft
C     jw / 30-05-12  last revised

C     This subroutine action is as set out below, reading from LU.
C     LU might be a user-defined data file or it might be a scratch
C     file for temp storage whilst the load combination arrays are being
C     re-ALLOCATEd for increased load combination data storage.

C     set iCOMB=1
C     pre-load NextLine with next LComb name
C     START OUTER LOOP
C     Load combinations
C        Read COMB name from NextLine

C        set iLCasC=1
C        pre-load NextLine with next loadcase entry
C        START INNER LOOP
C        Loadcase entry lines
C           Read next loadcase entry from NextLine
C           Read next loadcase entry into NextLine
C           If NextLine non-blank, increment iLCasC
C           If NextLine is blank, this COMB finished, exit loop
C        Cycle inner loop
C
C        Read next LComb name into NextLine
C        If NextLine non-blank, increment iCOMB
C        If NextLine is blank, all load COMBS finished, jump out of loop
C     Cycle outer loop

      USE LUnits
      USE PARAMS
      USE LOADING

      CHARACTER*80 NextLine

      LOGICAL MoreLComb, MoreLCasC, AcceptMoreLCase,
     +        MoreDOTS, FoundIN, GotData

C     --------------------------
C     FIRST, COUNT THE COMB DATA
C     (if necessary)
C     --------------------------
C     How many load combinations are there?
C     What is the largest number of loadcases in any load combination?
C
C     This is done only if input is from a user input file.
C     It is not done if the input comes from a temp file.
C     Which of these applies depends on whether or not the COMB array
C     has been allocated yet.

      IF(.NOT.ALLOCATED(LOADCOMB)) THEN
C         (Yes, it IS necessary to count the COMBs)

C         Reading combs for the first time.
C         NCOMB needs to be counted.
C         The load comb arrays need to be allocated.

          MaxLCasC=0

C         COUNT NCOMB
          iCOMB=0
          MoreLComb = .TRUE.
          DO WHILE(MoreLComb)
              iCOMB=iCOMB+1

C             Count the number of loadcases, NLCASC, in iCOMB
              iLCasC=0
              IF(LEN_TRIM(NextLine).NE.0) iLCasC = 1
              MoreLCasC =.FALSE.
              READ(LU,'(A)') NextLine
              IF(LEN_TRIM(NextLine).GT.0) MoreLCasC =.TRUE.
              DO WHILE(MoreLCasC)
                  iLCasC=iLCasC+1
                  READ(LU,'(A)') NextLine
                  IF(LEN_TRIM(NextLine).EQ.0) MoreLCasC = .FALSE.
              END DO!MoreLCasC
              MaxLCasC=Max(MaxLCasC,iLCasC)

              READ(LU,'(A)') NextLine
              IF(LEN_TRIM(NextLine).EQ.0) MoreLComb = .FALSE.
          END DO!(MoreLComb)

C         As counted:
C         - There are iCOMB load combinbations,
C         - with up to MaxLCasC load cases per combination.

C         Now allocate the load comb. array size for the first time,
C         allocating slightly more space than counted as needed.
          NCOMB  = iCOMB+1
          NLCASC = MAX(MaxLCasC+1,5)
          CALL ALLOC3

C         ..and find the start of the loads again, ready to
C         read the loads into the arrays.
          REWIND(LU)
          MoreLComb =
     +    FoundIN(LU,'COMBINATIONS',GotData,NextLine)
     +       .OR.
     +    FoundIN(LU,'LOAD COMB',GotData,NextLine)
     +       .OR.
     +    FoundIN(LU,'COMBINATION LOAD',GotData,NextLine)
     +       .OR.
     +    FoundIN(LU,'COMBINED LOAD',GotData,NextLine)

      ELSE
C         (No, it is NOT necessary to count the COMBs)

C         Combinations must have been read before, and
C         the program is now just re-reading the combinations
C         after ALLOC3PLUS has reallocated the arrays.
C         NCOMB is already known.
C         The load arrays do not need to be allocated.

C         READ(LU,'(A)', END=999) NextLine
          IF(LEN_TRIM(NextLine).GT.0) MoreLComb = .TRUE.

      END IF !(.NOT.ALLOCATED(COMB))


C     --------------------------
C     NEXT, READ THE COMB DATA:
C     --------------------------

C     INITIALISE FOR FIRST LComb
C     Set iCOMB to 1.
C     Set (AcceptMoreLCase) to .TRUE.
C     The first 'NextLine' has already been read from LU.
C     If (non-blank NextLine) set (MoreLComb) to .TRUE.
      iCOMB = 1
      AcceptMoreLCase = .TRUE.

C     ENTER LComb LOOP
C     Start with Comb name
      DO WHILE(MoreLComb)
C        NEXT COMB
C        MoreLComb will have been set to .FALSE. if NextLine blank.
C        If(MoreLComb) then NextLine WILL contain next LComb name.

         IF(AcceptMoreLCase) CombName(iCOMB)=TRIM(NextLine)
         iLCasC = 1
         MoreLCasC = .TRUE.
         READ(LU,'(A)', END=999) NextLine
         IF(LEN_TRIM(NextLine).EQ.0) MoreLCasC = .FALSE.

C        ENTER LOADCASE LOOP
C        MoreLCasC will be .FALSE. if NextLine is blank
C        If(MoreLCasC) then NextLine WILL contain next load entry
C>
         DO WHILE(MoreLCasC)
C           No need to check no.of load cases is within NLCASC limit
C           because this will have been done when loading the NextLine,
C           at the end of this loop

C           READ LOAD CASE ENTRY IN THE CURRENT LComb
C           FROM THE NextLine
            IF(AcceptMoreLCase) THEN
               LCNum=0
               Factr=0.0

               iComma=INDEX(NextLine,',')
               iEnd  =LEN_TRIM(NextLine)

               READ(NextLine(1:iComma-1),'(I10)') LCNum
               LOADCOMB(iCOMB,iLCasC)%LCNum = LCNum

               READ(NextLine(iComma+1:iEnd),'(F14.6)') Factr
               LOADCOMB(iCOMB,iLCasC)%Factr = Factr

            END IF!(AcceptMoreLCase)

C           ANY MORE Loadcase ENTRIES FOR THIS COMB?

C           READ THE NEXT LOADCASE ENTRY INTO NextLine
C           If non-blank line then check that there's room for it
C           If it's a blank line then that's the end of this LComb
            READ(LU,'(A)', END=999) NextLine
            IF(LEN_TRIM(NextLine).NE.0) THEN
               iLCasC=iLCasC+1
               IF(iLCasC.EQ.NLCASC) THEN
                  CALL Alloc3Plus('NLCASC',25)!More LCases per Comb
C                 -opens a temp file
C                 -Saves all stored LCombNames and COMBS to temp file
C                 -DEALLOCATES LCases and LCombNames
C                 -ALLOCATES COMBS to larger sizes
C                 -Retrieves stored LCombName & COMBS from temp file
C                 -Closes and deletes the temp file
               END IF!(iLCasC.EQ.NLCASC)
            ELSE !Blank NextLine
               MoreLCasC =.FALSE. !No more loads in this LC
            END IF !LEN_TRIM(NextLine).NE.0
         END DO !While(MoreLCasC)

C        ANY MORE LOAD COMBS?
C
C        READ THE NEXT LOADCOMB NAME INTO THE NextLine
C        If non-blank line then check that there's room for it
C        If it's a blank line then that's the end of all Combs
         READ(LU,'(A)', END=999) NextLine
         IF(LEN_TRIM(NextLine).GT.0) THEN !non-blank line
            iCOMB = iCOMB+1
C           Check no.of LCombs is within NCOMB limit
C           If too many then stay in the loop so that the program
C           can reach the end of the COMB data but set (AcceptMoreLCase)
C           to .FALSE. so the program won't read data from the NextLine
            IF(iCOMB.GT.NCOMB) THEN
C              Strip all '.' from COMB name cos used as
C              end marker in subroutine BailOUT
               MoreDots=.TRUE.
               DO jDot=1,LEN_TRIM(NextLine)
                  IF(NextLine(jDot:jDot).EQ.'.') NextLine(jDot:jDot)=','
               END DO
               CALL BailOUT
     +         ('ERROR|'//
     +         'More load combinations than the specified limit.|'//
     +         'Load combination(s) ['//TRIM(NextLine)//'] onwards '//
     +         'will not be used.$' ,
     +         'OPTIONS','YES')
               AcceptMoreLCase = .FALSE.
            END IF !iCOMB.GT.NLCAS
         ELSE !blank line
            MoreLComb = .FALSE.
         END IF !(non-blank line)

      END DO !while MoreLComb

      NCOMB = NCOMB-1
      RETURN


999   CALL BailOUT
     +      ('END OF FILE ERROR|'//
     +       'The data file needs two blank lines at the end to|'//
     +       'tell the program it has reached the end of the data.$' ,
     +       'OPTIONS','YES')

      END



      SUBROUTINE SaveCOMBS(LU)
C     ------------------------
C     jw / 30-05-12  draft
C     jw / 30-05-12  last revised

C     Save all load combination data to LU1

      USE PARAMS
      USE LOADING

      CHARACTER*16 I2CHAR, R2CHAR
      CHARACTER*30 LCombName
      LOGICAL MoreCOMB, MoreLCasC

      iCOMB = 1
      MoreComb = .TRUE.
C     COMB LOOP
      DO WHILE(MoreCOMB)
         LCombName = CombName(iCOMB)

C        LComb NAME
         IF(LEN_TRIM(LCombName).EQ.0) LCombName = '--'
         Write(LU,'(A)') TRIM(LCombName)

         iLCasC = 1
         MoreLcasC = .TRUE.
C        LOAD CASE LOOP
         DO WHILE(MoreLcasC)
            LCNum = LOADCOMB(iCOMB,iLCasC)%LCNum
            IF(LCNum.EQ.0) EXIT

            Factr = LOADCOMB(iCOMB,iLCasC)%Factr
            write(LU,'(A)')
     +      TRIM(ADJUSTL (I2Char(LCNum,IDUM)))//','//
     +      TRIM(ADJUSTL (R2CHAR(Factr,idum,'F16.6 ')))

            iLCasC=iLCasC+1
            IF(iLCasC.GT.NLCASC) EXIT
         END DO !(MoreLcasC)

         WRITE(LU,'(A)') '' !1st blank LCase line ends this LComb

         iCOMB = iCOMB+1
         IF(iCOMB.GT.NCOMB) EXIT
      END DO
      WRITE(LU,'(/)') !2nd blank line after all COMBs

      RETURN
      END




