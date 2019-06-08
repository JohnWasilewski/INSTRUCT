      SUBROUTINE ReadMEMBS(LU,NextLine)
C     ---------------------------------
C     jw / 16-09-12  draft
C     jw / 16-09-12  last revised

C     READ NODES from LU

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

      CHARACTER   NextLine*80,I2CHAR*8
      CHARACTER*2 ADoF(3)
      CHARACTER   Des*31

      LOGICAL     GoodLine, TTT(6)

      PARAMETER (ADoF = (/'Xx','Yy','Zz'/))

      GoodMem=.TRUE.
      MemLine=0

C     Enter loop repeatedly reading the next line in the file
C     (except the first 'NextLine', which is already read by FoundIN() )
      DO
         MemLine=MemLine+1
         GoodLine=.TRUE.

         IF(MemLine.NE.1) THEN  !NextLine not already read by FoundIN()
             NextLine=''
C            Read next line from input file
             READ(LU,'(A)') NextLine

C            If the line is blank, no further members to read
             IF(LEN_TRIM(NextLine).EQ.0) EXIT
         END IF

         READ(NextLine,*,END=201) IMem,iNode1,iNode2,iMTyp,Des
         GO TO 202
201          CONTINUE ! READ ERR
             IF(Imem*iNode1*iNode2*iMTyp.NE.0) THEN
                 Des=''
                 GO TO 202 ! No ERR, continue
             ELSE
             END IF
202      CONTINUE !No READ ERR
         IF (IMem.LE.0 .OR. IMem.GT.NMEMB) THEN
            CALL SCRmsg('**Error: Invalid member '//
     +      TRIM(I2CHAR(IMem,iDum))//' discarded.',.FALSE.)
            GoodLine = .FALSE.
         END IF

         IF (iNode1.LE.0 .OR. iNode1.GT.NNODE
     +   .OR.
     +   iNode2.LE.0 .OR. iNode2.GT.NNODE) THEN
            CALL SCRmsg('**Error: Invalid end-node(s) in member '//
     +      TRIM(I2CHAR(IMem,iDum))//' - member discarded.',.FALSE.)
            GoodLine = .FALSE.
         END IF

         IF (iMTyp.LE.0 .OR. iMTyp.GT.NMTYP) THEN
            CALL SCRmsg('**Error: Invalid member type for member '//
     +      TRIM(I2CHAR(IMem,iDum))//' - member discarded.',.FALSE.)
            GoodLine = .FALSE.
         END IF

         IF(GoodLine) THEN
C           Accept the line of data read from the file
C           and copy it to array storage
C           ----------------------------------------------
C           Start with member data, excluding the member
C           description and any member-end continuity-code
C           that may be embedded in the description
C           ----------------------------------------------

            MEMB(IMem)%Node1=iNode1
            MEMB(IMem)%Node2=iNode2
            MEMB(IMem)%MTyp=iMTyp

            MEMB(iMem)%StiffsStored = .FALSE.

            IF(LEN_TRIM(Des).GT.0)
     +      Des = NextLine(Index(NextLine,TRIM(Des)):LEN(NextLine))

C           -------------------------------------------
C           Now separate the member-end continuity-code
C           string from the member desciption, if a
C           continuity-code has been entered, and read
C           both the description and continuity code
C           -------------------------------------------
C           Whether or not the six member-end DoF are released is
C           stored in the LOGICAL array MEMB(Imem)%Released(1:6).
C
C           Data entry to this array is in the form of a 6-char
C           string embedded in the member description. The program
C           must first search the member description, Des, to see
C           whether the 6-char member-end release-codes string is
C           present. If it is, the program must then interpret the
C           six characters values as signifying that a member-end
C           release is .TRUE. or .FALSE.
C
C           The 6-char LOGICAL array MEMB(Imem)%Released(1:6)
C           indicates the release status as .TRUE. or .FALSE. for
C           x,y,z at end 1, then x y z at end 2, according to this
C           data-entry notation:
C           ' ',  '-'  or  '1'  = not released (stored as .FALSE.)
C           'r',  'R'  or  '0'  = released     (stored as .TRUE.).
C
C           Examples: '  R  R' or  '--r-r-' or '--R-R-'
C           would all put releases on Z(end 1) and Y(end 2)
C
C           0s and 1s mean fixities, not releases, with
C           the notation
C              '0' = not fixed
C              '1' = fixed.
C
C           Thus: '110101' would release Z(end 1) and Y(end 2)

C           Initialise member Imem with full continuity at both ends
            DO iDoF=1,6
               MEMB(Imem)%Released(iDoF)=.FALSE.
            END DO

C           Scan-search member iMem's descriptn, Des, to check
C           whether a 6-digit member-end continuities string of
C           0s, 1s, Rs or rs, dashes or spaces (ie one or more
C           of 'Rr10- ') has been read in as an embedded string
C           somewhere within Des.

C           i0 = start of scan search for continuity-code string
C           I1 = char1 of possible continuity string
C           I6 = char6 of continuity string
C           ii = last char of scan search

            ii=LEN_TRIM(DES)-5
            I1=0
            I6=0
            DO i0 = I6+1,ii

               I1=SCAN(DES(i0:ii),'0rR -1')
               If(I1.EQ.0) EXIT  ! No continuity-code chars found

C              If didn't exit then char I1 found by the
C              scan-search is a possible start point of a
C              6-char continuity code

C              ..so check the next 5 chars
               DO I6 = I1+1, I1+5
                  IF(SCAN(DES(i6:i6),'0rR -1').EQ.0) EXIT !1 char NBG
               END DO

C              Did the last loop complete five cycles?
               IF(I6-I1.EQ.6) THEN
C                 Yes, char I6 completes the 6-char code,

C                 A string of six continuity-code chars has
C                 been found, at DES(I1:I6), so:
C                 - read it,
C                 - remove it from the member description, Des
C                 - jump out from the search loop

C                 Store the member-end continuities string
                  DO iDoF=1,6
                     ii=I1-1 + iDoF
                     MEMB(Imem)%Released(iDoF)=
     +               (SCAN(DES(ii:ii),'0rR').EQ.1)
                  END DO

                  TTT=MEMB(Imem)%Released(1:6)

C                 Having now found a continuities string,
C                 stop searching
                  EXIT !From the loop, DO i0 = I6+1,ii
               ELSE
C                 No, char I6 did not complete the 6-char code
C                 so reset I1 ready to search again
                  I1=0
               END IF

            END DO
C           Finished reading good members data from
C           the data-entry line

C           Store DES without the member-end continuities string
            IF(I1.EQ.0) THEN
               MEMB(IMem)%Desc = TRIM(Des(1:24))
            ELSE
               MEMB(IMem)%Desc = TRIM(Des(1:I1-2))//
     +         Trim(ADJUSTL(Des(I6+1:LEN(Des)))//'        ')
            END IF

         ELSE
            GoodMem=.FALSE.

         END IF !GoodLine
      END DO !repeatedly reading the next line in the file

      DO Imem = 1,NMEMB
         IF ( MEMB(IMem)%Node1 .EQ. 0 .OR.
     +        MEMB(IMem)%Node2 .EQ. 0 .OR.
     +        MEMB(IMem)%MTyp  .EQ. 0 ) THEN
              CALL SCRmsg('**Invalid end-node(s) and/or member type '//
     +        'for member '//TRIM(I2CHAR(Imem,Idum)),.FALSE.)
              GoodMem=.FALSE.
         END IF
      END DO

      IF(.NOT.GoodMem) THEN
          CALL BailOUT('INCOMPLETE DATA:|'//
     +    'More MEMBER CONNECTIONS needed|'//
     +    'in the input data file.$',
     +    'OPTIONS','YES')
      END IF


      RETURN
      END
