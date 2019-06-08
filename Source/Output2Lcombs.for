      SUBROUTINE OutputLCOMBS
C     -----------------------
C     jw / 14-09-14 draft
C     jw / 14-09-14 last rev.
C
C     Output results tables for the loadcase COMBINATIONS.
C
C     This subroutine outputs all deflections, member-end forces and
C     reactions for all load combinations.
C
C     Individual load case results are output by subroutine OUTPUT1.

      USE Dislin
      USE LUnits
      USE Config
      USE RTF
      USE WINSTUFF
      USE Files
      USE Params
      USE Members
      USE Nodes
      USE Restraints
      USE Titles
      USE LOADING
      USE M
      USE RESULTS

      REAL*8  Dflctn(3), MembActn(6)
      INTEGER iTAB

      LOGICAL RSTRD                !Logical function

      LOGICAL FLEXR, NewPg !Logical variables

      CHARACTER OutpTITL*90, OutpCOMB*90,  OutpHdgs*90, OutpData*110
      CHARACTER OutFile*128

      OutpTITL = REPEAT(' ',PRLINW)
      OutpCOMB = OutpTITL
      OutpHdgs = OutpTITL
      OutpData = OutpTITL

      NewPg = .FALSE.


C-----------------------------------------------------------------------
C     LOAD COMBINATIONS
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C     Output the displacements
C-----------------------------------------------------------------------
      CALL NPage(PRLIPP+100,NewPg) ! begin results on a new page
      DO ICOMB=1,NCOMB
C         CAPTIONS --> CONSOLE

          WRITE(OutpTITL,1010)ICOMB, CombName(iCOMB)
1010      FORMAT('DISPLACEMENTS - Load combination',I3, ': ',A)
          CALL SWGTXT(idSCR,TRIM(OutpTITL))        !<- CONSOLE TITLE

          DO ILCASC=1,NLCASC
              LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C             LCName=LCName(LCNum)
              Factr = LOADCOMB(iCOMB,iLCasC)%Factr
              IF(LCNum.EQ.0) EXIT

              WRITE(OutpCOMB,1015)Factr, LCNum, TRIM(LCName(LCNum))
1015          FORMAT(F8.3,'  x  LCase',I3, ': ',A)
              CALL SWGTXT(idSCR,TRIM(OutpCOMB))    !<- CONSOLE COMBlist

          END DO !ILCASC=1,NLCASC

          WRITE(OutpHdgs,1020)
1020      FORMAT('NODE',11X,'d(x)',10X,'d(y)',10X,'r(z)')
          CALL SWGTXT(idSCR,TRIM(OutpHdgs))        !<- CONSOLE Headings

          iTAB1 = INT((FLOAT(PRLINW)-37.0
     +            -FLOAT(LEN_TRIM(CombName(iCOMB))))/2.0)
          iTAB2 = INT((FLOAT(PRLINW)-49.0)/2.0)

          DO INODE=1,NNODE

C            CAPTIONS --> OUTFILE
             IF(ILINE.GT.6) CALL NPage(3+MIN(NNODE-INODE,3),NewPg)
             IF(NewPg .OR. INODE.EQ.1) THEN
                 CALL LF(F%O%LU,4)
                 NewPg = .FALSE.
                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB1)//
     +           Bold$//OutpTITL//unBold$//CR$)      !<- FILE TITLE

                 DO ILCASC=1,NLCASC
                     LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C                    LCName=LCName(LCNum)
                     Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                     IF(LCNum.EQ.0) EXIT
                     WRITE(OutpCOMB,1015)
     +                     Factr, LCNum, TRIM(LCName(LCNum))

                     WRITE(F%O%LU,'(A)')
     +                     TRIM(REPEAT(' ',iTAB1)//
     +                     Small$//TRIM(AdjustL(OutpCOMB))//
     +                     unSmall$//CR$)            !<- FILE COMBlist

                           ILINE=ILINE+1
                 END DO !ILCASC=1,NLCASC

                 WRITE(F%O%LU,'(A)')
     +           CR$//TRIM(REPEAT(' ',iTAB2)//
     +           Bold$//OutpHdgs//unBold$//CR$)      !<- FILE Headings

                 ILINE=ILINE+3
             END IF

C            FOR EACH DoF,
             DO iDoF=1,3
C                Add the results from all COMBs:
                 Dflctn(iDoF) = 0.0
                 DO ILCasC = 1,NLCasC
                     LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
                     IF(LCNum.EQ.0) EXIT
                     Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                     Dflctn(iDoF) = Dflctn(iDoF) +
     +               Factr*VECTR(IDOFN(INODE,iDoF),LCNum)
                 END DO !ILCasC = 1,NLCasC
             END DO !iDoF=1,3

C            Check for any prescribed displacements (support settlemnts)
             DO iDoF=1,3
                NthRes = -KFIXD(IDOFN(INODE,IDOF))
                IF(NthRes.GT.0) THEN  !Supported DoF
                  DO ILCasC = 1,NLCasC
                     LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
                     IF(LCNum.EQ.0) EXIT
                     Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                     IF (ILCasC.EQ.1) THEN  !Substitute presc value
                         Dflctn(iDoF)=
     +                   Factr*PRESC(LCNum,NthRES)
                     ELSE
                         Dflctn(iDoF)=
     +                   Dflctn(iDoF)+Factr*PRESC(LCNum,NthRES)
                     END IF !(ILCasC.EQ.1)
                  END DO !ILCasC = 1,NLCasC
                END IF !(NthRes.GT.0)
             END DO !iDoF=1,3

             WRITE(OutpData,'(I4,4X,3F14.6)')
     +       INODE,(Dflctn(iDoF),IDOF=1,3)

C            RESULTS --> CONSOLE
             CALL SWGTXT(idSCR,TRIM(OutpData))      !<-- CONSOLE Reslts

C            RESULTS --> OUTFILE
             WRITE(F%O%LU,'(A)')
     +       TRIM(REPEAT(' ',iTAB2-1)//OutpData//CR$)!<-- FILE Results

             ILINE=ILINE+1

          END DO !INODE=1,NNODE
          CALL SWGTXT(idSCR,REPEAT('-',81))
      END DO !iCOMB=1,NComb
      CALL SWGTXT(idSCR,' ')

      CALL LF(F%O%LU,2)



C-----------------------------------------------------------------------
C     Output member-end actions
C-----------------------------------------------------------------------
      CALL NPage(PRLIPP+100,NewPg) ! begin results on a new page
      DO ICOMB=1,NComb
C        CAPTIONS --> CONSOLE
         WRITE(OutpTITL,2010)ICOMB, CombName(iCOMB)
2010     FORMAT('MEMBER ACTIONS - Load combination',I3, ': ',A)
         CALL SWGTXT(idSCR,TRIM(OutpTITL))  ! <-- CONSOLE TITLE

          DO ILCASC=1,NLCASC
              LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C             LCName=LCName(LCNum)
              Factr = LOADCOMB(iCOMB,iLCasC)%Factr
              IF(LCNum.EQ.0) EXIT
              WRITE(OutpCOMB,1015)Factr, LCNum, TRIM(LCName(LCNum))
              CALL SWGTXT(idSCR,TRIM(OutpCOMB))    !<- CONSOLE COMBlist
          END DO !ILCASC=1,NLCASC

         WRITE(OutpHdgs,2020)
2020     FORMAT('NODE', 6X, 'M(z)',6X,'F(y)',6X,'F(x)',
     +           5X,
     +          'MEMBER',
     +           5X,
     +          'F(x)',6X,'F(y)',6X,'M(z)',
     +           4X,
     +          'NODE')

         CALL SWGTXT(idSCR,
     +        TRIM(OutpHdgs(1:37)//
     +             OutpHdgs(40:46)//
     +             OutpHdgs(48:82)  ))           ! <-- CONSOLE Headings

         iTAB1 = INT((FLOAT(PRLINW)-38.0
     +            -FLOAT(LEN_TRIM(CombName(iCOMB))))/2.0)
         iTAB2 = INT((FLOAT(PRLINW)-82.0)/2.0)

         LASN1=MEMB(1)%Node2
         LASN2=MEMB(1)%Node1

         DO IMEMB=1,NMEMB

C           CAPTIONS --> OUTFILE
C           IF(ILINE.GT.6) CALL Npage(10,NewPg)
            IF(ILINE.GT.6) CALL Npage(5+MIN(NMEMB-IMEMB,4),NewPg)
            IF(NewPg .OR. IMEMB.EQ.1) THEN
                 CALL LF(F%O%LU,4)
                 NewPg = .FALSE.

                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB1)//
     +           Bold$//OutpTITL//unBold$//CR$)      !<-- FILE TITLE

                 DO ILCASC=1,NLCASC
                     LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C                    LCName=LCName(LCNum)
                     Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                     IF(LCNum.EQ.0) EXIT
                     WRITE(OutpCOMB,1015)
     +                     Factr, LCNum, TRIM(LCName(LCNum))
                     WRITE(F%O%LU,'(A)')
     +                     TRIM(REPEAT(' ',iTAB1)//
     +                     Small$//TRIM(AdjustL(OutpCOMB))//
     +                     unSmall$//CR$)            !<- FILE COMBlist
                           ILINE=ILINE+1
                 END DO !ILCASC=1,NLCASC

                 WRITE(F%O%LU,'(A)')
     +           CR$//TRIM(REPEAT(' ',iTAB2)//
     +           Bold$//OutpHdgs//unBold$//CR$)      !<-- FILE Headgs

                 ILINE=ILINE+4

            END IF!(NewPg .OR. IMEMB.EQ.1)

            NODE1=MEMB(IMEMB)%Node1
            NODE2=MEMB(IMEMB)%Node2

            IF(ILINE.GT.6) CALL Npage(MIN(NMEMB-IMEMB,3),NewPg)
            IF(NewPg .OR. IMEMB.EQ.1) THEN
               NewPg=.FALSE.
            END IF

            IF(NODE1.NE.LASN2 .AND. NODE2.NE.LASN1) CALL LF(F%O%LU,1)
            LASN1=NODE1
            LASN2=NODE2

            DO IACT=1,6
                MembRSLT(iLCas,iMemb,iDoF)=ACTNS(ILCAS,IMEMB,IACT)
            END DO

C           FOR EACH Member DoF,
            DO IACT=1,6
C               Add the results from all COMBs:
                MembActn(IACT) = 0.0
                DO ILCasC = 1,NLCasC
                    LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
                    IF(LCNum.EQ.0) EXIT
                    Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                    MembActn(IACT) = MembActn(IACT) +
     +              Factr*ACTNS(LCNum,IMEMB,IACT)
                END DO !ILCasC = 1,NLCasC
            END DO !IACT=1,6

C           Write a single line of results..
            WRITE(OutpData,
     +            '(I3,F12.3,2F10.3,3X,A12,I6,A12,3X,3F10.3,I6)')
     +            NODE1,
     +            (MembActn(IACT),IACT=3,1,-1),
     +            DarkBlue$//Bold$,IMEMB,UnBold$//Black$,
     +            (MembActn(IACT),IACT=4,6),
     +            NODE2

C           RESULTS --> CONSOLE
            CALL SWGTXT(idSCR,' '//                    !CONSOLE Reslts
     +      TRIM(OutpData(1:4)//
     +           OutpData(6:36)//
     +           OutpData(52:56)//
     +           OutpData(71:105)//' '//
     +           OutpData(106:110)))

C           RESULTS --> OUTFILE
            WRITE(F%O%LU,'(A)')
     +      TRIM(REPEAT(' ',MAX(0,iTAB2-1))//OutpData//CR$)!File Reslts

            ILINE=ILINE+1
         END DO !IMEMB=1,NMEMB

         CONTINUE

         ILINE=ILINE+4

         CALL SWGTXT(idSCR,REPEAT('-',81))
      END DO !iCOMB=1,NComb

      CALL SWGTXT(idSCR,' ')



C-----------------------------------------------------------------------
C     Output support reactions
C-----------------------------------------------------------------------
      iTAB=(PRLINW-49)/2

      CALL NPage(PRLIPP+100,NewPg) ! begin results on a new page
      DO ICOMB=1,NCOMB
         NREAC = NPRES+NSPRI  !Total number of reactions
         JREAC = 0            !Reaction count

C        CAPTIONS --> CONSOLE
         WRITE(OutpTITL,3010)ICOMB, CombName(iCOMB)
3010     FORMAT('REACTIONS - Load combination',I3, ': ',A)
         CALL SWGTXT(idSCR,TRIM(OutpTITL))  ! <-- CONSOLE TITLE

         DO ILCASC=1,NLCASC
            LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C           LCName=LCName(LCNum)
            Factr = LOADCOMB(iCOMB,iLCasC)%Factr
            IF(LCNum.EQ.0) EXIT

            WRITE(OutpCOMB,1015)Factr, LCNum, TRIM(LCName(LCNum))
            CALL SWGTXT(idSCR,TRIM(OutpCOMB))    !<- CONSOLE COMBlist
         END DO !ILCASC=1,NLCASC

         WRITE(OutpHdgs,3020)
3020     FORMAT('NODE',14X,'F(x)',10X,'F(y)',10X,'M(z)')
         CALL SWGTXT(idSCR,TRIM(OutpHdgs))  ! <-- CONSOLE Headings

         iTAB1 = INT((FLOAT(PRLINW)-38.0
     +            -FLOAT(LEN_TRIM(CombName(iCOMB))))/2.0)
         iTAB2 = INT((FLOAT(PRLINW)-52.0)/2.0)

         REACX = 0
         REACY = 0

         iPRES=1
         ISPRI=1

         DO INODE=1,NNODE

             IF(JREAC.EQ.NREAC) EXIT

C            CAPTIONS --> OUTFILE
             IF(ILINE.GT.6)
     +       CALL Npage(MAX(6+INT(0.8*(NPRES+NSPRI)),9),NewPg)
             IF(NewPg .OR. INODE.EQ.1) THEN
                 CALL LF(F%O%LU,4)
                 NewPg = .FALSE.

                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB1)//
     +           Bold$//OutpTITL//unBold$//CR$)        !<-- FILE TITLE

                 DO ILCASC=1,NLCASC
                     LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
C                    LCName=LCName(LCNum)
                     Factr = LOADCOMB(iCOMB,iLCasC)%Factr
                     IF(LCNum.EQ.0) EXIT
                     WRITE(OutpCOMB,1015)
     +                     Factr, LCNum, TRIM(LCName(LCNum))

                     WRITE(F%O%LU,'(A)')
     +                     TRIM(REPEAT(' ',iTAB1)//
     +                     Small$//TRIM(AdjustL(OutpCOMB))//
     +                     unSmall$//CR$)              !<- FILE COMBlist

                           ILINE=ILINE+1
                 END DO !ILCASC=1,NLCASC

                 WRITE(F%O%LU,'(A)')
     +           CR$//TRIM(REPEAT(' ',iTAB2)//
     +           Bold$//OutpHdgs//unBold$//CR$)        !<-- FILE Headgs
                 ILINE=ILINE+3
             END IF

            CALL Npage(6,NewPg)
            IF(.NOT.(NewPg .OR. INODE.EQ.1)) GOTO 3040

C           Check for any restraint or spring reaction(s) at this node
3040        DO 3190 IDOF=1,3
               IF(JREAC.EQ.NREAC) EXIT
               IDOF0=IDOFN(INODE,IDOF)

               IF(RSTRD(IDOF0)
     +             .OR.
     +             FLEXR(IDOF0,SPRINK) ) THEN

C                 This node has one or more restrained or sprung DoF
                  IOUT=IN3D(ILCAS,INODE,1,NNODE,3) -1
                  CALL LF(F%O%LU,1)

                  WRITE(OutpData,'(I4,10X)') INODE
                  iOD=12
                  jOD=80

                  DO IDOFF=1,3
                     IDOF0=IDOFN(INODE,IDOFF)
                     IF(RSTRD(IDOF0) .OR. FLEXR(IDOF0,SPRINK)) THEN
C                       Output a restraint or spring reaction

                        jReac = jReac+1

                        Reaction = 0.0

                        DO iLCasC=1,NLCASC
                            LCNum=LOADCOMB(iCOMB,iLCasC)%LCNum
                            IF(LCNum.EQ.0) EXIT
                            Factr = LOADCOMB(iCOMB,iLCasC)%Factr

                            Reaction =
     +                      Reaction + Factr*Reac(iLCasC,jReac)%RSLT

                        END DO !iLCasC=1,NLCASC

                        WRITE(OutpData(iOD:jOD),3101) Reaction
3101                    FORMAT(F14.4)

                     ELSE
C                       Output a blank field
                        WRITE(OutpData(iOD:jOD),'(14('' ''))')
                     END IF

                     iOD=iOD+14
                  END DO !IDOFF=1,3


C              RESULTS --> CONSOLE
               CALL SWGTXT(idSCR,TRIM(OutpData))     !<-- CONSOLE Reslts

C              RESULTS --> OUTFILE
               WRITE(F%O%LU,'(A)')
     +         TRIM(REPEAT(' ',iTAB2-1)//OutpData)   !<-- FILE Results
               ILINE=ILINE+1

C              Jump out of INODE loop if reactions all done
               IF(iPRES.GT.NPRES .AND. ISPRI.GT.NSPRI) GOTO 3500

C                 Jump out of IDOF loop to prevent repeating the node
                  EXIT

C              ELSE
C                 This node does not have any restrained DoF or springs
               END IF

3190           CONTINUE

         END DO !ICOMB=1,NCOMB
C           ILINE=ILINE+1

3500     ILINE=ILINE+4
         CALL SWGTXT(idSCR,REPEAT('-',81))
      END DO !iCOMB=1,NComb

      RETURN
      END










