      SUBROUTINE OutputLcases(ACTION)
C     -------------------------------
C     jw / 19-07-87 draft
C     jw / 27-09-12 last rev.
C
C     Output results tables for the loadcases.
C     Depending on the value of SELECT, this subroutine outputs either
C     the deflections or the member-end forces or the reactions.
C
C     Load COMBinations results are output by subroutine OUTPUT2.
C
C      Before solution:
C        VECTR originally contained the loading vectors.
C        ACTNS originally contained member-end fixity actions.
C
C      After solution:
C        After subroutine SOLVE:
C           VECTR contains displacements only
C           (at restraints these are zeros or other prescribed values).
C           ACTNS, as yet unchanged, still contains fixity actions.
C        After subroutine FORCE:
C           VECTR still contains displacements at unrestrained DoFs.
C
C           ACTNS contains fixity actions + displacement actions.
C
C     Results could have been output directly from within subroutines
C     SOLVE and FORCE, which calculated tables of deflections and
C     forces/reactions, respectively. However, outputting the results
C     separately helps to minimise the amount of code that has to be
C     in memory during the critical equation solution stage, when as
C     much space as possible has to be available for data.
C     This program structure make it possible to use an overlay linker
C     to increase the problem-size capacity of the program.
C
C     Results are not postponed and then output all together after
C     Completing the calculations, because displacements results
C     stored in VECTR locations corresponding to restrained DoF are
C     overlaid by subroutine FORCE to store and accumulate the
C     reactions at restraints. If, by then, displacements had not
C     been printed, then any non-zero prescribed displacements would
C     have been lost.  This procedure avoids having to test for
C     a restraint and then obtain restrained displacements from a
C     different array.

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
      USE M
      USE RESULTS


      CHARACTER*4 ACTION ! ('DISP' or 'FORC' or 'REAC')

      INTEGER ILCAS,INODE
      REAL*4  Dflctn(3)
      INTEGER iTAB

      LOGICAL RSTRD, FLEXR, NewPg

      CHARACTER OutpTITL*90, OutpHdgs*90, OutpData*110

      OutpTITL = REPEAT(' ',PRLINW)
      OutpHdgs = OutpTITL
      OutpData = OutpTITL
      NewPg = .FALSE.


      SELECT CASE(ACTION)

C-----------------------------------------------------------------------
C     LOAD CASES
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Output the displacements
C----------------------------------------------------------------------
      CASE('DISP')

      SOLVED = .TRUE.
      CALL LIST(F%O%LU)
      CALL LIST(0)

      CALL NPage(PRLIPP+100,NewPg) ! begin results on a new page
      DO ILCAS=1,NLCAS

C         CAPTIONS --> CONSOLE
          WRITE(OutpTITL,1010)ILCAS
          WRITE(OutpHdgs,1020)
1010      FORMAT('DISPLACEMENTS - Load case',I3)
1020      FORMAT('NODE',11X,'d(x)',10X,'d(y)',10X,'r(z)')
          CALL SWGTXT(idSCR,TRIM(OutpTITL))  ! <-- CONSOLE TITLE
          CALL SWGTXT(idSCR,TRIM(OutpHdgs))  ! <-- CONSOLE Headings

          iTAB1 = INT((FLOAT(PRLINW)-28.0)/2.0)
          iTAB2 = INT((FLOAT(PRLINW)-49.0)/2.0)

          DO INODE=1,NNODE

C            CAPTIONS --> OUTFILE
             IF(ILINE.GT.6) CALL NPage(3+MIN(NNODE-INODE,3),NewPg)
             IF(NewPg .OR. INODE.EQ.1) THEN
                 CALL LF(F%O%LU,4)
                 NewPg = .FALSE.
                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB1)//
     +           Bold$//OutpTITL//unBold$//CR$//CR$) !<-- FILE TITLE
                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB2)//
     +           Bold$//OutpHdgs//unBold$//CR$)      !<-- FILE Headgs
                 ILINE=ILINE+3
             END IF

             DO iDoF=1,3
                Dflctn(iDoF) = VECTR(IDOFN(INODE,IDOF),ILCAS)!Calc vals
             END DO

C            Check for any prescribed displacements (support settlemnts)
             DO iDoF=1,3
                NthRes = -KFIXD(IDOFN(INODE,IDOF))
                IF(NthRes.GT.0) !Supported DoF
     +          Dflctn(iDoF)=PRESC(ILCAS,NthRES) !Substitute presc value
             END DO

             DO iDoF=1,3
                 DeflRSLT(iLCas,iNode,iDoF) = Dflctn(iDoF)
             END DO

             WRITE(OutpData,'(I4,4X,3F14.6)')
     +       INODE,(Dflctn(iDoF),IDOF=1,3)

C            RESULTS --> CONSOLE
             CALL SWGTXT(idSCR,TRIM(OutpData))  !<-- CONSOLE Reslts

C            RESULTS --> OUTFILE
             WRITE(F%O%LU,'(A)')
     +       TRIM(REPEAT(' ',iTAB2-1)//OutpData//CR$)!<-- FILE Results
             ILINE=ILINE+1

          END DO !INODE=1,NNODE
          CALL SWGTXT(idSCR,REPEAT('-',81))
      END DO !ILCAS=1,NLCAS
      CALL SWGTXT(idSCR,' ')

      CALL LF(F%O%LU,2)
      RETURN



C-----------------------------------------------------------------------
C     Output member-end actions
C-----------------------------------------------------------------------
          CASE('FORC')

      CALL NPage(PRLIPP+100,NewPg) ! begin results on a new page
      DO 2800 ILCAS=1,NLCAS

C        CAPTIONS --> CONSOLE
         WRITE(OutpTITL,2010)ILCAS
         WRITE(OutpHdgs,2020)
2010     FORMAT('MEMBER ACTIONS - Load case',I3)
2020     FORMAT('NODE', 6X, 'M(z)',6X,'F(y)',6X,'F(x)',
     +           5X,
     +          'MEMBER',
     +           5X,
     +          'F(x)',6X,'F(y)',6X,'M(z)',
     +           4X,
     +          'NODE')

         CALL SWGTXT(idSCR,TRIM(OutpTITL))  ! <-- CONSOLE TITLE
         CALL SWGTXT(idSCR,
     +        TRIM(OutpHdgs(1:37)//
     +             OutpHdgs(40:46)//
     +             OutpHdgs(48:82)  ))  ! <-- CONSOLE Headings

         iTAB1 = INT((FLOAT(PRLINW)-29.0)/2.0)
         iTAB2 = INT((FLOAT(PRLINW)-82.0)/2.0)

         LASN1=MEMB(1)%Node2
         LASN2=MEMB(1)%Node1

         DO 2200 IMEMB=1,NMEMB

C            CAPTIONS --> OUTFILE
C            IF(ILINE.GT.6) CALL Npage(10,NewPg)
             IF(ILINE.GT.6) CALL Npage(3+MIN(NMEMB-IMEMB,3),NewPg)
             IF(NewPg .OR. IMEMB.EQ.1) THEN
                 CALL LF(F%O%LU,4)
                 NewPg = .FALSE.
                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB1)//
     +           Bold$//OutpTITL//unBold$//CR$//CR$) !<-- FILE TITLE
                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB2)//
     +           Bold$//OutpHdgs//unBold$//CR$)      !<-- FILE Headgs
                 ILINE=ILINE+3
             END IF

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
                MembRSLT(iLCas,iMemb,IACT)=ACTNS(ILCAS,IMEMB,IACT)
            END DO

C           Write a single line of results..
            WRITE(OutpData,
     +            '(I3,F12.3,2F10.3,3X,A12,I6,A12,3X,3F10.3,I6)')
     +            NODE1,
     +            (ACTNS(ILCAS,IMEMB,IACT),IACT=3,1,-1),
     +            DarkBlue$//Bold$,IMEMB,UnBold$//Black$,
     +            (ACTNS(ILCAS,IMEMB,IACT),IACT=4,6),
     +            NODE2

C           RESULTS --> CONSOLE
            CALL SWGTXT(idSCR,' '//                      !CONSOLE Reslts
     +      TRIM(OutpData(1:4)//
     +           OutpData(6:36)//
     +           OutpData(52:56)//
     +           OutpData(71:105)//' '//
     +           OutpData(106:110)))

C           RESULTS --> OUTFILE
            WRITE(F%O%LU,'(A)')
     +      TRIM(REPEAT(' ',MAX(0,iTAB2-1))//OutpData//CR$)!File Reslts

2200        ILINE=ILINE+1


C        Equilibrium checks
         iTAB=(PRLINW-55)/2
         ERAVGX=0.0
         ERAVGY=0.0
         ERAVGZ=0.0
         ERMAXX=0.0
         ERMAXY=0.0
         ERMAXZ=0.0
         NNMAXX=0
         NNMAXY=0
         NNMAXZ=0

         DO 2300 INODE=1,NNODE

            ERR  = ABS(EQLIB(ILCAS,IDOFN(INODE,1)))
            ERAVGX = ERAVGX+ERR
            IF (ERR.GT.ERMAXX) THEN
               ERMAXX = ERR
               NNMAXX = INODE
               END IF

            ERR  = ABS(EQLIB(ILCAS,IDOFN(INODE,2)))
            ERAVGY = ERAVGY+ERR
            IF (ERR.GT.ERMAXY) THEN
               ERMAXY = ERR
               NNMAXY = INODE
               END IF

            ERR  = ABS(EQLIB(ILCAS,IDOFN(INODE,3)))
            ERAVGZ = ERAVGZ+ERR
            IF (ERR.GT.ERMAXZ) THEN
               ERMAXZ = ERR
               NNMAXZ = INODE
               END IF

2300        CONTINUE

C        Equilibrium checks
C        ------------------
         ERAVGX=ERAVGX/INODE
         ERAVGY=ERAVGY/INODE
         ERAVGZ=ERAVGZ/INODE

         WRITE(OutpHdgs,2410) REPEAT(' ',iTAB)
2410     FORMAT(A,' Node equilibrium  :',6X,'F(x)',8X,'F(y)',8X,'M(z)')

         CALL SWGTXT(idSCR,' ')
         CALL SWGTXT(idSCR,' '//TRIM(OutpHdgs))                     !Con
         WRITE(F%O%LU,'(A)') CR$//TRIM(OutpHdgs//CR$)               !Fil

         ILINE=ILINE+2

C        CALL INDENT(F%O%LU,iTAB)
         WRITE
     +   (OutpData,'(A, I4,''   Max err(x)  :'',F12.5)')
     +    REPEAT(' ',iTAB), NNMAXX,ERMAXX
         CALL SWGTXT(idSCR,' '//TRIM(OutpData))                     !Con
         WRITE(F%O%LU,'(A)')TRIM(OutpData//CR$)                     !Fil

C        CALL INDENT(F%O%LU,iTAB)
         WRITE
     +   (OutpData,'(A, I4,''   Max err(y)  :'',12X,F12.5)')
     +    REPEAT(' ',iTAB), NNMAXY,ERMAXY
         CALL SWGTXT(idSCR,' '//TRIM(OutpData))                     !Con
         WRITE(F%O%LU,'(A)')TRIM(OutpData//CR$)                     !Fil

C        CALL INDENT(F%O%LU,iTAB)
         WRITE
     +   (OutpData,'(A, I4,''   Max err(z)  :'',24X,F12.5)')
     +    REPEAT(' ',iTAB), NNMAXZ,ERMAXZ
         CALL SWGTXT(idSCR,' '//TRIM(OutpData))                     !Con
         WRITE(F%O%LU,'(A)')TRIM(OutpData//CR$)                     !Fil

C        CALL INDENT(F%O%LU,iTAB)
         WRITE
     +   (OutpData,'(A,3F12.5)')
     +   REPEAT(' ',iTAB)//' Mean nodal errors :',ERAVGX,ERAVGY,ERAVGZ
         CALL SWGTXT(idSCR,' '//TRIM(OutpData))                     !Con
         WRITE(F%O%LU,'(A)')TRIM(OutpData//CR$)                     !Fil

         ILINE=ILINE+4

         CALL SWGTXT(idSCR,REPEAT('-',81))
2800     CONTINUE
      CALL SWGTXT(idSCR,' ')

      RETURN



C-----------------------------------------------------------------------
C     Output the support reactions
C-----------------------------------------------------------------------
          CASE('REAC')

      iTAB=(PRLINW-49)/2
      CALL NPage(PRLIPP+100,NewPg) ! begin results on a new page
      DO 3900 ILCAS=1,NLCAS
         NREAC = NPRES+NSPRI  !Total number of reactions
         JREAC = 0            !Reaction count

C        CAPTIONS --> CONSOLE
         WRITE(OutpTITL,3010)ILCAS
         WRITE(OutpHdgs,3020)
3010     FORMAT('REACTIONS - Load case',I3)
3020     FORMAT('NODE',14X,'F(x)',10X,'F(y)',10X,'M(z)')
         CALL SWGTXT(idSCR,TRIM(OutpTITL))  ! <-- CONSOLE TITLE
         CALL SWGTXT(idSCR,TRIM(OutpHdgs))  ! <-- CONSOLE Headings

         iTAB1 = INT((FLOAT(PRLINW)-24.0)/2.0)
         iTAB2 = INT((FLOAT(PRLINW)-52.0)/2.0)

         REACX = 0
         REACY = 0

         iPRES=1
         ISPRI=1

         DO 3200 INODE=1,NNODE

             IF(JREAC.EQ.NREAC) EXIT

C            CAPTIONS --> OUTFILE
             IF(ILINE.GT.6)
     +       CALL Npage(MAX(6+INT(0.8*(NPRES+NSPRI)),9),NewPg)
             IF(NewPg .OR. INODE.EQ.1) THEN
                 CALL LF(F%O%LU,4)
                 NewPg = .FALSE.

                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB1)//
     +           Bold$//OutpTITL//unBold$//CR$//CR$)    !<-- FILE TITLE
                 WRITE(F%O%LU,'(A)')
     +           TRIM(REPEAT(' ',iTAB2)//
     +           Bold$//OutpHdgs//unBold$//CR$)         !<-- FILE Headgs
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
                     IF(RSTRD(IDOF0)) THEN
C                       Output a restraint reaction

                        IREAC=-KFIXD(IDOF0)

                        jReac = jReac+1
                        Reac(iLCas,jReac)%iDoF = IDOFF
                        Reac(iLCas,jReac)%iNod = INODE
                        Reac(iLCas,jReac)%RSLT =
     +                  REACT(ILCAS,IREAC)-FORCR(ILCAS,IREAC)

                        WRITE(OutpData(iOD:jOD),3101)
     +                  Reac(iLCas,jReac)%RSLT
3101                    FORMAT(F14.4)
                        iOD=iOD+14
                        iPRES=iPRES+1

                        IF(IDOFF.EQ.1)
     +                 REACX=REACX+REACT(ILCAS,IREAC)-FORCR(ILCAS,IREAC)

                        IF(IDOFF.EQ.2)
     +                 REACY=REACY+REACT(ILCAS,IREAC)-FORCR(ILCAS,IREAC)

                     ELSE IF (FLEXR(IDOF0,SPRINK)) THEN
C                       Output a spring reaction
                        jReac = jReac+1
                        Reac(iLCas,jReac)%iDoF = IDOFF
                        Reac(iLCas,jReac)%iNod = INODE
                        Reac(iLCas,jReac)%RSLT = SREAC(ILCAS,ISPRI)

                        WRITE(OutpData(iOD:jOD),3101)
     +                  SREAC(ILCAS,ISPRI)
                        iOD=iOD+14
                        IF(IDOFF.EQ.1) REACX=REACX+SREAC(ILCAS,ISPRI)
                        IF(IDOFF.EQ.2) REACY=REACY+SREAC(ILCAS,ISPRI)
                        ISPRI=ISPRI+1

                     ELSE
C                       Output a blank field
                        WRITE(OutpData(iOD:jOD),'(14('' ''))')
                        iOD=iOD+14
                     END IF
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
                  GOTO 3200

C              ELSE
C                 This node does not have any restrained DoF or springs
               END IF

3190           CONTINUE

3200        CONTINUE
C           ILINE=ILINE+1

C        Equilibrium checks
C        ------------------
3500     CALL LF(F%O%LU,1)

         WRITE(OutpData,'(10X,2(3X,11(''-'')))')
         CALL SWGTXT(idSCR,' '//TRIM(OutpData))                     !Con
         WRITE(F%O%LU,'(A)')TRIM(REPEAT(' ',iTAB2)//OutpData//CR$)  !Fil
         ILINE=ILINE+1

C        Total reactions
C        ---------------
         WRITE(OutpData,'(''Reactions: '', 2F14.3)') REACX,REACY
         CALL SWGTXT(idSCR,TRIM(OutpData))                          !Con
         WRITE(F%O%LU,'(A)')TRIM(REPEAT(' ',iTAB2-1)//OutpData//CR$)!Fil
         ILINE=ILINE+1

C        Total applied loads
C        -------------------
         WRITE(OutpData,'(''Loads    : '', 2F14.3)')
     +   EQLIR(1,ILCAS), EQLIR(2,ILCAS)
         CALL SWGTXT(idSCR,TRIM(OutpData))                          !Con
         WRITE(F%O%LU,'(A)')TRIM(REPEAT(' ',iTAB2-1)//OutpData//CR$)!Fil
         ILINE=ILINE+1

C        Errors
C        ------
         WRITE(OutpData,'(''Errors   : '', 2F14.5)')
     +   EQLIR(1,ILCAS)+REACX,
     +   EQLIR(2,ILCAS)+REACY
         CALL SWGTXT(idSCR,TRIM(OutpData))                          !Con
         WRITE(F%O%LU,'(A)')TRIM(REPEAT(' ',iTAB2-1)//OutpData//CR$)!Fil
         ILINE=ILINE+1

         ILINE=ILINE+4
         CALL SWGTXT(idSCR,REPEAT('-',81))
3900     CONTINUE

      RETURN


      END SELECT
      END










