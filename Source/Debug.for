      Subroutine Debug0
C     -----------------
      USE dislin
      USE WINSTUFF
      USE LUnits
      USE Files
      USE RTF

C     CHARACTER*120 FNdebug
C     F%O%LU = F%O%LU+100
C
C     FNdebug = TRIM(F%Path)//TRIM(F%Name)//'.dbg'
C     OPEN(UNIT=F%O%LU,
C    +     FILE=TRIM(FNdebug),
C    +     STATUS='NEW')

      WRITE(F%O%LU,'(A)') RTFiniFONTS$
      WRITE(F%O%LU,'(A)') RTFiniColour$
      WRITE(F%O%LU,'(A)') TRIM(RTFiniPAGE$)
      WRITE(F%O%LU,'(A)') RTFiniPARA$
      WRITE(F%O%LU,'(A)') Small$

      CALL FLUSH (F%O%LU)

      RETURN
      END




      Subroutine Debug1(SubPrg,iSeq)
C     ------------------------------
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

      CHARACTER SubPrg*8, SubPr*10, Seq*1
      CHARACTER*16 I2CHAR
      CHARACTER*8, PARAMETER::Pad='        '
      INTEGER iSeq

      LenSP = LEN_TRIM(SubPrg)
      Seq   = TRIM(I2CHAR(iSeq,ndigits))
      If(LenSP.LT.8) THEN
          SubPr = Trim(SubPrg)//Seq
          LenSP = LenSP+1
      ELSE
          SubPr = SubPrg(1:7)//Seq
          LenSP = 8
      END IF
      SubPr = SubPr(1:LenSP)//REPEAT(' ',8-LenSP)//': '

      write(F%O%LU,102)
     +  (SubPr,
     +   iiMemb,
     +   MEMB(iiMEMB)%Node1,
     +   MEMB(iiMEMB)%Node2,
     +   MEMB(iiMEMB)%MTyp,
     +   (MEMB(iiMEMB)%Released(ii),ii=1,6),
     +   TRIM(MEMB(iiMEMB)%Desc)//CR$, iiMemb=1,NMEMB)

      ii=4

 102  FORMAT(/60(A10,
     + 'MEMB(',I3,') = ',2i4,2X,i4,2x,6L3,2X,A,/))

      RETURN
      END



      Subroutine Debug2(ISTERM,IMTERM, STIFS)
C     ---------------------------------------

      USE dislin
      USE WINSTUFF
      USE LUnits
      USE Files
      USE RTF

      USE PARAMS
      USE Restraints
      USE M

      REAL*8   STIFS(21)

      write(F%O%LU,101)
     +     ISTERM,ISTERM,       RSTIF(ISTERM),
     +            IMTERM,       STIFS(IMTERM),
     +                          RSTIF(ISTERM)+STIFS(IMTERM)
C     RSTIF(ISTERM) = RSTIF(ISTERM) + STIFS(IMTERM)
101   FORMAT ('\par ',
     + 'RSTIF(',I4,')  =      RSTIF(',I4,')', 12X,'=',F24.3,'\par ',
     +        17X,    '+ STIFPS(',I4,')', 20X,'+',F16.3,'\par ',
     +        54X,                            '--------------','\par ',
     +        50 X,                            F18.3,'\par \par ')

      RETURN
      END



      Subroutine Debug3(String)
C     -------------------------
      USE dislin
      USE WINSTUFF
      USE LUnits
      USE Files
      USE RTF

      USE PARAMS
      USE Restraints
      USE M

      CHARACTER*60 String
      character I2Char*16

      write(F%O%LU,'(A)') CR$
      write(F%O%LU,'(A)') TRIM(String)
      write(F%O%LU,'(A)') CR$
      write(F%O%LU,'(''RSTIF ='')')
      DO iii=1,JDND
         write(F%O%LU,'(A,F14.3 )')
     +   '   RSTIF('//trim(i2char(iii,idum))//') =', RSTIF(iii)
      END DO
      write(F%O%LU,'(A)') CR$
      RETURN
      END




      Subroutine Debug4(iMEMB)
C     ------------------------
C     Print the stiffness matrix

      USE dislin
      USE WINSTUFF
      USE LUnits
      USE Files
      USE RTF
      USE MEMBERS

      USE Params
      USE Restraints
      USE Config

      CHARACTER*60 String
      character I2Char*16

C       (  1   2   3   4  [5]  6  )
C       (  -   7   8   9 [10] 11  )
C       (  -   -  12  13 [14] 15  )
C       (  -   -   - [16] 17  18  )
C       (  -   -   -   -  19  20  )
C       (  -   -   -   -   -  21  )
C
C        ik = j + (i-1)*6 - i(i-1)/2
C           = j+ (i-1)*(6-i/2)
C           = j + (i-1)*(12-i)/2
C
C        ik(1,5) = 5 + 0*11/2 = 5
C        ik(3,5) = 5 + 2*9/2  = 14
C        ik(2,5) = 5 + 1*10/2 = 10
C        ik(4,4) = 4 + 3*8/2  = 16

      CALL nPAGE(PRLIPP-6,NewPg)

      WRITE(F%O%LU,'(A)') CR$//CR$//'MEMBER '//TRIM(I2char(iMEMB))//
     +                    CR$//Small$
      WRITE(F%O%LU,'(A)') 'Local stiffnesses'//Repeat(' ',40)//
     +                    'Global stiffnesses'//CR$

      DO kI = 1,6
          DO kJ = 1,6
              IF(kI.LE.kJ) THEN
                  ii = kI
                  ij = kJ
              ELSE
                  ii = kJ
                  ij = kI
              END IF !(kI.GT.kJ)

              WRITE(F%O%LU,20)
     +        MEMB(iMemb)%LStiff(ij+((ii-1)*(12-ii))/2)
20            FORMAT(F9.0)
          END DO !kj = 1,NDOFR

          WRITE(F%O%LU,'(A)') '   '

          DO kJ = 1,6
              IF(kI.LE.kJ) THEN
                  ii = kI
                  ij = kJ
              ELSE
                  ii = kJ
                  ij = kI
              END IF !(kI.GT.kJ)

              WRITE(F%O%LU,20)
     +        MEMB(iMemb)%GStiff(ij+((ii-1)*(12-ii))/2)
          END DO !kj = 1,NDOFR

          WRITE(F%O%LU,'(A)') CR$

      END DO !ki = 1,JDIAG(NDOFR)
      WRITE(F%O%LU,'(A)') unSmall$//CR$//CR$//CR$

      CALL FLUSH (F%O%LU)

      RETURN
      END





      Subroutine Debug5(String)
C     ------------------------
C     Print the stiffness matrix

      USE dislin
      USE WINSTUFF
      USE LUnits
      USE Files
      USE RTF

      USE M

      USE Params
      USE Restraints
      USE Config

      CHARACTER*60 String
      character I2Char*16, KValu*9


C     K-matrix
C     example  ( 1  2  .  .  .  .  . )   JDIAG = ( 1 )
C     -------  ( =  3  4  6  .  .  . )           ( 2 )
C              ( .  =  5  7  . 11  . )           ( 2 )
C              ( .  =  =  8  9 12  . )           ( 3 )
C              ( .  .  .  = 10 13 15 )           ( 2 )
C              ( .  .  =  =  = 14 16 )           ( 4 )
C              ( .  .  .  .  =  = 17 )           ( 3 )

      WRITE(F%O%LU,'(A)') CR$//'STRUCTURE STIFFNESS MATRIX'//CR$
      CALL FLUSH (F%O%LU)

      WRITE(F%O%LU,'(A)')
     +  TRIM(I2CHAR(NDOFR,iDum))//' x '//TRIM(I2CHAR(NDOFR,iDum))
      CALL FLUSH (F%O%LU)

      WRITE(F%O%LU,'(A)')CR$//CR$
      CALL FLUSH (F%O%LU)

C     DO kI = 1,JDIAG(NDOFR)
C           DO kJ = 1,NDOFR
C                  KValu = ' '
C             IF(kI.LE.kJ) THEN
C                 ii = kI
C                 ij = kJ
C             ELSE
C                 ii = kJ
C                 ij = kI
C             END IF !(kI.GT.kJ)
C
C             IF(kI*kJ.EQ.1) THEN
C                 WRITE(KValu,20) RSTIF(1)
C             ELSE IF(JDIAG(ij)-JDIAG(ij-1) .GT. ij-ii) THEN
C                 WRITE(KValu,20) RSTIF(JDIAG(ij)-(ij-ii))
C20                FORMAT(F9.0)
C             ELSE
C                 WRITE(KValu,20) 0.0
C             END IF
C             N0=F%O%LU
C             WRITE(F%O%LU,'(A)') KValu
C
C         END DO !kj = 1,NDOFR
C         WRITE(F%O%LU,'(A)') CR$//CR$
C     END DO !ki = 1,JDIAG(NDOFR)

      WRITE(F%O%LU,'(A)') Small$

      DO Ki=1,NDOFR
          DO Kj=1,NDOFR
              WRITE(F%O%LU,'(F9.0)') RSTIF(KXY(Ki,Kj,JDIAG))
              CALL FLUSH (F%O%LU)
          END DO !Kj=1,NDOFR
          WRITE(F%O%LU,'(A)') CR$//CR$
          CALL FLUSH (F%O%LU)
      END DO !Ki=1,NDOFR
      WRITE(F%O%LU,'(A)') unSmall$//CR$//CR$

      CALL FLUSH (F%O%LU)


      RETURN
      END