      SUBROUTINE GetRESTR(iNode,String)
C     ---------------------------------
C     jw / 15-05-12  draft
C     jw / 14-07-13  last revised

      USE DISLIN
      USE PARAMS
      USE NODES
      USE Restraints
      USE WINSTUFF

      CHARACTER I2CHAR*8,String*6, A2
      CHARACTER*2 ADoF(3)

      PARAMETER (ADoF = (/'xX','yY','zZ'/))

C     Check whether RCODE has been run already

C     If RCODE has been called already, it will need to be
C     called again after reading new restraints data, so KFIXD
C     needs to be un-coded back to zeros and ones.
      IF(RCODEd) THEN
          CALL RdeCODE
      ELSE
          iPRES=0
      END IF !(RCODEd)

      DO iDoF=1,3
         IF(SCAN(String,ADoF(iDoF)).GT.0) THEN
C           Found a valid restraint direction, so check whether
C           it is a new restraint or has been restrained
C           already by a previous data-entry line

            IF (KFIXD(IDOFN(ABS(iNode),iDoF)) .NE. 1) THEN
C              It is a new restraint, so increment the
C              restraints total and check that the specified
C              total has not been exceeded
               iPRES = iPRES+1

               IF(iPRES.LE.NPRES) THEN
C                 Permitted total has not been not exceeded.
C                 The line of data read from the file is
C                 therefore verified as a valid new restraint
C                 so accept it by storing a 1 in array vector,
C                 KFIXD() at position IDOFN((iNode),IDOF).
C                 This is to tell subroutine RCODE that this is
C                 a rigid restraint.

                  KFIXD(IDOFN(ABS(iNode),iDoF)) = 1
                  GoodRES=.TRUE.

               ELSE
C                 Permitted restraints total has been exceeded.
                  CALL BailOut
     +            ('Error: Too many restraints. '//
     +            'Restraint <'//TRIM(I2CHAR(iNode,idum))//
     +             ADoF(iDoF)(1:1)//'> ignored.$','OPTIONS' ,'YES')
                  GoodRES=.FALSE.

               END IF !Restraints total check to specfd limit

            ELSE

C              This is not a new restraint, it is a repeat of a
C              restraint already entered, so issue a warning
C              message. Do not increment the restraints total
C              and there is no need to to store a 1 in array
C              vector, KFIXD() at position IDOFN((iNode),IDOF)
C              because there is already a 1 stored there.
C
               A2=ADoF(iDoF)

               CALL BailOut
     +         ('Warning: Restraint on node '//
     +         TRIM(I2CHAR(iNode,iDum))//'('//A2//') '//
     +         'entered more than once.$','OPTIONS' ,'YES')
               GoodRES = .TRUE.

            END IF !Whether this is a new restraint

         ELSE

C           Restraint direction iDoF does not appear in the
C           string following the specified restraint node.

C           Check to see whether a restraint has been entered
C           previously in direction iDoF.

            IF(KFIXD(IDOFN(ABS(iNode),iDoF)) .EQ. 1) THEN

C              If it has, then interpret the user's intention
C              in this data entry line according to the
C              following rule:
C               * If at least one of the other restraint
C                 directions HAS been specified then assume
C                 that the current data entry line is simply
C                 adding just that/those other restraint(s),
C                 in which case, just cycle to the next iDoF.
C               * If no other restraint direction(s) specified
C                 then assume the purpose of this data entry
C                 line is to remove the previously-entered
C                 restraint iDoF from on the selected node.

               IF(SCAN(String,ADoF(1)//ADoF(2)//ADoF(3)).GT.0)
     +         THEN
                  CYCLE
               ELSE
                  KFIXD(IDOFN(ABS(iNode),iDoF)) = 0
                  iPRES = iPRES-1
               END IF !Whether ANY valid restrd direction specified

            ELSE
C              This node is not already restrained in direction iDoF
               CYCLE

            END IF !Whether already restrained in directn iDoF

         END IF !Whether a valid restraint direction entered

      END DO !iDoF=1,3
      END SUBROUTINE GetRESTR