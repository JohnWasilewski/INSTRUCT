      SUBROUTINE ClearAllData
C     -----------------------
C     jw / 29-07-12  1st draft started
C     jw / 29-07-12  last rev.

C     Clear/de-allocate all arrays
C     Clear all problrem size params

      USE dislin
      USE LUnits
      USE FILES
      USE WINSTUFF
      USE M
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING
      USE DRW

C     NMATS = 0
C     NMTYP = 0
C     NNODE = 0
C     NMEMB = 0
C     NPRES = 0
C     NSPRI = 0

C     IF(F%Named) THEN
C        IF(F%I%Open) CLOSE(F%I%LU)
C        F%I%OPEN        = .FALSE.
C        IF(F%O%Open) CLOSE(F%O%LU)
C        F%O%OPEN        = .FALSE.
C     END IF

C     F%Named         = .FALSE.
C     F%Name          = ''
C     F%BakExists     = .FALSE.
C     F%BAKdeletable  = .FALSE.
C     F%I%Exists      = .FALSE.
C     F%I%EXT         = ''
C     F%O%Exists      = .FALSE.
C     F%O%EXT         = ''
      F%InpDataPosted = .FALSE.

      CALL EndAlloc

      GoodTIT = .FALSE.
      GoodPAR = .FALSE.
      GoodMAT = .FALSE.
      GoodPRP = .FALSE.
      GoodNOD = .FALSE.
      GoodMEM = .FALSE.
      GoodRES = .FALSE.
      GoodLOD = .FALSE.
C     MORE    = .FALSE.

      MemRsrvd= .FALSE.
      
      TITLE1 = ' '
      TITLE2 = ' '
      TITLE3 = ' '
      DESC   = ' '
      ENG    = ' '
      

      RETURN
      END




      SUBROUTINE ENDALLOC
C     -------------------
C     jw / 18-05-12  draft
C     jw / 18-05-12  last mod

      USE dislin
      USE WINSTUFF
      USE FILES
      USE M
      USE TITLES
      USE PARAMS
      USE MATERIALS
      USE PROPERTIES
      USE NODES
      USE MEMBERS
      USE Restraints
      USE LOADING


      IF(ALLOCATED(MATLS)) THEN
         DEALLOCATE(MATLS, STAT=iERR)
         IF(iERR.GT.0) WRITE(LUO,
     +    '(''Warning - MATLS memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(PROPS))THEN
         DEALLOCATE(PROPS, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - PROPS memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(NODE))THEN
         DEALLOCATE(NODE, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - NODES memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(MEMB))THEN
         DEALLOCATE(MEMB, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,'(''Warning: MEMB memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(SPRING))THEN
         DEALLOCATE(SPRING, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - SPRING memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(JDIAG))THEN
         DEALLOCATE(JDIAG, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - JDIAG memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(KFIXD))THEN
         DEALLOCATE(KFIXD, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - KFIXD memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(LCName))THEN
         DEALLOCATE(LCName, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - LCName memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(LOADCASE))THEN
         DEALLOCATE(LOADCASE, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - LOADCASE memory lost until next reboot'')')
      END IF

      IF(ALLOCATED(LOADCOMB))THEN
         DEALLOCATE(LOADCOMB, STAT=iERR)
         IF(iERR.GT.0)
     +   WRITE(LUO,
     +    '(''Warning - LOADCOMB memory lost until next reboot'')')
      END IF

      RETURN
      END