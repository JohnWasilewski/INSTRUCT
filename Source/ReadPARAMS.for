      SUBROUTINE ReadPARAMS(LU,NextLine)
C     ----------------------------------
      USE PARAMS
      LOGICAL NoCmnt
      CHARACTER NextLine*80

      IF(NMATS.EQ.0) THEN
         READ(NextLine,*) NMATS
      ELSE
         READ(NextLine,*) iDUM
      END IF

      IF(NMTYP.EQ.0) THEN
         READ(LU,*) NMTYP
      ELSE
         READ(LU,*) iDUM
      END IF

      IF(NNODE.EQ.0) THEN
         READ(LU,*) NNODE
      ELSE
         READ(LU,*) iDUM
      END IF

      IF(NMEMB.EQ.0) THEN
         READ(LU,*) NMEMB
      ELSE
         READ(LU,*) iDUM
      END IF

      IF(NPRES.EQ.0) THEN
         READ(LU,*) NPRES
      ELSE
         READ(LU,*) iDUM
      END IF

      IF(NSPRI.EQ.0) THEN
         READ(LU,*) NSPRI
      ELSE
         READ(LU,*) iDUM
      END IF

C     READ(LU,*)       NMTYP
C     READ(LU,*)       NNODE
C     READ(LU,*)       NMEMB
C     READ(LU,*)       NPRES
C     READ(LU,*)       NSPRI
C     READ(LU,*)       NLCAS

C     Warnings
C     --------
      NoCmnt = .TRUE.

      IF(NMATS.GT.8) THEN
         CALL SCRmsg('Warning: So many material types intended?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NMTYP.GT.8) THEN
         CALL SCRmsg('Warning: So many member types intended?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NNODE.LT.4) THEN
         CALL SCRmsg('Warning: Really so few nodes?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NMEMB.LT.3) THEN
         CALL SCRmsg('Warning: So few members intended?',NoCmnt)
         NoCmnt = .FALSE.
      END IF

      IF(NPRES.LT.3) THEN
         CALL SCRmsg('Warning: With so few rigid restraints, '//
     +               'spring(s) will also be necessary',NoCmnt)
         NoCmnt = .FALSE.
      END IF

C     Errors
C     ------
      GoodPAR = .TRUE.

      IF(NMATS.LT.1) THEN
         CALL SCRmsg('**Error: At least 1 material type is needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NMTYP.LT.1) THEN
         CALL SCRmsg('**Error: At least 1 member type is needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NNODE.LT.2) THEN
         CALL SCRmsg('**Error: At least 2 nodes are needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NMEMB.LT.1) THEN
         CALL SCRmsg('**Error: At least 1 member is needed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NPRES.LT.0) THEN
         CALL SCRmsg('**Error: Negative no. of restraints not allowed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NPRES.GE.3*NNODE) THEN
         CALL SCRmsg('**Error: Too many restraints',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NPRES+NSPRI.LT.3) THEN
         CALL SCRmsg
     +   ('**Error: More supports and/or springs essential',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NSPRI.LT.0) THEN
         CALL SCRmsg
     +   ('**Error: Negative number of springs not allowed',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(NSPRI.GT.3*NNODE-NPRES) THEN
         CALL SCRmsg
     +   ('**Error: Too many springs (more than total DoF)',
     +   (NoCmnt.AND.GoodPAR) )
         GoodPAR = .FALSE.
      END IF

      IF(.NOT.GoodPAR) THEN
C        RETURN leaving the error messages still displayed
         CALL SCRmsg(REPEAT('-',80),(.FALSE.))
         RETURN
      END IF

      RETURN
      END
