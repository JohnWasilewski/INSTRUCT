      SUBROUTINE BAILOUT(MSG,ALLOW,DEFAULT)
C     -------------------------------------
C     Sends the contents of MSG as far as the character before '$'.
C     Then either RETURNs (to continue) or exits.
 
      USE DISLIN

      CHARACTER*(*) MSG
      CHARACTER DEFAULT*3, ALLOW*7

      IF(ALLOW.EQ.'OPTIONS') THEN
C        User can select YES to continue or NO to quit
         IF(DEFAULT.EQ.'YES') THEN
            iOK=1
         ELSE
            iOK=0
         END IF
         CALL DWGBUT
     +   (MSG(1:INDEX(MSG,'$')-1)//'||'//
     +   'Continue?|'//
     +   '  - YES|'//
     +   '  - NO to quit',iOK)
         IF(iOK.EQ.0) THEN
            CALL EndAlloc
            CALL CloseALL
         END IF !Returns ready to resume
      ELSE
C        No option
         CALL DWGMSG(MSG(1:INDEX(MSG,'.')))
         CALL EndAlloc
         CALL CloseALL
      END IF

      RETURN
      END SUBROUTINE BAILOUT