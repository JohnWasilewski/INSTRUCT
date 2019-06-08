      SUBROUTINE ParsePath(PATH,NAME,EXT)
C     -----------------------------------

C     Parse PATHname into PATH (no filename), NAME and EXT

      CHARACTER PATH*256, NAME*96, EXT*6

      iLen=LEN(Trim(AdjustL(PATH)))
      iBsl=INDEX(PATH,'\',.TRUE.)
      iDot=INDEX(PATH,'.',.TRUE.)

      IF(iDot.GT.iBsl) THEN
         LenEXT=iLen-iDot
         EXT(1:LenEXT) = PATH(iDot+1:iLen)
      ELSE
         iDot=iLEN+1
         LenEXT=0
         EXT=REPEAT(' ',12)
      END IF

      LenNAM=iDot-iBsl
      NAME(1:LenNAM) = PATH(iBsl+1:iDot-1)
      PATH=TRIM(PATH(1:iBsl)//REPEAT(' ',LenNAM+LenEXT+1))

C     NAME=TRIM(NAME)//'.'//TRIM(EXT)

      RETURN
      END
