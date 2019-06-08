      MODULE WINSTUFF
C     ----------------
C                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Window declarations
C                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     jw 09-01-12 draft
C                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     jw 22-02-12 last amended
C     --------------------
C     MISC FLAGS
C     --------------------
      LOGICAL  JustStrtd,
     +         MadeSCR, ShowsSCR,
     +         MORE,
     +         SOLVED


C     --------------------
C     IDENTIFIERS
C     Window, misc buttons
C     --------------------
C     INTEGER  Win1,
C    +         id0,id1,id2,
C    +         idOK,idCANCEL,
C    +         idLIST, idSCR, idLab,
C    +         idTitPAN,idTitPAN1,idTitPAN2,idTitPAN3,
C    +         idParPAN,idParPAN1,idParPAN2,idParPAN3,idParPAN4,
C    +         idMatPAN,idMatPAN1,idMatPAN2,idMatTbl,
C    +         idPrpPAN,idPrpPAN1,idPrpPAN2,idPrpTbl,
C    +         idMemPAN,idMemPAN1,idMemPAN2,idMemTbl,
C    +         idNodPAN,idNodPAN1,idNodPAN2,idNodTbl,
C    +         idResPAN,idResPAN1,idResPAN2,idResPAN3,idResTbl,idSprTbl,
C    +         idLodPAN,idLodPAN1,idLodPAN2,
C    +         idLodTbl,idLCNameTbl,idLCas,idMoreLoads
      INTEGER  Win1,
     +         id0,id1,id2,
     +         idOK,idCANCEL,
     +         idLIST, idSCR, idLab,
     +         idLod,idMoreLoads

C     --------------------
C     SCREEN SIZE
C     --------------------

C     Use CALL GETSCR(NWPIX,NHPIX) to set the screensize values
      INTEGER  NWPIX, ! Screensize - width
     +         NHPIX  ! Screensize - height


C     -------------------------------
C     WINDOW & PANE SIZES & POSITIONS
C     -------------------------------

C              Program window
      INTEGER  Win1L, !Margin left
     +         Win1T, !Margin top
     +         Win1W, !No of pixels wide
     +         Win1H  !No of pixels high

C              Pane 1
      INTEGER  Pan1L, !Margin left
     +         Pan1T, !Margin top
     +         Pan1W, !No of pixels wide
     +         Pan1H, !No of pixels high
     +         Pan1R  !Margin Right

C              Pane 2
      INTEGER  Pan2L, !Margin left
     +         Pan2T, !Margin top
     +         Pan2W, !No of pixels wide
     +         Pan2H, !No of pixels high
     +         Pan2R  !Margin Right

C              Pane 3
      INTEGER  Pan3L, !Margin left
     +         Pan3T, !Margin top
     +         Pan3W, !No of pixels wide
     +         Pan3H, !No of pixels high
     +         Pan3R, !Margin Right
     +         Pan3B  !Bottom

C              Pane 4
      INTEGER  Pan4L, !Margin left
     +         Pan4T, !Margin top
     +         Pan4W, !No of pixels wide
     +         Pan4H, !No of pixels high
     +         Pan4R  !Margin Right

C              Pane 5
      INTEGER  Pan5L, !Margin left
     +         Pan5T, !Margin top
     +         Pan5W, !No of pixels wide
     +         Pan5H  !No of pixels high

C              Pane 6
      INTEGER  Pan6L, !Margin left
     +         Pan6T, !Margin top
     +         Pan6W, !No of pixels wide
     +         Pan6H  !No of pixels high


C     --------------------
C     MENUS
C     --------------------
      INTEGER  MenuF,
     +            MenuF_New,
     +            MenuF_Open,
     +            MenuF_Save,
     +            MenuF_SaveAs,
     +            MenuF_Exit,
     +         MenuS,
     +            MenuS_Titles,
     +            MenuS_Params,
     +            MenuS_Matls,
     +            MenuS_Props,
     +            MenuS_Nodes,
     +            MenuS_Membs,
     +            MenuS_Restr,
     +            MenuS_DRW,
     +         MenuL,
     +            MenuL_Loads,
     +            MenuL_Combs,
     +         MenuO,
     +            MenuO_List,
     +            MenuO_Solve,
     +            MenuO_DRWmemthk,
     +            MenuO_DRWlines,
     +         MenuH,
     +            MenuHI,
     +            MenuHG


C     --------------------
C     TYPEFACES
C     --------------------
      Type :: FONT
         CHARACTER :: Styl*30
         INTEGER   :: Size
      END TYPE

      TYPE (FONT)  :: PrpS,  MonS,  ! Small
     +                PrpN,  MonN,  ! Normal
     +                PrpB,  MonB,  ! Normal, Bold
     +                PrpL,  MonL,  ! Large
     +                PrpLB, MonLB  ! Large, Bol

C     Monopitch fonts
      PARAMETER (MonS  = FONT('Courier',16))
      PARAMETER (MonN  = FONT('Courier',18))
      PARAMETER (MonB  = FONT('Courier-Bold',18))
      PARAMETER (MonL  = FONT('Courier',22))
      PARAMETER (MonLB = FONT('Courier-Bold',22))

C     Proportional fonts
      PARAMETER (PrpS  = FONT('Helvetica',16))
      PARAMETER (PrpN  = FONT('Helvetica',18))
      PARAMETER (PrpB  = FONT('Helvetica-Bold',18))
      PARAMETER (PrpL  = FONT('Helvetica',22))
      PARAMETER (PrpLB = FONT('Helvetica-Bold',22))

      SAVE

      END MODULE WinStuff
