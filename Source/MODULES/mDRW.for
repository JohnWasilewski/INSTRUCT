      MODULE DRW
C     ----------
C     jw 08-06-12 last amended

      REAL    MinX,MinY,  !Frame min X,Y: frame units (say, metres)
     +        MaxX,MaxY,  !Frame Max X,Y: m (assumed from now on)
     +        FraX0,FraY0 !Diagram origin: m

      INTEGER Pan3Wi,Pan3Hi,    !Pane width, height: pixels
     +        LRMargin,TBMargin,!Plot margins: pixels
     +        PanCx,PanCy,      !Plot & frame centre: pixels
     +        PanX0,PanY0       !Frame origin: pixels

      INTEGER idDRW
      LOGICAL MadeDRW, ShowsDRW, ShowThknss

      END MODULE DRW