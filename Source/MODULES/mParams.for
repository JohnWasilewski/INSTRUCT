      MODULE PARAMS
C     -------------
C     jw 18-02-12 draft
C     jw 18-02-12 last amended

      LOGICAL  ShowsPAR, GoodPAR, ReSizing

      INTEGER  idParPAN,
     +            idParPAN1,
     +            idParPAN2,
     +            idParPAN3,
     +            idParPAN4,
     +         idNMATS,
     +         idNMTYP,
     +         idNNODE,
     +         idNMEMB,
     +         idNPRES,
     +         idNSPRI

      INTEGER  NMATS,  NMTYP,  NNODE,  NMEMB,  NPRES,  NSPRI
      INTEGER  NMATSx, NMTYPx, NNODEx, NMEMBx, NPRESx, NSPRIx
      INTEGER  NLCAS, NLOAD, NCOMB, NLCASC
      INTEGER  NDOFG, NDOFR, NACTN, MBYTES, NBYTES, MALLOC1
      INTEGER  JDND

      END MODULE PARAMS