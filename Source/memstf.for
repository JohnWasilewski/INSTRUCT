      SUBROUTINE MEMSTF(IMEMB,GLOBAL,NODE1,NODE2,STIFS,Lx,Ly,L)
C     ---------------------------------------------------------
C     jw / 11-03-86  draft
C     jw / 20-05-14  last amended

C     Calculate member stiffnesses relative to global member or local
C     structure axes depending upon whether the value of parameter
C     GLOBAL is .TRUE. or .FALSE. .

C     Slope-deflection equations
C
C      Nodal    Both ends            Moment release     Shear release
C     action    fully fixed          at remote end      at remote end
C     ------    ------------         --------------     -------------
C       Ma     (2EI/L)(2GrA+GrB-3m)  (3EI/L)(GrA-GrCh)  (EI/L)GrA
C       Mb     (2EI/L)(GrA+2GrB-3m)  (3EI/L)(GrB-GrCh)  (EI/L)GrB
C       Q      (6EI/l2)(GrA+GrB-2m)  (3EI/l2)(GrA-GrCh)   0
C
C       where, GrA,GrB are the end slopes at ends A,B
C       and    GrCh is the chord gradient, (yB-yA)/(xB-xL)


C     MEMBER STIFFNESSES
C
C                      ACTION               OTHER END
C                      -------              ---------
C     Moments  4EI/L  [  Unit  ]===member===[ Full  ]  2EI/L   Moments
C     Shears   6EI/L2 [rotation]            [fixity ] 6EI/L2   Shears
C
C     Moments  6EI/L2 [ Unit   ]===member===[ Full  ]  6EI/L2  Moments
C     Shears  12EI/L3 [shearing]            [fixity ] 12EI/L3  Shears
C
C     Moments  3EI/L  [  Unit  ]===member===[ Moment]  0       Moments
C     Shears   3EI/L2 [rotation]            [release] 3EI/L2   Shears
C
C     Moments  3EI/L2 [  Unit  ]===member===[ Moment]  0       Moments
C     Shears   3EI/L3 [shearing]            [release] 3EI/L3   Shears
C
C     Moments    EI/L [  Unit  ]===member===[ Shear ]  EI/L    Moments
C     Shears       0  [rotation]            [release]  0       Shears
C
C     Moments      0  [  Unit  ]===member===[ Shear ]  0       Moments
C     Shears       0  [shearing]            [release]  0       Shears
C

C     First calculate stiffnesses in local member axes,
C     then, if required, transform to global structure axes.

C     Short-cuts are used to abbreviate the full procedure for
C     global structure stiffnesses by evaluating direct expressions
C     that include the transformation multipliers.

C     The symmetrical lower triangle of local member stiffness terms
C     is not stored.  The 2-D upper triangular stiffnesses are stored
C     in a 1-D array by the following scheme:
C
C       (  1   2   3   4   5   6  )
C       (      7   8   9  10  11  )
C       (         12  13  14  15  )
C       (             16  17  18  )
C       (                 19  20  )
C       (                     21  )

C     The expression for the position of the (i,j)th term in the 1-D
C     array is  ((12-i)*(i-1)/2 + j)

      USE Members
      USE Materials
      USE Properties
      USE Nodes

      LOGICAL   GLOBAL, LOCAL
      LOGICAL   Rel(6), Rx1,Ry1,Rz1, Rx2,Ry2,Rz2

      INTEGER   IMEMB, NODE1,NODE2

      REAL*4    AX,AY,IZ, EMOD,GMOD

      REAL*8    Lx,Ly,L, K(21), STIFS(21)

      REAL*8    K1,  K2,  K3,  K4,  K5,  K6,
     +               K7,  K8,  K9,  K10, K11,
     +                    K12, K13, K14, K15,
     +                         K16, K17, K18,
     +                              K19, K20,
     +                                   K21

      REAL*8    EAL,EIL, TEIL,THREIL,FEIL,
     +          THREIL2,SEIL2,THREIL3,SEIL3,NEIL3,TWLEIL3,
     +          LINV, SINA,SINSQ, COSA,COSSQ, SINCOS

      EQUIVALENCE (Rx1, Rel(1)),
     +            (Ry1, Rel(2)),
     +            (Rz1, Rel(3)),
     +            (Rx2, Rel(4)),
     +            (Ry2, Rel(5)),
     +            (Rz2, Rel(6))

      EQUIVALENCE (K(1),K1), (K(8),K8),   (K(15),K15), !Although this is
     +            (K(2),K2), (K(9),K9),   (K(16),K16), !mainly to help
     +            (K(3),K3), (K(10),K10), (K(17),K17), !readability, it
     +            (K(4),K4), (K(11),K11), (K(18),K18), !might just also
     +            (K(5),K5), (K(12),K12), (K(19),K19), !be a smidgeon
     +            (K(6),K6), (K(13),K13), (K(20),K20), !faster to use
     +            (K(7),K7), (K(14),K14), (K(21),K21)  !real not array
                                                       !variables for K.


C     CALL DISP(0,'**MEMSTF',8,1)

      LOCAL = (.NOT. GLOBAL)
      STIFS = 0.0

C..   Extract the member characteristics (material constants,
C     section props, and end-node nos.) from array storage

C     CALL Debug1('MemStf  ',1)
      CALL GETMEM (iMEMB,NODE1,NODE2,
     +     AX,AY,IZ,Lx,Ly,L,GAMMA,EMOD,GMOD,REL)


      IF(.NOT.MEMB(iMemb)%StiffsStored) THEN
C         (MEMB(iMemb)%LStiff(IK) and MEMB(iMemb)%LStiff(IK), IK=1,21)
C         need to be calculated for the first and only time

          K=0.0
          MEMB(iMemb)%LStiff = 0.0
          MEMB(iMemb)%GStiff = 0.0

C..       Calculate the 9 different terms that appear in the local
C         reference frame stiffness matrix for the member.

              LINV    = 1.0/L
              EAL     = EMOD*AX*LINV                      !   EA/L
              EIL     = EMOD*IZ*LINV                      !   EI/L
              TEIL    = EIL+EIL                           !   2EI/L
              THREIL  = EIL+TEIL                          !   3EI/L
              FEIL    = TEIL+TEIL                         !   4EI/L
              THREIL2 = (TEIL+EIL)*LINV                   !   3EI/L2
              SEIL2   = THREIL2+THREIL2                   !   6EI/L2
              THREIL3 = THREIL2*LINV                      !   3EI/L3
              SEIL3   = THREIL3+THREIL3                   !   6EI/L3
              NEIL3   = SEIL3+THREIL3                     !   9EI/L3
              TWLEIL3 = THREIL3+THREIL3+THREIL3+THREIL3   !   12EI/L3
C             TWLEIL3 = (SEIL2+SEIL2)*LINV


C         -------------------------
C         LOCAL MEMBER STIFFNESSES
C         -------------------------

C         --------------
C         If fully fixed
C         --------------

C                     [ EAL                 -EAL                 ]
C                     [      TWLEIL3  SEIL2        -TWLEIL3  SEIL2 ]
C     [Local    ]     [       SEIL2   FEIL         -SEIL2   TEIL ]
C     [stiffness]  =  [-EAL                  EAL                 ]
C     [terms    ]     [     -TWLEIL3 -SEIL2         TWLEIL3 -SEIL2 ]
C                     [       SEIL2   TEIL         -SEIL2   FEIL ]


           K1 =  EAL
           K4 = -EAL
          K16 = EAL

           K7 = TWLEIL3
           K8 = SEIL2
          K10 = -TWLEIL3
          K11 = SEIL2

          K12 = FEIL
          K14 = -SEIL2
          K15 = TEIL

          K19 = TWLEIL3
          K20 = -SEIL2

          K21 = FEIL


C         IF(Fixed) GO TO 339

C         ---------------------------------------------
C         Begin adjustments for any member-end releases
C         ---------------------------------------------


          IF(Rx1 .OR. Rx2) THEN
C         Release DoF(1) or DoF(4):
C         -------------------------
C                           [ 0                    0                  ]
C                           [     ......  .....         ......  ..... ]
C           [Local    ]     [     ......  .....         ......  ..... ]
C           [stiffness]  =  [ 0                    0                  ]
C           [terms    ]     [     ......  .....         ......  ..... ]
C                           [     ......  .....         ......  ..... ]

              K1 = 0.0
              K4 = 0.0
             K16 = 0.0

          END IF



          IF(Ry1 .OR. Ry2) THEN
C         Release DoF(2) or DoF(5):
C         -------------------------
C                           [ ...                ...               ]
C                           [        0     0             0     0   ]
C           [Local    ]     [        0    EIL            0   -EIL  ]
C           [stiffness]  =  [ ...                ...               ]
C           [terms    ]     [        0     0             0     0   ]
C                           [        0   -EIL            0    EIL  ]


            K7  = 0.0
            K8  = 0.0
            K10 = 0.0
            K11 = 0.0

C           K12 = EIL
            IF(K12.NE.0.0) K12 = K12-THREIL

            K14 = 0.0

C           K15 = -EIL
            IF(K15.NE.0.0) K15 = K15-THREIL

            K19 = 0.0
            K20 = 0.0

C           K21 = EIL
            IF(K21.NE.0.0) K21 = K21-THREIL

          END IF


          IF(Rz1) THEN
C         Release DoF(3):
C         ---------------
C                           [  ...                 ...                 ]
C                           [        3EIL3    0          -3EIL3  3EIL2 ]
C           [Local    ]     [          0      0             0      0   ]
C           [stiffness]  =  [  ...                 ...                 ]
C           [terms    ]     [       -3EIL3    0           3EIL3 -3EIL2 ]
C                           [        3EIL2    0          -3EIL2   3EIL ]

C           K7  =  THREIL3
            IF(K7.NE.0.0) K7 = K7-NEIL3

            K8  =  0.0

C           K10 = -THREIL3
            IF(K10.NE.0.0) K10 = K10+NEIL3

C           K11 =  THREIL2
            IF(K11.NE.0.0) K11 = K11-THREIL2

            K12 =  0.0
            K14 =  0.0
            K15 =  0.0

C           K19 =  THREIL3
            IF(K19.NE.0.0) K19 = K19-NEIL3

C           K20 = -THREIL2
            IF(K20.NE.0.0) K20 = K20+THREIL2

C           K21 =  THREIL
            IF(K21.NE.0.0) K21 = K21-EIL

          END IF


          IF(Rz2) THEN
C         Release DoF(6):
C         ---------------
C                           [ ...                  ...                 ]
C                           [       3EIL3  3EIL2         -3EIL3    0   ]
C           [Local    ]     [       3EIL2   3EIL         -3EIL2    0   ]
C           [stiffness]  =  [ ...                  ...                 ]
C           [terms    ]     [      -3EIL3 -3EIL2          3EIL3    0   ]
C                           [          0     0              0      0   ]

C           K7 = THREIL3
            IF(K7.NE.0.0) K7 = K7-NEIL3

C           IF(K8.NE.0.0)
C    +         K8 = THREIL2
            IF(K8.NE.0.0) K8 = K8-THREIL2


C           K10 = -THREIL3
            IF(K10.NE.0.0) K10 = K10+NEIL3

            K11 = 0.0

C           IF(K2.NE.0.0)
C     +        K12 = THREIL
            IF(K12.NE.0.0) K12 = K12-EIL

C           IF(K14.NE.0.0)
C    +         K14 = -THREIL2
            IF(K14.NE.0.0) K14 = K14+THREIL2

C           K15 = 0.0

C           K19 = THREIL3
            IF(K19.NE.0.0) K19 = K19-NEIL3

            K20 = 0.0
            K21 = 0.0
          END IF



C         ----------------------------------------------
C         End of adjustments for any member-end releases
C         ----------------------------------------------

          DO IK=1,21
             MEMB(iMemb)%LStiff(IK) = K(IK)
          END DO

C339      IF(LOCAL) RETURN


C         -------------------------
C         GLOBAL MEMBER STIFFNESSES
C         -------------------------

C..       Calculate the 5 trig functions needed for the transformation
C         from member local to structure global axes.


C                                |      |      |
C                                |\     |     /|
C                                | \    |    / |
C                                |  \   |   /  |
C            |           sin = + |   \  |  /   | sin = +
C        S   |  A        cos = - |    \ | /    | cos = +
C            |                   |     \|/     |
C     -------+-------      ----+-+------+-----++------
C            |                   |     /|\     |
C        T   |  C        sin = - |    / | \    | sin = -
C            |           cos = - |   /  |  \   | cos = +
C                                |  /   |   \  |
C                                | /    |    \ |                                   \
C      Sign of   same as         |/     |     \|
C      -------   -------         |      |      |
C        sin       dy
C        cos       dx


          SinA    = Ly/L
          SinSQ   = SINA*SINA
          CosA    = Lx/L
          CosSQ   = COSA*COSA
          SinCos  = SINA*COSA


C..       Transform the local stiffness to global axes by evaluating
C         explicit expressions for each term of the matrix product:
C
C                                                            Transpose
C         [Global   ]     [     ]     [Local    ]     [     ]
C         [stiffness]  =  [ ROT ]  *  [stiffness]  *  [ ROT ]
C         [terms    ]     [     ]     [terms    ]     [     ]
C
C
C
C         Where..
C                     [ COSA  -SINA      -       -       -      - ]
C                     [ SINA   COSA      -       -       -      - ]
C         [     ]     [    -      -    1.00      -       -      - ]
C         [ ROT ]  =  [    -      -      -    COSA   -SINA      - ]
C         [     ]     [    -      -      -    SINA    COSA      - ]
C                     [    -      -      -       -       -   1.00 ]
C
C
C Expanding..
C
C  [C -S  -  -  -  -]   [K1 K2  K3  K4  K5  K6 ]   [ C  S  -  -  -  -]
C  [S  C  -  -  -  -]   [K2 K7  K8  K9  K10 K11]   [-S  C  -  -  -  -]
C  [-  -  1  -  -  -] x [K3 K8  K12 K13 K14 K15] x [ -  -  1  -  -  -]
C  [-  -  -  C -S  -]   [K4 K9  K13 K16 K17 K18]   [ -  -  -  C  S  -]
C  [-  -  -  S  C  -]   [K5 K10 K14 K17 K19 K20]   [ -  -  - -S  C  -]
C  [-  -  -  -  -  1]   [K6 K11 K15 K18 K20 K21]   [ -  -  -  -  -  1]
C
C    =
C
C [K1c-K2s K2c-K7s  K3c-K8s   K4c-K9s   K5c-K10s  K6c-K11s ]   [ C S - - - -]
C [K1s+K2c K2s+K7c  K3s+K8c   K4s+K9c   K5s+K10c  K6s+K11c ]   [-S C - - - -]
C [K3      K8       K12       K13       K14       K15      ] x [ - - 1 - - -]
C [K4c-K5s K9c-K10s K13c-K14s K16c-K17s K17c-K19s K18c-K20s]   [ - - - C S -]
C [K4s+K5c K9s+K10c K13s+K14c K16s+K17c K17s+K19c K18s+K20c]   [ - - --S C -]
C [K6      K11       K15      K18       K20       K21      ]   [ - - - - - 1]
C
C    =
C
C[K1cc-K2sc   K1sc-K2ss   K3c-K8s   K4cc-K9sc    K4sc-K9ss    K6c-K11s ]
C[-K2sc+K7ss  +K2cc-K7sc            -K5sc+K10ss  +K5cc-K10sc           ]
C[                                                                     ]
C[K1sc+K2cc   K1ss+K2sc   K3s+K8c   K4sc+K9cc    K4ss+K9sc    K6s+K11c ]
C[-K2ss-K7sc  +K2sc+K7cc            -K5ss-K10sc  +K5sc+K10cc           ]
C[                                                                     ]
C[K3c-K8s     K3s+K8c     K12       K13c-K14s    K13s+K14c    K15      ]
C[                                                                     ]
C[K4cc-K5sc   K4sc-K5ss   K13c-K14s K16cc-K17sc  K16sc-K17ss  K18c-K20s]
C[-K9sc+K10ss +K9cc-K10sc           -K17sc+K19ss +K17cc-K19sc          ]
C[                                                                     ]
C[K4sc+K5cc   K4ss+K5sc   K13s+K14c K16sc+K17cc  K16ss+K17sc  K18s+K20c]
C[-K9ss-K10sc +K9sc+K10cc           -K17ss-K19sc +K17sc+K19cc          ]
C[                                                                     ]
C[K6c-K11s    K6s+K11c    K15       K18c-K20s    K18s+K20c    K21      }
C

C         Global stiffness terms
C         ----------------------
          MEMB(iMemb)%GStiff( 1)=K1*CosSQ  + K7*SinSQ - 2.0*K2*SinCos
          MEMB(iMemb)%GStiff( 2)=(K1-K7)*SinCos + K2*(CosSQ-SinSQ)
          MEMB(iMemb)%GStiff( 3)=K3*CosA - K8*SinA
          MEMB(iMemb)%GStiff( 4)=K4*CosSQ + K10*SinSQ - (K5+K9)*SinCos
          MEMB(iMemb)%GStiff( 5)=(K4-K10)*SinCos + K5*CosSQ - K9*SinSQ
          MEMB(iMemb)%GStiff( 6)=K6*CosA - K11*SinA
          MEMB(iMemb)%GStiff( 7)=K1*SinSQ + K7*CosSQ + 2.0*K2*SinCos
          MEMB(iMemb)%GStiff( 8)=K3*SinA + K8*CosA
          MEMB(iMemb)%GStiff( 9)=(K4-K10)*SinCos - K5*SinSQ + K9*CosSQ
          MEMB(iMemb)%GStiff(10)=K4*SinSQ + K10*CosSQ + (K5+K9)*SinCos
          MEMB(iMemb)%GStiff(11)=K6*SinA + K11*CosA
          MEMB(iMemb)%GStiff(12)=K12
          MEMB(iMemb)%GStiff(13)=K13*CosA - K14*SinA
          MEMB(iMemb)%GStiff(14)=K13*SinA + K14*CosA
          MEMB(iMemb)%GStiff(15)=K15
          MEMB(iMemb)%GStiff(16)=K16*CosSQ + K19*SinSQ - 2.0*K17*SinCos
          MEMB(iMemb)%GStiff(17)=(K16-K19)*SinCos + K17*(CosSQ-SinSQ)
          MEMB(iMemb)%GStiff(18)=K18*CosA - K20*SinA
          MEMB(iMemb)%GStiff(19)=K16*SinSQ + K19*CosSQ + 2.0*K17*SinCos
          MEMB(iMemb)%GStiff(20)=K18*SinA + K20*CosA
          MEMB(iMemb)%GStiff(21)=K21

C         MEMB(iMemb)%GStiff( 1) =  K1*CosSQ  + K7*SinSQ
C         MEMB(iMemb)%GStiff( 2) =  K1*SinCos - K7*SinCos
C         MEMB(iMemb)%GStiff( 3) = -K8*SinA
C         MEMB(iMemb)%GStiff( 4) =  K4*CosSQ  + K10*SinSQ
C         MEMB(iMemb)%GStiff( 5) =  K4*SinCos - K10*SinCos
C         MEMB(iMemb)%GStiff( 6) = -K11*SinA
C         MEMB(iMemb)%GStiff( 7) =  K1*SinSQ  + K7*CosSQ
C         MEMB(iMemb)%GStiff( 8) =  K8*CosA
C         MEMB(iMemb)%GStiff( 9) =  K4*SinCos - K10*SinCos
C         MEMB(iMemb)%GStiff(10) =  K4*SinSQ  + K10*CosSQ
C         MEMB(iMemb)%GStiff(11) =  K11*CosA
C         MEMB(iMemb)%GStiff(12) =  K12
C         MEMB(iMemb)%GStiff(13) = -K14*SinA
C         MEMB(iMemb)%GStiff(14) =  K14*CosA
C         MEMB(iMemb)%GStiff(15) =  K15
C         MEMB(iMemb)%GStiff(16) =  K16*CosSQ  + K19*SinSQ
C         MEMB(iMemb)%GStiff(17) =  K16*SinCos - K19*SinCos
C         MEMB(iMemb)%GStiff(18) = -K20*SinA
C         MEMB(iMemb)%GStiff(19) =  K16*SinSQ + K19*CosSQ
C         MEMB(iMemb)%GStiff(20) =  K20*CosA
C         MEMB(iMemb)%GStiff(21) =  K21

          MEMB(iMemb)%StiffsStored = .TRUE.

      END IF !(.NOT.MEMB(iMemb)%StiffsStored)
             !if calculation of MEMB(iMemb)%L and MEMB(iMemb)%G
             !is unnecessary, all previous code has been skipped

      IF(LOCAL) THEN
          DO iStif=1,21
              STIFS(iStif) = MEMB(iMemb)%LStiff(iStif)       !(LOCAL)
          END DO
      ELSE
          DO iStif=1,21
              STIFS(iStif) = MEMB(iMemb)%GStiff(iStif)       !(GLOBAL)
          END DO
      END IF !(LOCAL or GLOBAL)

C     CALL DEBUG4(iMEMB)

C     DO iStif=1,21
C         IF(LOCAL) THEN
C             STIFS(iStif) = MEMB(iMemb)%LStiff(iStif)
C         END IF
C     ELSE !(GLOBAL)
C             STIFS(iStif) = MEMB(iMemb)%GStiff(iStif)
C         END IF
C     END DO

      RETURN
      END

