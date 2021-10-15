#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - MODEL INITIAL CONDITIONS -                                    *
*                                                                     *
***********************************************************************
      SUBROUTINE MODEL_IC__TMYB( P, N )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  P(*)
*     .. Outputs
      DOUBLE PRECISION  N(*)
*     .. Local

      N(_n1_)  = P(_n1_0_)
      N(_n2_)  = P(_n2_0_)
      N(_n3_)  = P(_n3_0_)
      N(_n4_)  = P(_n4_0_)
      N(_n5_)  = P(_n5_0_)
      N(_n6_)  = P(_n6_0_)
c      N(_n7_)  = P(_n7_0_)
      N(_np1_) = P(_np1_0_)
      N(_np2_) = P(_np2_0_)

      RETURN
      END



***********************************************************************
*                                                                     *
*     - MODEL INCLUSION -                                             *
*       Set the parameter inclusion list to TRUE so that we only      *
*       output parameters which this model uses.                      *
*                                                                     *
***********************************************************************
      SUBROUTINE MODEL_INCL__TMYB( P )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs
      LOGICAL           P(*)

c      P( _B_n7n1_ ) = .TRUE.
c      P( _B_n7n2_ ) = .TRUE.
c      P( _B_n7n3_ ) = .TRUE.
c      P( _B_n7n4_ ) = .TRUE.
c      P( _B_n7n5_ ) = .TRUE.
c      P( _B_n7n6_ ) = .TRUE.

      P( _B_n6n1_ ) = .TRUE.
      P( _B_n6n2_ ) = .TRUE.
      P( _B_n6n3_ ) = .TRUE.
      P( _B_n6n4_ ) = .TRUE.
      P( _B_n6n5_ ) = .TRUE.

      P( _B_n5n1_ ) = .TRUE.
      P( _B_n5n2_ ) = .TRUE.
      P( _B_n5n3_ ) = .TRUE.
      P( _B_n5n4_ ) = .TRUE.

      P( _B_n4n1_ ) = .TRUE.
      P( _B_n4n2_ ) = .TRUE.
      P( _B_n4n3_ ) = .TRUE.

c      P( _B_n3n1_ ) = .TRUE.
c      P( _B_n3n2_ ) = .TRUE.

      P( _B_n2n1_ ) = .TRUE.

      P( _k_ET1_  ) = .TRUE.
      P( _k_ET2_  ) = .TRUE.
      P( _k_ET3_  ) = .TRUE.
c      P( _k_ET4_  ) = .TRUE.
      P( _kp_ET1_ ) = .TRUE.
      P( _kp_ET2_ ) = .TRUE.

      P( _k_Yb_   ) = .TRUE.
      P( _k_R2_   ) = .TRUE.
      P( _k_R4_   ) = .TRUE.
      P( _k_R5_   ) = .TRUE.
      P( _k_R6_   ) = .TRUE.
c      P( _k_R7_   ) = .TRUE.

c      P( _k_NR1_  ) = .TRUE.
c      P( _k_NR2_  ) = .TRUE.
      P( _k_NR3_  ) = .TRUE.
c      P( _k_NR4_  ) = .TRUE.
      P( _k_NR5_  ) = .TRUE.

      P( _n1_0_   ) = .TRUE.
      P( _n2_0_   ) = .TRUE.
      P( _n3_0_   ) = .TRUE.
      P( _n4_0_   ) = .TRUE.
      P( _n5_0_   ) = .TRUE.
      P( _n6_0_   ) = .TRUE.
c      P( _n7_0_   ) = .TRUE.
      P( _np1_0_  ) = .TRUE.
      P( _np2_0_  ) = .TRUE.

      P( _D_      ) = .TRUE.
      P( _F_      ) = .TRUE.


      RETURN
      END



***********************************************************************
*                                                                     *
*     - MODEL ERYB -                                                  *
*       Proposed upconversion model                                   *
*                                                                     *
***********************************************************************
c      SUBROUTINE MODEL__ERYB( N, B, K, F, D, dN )
      SUBROUTINE MODEL__TMYB( N, P,  dN )
      IMPLICIT NONE
*     .. Inputs
c      DOUBLE PRECISION  B(*), K(*), F, D
      DOUBLE PRECISION  P(*)
      DOUBLE PRECISION  N(*)
*     .. Outputs
      DOUBLE PRECISION  dN(*)


*########################################################################
*#                                                                      #
*#   -- MODEL RATE CONSTANTS --                                         #
*#                                                                      #
*#   NOTE: Be very certain that these correspond to the model selected  #
*#         above.  See model-eryb.fpp, model-eryb-kcr2.fpp, ...         #
*#                                                                      #
*#   1.  k_Yb     Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                                                                      #
*#   2.  k_ET1    Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                Tm3+: 3H_6    (n1)  --> 3H_5    (n3)                  #
*#                                                                      #
*#   3.  k_ET2    Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                Tm3+: 3H_4    (n2)  --> 3F_2    (n5)                  #
*#                                                                      #
*#   4.  k_ET3    Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                Tm3+: 3F_4    (n4)  --> 1G_4    (n6)                  #
*#                                                                      #
*#   5.  k'_ET1   Yb3+: 2F_7/2  (np1) --> 2F_5/2  (np2)                 #
*#                Tm3+: 3F_4    (n4)  --> 3H_6    (n1)                  #
*#                                                                      #
*#   6.  k'_ET2   Yb3+: 2F_7/2  (np1) --> 2F_5/2  (np1)                 #
*#                Tm3+: 1G_4    (n6)  --> 3F_4    (n4)                  #
*#                                                                      #
*#   7.  k_R2     Tm3+: 3H_4    (n2)  --> 3H_6    (n1)   NIR            #
*#                                                                      #
*#   8.  k_R4     Tm3+: 3F_4    (n4)  --> 3H_6    (n1)   NIR            #
*#                Tm3+: 3F_4    (n4)  --> 3H_4    (n2)                  #
*#                                                                      #
*#   9.  k_R5     Tm3+: 3F_2,3  (n5)  --> 3H_6    (n1)   Red            #
*#                                                                      #
*#   10. k_R6     Tm3+: 1G_4    (n2)  --> 3H_6    (n1)   Blue           #
*#                                                                      #
*#   12. k_NR3    Tm3+: 3H_5    (n3)  --> 3H_4    (n2)                  #
*#                                                                      #
*#   13. k_NR5    Tm3+: 3F_2,3  (n5)  --> 3F_4    (n4)                  #
*#                                                                      #
*########################################################################

         dN(_n1_) = 
c     &            + P(_B_n7n1_) * P(_k_R7_)   * N(_n7_)
     &            + P(_B_n6n1_) * P(_k_R6_)   * N(_n6_)
     &            + P(_B_n5n1_) * P(_k_R5_)   * N(_n5_)
     &            + P(_B_n4n1_) * P(_k_R4_)   * N(_n4_)
c     &            + P(_B_n3n1_) * P(_k_R3_)   * N(_n3_)
     &            + P(_B_n2n1_) * P(_k_R2_)   * N(_n2_)
     &            -               P(_k_ET1_)  * N(_n1_) * N(_np2_)
     &            +               P(_kp_ET1_) * N(_n4_) * N(_np1_)

         dN(_n2_) = 
c     &            + P(_B_n7n2_) * P(_k_R7_)   * N(_n7_)
     &            + P(_B_n6n2_) * P(_k_R6_)   * N(_n6_)
     &            + P(_B_n5n2_) * P(_k_R5_)   * N(_n5_)
     &            + P(_B_n4n2_) * P(_k_R4_)   * N(_n4_)
c     &            + P(_B_n3n2_) * P(_k_R3_)   * N(_n3_)
     &            -               P(_k_R2_)   * N(_n2_)
     &            +               P(_k_NR3_)  * N(_n3_)
     &            -               P(_k_ET2_)  * N(_n2_) * N(_np2_)

         dN(_n3_) = 
c     &            + P(_B_n7n3_) * P(_k_R7_)   * N(_n7_)
     &            + P(_B_n6n3_) * P(_k_R6_)   * N(_n6_)
     &            + P(_B_n5n3_) * P(_k_R5_)   * N(_n5_)
     &            + P(_B_n4n3_) * P(_k_R4_)   * N(_n4_)
c     &            -               P(_k_R3_)   * N(_n3_)
     &            -               P(_k_NR3_)  * N(_n3_)
     &            +               P(_k_ET1_)  * N(_n1_) * N(_np2_)

         dN(_n4_) =
c     &            + P(_B_n7n4_) * P(_k_R7_)   * N(_n7_)
     &            + P(_B_n6n4_) * P(_k_R6_)   * N(_n6_)
     &            + P(_B_n5n4_) * P(_k_R5_)   * N(_n5_)
     &            -               P(_k_R4_)   * N(_n4_)
     &            +               P(_k_NR5_)  * N(_n5_)
     &            -               P(_k_ET3_)  * N(_n4_) * N(_np2_)
     &            -               P(_kp_ET1_) * N(_n4_) * N(_np1_)
     &            +               P(_kp_ET2_) * N(_n6_) * N(_np1_)

         dN(_n5_) =
c     &            + P(_B_n7n5_) * P(_k_R7_)   * N(_n7_)
     &            + P(_B_n6n5_) * P(_k_R6_)   * N(_n6_)
     &            -               P(_k_R5_)   * N(_n5_)
     &            -               P(_k_NR5_)  * N(_n5_)
     &            +               P(_k_ET2_)  * N(_n2_) * N(_np2_)

         dN(_n6_) =
c     &            + P(_B_n7n6_) * P(_k_R7_)   * N(_n7_)
     &            -               P(_k_R6_)   * N(_n6_)
     &            +               P(_k_ET3_)  * N(_n4_) * N(_np2_)
c     &            -               P(_k_ET4_)  * N(_n6_) * N(_np2_)
     &            -               P(_kp_ET2_) * N(_n6_) * N(_np1_)


c         dN(_n7_) =
c     &            -               P(_k_R7_)   * N(_n7_)
c     &            +               P(_k_ET4_)  * N(_n6_) * N(_np2_)

         dN(_np2_) =
     &                            P(_F_)      * P(_D_)  * N(_np1_)
     &             -              P(_k_Yb_)   * N(_np2_)
     &             -              P(_k_ET1_)  * N(_n1_) * N(_np2_)
     &             -              P(_k_ET2_)  * N(_n2_) * N(_np2_)
     &             -              P(_k_ET3_)  * N(_n4_) * N(_np2_) 
c     &             -              P(_k_ET4_)  * N(_n6_) * N(_np2_) 
     &             +              P(_kp_ET1_) * N(_n4_) * N(_np1_)
     &             +              P(_kp_ET2_) * N(_n6_) * N(_np1_)

         dN(_np1_) = -dN(_np2_) 


      RETURN
      END



***********************************************************************
*                                                                     *
*     GENERATE RADIATIVE OUTPUTS                                      *
*                                                                     *
***********************************************************************
c      SUBROUTINE ERYB_RADIATIVE_OUTPUTS( B, K, N, R )
c      SUBROUTINE RADIATIVES__ERYB( N, B, K,  R )
      SUBROUTINE RADIATIVES__TMYB( N, P,  R )
      IMPLICIT NONE
*     .. Inputs
c      DOUBLE PRECISION  B(*), K(*), 
      DOUBLE PRECISION  P(*)
      DOUBLE PRECISION  N(*)
*     .. Outputs
      DOUBLE PRECISION  R(*)
*     .. Local
      INTEGER           ii


      DO ii=1, _maxnlvls_
         R(ii)=_zero_
      END DO

      R(_n2_)  = P(_B_n2n1_)*P(_k_R2_)*N(_n2_)
c      R(_n3_)  = P(_B_n3n1_)*P(_k_R3_)*N(_n3_) + P(_k_Yb_)*N(_np2_)
      R(_n4_)  = P(_B_n4n1_)*P(_k_R4_)*N(_n4_)
      R(_n5_)  = P(_B_n5n1_)*P(_k_R5_)*N(_n5_)
      R(_n6_)  = P(_B_n6n1_)*P(_k_R1_)*N(_n6_)

      R(_np2_) =             P(_k_Yb_)*N(_np2_)

*     .. Diagnostics
      R(_np1_) = P(_B_n3n1_)*P(_k_R3_)*N(_n3_)

      RETURN
      END



