#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - MODEL INITIAL CONDITIONS -                                    *
*                                                                     *
***********************************************************************
      SUBROUTINE MODEL_IC__ERYB( P, N )
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
      SUBROUTINE MODEL_INCL__ERYB( P )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs
      LOGICAL           P(*)

      P( _B_n6n1_ ) = .TRUE.
      P( _B_n6n2_ ) = .TRUE.
      P( _B_n6n3_ ) = .TRUE.
      P( _B_n5n1_ ) = .TRUE.
      P( _B_n5n2_ ) = .TRUE.
      P( _B_n5n3_ ) = .TRUE.
      P( _B_n3n1_ ) = .TRUE.
      P( _B_n3n2_ ) = .TRUE.

      P( _k_Yb_   ) = .TRUE.
      P( _k_CR_   ) = .TRUE.
      P( _k_ET1_  ) = .TRUE.
      P( _k_ET2_  ) = .TRUE.
      P( _k_ET3_  ) = .TRUE.
      P( _kp_ET1_ ) = .TRUE.
      P( _kp_ET2_ ) = .TRUE.
      P( _k_R1_   ) = .TRUE.
      P( _k_R2_   ) = .TRUE.
      P( _k_R3_   ) = .TRUE.
      P( _k_n2_   ) = .TRUE.
      P( _k_NR1_  ) = .TRUE.
      P( _k_NR2_  ) = .TRUE.

      P( _n1_0_   ) = .TRUE.
      P( _n2_0_   ) = .TRUE.
      P( _n3_0_   ) = .TRUE.
      P( _n4_0_   ) = .TRUE.
      P( _n5_0_   ) = .TRUE.
      P( _n6_0_   ) = .TRUE.
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
      SUBROUTINE MODEL__ERYB( N, P,  dN )
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
*#   NOTE: 2H_11/2 <--> 4S_3/2 (n6) in thermal equilibrium              #
*#   1.  k_Yb     Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                                                                      #
*#   2.  k_CR     Er3+: 4I_15/2 (n1)  --> 4I_13/2 (n2)                  #
*#                Er3+: 4S_3/2  (n6)  --> 4I_9/2  (n3)                  #
*#                                                                      #
*#   3.  k_ET1    Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                Er3+: 4I_15/2 (n1)  --> 4I_11/2 (n3)                  #
*#                                                                      #
*#   4.  k_ET2    Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                Er3+: 4I_13/2 (n2)  --> 4F_9/2  (n5)                  #
*#                                                                      #
*#   5.  k_ET3    Yb3+: 2F_5/2  (np2) --> 2F_7/2  (np1)                 #
*#                Er3+: 4I_11/2 (n3)  --> 4F_7/2  (n7) --> 4S_3/2 (n6)  #
*#                                                                      #
*#   6.  k'_ET1   Yb3+: 2F_7/2  (np1) --> 2F_5/2  (np2)                 #
*#                Er3+: 4I_11/2 (n3)  --> 4I_15/2 (n1)                  #
*#                                                                      #
*#   7.  k'_ET2   Yb3+: 2F_7/2  (np1) --> 2F_5/2  (np1)                 #
*#                Er3+: 4F_9/2  (n5)  --> 4I_13/2 (n2)                  #
*#                                                                      #
*#   8.  k_R1     Er3+: 2H_11/2 (n6)  --> 4I_15/2 (n1)   Green          #
*#                Er3+: 2H_11/2 (n6)  --> 4I_13/2 (n2)                  #
*#                Er3+: 2H_11/2 (n6)  --> 4I_11/2 (n3)                  #
*#                                                                      #
*#   9.  k_R2     Er3+: 4F_9/2  (n5)  --> 4I_15/2 (n1)   Red            #
*#                Er3+: 4F_9/2  (n5)  --> 4I_13/2 (n2)                  #
*#                Er3+: 4F_9/2  (n5)  --> 4I_11/2 (n3)                  #
*#                                                                      #
*#   10. k_R3     Er3+: 4I_11/2 (n3)  --> 4I_15/2 (n1)   976nm          #
*#                Er3+: 4I_11/2 (n3)  --> 4I_13/2 (n2)                  #
*#                                                                      #
*#   11. k_n2     Er3+: 4I_13/2 (n2)  --> 4I_15/2 (n1)   1500nm (NIR)   #
*#                                                                      #
*#   12. k_NR1    Er3+: 4S_3/2  (n6)  --> 4F_9/2  (n5)                  #
*#                                                                      #
*#   13. k_NR2    Er3+: 4I_11/2 (n3)  --> 4I_13/2 (n2)                  #
*#                                                                      #
*########################################################################

         dN(_n1_) = P(_B_n6n1_) * P(_k_R1_)   * N(_n6_)
     &            + P(_B_n5n1_) * P(_k_R2_)   * N(_n5_)
     &            + P(_B_n3n1_) * P(_k_R3_)   * N(_n3_)
     &            +               P(_k_n2_)   * N(_n2_)
     &            -               P(_k_ET1_)  * N(_n1_) * N(_np2_)
     &            +               P(_kp_ET1_) * N(_n3_) * N(_np1_)
     &            -               P(_k_CR_)   * N(_n1_) * N(_n6_)

         dN(_n2_) = P(_B_n6n2_) * P(_k_R1_)   * N(_n6_)
     &            + P(_B_n5n2_) * P(_k_R2_)   * N(_n5_)
     &            + P(_B_n3n2_) * P(_k_R3_)   * N(_n3_)
     &            +               P(_k_NR2_)  * N(_n3_)
     &            -               P(_k_n2_)   * N(_n2_)
     &            -               P(_k_ET2_)  * N(_n2_) * N(_np2_)
     &            +               P(_kp_ET2_) * N(_n5_) * N(_np1_)
     &            +               P(_k_CR_)   * N(_n1_) * N(_n6_)

         dN(_n3_) = P(_B_n6n3_) * P(_k_R1_)   * N(_n6_)
     &            + P(_B_n5n3_) * P(_k_R2_)   * N(_n5_)
     &            -               P(_k_R3_)   * N(_n3_)
     &            -               P(_k_NR2_)  * N(_n3_)
     &            +               P(_k_ET1_)  * N(_n1_) * N(_np2_)
     &            -               P(_kp_ET1_) * N(_n3_) * N(_np1_)
     &            -               P(_k_ET3_)  * N(_n3_) * N(_np2_)
     &            +               P(_k_CR_)   * N(_n1_) * N(_n6_)

         dN(_n5_) =               P(_k_NR1_)  * N(_n6_)
     &            +               P(_k_ET2_)  * N(_n2_) * N(_np2_)
     &            -               P(_kp_ET2_) * N(_n5_) * N(_np1_)
     &            -               P(_k_R2_)   * N(_n5_)

         dN(_n6_) =               P(_k_ET3_)  * N(_n3_) * N(_np2_)
     &            -               P(_k_NR1_)  * N(_n6_)
     &            -               P(_k_R1_)   * N(_n6_)
     &            -               P(_k_CR_)   * N(_n1_) * N(_n6_)

c         dN(_np1_) =                           -N(_np2_) 

         dN(_np2_) = P(_F_)     * P(_D_)      * N(_np1_)
     &             -              P(_k_Yb_)   * N(_np2_)
     &             -              P(_k_ET1_)  * N(_n1_) * N(_np2_)
     &             +              P(_kp_ET1_) * N(_n3_) * N(_np1_)
     &             -              P(_k_ET2_)  * N(_n2_) * N(_np2_)
     &             +              P(_kp_ET2_) * N(_n5_) * N(_np1_)
     &             -              P(_k_ET3_)  * N(_n3_) * N(_np2_) 

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
      SUBROUTINE RADIATIVES__ERYB( N, P,  R )
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

      R(_n2_)  =             P(_k_n2_)*N(_n2_)
      R(_n3_)  = P(_B_n3n1_)*P(_k_R3_)*N(_n3_) + P(_k_Yb_)*N(_np2_)
      R(_n5_)  = P(_B_n5n1_)*P(_k_R2_)*N(_n5_)
      R(_n6_)  = P(_B_n6n1_)*P(_k_R1_)*N(_n6_)

*     .. Diagnostics
      R(_np1_) = P(_B_n3n1_)*P(_k_R3_)*N(_n3_)
      R(_np2_) =             P(_k_Yb_)*N(_np2_)

      RETURN
      END



