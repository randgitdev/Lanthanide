#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - SELECT ODE SOLVER ROUTINE -                                   *
*                                                                     *
***********************************************************************
      SUBROUTINE ODE_SOLVER( SOLVER, MODEL, dT,
     &                       N, NCOND, N0, NLVLS, nN, LDN,
     &                       P )
c     &                       B, nB,
c     &                       K, nK,
c     &                       F, D )
      IMPLICIT NONE
*     .. Input
      CHARACTER         SOLVER*(*)
c      CHARACTER         MODEL*(*)
c      INTEGER           MODEL
      DOUBLE PRECISION  dT
      DOUBLE PRECISION  N(*), NCOND(*), N0(*)
      DOUBLE PRECISION  P(*)
c      DOUBLE PRECISION  B(*), K(*), F, D
      INTEGER           NLVLS, nN, LDN
c      INTEGER           nB, nK
*     .. Functions
      LOGICAL           STRCMP
      EXTERNAL          MODEL


      IF ( STRCMP(SOLVER,'EULER') ) THEN
         CALL EULER( MODEL, dT,
     &               N, NCOND, N0, NLVLS, nN, LDN,
     &               P )
c     &               B, nB, K, nK, F, D )

      ELSEIF ( STRCMP(SOLVER,'RK4') ) THEN
         CALL RK4(   MODEL, dT,
     &               N, NCOND, N0, NLVLS, nN, LDN,
     &               P )
c     &               B, nB, K, nK, F, D )

      ELSE
         CALL CRITICAL('No valid integrator given')

      ENDIF


      RETURN
      END




***********************************************************************
*                                                                     *
*     - EULER ODE SOLVER -                                            *
*                                                                     *
***********************************************************************
      SUBROUTINE EULER( MODEL, dT,
     &                  N, NCOND, N0, NLVLS, nN, LDN,
     &                  P )
c     &                  B, nB,
c     &                  K, nK,
c     &                  F, D )
      IMPLICIT NONE
*     .. Inputs
c      CHARACTER         MODEL*(*)
c      INTEGER           MODEL
      DOUBLE PRECISION  dT
      DOUBLE PRECISION  N0(*)
      INTEGER           NLVLS, nN, LDN
      DOUBLE PRECISION  P(*)
c      DOUBLE PRECISION  B(*)
c      INTEGER           nB
c      DOUBLE PRECISION  K(*)
c      INTEGER           nK
c      DOUBLE PRECISION  F, D
*     .. Outputs
      DOUBLE PRECISION  N( LDN, * )
      DOUBLE PRECISION  NCOND( LDN, * )
*     .. Local variables
      INTEGER           II, JJ, KK
      INTEGER           JJ1


*     .. Zero the populations ..
      DO II = 1, NLVLS
         DO JJ = 1, nN
            N(II,JJ) = _zero_
         END DO
      END DO
*     ..
*     ..
*     .. Set initial populations ..
      DO II = 1, NLVLS
         N(II, 1) = N0(II)
      END DO
*     ..
*     ..
*     .. Beginning of integration loop ..
      DO JJ = 1, nN-1
         JJ1=JJ+1

c        Instant speed-up by calling the routine directly
         CALL MODEL( N(1,JJ), P,  N(1,JJ1) )

         CALL VECCOPY( N(1,JJ1), NCOND(1,JJ), NLVLS )

         CALL VECSCALE( dT, N(1,JJ1), NLVLS, N(1,JJ1) )
         CALL VECSUM( N(1,JJ), N(1,JJ1), NLVLS, N(1,JJ1) )
*     ..
*     .. End of integration loop
      END DO

*     ..
      RETURN
      END



***********************************************************************
*                                                                     *
*     RUNGE-KUTTA INTEGRATION                                         *
*                                                                     *
***********************************************************************
      SUBROUTINE RK4( MODEL, dT,
     &                N, NCOND, N0, NLVLS, nN, LDN,
     &                P )
c     &                B, nB,
c     &                K, nK,
c     &                F, D )
      IMPLICIT NONE
*     .. Inputs
c      CHARACTER         MODEL*(*)
c      INTEGER           MODEL
      DOUBLE PRECISION  dT
      DOUBLE PRECISION  N0(*)
      INTEGER           NLVLS, nN, LDN
      DOUBLE PRECISION  P(*)
c      DOUBLE PRECISION  B(*)
c      INTEGER           nB
c      DOUBLE PRECISION  K(*)
c      INTEGER           nK
c      DOUBLE PRECISION  F, D
*     .. Outputs
      DOUBLE PRECISION  N( LDN, * )
      DOUBLE PRECISION  NCOND( LDN, * )
*     .. Local variables
      INTEGER           II, JJ, KK
      INTEGER           JJ1
      DOUBLE PRECISION  Y(_maxnlvls_)
      DOUBLE PRECISION  M1(_maxnlvls_), M2(_maxnlvls_)
      DOUBLE PRECISION  M3(_maxnlvls_), M4(_maxnlvls_)


*     .. Zero the populations ..
      DO II = 1, NLVLS
         DO JJ = 1, nN
            N(II,JJ) = _zero_
         END DO
      END DO
*     ..
*     ..
*     .. Set initial populations ..
      DO II = 1, NLVLS
         N(II, 1) = N0(II)
      END DO
*     ..
*     ..
*     .. Beginning of integration loop ..
      DO JJ = 1, nN-1
         JJ1=JJ+1

*     .. Calculate M1
         CALL MODEL( N(1,JJ), P,  M1 )

*     .. Calculate M2 midway with slope M1
         CALL VECSCALE( 0.5D+0*dT, M1, NLVLS, Y )
         CALL VECSUM( N(1,JJ), Y, NLVLS, Y )
         CALL MODEL( Y, P,  M2 )

*     .. Calculate M3 midway with slope M2
         CALL VECSCALE( 0.5D+0*dT, M2, NLVLS, Y )
         CALL VECSUM( N(1,JJ), Y, NLVLS, Y )
         CALL MODEL( Y, P,  M3 )

*     .. Calculate M4 from M3
         CALL VECSCALE( dT, M3, NLVLS, Y )
         CALL VECSUM( N(1,JJ), Y, NLVLS, Y )
         CALL MODEL( Y, P,  M4 )

*     ..
         CALL VECSCALE( 2.0D+0, M2, NLVLS, M2 )
         CALL VECSCALE( 2.0D+0, M3, NLVLS, M3 )
         CALL VECSUM( M1, M2, NLVLS, Y )
         CALL VECSUM( Y,  M3, NLVLS, Y )
         CALL VECSUM( Y,  M4, NLVLS, Y )
         CALL VECSCALE( (1.0D+0 / 6.0D+0)*dT, Y, NLVLS, Y )
         CALL VECSUM( N(1,JJ), Y, NLVLS, N(1,JJ1) )

*     ..
         CALL VECCOPY( M1, NCOND(1,JJ), NLVLS )

*     ..
*     .. End of integration loop
      END DO


      RETURN
      END


