#include "lanthanide.h"


***********************************************************************
*                                                                     *
*     - SELECT UPCONVERSION MODEL -                                   *
*                                                                     *
***********************************************************************
      SUBROUTINE INITIALIZE_MODEL( )
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-simulation.h"
#include "common-parameter-initial.h"
*     .. Input
*     .. Outputs
*     .. Functions
      LOGICAL           STRCMP


      IF ( STRCMP(UPMODEL,'ErYb') ) THEN
         CALL MODEL_INCL__ERYB( PINCL )

      ELSEIF ( STRCMP(UPMODEL,'ErYb+k_CR2') ) THEN
         CALL MODEL_INCL__ERYB_KCR2( PINCL )

      ELSEIF ( STRCMP(UPMODEL,'TmYb') ) THEN
         CALL MODEL_INCL__TMYB( PINCL )

      ELSE
         CALL CRITICAL('No valid upconv model given')

      ENDIF


      RETURN
      END




***********************************************************************
*                                                                     *
*     - SELECT UPCONVERSION MODEL -                                   *
*                                                                     *
***********************************************************************
      SUBROUTINE MODEL_INITIAL_CONDITIONS( MODEL, P,  N)
      IMPLICIT NONE
*     .. Input
      CHARACTER         MODEL*(*)
      DOUBLE PRECISION  P(*)
*     .. Outputs
      DOUBLE PRECISION  N(*)
*     .. Functions
      LOGICAL           STRCMP


      IF ( STRCMP(MODEL,'ErYb') ) THEN
         CALL MODEL_IC__ERYB( P, N)

      ELSEIF ( STRCMP(MODEL,'ErYb+k_CR2') ) THEN
         CALL MODEL_IC__ERYB_KCR2( P, N)

      ELSEIF ( STRCMP(MODEL,'TmYb') ) THEN
         CALL MODEL_IC__TMYB( P, N)

      ELSE
         CALL CRITICAL('No valid upconv model given')

      ENDIF


      RETURN
      END



***********************************************************************
*                                                                     *
*     - SELECT UPCONVERSION MODEL -                                   *
*                                                                     *
***********************************************************************
c      SUBROUTINE UPCONV_MODEL( MODEL, N, B, K, F, D, dN )
      SUBROUTINE UPCONV_MODEL( SOLVER, MODEL, dT,
     &                         N, NCOND, N0, NLVLS, nN, LDN,
     &                         P )
c     &                         B, nB,
c     &                         K, nK,
c     &                         F, D )

      IMPLICIT NONE
*     .. Input
      CHARACTER         SOLVER*(*)
      CHARACTER         MODEL*(*)
c      INTEGER           MODEL
      DOUBLE PRECISION  dT
c      DOUBLE PRECISION  B(*), K(*), F, D
      DOUBLE PRECISION  N(*), N0(*), NCOND(*)
      INTEGER           NLVLS, nN, LDN
c      INTEGER           nB, nK
      DOUBLE PRECISION  P(*)
*     .. Outputs
c      DOUBLE PRECISION  dN(*)
*     .. Functions
      LOGICAL           STRCMP
      EXTERNAL          MODEL__ERYB
      EXTERNAL          MODEL__ERYB_KCR2
      EXTERNAL          MODEL__TMYB


*     NOTE:  This function will be the place to call our new
*            MODEL_IC subroutine to map our initial conditions to
*            the separately defined population level arrays.

c      IF ( MODEL.EQ.0 ) THEN
c         CALL CRITICAL('No radiative model given')

c      WRITE(*,'(A,A)') 'Model=', MODEL
      IF ( STRCMP(MODEL,'ErYb') ) THEN
         CALL ODE_SOLVER( SOLVER, MODEL__ERYB, dT,
     &                    N, NCOND, N0, NLVLS, nN, LDN,
     &                    P )
c     &                    B, nB, K, nK, F, D )

      ELSEIF ( STRCMP(MODEL,'ErYb+k_CR2') ) THEN
         CALL ODE_SOLVER( SOLVER, MODEL__ERYB_KCR2, dT,
     &                    N, NCOND, N0, NLVLS, nN, LDN,
     &                    P )
c     &                    B, nB, K, nK, F, D )

      ELSEIF ( STRCMP(MODEL,'TmYb') ) THEN
         CALL ODE_SOLVER( SOLVER, MODEL__TMYB, dT,
     &                    N, NCOND, N0, NLVLS, nN, LDN,
     &                    P )
c     &                    B, nB, K, nK, F, D )

      ELSE
         CALL CRITICAL('No valid upconv model given')

      ENDIF


      RETURN
      END




***********************************************************************
*                                                                     *
*     - SELECT RADIATIVE OUTPUT -                                     *
*       Select the radiative output function corresponding to         *
*       the selected upconversion model.                              *
*                                                                     *
***********************************************************************
c      SUBROUTINE RADIATIVE_OUTPUT( MODEL, N, LDN, NSIM, B, K, R )
      SUBROUTINE RADIATIVE_OUTPUT( MODEL, N, LDN, NSIM, P,  R )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         MODEL*(*)
c      INTEGER           MODEL
      INTEGER           LDN, NSIM
c      DOUBLE PRECISION  N(*), B(*), K(*)
      DOUBLE PRECISION  N(LDN,*)
      DOUBLE PRECISION  P(*)
*     .. Outputs
c      DOUBLE PRECISION  R(*)
      DOUBLE PRECISION  R(LDN,*)
*     .. Functions
      LOGICAL           STRCMP
      EXTERNAL          RADIATIVES__ERYB
      EXTERNAL          RADIATIVES__ERYB_KCR2
      EXTERNAL          RADIATIVES__TMYB


      IF (     STRCMP(MODEL,'ErYb') ) THEN
         CALL POP_TO_RAD( RADIATIVES__ERYB,
     &                    N, LDN, NSIM,
     &                    P, R )

      ELSEIF ( STRCMP(MODEL,'ErYb+k_CR2') ) THEN
         CALL POP_TO_RAD( RADIATIVES__ERYB_KCR2,
     &                    N, LDN, NSIM,
     &                    P, R )

      ELSEIF ( STRCMP(MODEL,'TmYb') ) THEN
         CALL POP_TO_RAD( RADIATIVES__TMYB,
     &                    N, LDN, NSIM,
     &                    P, R )

      ELSE
         CALL CRITICAL('No valid radiative model given')

      ENDIF


      RETURN
      END




***********************************************************************
*                                                                     *
*     - POP_TO_RAD -                                                  *
*       Convert the populations to a radiative intensity.             *
*       The purpose of this routine is to progress through the        *
*       entire simulation data array and pass each column, in turn,   *
*       to the RADIATIVE_OUTPUT function which operates on one set    *
*       of population levels at a time.                               *
*                                                                     *
***********************************************************************
c      SUBROUTINE POP_TO_RAD( RADIATIVES, N, LDN, NSIM, B, K, R )
      SUBROUTINE POP_TO_RAD( RADIATIVES, N, LDN, NSIM, P,  R )
      IMPLICIT NONE
*     .. Inputs
*     .. NOTE:  _maxnlvls_ is the size of the leading dimension of N and R
      INTEGER           NSIM, LDN
      DOUBLE PRECISION  N( LDN, * ), P(*)
*     .. Outputs
      DOUBLE PRECISION  R( LDN, * )
*     .. Local
      INTEGER           jj


      DO jj=1,NSIM
         CALL RADIATIVES( N(1,jj), P,  R(1,jj) )
      END DO


      RETURN
      END

