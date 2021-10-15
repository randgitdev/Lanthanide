#include "lanthanide.h"


***********************************************************************
*                                                                     *
*     - DEFAULT SIMPLEX PARAMETERS -                                  *
*                                                                     *
***********************************************************************
c      SUBROUTINE SIMPLEX_INIT()
      SUBROUTINE INITIALIZE_SIMPLEX()
      IMPLICIT NONE

*     .. COMMON data block
#include "common-simplex.h"


*     .. Default SIMPLEX control constants
      SMPLX_DELTA = 1.5 ! Factor by which each dimension is multiplied
c      initFactor = 1.4999
      SMPLX_ALPHA = 0.9985 ! Reflection
      SMPLX_GAMMA = 1.95   ! Expansion
      SMPLX_RHO   = 0.5015 ! Contraction
      SMPLX_SIGMA = 0.4995 ! Reduction
c      sigma = 0.995 
*     ..
*     .. Set simplex iterations
      SMPLX_ITERATIONS = 1000

      SMPLX_LOG = .FALSE.
      SMPLX_BOUND = .FALSE.


      RETURN
      END



***********************************************************************
*                                                                     *
*     - SET SIMPLEX PARAMETERS -                                      *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION PARSE_SIMPLEX_PARAMETER( VB, PSYMBOL, PVALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         PSYMBOL*(*), PVALUE*(*)
*     .. COMMON data block
#include "common-simplex.h"

*     .. Functions
      LOGICAL           STRCMP
*     .. Local
      DOUBLE PRECISION  vard
      INTEGER           vari
      LOGICAL           varl


      PARSE_SIMPLEX_PARAMETER = .TRUE.

      IF     ( STRCMP( PSYMBOL, 'SIMPLEX_iterations' ) ) THEN
         READ(PVALUE, *) vari
         SMPLX_ITERATIONS = vari
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( PSYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( PSYMBOL, 'SIMPLEX_initfactor' )
     &         .OR.STRCMP( PSYMBOL, 'SIMPLEX_delta' ) ) THEN
         READ(PVALUE, *) vard
         SMPLX_DELTA = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( PSYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( PSYMBOL, 'SIMPLEX_alpha' ) ) THEN
         READ(PVALUE, *) vard
         SMPLX_ALPHA = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( PSYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( PSYMBOL, 'SIMPLEX_gamma' ) ) THEN
         READ(PVALUE, *) vard
         SMPLX_GAMMA = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( PSYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( PSYMBOL, 'SIMPLEX_rho' ) ) THEN
         READ(PVALUE, *) vard
         SMPLX_RHO = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( PSYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( PSYMBOL, 'SIMPLEX_sigma' ) ) THEN
         READ(PVALUE, *) vard
         SMPLX_SIGMA = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( PSYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( PSYMBOL, 'SIMPLEX_logarithmic' ) ) THEN
         READ(PVALUE, *) varl
         SMPLX_LOG = varl
         CALL SET_PVEC_LOG( varl )
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_LVALUE( PSYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF (    STRCMP( PSYMBOL, 'SIMPLEX_bound' )
     &        .OR.STRCMP( PSYMBOL, 'SIMPLEX_constraint' ) ) THEN
         READ(PVALUE, *) varl
         SMPLX_BOUND = varl
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_LVALUE( PSYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSE
         PARSE_SIMPLEX_PARAMETER = .FALSE.

      END IF



      RETURN
      END



***********************************************************************
*                                                                     *
*     - SIMPLEX -                                                     *
*                                                                     *
***********************************************************************
      SUBROUTINE SIMPLEX( VB, LUN, RC, 
     &                     PINI, PLOWER, PUPPER, NPS,
     &                     FINAL_PVEC,
     &                     FINAL_COST,
     &                     FINAL_ITERATIONS )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB, LUN, RC
      DOUBLE PRECISION  PINI(*), PLOWER(*), PUPPER(*)
      INTEGER           NPS
*     .. Outputs
      DOUBLE PRECISION  FINAL_PVEC(*)
      DOUBLE PRECISION  FINAL_COST
      INTEGER           FINAL_ITERATIONS
*     .. COMMON data block
#include "common-simplex.h"

*     .. Functions
*     .. Simplex data structures
c      DOUBLE PRECISION  P(_maxnps_), PINI(_maxnps_)
      DOUBLE PRECISION  P(_maxnps_)
      DOUBLE PRECISION  PP(_maxnps_, _maxnps_+1)
      DOUBLE PRECISION  PCOST(_maxnps_+1)
      INTEGER           iCostH, iCostL, iCost2H
*     ..
      DOUBLE PRECISION  CENT(_maxnps_), XX(_maxnps_), XY(_maxnps_)
      DOUBLE PRECISION  XR(_maxnps_), XE(_maxnps_), XC(_maxnps_)
      DOUBLE PRECISION  XT(_maxnps_)
      DOUBLE PRECISION  X0(_maxnps_)
      DOUBLE PRECISION  costXR, costXE, costXC, cost
*     ..
*     .. Local variables
      INTEGER           ii, jj, kk
      INTEGER           numret
*     ..
*     ..
*     ..
*     .. Parameters are:
*     ..  1. all rate constants, K's
*     ..  2. initial pop. n'_1
*     ..  3. initial pop. n'_2
*     ..  4. D, laser absorption (sigma), 
*     ..  NOTE:  initial pop. n_1 is a constant factor of n'_1
*     .. Pulsed excitation: Set laser intensity to zero
*     .. CW excitation:     Set initial n'_2 to zero


c      WRITE(*,*) 'SMPLX_LOG=', SMPLX_LOG

*     ..
*     .. Create initial simplex parameter matrix PP
      DO jj=1,NPS+1 ! PP is an N x N+1 matrix
         DO ii=1,NPS
            PP(ii,jj) = PINI(ii)
            IF ( ii.EQ.(jj-1) ) THEN
               IF (.NOT.SMPLX_LOG) THEN
                  PP(ii,jj) = PP(ii,jj) * SMPLX_DELTA
               ELSE
                  PP(ii,jj) = PP(ii,jj) + DLOG10(SMPLX_DELTA)
               END IF
c               IF (PP(ii,jj).GT.PUPPER(ii)) THEN
c                  PP(ii,jj) = 0.5*(PINI(ii) + PUPPER(ii))
c               END IF
            END IF
         END DO
         CALL ASYMPTOTIC_BOUNDS( PP(1,jj), PINI, PLOWER, PUPPER, NPS )
         CALL RUNSIM( VB, LUN, .FALSE., PP(1,jj), PCOST(jj) )

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_lf_) THEN
            WRITE(*,*) 'numret=', numret
c            WRITE(*,*) jj, NSIM, PCOST(jj)
            CALL SCREENDUMP( LUN, RC, jj, PCOST(jj), PP(1,jj) )
         END IF
*/////////////////////////////// Output ////////////////////////////////
      END DO


*.............
*     .. ITERATE THE SIMPLEX
      DO ii=1, SMPLX_ITERATIONS
*     ..
*     .. 1. Order the simplex
*     .. X0 = PP(1,iCostH)   Point with the highest cost
*     ..
         CALL SORTSIMPLEX( PCOST, NPS+1, iCostL, iCostH, iCost2H )
         CALL VECCOPY( PP(1,iCostH), X0, NPS )

      FINAL_ITERATIONS = ii
      FINAL_COST = PCOST(iCostL)
      CALL VECCOPY( PP(1,iCostL), FINAL_PVEC, NPS )

*/////////////////////////////// Output ////////////////////////////////
         IF ( VB.GE._vb_mf_ ) THEN
            WRITE(*,*) 'Index Low= ', iCostL
            WRITE(*,*) 'Index High=', iCostH
         END IF
*/////////////////////////////// Output ////////////////////////////////


*     ..
*     .. 2. Calculate centroid
*     ..    CENT = ( SUM(PP) - X0 ) ./ N
*     ..
         CALL MATSUM( PP, NPS, NPS+1, _maxnps_, CENT )
         CALL VECDIFF( CENT, X0, NPS, CENT )
         CALL VECSCALE( dble(1)/dble(NPS), CENT, NPS, CENT )
*/////////////////////////////// Output ////////////////////////////////
         IF ( VB.GE._vb_vhf_ ) THEN
c         WRITE(*,*) ( CENT(jj), jj=1,17 )
c         WRITE(*,*) 'Centroid'
c         WRITE(*,1310) ( CENT(kk), kk=1,3 )
c         WRITE(*,1320) ( CENT(kk), kk=4,8 )
c         WRITE(*,1330) ( CENT(kk), kk=9,13 )
c         WRITE(*,1340) CENT(14), CENT(15), CENT(16)
c         WRITE(*,*) ''
         END IF
*/////////////////////////////// Output ////////////////////////////////


*     ..
*     .. 3. Reflect centroid
*     ..
*     .. XR = CENT + SMPLX_ALPHA*( CENT - X0 )
         CALL VECDIFF( CENT, X0, NPS,   XX )
         CALL SIMPLEX_SCALE( SMPLX_ALPHA, XX, NPS,   XX )
         CALL VECSUM( CENT, XX, NPS,   XR )

         CALL ASYMPTOTIC_BOUNDS( XR, X0, PLOWER, PUPPER, NPS )
         CALL RUNSIM( VB, LUN, .FALSE., XR, costXR )


*     ..
*     .. 4. Test level of success
*     ..
         IF (      costXR.LT.PCOST(iCost2H) 
     &       .AND. costXR.GT.PCOST(iCostL)  ) THEN

*/////////////////////////////// Output ////////////////////////////////
            IF (VB.GE._vb_lf_) THEN
               WRITE(*,*) 'MODERATE SUCCESS'
            END IF
*/////////////////////////////// Output ////////////////////////////////
            CALL VECCOPY( XR, PP(1,iCostH), NPS )
            PCOST(iCostH) = costXR


*     ..
*     .. 5. EXPANSION
*     ..
*     .. XE = CENT + SMPLX_GAMMA*( CENT - X0 )
         ELSEIF ( costXR.LT.PCOST(iCostL) ) THEN

c           redundant?
            CALL VECDIFF( CENT, X0, NPS, XX )
            CALL SIMPLEX_SCALE( SMPLX_GAMMA, XX, NPS,   XX )
            CALL VECSUM( CENT, XX, NPS, XE )

            CALL ASYMPTOTIC_BOUNDS( XE, X0, PLOWER, PUPPER, NPS )
            CALL RUNSIM( VB, LUN, .FALSE., XE, costXE )

            IF ( costXE.LT.costXR ) THEN
*/////////////////////////////// Output ////////////////////////////////
               IF (VB.GE._vb_lf_) THEN
                  WRITE(*,*) 'SUCCESS + EXPANSION'
               END IF
*/////////////////////////////// Output ////////////////////////////////
               CALL VECCOPY( XE, PP(1,iCostH), NPS )
               PCOST(iCostH) = costXE
            ELSE
*/////////////////////////////// Output ////////////////////////////////
               IF (VB.GE._vb_lf_) THEN
                  WRITE(*,*) 'SUCCESS'
               ENDIF
*/////////////////////////////// Output ////////////////////////////////
               CALL VECCOPY( XR, PP(1,iCostH), NPS )
               PCOST(iCostH) = costXR
            ENDIF


*     ..
*     .. 6. CONTRACTION
*     ..
*     .. XC = PP(iCostH) + SMPLX_RHO*( CENT - X0 )
         ELSE

c           redundant?
            CALL VECDIFF( CENT, X0, NPS, XX )
            CALL SIMPLEX_SCALE( SMPLX_RHO, XX, NPS,   XX )
            CALL VECSUM( X0, XX, NPS,   XC )

c           Not needed?
            CALL ASYMPTOTIC_BOUNDS( XC, X0, PLOWER, PUPPER, NPS )
            CALL RUNSIM( VB, LUN, .FALSE., XC, costXC )

            IF ( costXC.LT.PCOST(iCostH) ) THEN

*/////////////////////////////// Output ////////////////////////////////
               IF (VB.GE._vb_lf_) THEN
                  WRITE(*,*) 'CONTRACTION'
               ENDIF
*/////////////////////////////// Output ////////////////////////////////
               CALL VECCOPY( XC, PP(1,iCostH), NPS )
               PCOST(iCostH) = costXC


*     .. 7. REDUCTION
            ELSE

*/////////////////////////////// Output ////////////////////////////////
               IF (VB.GE._vb_lf_) THEN
                  WRITE(*,*) 'REDUCTION'
               END IF
*/////////////////////////////// Output ////////////////////////////////

               DO jj=1, NPS+1
                  IF (jj.NE.iCostL) THEN
                     CALL VECDIFF( PP(1,jj), PP(1,iCostL), NPS,   XX )
                     CALL SIMPLEX_SCALE( SMPLX_SIGMA, XX, NPS,   XX )
                     CALL VECSUM( XX, PP(1,iCostL), NPS,   PP(1,jj) )
                     CALL RUNSIM( VB, LUN, .FALSE., PP(1,jj), PCOST(jj) )
                  ENDIF
               ENDDO

            ENDIF

         ENDIF

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_lf_) THEN
            CALL SCREENDUMP( LUN, RC, ii, PCOST(iCostL), PP(1,iCostL) )
         ELSE
            WRITE(*,*) 'REPEAT COUNT:  ', RC, '  ITERATION:  ', ii
         END IF
*/////////////////////////////// Output ////////////////////////////////

*.............
      END DO
*............. END SIMPLEX ITERATION


*     .. Make sure the output is of the lowest cost
      FINAL_ITERATIONS = ii-1
      CALL SORTSIMPLEX( PCOST, NPS+1, iCostL, iCostH, iCost2H )
      FINAL_COST = PCOST(iCostL)
      CALL VECCOPY( PP(1,iCostL), FINAL_PVEC, NPS )

      CALL RUNSIM( VB, LUN, .FALSE., FINAL_PVEC, PCOST(iCostH) )


      RETURN
      END




************************************************************************
*                                                                      *
*     - SIMPLEX SCALING -                                              *
*       Scale the parameter vector, linearly or logarithmically,       *
*       and test against the lower and upper bound constraint          *
*       vectors.                                                       *
*                                                                      *
************************************************************************
      SUBROUTINE SIMPLEX_SCALE( COEFF, Xin, NN, Xout )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  COEFF
      DOUBLE PRECISION  Xin(*)
      INTEGER           NN
*     .. Outputs
      DOUBLE PRECISION  Xout(*)
*     .. COMMON data block
#include "common-simplex.h"

*     .. Local
      DOUBLE PRECISION  Y(_maxnps_)


      IF (.NOT.SMPLX_LOG) THEN
         CALL VECSCALE( COEFF, Xin, NN,   Xout )
      ELSE
         CALL VECFILL( DLOG10(COEFF), NN,   Y )
         CALL VECSUM( Xin, Y, NN,   Xout )
      END IF


      RETURN
      END



*.......................................................................
      SUBROUTINE ASYMPTOTIC_BOUNDS( Xfinal, Xorig, Xlower, Xupper, NN )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  Xorig(*), Xfinal(*), Xlower(*), Xupper(*)
      INTEGER           NN
*     .. Outputs
*     .. COMMON data block
#include "common-simplex.h"

*     .. Local
c      DOUBLE PRECISION  Y(_maxnps_)
      INTEGER           ii, jj

      IF (SMPLX_BOUND) THEN
c         WRITE(*,*) 'SMPLX_BOUND=',SMPLX_BOUND
         DO ii=1, NN
            IF ( Xfinal(ii).LT.Xlower(ii) ) THEN
               WRITE(*,*) 'SIMPLEX CONSTRAINT -- LOWER BOUND'
               Xfinal(ii) = 0.5*( Xorig(ii) + Xlower(ii) )
            ELSE IF ( Xfinal(ii).GT.Xupper(ii) ) THEN
               WRITE(*,*) 'SIMPLEX CONSTRAINT -- UPPER BOUND'
               Xfinal(ii) = 0.5*( Xorig(ii) + Xupper(ii) )
            END IF
         END DO
      END IF

      RETURN
      END



************************************************************************
*                                                                      *
*     - SIMPLEX ORDERING -                                             *
*                                                                      *
************************************************************************
      SUBROUTINE SORTSIMPLEX( PCOST, NPCS, iCostL, iCostH, iCost2H )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  PCOST(*)
      INTEGER           NPCS
*     .. Outputs
      INTEGER           iCostL, iCostH, iCost2H
*     .. Local
      INTEGER           jj


* Find index for lowest and highest cost parameter vectors
*................
         iCostL=1
         iCostH=1
         DO jj=2,NPCS
            IF ( PCOST(jj).LT.PCOST(iCostL) )   iCostL = jj
            IF ( PCOST(jj).GT.PCOST(iCostH) )   iCostH = jj
         END DO
*................


* Now, find the index for the second highest parameter vector
*................
         iCost2H=iCostL   !Safe bet, all are greater than the lowest
         DO jj=1,NPCS
            IF ( PCOST(jj).GT.PCOST(iCost2H) .AND. jj.NE.iCostH ) 
     &           iCost2H = jj
         END DO
*................

      RETURN
      END


