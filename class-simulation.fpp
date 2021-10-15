#include "lanthanide.h"

************************************************************************
*                                                                      *
*     - DEFAULT SIMULATION PARAMETERS -                                *
*                                                                      *
************************************************************************
      SUBROUTINE INITIALIZE_SIMULATION()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-simulation.h"


*     .. Set default upconversion model and numerical ODE solver
c      UPMODEL = 'ERYB_MODEL'
      UPMODEL = 'ErYb'
c      UPMODEL = 1  ! 'ERYB_MODEL'
      ODESOLVER = 'EULER'
*     ..
*     .. Set default simulation delta time
      DELTA_T = 2.50D-06
*     .. Set the MINIMUM default simulation total time
      TOTAL_T = 1.00D-03


      RETURN
      END



************************************************************************
*                                                                      *
*     - DEFAULT SIMULATION PARAMETERS -                                *
*                                                                      *
************************************************************************
      SUBROUTINE CALCULATE_SIMULATION_TIME()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-simulation.h"


*     .. Find index into simulation for each decay curve data point
      CALL DC__CALCULATE_INDEX( DELTA_T )
*     ..
*     .. Find the maximum needed simulation index by taking the
*     .. maxmimum index from last step
      NSIM = TOTAL_T / DELTA_T
      CALL DC__ADJUST_NSIM( NSIM )

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   - Error condition
      IF ( NSIM.GT._maxnsim_ )  CALL CRITICAL('Increase sim buffer size')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


      RETURN
      END



************************************************************************
*                                                                      *
*     - SET SIMULATION PARAMETERS -                                    *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION PARSE_SIMULATION( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*), VALUE*(*)
*     .. Outputs
*     .. COMMON data blocks
#include "common-simulation.h"


*     .. Functions
      CHARACTER*120     STRCAT
      LOGICAL           STRCMP
*     .. Local
      CHARACTER*120     vars
      DOUBLE PRECISION  vard
      INTEGER           vari
      LOGICAL           varl


      PARSE_SIMULATION = .TRUE.

      IF (     STRCMP( symbol, 'UPCONVERSION_MODEL' )
     &     .OR.STRCMP( symbol, 'UPCONV_MODEL') ) THEN
         READ(value, '(A)') vars
         UPMODEL = vars
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, ' = '//value)
*/////////////////////////////// Output ////////////////////////////////
c         READ(value, *) vari
c         UPMODEL = vari
c*/////////////////////////////// Output ////////////////////////////////
c         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( symbol, vari )
c*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( symbol, 'ODE_SOLVER' ) ) THEN
         READ(value, '(A)') vars
         ODESOLVER = vars
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, ' = '//value)
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( symbol, 'DELTA_TIME' ) ) THEN
         READ(value, *) vard
         DELTA_T = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( symbol, vard )
*/////////////////////////////// Output ////////////////////////////////

      ELSEIF ( STRCMP( symbol, 'TOTAL_TIME' ) ) THEN
         READ(value, *) vard
         TOTAL_T = vard
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( symbol, vard )
*/////////////////////////////// Output ////////////////////////////////


      ELSE
         PARSE_SIMULATION = .FALSE.

      END IF

      RETURN
      END



************************************************************************
*                                                                      *
*     - RUNSIM -                                                       *
*                                                                      *
************************************************************************
      SUBROUTINE RUNSIM( VB, LUN, WRITE_OUTPUT, 
     &                   VP_VEC, COST )
c     &                   WRITE_OUTPUT )
      IMPLICIT NONE
*     .. INPUTS
      INTEGER           VB, LUN
      DOUBLE PRECISION  VP_VEC(*)
      LOGICAL           WRITE_OUTPUT
c      INTEGER           SKIP
*     ..
c      INTEGER           nM, nN, LDN
*     ..
*     .. OUTPUTS
      DOUBLE PRECISION  COST

*     .. COMMON data blocks
#include "common-simulation.h"
#include "common-parameter-initial.h"
#include "common-data-dc.h"
#include "common-data-ii.h"
#include "common-data-cw.h"
#include "common-energy.h"
#include "common-control.h"
#include "common-parameter-hash.h"
*     ................

*     .. Functions
      DOUBLE PRECISION  CHISQU_DC
      DOUBLE PRECISION  CHISQU_II
      DOUBLE PRECISION  CHISQU_CW
      DOUBLE PRECISION  LASERFLUX
      DOUBLE PRECISION  VECTRAPEZOID
      DOUBLE PRECISION  COST_DC
      DOUBLE PRECISION  COST_II
      CHARACTER*40      STRDCAT
      CHARACTER*5       DEBLANK
      LOGICAL           STRCMP
      CHARACTER*_mplsl_  STRING_POPULATION_LEVEL
      CHARACTER*_mpsl_   STRING_PARAMETER

*     .. Local variables
      DOUBLE PRECISION  NPULSE( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  NCW   ( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  N     ( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  NCOND ( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  RPULSE( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  RCW   ( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  R     ( _maxnlvls_, _maxnsim_ )
      DOUBLE PRECISION  RT    ( _maxnsim_, _maxnlvls_ )

      DOUBLE PRECISION  P_VEC( _max_n_params_ )
c      DOUBLE PRECISION  B( _maxnbs_ ), K( _maxnks_ ), N0( _maxnlvls_ )
      DOUBLE PRECISION  D, F
      DOUBLE PRECISION  N0( _maxnlvls_ )
      DOUBLE PRECISION  N0PUL( _maxnlvls_ ), N0CW( _maxnlvls_ )
c      DOUBLE PRECISION  RTN( _maxnsim_, _maxnlvls_ )
      DOUBLE PRECISION  Rn( _maxnsim_ )
      DOUBLE PRECISION  IIR( _maxnlvls_, _maxniidatas_ )
      DOUBLE PRECISION  IINT(_maxnlvls_)
*     ..
      DOUBLE PRECISION  dT
      DOUBLE PRECISION  tSim(_maxnsim_)
*     ..
      DOUBLE PRECISION  previous, cinc
      INTEGER           ii, jj, kk, iii, jjj
      INTEGER           idx, lvl
c      INTEGER           numret
c      CHARACTER         energy_string*10
c      CHARACTER*40      fname_pulse, fname_rad, fname_cw, fname_cond
c      CHARACTER*40      tstring1, tstring2


*     .. Run sim, pulsed and cw, then calculate cost
*     .. # of simulation points equals the column rank of N
c      NSIM = nN
      dT = DELTA_T
*     .. Produce simulation time vector, for output
      DO ii=1, NSIM
         tSim(ii) = dT * (ii-1)
      END DO

*     .. Initialize cost
      COST = _zero_

      CALL MATZERO( IIR, NLVLS, N_IIDATAS, _maxnlvls_ )

*     .. 1. Pulsed excitation - Decay curve data sets ..................
*/////////////////////////////// Output ////////////////////////////////
      IF ( VB.GE._vb_mf_ ) THEN
         WRITE(LUN,*) ''
         WRITE(LUN,*) 'N_DCFILES=',N_DCFILES
         WRITE(LUN,*) 'N_IIDATAS=',N_IIDATAS
         WRITE(LUN,*) 'N_CWDATAS=',N_CWDATAS
      END IF
*/////////////////////////////// Output ////////////////////////////////

*     .. For each unique pulse energy, run the simulation and then
*        compare to the decay-curve files and integrated intensity
*        data sets to calculate the costs.
      DO jj=1, N_UNIQUE_ENERGY
         CALL EXTRACT_PVEC( jj, VP_VEC,  P_VEC )
         P_VEC( _F_ ) = _zero_
         CALL MODEL_INITIAL_CONDITIONS( UPMODEL, P_VEC,  N0 )
         CALL UPCONV_MODEL(ODESOLVER, UPMODEL, dT,
     &                     NPULSE, NCOND, N0, NLVLS, NSIM, _maxnlvls_,
     &                     P_VEC )
         CALL RADIATIVE_OUTPUT( UPMODEL, NPULSE, _maxnlvls_, NSIM,
     &                          P_VEC,  RPULSE )
         CALL MATTRANSPOSE( RPULSE, NLVLS, NSIM, _maxnlvls_, 
     &                      RT, _maxnsim_ )
         DO kk=1, NLVLS
            IINT(kk) = VECTRAPEZOID( RT(1,kk), dT, NSIM )
         ENDDO

*/////////////////////////////// Output ////////////////////////////////
         IF ( VB.GE._vb_mf_ ) THEN
            WRITE(LUN,*) ''
            WRITE(LUN,'(/A)') '----------------------------'
            WRITE(LUN,'(A, E10.3, A)') 'Pulse energy = ',
     &                               UNIQUE_ENERGY(jj)*1000, 'mJ'
            WRITE(LUN,'(A)') '----------------------------'
            WRITE(LUN,'(A, I4)') '# of DC files: ', DB_DC_NIDX(jj)
         ENDIF
*/////////////////////////////// Output ////////////////////////////////

*        .. Pulsed excitation - Decay curve data sets ..
         cinc = COST_DC( VB, LUN, WRITE_OUTPUT, jj, RT, _maxnsim_, tSim )
         COST = COST + cinc

c        .. Pulsed excitation - Integrated Intensity data sets ..
         cinc = COST_II( VB, LUN, WRITE_OUTPUT, jj, IINT, IIR, _maxnlvls_ )
         COST = COST + cinc

         CALL REPORT_QUANTUM_EFFICIENCY( VB, LUN, IINT, P_VEC )

*/////////////////////////////// Output ////////////////////////////////
         IF ( VB.GE._vb_vhf_ ) THEN
            WRITE(LUN,*) ' '
            WRITE(LUN,*) 'N UNIQUE ENERGY=', N_UNIQUE_ENERGY
            WRITE(LUN,*) 'UNIQUE ENERGY=', UNIQUE_ENERGY(jj)
            WRITE(LUN,*) 'N PULSE=', DB_DC_NIDX(jj)
            WRITE(LUN,*) 'N II=', DB_II_NIDX(jj)
c            WRITE(LUN,*) 'NUMRET=', numret
            WRITE(LUN,*) ' '
         END IF
         IF ( WRITE_OUTPUT ) THEN
            CALL WRITE_DC_OUTPUT( VB, LUN, jj,
     &                            tSim, NPULSE, RPULSE, NCOND,
     &                            NLVLS, NSIM, _maxnlvls_ )
         END IF
*/////////////////////////////// Output ////////////////////////////////

      END DO


*/////////////////////////////// Output ////////////////////////////////
      IF ( WRITE_OUTPUT ) THEN
         CALL DATAOUTSIM( 'output-simii.dat', II_ENERGY,
     &                    IIR, NLVLS, N_IIDATAS, _maxnlvls_, 1)
      END IF
*/////////////////////////////// Output ////////////////////////////////


*     .. CW excitation .................................................
      CALL EXTRACT_PVEC( 1, VP_VEC,  P_VEC )
      CALL MAKE_CW_N0( P_VEC, P_VEC )
      CALL MODEL_INITIAL_CONDITIONS( UPMODEL, P_VEC,  N0CW )

      DO ii=1, N_CWDATAS
         F = LASERFLUX( CW_POWER(ii), CW_LAMBDA(ii), CW_RADIUS(ii) )
         P_VEC( _F_ ) = F
         CALL UPCONV_MODEL(ODESOLVER, UPMODEL, dT,
     &                     NCW, NCOND, N0CW, NLVLS, NSIM, _maxnlvls_,
     &                     P_VEC )
         CALL RADIATIVE_OUTPUT( UPMODEL, NCW, _maxnlvls_, NSIM, P_VEC, RCW )
         cinc = CHISQU_CW( VB, RCW(1,NSIM), ii )
         COST = COST + cinc

*/////////////////////////////// Output ////////////////////////////////
         IF ( VB.GE._vb_hf_ )  WRITE(LUN,*) 'Run solver CW'
         IF ( VB.GE._vb_mf_ )  WRITE(LUN,*) 'COST CW INC =', cinc
         IF ( WRITE_OUTPUT ) THEN
            CALL WRITE_CW_OUTPUT( VB, LUN, ii,
     &                            tSim, RCW,
     &                            NLVLS, NSIM, _maxnlvls_ )
         END IF
*/////////////////////////////// Output ////////////////////////////////

      END DO


      RETURN
      END




************************************************************************
*                                                                      *
*     - COST DC -                                                      *
*                                                                      *
************************************************************************
      DOUBLE PRECISION FUNCTION COST_DC( VB, LUN, WRITE_OUTPUT,
     &                                   EIDX, RT, LDR, TSIM )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB, LUN
      LOGICAL           WRITE_OUTPUT
      INTEGER           LDR, EIDX
      DOUBLE PRECISION  RT(LDR,*)
      DOUBLE PRECISION  TSIM(*)
*     .. Outputs
*     .. COMMON data blocks
#include "common-data-dc.h"
#include "common-energy.h"
#include "common-parameter-hash.h"
#include "common-simulation.h"
*     .. Local
      DOUBLE PRECISION  Rn(_maxnsim_)
      DOUBLE PRECISION  cinc
      INTEGER           idx, lvl
      INTEGER           ii
*     .. Functions
      DOUBLE PRECISION  CHISQU_DC
c      CHARACTER*5       DEBLANK
      CHARACTER*_mplsl_ STRING_POPULATION_LEVEL
      LOGICAL           STRCMP

      COST_DC = _zero_

      DO ii=1, DB_DC_NIDX(EIDX)
         idx = DB_DC_IDX(ii,EIDX)
         lvl = DC_SIMLVL(idx)
         CALL VECCOPY( RT(1, lvl), Rn, NSIM )
         CALL DC_NORMALIZE( DC_SIMNORM(idx), Rn, tSim, NSIM, Rn )
         cinc = CHISQU_DC( VB, Rn, NSIM, idx )
         COST_DC = COST_DC + cinc

*/////////////////////////////// Output ////////////////////////////////
         IF (WRITE_OUTPUT) THEN
            IF (.NOT.STRCMP( DC_SIM_OUTPUT_FILE(idx), 'none' )) THEN
               CALL DATAOUTEXP( DC_SIM_OUTPUT_FILE(idx),
     &                          NSIM, tSim, Rn )
            ENDIF
         ENDIF
         IF (VB.GE._vb_mf_) THEN
            WRITE(LUN,'(3X, A, A, A, 10X,A, E14.8)') 
     &         'DC ( ',
     &         STRING_POPULATION_LEVEL( lvl ),
     &         ')',
     &         'COST INC = ', cinc
c     &         'DC ( ',
c     &         DEBLANK(HASH_PL_STR( 1, lvl )),
c     &         ')',
c     &         'COST INC = ', cinc
         ENDIF
*/////////////////////////////// Output ////////////////////////////////
      ENDDO


      RETURN
      END




************************************************************************
*                                                                      *
*     - COST II -                                                      *
*                                                                      *
************************************************************************
      DOUBLE PRECISION FUNCTION COST_II( VB, LUN, WRITE_OUTPUT,
     &                                   EIDX, IINT, IIR, LDI )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB, LUN
      LOGICAL           WRITE_OUTPUT
      INTEGER           LDI, EIDX
      DOUBLE PRECISION  IIR(LDI,*)
      DOUBLE PRECISION  IINT(*)
*     .. Outputs
*     .. COMMON data blocks
#include "common-data-ii.h"
#include "common-energy.h"
#include "common-parameter-hash.h"
#include "common-simulation.h"
*     .. Local
      DOUBLE PRECISION  Rn(_maxnsim_)
      DOUBLE PRECISION  cinc
      INTEGER           idx, lvl
      INTEGER           ii, jj, kk
*     .. Functions
      DOUBLE PRECISION  CHISQU_II
c      CHARACTER*5       DEBLANK
      CHARACTER*_mplsl_ STRING_POPULATION_LEVEL
      LOGICAL           STRCMP

      COST_II = _zero_

      DO ii=1, DB_II_NIDX(EIDX)
         idx = DB_II_IDX(ii,EIDX)
         lvl = II_DENOM(idx)
c         IIR(lvl,idx) = VECTRAPEZOID( RT(1,lvl), dT, NSIM )
         CALL VECCOPY( IINT, IIR(1,idx), LDI )

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_) THEN
            WRITE(LUN,'(/A,I4)') '# of II ratios: ',II_NRATIOS(IDX)
         ENDIF
         IF ( VB.GE._vb_hf_ ) THEN
            WRITE(LUN,*) 'denom lvl=',lvl
            WRITE(LUN,*) 'IIR=',IIR(lvl,idx)
         END IF
*/////////////////////////////// Output ////////////////////////////////

         DO kk=1, II_NRATIOS(idx)
            lvl = II_NUMER(kk,idx)
c            IIR(lvl,idx) = VECTRAPEZOID( RT(1,lvl), dT, NSIM )
            cinc = CHISQU_II( VB, LUN, IIR(1,idx), kk, idx )
            COST_II = COST_II + cinc

*/////////////////////////////// Output ////////////////////////////////
            IF (VB.GE._vb_mf_) THEN
               WRITE(LUN,'(3X, A, A, A, A, A, 2X,A, E14.8)') 
     &            'II ( ',
     &            STRING_POPULATION_LEVEL( II_NUMER(kk,idx) ),
     &            'to ',
     &            STRING_POPULATION_LEVEL( II_DENOM(idx)    ),
     &            ')',
     &            'COST INC = ', cinc
c     &            'II ( ',
c     &            DEBLANK(HASH_PL_STR( 1, II_NUMER(kk,idx) )),
c     &            'to ',
c     &            DEBLANK(HASH_PL_STR( 1, II_DENOM(idx)    )),
c     &            ')',
c     &            'COST INC = ', cinc
            ENDIF
            IF ( VB.GE._vb_hf_ ) THEN
               WRITE(LUN,*) 'numer lvl=',lvl
               WRITE(LUN,*) 'IIR=',IIR(lvl,idx)
            END IF
*/////////////////////////////// Output ////////////////////////////////

         END DO
      END DO

      RETURN
      END




************************************************************************
*                                                                      *
*     - WRITE DC SIMULATION OUTPUT FILES -                             *
*                                                                      *
************************************************************************
      SUBROUTINE WRITE_DC_OUTPUT( VB, LUN, EIDX,
     &                            TSIM, NPULSE, RPULSE, NCOND,
     &                            NLVLS, NSIM, LDN )
      IMPLICIT NONE
*     .. Input
      INTEGER           VB, LUN
      INTEGER           EIDX
      DOUBLE PRECISION  TSIM(*)
      DOUBLE PRECISION  NPULSE(LDN,*), RPULSE(LDN,*), NCOND(LDN,*)
      INTEGER           NLVLS, NSIM, LDN
*     .. Output
*     .. COMMON data
#include "common-energy.h"
#include "common-control.h"
*     .. Local
      CHARACTER         energy_string*10
      CHARACTER*40      fname_pulse, fname_rad, fname_cw, fname_cond
      CHARACTER*40      tstring1, tstring2
*     .. Functions
      CHARACTER*40      STRDCAT

*     .. Produce simulation time vector, for output
c      DO ii=1, NSIM
c         tSim(ii) = dT * (ii-1)
c      END DO

      WRITE(energy_string, '(F8.3)') UNIQUE_ENERGY(EIDX) * 1000
      tstring1 = STRDCAT('output-simpulse-', energy_string)
      fname_pulse = STRDCAT( tstring1, 'mJ.dat' )
      tstring1 = STRDCAT('output-simrad-', energy_string)
      fname_rad   = STRDCAT( tstring1, 'mJ.dat' )
      tstring1 = STRDCAT('output-simcond-', energy_string)
      fname_cond  = STRDCAT( tstring1, 'mJ.dat' )

      IF ( VB.GE._vb_mf_ ) THEN
         WRITE(LUN,*) ' '
         WRITE(LUN,*) 'Filename (PULSE):     ', fname_pulse
         WRITE(LUN,*) 'Filename (RADIATIVE): ', fname_rad
         WRITE(LUN,*) 'Filename (COND):      ', fname_cond
      END IF

      CALL DATAOUTSIM(fname_pulse, tSim,
     &                NPULSE, NLVLS, NSIM, _maxnlvls_, OUTPUT_SKIP)
      CALL DATAOUTSIM(fname_rad,   tSim,
     &                RPULSE, NLVLS, NSIM, _maxnlvls_, OUTPUT_SKIP)
      IF ( OUTPUT_COND ) THEN
         CALL DATAOUTSIM(fname_cond,  tSim,
     &                   NCOND, NLVLS,  NSIM, _maxnlvls_, 1)
      END IF

      RETURN
      END



************************************************************************
*                                                                      *
*     - WRITE CW SIMULATION OUTPUT FILES -                             *
*                                                                      *
************************************************************************
      SUBROUTINE WRITE_CW_OUTPUT( VB, LUN, EIDX,
     &                               TSIM, RCW,
     &                               NLVLS, NSIM, LDN )
      IMPLICIT NONE
*     .. Input
      INTEGER           VB, LUN
      INTEGER           EIDX
      DOUBLE PRECISION  TSIM(*)
      DOUBLE PRECISION  RCW(LDN,*)
      INTEGER           NLVLS, NSIM, LDN
*     .. Output
*     .. COMMON data
#include "common-data-cw.h"
#include "common-control.h"
*     .. Local
      CHARACTER         energy_string*10
      CHARACTER*40      fname_pulse, fname_rad, fname_cw, fname_cond
      CHARACTER*40      tstring1, tstring2
*     .. Functions
      CHARACTER*40      STRDCAT

*     .. Produce simulation time vector, for output
c      DO jj=1, NSIM
c         tSim(jj) = dT * (jj-1)
c      END DO

      WRITE(energy_string, '(F8.3)') CW_POWER(EIDX) * 1000
      tstring1 = STRDCAT('output-simcw-', energy_string)
      fname_cw = STRDCAT( tstring1, 'mW.dat' )

      IF ( VB.GE._vb_mf_ ) THEN
         WRITE(LUN,*) 'Filename (CW):        ', fname_cw
      END IF

      CALL DATAOUTSIM(fname_cw, tSim,
     &                RCW, NLVLS, NSIM, _maxnlvls_, OUTPUT_SKIP)

      RETURN
      END




