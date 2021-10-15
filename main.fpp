#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     PROGRAM LANTHANIDE                                              *
*                                                                     *
***********************************************************************
      PROGRAM LANTHANIDE
      IMPLICIT NONE
*     ..
*     .. COMMON data blocks
*     ................
#include "common-control.h"
#include "common-parameter-initial.h"
*     ................
*     ..
*     .. Simulation data structures
c      DOUBLE PRECISION  B(_maxnbs_), K(_maxnks_), N0(_maxnlvls_), D
*     ..
*     .. Simplex data structures
      INTEGER           NPS
      DOUBLE PRECISION  P(_maxnps_), PINI(_maxnps_)
      DOUBLE PRECISION  PLO(_maxnps_), PHI(_maxnps_)
      DOUBLE PRECISION  VPVEC(_maxnps_)
      DOUBLE PRECISION  VPVEC_LO(_maxnps_), VPVEC_HI(_maxnps_)
c      DOUBLE PRECISION  FINAL_PVEC(_maxnps_)
c      DOUBLE PRECISION  FINAL_COST
c      INTEGER           FINAL_ITERS
c      INTEGER           REPEAT_COUNT
c      COMMON /FINAL_OUTPUT/
c     &                  FINAL_PVEC, FINAL_COST, FINAL_ITERS, REPEAT_COUNT
*     ..
*     .. Inputs
      CHARACTER*80      CMDFILE
      INTEGER           NARGS
      LOGICAL           CFEXISTS
*     ..
*     .. Outputs
      CHARACTER*80      FILEOUT
c      INTEGER           OUTPUT_SKIP, VERBOSE
c      LOGICAL           OUTPUT
*     ..
*     .. Functions
      EXTERNAL          FSIGNAL_SIGINT
*     .. Local variables
c      INTEGER           REPEAT_TOTAL
c      INTEGER           REPEAT_COUNT
      INTEGER           num, ii, jj
      INTEGER           LUN



*****************************  BEGIN CODE  *****************************
*     .. Install signal handler
      CALL SIGNAL( 2, FSIGNAL_SIGINT )

*     .. Get the command file from the command line
      NARGS = IARGC()
      WRITE(*,*) 'NARGS=',NARGS
      IF( NARGS.LT.1 ) CALL CRITICAL('No command file given')
      IF( NARGS.GT.1 ) CALL CRITICAL('Too many command line arguments')
      CALL GETARG(1, CMDFILE)
      INQUIRE(FILE=CMDFILE, EXIST=CFEXISTS)
      IF( .NOT.CFEXISTS ) CALL CRITICAL('Command file does not exist')


*     .. INITIALIZE CLASS DATA STRUCTURES .....................
      CALL INITIALIZE_PARAMETER( )
      CALL INITIALIZE_PARAMETER_VECTOR( )
      CALL INITIALIZE_UNIQUE_ENERGY( )
*     ..
      CALL INITIALIZE_DATA_DC( )
      CALL INITIALIZE_DATA_II( )
      CALL INITIALIZE_DATA_CW( )
*     ..
      CALL INITIALIZE_SIMULATION()
      CALL INITIALIZE_SIMPLEX()
*     ..
      CALL INITIALIZE_CONTROL()
      CALL INITIALIZE_QUANTUM_EFFICIENCY()
*     ........................................................

*     .. Call the command file parser
      CALL CMDPARSE( CMDFILE )

      CALL INITIALIZE_MODEL( )
      CALL SCALE_INITIAL_PARAMETERS( )
*     ..
      CALL TEST_PARAMETER_ERRORS()
*     ..
      CALL CALCULATE_SIMULATION_TIME( )
*     ..
*     .. Copy the initial parameter values into the P_ vector
*     .. initial arrays
c      CALL TEST_PARAMETER_ERRORS()
      CALL COPY_PINITIALS()
*     ..
*     .. Create initial parameter vector, PINI
      CALL MAKE_VP_VEC( VPVEC, VPVEC_LO, VPVEC_HI, NPS )
*     ..
*     .. Set output to screen
      LUN = 6
*     ..
*     .. Call the Nelder-Mead simplex minimization routine
*     .. Repeat, if desired
      CALL VECCOPY( VPVEC, FINAL_VPVEC, NPS )
      DO REPEAT_COUNT=1, REPEAT_TOTAL

         CALL SIMPLEX( VERBOSE, LUN, REPEAT_COUNT,
     &                 VPVEC, VPVEC_LO, VPVEC_HI, NPS,
     &                 FINAL_VPVEC,
     &                 FINAL_COST,
     &                 FINAL_ITERS )

         IF ( REPEAT_COUNT.EQ.REPEAT_TOTAL)  EXIT

         CALL VECCOPY( FINAL_VPVEC, VPVEC, NPS )
         CALL TOGGLE_MASKED_PARAMETERS( VPVEC, VPVEC_LO, VPVEC_HI, NPS )

      END DO


*/////////////////////////////// Output ////////////////////////////////
c      LUN = 6
      CALL RUNSIM( _vb_mf_, LUN, .FALSE., FINAL_VPVEC,  FINAL_COST )
      CALL SCREENDUMP( LUN, REPEAT_TOTAL,
     &                 FINAL_ITERS, FINAL_COST, FINAL_VPVEC )


      IF (OUTPUT) THEN
*     .. Set output to file
         LUN = 40
         OPEN ( UNIT=LUN,
     &          FILE='output-screen.dat',
     &          FORM='FORMATTED',
     &          STATUS='UNKNOWN',
     &          ACCESS='SEQUENTIAL')

*     .. Run the simulation one final time to generate output files
         CALL RUNSIM( _vb_mf_, LUN, .TRUE., FINAL_VPVEC,  FINAL_COST )

         CALL SCREENDUMP( LUN, REPEAT_TOTAL,
     &                    FINAL_ITERS, FINAL_COST, FINAL_VPVEC )

*     .. Output the normalized versions of our decay curve data
*     .. for comparative analysis
         CALL OUTPUT_DCDATAS()

         CLOSE( LUN )

      END IF
*/////////////////////////////// Output ////////////////////////////////


      STOP
      END


************************************************************************
*                         - END MAIN PROGRAM -                         *
************************************************************************




************************************************************************
*                                                                      *
*     - FSIGNAL SIGINT -                                               *
*                                                                      *
************************************************************************
      SUBROUTINE FSIGNAL_SIGINT()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-control.h"

*     ..
*     .. Local
      INTEGER           LUN

*/////////////////////////////// Output ////////////////////////////////
      LUN = 6
      WRITE(*,*) 'ABORTING...'
      CALL RUNSIM( _vb_mf_, LUN, .FALSE., FINAL_VPVEC,  FINAL_COST )
      CALL SCREENDUMP( LUN, REPEAT_COUNT,
     &                 FINAL_ITERS, FINAL_COST, FINAL_VPVEC )

      IF (OUTPUT) THEN
*     .. Set output to file
         LUN = 40
         OPEN ( UNIT=LUN,
     &          FILE='output-screen.dat',
     &          FORM='FORMATTED',
     &          STATUS='UNKNOWN',
     &          ACCESS='SEQUENTIAL')

*     .. Run the simulation one final time to generate output files
         CALL RUNSIM( _vb_mf_, LUN, .TRUE., FINAL_VPVEC,  FINAL_COST )
         CALL SCREENDUMP( LUN, REPEAT_COUNT,
     &                    FINAL_ITERS, FINAL_COST, FINAL_VPVEC )

*     .. Output the normalized versions of our decay curve data
*     .. for comparative analysis
         CALL OUTPUT_DCDATAS()
         CLOSE( LUN )

      END IF
*/////////////////////////////// Output ////////////////////////////////


      STOP

      RETURN
      END


