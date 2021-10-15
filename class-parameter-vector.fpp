#include "lanthanide.h"


***********************************************************************
*                                                                     *
*     - ZEROIZE PARAMETER VECTOR -                                    *
*                                                                     *
***********************************************************************
c      SUBROUTINE ZEROIZE_PVEC( )
      SUBROUTINE INITIALIZE_PARAMETER_VECTOR( )
      IMPLICIT NONE
*     .. COMMON data block
#include "common-parameter-vector.h"


      P_LOG = .FALSE.

      RETURN
      END



************************************************************************
      SUBROUTINE SET_PVEC_LOG( BOOL )
      IMPLICIT NONE
*     .. Inputs
      LOGICAL           BOOL
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-vector.h"

*     .. Functions

      P_LOG = BOOL

      RETURN
      END



************************************************************************
*     .. Not actually called anywhere at this time
      LOGICAL FUNCTION PARSE_PVEC( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*), VALUE*(*)
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-vector.h"

*     .. Local
      LOGICAL           varl
*     .. Functions
      LOGICAL           STRCMP


      PARSE_PVEC = .FALSE.
      IF ( STRCMP(SYMBOL, 'LOGARITHMIC') ) THEN
         READ(VALUE, *) varl
         P_LOG = varl
         PARSE_PVEC = .TRUE.

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_) CALL WRITE_SYMBOL_LVALUE( SYMBOL, varl )
*/////////////////////////////// Output ////////////////////////////////

      ENDIF


      RETURN
      END



************************************************************************
      DOUBLE PRECISION FUNCTION PVEC_FT( ORIG )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  ORIG
*     .. COMMON data block
#include "common-parameter-vector.h"

*     .. Local
*     .. Functions
      LOGICAL           STRCMP
      

c      WRITE(*,*) 'P_LOG=', P_LOG

      IF ( P_LOG ) THEN
         PVEC_FT = DLOG10( ORIG )
      ELSE
         PVEC_FT = ORIG
      END IF


      RETURN
      END



************************************************************************
      DOUBLE PRECISION FUNCTION PVEC_IT( TRANS )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  TRANS
*     .. COMMON data block
#include "common-parameter-vector.h"

*     .. Local
*     .. Functions
      LOGICAL           STRCMP
      

      IF ( P_LOG ) THEN
         PVEC_IT = 10**TRANS
      ELSE
         PVEC_IT = TRANS
      END IF


      RETURN
      END



************************************************************************
*                                                                      *
*     - ZEROIZE PARAMETER VECTOR -                                     *
*                                                                      *
************************************************************************
      SUBROUTINE TOGGLE_MASKED_PARAMETERS( VP_VEC, VP_LO, VP_HI, NPS )
      IMPLICIT NONE

*     .. Inputs
      DOUBLE PRECISION  VP_VEC(*), VP_LO(*), VP_HI(*)
      INTEGER           NPS

*     .. COMMON data block
#include "common-parameter-initial.h"
#include "common-parameter-vector.h"
#include "common-energy.h"


*     .. Functions
      DOUBLE PRECISION  PVEC_FT, PVEC_IT
*     .. Local
      INTEGER           ii, jj


*     .. Steps involved:
*     .. 1. Extract the current parameter vector and save off the
*           individual components in the pvector common block.
*           Included in this is the need to create an N0 for each
*           unique decay curve or integrated intensity energy.
*           This will account for the situation where we had multiple,
*           independent population levels as variable parameters, but
*           they are now held fixed after the parameters are toggled.
*     .. 2. Toggle the variable parameters according to the XOR masks
*     .. 3. Rebuild the parameter vector from our new pvector common block.


*     .. 1. Extract dynamic parameters from the parameter vector
      DO jj=1, N_UNIQUE_ENERGY
         CALL EXTRACT_PVEC( jj, VP_VEC,  P_PIVAL(1, jj) )
      ENDDO


*     .. 2. Apply the toggle (XOR) masks; see class-parameter.fpp
      CALL MASK_PARAMETERS()


*     .. 3.  Rebuild the parameter vector
      CALL MAKE_VP_VEC( VP_VEC, VP_LO, VP_HI, NPS )


      RETURN
      END



************************************************************************
*                                                                      *
*     - POPULATE PINITIALS DATA STRUCTURES -                           *
*                                                                      *
************************************************************************
      SUBROUTINE COPY_PINITIALS( )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs

*     .. COMMON data block
#include "common-parameter-initial.h"
#include "common-parameter-vector.h"
#include "common-energy.h"

*     .. Functions
      DOUBLE PRECISION  PVEC_FT, PVEC_IT
*     .. Local
      INTEGER           ii, jj, kk


*     .. 2. Copy parameters ...........................
      CALL VECCOPY( PIVAL(1,1), P_PIVAL(1,1), _max_n_params_ )
      DO jj=2,N_UNIQUE_ENERGY
         CALL VECCOPY( PIVAL(1,jj), P_PIVAL(1,jj), _max_n_params_ )
      END DO


      RETURN
      END




************************************************************************
*                                                                      *
*     - CONSTRUCT PARAMETER VECTOR -                                   *
*       Input rate constants, initial populations and laser            *
*       laser absorption and return in a parameter vector              *
*                                                                      *
************************************************************************
      SUBROUTINE MAKE_VP_VEC( VP_VEC, VP_LO, VP_HI, NPS )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs
      DOUBLE PRECISION  VP_VEC(*), VP_LO(*), VP_HI(*)
      INTEGER           NPS

*     .. COMMON data block
#include "common-parameter-initial.h"
#include "common-parameter-vector.h"
#include "common-energy.h"


*     .. Functions
      DOUBLE PRECISION  PVEC_FT, PVEC_IT

*     .. Local
      INTEGER           ii, jj


*     .. Setup initial parameter vector
      NPS=0

*     .. 1. Initial populations .......................
      DO ii=1, _max_n_params_
*           .. One or more decay curve files
*           .. NOTE: We check that N_UNIQUE_ENERGY.GT.0 to verify that
*           ..       we have more than just CW data (DC or II)
         P_INDEP(ii) =       PXSET(ii) 
     &                .AND. .NOT.SCALE_TO_ENERGY
     &                .AND.  N_UNIQUE_ENERGY.GT.0
      ENDDO

*     .. 2. Initial populations .......................
      DO ii=1, _max_n_params_
         IF ( PPARA(ii).EQV..TRUE. ) THEN
            IF ( P_INDEP(ii) ) THEN
               DO jj=1, N_UNIQUE_ENERGY
                  NPS=NPS+1
                  VP_VEC(NPS) = PVEC_FT( P_PIVAL(ii,jj) )
                  VP_LO(NPS)  = PVEC_FT( PIVLO(ii) )
                  VP_HI(NPS)  = PVEC_FT( PIVHI(ii) )
               ENDDO
            ELSE
               NPS=NPS+1
               VP_VEC(NPS) = PVEC_FT( P_PIVAL(ii,LOWEST_ENERGY) )
               VP_LO(NPS)  = PVEC_FT( PIVLO(ii) )
               VP_HI(NPS)  = PVEC_FT( PIVHI(ii) )
            ENDIF
         ENDIF
      ENDDO

      P_NPS = NPS


      RETURN
      END



************************************************************************
*                                                                      *
*                                                                      *
************************************************************************
c      SUBROUTINE EXTRACT_PVEC( EIDX, VP, NPS, B, K, N0, D )
c      SUBROUTINE EXTRACT_PVEC( EIDX, VP, NPS,  P )
      SUBROUTINE EXTRACT_PVEC( EIDX, VP,  P )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  VP(*)
      INTEGER           EIDX
*     .. Outputs
c      DOUBLE PRECISION  B(*),    K(*),    N0(*),    D
      DOUBLE PRECISION  P(*)

*     .. COMMON data block
#include "common-parameter-initial.h"
#include "common-parameter-vector.h"
#include "common-energy.h"

*     .. Functions
      LOGICAL           STRCMP
      DOUBLE PRECISION  PVEC_FT, PVEC_IT
*     .. Local
      INTEGER           ii
      INTEGER           NPS


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   NOTE: This logical condition is complicated owing to the fact
*         that we need to accomodate the situation where we have only
*         CW data and N_UNIQUE_ENERGY = 0
*              .. 1 <= EIDX <= N_UNIQUE_ENERGY
      IF ( EIDX.LT.1 .OR.
     &     (N_UNIQUE_ENERGY.GT.0 .AND. EIDX.GT.N_UNIQUE_ENERGY) ) THEN
         CALL CRITICAL( 'EIDX out of range (EXTRACT_PVEC)' )
      END IF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

*     .. 1. Copy the initial values, covering anything which is not dynamic
      CALL VECCOPY( P_PIVAL(1,EIDX), P, _max_n_params_ )

*     .. 2. Extract dynamic parameters from the parameter vector
      NPS=0
      DO ii=1, _max_n_params_
         IF ( PPARA(ii).EQV..TRUE. ) THEN
            IF ( P_INDEP(ii) ) THEN
*              .. In this case, we have independent parameters
               NPS=NPS+EIDX
               P(ii) = PVEC_IT( VP(NPS) )
               NPS=NPS+(N_UNIQUE_ENERGY-EIDX)
            ELSE
               NPS=NPS+1
               P(ii) = PVEC_IT( VP(NPS) )
            END IF
         END IF
      END DO

*     .. 3. Check if we need to scale any N0 to the input pulse energy
*     CRAP, N0(LOWEST_ENERGY) won't work
*     Instead, multiply by itself since there ARE NOT independent
*     parameters if we are scaling to the energy.
*     We will have set N0(ii) either from N0IVAL or extracted it from
*     the parameter vector.
      IF ( SCALE_TO_ENERGY ) THEN
         DO ii=1, _max_n_params_
            IF ( PXSET(ii) ) THEN
               P(ii) =  UNIQUE_ENERGY(EIDX)
     &                   * ( P(ii) / UNIQUE_ENERGY(LOWEST_ENERGY) )
            END IF
         END DO
      END IF


*     .. 4. Now, check for fixed ratios of parameters
      DO ii=1, _max_n_params_
         IF ( PRSET(ii)  )  P(ii)  = PRVAL(ii)  * P(  PRLVL(ii)  )
      END DO


*     .. 5. Check for fixed sums of parameters
      DO ii=1, _max_n_params_
         IF ( PSSET(ii)  )  P(ii)  = PSVAL(ii)  - P(  PSLVL(ii)  )
      END DO


      RETURN
      END




************************************************************************
*                                                                      *
*                                                                      *
************************************************************************
c      SUBROUTINE MAKEN0( N0, N0PUL, N0CW )
      SUBROUTINE MAKE_CW_N0( PVEC, PVEC_CW )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  PVEC(*)
*     .. Outputs
c      DOUBLE PRECISION  N0PUL(*), N0CW(*)
      DOUBLE PRECISION  PVEC_CW(*)

*     .. COMMON data blocks
#include "common-parameter-initial.h"

*     .. Local
      INTEGER           ii


c      CALL VECCOPY( N0, N0PUL, NLVLS )
      CALL VECCOPY( PVEC, PVEC_CW, _max_n_params_ )
      DO ii=1, _max_n_params_
         IF ( PXSET(ii) )   PVEC_CW(ii) = _zero_
      END DO


      RETURN
      END





