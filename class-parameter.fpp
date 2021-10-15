#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - INITIALIZE PARAMETER ARRAY -                                  *
*                                                                     *
***********************************************************************
      SUBROUTINE INITIALIZE_PARAMETER( )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-initial.h"
*     .....

*     .. Local
      INTEGER           ii, jj

      DO ii=1, _max_n_params_
c         CALL RESET_FLAGS_N0( ii )

c         N0IVAL(ii) = _zero_
c         N0ISET(ii) = .FALSE.
         DO jj=1, _max_n_energies_
            PIVAL(ii,jj) = _zero_
            PISET(ii,jj) = .FALSE.
         ENDDO

         PIVLO(ii) = _zero_
         PIVHI(ii) = _huge_
         PPARA(ii) = .FALSE.
         PMASK(ii) = .FALSE.

         PRVAL(ii) = _zero_
         PRSET(ii) = .FALSE.
         PRLVL(ii) = 0
         
         PESET(ii) = .FALSE.
         PXSET(ii) = .FALSE.
         
         PPVAL(ii) = _zero_
         PPSET(ii) = .FALSE.
         
         PSVAL(ii) = _zero_
         PSSET(ii) = .FALSE.
         PSLVL(ii) = 0

         PINCL(ii) = .FALSE.
      END DO

      NLVLS = _maxnlvls_

      RETURN
      END



*.......................................................................
      SUBROUTINE RESET_PARAMETER_FLAGS( IDX )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           IDX
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-initial.h"

*     .. Local
      INTEGER           jj

c      N0ISET(IDX) = .FALSE.
c      DO jj=1,_max_n_energies_
c         N0ISET(IDX,jj) = .FALSE.
c      ENDDO
      PPARA(IDX) = .FALSE.
      PMASK(IDX) = .FALSE.
      
      PRSET(IDX) = .FALSE.
      PESET(IDX) = .FALSE.
      PXSET(IDX) = .FALSE.
      PPSET(IDX) = .FALSE.
      PSSET(IDX) = .FALSE.


      RETURN
      END




************************************************************************
*                                                                      *
*     - TEST FOR LOGICAL ERRORS IN INITIAL PARAMETERS -                *
*                                                                      *
************************************************************************
      SUBROUTINE TEST_PARAMETER_ERRORS( )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs

*     .. COMMON data block
#include "common-parameter-initial.h"
#include "common-parameter-vector.h"
#include "common-energy.h"


*     .. Functions
      LOGICAL           STRCMP
      CHARACTER*_mpsl_  STRING_PARAMETER

      DOUBLE PRECISION  PVEC_FT, PVEC_IT

*     .. Local
      INTEGER           ii, jj, kk


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*     - Error Condition -
      DO ii=1,_max_n_params_
         IF ( (PPARA(ii).OR.PMASK(ii)) .AND. (PRSET(ii).OR.PSSET(ii)) ) THEN
            WRITE(*,'(/A,A)') '@ P index=', STRING_PARAMETER( ii )
            CALL CRITICAL(  'Cannot be both a variable parameter and a '
     &                    //'fixed ratio or sum of another parameter' )
         ELSEIF ( PRSET(ii).AND.PSSET(ii) ) THEN
            WRITE(*,'(/A,A)') '@ P index=', STRING_PARAMETER( ii )
            CALL CRITICAL(  'Cannot be both a '
     &                    //'fixed ratio and sum of another parameter' )
         ELSEIF ( PESET(ii).AND.SCALE_TO_ENERGY ) THEN
            WRITE(*,'(/A,A)') '@ P index=', STRING_PARAMETER( ii )
            CALL CRITICAL(  'Cannot be both SCALE_TO_ENERGY and '
     &                    //'have parameters specified at a '
     &                    //'particular energy' )
         ENDIF
         IF ( PESET(ii) ) THEN
            DO jj=1, N_UNIQUE_ENERGY
               IF (.NOT.PISET(ii,jj) ) THEN
                  WRITE(*,'(/A,A)') '@ P index=', STRING_PARAMETER( ii )
                  WRITE(*,'(A,E10.3)') 'UNIQUE ENERGY=', UNIQUE_ENERGY(jj)
                  CALL CRITICAL(  ' Incomplete specification of '
     &                         // 'populations: populations must be '
     &                         // 'specified for ALL pulse energies.')
               ENDIF
            ENDDO
         ENDIF
      ENDDO
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      RETURN
      END




***********************************************************************
*                                                                     *
*     - MASK PARAMETERS -                                             *
*                                                                     *
***********************************************************************
      SUBROUTINE MASK_PARAMETERS( )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-initial.h"

*     .. Local
      INTEGER           ii

      DO ii=1, _max_n_params_
         PPARA(ii) = PPARA(ii).NEQV.PMASK(ii)
      ENDDO


      RETURN
      END




***********************************************************************
*                                                                     *
*     - SCALE INITIAL PARAMETERS -                                    *
*       For any parameters which scale with energy and have not       *
*       been explicitly set, we must initialize using the lowest      *
*       unique energy and scaling accordingly.                        *
*                                                                     *
***********************************************************************
      SUBROUTINE SCALE_INITIAL_PARAMETERS( )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-initial.h"
#include "common-energy.h"

*     .. Functions
      DOUBLE PRECISION  PVEC_FT, PVEC_IT
*     .. Local
      INTEGER           ii, jj, kk


*     .. 1. Initial populations .......................
      DO ii=1, _max_n_params_
         IF ( PXSET(ii).AND. .NOT.PESET(ii) ) THEN
            DO jj=1, N_UNIQUE_ENERGY
               PIVAL(ii,jj) = PIVAL(ii,jj)
     &             * (UNIQUE_ENERGY(jj) / UNIQUE_ENERGY(LOWEST_ENERGY))
            ENDDO
         ENDIF
      ENDDO

      RETURN
      END




***********************************************************************
*                                                                     *
*     - PARSE N0 -                                                    *
*       Test whether the input SYMBOL is parse-able as a              *
*       parameter and set the corresponding initial                   *
*       parameter according to VALUE if so.                           *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION PARSE_PARAMETERS( VB, SYMBOL, VALUE, IDXAST, IDXAT )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*), VALUE*(*)
      INTEGER           IDXAST, IDXAT
*     .. Outputs
*     .. COMMON data block
#include "common-parameter-initial.h"

*     .. Functions
      INTEGER           INDEX_UNIQUE_ENERGY
      LOGICAL           INDEX_PARAMETER
      LOGICAL           INDEX_PARAMETER_PRIOR
      LOGICAL           INDEX_PARAMETER_RATIO
      LOGICAL           INDEX_PARAMETER_SUM
      LOGICAL           INDEX_PARAMETER_LBOUND
      LOGICAL           INDEX_PARAMETER_UBOUND
      LOGICAL           INDEX_PARAMETER_w_ENERGY
      LOGICAL           STRCMP
      CHARACTER*_mpsl_  STRING_PARAMETER
*     .. Local
      DOUBLE PRECISION  vard, energy
      INTEGER           pidx, eidx, lvl1, lvl2
      INTEGER           ii, jj

*     ..
      PARSE_PARAMETERS = .TRUE.

      IF ( INDEX_PARAMETER(SYMBOL, pidx) ) THEN
         CALL RESET_PARAMETER_FLAGS( pidx )
         READ(VALUE, *) vard
         DO jj=1,_max_n_energies_
            PIVAL(pidx, jj) = vard
c            N0ISET(pidx, jj) = .TRUE.
         ENDDO

         IF ( IDXAST.GT.0 )  PPARA(pidx) = .TRUE.
         IF ( IDXAT.GT.0  )  PMASK(pidx) = .TRUE.

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( INDEX_PARAMETER_w_ENERGY( SYMBOL, pidx, energy) ) THEN
         READ(value, *) vard
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF (energy.LT.1.0D-06 .OR. energy.GT.1.0D-01) THEN
            CALL CRITICAL('N0 energy exceeds bounds')
         ENDIF
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         CALL RESET_PARAMETER_FLAGS( pidx )
         CALL SET_UNIQUE_ENERGY( VB, 'N0', 0, energy )
         eidx = INDEX_UNIQUE_ENERGY( energy )

         PIVAL(pidx, eidx) = vard
         PISET(pidx, eidx) = .TRUE.
         PXSET(pidx) = .TRUE.
         PESET(pidx) = .TRUE.
         IF ( IDXAST.GT.0 )  PPARA(pidx) = .TRUE.
         IF ( IDXAT.GT.0  )  PMASK(pidx) = .TRUE.

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( INDEX_PARAMETER_RATIO( SYMBOL, lvl1, lvl2 ) ) THEN
         READ(value, *) vard
         PRVAL(lvl1) = vard
         PRLVL(lvl1) = lvl2
         PRSET(lvl1) = .TRUE.

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_hf_) THEN
            WRITE(*,*) 'LVL1=',lvl1
            WRITE(*,*) 'LVL2=',lvl2
            WRITE(*,*) 'RATIO=',vard
         END IF
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( symbol, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( INDEX_PARAMETER_SUM( SYMBOL, lvl1, lvl2 ) ) THEN
         READ(value, *) vard
         PSVAL(lvl1) = vard
         PSLVL(lvl1) = lvl2
         PSSET(lvl1) = .TRUE.

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_hf_) THEN
            WRITE(*,*) 'LVL1=',lvl1
            WRITE(*,*) 'LVL2=',lvl2
            WRITE(*,*) 'SUM=',vard
         END IF
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( symbol, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( INDEX_PARAMETER_LBOUND(SYMBOL, pidx) ) THEN
         READ(value, *) vard
         PIVLO(pidx) = vard

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         DO jj=1,_max_n_energies_
            IF ( PIVAL(pidx,jj).LT.PIVLO(pidx) ) THEN
               WRITE(*,'(/A,A)') '@ P index=', STRING_PARAMETER(pidx)
               CALL CRITICAL('Initial parameter exceeds lbound')
            ENDIF
         ENDDO
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



      ELSEIF ( INDEX_PARAMETER_UBOUND(SYMBOL, pidx) ) THEN
         READ(value, *) vard
         PIVHI(pidx) = vard

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         DO jj=1,_max_n_energies_
            IF ( PIVAL(pidx,jj).GT.PIVHI(pidx) ) THEN
               WRITE(*,'(/A,A)') '@ P index=', STRING_PARAMETER(pidx)
               CALL CRITICAL('Initial parameter exceeds ubound')
            END IF
         ENDDO
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



      ELSEIF (    STRCMP( SYMBOL, 'INPUT_LEVEL' )
     &        .OR.STRCMP( SYMBOL, 'ENERGY_INPUT_LEVEL' )
     &        .OR.STRCMP( SYMBOL, 'EXCITATION_LEVEL' ) 
     &        .OR.STRCMP( SYMBOL, 'POWER_DEPENDENT' ) ) THEN

         IF ( INDEX_PARAMETER( VALUE, lvl1 ) ) THEN
            PXSET(lvl1) = .TRUE.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ELSE
            CALL CRITICAL( 'Illegal energy input level' )
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         END IF

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_) THEN
            WRITE(*,*) 'Energy input level=', lvl1
            CALL WRITE_SYMBOL_IVALUE( SYMBOL, lvl1 )
         END IF
*/////////////////////////////// Output ////////////////////////////////



      ELSE
         PARSE_PARAMETERS = .FALSE.

      END IF


      RETURN
      END




************************************************************************
*                                                                      *
*     - INDEX PARAMETER w/ ENERGY -                                    *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER_w_ENERGY( SYMBOL, IDX, ENERGY )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
*     .. Outputs
      INTEGER           IDX
      DOUBLE PRECISION  ENERGY
*     .. Functions
      LOGICAL           STRCMP, STRNCMP
      LOGICAL           INDEX_PARAMETER
*     .. Local
      CHARACTER*30      pm, en
      INTEGER           idxbra, idxket

      idxbra = INDEX( SYMBOL, '[' )
      idxket = INDEX( SYMBOL, ']' )

      INDEX_PARAMETER_w_ENERGY = .TRUE.

      IF      ( idxbra.EQ.0 ) THEN
         INDEX_PARAMETER_w_ENERGY = .FALSE.

      ELSE IF ( idxket.LE.idxbra ) THEN
         INDEX_PARAMETER_w_ENERGY = .FALSE.

      ELSE
         pm = SYMBOL(:idxbra-1)
         en = SYMBOL(idxbra+1:idxket-1)

         IF (.NOT.INDEX_PARAMETER( pm, idx ) ) THEN
            INDEX_PARAMETER_w_ENERGY = .FALSE.
         ELSE
            READ(en, *) ENERGY
         END IF

      END IF

      RETURN
      END




************************************************************************
*                                                                      *
*     - INDEX PARAMETER LOWER BOUND -                                  *
*       Identify LOWER[] declaration for any parameter                 *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER_LBOUND( SYMBOL, IDX )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
*     .. Outputs
      INTEGER           IDX
*     .. Functions
      LOGICAL           STRCMP, STRNCMP
      LOGICAL           INDEX_PARAMETER
*     .. Local
      CHARACTER*20      pm
      INTEGER           idxbra, idxket

      idxbra = INDEX( SYMBOL, '[' )
      idxket = INDEX( SYMBOL, ']' )

      INDEX_PARAMETER_LBOUND = .TRUE.

      IF ( .NOT.STRNCMP( SYMBOL, 'LOWER', 5 ) ) THEN
         INDEX_PARAMETER_LBOUND = .FALSE.

      ELSE IF ( idxbra.EQ.0 ) THEN
         INDEX_PARAMETER_LBOUND = .FALSE.

      ELSE IF ( idxket.LE.idxbra ) THEN
         INDEX_PARAMETER_LBOUND = .FALSE.

      ELSE
         pm = SYMBOL(idxbra+1:idxket-1)

         IF (.NOT.INDEX_PARAMETER( pm, idx ) ) THEN
            INDEX_PARAMETER_LBOUND = .FALSE.
         END IF

      END IF

      RETURN
      END



************************************************************************
*                                                                      *
*     - INDEX PARAMETER UPPER BOUND -                                  *
*       Identify UPPER[] declaration for any parameter                 *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER_UBOUND( SYMBOL, IDX )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
*     .. Outputs
      INTEGER           IDX
*     .. Functions
      LOGICAL           STRCMP, STRNCMP
      LOGICAL           INDEX_PARAMETER
*     .. Local
      CHARACTER*20      pm
      INTEGER           idxbra, idxket

      idxbra = INDEX( SYMBOL, '[' )
      idxket = INDEX( SYMBOL, ']' )

      INDEX_PARAMETER_UBOUND = .TRUE.

      IF ( .NOT.STRNCMP( SYMBOL, 'UPPER', 5 ) ) THEN
         INDEX_PARAMETER_UBOUND = .FALSE.

      ELSE IF ( idxbra.EQ.0 ) THEN
         INDEX_PARAMETER_UBOUND = .FALSE.

      ELSE IF ( idxket.LE.idxbra ) THEN
         INDEX_PARAMETER_UBOUND = .FALSE.

      ELSE
         pm = SYMBOL(idxbra+1:idxket-1)

         IF (.NOT.INDEX_PARAMETER( pm, idx ) ) THEN
            INDEX_PARAMETER_UBOUND = .FALSE.
         END IF

      END IF

      RETURN
      END




***********************************************************************
*                                                                     *
*     - INDEX PARAMETER RATIO -                                       *
*       Converts a RATIO[ numer / denom ] = < ratio > command         *
*       into numerator and denominator levels and sets the            *
*       numerator at each simulation iteration as the fixed ratio     *
*       of the denominator.  This serves the same function as the     *
*       older N1TONP1 AND N1TONP1USE variables.                       *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER_RATIO( SYMBOL, IDX1, IDX2 )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
*     .. Outputs
      INTEGER           IDX1, IDX2
*     .. Local
      INTEGER           idxbra, idxket, idxcom
      CHARACTER*20      numer, denom
*     .. Functions
      LOGICAL           STRCMP, STRNCMP
      LOGICAL           INDEX_PARAMETER

*     .. Example:  RATIO[ n1 / np1 ] = 0.11

      idxbra = INDEX( SYMBOL, '[' )
      idxket = INDEX( SYMBOL, ']' )
      idxcom = INDEX( SYMBOL, '/' )

*     .. Set initial return value
      INDEX_PARAMETER_RATIO = .TRUE.

      IF ( .NOT.STRNCMP( SYMBOL, 'RATIO', 5 ) ) THEN
         INDEX_PARAMETER_RATIO = .FALSE.

      ELSE IF ( idxbra.EQ.0      ) THEN
         INDEX_PARAMETER_RATIO = .FALSE.

      ELSE IF ( idxcom.LE.idxbra ) THEN
         INDEX_PARAMETER_RATIO = .FALSE.

      ELSE IF ( idxket.LE.idxcom ) THEN
         INDEX_PARAMETER_RATIO = .FALSE.

      ELSE
         numer = SYMBOL(idxbra+1:idxcom-1)
         denom = SYMBOL(idxcom+1:idxket-1)

         IF ( .NOT.INDEX_PARAMETER( numer, idx1 ) ) THEN
            INDEX_PARAMETER_RATIO = .FALSE.

         ELSE IF ( .NOT.INDEX_PARAMETER( denom, idx2 ) ) THEN
            INDEX_PARAMETER_RATIO = .FALSE.

         END IF

      END IF

      RETURN
      END




************************************************************************
*                                                                      *
*     - INDEX PARAMETER SUM -                                          *
*       Converts a SUM[ k1 + k2 ] = <sum> command                      *
*       into two levels, k1 and k2.  These are used to constrain       *
*       k1 as the value of <sum> - k2.                                 *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER_SUM( SYMBOL, IDX1, IDX2 )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
*     .. Outputs
      INTEGER           IDX1, IDX2
*     .. Local
      INTEGER           idxbra, idxket, idxcom
      CHARACTER*20      left, right
*     .. Functions
      LOGICAL           STRCMP, STRNCMP
      LOGICAL           INDEX_PARAMETER

*     .. Example:  SUM[ kNR1 + kR1 ] = 2700

      idxbra = INDEX( SYMBOL, '[' )
      idxket = INDEX( SYMBOL, ']' )
      idxcom = INDEX( SYMBOL, '+' )

*     .. Set initial return value
      INDEX_PARAMETER_SUM = .TRUE.

      IF ( .NOT.STRNCMP( SYMBOL, 'SUM', 3 ) ) THEN
         INDEX_PARAMETER_SUM = .FALSE.

      ELSE IF ( idxbra.EQ.0      ) THEN
         INDEX_PARAMETER_SUM = .FALSE.

      ELSE IF ( idxcom.LE.idxbra ) THEN
         INDEX_PARAMETER_SUM = .FALSE.

      ELSE IF ( idxket.LE.idxcom ) THEN
         INDEX_PARAMETER_SUM = .FALSE.

      ELSE
         left  = SYMBOL(idxbra+1:idxcom-1)
         right = SYMBOL(idxcom+1:idxket-1)

         IF      ( .NOT.INDEX_PARAMETER( left,  idx1 ) ) THEN
            INDEX_PARAMETER_SUM = .FALSE.

         ELSE IF ( .NOT.INDEX_PARAMETER( right, idx2 ) ) THEN
            INDEX_PARAMETER_SUM = .FALSE.

         END IF

      END IF

      RETURN
      END




************************************************************************
*                                                                      *
*     - INDEX PARAMETER PRIORS -                                       *
*       Identify PRIOR[] declaration for any parameter                 *
*       NOTE:  Not implemented yet                                     *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER_PRIOR( SYMBOL, IDX )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
*     .. Outputs
      INTEGER           IDX
*     .. Functions
      LOGICAL           STRCMP, STRNCMP
      LOGICAL           INDEX_PARAMETER
*     .. Local
      CHARACTER*20      pm
      INTEGER           idxbra, idxket

      idxbra = INDEX( SYMBOL, '[' )
      idxket = INDEX( SYMBOL, ']' )

      INDEX_PARAMETER_PRIOR = .TRUE.

      IF ( .NOT.STRNCMP( SYMBOL, 'PRIOR', 5 ) ) THEN
         INDEX_PARAMETER_PRIOR = .FALSE.

      ELSE IF ( idxbra.EQ.0 ) THEN
         INDEX_PARAMETER_PRIOR = .FALSE.

      ELSE IF ( idxket.LE.idxbra ) THEN
         INDEX_PARAMETER_PRIOR = .FALSE.

      ELSE
         pm = SYMBOL(idxbra+1:idxket-1)

         IF (.NOT.INDEX_PARAMETER( pm, idx ) ) THEN
            INDEX_PARAMETER_PRIOR = .FALSE.
         END IF

      END IF

      RETURN
      END




***********************************************************************
*                                                                     *
*     - INDEX PARAMETERS -                                            *
*       Converts parameter name into an array index                   *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION INDEX_PARAMETER( PSTRING, PINDEX )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         PSTRING*(*)
*     .. Outputs
      INTEGER           PINDEX
*     .. Functions
      LOGICAL           STRCMP
*     .. COMMON data block
#include "common-parameter-hash.h"

*     .. Local
      INTEGER           ii, jj


*     .. Set initial return value
      INDEX_PARAMETER = .FALSE.

*     ..
      DO jj=1, _max_n_params_
         DO ii=1, _param_hash_variations_
            IF ( STRCMP( PSTRING, HASH_PARAM_STR(ii, jj) ) ) THEN
               PINDEX = jj
               INDEX_PARAMETER = .TRUE.
               EXIT
            END IF
         END DO
         IF ( INDEX_PARAMETER )  EXIT
      END DO


      RETURN
      END




*.......................................................................
      CHARACTER*(*) FUNCTION STRING_PARAMETER( PINDEX )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           PINDEX
*     .. Outputs
*     .. Local
*     .. COMMON data block
#include "common-parameter-hash.h"


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF ( PINDEX.LT.1 .OR. PINDEX.GT._max_n_params_ ) THEN
         CALL CRITICAL( 'Index out of range (STRING_PARAMETER)' )
      END IF
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      STRING_PARAMETER = HASH_PARAM_STR( _param_output_variation_, PINDEX )


      RETURN
      END



***********************************************************************
*                                                                     *
*     - INDEX POPULATION LEVELS -                                     *
*       Converts population level name into an array index            *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION INDEX_POPULATION_LEVEL( PSTRING, PINDEX )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         PSTRING*(*)
*     .. Outputs
      INTEGER           PINDEX
*     .. Functions
      LOGICAL           STRCMP
*     .. COMMON data block
#include "common-parameter-hash.h"

*     .. Local
      INTEGER           ii, jj


*     .. Set initial return value
      INDEX_POPULATION_LEVEL = .FALSE.

*     ..
      DO jj=1, _maxnlvls_
         DO ii=1, _pl_hash_variations_
            IF ( STRCMP( PSTRING, HASH_PL_STR(ii, jj) ) ) THEN
               PINDEX = jj
               INDEX_POPULATION_LEVEL = .TRUE.
               EXIT
            END IF
         END DO
         IF ( INDEX_POPULATION_LEVEL )  EXIT
      END DO


      RETURN
      END




*.......................................................................
      CHARACTER*(*) FUNCTION STRING_POPULATION_LEVEL( PINDEX )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           PINDEX
*     .. Outputs
*     .. Local
*     .. COMMON data block
#include "common-parameter-hash.h"


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF ( PINDEX.LT.1 .OR. PINDEX.GT._maxnlvls_ ) THEN
         CALL CRITICAL( 'Index out of range (STRING_POPULATION_LEVEL)' )
      END IF
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      STRING_POPULATION_LEVEL = HASH_PL_STR( _pl_output_variation_, PINDEX )


      RETURN
      END




