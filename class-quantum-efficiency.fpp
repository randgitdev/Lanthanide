#include "lanthanide.h"

************************************************************************
*                                                                      *
*     - INITIALIZE QUANTUM EFFICIENCY DATA STRUCTURES -                *
*                                                                      *
************************************************************************
      SUBROUTINE INITIALIZE_QUANTUM_EFFICIENCY( )
      IMPLICIT NONE
*     .. COMMON data block
#include "common-quantum-efficiency.h"
*     .. Local
      INTEGER           ii

      DO ii=1, _maxnlvls_
         QE_LEVEL(ii) = .FALSE.
      ENDDO

      RETURN
      END




************************************************************************
*                                                                      *
*     - TEST FOR QUANTUM EFFICIENCY INPUT ERRORS -                     *
*                                                                      *
************************************************************************
      SUBROUTINE TEST_QUANTUM_EFFICIENCY_ERRORS( )
      IMPLICIT NONE



      RETURN
      END




************************************************************************
*                                                                      *
*     - PARSE QUANTUM EFFICIENCY -                                     *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION PARSE_QUANTUM_EFFICIENCY( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*)
      CHARACTER         VALUE*(*)
*     .. Outputs
*     .. COMMON data block
#include "common-quantum-efficiency.h"
*     .. Functions
      LOGICAL           STRCMP
      LOGICAL           INDEX_POPULATION_LEVEL
      LOGICAL           INDEX_PARAMETER
*     .. Local
      INTEGER           vari
      INTEGER           ii


*     .. Set initial return value
      PARSE_QUANTUM_EFFICIENCY = .TRUE.

      IF ( STRCMP(SYMBOL, 'QE_level') ) THEN
         IF ( .NOT.INDEX_POPULATION_LEVEL( VALUE, vari ) ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            CALL CRITICAL('Illegal or non-existent energy level (QE)')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
            QE_LEVEL(vari) = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
            IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////
         ENDIF


      ELSEIF (    STRCMP(SYMBOL, 'QE_denominator') 
     &        .OR.STRCMP(SYMBOL, 'QE_denom') ) THEN
         IF ( .NOT.INDEX_PARAMETER( VALUE, vari ) ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            CALL CRITICAL('Illegal or non-existent QE denominator')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
            QE_DENOM = vari
*/////////////////////////////// Output ////////////////////////////////
            IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////
         ENDIF


      ELSE
         PARSE_QUANTUM_EFFICIENCY = .FALSE.


      ENDIF


      RETURN
      END



************************************************************************
*                                                                      *
*     - REPORT QUANTUM EFFICIENCIES -                                  *
*                                                                      *
************************************************************************
      SUBROUTINE REPORT_QUANTUM_EFFICIENCY( VB, LUN, IINT, P_VEC )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB, LUN
      DOUBLE PRECISION  IINT(*), P_VEC(*)
*     .. Outputs
*     .. COMMON data blocks
c#include "common-data-ii.h"
#include "common-quantum-efficiency.h"
*     .. Local
      CHARACTER*_mpsl_  dstr
      CHARACTER*_mplsl_ nstr
      INTEGER           ii
*     .. Functions
      CHARACTER*_mpsl_  STRING_PARAMETER
      CHARACTER*_mplsl_ STRING_POPULATION_LEVEL

c      dstr = STRING_PARAMETER( _np2_0_ )
      dstr = STRING_PARAMETER( QE_denom )

      IF ( VB.GE._vb_mf_ ) THEN
         WRITE(LUN,'(/A)') 'Quantum Efficiency:'
         DO ii=1, _maxnlvls_
            IF ( QE_LEVEL(ii) ) THEN
               nstr = STRING_POPULATION_LEVEL( ii )
               WRITE(LUN,'(3X,A,        F5.3)') 
     &                     nstr//'= ', IINT(ii) / P_VEC(QE_denom)
            ENDIF
         ENDDO
      ENDIF

      RETURN
      END



