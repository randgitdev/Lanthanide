#include "lanthanide.h"


************************************************************************
*                                                                      *
*     - DEFAULT CONTROL PARAMETERS -                                   *
*                                                                      *
************************************************************************
      SUBROUTINE INITIALIZE_CONTROL()
      IMPLICIT NONE

*     .. COMMON data block
#include "common-control.h"
*     ....................

      OUTPUT = .TRUE.
      OUTPUT_SKIP = 10
      OUTPUT_COND = .FALSE.
      VERBOSE = _vb_hf_
      REPEAT_TOTAL = 1


      RETURN
      END




************************************************************************
*                                                                      *
*     - PARSE CONTROL -                                                *
*       Test whether the input SYMBOL is parse-able as an input        *
*       for any control parameters and set the corresponding           *
*       control value according to VALUE if so.                        *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION PARSE_CONTROL( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*), VALUE*(*)
*     .. Outputs

*     .. COMMON data block
#include "common-control.h"
*     ....................

*     .. Functions
      LOGICAL           STRCMP
*     .. Local
      LOGICAL           varl
      INTEGER           vari

*     ..
      PARSE_CONTROL = .TRUE.

      IF (     STRCMP( symbol, 'OUTPUT' ) ) THEN
         READ(value, *) varl
         OUTPUT = varl
*/////////////////////////////// Output ////////////////////////////////
         IF (VERBOSE.GE._vb_mf_) CALL WRITE_SYMBOL_LVALUE( symbol, varl )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP( symbol, 'OUTPUT_skip' ) ) THEN
         READ(value, *) vari
         OUTPUT_SKIP = vari
*/////////////////////////////// Output ////////////////////////////////
         IF (VERBOSE.GE._vb_mf_) CALL WRITE_SYMBOL_IVALUE( symbol, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP( symbol, 'OUTPUT_COND' ) ) THEN
         READ(value, *) varl
         OUTPUT_COND = varl
*/////////////////////////////// Output ////////////////////////////////
         IF (VERBOSE.GE._vb_mf_) CALL WRITE_SYMBOL_LVALUE( symbol, varl )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP( symbol, 'VERBOSE' ) ) THEN
         READ(value, *) vari
         VERBOSE = vari
*/////////////////////////////// Output ////////////////////////////////
         IF (VERBOSE.GE._vb_mf_) CALL WRITE_SYMBOL_IVALUE( symbol, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF (    STRCMP( symbol, 'REPEAT' )
     &        .OR.STRCMP( symbol, 'REPEAT_TOTAL') ) THEN
         READ(value, *) vari
         REPEAT_TOTAL = vari
*/////////////////////////////// Output ////////////////////////////////
         IF (VERBOSE.GE._vb_mf_) CALL WRITE_SYMBOL_IVALUE( symbol, vari )
*/////////////////////////////// Output ////////////////////////////////



      ELSE
         PARSE_CONTROL = .FALSE.

      END IF


      RETURN
      END



