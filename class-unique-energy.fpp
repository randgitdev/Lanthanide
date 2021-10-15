#include "lanthanide.h"


************************************************************************
*                                                                      *
*     - ZEROIZE PARAMETER ARRAYS -                                     *
*                                                                      *
************************************************************************
c      SUBROUTINE ZEROIZE_UNIQUE_ENERGIES( )
      SUBROUTINE INITIALIZE_UNIQUE_ENERGY( )
      IMPLICIT NONE
*     .. Inputs
*     .. Outputs

      CALL SET_UNIQUE_ENERGY( 0, 'RESET', 0, _zero_ )

      RETURN
      END



************************************************************************
      INTEGER FUNCTION INDEX_UNIQUE_ENERGY( ENERGY )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  ENERGY
*     .. Outputs
*     .. COMMON data block
#include "common-energy.h"
*     .. Functions
      LOGICAL           DEQUIV
*     .. Local
      INTEGER           eidx, ii
      
      eidx = 0
      DO ii=1,N_UNIQUE_ENERGY
         IF ( DEQUIV(ENERGY, UNIQUE_ENERGY(ii)) ) THEN
            eidx = ii
         ENDIF
      ENDDO

      INDEX_UNIQUE_ENERGY = eidx

      RETURN
      END



************************************************************************
*                                                                      *
*     - SET UNIQUE ENERGY -                                            *
*       We maintain a database of unique energies used for both        *
*       decay curve (DC or pulse) data and integrated intensity (II)   *
*       data.  In the database, we also maintain arrays of indices     *
*       referring back into the decay curve or integrated intensity    *
*       datas for easy comparison when we calculate the costs in       *
*       our RUNSIM subroutine.                                         *
*                                                                      *
************************************************************************
      SUBROUTINE SET_UNIQUE_ENERGY( VB, DATABASE, IDX, ENERGY )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         DATABASE*(*)
      DOUBLE PRECISION  ENERGY
      INTEGER           IDX
*     .. Outputs
*     .. COMMON data block
#include "common-energy.h"
*     .. Functions
      LOGICAL           STRCMP
      LOGICAL           DEQUIV
*     .. Local
      INTEGER           eidx, ii, jj

*     .. eidx = energy index, i.e. index into unique energy database
*     .. IDX = index into either the dc or ii datastructure

*     .. Test against all existing energies to determine uniqueness
      eidx = 0
      DO jj=1,N_UNIQUE_ENERGY
*         IF ( ENERGY.EQ.UNIQUE_ENERGY(jj) ) THEN
         IF ( DEQUIV(ENERGY, UNIQUE_ENERGY(jj)) ) THEN
            eidx = jj
         ENDIF
      ENDDO
*     .. If unique, add it to the end of the list
      IF ( eidx.EQ.0 ) THEN
         N_UNIQUE_ENERGY = N_UNIQUE_ENERGY + 1
         eidx = N_UNIQUE_ENERGY
         UNIQUE_ENERGY(eidx) = ENERGY
      ENDIF
*     .. Sort for lowest energy
*     .. NOTE: This is the least efficient but most straight-forward
*     ..       and safe way to accomplish this.
      LOWEST_ENERGY=1
      DO jj=1, N_UNIQUE_ENERGY
         IF ( UNIQUE_ENERGY(jj).LT.UNIQUE_ENERGY(LOWEST_ENERGY) ) THEN
            LOWEST_ENERGY=jj
         ENDIF
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  WRITE(*,*) 'UNIQUE ENERGY=',UNIQUE_ENERGY(jj)
*/////////////////////////////// Output ////////////////////////////////
      END DO


*     .. Determine, based upon the database, what to do next
      IF      ( STRCMP(DATABASE, 'PULSE') ) THEN
         DB_DC_NIDX(eidx) = DB_DC_NIDX(eidx) + 1
         ii = DB_DC_NIDX(eidx)
         DB_DC_IDX(ii, eidx) = IDX
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_hf_)  WRITE(*,*) 'ADDING PULSE ENERGY=', ENERGY
         IF (VB.GE._vb_hf_)  WRITE(*,*) 'eidx=',eidx
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(DATABASE, 'II')    ) THEN
         DB_II_NIDX(eidx) = DB_II_NIDX(eidx) + 1
         ii = DB_II_NIDX(eidx)
         DB_II_IDX(ii, eidx) = IDX


      ELSEIF ( STRCMP(DATABASE, 'N0')    ) THEN 
      

      ELSEIF ( STRCMP(DATABASE, 'RESET') ) THEN
         N_UNIQUE_ENERGY = 0
*         DO jj=1, _maxndcfiles_+_maxniidatas_
         DO jj=1, _max_n_energies_
            DB_DC_NIDX(jj) = 0
            DB_II_NIDX(jj)  = 0
         ENDDO
         SCALE_TO_ENERGY = .FALSE.
         LOWEST_ENERGY = 0


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   - Error Condition -
      ELSE
         CALL CRITICAL( 'Illegal Database - This should NEVER happen' )
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      ENDIF


      RETURN
      END



************************************************************************
*                                                                      *
*     - PARSE ENERGY -                                                 *
*       Test whether the input SYMBOL is parse-able as an input        *
*       for the energy database and set the corresponding              *
*       energy database value according to VALUE if so.                *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION PARSE_ENERGY( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*), VALUE*(*)
*     .. Outputs
*     .. COMMON data block
#include "common-energy.h"

*     .. Functions
      LOGICAL           STRCMP
*     .. Local
      LOGICAL           varl

*     ..
      PARSE_ENERGY = .TRUE.

      IF ( STRCMP( symbol, 'SCALE_TO_ENERGY' ) ) THEN
         READ(value, *) varl
c         CALL SET_SCALE_TO_ENERGY( varl )
         SCALE_TO_ENERGY = varl

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_) CALL WRITE_SYMBOL_LVALUE( symbol, varl )
*/////////////////////////////// Output ////////////////////////////////

      ELSE
         PARSE_ENERGY = .FALSE.

      ENDIF


      RETURN
      END


