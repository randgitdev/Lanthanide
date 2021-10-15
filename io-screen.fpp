#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - DUMP PARAMETERS TO SCREEN -                                   *
*                                                                     *
***********************************************************************
      SUBROUTINE SCREENDUMP( LUN, RC, ITER, COST, VPVEC )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           LUN
      DOUBLE PRECISION  VPVEC(*)
      DOUBLE PRECISION  COST
      INTEGER           RC, ITER

*     .. COMMON data blocks
#include "common-parameter-initial.h"
#include "common-energy.h"
#include "common-simplex.h"
*     .....................

*     .. Local
      DOUBLE PRECISION  PVEC(_max_n_params_, _max_n_energies_)

      CHARACTER*(_mpsl_+3)    stra, strb, strc, strd
      CHARACTER*(_mpsl_+3+10) strla, strlb, strle
      INTEGER           ii, jj, kk
*     .. Functions
c      CHARACTER*_mplsl_     STRING_POPULATION_LEVEL
      CHARACTER*_mpsl_        STRING_PARAMETER
c      CHARACTER*(_mbrsl_+3) STRCAT
      CHARACTER*(_mpsl_+3+11) STRCAT
      LOGICAL               STRCMP

      INTEGER               STRLEN



      CALL EXTRACT_PVEC( 1, VPVEC, PVEC(1,1) )
      DO jj=2, N_UNIQUE_ENERGY
         CALL EXTRACT_PVEC( jj, VPVEC, PVEC(1,jj) )
      ENDDO

*///////////////////////////////////////////////////////////////////////
c      WRITE(lun,13000) ITER, COST
      WRITE(lun,'(/A, I4, 4X, A, I6, 4X, A, E14.8)') 
     &             'Repeat count: ', RC, 
     &             'Iteration: ', ITER,
     &             'Cost: ', COST

c13000 FORMAT ( 1X, 'Iteration: ', I6, 6X, 'Cost: ', E14.8 )

      IF (SMPLX_BOUND) THEN
         WRITE(lun,'(/, 16X,A,  19X,A,   7X,A,    7X,A)') 
     &                 'Value','IValue','LBound','UBound'
         WRITE(lun,'(   16X,A,  19X,A,   7X,A,    7X,A)') 
     &                 '-----','------','------','------'
      ELSE
         WRITE(lun,'(/, 16X,A, 19X,A)') 'Value','IValue'
         WRITE(lun,'(   16X,A, 19X,A)') '-----','------'
      ENDIF


*///////////////////////////////////////////////////////////////////////
*     .. Dump initial populations
      WRITE(lun,'(/ A)') '# Initial population ( n(0) ):'
      DO ii=1, _max_n_params_
         IF ( PINCL(ii).AND. .NOT.PXSET(ii) ) THEN
            stra=STRING_PARAMETER( ii )
            strb=STRCAT( stra, '=' )
            strc=' '//strb
            IF ( PPARA(ii) )  strc='*'//strb
            strd=' '//strc
            IF ( PMASK(ii) )  strd='@'//strc
c            WRITE(lun,'(1X, A, E13.6)')
            IF (SMPLX_BOUND) THEN
               WRITE(lun,
     &            '(1X,A, E13.6,      12X,E11.4,   2X,E11.4,  2X,E11.4)') 
     &              strd, PVEC(ii,1), PIVAL(ii,1), PIVLO(ii), PIVHI(ii)
            ELSE
               WRITE(lun,
     &            '(1X,A, E13.6,      12X,E11.4)') 
     &              strd, PVEC(ii,1), PIVAL(ii,1)
            ENDIF
         END IF
      END DO

      IF ( SCALE_TO_ENERGY ) THEN
         WRITE(lun,'(A, E10.3)') '-- Scaled to population at E=',
     &                               UNIQUE_ENERGY(LOWEST_ENERGY)
      END IF

      IF (N_UNIQUE_ENERGY.GE.1) THEN
         WRITE(lun,'(A)') '#      [[Pulse Energy]]'
      ENDIF

c      DO jj=1, N_UNIQUE_ENERGY
c         CALL EXTRACT_PVEC( jj, VPVEC, PVEC )
c         WRITE(lun,'(A, E10.3, A)') '-- @ pulse energy=',
c     &                           UNIQUE_ENERGY(jj)*1000, 'mJ'
      DO ii=1, _max_n_params_
         IF ( PINCL(ii).AND.PXSET(ii) ) THEN
            DO jj=1, N_UNIQUE_ENERGY
               strla=STRING_PARAMETER( ii )
c               strb=STRCAT( stra, '=' )
               strlb=strla
               strla=' '//strlb
               IF ( PPARA(ii) )  strla='*'//strlb
               strlb=' '//strla
               IF ( PMASK(ii) )  strlb='@'//strla
c               WRITE(lun,'(3X, A, E13.6)')
               WRITE(strle,'(E9.3)') UNIQUE_ENERGY(jj)
               strla=STRCAT( strlb, '[' )
               strlb=STRCAT( strla, strle )
               strla=STRCAT( strlb, ']=' )

               IF (SMPLX_BOUND) THEN
                  WRITE(lun,
     &           '(1X,A,  E13.6,    2X,E11.4,     2X,E11.4,  2X,E11.4)')
     &             strla, PVEC(ii,jj), PIVAL(ii,jj), PIVLO(ii), PIVHI(ii)
               ELSE
                  WRITE(lun,
     &           '(1X,A,  E13.6,    2X,E11.4)') 
     &             strla, PVEC(ii,jj), PIVAL(ii,jj)
               ENDIF
            ENDDO
         ENDIF
      ENDDO

c      ENDDO



      WRITE(lun,'(//A)') ' '



      RETURN
      END




***********************************************************************
*                                                                     *
*     - WRITE SYMBOLS / VALUES TO SCREEN -                            *
*                                                                     *
***********************************************************************
      SUBROUTINE WRITE_SYMBOL_DVALUE( SYMBOL, VARD )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
      DOUBLE PRECISION  VARD
*     .. Local
      CHARACTER*30      svalue
*     .. Functions
      CHARACTER*72      STRCAT

      WRITE(svalue,*) VARD
      WRITE(*,*) STRCAT( SYMBOL, ' = '//svalue)

      RETURN
      END


*.......................................................................
      SUBROUTINE WRITE_SYMBOL_IVALUE( SYMBOL, VARI )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
      INTEGER           VARI
*     .. Local
      CHARACTER*30      svalue
*     .. Functions
      CHARACTER*72      STRCAT

      WRITE(svalue,*) VARI
      WRITE(*,*) STRCAT( SYMBOL, ' = '//svalue)

      RETURN
      END



*.......................................................................
      SUBROUTINE WRITE_SYMBOL_LVALUE( SYMBOL, VARL )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         SYMBOL*(*)
      LOGICAL           VARL
*     .. Local
      CHARACTER*30      svalue
*     .. Functions
      CHARACTER*72      STRCAT

      WRITE(svalue,*) VARL
      WRITE(*,*) STRCAT( SYMBOL, ' = '//svalue)

      RETURN
      END


