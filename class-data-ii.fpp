#include "lanthanide.h"


***********************************************************************
*                                                                     *
*     - ZEROIZE II -                                                  *
*                                                                     *
***********************************************************************
c      SUBROUTINE ZEROIZE_IIDATAS()
      SUBROUTINE INITIALIZE_DATA_II()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-data-ii.h"


      N_IIDATAS = 0

      RETURN
      END



***********************************************************************
*                                                                     *
*     MANAGE II DATA RECORDS                                          *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION PARSE_IIDATA_RECORD( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*)
      CHARACTER         VALUE*(*)
*     .. II Data Record
*     .. NOTE: Multiplied by 20 (or so) so that we don't over-run the
*     ..       buffer by simply commenting out the II_add_record
      DOUBLE PRECISION  IIR_RADINT(20*_maxnlvls_)
      LOGICAL           IIR_RADINTSET
      DOUBLE PRECISION  IIR_ENERGY,    IIR_LAMBDA,    IIR_RADIUS
      LOGICAL           IIR_ENERGYSET, IIR_LAMBDASET, IIR_RADIUSSET
      DOUBLE PRECISION  IIR_SIGMA
      LOGICAL           IIR_SIGMASET
      INTEGER           IIR_LEVEL(20*_maxnlvls_), IIR_NLVLS
      INTEGER           IIR_DENOM
      LOGICAL           IIR_DENOMSET

      SAVE              IIR_RADINT
      SAVE              IIR_RADINTSET
      SAVE              IIR_ENERGY,    IIR_LAMBDA,    IIR_RADIUS
      SAVE              IIR_ENERGYSET, IIR_LAMBDASET, IIR_RADIUSSET
      SAVE              IIR_SIGMA
      SAVE              IIR_SIGMASET
      SAVE              IIR_LEVEL, IIR_NLVLS
      SAVE              IIR_DENOM, IIR_DENOMSET

*     .. Functions
      LOGICAL           STRCMP
      LOGICAL           INDEX_POPULATION_LEVEL
*     .. Local
      DOUBLE PRECISION  vard
      INTEGER           vari
      INTEGER           ii


*     .. Set initial return value
      PARSE_IIDATA_RECORD = .TRUE.


      IF ( STRCMP(SYMBOL, 'II_level') ) THEN
c         READ(VALUE,*) vari
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF ( .NOT.INDEX_POPULATION_LEVEL( VALUE, vari ) )
     &      CALL CRITICAL('Illegal or non-existent II level')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

         IF ( IIR_NLVLS.EQ.0 ) THEN
            IIR_NLVLS = IIR_NLVLS+1
            IIR_LEVEL(IIR_NLVLS) = vari
            IIR_RADINTSET = .FALSE.
         ELSEIF ( IIR_LEVEL(IIR_NLVLS).EQ.0 ) THEN
            IIR_LEVEL(IIR_NLVLS) = vari
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ELSEIF ( .NOT.IIR_RADINTSET ) THEN
c            WRITE(*,*) 'NLVLS=', CWR_NLVLS
c            WRITE(*,*) 'LEVEL=', CWR_LEVEL(CWR_NLVLS)
            CALL CRITICAL('No integrated intensity input')
         ELSEIF ( (IIR_NLVLS+1).GT._maxniidatas_ ) THEN
            CALL CRITICAL('Exceeded max. number of II data entries')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
            IIR_NLVLS = IIR_NLVLS+1
            IIR_LEVEL(IIR_NLVLS) = vari
            IIR_RADINTSET = .FALSE.
         ENDIF
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'II_intensity') ) THEN
         READ(VALUE,*) vard

         IF ( IIR_NLVLS.EQ.0 ) THEN
            IIR_NLVLS = IIR_NLVLS+1
            IIR_RADINT(IIR_NLVLS) = vard
            IIR_RADINTSET = .TRUE.
         ELSEIF ( .NOT.IIR_RADINTSET ) THEN
            IIR_RADINT(IIR_NLVLS) = vard
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ELSEIF ( IIR_LEVEL(IIR_NLVLS).EQ.0 ) THEN
            CALL CRITICAL('No integrated intensity energy level input')
         ELSEIF ( (IIR_NLVLS+1).GT._maxniidatas_ ) THEN
            CALL CRITICAL('Exceeded max. number of II data entries')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
            IIR_NLVLS = IIR_NLVLS+1
            IIR_RADINT(IIR_NLVLS) = vard
            IIR_RADINTSET = .TRUE.
         ENDIF
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////


      
      ELSEIF (    STRCMP(SYMBOL, 'II_denominator')
     &        .OR.STRCMP(SYMBOL, 'II_denom') ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF ( .NOT.INDEX_POPULATION_LEVEL( VALUE, vari ) )
     &      CALL CRITICAL('Illegal or non-existent II level')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         IIR_DENOM = vari
         IIR_DENOMSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'II_energy') ) THEN
         READ(VALUE,*) vard
         IIR_ENERGY = vard
         IIR_ENERGYSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'II_lambda') ) THEN
         READ(VALUE,*) vard
         IIR_LAMBDA = vard
         IIR_LAMBDASET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'II_radius') ) THEN
         READ(VALUE,*) vard
         IIR_RADIUS = vard
         IIR_RADIUSSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'II_sigma') ) THEN
         READ(VALUE,*) vard
         IIR_SIGMA = vard
         IIR_SIGMASET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'II_add_record') ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF ( IIR_LEVEL(IIR_NLVLS).EQ.0 ) THEN
            CALL CRITICAL('No II energy level input')
         ELSEIF ( .NOT.IIR_RADINTSET ) THEN
            CALL CRITICAL('No II input')
         ELSEIF ( .NOT.IIR_ENERGYSET ) THEN
            CALL CRITICAL('No II pulse energy input')
         ELSEIF ( .NOT.IIR_LAMBDASET ) THEN
            CALL CRITICAL('No II wavelength input')
c         ELSEIF ( .NOT.IIR_RADIUSSET ) THEN
c            CALL CRITICAL('No II focus radius set')
         ELSEIF ( .NOT.IIR_SIGMASET ) THEN
            CALL CRITICAL('No II Std. Dev. input')
         ELSEIF ( IIR_NLVLS.LT.2 ) THEN
            CALL CRITICAL('2 or more II intensities required')
         ELSEIF ( .NOT.IIR_DENOMSET ) THEN
            CALL CRITICAL('No II denominator input')
         ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

         CALL ADD_IIDATASET( VB,
     &                       IIR_LEVEL,  IIR_NLVLS, IIR_RADINT,
     &                       IIR_ENERGY, IIR_LAMBDA, IIR_RADIUS,
     &                       IIR_SIGMA,  IIR_DENOM )

         IIR_NLVLS = 0
         DO ii=1,_maxnlvls_
            IIR_RADINT(ii) = _zero_
            IIR_LEVEL(ii) = 0
         END DO
         IIR_RADINTSET = .FALSE.
         IIR_ENERGYSET = .FALSE.
         IIR_LAMBDASET = .FALSE.
         IIR_RADIUSSET = .FALSE.
         IIR_SIGMASET  = .FALSE.
         IIR_DENOMSET  = .FALSE.



      ELSEIF ( STRCMP(SYMBOL, 'RESET') ) THEN
         IIR_NLVLS = 0
         DO ii=1,_maxnlvls_
            IIR_RADINT(ii) = _zero_
            IIR_LEVEL(ii) = 0
         END DO
         IIR_RADINTSET = .FALSE.
         IIR_ENERGYSET = .FALSE.
         IIR_LAMBDASET = .FALSE.
         IIR_RADIUSSET = .FALSE.
         IIR_SIGMASET  = .FALSE.
         IIR_DENOMSET  = .FALSE.

         PARSE_IIDATA_RECORD = .FALSE.



      ELSE
         PARSE_IIDATA_RECORD = .FALSE.

      ENDIF


      RETURN
      END




************************************************************************
*                                                                      *
*     - ADD AN II DATA SET -                                           *
*                                                                      *
************************************************************************
      SUBROUTINE ADD_IIDATASET( VB,
     &                          IIR_LEVEL,  IIR_NLVLS,  IIR_RADINT,
     &                          IIR_ENERGY, IIR_LAMBDA, IIR_RADIUS,
     &                          IIR_SIGMA,  IIR_DENOM )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      DOUBLE PRECISION  IIR_RADINT(*)
      INTEGER           IIR_LEVEL(*), IIR_NLVLS
      DOUBLE PRECISION  IIR_ENERGY, IIR_LAMBDA, IIR_RADIUS, IIR_SIGMA
      INTEGER           IIR_DENOM

*     .. COMMON data blocks
#include "common-data-ii.h"


*     .. Local
      INTEGER           low
      INTEGER           idx
      INTEGER           ii, jj

      LOGICAL           lvlset(_maxnlvls_)
      DOUBLE PRECISION  radint(_maxnlvls_)
      INTEGER           numlvls


*     ..
      N_IIDATAS = N_IIDATAS+1
      idx = N_IIDATAS
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   - Error Condition -
      IF ( N_IIDATAS.GT._maxniidatas_ ) THEN
         CALL CRITICAL('Too many II data records')
      ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

*     .. Go through the IIR_LEVEL buffer and see how many unique levels
*     .. were actually assigned
      DO ii=1, _maxnlvls_
         lvlset(ii) = .FALSE.
      END DO
      DO ii=1, IIR_NLVLS
         lvlset( IIR_LEVEL(ii) ) = .TRUE.
         radint( IIR_LEVEL(ii) ) = IIR_RADINT(ii)
      END DO
      numlvls = 0
      DO ii=1, _maxnlvls_
         IF ( lvlset(ii) )  numlvls = numlvls+1
      END DO


*     .. 2 or more radiative intensities are guaranteed
      II_NRATIOS(idx) = numlvls-1


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF ( .NOT.lvlset( IIR_DENOM ) ) THEN
         CALL CRITICAL('No intensity set for denominator level')
      ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      II_DENOM(idx) = IIR_DENOM

      ii=0
      DO jj=1,_maxnlvls_
c         IF ( lvlset(jj) .AND. (jj.NE.low) ) THEN
         IF ( lvlset(jj) .AND. (jj.NE.IIR_DENOM) ) THEN
            ii=ii+1
*            II_NUMER(ii, idx)  = IIR_LEVEL(jj)
            II_NUMER(ii, idx)  = jj
*            II_RATIOS(ii, idx) = IIR_RADINT(jj)/IIR_RADINT(low)
c            II_RATIOS(ii, idx) = radint(jj)/radint(low)
            II_RATIOS(ii, idx) = radint(jj)/radint(IIR_DENOM)
         END IF
      END DO

*     ..
      II_ENERGY(idx) = IIR_ENERGY
      II_LAMBDA(idx) = IIR_LAMBDA
      II_RADIUS(idx) = IIR_RADIUS
      II_SIGMA(idx)  = IIR_SIGMA
c      WRITE(*,*) 'SIGMA=',II_SIGMA(idx)

      CALL SET_UNIQUE_ENERGY( VB, 'II', idx, IIR_ENERGY )

      RETURN
      END




***********************************************************************
*                                                                     *
*     - CALCULATE CHI-SQUARED -                                       *
*                                                                     *
***********************************************************************
      DOUBLE PRECISION FUNCTION CHISQU_II( VB, LUN, R, II, IDX )
      IMPLICIT NONE
*     .. Input variables
      INTEGER            VB, LUN
      INTEGER            II, IDX
      DOUBLE PRECISION   R(*)

*     .. COMMON data blocks
#include "common-data-ii.h"

*     .. Local variables
      DOUBLE PRECISION   simratio, expratio
      DOUBLE PRECISION   cost, ccost
c      INTEGER            ii, jj, kk
      INTEGER            nlvl, dlvl
      CHARACTER*5        nstr, dstr


*     ..
      ccost=_zero_

      nlvl = II_NUMER(ii,IDX)
      dlvl = II_DENOM(IDX)

c      simratio = R( II_NUMER(ii,IDX) ) / R( II_DENOM(IDX) )
      simratio = R( nlvl ) / R( dlvl )
      expratio = II_RATIOS(ii,IDX)

*     .. Old way
c      cost = ( simratio - expratio )**2 / II_SIGMA(IDX)**2
*     .. New way
      cost = ( 1 - (simratio / expratio) )**2 / II_SIGMA(IDX)**2
      ccost = ccost + cost

*/////////////////////////////// Output ////////////////////////////////
      IF (VB.GE._vb_mf_) THEN
         IF (nlvl.EQ._n6_) nstr = 'Green'
         IF (nlvl.EQ._n5_) nstr = 'Red  '
         IF (nlvl.EQ._n3_) nstr = '976  '
         IF (nlvl.EQ._n2_) nstr = 'NIR  '

         IF (dlvl.EQ._n6_) dstr = 'Green'
         IF (dlvl.EQ._n5_) dstr = 'Red  '
         IF (dlvl.EQ._n3_) dstr = '976  '
         IF (dlvl.EQ._n2_) dstr = 'NIR  '

         WRITE(LUN,'(A, A, A, A, E15.8, 4X,A, E15.8)') 
     &             nstr, ' to ', dstr, '=', simratio,
     &             'experimental= ', expratio

      END IF
*/////////////////////////////// Output ////////////////////////////////

      CHISQU_II = ccost

      RETURN
      END







