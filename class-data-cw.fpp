#include "lanthanide.h"


***********************************************************************
*                                                                     *
*     - ZEROIZE CW -                                                  *
*                                                                     *
***********************************************************************
c      SUBROUTINE ZEROIZE_CWDATAS()
      SUBROUTINE INITIALIZE_DATA_CW()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-data-cw.h"


      N_CWDATAS = 0

      RETURN
      END



***********************************************************************
*                                                                     *
*     MANAGE CW DATA RECORDS                                          *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION PARSE_CWDATA_RECORD( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*)
      CHARACTER         VALUE*(*)
*     .. CW data record
*     .. NOTE: Multiplied by 20 (or so) so that we don't over-run the
*     ..       buffer by simply commenting out the II_add_record
      DOUBLE PRECISION  CWR_RADINT(20*_maxnlvls_)
      LOGICAL           CWR_RADINTSET
      DOUBLE PRECISION  CWR_POWER,    CWR_LAMBDA,    CWR_RADIUS
      LOGICAL           CWR_POWERSET, CWR_LAMBDASET, CWR_RADIUSSET
      DOUBLE PRECISION  CWR_SIGMA
      LOGICAL           CWR_SIGMASET
      INTEGER           CWR_LEVEL(20*_maxnlvls_), CWR_NLVLS
      INTEGER           CWR_DENOM
      LOGICAL           CWR_DENOMSET

      SAVE              CWR_RADINT
      SAVE              CWR_RADINTSET
      SAVE              CWR_POWER,    CWR_LAMBDA,    CWR_RADIUS
      SAVE              CWR_POWERSET, CWR_LAMBDASET, CWR_RADIUSSET
      SAVE              CWR_SIGMA
      SAVE              CWR_SIGMASET
      SAVE              CWR_LEVEL, CWR_NLVLS
      SAVE              CWR_DENOM, CWR_DENOMSET

*     .. Functions
      LOGICAL           STRCMP
      LOGICAL           INDEX_POPULATION_LEVEL
*     .. Local
      DOUBLE PRECISION  vard
      INTEGER           vari
      INTEGER           ii


*     .. Set initial return value
      PARSE_CWDATA_RECORD = .TRUE.


      IF ( STRCMP(SYMBOL, 'CW_level') ) THEN
         IF ( .NOT.INDEX_POPULATION_LEVEL( VALUE, vari ) )
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     &      CALL CRITICAL('Illegal or non-existent CW level')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c         READ(VALUE,*) vari
         IF ( CWR_NLVLS.EQ.0 ) THEN
            CWR_NLVLS = CWR_NLVLS+1
            CWR_LEVEL(CWR_NLVLS) = vari
            CWR_RADINTSET = .FALSE.
         ELSEIF ( CWR_LEVEL(CWR_NLVLS).EQ.0 ) THEN
            CWR_LEVEL(CWR_NLVLS) = vari
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ELSEIF ( .NOT.CWR_RADINTSET ) THEN
c            WRITE(*,*) 'NLVLS=', CWR_NLVLS
c            WRITE(*,*) 'LEVEL=', CWR_LEVEL(CWR_NLVLS)
            CALL CRITICAL('No radiative intensity input')
         ELSEIF ( (CWR_NLVLS+1).GT._maxncwdatas_ ) THEN
            CALL CRITICAL('Exceeded max. number of CW data entries')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
            CWR_NLVLS = CWR_NLVLS+1
            CWR_LEVEL(CWR_NLVLS) = vari
            CWR_RADINTSET = .FALSE.
         ENDIF
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'CW_intensity') ) THEN
         READ(VALUE,*) vard

         IF ( CWR_NLVLS.EQ.0 ) THEN
            CWR_NLVLS = CWR_NLVLS+1
            CWR_RADINT(CWR_NLVLS) = vard
            CWR_RADINTSET = .TRUE.
         ELSEIF ( .NOT.CWR_RADINTSET ) THEN
            CWR_RADINT(CWR_NLVLS) = vard
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         ELSEIF ( CWR_LEVEL(CWR_NLVLS).EQ.0 ) THEN
            CALL CRITICAL('No radiative energy level input')
         ELSEIF ( (CWR_NLVLS+1).GT._maxncwdatas_ ) THEN
            CALL CRITICAL('Exceeded max. number of CW data entries')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
            CWR_NLVLS = CWR_NLVLS+1
            CWR_RADINT(CWR_NLVLS) = vard
            CWR_RADINTSET = .TRUE.
         ENDIF
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF (    STRCMP(SYMBOL, 'CW_denominator')
     &        .OR.STRCMP(SYMBOL, 'CW_denom') ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF ( .NOT.INDEX_POPULATION_LEVEL( VALUE, vari ) )
     &      CALL CRITICAL('Illegal or non-existent II level')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         CWR_DENOM = vari
         CWR_DENOMSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'CW_power') ) THEN
         READ(VALUE,*) vard
         CWR_POWER = vard
         CWR_POWERSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'CW_lambda') ) THEN
         READ(VALUE,*) vard
         CWR_LAMBDA = vard
         CWR_LAMBDASET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'CW_radius') ) THEN
         READ(VALUE,*) vard
         CWR_RADIUS = vard
         CWR_RADIUSSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'CW_sigma') ) THEN
         READ(VALUE,*) vard
         CWR_SIGMA = vard
         CWR_SIGMASET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////



      ELSEIF ( STRCMP(SYMBOL, 'CW_add_record') ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF ( CWR_LEVEL(CWR_NLVLS).EQ.0 ) THEN
            CALL CRITICAL('No radiative energy level input')
         ELSEIF ( .NOT.CWR_RADINTSET ) THEN
            CALL CRITICAL('No radiative intensity input')
         ELSEIF ( .NOT.CWR_POWERSET ) THEN
            CALL CRITICAL('No radiative power input')
         ELSEIF ( .NOT.CWR_LAMBDASET ) THEN
            CALL CRITICAL('No radiative wavelength input')
         ELSEIF ( .NOT.CWR_RADIUSSET ) THEN
            CALL CRITICAL('No focus radius set')
         ELSEIF ( .NOT.CWR_SIGMASET ) THEN
            CALL CRITICAL('No radiative intensity Std. Dev. input')
         ELSEIF ( CWR_NLVLS.LT.2 ) THEN
            CALL CRITICAL('2 or more intensities required')
         ELSEIF ( .NOT.CWR_DENOMSET ) THEN
            CALL CRITICAL('No CW denominator input')
         ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

         CALL ADD_CWDATASET( VB,
     &                       CWR_LEVEL, CWR_NLVLS, CWR_RADINT,
     &                       CWR_POWER, CWR_LAMBDA, CWR_RADIUS,
     &                       CWR_SIGMA, CWR_DENOM )

         CWR_NLVLS = 0
         DO ii=1,_maxnlvls_
            CWR_RADINT(ii) = _zero_
            CWR_LEVEL(ii) = 0
         END DO
         CWR_RADINTSET = .FALSE.
         CWR_POWERSET  = .FALSE.
         CWR_LAMBDASET = .FALSE.
         CWR_RADIUSSET = .FALSE.
         CWR_SIGMASET  = .FALSE.
         CWR_DENOMSET  = .FALSE.



      ELSEIF ( STRCMP(SYMBOL, 'RESET') ) THEN
         CWR_NLVLS = 0
         DO ii=1,_maxnlvls_
            CWR_RADINT(ii) = _zero_
            CWR_LEVEL(ii) = 0
         END DO
         CWR_RADINTSET = .FALSE.
         CWR_POWERSET  = .FALSE.
         CWR_LAMBDASET = .FALSE.
         CWR_RADIUSSET = .FALSE.
         CWR_SIGMASET  = .FALSE.
         CWR_DENOMSET  = .FALSE.

         PARSE_CWDATA_RECORD = .FALSE.



      ELSE
         PARSE_CWDATA_RECORD = .FALSE.

      ENDIF


      RETURN
      END




***********************************************************************
*                                                                     *
*     ADD A CW DATA SET                                               *
*                                                                     *
***********************************************************************
      SUBROUTINE ADD_CWDATASET( VB,
     &                          CWR_LEVEL, CWR_NLVLS, CWR_RADINT,
     &                          CWR_POWER, CWR_LAMBDA, CWR_RADIUS,
     &                          CWR_SIGMA, CWR_DENOM )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      DOUBLE PRECISION  CWR_RADINT(*)
      INTEGER           CWR_LEVEL(*), CWR_NLVLS
      DOUBLE PRECISION  CWR_POWER, CWR_LAMBDA, CWR_RADIUS, CWR_SIGMA
      INTEGER           CWR_DENOM

*     .. COMMON data blocks
#include "common-data-cw.h"

*     .. Local
      INTEGER           low
      INTEGER           idx
      INTEGER           ii, jj

      LOGICAL           lvlset(_maxnlvls_)
      DOUBLE PRECISION  radint(_maxnlvls_)
      INTEGER           numlvls


*     ..
      N_CWDATAS = N_CWDATAS+1
      idx = N_CWDATAS
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   - Error Condition -
      IF ( N_CWDATAS.GT._maxncwdatas_ ) THEN
         CALL CRITICAL('Too many CW data records')
      ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

*     .. Go through the CWR_LEVEL buffer and see how many unique levels
*     .. were actually assigned
      DO ii=1, _maxnlvls_
         lvlset(ii) = .FALSE.
      END DO
      DO ii=1, CWR_NLVLS
         lvlset( CWR_LEVEL(ii) ) = .TRUE.
         radint( CWR_LEVEL(ii) ) = CWR_RADINT(ii)
      END DO
      numlvls = 0
      DO ii=1, _maxnlvls_
         IF ( lvlset(ii) )  numlvls = numlvls+1
      END DO


*     .. 2 or more radiative intensities are guaranteed
*      CW_NRATIOS(N_CWDATAS) = CWR_NLVLS-1
      CW_NRATIOS(idx) = numlvls-1

c*     .. Setup denominator, numerators and ratios
c      DO ii=1, _maxnlvls_
c        IF ( lvlset(ii) ) THEN
c           low=ii
c           EXIT
c        ENDIF
c      END DO
c      CW_DENOM(idx) = low

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF ( .NOT.lvlset( CWR_DENOM ) ) THEN
         CALL CRITICAL('No CW intensity set for denominator level')
      ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      CW_DENOM(idx) = CWR_DENOM

      jj=0
      DO ii=1,_maxnlvls_
c         IF ( lvlset(ii) .AND. (ii.NE.low) ) THEN
         IF ( lvlset(ii) .AND. (ii.NE.CWR_DENOM) ) THEN
            jj=jj+1
*            CW_NUMER(jj,N_CWDATAS)  = CWR_LEVEL(ii)
            CW_NUMER(jj, idx)  = ii
*            CW_RATIOS(jj,N_CWDATAS) = CWR_RADINT(ii)/CWR_RADINT(low)
c            CW_RATIOS(jj, idx) = radint(ii)/radint(low)
            CW_RATIOS(jj, idx) = radint(ii)/radint(CWR_DENOM)
         END IF
      END DO

*     ..
      CW_POWER(idx)  = CWR_POWER
      CW_LAMBDA(idx) = CWR_LAMBDA
      CW_RADIUS(idx) = CWR_RADIUS
      CW_SIGMA(idx)  = CWR_SIGMA
c      WRITE(*,*) 'SIGMA=',CW_SIGMA(idx)


      RETURN
      END




***********************************************************************
*                                                                     *
*     - CALCULATE CHI-SQUARED -                                       *
*                                                                     *
***********************************************************************
c      DOUBLE PRECISION FUNCTION CHISQU_CW( VB, RCW, B, K, IDX )
      DOUBLE PRECISION FUNCTION CHISQU_CW( VB, RCW, IDX )
      IMPLICIT NONE
*     .. Input variables
      INTEGER            VB
      INTEGER            IDX
      DOUBLE PRECISION   RCW( * )
c      DOUBLE PRECISION   B(*), K(*)

*     .. COMMON data blocks
#include "common-data-cw.h"

*     .. Local variables
      DOUBLE PRECISION   R(_maxnlvls_)

      DOUBLE PRECISION   simratio, expratio
      DOUBLE PRECISION   cost, ccost
      INTEGER            nlvl, dlvl
      INTEGER            ii, jj
      CHARACTER*5        nstr, dstr

c     Ex:  index array mapping points in Exp. data to sim
c      ixExGn(ii) = NINT( tExGn(ii)/tSmDel )

c      This is currently being called within RUNSIM
c      CALL ERYB_RADIATIVE_OUTPUTS( B, K, NCW(1,nN), R )


      ccost=_zero_

*/////////////////////////////// Output ////////////////////////////////
c      IF (VB.GE._vb_mf_)  WRITE(*,*) 'CW_NRATIOS=',CW_NRATIOS(IDX)
*/////////////////////////////// Output ////////////////////////////////

      DO ii=1,CW_NRATIOS(IDX)
         nlvl = CW_NUMER(ii,IDX)
         dlvl = CW_DENOM(IDX)

c         simratio = RCW( CW_NUMER(ii,IDX) ) / RCW( CW_DENOM(IDX) )
         simratio = RCW( nlvl ) / RCW( dlvl )
         expratio = CW_RATIOS(ii,IDX)

*        .. Old way
c         cost = ( simratio - expratio )**2 / CW_SIGMA(IDX)**2
*        .. New way
         cost = ( 1 - (simratio / expratio) )**2 / CW_SIGMA(IDX)**2
         ccost = ccost + cost

*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_hf_) THEN
            WRITE(*,*) 'simratio=',simratio
            WRITE(*,*) 'expratio=',expratio
            WRITE(*,*) 'sigma=',CW_SIGMA(IDX)
         END IF
         IF (VB.GE._vb_mf_) THEN
            IF (nlvl.EQ._n6_) nstr = 'Green'
            IF (nlvl.EQ._n5_) nstr = 'Red  '
            IF (nlvl.EQ._n3_) nstr = '976  '
            IF (nlvl.EQ._n2_) nstr = 'NIR  '
   
            IF (dlvl.EQ._n6_) dstr = 'Green'
            IF (dlvl.EQ._n5_) dstr = 'Red  '
            IF (dlvl.EQ._n3_) dstr = '976  '
            IF (dlvl.EQ._n2_) dstr = 'NIR  '

            WRITE(*,'(A, A, A, A, E15.8, 4X,A, E15.8)')
     &              nstr, ' to ', dstr, '=', simratio,
     &              'experimental= ', expratio

         END IF
*/////////////////////////////// Output ////////////////////////////////
      END DO

      CHISQU_CW = ccost

      RETURN
      END



