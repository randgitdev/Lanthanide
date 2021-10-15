#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - SET N_DCFILES TO ZERO -                                       *
*                                                                     *
***********************************************************************
c      SUBROUTINE ZEROIZE_DCDATAS()
      SUBROUTINE INITIALIZE_DATA_DC()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-data-dc.h"


      N_DCFILES = 0

      RETURN
      END



***********************************************************************
*                                                                     *
*     - OUTPUT DC DATA -                                              *
*                                                                     *
***********************************************************************
c      SUBROUTINE DC__OUTPUT()
      SUBROUTINE OUTPUT_DCDATAS()
      IMPLICIT NONE
*     .. COMMON data blocks
#include "common-data-dc.h"

*     .. Functions
      LOGICAL           STRCMP
*     .. Local
      INTEGER           ii

      DO ii=1, N_DCFILES
         IF ( .NOT.STRCMP( DC_EXP_OUTPUT_FILE(ii), 'none' ) ) THEN
            CALL DATAOUTEXP( DC_EXP_OUTPUT_FILE(ii), DC_NDATA(ii),
     &                       DC_TIME(1,ii), DC_DATA(1,ii) )
         END IF
      END DO


      RETURN
      END


***********************************************************************
*                                                                     *
*     - CALCULATE DC INDEX -                                          *
*                                                                     *
***********************************************************************
      SUBROUTINE DC__CALCULATE_INDEX( dT )
      IMPLICIT NONE
*     .. Input
      DOUBLE PRECISION  dT

*     .. COMMON data blocks
#include "common-data-dc.h"

*     .. Local
      INTEGER           ii, jj

*     .. Find index into simulation for each experimental data point
      DO jj=1,N_DCFILES
c         WRITE(*,*) 'DC_TIME(0)=',DC_TIME(1,jj)
         DO ii=1,DC_NDATA(jj)
c            DC_SIMIDX(ii,jj) = NINT( DC_TIME(ii,jj)/tSimDelta )
            DC_SIMIDX(ii,jj) = NINT( DC_TIME(ii,jj)/dT ) + 1
c            IF ( DC_SIMIDX(ii,jj).EQ.0 ) WRITE(*,*) 'ZERO INDEX'
         END DO
      END DO


      RETURN
      END




***********************************************************************
*                                                                     *
*     - ADJUST NSIM FOR DC FILES -                                    *
*                                                                     *
***********************************************************************
      SUBROUTINE DC__ADJUST_NSIM( NSIM )
      IMPLICIT NONE
*     .. Input and Output
      INTEGER           NSIM

*     .. COMMON data blocks
#include "common-data-dc.h"


*     .. Local
      INTEGER           ii, jj

*     .. Find the maximum needed simulation index
      DO jj=1,N_DCFILES
c         WRITE(*,*) 'DC_SIMIDX=',DC_SIMIDX(DC_NDATA(jj),jj)
         IF ( DC_SIMIDX(DC_NDATA(jj),jj).GT.NSIM ) THEN
            NSIM = DC_SIMIDX( DC_NDATA(jj),jj )
         ENDIF
      END DO


      RETURN
      END



***********************************************************************
*                                                                     *
*     MANAGE DC DATA FILES                                            *
*                                                                     *
***********************************************************************
      LOGICAL FUNCTION PARSE_DCDATA_RECORD( VB, SYMBOL, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         SYMBOL*(*), VALUE*(*)
*     .. Pulse data file
      CHARACTER*120     DCR_DIR
      LOGICAL           DCR_DIRSET
      CHARACTER*120     DCR_FILE
      LOGICAL           DCR_FILESET
      CHARACTER*120     DCR_EXP_OUTPUT
      LOGICAL           DCR_EXP_OUTPUTSET
      CHARACTER*120     DCR_SIM_OUTPUT
      LOGICAL           DCR_SIM_OUTPUTSET
      CHARACTER*30      DCR_EXP_NORM, DCR_SIM_NORM
      LOGICAL           DCR_EXP_NORMSET, DCR_SIM_NORMSET
      INTEGER           DCR_IFIRST,    DCR_ILAST,    DCR_IDELAY
      INTEGER           DCR_LEVEL
      LOGICAL           DCR_IFIRSTSET, DCR_ILASTSET, DCR_IDELAYSET
      LOGICAL           DCR_LEVELSET
      DOUBLE PRECISION  DCR_ENERGY,    DCR_BKGD,     DCR_SIGMA
      LOGICAL           DCR_ENERGYSET, DCR_BKGDSET,  DCR_SIGMASET

      SAVE              DCR_DIR
      SAVE              DCR_DIRSET
      SAVE              DCR_FILE
      SAVE              DCR_FILESET
      SAVE              DCR_EXP_OUTPUT
      SAVE              DCR_EXP_OUTPUTSET
      SAVE              DCR_SIM_OUTPUT
      SAVE              DCR_SIM_OUTPUTSET
      SAVE              DCR_EXP_NORM,   DCR_SIM_NORM
      SAVE              DCR_EXP_NORMSET, DCR_SIM_NORMSET
      SAVE              DCR_IFIRST,    DCR_ILAST,    DCR_IDELAY
      SAVE              DCR_LEVEL
      SAVE              DCR_IFIRSTSET, DCR_ILASTSET, DCR_IDELAYSET
      SAVE              DCR_LEVELSET
      SAVE              DCR_ENERGY,    DCR_BKGD,     DCR_SIGMA
      SAVE              DCR_ENERGYSET, DCR_BKGDSET,  DCR_SIGMASET

*     .. Functions
      LOGICAL           STRCMP
      CHARACTER*120     STRCAT
      LOGICAL           INDEX_POPULATION_LEVEL
*     .. Local
      DOUBLE PRECISION  vard
      INTEGER           vari


*     .. Set initial return value
c      DCDATA_RECORD = .TRUE.
      PARSE_DCDATA_RECORD = .TRUE.


c      WRITE(*,*) 'MODE=',MODE
      IF ( STRCMP(SYMBOL, 'DC_directory') ) THEN
         DCR_DIR = VALUE
         DCR_DIRSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
c         WRITE(*,*) STRCAT( SYMBOL, ' = '//VALUE )
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, STRCAT(' = ', VALUE) )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_file') ) THEN
         IF ( DCR_DIRSET ) THEN
            DCR_FILE = STRCAT( DCR_DIR, VALUE )
c            WRITE(*,*) 'DIRSET'
         ELSE
            DCR_FILE = VALUE
c            WRITE(*,*) '.NOT.DIRSET'
         ENDIF
         DCR_FILESET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
c         WRITE(*,*) STRCAT( SYMBOL, ' = '//VALUE )
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, STRCAT(' = ', VALUE) )
         IF (VB.GE._vb_hf_)  WRITE(*,*) 'FILE=', DCR_FILE
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_exp_output') ) THEN
         DCR_EXP_OUTPUT = VALUE
         DCR_EXP_OUTPUTSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
c         WRITE(*,*) STRCAT( SYMBOL, ' = '//VALUE )
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, STRCAT(' = ', VALUE) )
         IF (VB.GE._vb_hf_)  WRITE(*,*) 'OUTPUT=', DCR_EXP_OUTPUT
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_sim_output') ) THEN
         DCR_SIM_OUTPUT = VALUE
         DCR_SIM_OUTPUTSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
c         WRITE(*,*) STRCAT( SYMBOL, ' = '//VALUE )
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, STRCAT(' = ', VALUE) )
         IF (VB.GE._vb_hf_)  WRITE(*,*) 'OUTPUT=', DCR_SIM_OUTPUT
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF (    STRCMP(SYMBOL, 'DC_ifirst')
     &        .OR.STRCMP(SYMBOL, 'DC_index_t0') ) THEN
         READ(VALUE,*) vari
         DCR_IFIRST = vari
         DCR_IFIRSTSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF (    STRCMP(SYMBOL, 'DC_ilast')
     &        .OR.STRCMP(SYMBOL, 'DC_index_tN') ) THEN
         READ(VALUE,*) vari
         DCR_ILAST = vari
         DCR_ILASTSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_idelay') ) THEN
         READ(VALUE,*) vari
         DCR_IDELAY = vari
         DCR_IDELAYSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_energy') ) THEN
         READ(VALUE,*) vard
         DCR_ENERGY = vard
         DCR_ENERGYSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_level') ) THEN
c         READ(VALUE,*) vari
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*      - Error conditions -
         IF ( .NOT.INDEX_POPULATION_LEVEL( VALUE, vari ) )
     &      CALL CRITICAL('Illegal or non-existent PULSE level')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         DCR_LEVEL = vari
         DCR_LEVELSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_IVALUE( SYMBOL, vari )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_background') ) THEN
         READ(VALUE,*) vard
         DCR_BKGD = vard
         DCR_BKGDSET= .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_sigma') ) THEN
         READ(VALUE,*) vard
         DCR_SIGMA = vard
         DCR_SIGMASET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
         IF (VB.GE._vb_mf_)  CALL WRITE_SYMBOL_DVALUE( SYMBOL, vard )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF (   STRCMP(SYMBOL, 'DC_norm') 
     &       .OR.STRCMP(SYMBOL, 'DC_exp_norm') ) THEN
c         READ(VALUE,*) vars
         DCR_EXP_NORM = VALUE
         DCR_EXP_NORMSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
c         WRITE(*,*) STRCAT( SYMBOL, ' = '//VALUE )
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, STRCAT(' = ', VALUE) )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_sim_norm') ) THEN
c         READ(VALUE,*) vars
         DCR_SIM_NORM = VALUE
         DCR_SIM_NORMSET = .TRUE.
*/////////////////////////////// Output ////////////////////////////////
c         WRITE(*,*) STRCAT( SYMBOL, ' = '//VALUE )
         IF (VB.GE._vb_mf_)  WRITE(*,*) STRCAT( SYMBOL, STRCAT(' = ', VALUE) )
*/////////////////////////////// Output ////////////////////////////////


      ELSEIF ( STRCMP(SYMBOL, 'DC_add_record') ) THEN
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF (     .NOT.DCR_FILESET ) THEN
            CALL CRITICAL('No DC input file')
         ELSEIF ( .NOT.DCR_IFIRSTSET ) THEN
            CALL CRITICAL('No DC initial index input')
         ELSEIF ( .NOT.DCR_ILASTSET ) THEN 
            CALL CRITICAL('No DC final index input')
         ELSEIF ( .NOT.DCR_IDELAYSET ) THEN
            CALL CRITICAL('No DC index delay input')
         ELSEIF ( .NOT.DCR_ENERGYSET ) THEN
            CALL CRITICAL('No DC pulse energy input')
         ELSEIF ( .NOT.DCR_LEVELSET ) THEN 
            CALL CRITICAL('No DC energy level input')
         ELSEIF ( .NOT.DCR_BKGDSET ) THEN  
            CALL CRITICAL('No DC background input')
         ELSEIF ( .NOT.DCR_SIGMASET ) THEN 
            CALL CRITICAL('No DC std. dev. input')
         ELSEIF ( .NOT.DCR_EXP_NORMSET ) THEN  
            CALL CRITICAL('No DC exp. normalization mode input')
         ELSEIF ( .NOT.DCR_SIM_NORMSET ) THEN  
            CALL CRITICAL('No DC sim. normalization mode input')
         ELSEIF ( .NOT.DCR_EXP_OUTPUTSET ) THEN
            CALL CRITICAL('No DC exp output file')
         ELSEIF ( .NOT.DCR_SIM_OUTPUTSET ) THEN
            CALL CRITICAL('No DC sim output file')
         ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

         CALL ADD_DCFILE( VB,
     &                    DCR_FILE, DCR_IFIRST, DCR_ILAST, DCR_IDELAY,
     &                    DCR_ENERGY, DCR_LEVEL, DCR_BKGD, DCR_SIGMA,
     &                    DCR_EXP_NORM, DCR_SIM_NORM,
     &                    DCR_EXP_OUTPUT, DCR_SIM_OUTPUT )
*     .. Notice here that we do NOT reset DCR_DIRSET!!!
*     .. It remains in effect until a reset or it is overwritten
         DCR_FILESET   = .FALSE.
         DCR_IFIRSTSET = .FALSE.
         DCR_ILASTSET  = .FALSE.
         DCR_IDELAYSET = .FALSE.
         DCR_ENERGYSET = .FALSE.
         DCR_LEVELSET  = .FALSE.
         DCR_BKGDSET   = .FALSE.
         DCR_SIGMASET  = .FALSE.
         DCR_EXP_NORMSET   = .FALSE.
         DCR_SIM_NORMSET   = .FALSE.
         DCR_EXP_OUTPUTSET = .FALSE.
         DCR_SIM_OUTPUTSET = .FALSE.


      ELSEIF ( STRCMP(SYMBOL, 'RESET') ) THEN
         DCR_DIRSET    = .FALSE.
         DCR_FILESET   = .FALSE.
         DCR_IFIRSTSET = .FALSE.
         DCR_ILASTSET  = .FALSE.
         DCR_IDELAYSET = .FALSE.
         DCR_ENERGYSET = .FALSE.
         DCR_LEVELSET  = .FALSE.
         DCR_BKGDSET   = .FALSE.
         DCR_SIGMASET  = .FALSE.
         DCR_EXP_NORMSET   = .FALSE.
         DCR_SIM_NORMSET   = .FALSE.
         DCR_EXP_OUTPUTSET = .FALSE.
         DCR_SIM_OUTPUTSET = .FALSE.

         PARSE_DCDATA_RECORD = .FALSE.


      ELSE
         PARSE_DCDATA_RECORD = .FALSE.


      ENDIF


      RETURN
      END




************************************************************************
*                                                                      *
*     - ADD A DC DATA FILE -                                           *
*                                                                      *
************************************************************************
      SUBROUTINE ADD_DCFILE( VB,
     &                       DCR_FILE, DCR_IFIRST, DCR_ILAST, DCR_IDELAY,
     &                       DCR_ENERGY, DCR_LEVEL, DCR_BKGD, DCR_SIGMA,
     &                       DCR_EXP_NORM, DCR_SIM_NORM,
     &                       DCR_EXP_OUTPUT, DCR_SIM_OUTPUT )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           VB
      CHARACTER         DCR_FILE*(*)
      CHARACTER         DCR_EXP_NORM*(*), DCR_SIM_NORM*(*)
      CHARACTER         DCR_EXP_OUTPUT*(*), DCR_SIM_OUTPUT*(*)
      INTEGER           DCR_IFIRST, DCR_ILAST, DCR_IDELAY, DCR_LEVEL
      DOUBLE PRECISION  DCR_ENERGY, DCR_BKGD, DCR_SIGMA

*     .. COMMON data blocks
#include "common-data-dc.h"

*     .. Function
      CHARACTER*30      DEBLANK
      DOUBLE PRECISION  VECPEAK, VECWINAVG
      INTEGER           VECMAXI
      LOGICAL           STRCMP
*     .. Local
      INTEGER           ii, jj
      INTEGER           idx
      DOUBLE PRECISION  peak
      DOUBLE PRECISION  tdiff, tbest

      DOUBLE PRECISION  dblrem
      CHARACTER*30      strnorm, strrem, strmod, strpred
      INTEGER           idxcolon, intrem
      INTEGER           pidx, pidxl, pidxr

      DOUBLE PRECISION  datas(_maxdcbuf_), times(_maxdcbuf_)
      INTEGER           ndatas, idelay


*     ..
      N_DCFILES = N_DCFILES+1
      idx = N_DCFILES

*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   - Error Condition -
      IF ( N_DCFILES.GT._maxndcfiles_ ) THEN
         CALL CRITICAL(  'Too many DECAY CURVE data files.'
     &                // ' Increase _maxndcfiles_ in lanthanide.h and'
     &                // ' recompile.')
      ENDIF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

*/////////////////////////////// Output ////////////////////////////////
      IF (VB.GE._vb_hf_) THEN
         WRITE(*,*) 'DCR_FILE=',DCR_FILE
         WRITE(*,*) 'DCR_EXP_NORM=',DCR_EXP_NORM
         WRITE(*,*) 'DCR_SIM_NORM=',DCR_SIM_NORM
      END IF
*/////////////////////////////// Output ////////////////////////////////

*     .. Use the DATAIN function to read the file
      CALL DATAIN( DCR_FILE, DCR_IFIRST, DCR_ILAST, DCR_BKGD,
     &             times, datas, ndatas )

*     .. Normalize our input data by the method specified in DCR_EXP_NORM
      CALL DC_NORMALIZE( DCR_EXP_NORM, datas, times, ndatas, datas )



      idelay = DCR_IDELAY

c      WRITE(*,*) 'idelay=', idelay
c      WRITE(*,*) 'ndatas=', ndatas
c      WRITE(*,*) 'pidx=', pidx
      IF (idelay.LT.0)  CALL CRITICAL('Negative DC_idelay');
c      IF ((ind0+IDELAY).GE.indN) CALL CRITICAL('DC_idelay too large');
      CALL VECCOPY( datas(idelay+1), DC_DATA(1,idx), ndatas-idelay )
      CALL VECCOPY( times(idelay+1), DC_TIME(1,idx), ndatas-idelay )
      DC_NDATA(idx) = ndatas - IDELAY


*     .. Set the input excitation energy (Joules) and the decay-curve's
*        energy level (n1, n2, ... )
      DC_ENERGY(idx) = DCR_ENERGY
      DC_SIMLVL(idx) = DCR_LEVEL
      DC_SIGMA(idx)  = DCR_SIGMA

c      DC_SIMNORM(idx) = strnorm
      DC_SIMNORM(idx) = DCR_SIM_NORM

*     .. Update our unique pulse energy database
      CALL SET_UNIQUE_ENERGY( VB, 'PULSE', idx, DCR_ENERGY )
*/////////////////////////////// Output ////////////////////////////////
      IF (VB.GE._vb_hf_)  WRITE(*,*) 'ADD PULSE ENERGY=', DCR_ENERGY
      IF (VB.GE._vb_hf_) THEN
         WRITE(*,*) 'PIDXL=', pidxl
         WRITE(*,*) 'PIDXR=', pidxr
         WRITE(*,*) 'PEAK=', peak
      ENDIF
*/////////////////////////////// Output ////////////////////////////////

*     .. Set output filename
      DC_EXP_OUTPUT_FILE(idx) = DCR_EXP_OUTPUT
      DC_SIM_OUTPUT_FILE(idx) = DCR_SIM_OUTPUT
*/////////////////////////////// Output ////////////////////////////////
      IF (VB.GE._vb_hf_)   WRITE(*,*) 'DC_EXP_OUTPUT=',DCR_EXP_OUTPUT
      IF (VB.GE._vb_hf_)   WRITE(*,*) 'DC_SIM_OUTPUT=',DCR_SIM_OUTPUT
*/////////////////////////////// Output ////////////////////////////////


      RETURN
      END



************************************************************************
      SUBROUTINE DC_NORMALIZE( DCR_NORM, DATAS, TIMES, NDATAS,
     &                         DATAS_NORMED )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         DCR_NORM*(*)
      DOUBLE PRECISION  DATAS(*), TIMES(*)
      DOUBLE PRECISION  DATAS_NORMED(*)
*     .. Outputs
*     .. Functions
      CHARACTER*30      DEBLANK
      DOUBLE PRECISION  VECPEAK, VECWINAVG
      INTEGER           VECMAXI
      LOGICAL           STRCMP
*     .. Locals
      INTEGER           ii, jj
      INTEGER           idx
      DOUBLE PRECISION  peak
      DOUBLE PRECISION  tdiff, tbest

      DOUBLE PRECISION  dblrem
      CHARACTER*30      strnorm, strrem, strmod, strpred
      INTEGER           idxcolon, intrem
      INTEGER           pidx, pidxl, pidxr

      INTEGER           ndatas, idelay

c      WRITE(*,'(A)') DCR_NORM

*     .. Normalize our input data by the method specified in DCR_EXP_NORM
      strnorm = DCR_NORM
      idxcolon = INDEX( strnorm, ':' )
      intrem = 1
      IF ( idxcolon.GT.0 ) THEN
         strnorm = DCR_NORM(:idxcolon-1)
         strrem  = DEBLANK( DCR_NORM(idxcolon+1:) )
      ENDIF


      pidx = VECMAXI( datas, ndatas )
      IF ( STRCMP( strnorm, 'MAX' ) ) THEN
         CALL DC_NORM_MODIFIER( DCR_NORM, pidxl, pidxr )
         peak = datas(pidx+pidxr-pidxl)


      ELSEIF ( STRCMP ( strnorm, 'AVG' ) ) THEN
         CALL DC_NORM_MODIFIER( DCR_NORM, pidxl, pidxr )
         peak = VECWINAVG( datas, pidx-pidxl, pidx+pidxr )


      ELSEIF ( STRCMP ( strnorm, 'VALUE' ) ) THEN
         READ(strrem, *) dblrem
         IF ( dblrem.LT.1.0 .OR. dblrem.GT.1.0E5 ) THEN
            CALL CRITICAL( 'DC_norm modifier out of range (1-1e5)')
         ENDIF
         peak = dblrem


      ELSEIF ( STRCMP ( strnorm, 'POINT' ) ) THEN
         READ(strrem, *) intrem
         IF ( intrem.LT.1 .OR. intrem.GT.300 ) THEN
            CALL CRITICAL( 'DC_norm modifier out of range (1-300)' )
         ENDIF
         pidxr = intrem
c         IF ( intrem.EQ.0 ) pidxr = 1
         peak = datas(pidxr)
c         DC_NORMTIME(idx) = times(pidxr)
c         WRITE(*,*) 'PEAK TIME=', times(pidxr)
c         WRITE(*,*) 'PEAK VALUE=', datas(pidxr)


      ELSEIF ( STRCMP ( strnorm, 'TIME' ) ) THEN
         READ(strrem, *) dblrem
         IF ( dblrem.LT.1.0E-06 .OR. dblrem.GT.1.0E-03 ) THEN
            CALL CRITICAL( 'DC_norm modifier out of range (1-1000usec)')
         ENDIF
         tbest = DABS( times(1) - dblrem ) 
         pidxr = 1
         DO ii=2, ndatas
            tdiff = DABS( times(ii) - dblrem )
            IF ( tdiff.LT.tbest ) THEN
               tbest = tdiff
               pidxr = ii
            ENDIF
         END DO
         IF ( pidxr.EQ.ndatas ) CALL CRITICAL( 'TIME out of range' );
         peak = datas(pidxr)
c         DC_NORMTIME(idx) = times(pidxr)
c         WRITE(*,*) 'PEAK TIME=', times(pidxr)
c         WRITE(*,*) 'PEAK VALUE=', datas(pidxr)
c         WRITE(*,*) 'PIDXR=',pidxr


      ELSE
         CALL CRITICAL( 'Unrecognized DC_norm specification' )


      ENDIF


      CALL VECSCALE( 1 / peak, DATAS, NDATAS, DATAS_NORMED )


      RETURN
      END



************************************************************************
      SUBROUTINE DC_NORM_MODIFIER( DCNORM, PIDXL, PIDXR )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         DCNORM*(*)
*     .. Outputs
      INTEGER           PIDXL, PIDXR
*     .. Functions
      CHARACTER*30      DEBLANK
      LOGICAL           STRCMP
*     .. Locals
      CHARACTER*30      strnorm, strrem, strmod, strpred
      INTEGER           idxcolon, intmod

*     .. Normalize our input data by the method specified in DCR_EXP_NORM
      pidxl = 10
      pidxr = 10

      strnorm = DCNORM
      idxcolon = INDEX( strnorm, ':' )
      IF ( idxcolon.GT.0 ) THEN
         strnorm = DCNORM(:idxcolon-1)
         strrem  = DEBLANK( DCNORM(idxcolon+1:) )
         strpred = strrem(1:1)
         strmod  = strrem(2:)
         READ(strmod, *) intmod
         IF ( intmod.LT.0 .OR. intmod.GT.300 ) THEN
            CALL CRITICAL( 'DC_norm modifier out of range' )
         ENDIF
         IF     ( strpred.EQ.'S' ) THEN
            pidxl = intmod
            pidxr = intmod
         ELSEIF ( strpred.EQ.'R' ) THEN
            pidxl = 0
            pidxr = intmod
         ELSEIF ( strpred.EQ.'L' ) THEN
            pidxl = intmod
            pidxr = 0
         ELSE
            CALL CRITICAL( 'Unrecognized DC_norm modifier predicate' )
         ENDIF
      ENDIF

c      WRITE(*,*) 'PIDX=', pidx
c      WRITE(*,*) 'PIDXL=', pidxl
c      WRITE(*,*) 'PIDXR=', pidxr

      RETURN
      END



***********************************************************************
*                                                                     *
*     - CALCULATE CHI-SQUARED -                                       *
*                                                                     *
***********************************************************************
      DOUBLE PRECISION FUNCTION CHISQU_DC( VB, PULSE, NSIM, IDX )
      IMPLICIT NONE
*     .. Input variables
      INTEGER            VB
      INTEGER            NSIM, IDX
      DOUBLE PRECISION   PULSE( * )
c      DOUBLE PRECISION   dT

*     .. COMMON data blocks
#include "common-data-dc.h"

*     .. Local variables
      INTEGER            ii, jj
      DOUBLE PRECISION   cost, ccost


      ccost = _zero_
      DO ii = 1, DC_NDATA(IDX)
         cost = (  PULSE( DC_SIMIDX(ii,IDX) )
     &           - DC_DATA(ii,IDX) )**2
         ccost = ccost + cost
      END DO

      cost = ccost / DC_SIGMA(IDX)**2
c      WRITE(*,*) 'DC_SIGMA=',DC_SIGMA(IDX)
      CHISQU_DC = cost / dble( DC_NDATA(IDX) )


      RETURN
      END



