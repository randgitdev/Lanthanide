#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     PARSE COMMAND FILE                                              *
*                                                                     *
***********************************************************************
      SUBROUTINE CMDPARSE( CMDFILE )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         CMDFILE*(*)
*     .. Outputs
*     ..
*     .. COMMON data blocks
#include "common-control.h"
*     ................

*     .. Local
      INTEGER           vari
      DOUBLE PRECISION  vard
      CHARACTER*120     vars
      LOGICAL           varl
      CHARACTER*120     LINEIN, LINE
      CHARACTER*120     symbol, subsymbol, value
      INTEGER           ios
      INTEGER           idxeql, idxast, idxat
      INTEGER           bindex, kindex, nindex
      LOGICAL           initrec
      INTEGER           idx1, idx2
      INTEGER           ii, jj, kk

*     .. Functions
      CHARACTER*120     DEBLANK, STRCAT
      INTEGER           STRLEN
      LOGICAL           STRCMP

      LOGICAL           PARSE_PARAMETERS
      LOGICAL           PARSE_DCDATA_RECORD
      LOGICAL           PARSE_IIDATA_RECORD
      LOGICAL           PARSE_CWDATA_RECORD
      LOGICAL           PARSE_ENERGY
      LOGICAL           PARSE_SIMULATION
      LOGICAL           PARSE_SIMPLEX_PARAMETER
      LOGICAL           PARSE_CONTROL
      LOGICAL           PARSE_QUANTUM_EFFICIENCY


*     ..
*     .. Open command file
      WRITE(*,*) CMDFILE
      OPEN ( UNIT=20,
     &       FILE=CMDFILE,
     &       FORM='FORMATTED',
     &       STATUS='OLD',
     &       ACCESS='SEQUENTIAL' )

*     .. Zeroize experimental data records
      initrec = PARSE_DCDATA_RECORD('RESET', '')
      initrec = PARSE_IIDATA_RECORD('RESET', '')
      initrec = PARSE_CWDATA_RECORD('RESET', '')



*     .. Begin parsing loop
      ii = 0
c110   READ (20, *, IOSTAT=ios) LINEIN
110   READ (20, '(A)', IOSTAT=ios) LINEIN
      IF ( ios.EQ.0 ) THEN
         ii = ii + 1
         LINE = DEBLANK( LINEIN )
c         idxat  = INDEX( LINE, '@' )
c         idxast = INDEX( LINE, '*' )
         idxeql = INDEX( LINE, '=' )

         value = DEBLANK( LINE(idxeql+1:) )
         symbol = LINE

c         idxeql = INDEX( symbol, '=' )
         IF ( idxeql.NE.0 ) THEN
            symbol = LINE(:idxeql-1)
         ENDIF

c         subsymbol = symbol
         idxat  = INDEX( symbol, '@' )
         IF ( idxat.NE.0 ) THEN
           subsymbol = symbol(idxat+1:)
           symbol = subsymbol
         ENDIF

c         subsymbol = symbol
         idxast = INDEX( symbol, '*' )
         IF ( idxast.NE.0 ) THEN
           subsymbol = symbol(idxast+1:)
           symbol = subsymbol
         ENDIF

c         WRITE(*,*) LINEIN
c         WRITE(*,*) 'LINE=',LINE
c         WRITE(*,*) 'idxeql=',idxeql
c         WRITE(*,*) 'idxast=',idxast
c         WRITE(*,*) 'symbol=',symbol
c         WRITE(*,*) 'subsymbol=',subsymbol
c         WRITE(*,*) 'value=',value

******** Begin parse *************

*        .. 1. Check for a blank line
c         IF ( STRLEN(LINE).EQ.0 ) THEN
         IF ( STRLEN(LINE).LE.0 ) THEN

*        .. 2. Check for a comment
         ELSEIF ( LINE(1:1).EQ.'#' ) THEN
*/////////////////////////////// Output ////////////////////////////////
c            WRITE(*,*) 'COMMENT LINE #', ii
            IF (VERBOSE.GE._vb_mf_)  WRITE(*,*) 'COMMENT ', LINE(1:50)
*/////////////////////////////// Output ////////////////////////////////

*        .. 3. Might not want this one
c         ELSEIF ( idxeql.EQ.0 ) THEN

         ELSEIF ( PARSE_PARAMETERS( VERBOSE, symbol, value,
     &                              idxast, idxat ) ) THEN

*        ..........................................................
         ELSEIF ( PARSE_SIMULATION( VERBOSE, symbol, value ) ) THEN

*        ..........................................................
         ELSEIF ( PARSE_ENERGY( VERBOSE, symbol, value ) ) THEN

*        ..........................................................
         ELSEIF ( PARSE_SIMPLEX_PARAMETER( VERBOSE, symbol, value ) ) THEN

*        ..........................................................
         ELSEIF ( PARSE_DCDATA_RECORD( VERBOSE, symbol, value ) ) THEN 
         
         ELSEIF ( PARSE_IIDATA_RECORD( VERBOSE, symbol, value ) ) THEN

         ELSEIF ( PARSE_CWDATA_RECORD( VERBOSE, symbol, value ) ) THEN

*        ..........................................................

         ELSEIF ( PARSE_CONTROL( VERBOSE, symbol, value ) ) THEN

*        ..........................................................
         ELSEIF ( PARSE_QUANTUM_EFFICIENCY( VERBOSE, symbol, value ) ) THEN
         
         
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*      - Error condition -
         ELSE
c            WRITE(*,*) 'UNIDENTIFIED SYMBOL'
            WRITE(*,*) STRCAT(symbol, ' = '//value)
            CALL CRITICAL('UNIDENTIFIED SYMBOL')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

         ENDIF
******** End parse ***************

         GOTO 110
      ENDIF 

      CLOSE (20)


      RETURN
      END





