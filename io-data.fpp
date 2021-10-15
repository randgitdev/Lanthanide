#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     READ INPUT FILES                                                *
*                                                                     *
***********************************************************************
c      SUBROUTINE DATAIN(FILEIN, INDEX0, INDEXN, IDELAY, Y0, T, D, N)
      SUBROUTINE DATAIN(FILEIN, INDEX0, INDEXN, Y0, T, D, N)
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         FILEIN*(*)
c      INTEGER           INDEX0, INDEXN, IDELAY
      INTEGER           INDEX0, INDEXN
      DOUBLE PRECISION  Y0
*     .. Outputs
      DOUBLE PRECISION  T(*), D(*)
      INTEGER           N
*     .. Local
      CHARACTER*80      header
      DOUBLE PRECISION  tt, time0, dd
      DOUBLE PRECISION  DAT(_maxdcbuf_,2)
      INTEGER           ios, ind0, indN
      INTEGER           ii, jj, kk


*     .. Open a two-column CSV data file and read times and intensity 
      OPEN ( UNIT=10,
     &       FILE=FILEIN,
     &       FORM='FORMATTED',
     &       STATUS='OLD',
     &       ACCESS='SEQUENTIAL')

      ii = 1
c      READ (10, '(A)', IOSTAT=ios) header
110   READ (10, *, IOSTAT=ios) ( DAT(ii,jj), jj=1,2 )
      IF (ios.EQ.0) THEN
c         time(ii) = tt
c         dat(ii) = dd
         ii = ii + 1
         GOTO 110
      ENDIF
*     .. Close the file
      CLOSE (10)

*     .. Setup start and end indices
      ind0 = 1
      IF (INDEX0.NE.0) ind0 = INDEX0
      indN = ii-1
      IF (INDEXN.GE.indN) CALL CRITICAL('ilast exceeds file size');
      IF (INDEXN.NE.0) indN = INDEXN

c      IF (IDELAY.LT.0)  CALL CRITICAL('Negative DC_idelay');
c      IF ((ind0+IDELAY).GE.indN) CALL CRITICAL('DC_idelay too large');


*     .. Find time zero at index zero
      time0 = DAT(ind0,1)

*     .. Copy data, accounting for a beginning and ending index
*     ..  and adjusting the times to start at time = 0
*     .. Also, select the largest data element and use to normalize
*     ..  the data set.
      kk = 0
c      DO jj = (ind0+IDELAY), indN
      DO jj = ind0, indN
         kk = kk + 1
         T(kk) = DAT(jj,1) - time0
         D(kk) = DAT(jj,2) - Y0
      END DO
      N = indN - ind0 + 1

      RETURN
      END



***********************************************************************
*                                                                     *
*     WRITE OUTPUT FILE                                               *
*                                                                     *
***********************************************************************
      SUBROUTINE DATAOUTSIM( FILEOUT, TSIM,
     &                       N, mN, nN, LDN, SKIP )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           mN, nN, LDN, SKIP
      CHARACTER         FILEOUT*(*)
      DOUBLE PRECISION  N( LDN, * )
      DOUBLE PRECISION  TSIM(*)
*     .. Local
      CHARACTER         str_mN*2, fstr*30
      INTEGER           ii, jj, kk


*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Error  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF ( SKIP.LE.0 )  CALL CRITICAL('Invalid file output skip')
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  Error  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      WRITE( str_mN, '(I2)' ) mN
      fstr = '(E17.10, ' // str_mN // '('','', E17.10))'
c      WRITE(*,*) fstr

*     .. Open a two-column CSV data file and read times and intensity 
      OPEN ( UNIT=30,
     &       FILE=FILEOUT,
     &       FORM='FORMATTED',
     &       STATUS='UNKNOWN',
     &       ACCESS='SEQUENTIAL')


*     .. NOTE: We are transposing the data for output
      DO 110 jj=1, nN, SKIP
c         WRITE (30, 10011) TSIM(jj), ( N(ii,jj), ii=1,mN )
         WRITE (30, fstr) TSIM(jj), ( N(ii,jj), ii=1,mN )
110   CONTINUE

      CLOSE (30)


cc10010 FORMAT ( 8(E10.3, 2X) )
c10010 FORMAT ( 8(E10.3, ', ') )
c10011 FORMAT ( E17.10, 8 (',', E17.10))

      RETURN
      END



*......................................................................
      SUBROUTINE DATAOUTEXP( FILEN, NN, TN, DN )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER         FILEN*(*)
      DOUBLE PRECISION  TN(*)
      DOUBLE PRECISION  DN(*)
      INTEGER           NN
*     .. Local
      INTEGER           ii, jj, kk


      OPEN ( UNIT=35,
     &       FILE=FILEN,
     &       FORM='FORMATTED',
     &       STATUS='UNKNOWN',
     &       ACCESS='SEQUENTIAL')


      DO 110 ii =1, NN
c         WRITE (35, '(E12.5, E12.5)') TGN(ii), DGN(ii)
         WRITE (35, 10020) TN(ii), DN(ii)
110   CONTINUE

      CLOSE (35)

c10020 FORMAT ( E12.5, ', ', E12.5 )
10020 FORMAT ( E17.10, ', ', E17.10 )

      RETURN
      END



