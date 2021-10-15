***********************************************************************
*                                                                     *
*     - VECTOR ROUTINES -                                             *
*                                                                     *
***********************************************************************
      SUBROUTINE CENTROID(PP, C, NPS, LDP)
      IMPLICIT NONE
      INTEGER           NPS, LDP
      DOUBLE PRECISION  PP(LDP, *), C(*)
      INTEGER           jj, kk

      DO jj=1,NPS+1
         DO kk=1,NPS
            C(kk) = C(kk) + PP(kk,jj)
         END DO
      END DO

      RETURN
      END


*................................................*
      SUBROUTINE VECZERO(X, n)
      IMPLICIT NONE
      DOUBLE PRECISION  X(*)
      INTEGER           n
*     .. Local variables
      INTEGER           ii

      DO ii=1,n
         X(ii) = 0.00D+00
      END DO

      RETURN
      END


*................................................*
      SUBROUTINE VECCOPY(X, Y, n)
      IMPLICIT NONE
      DOUBLE PRECISION  X(*), Y(*)
      INTEGER           n
*     .. Local variables
      INTEGER           ii

      DO ii=1,n
         Y(ii) = X(ii)
      END DO

      RETURN
      END


*................................................*
      SUBROUTINE VECFILL(a, n, X)
      IMPLICIT NONE
      DOUBLE PRECISION  a
      INTEGER           n
*     .. Outputs
      DOUBLE PRECISION  X(*)
*     .. Local variables
      INTEGER           ii

      DO ii=1,n
         X(ii) = a
      END DO

      RETURN
      END


*................................................*
      SUBROUTINE VECSCALE(a, X, n, Y)
      IMPLICIT NONE
      DOUBLE PRECISION  X(*), Y(*)
      DOUBLE PRECISION  a
      INTEGER           n
*     .. Local variables
      INTEGER           ii

      DO ii=1,n
         Y(ii) = a * X(ii)
      END DO

      RETURN
      END


*................................................*
      SUBROUTINE VECSUM( X, Y, n, S )
      IMPLICIT NONE
      DOUBLE PRECISION  X(*), Y(*), S(*)
      INTEGER           n
*     .. Local variables
      INTEGER           ii

      DO ii=1,n
         S(ii) = X(ii) + Y(ii)
      END DO

      RETURN
      END


*................................................*
      SUBROUTINE VECDIFF( X, Y, n, D )
      IMPLICIT NONE
      DOUBLE PRECISION  X(*), Y(*), D(*)
      INTEGER           n
*     .. Local variables
      INTEGER           ii

      DO ii=1,n
         D(ii) = X(ii) - Y(ii)
      END DO 

      RETURN
      END


*.......................................................................
      DOUBLE PRECISION FUNCTION VECMAX( VIN, NVEC )
      IMPLICIT NONE
*     .. Input
      INTEGER           NVEC
      DOUBLE PRECISION  VIN(*)
*     .. Output
c      DOUBLE PRECISION  VOUT(*)
*     .. Functions
*     .. Local
      DOUBLE PRECISION  peak
      INTEGER           ii, ind, num


      peak = VIN(1)
      DO ii=2, NVEC
         IF ( VIN(ii).GT.peak ) peak=VIN(ii)
      END DO

      VECMAX = peak

      RETURN
      END



*.......................................................................
      INTEGER FUNCTION VECMAXI( VIN, NVEC )
      IMPLICIT NONE
*     .. Input
      INTEGER           NVEC
      DOUBLE PRECISION  VIN(*)
*     .. Output
*     .. Functions
*     .. Local
      INTEGER           ii, peak


c      peak = VIN(1)
      peak = 1
      DO ii=2, NVEC
         IF ( VIN(ii).GT.VIN(peak) )  peak=ii
      END DO

      VECMAXI = peak

      RETURN
      END



*.......................................................................
      DOUBLE PRECISION FUNCTION VECWINAVG( VIN, IDXL, IDXR )
      IMPLICIT NONE
*     .. Input
      DOUBLE PRECISION  VIN(*)
      INTEGER           IDXL, IDXR
*     .. Output
*     .. Functions
*     .. Local
      DOUBLE PRECISION  mean
      INTEGER           ii, num


      mean = 0.0D+00
      num = 0
      DO ii=IDXL, IDXR
         IF (ii.GT.0) THEN
            mean = mean + VIN(ii)
            num = num + 1

c      WRITE(*,*) 'VIN(ii)=', VIN(ii), ' num=', num

         END IF
      END DO
      mean = mean / dble(num)

c      WRITE(*,*) 'mean=', mean

      VECWINAVG = mean

      RETURN
      END



************************************************************************
*                                                                      *
*     - NORMALIZE MATRIX ENTRIES -                                     *
*                                                                      *
************************************************************************
      SUBROUTINE MATNORMS( XIN, mX, nX, LDX,
     &                     XOUT, MODE, NORM, VALUE )
      IMPLICIT NONE
*     .. Inputs
      INTEGER           mX, nX, LDX
      DOUBLE PRECISION  XIN( LDX,* )
      CHARACTER         MODE*(*), NORM*(*)
      DOUBLE PRECISION  VALUE
*     .. Outputs
      DOUBLE PRECISION  XOUT( LDX,* )
*     .. Functions
      LOGICAL           STRCMP
*     .. Local
      DOUBLE PRECISION  dmax, mean
      INTEGER           ii, jj, kk, ind, num
      

*     .......................................
      IF ( STRCMP(MODE, 'ROW') ) THEN

         IF ( STRCMP(NORM, 'MAX') ) THEN
            DO ii=1, mX
               dmax = XIN(ii,1)
               DO jj=2, nX
                  IF ( XIN(ii,jj).GT.dmax ) dmax=XIN(ii,jj)
               END DO
               
               DO jj=1, nX
                  XOUT(ii,jj) = XIN(ii,jj) / dmax
               END DO
            END DO

         ELSEIF ( STRCMP(NORM, 'VALUE') ) THEN
            DO jj=1, nX
               XOUT(ii,jj) = XIN(ii,jj) / VALUE
            END DO

         END IF


*     .......................................
      ELSEIF ( STRCMP(MODE, 'COL') ) THEN

         IF ( STRCMP(NORM, 'MAX') ) THEN
            DO jj=1, nX
               dmax = XIN(1,jj)
               DO ii=2, mX
                  IF ( XIN(ii,jj).GT.dmax ) dmax=XIN(ii,jj)
               END DO
               
               DO ii=1, mX
                  XOUT(ii,jj) = XIN(ii,jj) / dmax
               END DO
            END DO

         ELSEIF ( STRCMP(NORM, 'VALUE') ) THEN
            DO ii=1, mX
               XOUT(ii,jj) = XIN(ii,jj) / VALUE
            END DO

         END IF


      END IF


      RETURN
      END



*................................................*
      SUBROUTINE MATTRANSPOSE( X, mm, nn, LDX, XT, LDXT )
      IMPLICIT NONE
      INTEGER           mm, nn, LDX, LDXT
      DOUBLE PRECISION  X(LDX,*), XT(LDXT,*)
*     .. Local
      INTEGER           ii, jj

      DO ii=1,mm
         DO jj=1,nn
            XT(jj,ii) = X(ii,jj)
         END DO
      END DO

      RETURN
      END



*................................................*
      SUBROUTINE MATSUM( A, m, n, LDA, S )
      IMPLICIT NONE
      INTEGER           m, n, LDA
      DOUBLE PRECISION  A(LDA,*), S(*)
*     .. Local variables
      INTEGER           ii, jj

      DO ii=1,m
         S(ii) = 0.00D+00
         DO jj=1,n
            S(ii) = S(ii) + A(ii,jj)
         END DO
      END DO

      RETURN
      END



*................................................*
      SUBROUTINE MATZERO(X, mX, nX, ldX)
      IMPLICIT NONE
      INTEGER           mX, nX, ldX
      DOUBLE PRECISION  X( ldX, * )
*     .. Local variables
      INTEGER           ii, jj

      DO jj=1, nX
         DO ii=1, mX
            X(ii,jj) = 0.00D+00
         END DO
      END DO

      RETURN
      END



