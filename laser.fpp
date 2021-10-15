#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     LASER PHOTON FLUX                                               *
*                                                                     *
***********************************************************************
      DOUBLE PRECISION FUNCTION LASERFLUX( POWER, LAMBDA, RADIUS )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  POWER, LAMBDA, RADIUS
*     .. Physical constants
      DOUBLE PRECISION  pi, h, c
c         PARAMETER      ( pi=3.14159 26535 89793D+00 )
         PARAMETER      ( pi=_pi_ )
c         PARAMETER      ( h=6.6260689633D-34 )
         PARAMETER      ( h=_hplanck_ )
c         PARAMETER      ( c=299 792 458 )
         PARAMETER      ( c=_c0_ )

c*     .. Characteristics of CW laser
c      power=30D-03   ! Power in milliwatts
c      lambda=976D-09 ! Wavelength in meters
c      radius=2.5D-02 ! Beam radius
c*     .. F              Laser intensity / photon flux
cc      F = (power*lambda)/(h*c*pi*radius**2)
c      F = LASERFLUX( power, lambda, radius )


c      LASERFLUX = (POWER*LAMBDA) / (h*c*pi*RADIUS**2)
      LASERFLUX =   (POWER * LAMBDA) 
     &            /
     &              (_hplanck_ * _c0_ * _pi_ * RADIUS**2)

      RETURN
      END




