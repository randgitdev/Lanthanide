
#define _dtol_  1.0D-08

************************************************************************
*                                                                      *
*     - FLAOTING POINT COMPARISON -                                    *
*                                                                      *
************************************************************************
      LOGICAL FUNCTION DEQUIV( X, Y )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  X, Y
*     .. Outputs
*     .. Local

      DEQUIV = DABS(X - Y).LT._dtol_

      RETURN
      END



