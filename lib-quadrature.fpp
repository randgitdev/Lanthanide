#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - VECTOR TRAPEZOID QUADRATURE -                                 *
*                                                                     *
***********************************************************************
      DOUBLE PRECISION FUNCTION VECTRAPEZOID( F, dT, NN )
      IMPLICIT NONE
*     .. Inputs
      DOUBLE PRECISION  F(*)
      DOUBLE PRECISION  dT
      INTEGER           NN
*     .. Local
c      DOUBLE PRECISION  ACUM, A, B
      DOUBLE PRECISION  ACUM
      INTEGER           ii


*.......................................................................
*   - Error condition
      IF( NN.LE.1 ) CALL CRITICAL('Insufficient points for integration')
*.......................................................................

c      A = _zero_
c      B = A + NN*dT

      ACUM = F(1)  ! F(A)
      DO ii=2,(NN-1)
         ACUM = ACUM + 2*F(ii)
      END DO
      ACUM = ACUM + F(NN)

      VECTRAPEZOID = dT * ACUM / 2.0D+00


      RETURN
      END

