#include "lanthanide.h"

c//   .. COMMON data blocks
c//    DOUBLE PRECISION  II_RADIATIVE(_maxnlvls_,  _maxniidatas_)
c//    DOUBLE PRECISION  II_RATIOS   (_maxnlvls_-1,_maxniidatas_)
c//    INTEGER           II_NUMER    (_maxnlvls_-1,_maxniidatas_)
      DOUBLE PRECISION  II_RADIATIVE(_maxnlvls_, _maxniidatas_)
      DOUBLE PRECISION  II_RATIOS   (_maxnlvls_, _maxniidatas_)
      INTEGER           II_NUMER    (_maxnlvls_, _maxniidatas_)
      INTEGER           II_DENOM  (_maxniidatas_)
      INTEGER           II_NRATIOS(_maxniidatas_)
      DOUBLE PRECISION  II_ENERGY (_maxniidatas_)
      DOUBLE PRECISION  II_LAMBDA (_maxniidatas_)
      DOUBLE PRECISION  II_RADIUS (_maxniidatas_)
      DOUBLE PRECISION  II_SIGMA  (_maxniidatas_)
      INTEGER           N_IIDATAS
      COMMON /IIDATAS/
     &                  II_RADIATIVE,
     &                  II_RATIOS, II_NUMER,  II_DENOM,  II_NRATIOS,
     &                  II_ENERGY, II_LAMBDA, II_RADIUS, II_SIGMA,
     &                  N_IIDATAS


