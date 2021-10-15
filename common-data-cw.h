#include "lanthanide.h"

c//   .. COMMON data blocks
c//    DOUBLE PRECISION  CW_RADIATIVE(_maxnlvls_, _maxncwdatas_)
c//    DOUBLE PRECISION  CW_RATIOS(_maxnlvls_-1,_maxncwdatas_)
c//    INTEGER           CW_NUMER(_maxnlvls_-1,_maxncwdatas_)
      DOUBLE PRECISION  CW_RADIATIVE(_maxnlvls_, _maxncwdatas_)
      DOUBLE PRECISION  CW_RATIOS   (_maxnlvls_, _maxncwdatas_)
      INTEGER           CW_NUMER    (_maxnlvls_, _maxncwdatas_)
      INTEGER           CW_DENOM  (_maxncwdatas_)
      INTEGER           CW_NRATIOS(_maxncwdatas_)
      DOUBLE PRECISION  CW_POWER  (_maxncwdatas_)
      DOUBLE PRECISION  CW_LAMBDA (_maxncwdatas_)
      DOUBLE PRECISION  CW_RADIUS (_maxncwdatas_)
      DOUBLE PRECISION  CW_SIGMA  (_maxncwdatas_)
      INTEGER           N_CWDATAS
      COMMON /CWDATAS/
     &                  CW_RADIATIVE,
     &                  CW_RATIOS, CW_NUMER,  CW_DENOM,  CW_NRATIOS,
     &                  CW_POWER,  CW_LAMBDA, CW_RADIUS, CW_SIGMA,
     &                  N_CWDATAS


