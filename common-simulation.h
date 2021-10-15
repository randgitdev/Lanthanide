#include "lanthanide.h"

c//   .. COMMON data blocks
c//   .. ODE solver function and upconversion model

      DOUBLE PRECISION  DELTA_T, TOTAL_T
      INTEGER           NSIM
c//    INTEGER           UPMODEL
      CHARACTER*15      UPMODEL
      CHARACTER*15      ODESOLVER

      COMMON /SIMULATION_PARAMETERS/
     &       DELTA_T, TOTAL_T, NSIM, UPMODEL, ODESOLVER


