#include "lanthanide.h"

c//   .. COMMON data block
c//      DOUBLE PRECISION  P_BIVAL(_maxnbs_)
c//      DOUBLE PRECISION  P_KIVAL(_maxnks_)
c//      DOUBLE PRECISION  P_N0IVAL(_maxnlvls_, _max_n_energies_)
c//      DOUBLE PRECISION  P_DIVAL
      INTEGER           P_NPS
      LOGICAL           P_LOG

      DOUBLE PRECISION  P_PIVAL(_max_n_params_, _max_n_energies_)
      LOGICAL           P_INDEP(_max_n_params_)

c//      COMMON /PVECTOR_PARAMETERS/
c//     &                  P_BIVAL, P_KIVAL, P_N0IVAL, P_DIVAL,
c//     &                  P_NPS, P_LOG

      COMMON /PVECTOR_PARAMETERS/
     &                  P_PIVAL, P_INDEP, P_NPS, P_LOG


