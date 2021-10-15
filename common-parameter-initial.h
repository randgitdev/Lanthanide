#include "lanthanide.h"

c//   .. COMMON data block

c//   .. IVAL = initial parameter values
      DOUBLE PRECISION  PIVAL(_max_n_params_, _max_n_energies_)
      LOGICAL           PISET(_max_n_params_, _max_n_energies_)

c//   .. IVLO, IVHI = lower (upper) bound for initial parameter values
      DOUBLE PRECISION  PIVLO(_max_n_params_)
      DOUBLE PRECISION  PIVHI(_max_n_params_)

c//   .. RVAL, RLVL, RSET = value of the ratio of one parameter to another
      DOUBLE PRECISION  PRVAL(_max_n_params_)
      INTEGER           PRLVL(_max_n_params_)
      LOGICAL           PRSET(_max_n_params_)

c//   .. PVAL, PSET = intended for future use denoting that the parameter
c//   ..              has a "prior" value set in the cost
      DOUBLE PRECISION  PPVAL(_max_n_params_)
      LOGICAL           PPSET(_max_n_params_)

c//   .. SVAL, SLVL, SSET = value of the sum of two parameters
      DOUBLE PRECISION  PSVAL(_max_n_params_)
      INTEGER           PSLVL(_max_n_params_)
      LOGICAL           PSSET(_max_n_params_)

c//   .. BPARA, BMASK = flag parameter as variable or set its toggle mask true
      LOGICAL           PPARA(_max_n_params_)
      LOGICAL           PMASK(_max_n_params_)

c//   .. ESET = set parameter (N0's) for each pulse energy
      LOGICAL           PESET(_max_n_params_)

c//   .. XSET = set parameter (N0's) as an excitation level
      LOGICAL           PXSET(_max_n_params_)

c//   Add N0SCAL, N0NEED
c//   .. INCL = indicates whether the parameter is included in the
c//   ..        chosen model
      LOGICAL           PINCL(_max_n_params_)

c//   .. NBS, NKS, NLVLS = # of branch ratios, rate constants and population levels
      INTEGER           NBS, NKS, NLVLS, PAD4BYTE


      COMMON /INITIAL_PARAMETERS/
     &                  PIVAL,
     &                  PIVLO,
     &                  PIVHI,
     &                  PRVAL,
     &                  PPVAL,
     &                  PSVAL,
     &                  PRLVL,
     &                  PSLVL,
     &                  PPARA,
     &                  PMASK,
     &                  PISET,
     &                  PRSET,
     &                  PESET,
     &                  PXSET,
     &                  PPSET,
     &                  PSSET,
     &                  PINCL,
     &                  NBS,   NKS,   NLVLS


