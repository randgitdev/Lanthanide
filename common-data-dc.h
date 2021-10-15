#include "lanthanide.h"

c//   .. COMMON data blocks
      DOUBLE PRECISION  DC_TIME (_maxdcbuf_, _maxndcfiles_)
      DOUBLE PRECISION  DC_DATA (_maxdcbuf_, _maxndcfiles_)
      INTEGER           DC_SIMIDX(_maxdcbuf_, _maxndcfiles_)
      INTEGER           DC_SIMLVL(_maxndcfiles_)
      DOUBLE PRECISION  DC_ENERGY(_maxndcfiles_)
      DOUBLE PRECISION  DC_SIGMA (_maxndcfiles_)
      INTEGER           DC_NDATA (_maxndcfiles_)
      INTEGER           N_DCFILES
      CHARACTER*120     DC_EXP_OUTPUT_FILE(_maxndcfiles_)
      CHARACTER*120     DC_SIM_OUTPUT_FILE(_maxndcfiles_)

c//      INTEGER           DC_NORMIDX(_maxndcfiles_)
      DOUBLE PRECISION  DC_NORMTIME(_maxndcfiles_)
      CHARACTER*30      DC_SIMNORM(_maxndcfiles_)

      COMMON /DCDATAS/
     &                  DC_TIME,   
     &                  DC_DATA,  
     &                  DC_ENERGY, 
     &                  DC_SIGMA, 
     &                  DC_NORMTIME,
     &                  DC_SIMIDX, 
     &                  DC_SIMLVL,
     &                  DC_NDATA, 
     &                  N_DCFILES,
     &                  DC_SIMNORM,
     &                  DC_EXP_OUTPUT_FILE,
     &                  DC_SIM_OUTPUT_FILE
c//     &                  DC_NORMIDX,


