#include "lanthanide.h"

c//   .. COMMON data blocks
      DOUBLE PRECISION  UNIQUE_ENERGY(_max_n_energies_)
      INTEGER           DB_II_IDX( _maxniidatas_, _max_n_energies_)
      INTEGER           DB_II_NIDX(_max_n_energies_)
      INTEGER           DB_DC_IDX( _maxndcfiles_, _max_n_energies_)
      INTEGER           DB_DC_NIDX(_max_n_energies_)
      INTEGER           N_UNIQUE_ENERGY
      INTEGER           LOWEST_ENERGY
      LOGICAL           SCALE_TO_ENERGY

      COMMON /ENERGY_DATABASE/
     &                  UNIQUE_ENERGY,
     &                  DB_DC_IDX,  DB_II_IDX,
     &                  DB_DC_NIDX, DB_II_NIDX,
     &                  N_UNIQUE_ENERGY,
     &                  LOWEST_ENERGY,
     &                  SCALE_TO_ENERGY


