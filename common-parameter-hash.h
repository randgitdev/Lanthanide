#include "lanthanide.h"

c//   .. Population level hash data
      CHARACTER*_mplsl_ HASH_PL_STR( _pl_hash_variations_, _maxnlvls_ )
      INTEGER           HASH_PL_INT( _maxnlvls_ )
      COMMON /HASH_POPULATION_LEVEL/ HASH_PL_STR, HASH_PL_INT


c//   .. Parameter hash data
      CHARACTER*_mpsl_  HASH_PARAM_STR( _param_hash_variations_, 
     &                                  _max_n_params_ )
      INTEGER           HASH_PARAM_IDX( _max_n_params_ )

      COMMON /HASH_PARAMETERS/ HASH_PARAM_STR, HASH_PARAM_IDX


