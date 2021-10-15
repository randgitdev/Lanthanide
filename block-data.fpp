#include "lanthanide.h"



      BLOCK DATA
      IMPLICIT NONE

*     .. COMMON data blocks
#include "common-parameter-hash.h"

*     .. Population level hash data
      DATA HASH_PL_STR / _pl_hash_strings_ /
      DATA HASH_PL_INT / _pl_hash_indices_ /

*     .. Parameter hash data
      DATA HASH_PARAM_STR / _param_hash_strings_ /
      DATA HASH_PARAM_IDX / _param_hash_indices_ /

      END

