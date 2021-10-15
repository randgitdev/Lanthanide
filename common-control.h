#include "lanthanide.h"

c//   .. COMMON data block
      LOGICAL           OUTPUT
      INTEGER           OUTPUT_SKIP
      INTEGER           OUTPUT_LIMIT

      LOGICAL           OUTPUT_COND
      INTEGER           OUTPUT_COND_LIMIT

      INTEGER           VERBOSE

      COMMON /COMMON_CONTROL/
     &                  OUTPUT, OUTPUT_SKIP, OUTPUT_LIMIT,
     &                  OUTPUT_COND, OUTPUT_COND_LIMIT,
     &                  VERBOSE


      DOUBLE PRECISION  FINAL_VPVEC(_max_n_params_)
      DOUBLE PRECISION  FINAL_COST
      INTEGER           FINAL_ITERS
      INTEGER           REPEAT_TOTAL
      INTEGER           REPEAT_COUNT

      COMMON /FINAL_OUTPUT/
     &                  FINAL_VPVEC, FINAL_COST, FINAL_ITERS,
     &                  REPEAT_COUNT, REPEAT_TOTAL

