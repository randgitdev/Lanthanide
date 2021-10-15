#include "lanthanide.h"

***********************************************************************
*                                                                     *
*     - CRITICAL ERROR -                                              *
*                                                                     *
***********************************************************************
      SUBROUTINE CRITICAL( MSG )
      IMPLICIT NONE
*     .. Input
      CHARACTER         MSG*(*)
*     .. Functions
      CHARACTER*120      STRCAT


c      WRITE(*,*) 'ERROR: ' // MSG
      WRITE(*,*) STRCAT( 'ERROR: ', MSG )

      STOP


      RETURN
      END


