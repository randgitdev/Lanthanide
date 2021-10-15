#define  MAXCMP  160

***********************************************************************
*                                                                     *
*     STRING LIBRARY                                                  *
*                                                                     *
***********************************************************************
      INTEGER FUNCTION STRLEN( STR )
      IMPLICIT NONE
      CHARACTER     STR*(*)
      INTEGER       II
      
      II = LEN(STR)
c      DO WHILE ( STR(II:II).EQ.' ' )
      DO WHILE ( II.GT.0 .AND. STR(II:II).EQ.' ' )
         II = II - 1
      END DO
      STRLEN = II

      RETURN
      END



*......................................................................*
      INTEGER FUNCTION STRBEGIN( STR )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER     STR*(*)
*     .. Local
      INTEGER       II, LL

      LL = LEN(STR)

      II = 1
c      DO WHILE ( STR(II:II).EQ.' ' )
      DO WHILE ( II.LE.LL .AND. STR(II:II).EQ.' ' )
         II = II + 1
      END DO
      STRBEGIN = II

      RETURN
      END



*......................................................................*
c      SUBROUTINE STRCAT( A, B, C )
      CHARACTER*(*) FUNCTION STRCAT( A, B )
      IMPLICIT NONE
      INTEGER       STRLEN
c      CHARACTER     A*(*), B*(*), C*(*)
      CHARACTER     A*(*), B*(*)
      INTEGER       LENA, LENB

      LENA = STRLEN(A)
      LENB = STRLEN(B)

c      C = A(1:LENA) // B(1:LENB)
      STRCAT = A(1:LENA) // B(1:LENB)

      RETURN
      END



*......................................................................*
c      SUBROUTINE STRCAT( A, B, C )
      CHARACTER*(*) FUNCTION STRDCAT( A, B )
      IMPLICIT NONE
      INTEGER       STRLEN, STRBEGIN
      CHARACTER     A*(*), B*(*)
      INTEGER       BEGA, BEGB
      INTEGER       LENA, LENB

      BEGA = STRBEGIN(A)
      LENA = STRLEN(A)

      BEGB = STRBEGIN(B)
      LENB = STRLEN(B)

      STRDCAT = A(BEGA:LENA) // B(BEGB:LENB)

      RETURN
      END



*......................................................................*
c      SUBROUTINE DEBLANK( STRIN, STROUT )
      CHARACTER*(*) FUNCTION DEBLANK( STR )
      IMPLICIT NONE
      INTEGER       STRBEGIN, STRLEN
      CHARACTER     STR*(*)
c      CHARACTER     STRIN*(*)
c      CHARACTER     STROUT*(*) 
      INTEGER       B, E

c      B = STRBEGIN( STRIN )
c      E = STRLEN( STRIN )
      B = STRBEGIN( STR )
      E = STRLEN( STR )

c      STROUT = STRIN( B:E )
      DEBLANK= STR( B:E )

      RETURN
      END



*......................................................................*
      LOGICAL FUNCTION STRCMP( STR1, STR2 )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER     STR1*(*), STR2*(*)
*     .. Local
      INTEGER       S1S, S1E
      INTEGER       S2S, S2E
*     .. Functions
      INTEGER       STRBEGIN, STRLEN


      S1S = STRBEGIN( STR1 )
      S1E = STRLEN( STR1 )
      S2S = STRBEGIN( STR2 )
      S2E = STRLEN( STR2 )

c      WRITE(*,'(A/)') 'STR1=',STR1
c      WRITE(*,'(A/)') 'STR2=',STR2
c      WRITE(*,*) 'S1S= ', S1S
c      WRITE(*,*) 'S1E= ', S1E
c      WRITE(*,*) 'S2S= ', S2S
c      WRITE(*,*) 'S2E= ', S2E

      STRCMP = .FALSE.
      IF ( STR1(S1S:S1E).EQ.STR2(S2S:S2E) ) STRCMP = .TRUE.


      RETURN
      END



*......................................................................*
      LOGICAL FUNCTION STRNCMP( STR1, STR2, N )
      IMPLICIT NONE
*     .. Inputs
      CHARACTER     STR1*(*), STR2*(*)
      INTEGER       N
*     .. Local
      INTEGER       S1S, S1E
      INTEGER       S2S, S2E
*     .. Functions
      INTEGER       STRBEGIN, STRLEN

*     .. Compare the first N non-blank characters in STR1 and STR2
      S1S = STRBEGIN( STR1 )
      S1E = LEN( STR1 )
      IF ( S1E.GT.(S1S+N-1) )  S1E = S1S+N-1

      S2S = STRBEGIN( STR2 )
      S2E = LEN( STR2 )
      IF ( S2E.GT.(S2S+N-1) )  S2E = S2S+N-1

c      WRITE(*,'(A/)') 'STR1=',STR1
c      WRITE(*,'(A/)') 'STR2=',STR2
c      WRITE(*,*) 'S1S= ', S1S
c      WRITE(*,*) 'S1E= ', S1E
c      WRITE(*,*) 'S2S= ', S2S
c      WRITE(*,*) 'S2E= ', S2E

      STRNCMP = .FALSE.
      IF ( STR1(S1S:S1E).EQ.STR2(S2S:S2E) ) STRNCMP = .TRUE.


      RETURN
      END



