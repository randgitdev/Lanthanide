/* in "csigfun.c" */
#include <signal.h>

typedef void (*sighandler_t)(int);

void signal_( int* signum, sighandler_t handler )
{
    signal( *signum, handler );
}


