#ifndef  _LANTHANIDE_H_
#define  _LANTHANIDE_H_

c// Parameter definition files
#include "define-population-levels.h"
#include "define-parameters.h"


c// Physical constants
#define  _zero_         0.0D+0
#define  _one_          1.0D+0
#define  _huge_         1.0D+100

#define  _pi_           3.141592653589793D+00
#define  _hplanck_      6.6260689633D-34
#define  _c0_           299792458


#define  _maxndcfiles_  100
#define  _maxniidatas_  20
#define  _maxncwdatas_  20

#define  _max_n_energies_  _maxndcfiles_+_maxniidatas_


c// Maximum number of expermimental data points
#define  _maxdcbuf_     20000
c// Maximum length of simulated model array
#define  _maxnsim_      2000000


c// max # of parameters total (# k's  +  # b's  +  # n0's + d )
c//#define  MAXNPS         _maxnbs_ + _maxnks_ + _maxnlvls_ + 1
c//#define  _maxnps_       _maxnbs_ + _maxnks_ + _maxnlvls_ + 1
#define  _maxnps_       _max_n_params_


c// Verbosity levels
#define  _vb_vlf_       0
#define  _vb_lf_        1
#define  _vb_mf_        2
#define  _vb_hf_        3
#define  _vb_vhf_       4


c// Common declarations

#endif


