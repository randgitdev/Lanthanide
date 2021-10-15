#ifndef _DEFINE_POPULATION_LEVELS_H_
#define _DEFINE_POPULATION_LEVELS_H_

c// Define level indices
#define  _n_1_          1
#define  _n1_           _n_1_
#define  _n_2_          2
#define  _n2_           _n_2_
#define  _n_3_          3
#define  _n3_           _n_3_
#define  _n_4_          4
#define  _n4_           _n_4_
#define  _n_5_          5
#define  _n5_           _n_5_
#define  _n_6_          6
#define  _n6_           _n_6_
#define  _n_7_          7
#define  _n7_           _n_7_
#define  _n_8_          8
#define  _n8_           _n_8_
#define  _n_9_          9
#define  _n9_           _n_9_

#define  _np_1_         10
#define  _np1_          _np_1_
#define  _np_2_         11
#define  _np2_          _np_2_
c// Define colors
#define  _GREEN_        _n6_
#define  _RED_          _n5_
#define  _NIR_          _n2_

c// max # of levels in lanthanide model
#define  _maxnlvls_     _np2_

c// Define the hash index array
#define  _pl_hash_indices_ \
            _n_1_, \
            _n_2_, \
            _n_3_, \
            _n_4_, \
            _n_5_, \
            _n_6_, \
            _n_7_, \
            _n_8_, \
            _n_9_, \
            _np_1_, \
            _np_2_


c// String names corresponding to the levels.
c// Note that each column is an acceptable variation on the name.
c// Don't forget to add the backslash (\) as the last character
c// on each line except the last.
#define  _pl_hash_strings_ \
    'n_1'  , 'n1'  , 'n_1'   , 'n1'   , \
    'n_2'  , 'n2'  , 'n_2'   , 'n2'   , \
    'n_3'  , 'n3'  , 'n_3'   , 'n3'   , \
    'n_4'  , 'n4'  , 'n_4'   , 'n4'   , \
    'n_5'  , 'n5'  , 'n_5'   , 'n5'   , \
    'n_6'  , 'n6'  , 'n_6'   , 'n6'   , \
    'n_7'  , 'n7'  , 'n_7'   , 'n7'   , \
    'n_8'  , 'n8'  , 'n_8'   , 'n8'   , \
    'n_9'  , 'n9'  , 'n_9'   , 'n9'   , \
    'np_1' , 'np1' , 'n''_1' , 'n''1' , \
    'np_2' , 'np2' , 'n''_2' , 'n''2'


c// Total # of variations in hash string, i.e. columns in the
c// above matrix
#define  _pl_hash_variations_  4
#define  _pl_output_variation_ 1

c//Max population level string length
#define  _mplsl_               4


#endif

