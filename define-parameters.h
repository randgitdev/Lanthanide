#ifndef _DEFINE_PARAMETERS_H_
#define _DEFINE_PARAMETERS_H_

c// To define a new model parameter, the following 3 steps must be completed
c// 1.  Define the preprocessor index mneumonic
c// 2.  Add the index mneumonic to the hash index array
c// 3.  Parameter name strings


c///////////////////////////////////////////////////////////////////////
c// 1. Define a preprocessor index mneumonic for the parameter
c//    This mneumonic may be used throughout the simulation.
c//    For example, see model-eryb.fpp
c//    Just add it to the bottom of the list and update _max_n_params_

c// Define branching ratio indices
#define  _B_n2n1_       1

#define  _B_n3n1_       2
#define  _B_n3n2_       3

#define  _B_n4n1_       4
#define  _B_n4n2_       5
#define  _B_n4n3_       6

#define  _B_n5n1_       7
#define  _B_n5n2_       8
#define  _B_n5n3_       9
#define  _B_n5n4_       10

#define  _B_n6n1_       11
#define  _B_n6n2_       12
#define  _B_n6n3_       13
#define  _B_n6n4_       14
#define  _B_n6n5_       15

#define  _B_n7n1_       16
#define  _B_n7n2_       17
#define  _B_n7n3_       18
#define  _B_n7n4_       19
#define  _B_n7n5_       20
#define  _B_n7n6_       21

c// Define rate constant indices
c// NOTE: Just add the index of the previous parameter so that
c//       we can just start numbering from 1 again.
#define  _k_CR_         22
#define  _k_CR1_        23
#define  _k_CR2_        24

#define  _k_ET1_        25
#define  _k_ET2_        26
#define  _k_ET3_        27
#define  _k_ET4_        28
#define  _kp_ET1_       29
#define  _kp_ET2_       30

#define  _k_R1_         31
#define  _k_R2_         32
#define  _k_R3_         33
#define  _k_R4_         34
#define  _k_R5_         35
#define  _k_R6_         36
#define  _k_R7_         37
#define  _k_R8_         38
#define  _k_R9_         39

#define  _k_NR1_        40
#define  _k_NR2_        41
#define  _k_NR3_        42
#define  _k_NR4_        43
#define  _k_NR5_        44

#define  _k_Yb_         45
#define  _k_n2_         46

c// Define level indices
c// NOTE: Just add the index of the previous parameter so that
c//       we can just start numbering from 1 again.
#define  _np1_0_        47
#define  _np2_0_        48

#define  _n1_0_         49
#define  _n2_0_         50
#define  _n3_0_         51
#define  _n4_0_         52
#define  _n5_0_         53
#define  _n6_0_         54
#define  _n7_0_         55
#define  _n8_0_         56
#define  _n9_0_         57

c// Define absorption cross section and other parameters here
#define  _D_            58
#define  _F_            59


c// Define the maximum number of parameters that we have here.
#define  _max_n_params_   _F_




c///////////////////////////////////////////////////////////////////////
c// 2. Define the hash index array
c//    Place all of our parameter indices into an array.
#define  _param_hash_indices_ \
            _B_n2n1_, \
            _B_n3n1_, \
            _B_n3n2_, \
            _B_n4n1_, \
            _B_n4n2_, \
            _B_n4n3_, \
            _B_n5n1_, \
            _B_n5n2_, \
            _B_n5n3_, \
            _B_n5n4_, \
            _B_n6n1_, \
            _B_n6n2_, \
            _B_n6n3_, \
            _B_n6n4_, \
            _B_n6n5_, \
            _B_n7n1_, \
            _B_n7n2_, \
            _B_n7n3_, \
            _B_n7n4_, \
            _B_n7n5_, \
            _B_n7n6_, \
            _k_CR_,   \
            _k_CR1_,  \
            _k_CR2_,  \
            _k_ET1_,  \
            _k_ET2_,  \
            _k_ET3_,  \
            _k_ET4_,  \
            _kp_ET1_, \
            _kp_ET2_, \
            _k_R1_,   \
            _k_R2_,   \
            _k_R3_,   \
            _k_R4_,   \
            _k_R5_,   \
            _k_R6_,   \
            _k_R7_,   \
            _k_R8_,   \
            _k_R9_,   \
            _k_NR1_,  \
            _k_NR2_,  \
            _k_NR3_,  \
            _k_NR4_,  \
            _k_NR5_,  \
            _k_Yb_,   \
            _k_n2_,   \
            _np1_0_,  \
            _np2_0_,  \
            _n1_0_,   \
            _n2_0_,   \
            _n3_0_,   \
            _n4_0_,   \
            _n5_0_,   \
            _n6_0_,   \
            _n7_0_,   \
            _n8_0_,   \
            _n9_0_,   \
            _D_,      \
            _F_


c///////////////////////////////////////////////////////////////////////
c// 3.  Parameter name strings
c//     String names corresponding to all of our model parameters.
c//     These allow for the command file to be parsed and also for
c//     simulation output to be generated.
c//     Note that each column is an acceptable variation on the name.
c//     Don't forget to add the backslash (\) as the last character
c//     on each line except the last.
c//     Make sure that each string is no longer that the defined maximum
c//     parameter string length (mpsl) set below.
#define  _param_hash_strings_ \
   'B_n2n1'  , 'B_n2->n1', 'Bn2n1'  , 'Bn2n1' , 'Bn2n1', 'Bn2n1', 'Bn2n1', 'Bn2n1', \
   'B_n3n1'  , 'B_n3->n1', 'Bn3n1'  , 'Bn3n1' , 'Bn3n1', 'Bn3n1', 'Bn3n1', 'Bn3n1', \
   'B_n3n2'  , 'B_n3->n2', 'Bn3n2'  , 'Bn3n2' , 'Bn3n2', 'Bn3n2', 'Bn3n2', 'Bn3n2', \
   'B_n4n1'  , 'B_n4->n1', 'Bn4n1'  , 'Bn4n1' , 'Bn4n1', 'Bn4n1', 'Bn4n1', 'Bn4n1', \
   'B_n4n2'  , 'B_n4->n2', 'Bn4n2'  , 'Bn4n2' , 'Bn4n2', 'Bn4n2', 'Bn4n2', 'Bn4n2', \
   'B_n4n3'  , 'B_n4->n3', 'Bn4n3'  , 'Bn4n3' , 'Bn4n3', 'Bn4n3', 'Bn4n3', 'Bn4n3', \
   'B_n5n1'  , 'B_n5->n1', 'Bn5n1'  , 'Bn5n1' , 'Bn5n1', 'Bn5n1', 'Bn5n1', 'Bn5n1', \
   'B_n5n2'  , 'B_n5->n2', 'Bn5n2'  , 'Bn5n2' , 'Bn5n2', 'Bn5n2', 'Bn5n2', 'Bn5n2', \
   'B_n5n3'  , 'B_n5->n3', 'Bn5n3'  , 'Bn5n3' , 'Bn5n3', 'Bn5n3', 'Bn5n3', 'Bn5n3', \
   'B_n5n4'  , 'B_n5->n4', 'Bn5n4'  , 'Bn5n4' , 'Bn5n4', 'Bn5n4', 'Bn5n4', 'Bn5n4', \
   'B_n6n1'  , 'B_n6->n1', 'Bn6n1'  , 'Bn6n1' , 'Bn6n1', 'Bn6n1', 'Bn6n1', 'Bn6n1', \
   'B_n6n2'  , 'B_n6->n2', 'Bn6n2'  , 'Bn6n2' , 'Bn6n2', 'Bn6n2', 'Bn6n2', 'Bn6n2', \
   'B_n6n3'  , 'B_n6->n3', 'Bn6n3'  , 'Bn6n3' , 'Bn6n3', 'Bn6n3', 'Bn6n3', 'Bn6n3', \
   'B_n6n4'  , 'B_n6->n4', 'Bn6n4'  , 'Bn6n4' , 'Bn6n4', 'Bn6n4', 'Bn6n4', 'Bn6n4', \
   'B_n6n5'  , 'B_n6->n5', 'Bn6n5'  , 'Bn6n5' , 'Bn6n5', 'Bn6n5', 'Bn6n5', 'Bn6n5', \
   'B_n7n1'  , 'B_n7->n1', 'Bn7n1'  , 'Bn7n1' , 'Bn7n1', 'Bn7n1', 'Bn7n1', 'Bn7n1', \
   'B_n7n2'  , 'B_n7->n2', 'Bn7n2'  , 'Bn7n2' , 'Bn7n2', 'Bn7n2', 'Bn7n2', 'Bn7n2', \
   'B_n7n3'  , 'B_n7->n3', 'Bn7n3'  , 'Bn7n3' , 'Bn7n3', 'Bn7n3', 'Bn7n3', 'Bn7n3', \
   'B_n7n4'  , 'B_n7->n4', 'Bn7n4'  , 'Bn7n4' , 'Bn7n4', 'Bn7n4', 'Bn7n4', 'Bn7n4', \
   'B_n7n5'  , 'B_n7->n5', 'Bn7n5'  , 'Bn7n5' , 'Bn7n5', 'Bn7n5', 'Bn7n5', 'Bn7n5', \
   'B_n7n6'  , 'B_n7->n6', 'Bn7n6'  , 'Bn7n6' , 'Bn7n6', 'Bn7n6', 'Bn7n6', 'Bn7n6', \
   'k_CR'    , 'k_CR'    , 'kCR'    , 'kCR'   , 'kCR'  , 'kCR'  , 'kCR'  , 'kCR'  , \
   'k_CR1'   , 'k_CR1'   , 'kCR1'   , 'kCR1'  , 'kCR1' , 'kCR1' , 'kCR1' , 'kCR1' , \
   'k_CR2'   , 'k_CR2'   , 'kCR2'   , 'kCR2'  , 'kCR2' , 'kCR2' , 'kCR2' , 'kCR2' , \
   'k_ET1'   , 'k_ET1'   , 'kET1'   , 'kET1'  , 'kET1' , 'kET1' , 'kET1' , 'kET1' , \
   'k_ET2'   , 'k_ET2'   , 'kET2'   , 'kET2'  , 'kET2' , 'kET2' , 'kET2' , 'kET2' , \
   'k_ET3'   , 'k_ET3'   , 'kET3'   , 'kET3'  , 'kET3' , 'kET3' , 'kET3' , 'kET3' , \
   'k_ET4'   , 'k_ET4'   , 'kET4'   , 'kET4'  , 'kET4' , 'kET4' , 'kET4' , 'kET4' , \
   'k''_ET1' , 'kp_ET1'  , 'k''ET1' , 'kpET1' , 'kpET1', 'kpET1', 'kpET1', 'kpET1', \
   'k''_ET2' , 'kp_ET2'  , 'k''ET2' , 'kpET2' , 'kpET2', 'kpET2', 'kpET2', 'kpET2', \
   'k_R1'    , 'k_R1'    , 'kR1'    , 'kR1'   , 'kR1'  , 'kR1'  , 'kR1'  , 'kR1'  , \
   'k_R2'    , 'k_R2'    , 'kR2'    , 'kR2'   , 'kR2'  , 'kR2'  , 'kR2'  , 'kR2'  , \
   'k_R3'    , 'k_R3'    , 'kR3'    , 'kR3'   , 'kR3'  , 'kR3'  , 'kR3'  , 'kR3'  , \
   'k_R4'    , 'k_R4'    , 'kR4'    , 'kR4'   , 'kR4'  , 'kR4'  , 'kR4'  , 'kR4'  , \
   'k_R5'    , 'k_R5'    , 'kR5'    , 'kR5'   , 'kR5'  , 'kR5'  , 'kR5'  , 'kR5'  , \
   'k_R6'    , 'k_R6'    , 'kR6'    , 'kR6'   , 'kR6'  , 'kR6'  , 'kR6'  , 'kR6'  , \
   'k_R7'    , 'k_R7'    , 'kR7'    , 'kR7'   , 'kR7'  , 'kR7'  , 'kR7'  , 'kR7'  , \
   'k_R8'    , 'k_R8'    , 'kR8'    , 'kR8'   , 'kR8'  , 'kR8'  , 'kR8'  , 'kR8'  , \
   'k_R9'    , 'k_R9'    , 'kR9'    , 'kR9'   , 'kR9'  , 'kR9'  , 'kR9'  , 'kR9'  , \
   'k_NR1'   , 'k_NR1'   , 'kNR1'   , 'kNR1'  , 'kNR1' , 'kNR1' , 'kNR1' , 'kNR1' , \
   'k_NR2'   , 'k_NR2'   , 'kNR2'   , 'kNR2'  , 'kNR2' , 'kNR2' , 'kNR2' , 'kNR2' , \
   'k_NR3'   , 'k_NR3'   , 'kNR3'   , 'kNR3'  , 'kNR3' , 'kNR3' , 'kNR3' , 'kNR3' , \
   'k_NR4'   , 'k_NR4'   , 'kNR4'   , 'kNR4'  , 'kNR4' , 'kNR4' , 'kNR4' , 'kNR4' , \
   'k_NR5'   , 'k_NR5'   , 'kNR5'   , 'kNR5'  , 'kNR5' , 'kNR5' , 'kNR5' , 'kNR5' , \
   'k_Yb'    , 'k_Yb'    , 'kYb'    , 'kYb'   , 'kYb'  , 'kYb'  , 'kYb'  , 'kYb'  , \
   'k_n2'    , 'k_n2'    , 'kn2'    , 'kn2'   , 'kn2'  , 'kn2'  , 'kn2'  , 'kn2'  , \
   'n''_1(0)', 'np_1(0)' , 'n''1(0)', 'np1(0)', 'n''_1', 'np_1' , 'n''1' , 'np1'  , \
   'n''_2(0)', 'np_2(0)' , 'n''2(0)', 'np2(0)', 'n''_2', 'np_2' , 'n''2' , 'np2'  , \
   'n_1(0)'  , 'n_1(0)'  , 'n1(0)'  , 'n1(0)' , 'n_1'  , 'n_1'  , 'n1'   , 'n1'   , \
   'n_2(0)'  , 'n_2(0)'  , 'n2(0)'  , 'n2(0)' , 'n_2'  , 'n_2'  , 'n2'   , 'n2'   , \
   'n_3(0)'  , 'n_3(0)'  , 'n3(0)'  , 'n3(0)' , 'n_3'  , 'n_3'  , 'n3'   , 'n3'   , \
   'n_4(0)'  , 'n_4(0)'  , 'n4(0)'  , 'n4(0)' , 'n_4'  , 'n_4'  , 'n4'   , 'n4'   , \
   'n_5(0)'  , 'n_5(0)'  , 'n5(0)'  , 'n5(0)' , 'n_5'  , 'n_5'  , 'n5'   , 'n5'   , \
   'n_6(0)'  , 'n_6(0)'  , 'n6(0)'  , 'n6(0)' , 'n_6'  , 'n_6'  , 'n6'   , 'n6'   , \
   'n_7(0)'  , 'n_7(0)'  , 'n7(0)'  , 'n7(0)' , 'n_7'  , 'n_7'  , 'n7'   , 'n7'   , \
   'n_8(0)'  , 'n_8(0)'  , 'n8(0)'  , 'n8(0)' , 'n_8'  , 'n_8'  , 'n8'   , 'n8'   , \
   'n_9(0)'  , 'n_9(0)'  , 'n9(0)'  , 'n9(0)' , 'n_9'  , 'n_9'  , 'n9'   , 'n9'   , \
   'D'       , 'd'       , 'd'      , 'd'     , 'd'    , 'd'    , 'd'    , 'd'    , \
   'F'       , 'f'       , 'f'      , 'f'     , 'f'    , 'f'    , 'f'    , 'f'


c// Total # of variations for each hash string, i.e. columns, in the
c// above matrix
#define  _param_hash_variations_  8
#define  _param_output_variation_ 1

c// Define the maximum string length of a parameter name.
c// Max Parameter String Length = mpsl
#define  _mpsl_                   8


#endif

