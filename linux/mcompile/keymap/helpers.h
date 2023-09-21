#pragma once
#ifndef HELPERS_H
#define HELPERS_H

#include <iostream>
#include "mc_savekeyboard.h"
#include "mc_kmxfile.h"
#include "keymap.h"

// why again here?
typedef std::vector<std::string> v_str_1D;
typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;


int test_helpers();

// append characters using VectorFile (Data for Other Language on [1][ ][ ]  )
int append_other_ToVector(v_dw_3D &All_Vector) ;

// get Keyvals from File and insert into All_Vector
bool InsertKeyvalsFromVectorFile(v_dw_3D &complete_Vector) ;

// create a Vector with all entries of file
int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector);

#endif /* HELPERS_H*/