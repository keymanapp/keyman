#pragma once
#ifndef HELPERS_H
#define HELPERS_H

#include <iostream>
#include "mc_savekeyboard.h"
#include "mc_kmxfile.h"
#include "keymap.h"


//_S2 do not review - all this will be deleted later

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

bool writeVectorToFile(v_dw_3D V) ;
bool writeFileToVector(v_dw_3D& complete_Vector, const char* infile);
bool CompareVector_To_VectorOfFile(v_dw_3D All_Vector,v_dw_3D File_Vector);
bool test_In_Out(v_dw_3D All_Vector);

// to check if content of Vector is ok
bool write_RGKEY_FileToVector(v_dw_2D& shift_states, const char* infile) ;
bool CompareVector_To_VectorOfFile_RGKEY(v_dw_2D Win_Vector,v_dw_2D Lin_Vector, v_dw_2D Map_Vector);

#endif /* HELPERS_H*/