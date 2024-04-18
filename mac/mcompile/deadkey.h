#pragma once
#ifndef DEADKEY_H
#define DEADKEY_H

#include <map>
#include "mc_import_rules.h"
//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################

/*

// create a vector for a dk combination ( ` + a  ->  à )
v_dw_1D createLine(std::string  first, std::string second, KMX_DWORD number, std::string nameresult);

// create a 2D-vector of all dk combinations ( ` + a -> à ;  ^ + a -> â ; `+ e -> è; ...)
void create_DKTable(v_dw_2D & dk_ComposeTable);

// find all possible dk combinations that exist
std::vector<DeadKey*> create_alDead();

// refine dk to those used in the underlying keyboard
void refine_alDead(KMX_WCHAR dk, std::vector<DeadKey*> &myVec, std::vector<DeadKey*> *p_All_Vec);
// check if entry is already there
bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey*> &myVec);

// get all combination for a specific deadkey(dk) from the dk_vector query_dk_combinations_for_specific_dk which holds all possible dk: ^-> â,ê,î,ô,û,...
bool query_dk_combinations_for_specific_dk(v_dw_2D * dk_ComposeTable, v_dw_2D & dk_SingleTable, KMX_DWORD dk);

// get the shifted character of a key and write shiftstate of KVal to shift
KMX_DWORD KMX_changeKeynameToCapital(KMX_DWORD KVal, KMX_DWORD &shift, GdkKeymap* keymap);
*/ 
# endif //DEADKEY_H