
#pragma once
#ifndef LIN_DEADKEY_H
#define LIN_DEADKEY_H

#include "LIN_mc_import_rules.h"
#include <map>


// create a 2D-vector of all dk combinations ( ` + a -> à ;  ^ + a -> â ; `+ e -> è; ...)
void create_DKTable(vec_dword_2D &dk_ComposeTable);

// find all possible dk combinations that exist
std::vector<DeadKey *> create_deadkeys_by_basechar();

// refine dk to those used in the underlying keyboard
void refine_alDead(KMX_WCHAR dk, std::vector<DeadKey *> &myVec, std::vector<DeadKey *> &r_All_Vec);

// check if entry is already there
bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey *> &myVec);

// query_dk_combinations_for_a specific_dk from dk_ComposeTable(which holds all dk combinations) e.g. for dk: ^ get â,ê,î,ô,û,...
bool query_dk_combinations_for_specific_dk(vec_dword_2D &dk_ComposeTable, KMX_DWORD dk, vec_dword_2D &dk_SingleTable);

// get the shifted character of a key and write shiftstate of KVal to shift
KMX_DWORD KMX_change_keyname_to_capital(KMX_DWORD kVal, KMX_DWORD &shift, GdkKeymap *keymap);

#endif /*LIN_DEADKEY_H*/
