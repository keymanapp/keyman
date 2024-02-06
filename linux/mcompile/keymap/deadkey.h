
#pragma once
#ifndef DEADKEY_H
#define DEADKEY_H

#include <map>
#include "mc_import_rules.h"

// creates a vector for a dk combination ( ` + a  ->  à )
v_dw_1D createLine(std::wstring  first, std::wstring second, KMX_DWORD number, std::wstring nameresult);

// creates a 2D-vector of all dk combinations ( ` + a -> à ;  ^ + a -> â ; `+ e -> è; ...)
KMX_DWORD create_DKTable(v_dw_2D & dk_ComposeTable);

//_S2 REVIEW
std::vector<DeadKey*> create_alDead();
std::vector<DeadKey*> reduce_alDead(std::vector<DeadKey*> dk_big) ;

// finds all combination for a specific deadkey(dk)
bool find_dk_combinations_for_single_dk(v_dw_2D * dk_ComposeTable, v_dw_2D & dk_SingleTable, KMX_DWORD dk);

// gets the shifted character of a key
KMX_DWORD KMX_changeKeynameToCapital(KMX_DWORD KVal, KMX_DWORD &shift, GdkKeymap* keymap) ;

# endif /*DEADKEY_H*/