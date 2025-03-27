
#pragma once
#ifndef DEADKEY_H
#define DEADKEY_H

#include "mc_import_rules.h"
#include <map>

/** @brief create a Vector of DeadKey containing all combinations of deadkey + character for ALL possible Linux keyboards */
std::vector<DeadKey*> create_deadkeys_by_basechar();

/** @brief filter entries for the currently used Linux Keyboard out of a vector of all existing deadKey combinations */
void refine_alDead(KMX_WCHAR dk, std::vector<DeadKey*>& dkVec, std::vector<DeadKey*>& r_All_Vec);

/** @brief check whether a deadkey already exists in the deadkey vector */
bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey*>& dkVec);

/** @brief find all deadkey combinations for a certain deadkey in a vector of all deadkey combinations */
bool query_dk_combinations_for_specific_dk(vec_dword_2D& r_dk_ComposeTable, KMX_DWORD dk, vec_dword_2D& dk_SingleTable);

/** @brief convert a character to the upper-case equivalent and find the corresponding shiftstate of the entered keyval */
KMX_DWORD KMX_change_keyname_to_capital(KMX_DWORD kVal, KMX_DWORD& shift, GdkKeymap* keymap);

/** @brief append a 1D-vector containing name, base character and unicode_value to a 2D-Vector */
void add_deadkey_combination(vec_dword_2D& dk_ComposeTable, std::string diacritic_name, std::string base_char, KMX_DWORD unicode_value);

/** @brief create a 2D-Vector containing all possible combinations of deadkey + character for all Linux keyboards */
void create_DKTable(vec_dword_2D& dk_ComposeTable);

#endif /*DEADKEY_H*/
