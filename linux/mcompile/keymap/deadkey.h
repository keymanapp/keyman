
#pragma once
#ifndef DEADKEY_H
#define DEADKEY_H

#include "mc_import_rules.h"
#include <map>

/**
 * @brief  create a Vector of DeadKey containing all combinations of deadkey + character for ALL possible Linux keyboards
 * @return vector of Deadkey* that holds all combinations of deadkey + character
 */
std::vector<DeadKey*> create_deadkeys_by_basechar();

/**
 * @brief  filter entries for the currently used Linux Keyboard out of a vector of all existing deadKey combinations
 * @param         dk        the deadkey for which all combinations will be found
 * @param[in,out] dkVec     combinations of deadkey + character for the currently used Linux Keyboard
 * @param         r_All_Vec all existing combinations of deadkey + character for ALL possible Linux keyboards
 */
void refine_alDead(KMX_WCHAR dk, std::vector<DeadKey*>& dkVec, std::vector<DeadKey*>& r_All_Vec);

/**
 * @brief  check whether a deadkey already exists in the deadkey vector
 * @param  dk    the deadkey to be found
 * @param  dkVec vector containing combinations of deadkey + character
 * @return true if deadkey alredy exists;
 *         false if not
 */
bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey*>& dkVec);

/**
 * @brief  find all deadkey combinations for a certain deadkey in a vector of all deadkey combinations
 * @param         r_dk_ComposeTable vector containing all possible deadkey combinations
 * @param         dk                deadkey of interest
 * @param[in,out] dk_SingleTable    vector containing all dk-character combinations for a specific deadkey dk
 * @return true if successful;
 *         false if not
 */
bool query_dk_combinations_for_specific_dk(vec_dword_2D& r_dk_ComposeTable, KMX_DWORD dk, vec_dword_2D& dk_SingleTable);

/**
 * @brief  convert a character to the upper-case equivalent and find the corresponding shiftstate
 *         of the entered keyval:  a(97) -> A(65) + Base   A(65) -> A(65) + Shift
 * @param         kval   keyval that might be changed
 * @param[in,out] shift  the shiftstate of the entered keyval
 * @param         keymap a pointer to the currently used (underlying) keyboard layout
 * @return the upper case equivalent of the keyval
 */
KMX_DWORD KMX_change_keyname_to_capital(KMX_DWORD kVal, KMX_DWORD& shift, GdkKeymap* keymap);

/**
 * @brief  append a 1D-vector containing name, base character and unicode_value to a 2D-Vector
 *         holding all possible combinations of deadkey + character for all Linux keyboards
 * @param[in,out] dk_ComposeTable 2D-Vector holding all possible combinations of deadkey + character
 * @param         diacritic_name  the name of a diacritic
 * @param         base_char       base character
 * @param         unicode_value   Unicode-value of the combined character
 */
void add_deadkey_combination(vec_dword_2D& dk_ComposeTable, std::string diacritic_name, std::string base_char, KMX_DWORD unicode_value);

/**
 * @brief  create a 2D-Vector containing all possible combinations of deadkey + character for all Linux keyboards
 *         the values are taken from  from: https://help.ubuntu.com/community/GtkDeadKeyTable#Accents
 *              dk_ComposeTable[i][0] : diacritic_name    		(e.g. dead_circumflex)
 *              dk_ComposeTable[i][1] : base_char   					(e.g. a)
 *              dk_ComposeTable[i][2] : unicode_value-Value   (e.g. 0x00E2)
 * @param[in,out] dk_ComposeTable
 */
void create_DKTable(vec_dword_2D& dk_ComposeTable);

#endif /*DEADKEY_H*/
