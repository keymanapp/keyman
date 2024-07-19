
#pragma once
#ifndef MC_IMPORT_RULES_H
#define MC_IMPORT_RULES_H

/** @brief  Find a keyvalue for given keycode, shiftstate and caps. A function similar to Window`s ToUnicodeEx() function.
 *          Contrary to what the function name might suggest, the function the mac_KMX_ToUnicodeEx does NOT process surrogate pairs.
 *          This is because it is used in mcompile only which only deals with latin scripts.
 *          In case this function is used for surrogate pairs, they will be ignored and a message will be printed out
 * @param keycode a key of the currently used keyboard Layout
 * @param pwszBuff Buffer to store resulting character
 * @param ss_rgkey a Windows-style shiftstate of the currently used keyboard Layout
 * @param caps state of the caps key of the currently used keyboard Layout
 * @param keyboard_layout the currently used (underlying)keyboard Layout
 * @return  -1 if a deadkey was found;
 *           0 if no translation is available;
 *          +1 if character was found and written to pwszBuff
 */
int mac_KMX_ToUnicodeEx(int keycode, PKMX_WCHAR pwszBuff, int ss_rgkey, int caps, const UCKeyboardLayout* keyboard_layout);

/** @brief  Base class for Deadkey*/
class DeadKey {
private:
  KMX_WCHAR m_deadchar;
  std::vector<KMX_WCHAR> m_rgbasechar;
  std::vector<KMX_WCHAR> m_rgcombchar;

public:
 /**
  * @brief Constructor
  * @param deadCharacter a deadkey
  */
  DeadKey(KMX_WCHAR deadCharacter);

  /**
   * @brief  return dead character
   * @return deadkey character
   */
   KMX_WCHAR KMX_DeadCharacter();

  /**
  * @brief set member variable with base and combined character
  * @param baseCharacter the base character
  * @param combinedCharacter the combined character
  * @return void
  */
  void KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter);

  /** @brief return size of array of basecharacters */
  int KMX_Count() {
    return this->m_rgbasechar.size();
  }

  /** @brief get member variable m_deadchar */
  KMX_WCHAR KMX_GetDeadCharacter() {
    return this->m_deadchar;
  }

  /** @brief return base character at index */
  KMX_WCHAR KMX_GetBaseCharacter(int index) {
    return this->m_rgbasechar[index];
  }

  /** @brief return combined character at index */
  KMX_WCHAR KMX_GetCombinedCharacter(int index) {
    return this->m_rgcombchar[index];
  }

/**
 * @brief check if character exists in DeadKey
 * @param baseCharacter a character to be found
 * @return true if found; false if not found
 */
  bool KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter);
};

#endif /*MC_IMPORT_RULES_H*/
