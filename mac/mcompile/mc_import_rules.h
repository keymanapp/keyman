
#pragma once
#ifndef MC_IMPORT_RULES_H
#define MC_IMPORT_RULES_H

/** @brief Find a keyvalue for given keycode, shiftstate and caps. A function similar to Window`s ToUnicodeEx() function. */
int mac_KMX_ToUnicodeEx(int keycode, PKMX_WCHAR pwszBuff, int ss_rgkey, int caps, const UCKeyboardLayout* keyboard_layout);

/** @brief Base class for Deadkey*/
class DeadKey {
private:
  KMX_WCHAR m_deadchar;
  std::vector<KMX_WCHAR> m_rgbasechar;
  std::vector<KMX_WCHAR> m_rgcombchar;

public:

  /** @brief Constructor */
  DeadKey(KMX_WCHAR deadCharacter);

  /** @brief return dead character */
   KMX_WCHAR KMX_DeadCharacter();

  /** @brief set member variable with base and combined character */
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

  /** @brief check if character exists in DeadKey */
  bool KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter);
};

#endif /*MC_IMPORT_RULES_H*/
