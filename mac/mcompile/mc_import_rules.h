
#pragma once
#ifndef MC_IMPORT_RULES_H
#define MC_IMPORT_RULES_H

int mac_KMX_ToUnicodeEx(int keycode, PKMX_WCHAR pwszBuff, int shift_state_pos, int caps, const UCKeyboardLayout* keyboard_layout);

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
 * @brief set Deadkey with values
 * @param baseCharacter the base character
 * @param combinedCharacter the combined character
 * @return void
 */
  void KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter);

  int KMX_Count() {
    return this->m_rgbasechar.size();
  }

  KMX_WCHAR KMX_GetDeadCharacter() {
    return this->m_deadchar;
  }

  KMX_WCHAR KMX_GetBaseCharacter(int index) {
    return this->m_rgbasechar[index];
  }

  KMX_WCHAR KMX_GetCombinedCharacter(int index) {
    return this->m_rgcombchar[index];
  }

/**
 * @brief check if character exists in DeadKey
 * @param baseCharacter
 * @return true if found; false if not found
 */
  bool KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter);
};

#endif  // MC_IMPORT_RULES_H
