
#pragma once
#ifndef MC_IMPORT_RULES_H
#define MC_IMPORT_RULES_H

class DeadKey {
private:
  KMX_WCHAR m_deadchar;
  std::vector<KMX_WCHAR> m_rgbasechar;
  std::vector<KMX_WCHAR> m_rgcombchar;

public:
  DeadKey(KMX_WCHAR deadCharacter);

  KMX_WCHAR KMX_DeadCharacter();

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

  bool KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter);
};

# endif /*MC_IMPORT_RULES_H*/
