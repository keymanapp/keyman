
#pragma once
#ifndef MC_IMPORT_RULES_H
#define MC_IMPORT_RULES_H
// _S2 ToDo open deadkey functions
class DeadKey {
private:
  KMX_WCHAR m_deadchar;
  std::vector<KMX_WCHAR> m_rgbasechar;
  std::vector<KMX_WCHAR> m_rgcombchar;

public:
  DeadKey(KMX_WCHAR deadCharacter) ;
  KMX_WCHAR KMX_DeadCharacter() ;

  void KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter) ;
  int KMX_Count() {
    return this->m_rgbasechar.size();
  }

  KMX_WCHAR KMX_GetBaseCharacter(int index) {
    return this->m_rgbasechar[index];
  }

  KMX_WCHAR KMX_GetCombinedCharacter(int index) {
    return this->m_rgcombchar[index];
  }

  bool ContainsBaseCharacter(WCHAR baseCharacter);


  bool KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter) ;
};

class DeadKey1 {
private:
  KMX_WCHAR m_deadchar;
  std::vector<KMX_WCHAR> m_rgbasechar;
  std::vector<KMX_WCHAR> m_rgcombchar;

public:
  DeadKey1(KMX_WCHAR deadCharacter) {
    this->m_deadchar = deadCharacter;
  }

  KMX_WCHAR KMX_DeadCharacter() {
    return this->m_deadchar;
  }

  void KMX_AddDeadKeyRow(KMX_WCHAR baseCharacter, KMX_WCHAR combinedCharacter) {
    this->m_rgbasechar.push_back(baseCharacter);
    this->m_rgcombchar.push_back(combinedCharacter);
  }

  int KMX_Count() {
    return this->m_rgbasechar.size();
  }

  KMX_WCHAR KMX_GetBaseCharacter(int index) {
    return this->m_rgbasechar[index];
  }

  KMX_WCHAR KMX_GetCombinedCharacter(int index) {
    return this->m_rgcombchar[index];
  }
/*
  bool ContainsBaseCharacter(WCHAR baseCharacter) {
    std::vector<WCHAR>::iterator it;
    for(it=this->m_rgbasechar.begin(); it<m_rgbasechar.end(); it++) {
      if(*it == baseCharacter) {
        return true;
      }
    }
    return false;
  }*/


  bool KMX_ContainsBaseCharacter(KMX_WCHAR baseCharacter) {
    std::vector<KMX_WCHAR>::iterator it;
    for(it=this->m_rgbasechar.begin(); it<m_rgbasechar.end(); it++) {
      if(*it == baseCharacter) {
        return true;
      }
    }
    return false;
  }
};


# endif /*MC_IMPORT_RULES_H*/
