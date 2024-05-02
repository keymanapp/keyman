/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <kmx/kmx_processevent.h>
#include <keyman/keyman_core_api_consts.h>
#include <state.hpp>
#include <option.hpp>

using namespace km::core;
using namespace kmx;

namespace {
  km_core_cu const
    *DEFAULT_PLATFORM                       = u"windows hardware desktop native",
    *DEFAULT_BASELAYOUT                     = u"kbdus.dll",
    *DEFAULT_BASELAYOUTALT                  = u"en-US",
    *DEFAULT_SIMULATEALTGR                  = u"0",
    *DEFAULT_BASELAYOUTGIVESCTRLRALTFORRALT = u"0";
}

KMX_Environment::KMX_Environment() {
  Set(KM_CORE_KMX_ENV_PLATFORM, DEFAULT_PLATFORM);
  Set(KM_CORE_KMX_ENV_BASELAYOUT, DEFAULT_BASELAYOUT);
  Set(KM_CORE_KMX_ENV_BASELAYOUTALT, DEFAULT_BASELAYOUTALT);
  Set(KM_CORE_KMX_ENV_SIMULATEALTGR, DEFAULT_SIMULATEALTGR);
  Set(KM_CORE_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT, DEFAULT_BASELAYOUTGIVESCTRLRALTFORRALT);
}


char16_t const * KMX_Environment::LookUp(std::u16string const & key) const {
  assert(!key.empty());
  if (!key.empty()) return nullptr;

  if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_PLATFORM)) {
    return _platform.c_str();
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_BASELAYOUT)) {
    return _baseLayout.c_str();
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_BASELAYOUTALT)) {
    return _baseLayoutAlt.c_str();
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_SIMULATEALTGR)) {
    return _simulateAltGr ? u"1" : u"0";
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT)) {
    return _baseLayoutGivesCtrlRAltForRAlt ? u"1" : u"0";
  }
  else {
    // Unsupported key
    return nullptr;
  }
}


void KMX_Environment::Set(std::u16string const & key, std::u16string const & value) {
  assert(!key.empty());

  if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_PLATFORM)) {
    _platform = value;
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_BASELAYOUT)) {
    _baseLayout = value;
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_BASELAYOUTALT)) {
    _baseLayoutAlt = value;
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_SIMULATEALTGR)) {
    _simulateAltGr = value == u"1";
  }
  else if (!u16icmp(key.c_str(), KM_CORE_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT)) {
    _baseLayoutGivesCtrlRAltForRAlt = value == u"1";
  }
  else {
    // Unsupported key
    assert(false);
  }
}
