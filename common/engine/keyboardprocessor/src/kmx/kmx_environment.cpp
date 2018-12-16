/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <kmx/kmx_processor.h>
#include <keyman/keyboardprocessor_consts.h>
#include <state.hpp>
#include <option.hpp>

using namespace km::kbp::kmx;

namespace {
  km_kbp_cp const
    *DEFAULT_PLATFORM                       = u"windows hardware desktop native",
    *DEFAULT_BASELAYOUT                     = u"kbdus.dll",
    *DEFAULT_BASELAYOUTALT                  = u"en-US",
    *DEFAULT_SIMULATEALTGR                  = u"0",
    *DEFAULT_CAPSLOCK                       = u"0",
    *DEFAULT_BASELAYOUTGIVESCTRLRALTFORRALT = u"0";
}

KMX_Environment::KMX_Environment() {
}

void KMX_Environment::InitOption(std::vector<option> &default_env, km_kbp_cp const *key, km_kbp_cp const *default_value) {
  default_env.emplace_back(KM_KBP_OPT_ENVIRONMENT, key, default_value);
  Load(key, default_value);
}

void KMX_Environment::Init(std::vector<option> &default_env) {
  InitOption(default_env, KM_KBP_KMX_ENV_PLATFORM, DEFAULT_PLATFORM);
  InitOption(default_env, KM_KBP_KMX_ENV_BASELAYOUT, DEFAULT_BASELAYOUT);
  InitOption(default_env, KM_KBP_KMX_ENV_BASELAYOUTALT, DEFAULT_BASELAYOUTALT);
  InitOption(default_env, KM_KBP_KMX_ENV_SIMULATEALTGR, DEFAULT_SIMULATEALTGR);
  InitOption(default_env, KM_KBP_KMX_ENV_CAPSLOCK, DEFAULT_CAPSLOCK);
  InitOption(default_env, KM_KBP_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT, DEFAULT_BASELAYOUTGIVESCTRLRALTFORRALT);

  // TODO refactor this and the keyboard option terminator into state.cpp and keyboard.cpp respectively
  default_env.emplace_back();
}

void KMX_Environment::Load(std::u16string const & key, std::u16string const & value) {
  assert(!key.empty());
  
  if (!u16icmp(key.c_str(), KM_KBP_KMX_ENV_PLATFORM)) {
    _platform = value;
  }
  else if (!u16icmp(key.c_str(), KM_KBP_KMX_ENV_BASELAYOUT)) {
    _baseLayout = value;
  }
  else if (!u16icmp(key.c_str(), KM_KBP_KMX_ENV_BASELAYOUTALT)) {
    _baseLayoutAlt = value;
  }
  else if (!u16icmp(key.c_str(), KM_KBP_KMX_ENV_SIMULATEALTGR)) {
    _simulateAltGr = value == u"1";
  } 
  else if (!u16icmp(key.c_str(), KM_KBP_KMX_ENV_CAPSLOCK)) {
    _capsLock = value == u"1";
  }
  else if (!u16icmp(key.c_str(), KM_KBP_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT)) {
    _baseLayoutGivesCtrlRAltForRAlt = value == u"1";
  }
  else {
    // Unsupported key
    assert(false);
  }
}
