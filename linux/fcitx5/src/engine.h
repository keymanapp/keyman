/*
 * Keyman Input Method for Fcitx
 *
 * Copyright (C) 2021 Google LLC
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */
#ifndef _FCITX5_ENGINE_H_
#define _FCITX5_ENGINE_H_

#include <memory>
#include <fcitx-config/configuration.h>
#include <fcitx-config/iniparser.h>
#include <fcitx-utils/event.h>
#include <fcitx-utils/eventdispatcher.h>
#include <fcitx-utils/i18n.h>
#include <fcitx-utils/library.h>
#include <fcitx-utils/log.h>
#include <fcitx/action.h>
#include <fcitx/addonfactory.h>
#include <fcitx/addonmanager.h>
#include <fcitx/icontheme.h>
#include <fcitx/inputcontextproperty.h>
#include <fcitx/inputmethodengine.h>
#include <fcitx/instance.h>
#include <fcitx/menu.h>
#include <keyman/keyboardprocessor.h>
#include "kmpmetadata.h"

namespace fcitx {

class KeymanState;
class KeymanKeyboard;

FCITX_CONFIGURATION(KeymanConfig,
                    ExternalOption config{this, "Configuration",
                                          _("Configuration"), "km-config"};);

class KeymanKeyboardData {
public:
    KeymanKeyboardData(Instance *instance, const KeymanKeyboard &metadata);
    ~KeymanKeyboardData();

    void load();
    const auto &metadata() const { return metadata_; }
    auto *kbpKeyboard() const { return keyboard_; }
    const auto &factory() const { return factory_; }
    void setOption(const km_kbp_cp *key, const km_kbp_cp *value);

private:
    Instance *instance_;
    bool loaded_ = false;
    std::string ldmlFile_;
    const KeymanKeyboard &metadata_;
    km_kbp_keyboard *keyboard_ = nullptr;
    FactoryFor<KeymanState> factory_;
    RawConfig config_;
};

class KeymanKeyboard : public InputMethodEntryUserData {
public:
    KeymanKeyboard(Instance *instance, const KmpKeyboardMetadata &keyboard,
                   const KmpMetadata &metadata, const std::string &dir)
        : id(keyboard.id), version(keyboard.version), baseDir(dir),
          name(keyboard.name),
          language(keyboard.languages.empty() ? ""
                                              : keyboard.languages[0].first),
          readme(metadata.readmeFile()), graphic(metadata.graphicFile()),
          data_(instance, *this) {}
    const std::string id;
    const std::string version;
    const std::string baseDir;
    const std::string name;
    const std::string language;
    const std::string readme;
    const std::string graphic;

    void load() const { data_.load(); }
    KeymanKeyboardData &data() const { return data_; }

private:
    mutable KeymanKeyboardData data_;
};

class KeymanEngine final : public InputMethodEngineV2 {
public:
    KeymanEngine(Instance *instance);
    Instance *instance() { return instance_; }
    void activate(const fcitx::InputMethodEntry &,
                  fcitx::InputContextEvent &) override;
    void keyEvent(const InputMethodEntry &entry, KeyEvent &keyEvent) override;
    std::vector<InputMethodEntry> listInputMethods() override;
    void reset(const fcitx::InputMethodEntry &,
               fcitx::InputContextEvent &) override;
    const fcitx::Configuration *getConfig() const override { return &config_; }
    std::string subMode(const fcitx::InputMethodEntry &,
                        fcitx::InputContext &) override;

private:
    FCITX_ADDON_DEPENDENCY_LOADER(dbus, instance_->addonManager());
    KeymanState *state(const fcitx::InputMethodEntry &entry,
                       fcitx::InputContext &ic);

    Instance *instance_;
    KeymanConfig config_;
};

class KeymanEngineFactory : public AddonFactory {
public:
    AddonInstance *create(AddonManager *manager) override {
        registerDomain("fcitx5-keyman", FCITX_INSTALL_LOCALEDIR);
        return new KeymanEngine(manager->instance());
    }
};
} // namespace fcitx

#endif // _FCITX_RIMEENGINE_H_
