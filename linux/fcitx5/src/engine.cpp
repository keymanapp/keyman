/*
 * Keyman Input Method for Fcitx
 *
 * Copyright (C) 2018 SIL International
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

// The file is forked from ibus-keyman/src/engine.c
#include "engine.h"
#include <fcntl.h>
#include <fcitx-utils/inputbuffer.h>
#include <fcitx-utils/log.h>
#include <fcitx-utils/utf8.h>
#include <keyman/keyboardprocessor.h>
#include "kmpdata.h"
#include "kmpmetadata.h"

#define MAXCONTEXT_ITEMS 128
#define KEYMAN_BACKSPACE 14
#define KEYMAN_BACKSPACE_KEYSYM 0xff08
#define KEYMAN_LCTRL 29
#define KEYMAN_LALT 56
#define KEYMAN_RCTRL 97
#define KEYMAN_RALT 100

FCITX_DEFINE_LOG_CATEGORY(keyman, "keyman");
#define FCITX_KEYMAN_DEBUG() FCITX_LOGC(::keyman, Debug)
#define FCITX_KEYMAN_ERROR() FCITX_LOGC(::keyman, Error)

namespace fcitx {

std::vector<char16_t> utf8ToUTF16(std::string_view str) {
    if (!utf8::validate(str)) {
        return {};
    }
    std::vector<char16_t> result;
    for (const auto ucs4 : utf8::MakeUTF8CharRange(str)) {
        if (ucs4 < 0x10000) {
            result.push_back(static_cast<char16_t>(ucs4));
        } else if (ucs4 < 0x110000) {
            result.push_back(0xD800 | (((ucs4 - 0x10000) >> 10) & 0x3ff));
            result.push_back(0xDC00 | (ucs4 & 0x3ff));
        } else {
            return {};
        }
    }
    result.push_back(0);
    return result;
}

template <typename Iter>
std::string utf16ToUTF8(Iter start, Iter end) {
    std::string result;
    while (start != end) {
        uint32_t ucs4 = 0;
        if (*start < 0xD800 || *start > 0xDFFF) {
            ucs4 = *start;
            start = std::next(start);
        } else if (0xD800 <= *start && *start <= 0xDBFF) {
            if (std::next(start) == end) {
                return {};
            }
            auto cur = *start;
            auto next = std::next(start);
            if (0xDC00 <= *next && *next <= 0xDFFF) {
                /* We have a valid surrogate pair.  */
                ucs4 = (((cur & 0x3FF) << 10) | (*next & 0x3FF)) + (1 << 16);
            } else {
                return {};
            }
            start = std::next(next);
        } else if (0xDC00 <= *start && *start <= 0xDFFF) {
            return {};
        }
        result.append(utf8::UCS4ToUTF8(ucs4));
    }
    return result;
}

static std::string get_current_context_text(km_kbp_context *context) {
    std::string result;
    UniqueCPtr<km_kbp_context_item, km_kbp_context_items_dispose>
        context_items_ptr;
    km_kbp_status status = KM_KBP_STATUS_OK;
    {
        km_kbp_context_item *context_items = nullptr;
        status = km_kbp_context_get(context, &context_items);
        context_items_ptr.reset(context_items);
    }
    if (status == KM_KBP_STATUS_OK) {
        size_t buf_size = 0;
        km_kbp_context_items_to_utf8(context_items_ptr.get(), nullptr,
                                     &buf_size);
        if (buf_size) {
            std::vector<char> buf;
            buf.resize(buf_size + 1);
            km_kbp_context_items_to_utf8(context_items_ptr.get(), buf.data(),
                                         &buf_size);
            return {buf.data()};
        }
    }
    return "";
}

class KeymanState : public InputContextProperty {
public:
    KeymanState(KeymanKeyboardData *keyboard, InputContext *ic)
        : keyboard_(keyboard), ic_(ic) {
        std::vector<km_kbp_option_item> keyboard_opts;

        keyboard_opts.emplace_back();
        keyboard_opts.back().scope = KM_KBP_OPT_ENVIRONMENT;
        const auto platform = utf8ToUTF16("platform");
        keyboard_opts.back().key = platform.data();
        const auto platformValue = utf8ToUTF16("linux desktop hardware native");
        keyboard_opts.back().value = platformValue.data();

        keyboard_opts.emplace_back();
        keyboard_opts.back().scope = KM_KBP_OPT_ENVIRONMENT;
        const auto baseLayout = utf8ToUTF16("baseLayout");
        keyboard_opts.back().key = baseLayout.data();
        const auto baseLayoutValue = utf8ToUTF16("kbdus.dll");
        keyboard_opts.back().value = baseLayoutValue.data();

        keyboard_opts.emplace_back();
        keyboard_opts.back().scope = KM_KBP_OPT_ENVIRONMENT;
        const auto baseLayoutAlt = utf8ToUTF16("baseLayoutAlt");
        keyboard_opts.back().key = baseLayoutAlt.data();
        const auto baseLayoutAltValue = utf8ToUTF16("en-US");
        keyboard_opts.back().value = baseLayoutAltValue.data();

        keyboard_opts.emplace_back();
        keyboard_opts.back().scope = 0;
        keyboard_opts.back().key = nullptr;
        keyboard_opts.back().value = nullptr;
        km_kbp_status status_state = km_kbp_state_create(
            keyboard_->kbpKeyboard(), keyboard_opts.data(), &state);
        if (status_state != KM_KBP_STATUS_OK) {
            FCITX_KEYMAN_ERROR() << "problem creating km_kbp_state for "
                                 << keyboard_->metadata().id;
            return;
        };
        resetContext();
    }

    void resetContext() {
        auto context = km_kbp_state_context(state);
        if (ic_->capabilityFlags().test(CapabilityFlag::SurroundingText)) {
            auto current_context_utf8 = get_current_context_text(context);

            auto text = ic_->surroundingText().text();
            auto context_pos = std::min(ic_->surroundingText().anchor(),
                                        ic_->surroundingText().cursor());
            auto context_start = context_pos > MAXCONTEXT_ITEMS
                                     ? context_pos - MAXCONTEXT_ITEMS
                                     : 0;

            auto startIter = utf8::nextNChar(text.begin(), context_start);
            auto endIter =
                utf8::nextNChar(startIter, context_pos - context_start);
            std::string new_context(startIter, endIter);
            if (!stringutils::endsWith(new_context, current_context_utf8)) {
                FCITX_KEYMAN_DEBUG()
                    << "setting context because it has changed from expected";
                km_kbp_context_item *context_items = nullptr;
                if (km_kbp_context_items_from_utf8(new_context.data(),
                                                   &context_items) ==
                    KM_KBP_STATUS_OK) {
                    km_kbp_context_set(context, context_items);
                }
                km_kbp_context_items_dispose(context_items);
            }
        } else {
            km_kbp_context_clear(context);
        }
    }

    void reset() {
        lctrl_pressed = false;
        rctrl_pressed = false;
        lalt_pressed = false;
        ralt_pressed = false;
        char_buffer.clear();
        emitting_keystroke = false;
    }

    auto *keyboard() { return keyboard_; }

    km_kbp_state *state = nullptr;
    bool lctrl_pressed = false;
    bool rctrl_pressed = false;
    bool lalt_pressed = false;
    bool ralt_pressed = false;
    InputBuffer char_buffer{InputBufferOption::FixedCursor};
    bool emitting_keystroke = false;

private:
    KeymanKeyboardData *keyboard_;
    InputContext *ic_;
};

KeymanEngine::KeymanEngine(Instance *instance) : instance_(instance) {}

std::vector<InputMethodEntry> KeymanEngine::listInputMethods() {
    // Locate all directory under $XDG_DATA/keyman
    std::set<std::string> keymapDirs;
    StandardPath::global().scanFiles(
        StandardPath::Type::Data, "keyman",
        [&keymapDirs](const std::string &path, const std::string &dir, bool) {
            if (fs::isdir(stringutils::joinPath(dir, path))) {
                keymapDirs.insert(path);
            }
            return true;
        });
    FCITX_INFO() << keymapDirs;
    std::unordered_map<std::string, std::unique_ptr<KeymanKeyboard>> keyboards;
    for (const auto &keymapDir : keymapDirs) {
        auto kmpJsonFiles = StandardPath::global().openAll(
            StandardPath::Type::Data,
            stringutils::joinPath("keyman", keymapDir, "kmp.json"), O_RDONLY);
        for (const auto &kmpJsonFile : kmpJsonFiles) {
            try {
                KmpMetadata metadata(kmpJsonFile.fd());
                for (const auto &[id, keyboard] : metadata.keyboards()) {
                    if (auto iter = keyboards.find(id);
                        iter != keyboards.end() &&
                        iter->second->version < keyboard.version) {
                        continue;
                    }
                    keyboards[id] = std::make_unique<KeymanKeyboard>(
                        instance_, keyboard, metadata,
                        fs::dirName(kmpJsonFile.path()));
                }
            } catch (...) {
            }
        }
    }
    std::vector<InputMethodEntry> result;
    for (auto &[id, keyboard] : keyboards) {
        result.emplace_back(stringutils::concat("keyman:", id),
                            stringutils::concat(keyboard->name, " (Keyman)"),
                            keyboard->language, "keyman");
        result.back()
            .setIcon("km-config")
            .setConfigurable(true)
            .setUserData(std::move(keyboard));
    }
    return result;
}

FCITX_ADDON_FACTORY(fcitx::KeymanEngineFactory);

} // namespace fcitx

void fcitx::KeymanKeyboardData::load() {
    if (loaded_) {
        return;
    }
    loaded_ = true;
    auto kmxPath = stringutils::joinPath(
        metadata_.baseDir, stringutils::concat(metadata_.id, ".kmx"));
    auto ldmlFile = stringutils::joinPath(
        metadata_.baseDir, stringutils::concat(metadata_.id, ".ldml"));
    if (!fs::isreg(ldmlFile)) {
        ldmlFile.clear();
    }
    ldmlFile_ = ldmlFile;
    if (!fs::isreg(kmxPath)) {
        FCITX_KEYMAN_ERROR() << "Failed to find kmx file. " << metadata_.id;
        return;
    }

    km_kbp_status status_keyboard =
        km_kbp_keyboard_load(kmxPath.data(), &keyboard_);

    if (status_keyboard != KM_KBP_STATUS_OK) {
        FCITX_KEYMAN_ERROR()
            << "problem creating km_kbp_keyboard" << metadata_.id;
        return;
    }

    instance_->inputContextManager().registerProperty(
        stringutils::concat("keymanState", metadata_.id), &factory_);

    config_ = RawConfig();
    readAsIni(config_, stringutils::concat("keyman/", metadata_.id, ".conf"));

    FCITX_KEYMAN_DEBUG() << config_;
}

void fcitx::KeymanKeyboardData::setOption(const km_kbp_cp *key,
                                          const km_kbp_cp *value) {
    auto keyEnd = key;
    while (*keyEnd) {
        ++keyEnd;
    }
    auto valueEnd = value;
    while (*valueEnd) {
        ++valueEnd;
    }

    auto utf8Key = utf16ToUTF8(key, keyEnd);
    auto utf8Value = utf16ToUTF8(value, valueEnd);

    if (!utf8Key.empty()) {
        config_.setValueByPath(utf8Key, utf8Value);
        safeSaveAsIni(config_,
                      stringutils::concat("keyman/", metadata_.id, ".conf"));
    }
}

fcitx::KeymanKeyboardData::KeymanKeyboardData(
    Instance *instance, const fcitx::KeymanKeyboard &metadata)
    : instance_(instance), metadata_(metadata),
      factory_(
          [this](InputContext &ic) { return new KeymanState(this, &ic); }) {}

fcitx::KeymanKeyboardData::~KeymanKeyboardData() { factory_.unregister(); }

void fcitx::KeymanEngine::activate(const fcitx::InputMethodEntry &entry,
                                   fcitx::InputContextEvent &) {
    auto data = static_cast<const KeymanKeyboard *>(entry.userData());
    data->load();
}

static bool ok_for_single_backspace(const km_kbp_action_item *action_items,
                                    size_t i, size_t num_actions) {
    for (size_t j = i + 1; j < num_actions; j++) {
        if (action_items[j].type == KM_KBP_IT_BACK ||
            action_items[j].type == KM_KBP_IT_CHAR ||
            action_items[j].type == KM_KBP_IT_EMIT_KEYSTROKE) {
            return false;
        }
    }
    return true;
}

void fcitx::KeymanEngine::keyEvent(const fcitx::InputMethodEntry &entry,
                                   fcitx::KeyEvent &keyEvent) {
    auto ic = keyEvent.inputContext();
    auto keyman = state(entry, *ic);
    if (!keyman) {
        return;
    }
    auto keycode = keyEvent.key().code() - 8;
    auto state = keyEvent.rawKey().states();
    if (keyEvent.isRelease()) {
        switch (keycode) {
        case KEYMAN_LCTRL:
            keyman->lctrl_pressed = false;
            break;
        case KEYMAN_RCTRL:
            keyman->rctrl_pressed = false;
            break;
        case KEYMAN_LALT:
            keyman->lalt_pressed = false;
            break;
        case KEYMAN_RALT:
            keyman->ralt_pressed = false;
            break;
        default:
            break;
        }
        return;
    }

    if (keycode < 0 || keycode > 255) {
        return;
    }

    if (keycode_to_vk[keycode] == 0) // key we don't handle
    {
        // save if a possible Ctrl or Alt modifier
        switch (keycode) {
        case KEYMAN_LCTRL:
            keyman->lctrl_pressed = true;
            break;
        case KEYMAN_RCTRL:
            keyman->rctrl_pressed = true;
            break;
        case KEYMAN_LALT:
            keyman->lalt_pressed = true;
            break;
        case KEYMAN_RALT:
            keyman->ralt_pressed = true;
            break;
        default:
            break;
        }
        return;
    }

    // keyman modifiers are different from X11
    uint16_t km_mod_state = 0;
    if (state.test(KeyState::Shift)) {
        km_mod_state |= KM_KBP_MODIFIER_SHIFT;
    }
    if (state.test(KeyState::Mod5)) {
        km_mod_state |= KM_KBP_MODIFIER_RALT;
        FCITX_KEYMAN_DEBUG() << "modstate KM_KBP_MODIFIER_RALT from Mod5";
    }
    if (state.test(KeyState::Mod1)) {
        if (keyman->ralt_pressed) {
            km_mod_state |= KM_KBP_MODIFIER_RALT;
            FCITX_KEYMAN_DEBUG()
                << "modstate KM_KBP_MODIFIER_RALT from ralt_pressed";
        }
        if (keyman->lalt_pressed) {
            km_mod_state |= KM_KBP_MODIFIER_LALT;
            FCITX_KEYMAN_DEBUG()
                << "modstate KM_KBP_MODIFIER_LALT from lalt_pressed";
        }
    }
    if (state.test(KeyState::Ctrl)) {
        if (keyman->rctrl_pressed) {
            km_mod_state |= KM_KBP_MODIFIER_RCTRL;
            FCITX_KEYMAN_DEBUG()
                << "modstate KM_KBP_MODIFIER_RCTRL from rctrl_pressed";
        }
        if (keyman->lctrl_pressed) {
            km_mod_state |= KM_KBP_MODIFIER_LCTRL;
            FCITX_KEYMAN_DEBUG()
                << "modstate KM_KBP_MODIFIER_LCTRL from lctrl_pressed";
        }
    }
    auto context = km_kbp_state_context(keyman->state);
    FCITX_KEYMAN_DEBUG() << ("before process key event")
                         << get_current_context_text(context);
    FCITX_KEYMAN_DEBUG() << "km_mod_state=" << km_mod_state;
    km_kbp_process_event(keyman->state, keycode_to_vk[keycode], km_mod_state);

    FCITX_KEYMAN_DEBUG() << ("after process key event")
                         << get_current_context_text(context);

    // km_kbp_state_action_items to get action items
    size_t num_action_items;
    const km_kbp_action_item *action_items =
        km_kbp_state_action_items(keyman->state, &num_action_items);

    std::vector<uint16_t> utf16Buffer;
    auto flushUtf16Buffer = [&utf16Buffer, keyman]() {
        if (utf16Buffer.empty()) {
            return;
        }
        std::string utf8 = utf16ToUTF8(utf16Buffer.begin(), utf16Buffer.end());
        keyman->char_buffer.type(utf8);
        utf16Buffer.clear();
    };

    for (size_t i = 0; i < num_action_items; i++) {
        if (action_items[i].type == KM_KBP_IT_CHAR) {
            FCITX_KEYMAN_DEBUG()
                << "CHAR action " << i + 1 << "/" << num_action_items;
            utf16Buffer.push_back(action_items[i].character);
            continue;
        } else {
            flushUtf16Buffer();
        }

        switch (action_items[i].type) {
        case KM_KBP_IT_MARKER:
            FCITX_KEYMAN_DEBUG()
                << "MARKER action " << i + 1 << num_action_items;
            break;
        case KM_KBP_IT_ALERT:
            FCITX_KEYMAN_DEBUG()
                << "ALERT action " << i + 1 << num_action_items;
            break;
        case KM_KBP_IT_BACK:
            FCITX_KEYMAN_DEBUG() << "BACK action " << i + 1 << num_action_items;
            if (!keyman->char_buffer.empty()) {
                keyman->char_buffer.backspace();
            } else if (ok_for_single_backspace(action_items, i,
                                               num_action_items)) {
                FCITX_KEYMAN_DEBUG() << "no char actions, just single back";
                return;
            } else {

                if (ic->capabilityFlags().test(
                        CapabilityFlag::SurroundingText)) {
                    ic->deleteSurroundingText(-1, 1);
                    FCITX_KEYMAN_DEBUG() << "deleting surrounding text 1 char";
                } else {
                    FCITX_KEYMAN_DEBUG()
                        << "forwarding backspace with reset context";
                    km_kbp_context_item *context_items;
                    km_kbp_context_get(km_kbp_state_context(keyman->state),
                                       &context_items);
                    keyman->resetContext();
                    ic->forwardKey(Key(FcitxKey_BackSpace));
                    km_kbp_context_set(km_kbp_state_context(keyman->state),
                                       context_items);
                    km_kbp_context_items_dispose(context_items);
                }
            }
            break;
        case KM_KBP_IT_PERSIST_OPT:
            FCITX_KEYMAN_DEBUG()
                << "PERSIST_OPT action " << i + 1 << "/" << num_action_items;
            // Save keyboard option
            if (action_items[i].option != NULL) {
                // Allocate for 1 option plus 1 pad struct of 0's
                std::array<km_kbp_option_item, 2> keyboard_opts;
                memmove(&(keyboard_opts[0]), action_items[i].option,
                        sizeof(km_kbp_option_item));
                memset(&keyboard_opts[1], 0, sizeof(km_kbp_option_item));
                // Propagate to all state.
                instance_->inputContextManager().foreach([this, &entry,
                                                          &keyboard_opts](
                                                             InputContext *ic) {
                    if (auto keyman = this->state(entry, *ic)) {
                        auto event_status = km_kbp_state_options_update(
                            keyman->state, keyboard_opts.data());
                        if (event_status != KM_KBP_STATUS_OK) {
                            FCITX_KEYMAN_DEBUG()
                                << "problem saving option for km_kbp_keyboard";
                        }
                    }
                    return true;
                });

                // Put the keyboard option into config
                if (action_items[i].option != NULL &&
                    action_items[i].option->key != NULL &&
                    action_items[i].option->value != NULL) {
                    FCITX_KEYMAN_DEBUG() << "Saving keyboard option to Config";
                    // Load the current keyboard options from DConf
                    keyman->keyboard()->setOption(
                        action_items[i].option->key,
                        action_items[i].option->value);
                }
            }
            break;
        case KM_KBP_IT_EMIT_KEYSTROKE:
            if (!keyman->char_buffer.empty()) {
                ic->commitString(keyman->char_buffer.userInput());
                keyman->char_buffer.clear();
            }
            FCITX_KEYMAN_DEBUG()
                << "EMIT_KEYSTROKE action " << i + 1 << num_action_items;
            keyman->emitting_keystroke = true;
            break;
        case KM_KBP_IT_INVALIDATE_CONTEXT:
            FCITX_KEYMAN_DEBUG()
                << "INVALIDATE_CONTEXT action " << i + 1 << num_action_items;
            km_kbp_context_clear(km_kbp_state_context(keyman->state));
            keyman->resetContext();
            break;
        case KM_KBP_IT_END:
            FCITX_KEYMAN_DEBUG() << "END action " << i + 1 << num_action_items;
            if (!keyman->char_buffer.empty()) {
                ic->commitString(keyman->char_buffer.userInput());
                keyman->char_buffer.clear();
            }
            if (keyman->emitting_keystroke) {
                keyman->emitting_keystroke = false;
                return;
            }
            break;
        default:
            FCITX_KEYMAN_DEBUG()
                << "Unknown action " << i + 1 << num_action_items;
        }
    }
    flushUtf16Buffer();
    context = km_kbp_state_context(keyman->state);
    FCITX_KEYMAN_DEBUG() << "after processing all actions";
    keyEvent.filterAndAccept();
}

void fcitx::KeymanEngine::reset(const fcitx::InputMethodEntry &entry,
                                fcitx::InputContextEvent &event) {
    auto ic = event.inputContext();
    auto keyman = state(entry, *ic);
    if (!keyman) {
        return;
    }
    keyman->resetContext();
    keyman->reset();
}

fcitx::KeymanState *
fcitx::KeymanEngine::state(const fcitx::InputMethodEntry &entry,
                           fcitx::InputContext &ic) {

    auto userData = static_cast<const KeymanKeyboard *>(entry.userData());
    auto &data = userData->data();
    // Check if data is ready.
    if (!data.kbpKeyboard() || !data.factory().registered()) {
        return nullptr;
    }
    auto keyman = ic.propertyFor(&data.factory());
    if (!keyman->state) {
        return nullptr;
    }
    return keyman;
}

std::string fcitx::KeymanEngine::subMode(const fcitx::InputMethodEntry &entry,
                                         fcitx::InputContext &ic) {
    auto keyman = state(entry, ic);
    if (!keyman) {
        return _("Not available");
    }
    return "";
}
