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
#include "kmpmetadata.h"
#include <optional>
#include <stdexcept>
#include <fcitx-utils/misc.h>
#include <fcitx-utils/stringutils.h>
#include <json-c/json.h>

namespace fcitx {

std::string readStringValue(json_object *object, const char *field,
                            std::string_view defaultValue = "") {
    if (auto subObject = json_object_object_get(object, field);
        subObject && json_object_get_type(subObject) == json_type_string) {
        return json_object_get_string(subObject);
    }
    return std::string{defaultValue};
}

std::string readDescriptionValue(json_object *object, const char *field,
                                 std::string_view defaultValue = "") {

    if (auto subObject = json_object_object_get(object, field);
        subObject && json_object_get_type(subObject) == json_type_object) {
        return readStringValue(subObject, "description", defaultValue);
    }
    return std::string{defaultValue};
}

KmpMetadata::KmpMetadata(int fd) {
    UniqueCPtr<json_object, json_object_put> obj(json_object_from_fd(fd));
    if (!obj) {
        throw std::runtime_error("Failed to parse kmp.json");
    }

    if (auto kmpSystem = json_object_object_get(obj.get(), "system");
        kmpSystem && json_object_get_type(kmpSystem) == json_type_object) {
        keymanDeveloperVersion_ =
            readStringValue(kmpSystem, "keymanDeveloperVersion");
        fileVersion_ = readStringValue(kmpSystem, "fileVersion");
    }

    if (auto kmpInfo = json_object_object_get(obj.get(), "info");
        kmpInfo && json_object_get_type(kmpInfo) == json_type_object) {
        name_ = readDescriptionValue(kmpInfo, "name");
        version_ = readDescriptionValue(kmpInfo, "version");
        copyright_ = readDescriptionValue(kmpInfo, "copyright");
        author_ = readDescriptionValue(kmpInfo, "author");
        website_ = readDescriptionValue(kmpInfo, "website");
    }

    if (auto kmpFiles = json_object_object_get(obj.get(), "files");
        kmpFiles && json_object_get_type(kmpFiles) == json_type_array) {
        for (size_t i = 0, e = json_object_array_length(kmpFiles); i < e; i++) {
            auto file = json_object_array_get_idx(kmpFiles, i);
            auto name = readStringValue(file, "name");
            auto description = readStringValue(file, "description");
            if (!name.empty()) {
                files_[name] = description;
            }
        }
    }

    if (auto kmpOptions = json_object_object_get(obj.get(), "options");
        kmpOptions && json_object_get_type(kmpOptions) == json_type_object) {
        readmeFile_ = readStringValue(kmpOptions, "readmeFile");
        graphicFile_ = readStringValue(kmpOptions, "graphicFile");
        if (!files_.count(readmeFile_)) {
            readmeFile_ = std::string();
        }
        if (!files_.count(graphicFile_)) {
            graphicFile_ = std::string();
        }
    }

    if (auto kmpKeyboards = json_object_object_get(obj.get(), "keyboards");
        kmpKeyboards && json_object_get_type(kmpKeyboards) == json_type_array) {
        for (size_t i = 0, e = json_object_array_length(kmpKeyboards); i < e;
             i++) {
            auto file = json_object_array_get_idx(kmpKeyboards, i);
            KmpKeyboardMetadata keyboard;
            keyboard.id = readStringValue(file, "id");
            if (keyboard.id.empty()) {
                continue;
            }
            keyboard.name = readStringValue(file, "name");
            if (keyboard.name.empty()) {
                keyboard.name = keyboard.id;
            }
            keyboard.version = readStringValue(file, "version");
            if (auto kmpLanguages = json_object_object_get(file, "languages");
                kmpLanguages &&
                json_object_get_type(kmpLanguages) == json_type_array) {
                for (size_t i = 0, e = json_object_array_length(kmpLanguages);
                     i < e; i++) {
                    auto language = json_object_array_get_idx(kmpLanguages, i);
                    auto languageName = readStringValue(language, "name");
                    auto languageId = readStringValue(language, "id");
                    if (languageId.empty()) {
                        continue;
                    }
                    keyboard.languages.emplace_back(languageId, languageName);
                }
            }

            auto kmxFile = stringutils::concat(keyboard.id, ".kmx");
            if (!files_.count(kmxFile)) {
                continue;
            }
            keyboards_[keyboard.id] = std::move(keyboard);
        }
    }
}

} // namespace fcitx
