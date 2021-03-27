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
#ifndef _FCITX5_KMPMETADATA_H_
#define _FCITX5_KMPMETADATA_H_

#include <string>
#include <unordered_map>
#include <vector>
#include <fcitx-utils/log.h>

namespace fcitx {

struct KmpKeyboardMetadata {
    std::string id;
    std::string name;
    std::string version;

    // The order matters here, because fcitx support only one language code.
    std::vector<std::pair<std::string, std::string>> languages;
};

inline LogMessageBuilder &operator<<(LogMessageBuilder &builder,
                                     const KmpKeyboardMetadata &keyboard) {
    builder << "KmpKeyboardMetadata(id=" << keyboard.id
            << ",name=" << keyboard.name << ",version=" << keyboard.version
            << ",languages=" << keyboard.languages << ")";
    return builder;
}

// Lots of property is not used within Fcitx, but we just try to save them all
// anyway.
class KmpMetadata {
public:
    KmpMetadata(int fd);

    const auto &keyboards() const { return keyboards_; }

    const auto &readmeFile() const { return readmeFile_; }
    const auto &graphicFile() const { return graphicFile_; }

private:
    // System
    std::string keymanDeveloperVersion_;
    std::string fileVersion_;
    // info
    std::string name_;
    std::string version_;
    std::string copyright_;
    std::string author_;
    std::string website_;
    // Option
    std::string readmeFile_;
    std::string graphicFile_;
    // Files
    // file name to description;
    std::unordered_map<std::string, std::string> files_;
    std::unordered_map<std::string, KmpKeyboardMetadata> keyboards_;
};

} // namespace fcitx

#endif // _FCITX5_KMPMETADATA_H_
