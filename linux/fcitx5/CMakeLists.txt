cmake_minimum_required(VERSION 3.4.0)

project(fcitx5-keyman VERSION 1.0.0)

include(FeatureSummary)
include(GNUInstallDirs)

find_package(Gettext REQUIRED)
find_package(Fcitx5Core 5.0.6 REQUIRED)
find_package(PkgConfig REQUIRED)
pkg_check_modules(Keyman REQUIRED IMPORTED_TARGET "keyman_kmn_processor")
pkg_check_modules(JsonC REQUIRED IMPORTED_TARGET "json-c")

add_definitions(-DFCITX_GETTEXT_DOMAIN=\"fcitx5-keyman\")
fcitx5_add_i18n_definition()

include("${FCITX_INSTALL_CMAKECONFIG_DIR}/Fcitx5Utils/Fcitx5CompilerSettings.cmake")

add_subdirectory(po)
add_subdirectory(src)

fcitx5_translate_desktop_file(org.fcitx.Fcitx5.Addon.Keyman.metainfo.xml.in
                              org.fcitx.Fcitx5.Addon.Keyman.metainfo.xml XML)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/org.fcitx.Fcitx5.Addon.Keyman.metainfo.xml" DESTINATION ${CMAKE_INSTALL_DATADIR}/metainfo)

feature_summary(WHAT ALL FATAL_ON_MISSING_REQUIRED_PACKAGES)

