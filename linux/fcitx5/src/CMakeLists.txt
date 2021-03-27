set(KEYMAN_SOURCES
    engine.cpp
    kmpmetadata.cpp
)
add_library(keyman MODULE ${KEYMAN_SOURCES})
target_link_libraries(keyman Fcitx5::Core Fcitx5::Config PkgConfig::Keyman PkgConfig::JsonC)
set_target_properties(keyman PROPERTIES PREFIX "")
install(TARGETS keyman DESTINATION "${CMAKE_INSTALL_LIBDIR}/fcitx5")
configure_file(keyman.conf.in.in keyman.conf.in)
fcitx5_translate_desktop_file("${CMAKE_CURRENT_BINARY_DIR}/keyman.conf.in" keyman.conf)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/keyman.conf" DESTINATION "${CMAKE_INSTALL_DATADIR}/fcitx5/addon")


