if builder_is_debug_build; then
  VS_MSBUILD_FLAG_DEBUG="//p:Configuration=Debug"
else
  VS_MSBUILD_FLAG_DEBUG="//p:Configuration=Release"
fi
