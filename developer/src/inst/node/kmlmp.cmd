@rem This script avoids path dependencies for node for distribution
@rem with Keyman Developer. When used on platforms other than Windows,
@rem node can be used directly with the compiler (`npm link` will setup).
@"%~dp0\node.js\node.exe" "%~dp0\kmc\kmlmp.mjs" %*
