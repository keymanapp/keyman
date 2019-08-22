@rem This script is a rough duplicate of /developer/inst/kmlmc.cmd. It is stored here
@rem in order to allow TIKE to call out to the compiler while debugging.
@"%~dp0..\..\..\..\developer\inst\dist\node.exe" "%~dp0..\..\..\..\developer\js\dist\cli.js" %*
