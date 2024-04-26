!include ..\Defines.mak

##
## In this file, $Version, $VersionWin, and $VersionRelease will be replaced by mkver. These are not
## Make variables, but mkver variables.
##

# In dev environments, we'll hack the tier to alpha; CI sets this for us in real builds.
!ifndef TIER
TIER=alpha
!endif

# We use different directories so that heat generates
# different identifiers for the various folders
KEYMAN_WIX_TEMP_BASE=$(TEMP)\keyman_wix_build
KEYMAN_WIX_TEMP_XML=$(TEMP)\keyman_wix_build\xml
KEYMAN_WIX_TEMP_CEF=$(TEMP)\keyman_wix_build\cef
KEYMAN_WIX_TEMP_TEMPLATES=$(TEMP)\keyman_wix_build\templates
KEYMAN_WIX_TEMP_KMC=$(TEMP)\keyman_wix_build\kmc
KEYMAN_WIX_TEMP_SERVER=$(TEMP)\keyman_wix_build\Server

KEYMAN_WIX_KMDEV_SERVER=$(DEVELOPER_ROOT)\bin\server
KEYMAN_DEVELOPER_TEMPLATES_ROOT=$(DEVELOPER_ROOT)\src\kmconvert\data

copykmdev: makeinstaller make-kmc-install-zip
    -mkdir $(DEVELOPER_ROOT)\release\$Version
    copy /Y $(DEVELOPER_ROOT)\src\inst\keymandeveloper.msi $(DEVELOPER_ROOT)\release\$Version\keymandeveloper.msi
    copy /Y $(DEVELOPER_ROOT)\src\inst\keymandeveloper-$Version.exe $(DEVELOPER_ROOT)\release\$Version\keymandeveloper-$Version.exe
    $(SIGCHECK) $(DEVELOPER_ROOT)\release\$Version\* > sig1
    $(VERIFY_SIGNATURES) < sig1
    -del sig1

test-releaseexists:
    if exist $(DEVELOPER_ROOT)\release\$Version\keymandeveloper*.msi echo. & echo Release $Version already exists. Delete it or update VERSION.md and try again & exit 1

candle: heat-cef heat-xml heat-templates heat-server heat-kmc
    $(WIXCANDLE) -dVERSION=$VersionWin -dRELEASE=$VersionRelease kmdev.wxs
    $(WIXCANDLE) -dVERSION=$VersionWin -dRELEASE=$VersionRelease -dXmlSourceDir=$(DEVELOPER_ROOT)\src\tike\xml xml.wxs
    $(WIXCANDLE) -dVERSION=$VersionWin -dRELEASE=$VersionRelease -dCefSourceDir=$(KEYMAN_CEF4DELPHI_ROOT) cef.wxs
    $(WIXCANDLE) -dVERSION=$VersionWin -dRELEASE=$VersionRelease -dTemplatesSourceDir=$(KEYMAN_DEVELOPER_TEMPLATES_ROOT) templates.wxs
    $(WIXCANDLE) -dVERSION=$VersionWin -dRELEASE=$VersionRelease -dkmcSourceDir=$(KEYMAN_WIX_TEMP_KMC) kmc.wxs
    $(WIXCANDLE) -dVERSION=$VersionWin -dRELEASE=$VersionRelease -dServerSourceDir=$(KEYMAN_WIX_KMDEV_SERVER) server.wxs

clean-heat: clean-heat-kmc

heat-xml:
# We copy the files to a temp folder in order to exclude thumbs.db, .vs, etc from harvesting
# We also copy over the OSK files from Keyman Engine (#11199)
    -rmdir /s/q $(KEYMAN_WIX_TEMP_XML)
    mkdir $(KEYMAN_WIX_TEMP_XML)
    xcopy $(KEYMAN_ROOT)\windows\src\engine\xml\osk $(DEVELOPER_ROOT)\src\tike\xml\osk\ /s /i
    xcopy $(DEVELOPER_ROOT)\src\tike\xml\* $(KEYMAN_WIX_TEMP_XML)\ /s

    -del /f /s /q $(KEYMAN_WIX_TEMP_XML)\Thumbs.db
    -rmdir /s/q $(KEYMAN_WIX_TEMP_XML)\app\node_modules
    -for /f %i in ('dir /a:d /s /b $(KEYMAN_WIX_TEMP_XML)\.vs') do rd /s /q %i
    $(WIXHEAT) dir $(KEYMAN_WIX_TEMP_XML) -o xml.wxs -ag -cg XML -dr INSTALLDIR -var var.XmlSourceDir -wx -nologo
# When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
    -rmdir /s/q $(KEYMAN_WIX_TEMP_XML)

heat-templates:
# We copy the files to a temp folder in order to exclude .git and README.md from harvesting
    -rmdir /s/q $(KEYMAN_WIX_TEMP_TEMPLATES)
    mkdir $(KEYMAN_WIX_TEMP_TEMPLATES)
    xcopy $(KEYMAN_DEVELOPER_TEMPLATES_ROOT)\* $(KEYMAN_WIX_TEMP_TEMPLATES)\ /s
    $(WIXHEAT) dir $(KEYMAN_WIX_TEMP_TEMPLATES) -o templates.wxs -ag -cg Templates -dr dirProjects -var var.TemplatesSourceDir -wx -nologo
# When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
    -rmdir /s/q $(KEYMAN_WIX_TEMP_TEMPLATES)

heat-cef:
# We copy the files to a temp folder in order to exclude .git and README.md from harvesting
    -rmdir /s/q $(KEYMAN_WIX_TEMP_CEF)
    mkdir $(KEYMAN_WIX_TEMP_CEF)
    xcopy $(KEYMAN_CEF4DELPHI_ROOT)\* $(KEYMAN_WIX_TEMP_CEF)\ /s
    $(WIXHEAT) dir $(KEYMAN_WIX_TEMP_CEF) -o cef.wxs -ag -cg CEF -dr INSTALLDIR -var var.CefSourceDir -wx -nologo
# When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
    -rmdir /s/q $(KEYMAN_WIX_TEMP_CEF)

heat-server:
# We copy the files to a temp folder in order to exclude .git and README.md from harvesting
    -rmdir /s/q $(KEYMAN_WIX_TEMP_SERVER)
    mkdir $(KEYMAN_WIX_TEMP_SERVER)
    xcopy $(KEYMAN_WIX_KMDEV_SERVER)\* $(KEYMAN_WIX_TEMP_SERVER)\ /s
    $(WIXHEAT) dir $(KEYMAN_WIX_TEMP_SERVER) -o server.wxs -ag -cg Server -dr INSTALLDIR -var var.ServerSourceDir -wx -nologo
# When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
    -rmdir /s/q $(KEYMAN_WIX_TEMP_SERVER)

heat-kmc:
    cd $(DEVELOPER_ROOT)\src\kmc
# Build the distributable package
    mkdir $(KEYMAN_WIX_TEMP_KMC)
    $(GIT_BASH_FOR_KEYMAN) build.sh bundle --build-path "$(KEYMAN_WIX_TEMP_KMC)"

# Build the .wxs file
    cd $(DEVELOPER_ROOT)\src\inst
    $(WIXHEAT) dir $(KEYMAN_WIX_TEMP_KMC) -o kmc.wxs -ag -cg kmc -dr INSTALLDIR -var var.kmcSourceDir -wx -nologo

clean-heat-kmc:
# the production build generates files that are not in source, e.g. .ps1 scripts
# When we candle/light build, we can grab the source files from the proper root so go ahead and delete the temp folder again
    -rmdir /s/q $(KEYMAN_WIX_TEMP_KMC)

makeinstaller:
    cd $(DEVELOPER_ROOT)\src\inst
    echo [Setup] > setup.inf
    echo Version=$Version >> setup.inf
    echo MSIFileName=keymandeveloper.msi >> setup.inf
    echo Title=Keyman Developer $VersionWithTag >>setup.inf
    $(WZZIP) setup.zip keymandeveloper.msi setup.inf
    copy /b $(DEVELOPER_PROGRAM)\setup.exe + setup.zip keymandeveloper-$Version.exe
    $(SIGNCODE) /d "Keyman Developer" keymandeveloper-$Version.exe

#
# Zip the files we distribute as part of the standalone kmc distro into release\$Version\kmcomp-$Version.zip
#

# TODO: rename this to keyman-developer-cli-$Version.zip
KMC_ZIP=$(DEVELOPER_ROOT)\release\$Version\kmcomp-$Version.zip

make-kmc-install-zip: copy-schemas
    cd $(DEVELOPER_ROOT)\bin

    $(WZZIP) -bd -bb0 $(KMC_ZIP) \
        kmconvert.exe \
        sentry.dll sentry.x64.dll \
        kmdecomp.exe \
        keyboard_info.schema.json \
        kmp.schema.json \
        keyman-touch-layout.spec.json keyman-touch-layout.clean.spec.json \
        xml\layoutbuilder\*.keyman-touch-layout \
        projects\* \
        server\*

# TODO: are these required?
# kpj.schema.json kvks.schema.json \
# ldml-keyboard3.schema.json ldml-keyboardtest3.schema.json \

copy-schemas:
    copy $(KEYMAN_ROOT)\common\schemas\keyboard_info\keyboard_info.schema.json $(DEVELOPER_ROOT)\bin
    copy $(KEYMAN_ROOT)\common\schemas\keyman-touch-layout\keyman-touch-layout.spec.json $(DEVELOPER_ROOT)\bin
    copy $(KEYMAN_ROOT)\common\schemas\keyman-touch-layout\keyman-touch-layout.clean.spec.json $(DEVELOPER_ROOT)\bin
    copy $(KEYMAN_ROOT)\common\schemas\displaymap\displaymap.schema.json $(DEVELOPER_ROOT)\bin
    copy $(KEYMAN_ROOT)\common\schemas\kmp\kmp.schema.json $(DEVELOPER_ROOT)\bin

