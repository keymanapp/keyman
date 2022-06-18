#
# E2E and Unit Tests for Keyman Developer projects
#

# ----------------------------------------------------------------------

TARGETS=developer-tests

test:
    $(MAKE) "TARGET=test" $(TARGETS)

!include ..\..\Header.mak

# ----------------------------------------------------------------------

developer-tests: \
  compile-supplementary-support \
  keyboard-info \
  keyboard-package-versions \
  kmcomp \
  kmcomp-empty-store-names \
  kmcomp-x64-structures \
  kmconvert \
  lexical-model-compiler \
  model-ts-parser

# ----------------------------------------------------------------------

compile-supplementary-support: .virtual
  cd $(DEVELOPER_ROOT)\src\test\auto\compile-supplementary-support
  $(MAKE) $(TARGET)

keyboard-info: .virtual
  cd $(DEVELOPER_ROOT)\src\test\auto\keyboard-info
  $(MAKE) $(TARGET)

keyboard-package-versions: .virtual
  cd $(DEVELOPER_ROOT)\src\test\auto\keyboard-package-versions
  $(MAKE) $(TARGET)

kmcomp: .virtual
  cd $(DEVELOPER_ROOT)\src\test\auto\kmcomp
  $(MAKE) $(TARGET)

kmcomp-empty-store-names: .virtual
  rem TODO
#  cd $(DEVELOPER_ROOT)\src\test\auto\kmcomp-empty-store-names
#  $(MAKE) $(TARGET)

kmcomp-x64-structures: .virtual
  cd $(DEVELOPER_ROOT)\src\test\auto\kmcomp-x64-structures
  $(MAKE) $(TARGET)

kmconvert: .virtual
  cd $(DEVELOPER_ROOT)\src\test\auto\kmconvert
  $(MAKE) $(TARGET)

lexical-model-compiler: .virtual
# TODO: move this to a test target in kmlmc
  cd $(DEVELOPER_ROOT)\src\kmlmc
  $(GIT_BASH_FOR_KEYMAN) build.sh -test

model-ts-parser: .virtual
  rem TODO
#  cd $(DEVELOPER_ROOT)\src\test\auto\model-ts-parser
#  $(MAKE) $(TARGET)

# ----------------------------------------------------------------------

!include ..\..\Target.mak

# ----------------------------------------------------------------------
# EOF
# ----------------------------------------------------------------------
