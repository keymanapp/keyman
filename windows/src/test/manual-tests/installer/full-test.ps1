# full-test.ps1

& .\build-releases.bat

if($LASTEXITCODE -ne 0) {
  Exit $LASTEXITCODE
}

.\vm-test.ps1
