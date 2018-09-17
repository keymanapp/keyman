###########################################################################
# Configuration parameters
###########################################################################

# Project to test (e.g. keymandesktop, keymandeveloper

$InstallerName="keymandesktop"

# Virtual machine parameters

$VMIP="172.23.14.244"  # after restoring from checkpoint it can take time for hostnames to be found; using IP address is simpler
$VMName="PELION-WIN10"
$Checkpoint="Keyman Install Start Point"

$VMTempPath = "\\${VMIP}\Temp"
$VMLocalTempPath = "c:\temp"

# Local paths

$psexec="psexec.exe"

# Versions to test

$protocols = @(
  @("11.0.1500.0"),   # 0
  @("11.0.1501.0"),   # 1
  @("11.0.1502.0"),   # 2
  @("22.0.900.0"),    # 3
  @("11.0.1500.0", "11.0.1501.0"),   # 4
  @("11.0.1500.0", "11.0.1502.0"),   # 5
  @("11.0.1500.0", "22.0.900.0"),    # 6
  @("11.0.1500.0", "11.0.1501.0", "11.0.1502.0"),   # 7
  @("11.0.1500.0", "11.0.1501.0", "11.0.1502.0", "22.0.900.0"),   # 8
  @("10.0.1201.0", "11.0.1500.0"),   # 9
  @("10.0.1201.0", "11.0.1500.0", "11.0.1501.0"),   # 10
  @("9.0.528.0", "11.0.1500.0"),     # 11
  @("9.0.528.0", "11.0.1500.0", "11.0.1501.0"),     # 12
  @("9.0.528.0", "10.0.1201.0", "11.0.1500.0", "11.0.1501.0", "22.0.900.0")    # 13
)
<#
Note: the 8.0.361.0 installer is giving a runtime error on scripted start. May be 
      incorrect parameters or something else. For now will focus on upgrades from
      9.0
  @("8.0.361.0", "11.0.1500.0"),
  @("8.0.361.0", "11.0.1500.0", "11.0.1501.0"),
  @("8.0.361.0", "9.0.528.0", "11.0.1500.0"),
  @("8.0.361.0", "9.0.528.0", "11.0.1500.0", "11.0.1501.0"),
  @("8.0.361.0", "10.0.1201.0", "11.0.1500.0"),
  @("8.0.361.0", "10.0.1201.0", "11.0.1500.0", "11.0.1501.0"),
  @("8.0.361.0", "9.0.528.0", "10.0.1201.0", "11.0.1500.0"),
  @("8.0.361.0", "9.0.528.0", "10.0.1201.0", "11.0.1500.0", "11.0.1501.0"),
  @("8.0.361.0", "9.0.528.0", "10.0.1201.0", "11.0.1500.0", "11.0.1501.0", "22.0.900.0")
)
#>

###########################################################################
# End configuration parameters
###########################################################################

$InstallerRoot="${env:KEYMAN_ROOT}\windows\release"
$KMValidationSourcePath="${PSScriptRoot}\km-validation.bat"
$ResultsPath="${PSScriptRoot}\results"

# 

$ScriptFilename="run.bat"
$VMLocalTempScriptFilename="${VMLocalTempPath}\${ScriptFilename}"
$VMTempScriptFilename="${VMTempPath}\${ScriptFilename}"
$ValidationFilename="validation.txt"
$VMValidationPath="${VMTempPath}\${ValidationFilename}"
$VMLocalValidationPath="${VMLocalTempPath}\${ValidationFilename}"

if(!(Test-Path $ResultsPath)) {
  mkdir $ResultsPath
}

#$protocols.Length
for ($i = 0; $i -lt $protocols.Length; $i++) {
  $protocol = $protocols[$i]
  
  Restore-VMSnapshot -Name $Checkpoint -VMName $VMName -Confirm:$false

  # Wait for VM to become available
  
  Write-Host "# Waiting for VM to become available"
  
  while(!(Test-Path $VMTempPath)) {
    Sleep 1
  }
  
  Write-Host "# VM is available, starting test protocol #${i}"
  Write-Host "Capturing baseline"

  # Run a baseline test -- no files installed

  Copy-Item -Path $KMValidationSourcePath -Destination "${VMTempPath}\km-validation.bat"
  
  if(Test-Path $VMTempScriptFilename) {
    Remove-Item $VMTempScriptFilename
  }
  
  if(Test-Path $VMValidationPath) {
    Remove-Item $VMValidationPath
  }
  
  "@echo (VM) Validating baseline" | Add-Content -Path $VMTempScriptFilename
  "@call ${VMLocalTempPath}\km-validation.bat > ${VMLocalValidationPath}" | Add-Content -Path $VMTempScriptFilename
  "@echo ..." | Add-Content -Path $VMTempScriptFilename
  "@exit /b 0" | Add-Content -Path $VMTempScriptFilename

  & $psexec -accepteula -nobanner -i \\$VMIP $VMLocalTempScriptFilename
  if($LASTEXITCODE -ne 0) {
    Exit $LASTEXITCODE
  }

  # Without a sleep, sometimes things get grumpy
  
  while(!(Test-Path $VMValidationPath)) {
    Sleep 1
  }
  Copy-Item -Path $VMValidationPath -Destination "${ResultsPath}\${i}-baseline.txt"


  # Run the protocol

  for ($j = 0; $j -lt $protocol.Length; $j++) {
    # Find the source installer
    
    $Version = $protocol[$j]
    $InstallerFilename="${InstallerName}-$Version.exe"
    Write-Host "Installing ${InstallerFilename}"
    
    $SourcePath="${InstallerRoot}\${InstallerFilename}"
    if(!(Test-Path $SourcePath)) {
      $SourcePath="${InstallerRoot}\${Version}\${InstallerFilename}"
      if(!(Test-Path $SourcePath)) {
        Write-Host "${SourcePath} does not exist. Aborting"
        Exit 1
      }
    }
    
    # Build the test script
    
    $DestPath = "${VMTempPath}\${InstallerFilename}"
    
    if(Test-Path $VMTempScriptFilename) {
      Remove-Item $VMTempScriptFilename
    }
    
    if(Test-Path $VMValidationPath) {
      Remove-Item $VMValidationPath
    }
    
    "@echo (VM) Starting ${InstallerFilename}" | Add-Content -Path $VMTempScriptFilename
    "@start /wait ${VMLocalTempPath}\${InstallerFilename} -s -r -o -s" | Add-Content -Path $VMTempScriptFilename
    "@if errorlevel 1 exit /b %errorlevel%" | Add-Content -Path $VMTempScriptFilename
    "@echo ..." | Add-Content -Path $VMTempScriptFilename
    "@echo (VM) Installer finished, validating result" | Add-Content -Path $VMTempScriptFilename
    "@call ${VMLocalTempPath}\km-validation.bat > ${VMLocalValidationPath}" | Add-Content -Path $VMTempScriptFilename
    "@echo ..." | Add-Content -Path $VMTempScriptFilename
    "@exit /b 0" | Add-Content -Path $VMTempScriptFilename
    
    Copy-Item -Path $SourcePath -Destination $DestPath
    Copy-Item -Path $KMValidationSourcePath -Destination "${VMTempPath}\km-validation.bat"
    
    # Run the test script
    
    & $psexec -accepteula -nobanner -i \\$VMIP $VMLocalTempScriptFilename
    if($LASTEXITCODE -ne 0) {
      Exit $LASTEXITCODE
    }
    
    # Without a sleep, sometimes things get grumpy
    
    while(!(Test-Path $VMValidationPath)) {
      Sleep 1
    }
    Copy-Item -Path $VMValidationPath -Destination "${ResultsPath}\${i}-${j}.txt"
  }
}
