# Automated testing of Keyman installation on a Hyper-V VM

## Virtual Machine Configuration

1. Create a Windows 10 x64 VM
2. Use the same username/password as your host machine (so psexec doesn't need credentials)
3. Configure the machine for automatic login (https://technet.microsoft.com/en-us/library/ee872306.aspx)
4. Add a world-writeable share (e.g. Temp = C:\temp)
5. Install the test root certificates from /common/windows/delphi/tools/certificates into the root store on the VM
6. Login and take a snapshot, give it a name like "Keyman Install Start Point"
7. Edit vm-test.ps1 and set the configuration parameters accordingly.

## Host Configuration

1. Copy installers for older versions of Keyman Desktop that you want to start with into
   the /windows/release folder (subdirectories not necessary)
2. Make sure you have psexec.exe (v2.2 or later) on path or edit vm-test.ps1 accordingly

## Preparing test releases

1. Edit vm-test.ps1 to set the test protocols you wish to run (each protocol is a base install followed by a series of upgrades).
2. Edit build-releases.bat to build the specific version numbers you need to build for the test.
3. Run build-releases from a standard Keyman build command prompt.

## Running the tests

1. From an elevated Powershell prompt, run vm-test.ps1
2. Results are stored in /windows/src/test/manual-tests/installer/results
