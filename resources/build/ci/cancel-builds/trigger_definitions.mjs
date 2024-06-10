// Maps to trigger-definitions.inc.sh and must be kept in sync
export const testBuildConfigurations = {
  'android': ['KeymanAndroid_TestPullRequests', 'KeymanAndroid_TestSamplesAndTestProjects'],
  'ios': ['Keyman_iOS_TestPullRequests', 'Keyman_iOS_TestSamplesAndTestProjects'],
  'linux': ['KeymanLinux_TestPullRequests', 'Keyman_Linux_Test_Integration', 'Keyman_Common_KPAPI_TestPullRequests_Linux', 'deb-pr-packaging_GitHub'],
  'mac': ['Keyman_KeymanMac_PullRequests', 'Keyman_Common_KPAPI_TestPullRequests_macOS'],
  'windows': ['KeymanDesktop_TestPullRequests', 'KeymanDesktop_TestPrRenderOnScreenKeyboards', 'Keyman_Common_KPAPI_TestPullRequests_Windows'],
  'web': ['Keymanweb_TestPullRequests', 'Keyman_Common_LMLayer_TestPullRequests', 'Keyman_Common_KPAPI_TestPullRequests_WASM'],
  'developer': ['Keyman_Developer_Test', 'Keyman_Test_Common_Web', 'Keyman_Test_Common_Windows', 'Keyman_Test_Common_Mac', 'Keyman_Test_Common_Linux'],

  'common_web': ['Keyman_Test_Common_Web'],
  'common_windows': ['Keyman_Test_Common_Windows'],
  'common_mac': ['Keyman_Test_Common_Mac'],
  'common_linux': ['Keyman_Test_Common_Linux'],
};