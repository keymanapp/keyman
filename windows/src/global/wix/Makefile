!include ..\..\defines.mak

build:
  rem nada - no longer using this, we will delete later

xbuild:
  cd wixui
  $(WIXCANDLE) BrowseDlg.wxs CancelDlg.wxs Common.wxs CustomizeDlg.wxs DiskCostDlg.wxs \
     ErrorDlg.wxs ErrorProgressText.wxs FatalError.wxs \
     FilesInUse.wxs LicenseAgreementDlg.wxs MaintenanceTypeDlg.wxs MaintenanceWelcomeDlg.wxs \
     MsiRMFilesInUse.wxs OutOfDiskDlg.wxs OutOfRbDiskDlg.wxs PrepareDlg.wxs ProgressDlg.wxs \
     ResumeDlg.wxs SetupTypeDlg.wxs UserExit.wxs VerifyReadyDlg.wxs WaitForCostingDlg.wxs \
     WelcomeDlg.wxs featuretree\WixUI_FeatureTree.wxs InstallDirDlg.wxs installdir\WixUI_InstallDir.wxs \
     WelcomeEulaDlg.wxs ..\wixui_keyman.wxs

  $(WIXLIT) -out ..\wixui_keyman.wixlib wixui_keyman.wixobj BrowseDlg.wixobj CancelDlg.wixobj Common.wixobj CustomizeDlg.wixobj DiskCostDlg.wixobj \
     ErrorDlg.wixobj ErrorProgressText.wixobj FatalError.wixobj \
     FilesInUse.wixobj LicenseAgreementDlg.wixobj MaintenanceTypeDlg.wixobj MaintenanceWelcomeDlg.wixobj \
     MsiRMFilesInUse.wixobj OutOfDiskDlg.wixobj OutOfRbDiskDlg.wixobj PrepareDlg.wixobj ProgressDlg.wixobj \
     ResumeDlg.wixobj SetupTypeDlg.wixobj UserExit.wixobj VerifyReadyDlg.wixobj WaitForCostingDlg.wixobj \
     WelcomeDlg.wixobj WixUI_FeatureTree.wixobj InstallDirDlg.wixobj WixUI_InstallDir.wixobj \
     WelcomeEulaDlg.wixobj 

clean:
  del *.wixlib
  cd wixui
  del *.wixobj
