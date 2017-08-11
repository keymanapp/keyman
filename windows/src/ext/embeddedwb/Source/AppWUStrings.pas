//*************************************************************
//                     Application web Updater strings        *
//                                                            *
//                     For Delphi 5 to XE                     *
//                         Freeware unit                      *
//                            by                              *
//                     Eran Bodankin (bsalsa)                 *
//                     bsalsa@gmail.com                       *
//                                                            *
//     Documentation and updated versions:                    *
//               http://www.bsalsa.com                        *
//*************************************************************
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: AppWUStrings.pas,v 1.2 2009/01/26 15:01:38 bsalsa Exp $

unit AppWUStrings;

interface

resourcestring

  sAbortMessage = 'Aborted! (User request).';
  sAbortOption = ' or press "Abort".';
  sAddingFilesToList = 'Adding files into the update list: ';
  sAddingRestartFiles = 'Adding files which require restart into the update list: ';
  sAlredayExists = ' already exists in ';
  sAuthor = 'bsalsa:  ';
  sBatchCommands = 'Batch File Commands: ';
  sBusy = 'Update procedure is running. Please wait';
  sCaption = 'Checking for updates... Please wait.';
  sChecking = 'Checking for Updates';
  sCheckingFolder = 'Checking if folder ';
  sCleaning = 'Cleaning unnecessary files. ';
  sCompany = 'Company:  ';
  sConfirm = 'Press "OK" to update ';
  sCopiedTo = ' --> has been copied to: ';
  sCreateFolder = ' trying to create the folder!';
  sCreateProcess = 'Creating a process to run the batch file.';
  sCreateSubBackup = ' trying to create the sub backup folder!';
  sCreating = 'Creating ';
  sCreatingBatch = 'Creating a batch file: ';
  sCurrentVersion = 'Application current version is: ';
  sDefinition = ' Definition';
  sDelete = ' trying to delete the file !';
  sDest = 'Destination: ';
  sDownloadFiles = ' downloading the updates!';
  sDownloadInfo = ' downloading the update Info file!';
  sDownloading = 'Downloading ';
  sEmail = 'Email:  ';
  sErrorCanceled = 'Updates checking was canceled (An error has been found).';
  sErrorCopyFileName = 'Error copy file name: ';
  sErrorCreating = 'Error Creating ';
  sErrorCurrentVersionDefinition = 'error has been found in the current version definition';
  sErrorMessage = 'An error occurred while ';
  sErrorNewVersionDefinition = 'error has been found in the new version definition';
  sErrorRunningCmd = 'An error occurred while trying to run the cmd file. ';
  sExists = ' exists. --> OK.';
  sExplore = 'explore';
  sFile = 'The File ';
  sFileCopyError = ' trying to copy the file';
  sFileDeleted = 'File deleted: ';
  sFileList = 'The files that will be updated are: ';
  sFileName = 'File name: ';
  sFileNotExist = 'File doesn'' t exist: ';
  sFinshedProcessing = 'Finished processing file names.';
  sFolder = ' folder.';
  sFolderDeleted = 'Folder deleted: ';
  sFolderOK = ' folder. --> OK.';
  sInitialing = 'Initializing the updater.';
  sInTo = ' to ';
  sLocateCurrentVersion = ' trying to locate the current version number!';
  sMailingReport = 'Mailing Error Report.... Hold On.';
  sMatchingDetails = 'Matching details: ';
  smFileNotExist = ' trying to locate the source folder!';
  sNewUpdateVersion = ' Update version is: ';
  sNoAvailableUpdates = 'There are no new available updates. ';
  sOpenFolder = 'Open folder : ';
  sOverWriting = ' (OverWriting.)';
  sProgress = 'Progress: ';
  sRestarting = 'Restarting.... Hold On.';
  sRestartMessage1 = 'Some of the updates require a restart. ';
  sRestartMessage2 = 'Press "Yes" to restart now, or press "Abort" to run the ' + 'updates checking later.';
  sResultMatch = 'Result: MATCH.';
  sResultNoMatch = 'Result: No match found!';
  sSection = ' section';
  sSecurityAlert = 'Security Alert!! ';
  sSiteError = 'trying to match the application ' + 'details with the remote web site details!';
  sStartDownloadingXml = 'Start downloading xml remote file.';
  sStartProcessing = 'Start processing file names. ';
  sStatus = 'Status: ';
  sStopped = 'Stopped (User Request).';
  sSuccess = ' was successful.';
  sSuccessMessageText = 'Update is done.';
  sTo = '  ---> to: ';
  sTrying = 'Trying to download: ';
  sTryingToLocate = 'trying to locate the web info file.';
  sUnableCreateErrorLog = 'Unable to create error log file: ';
  sUpdateAvailable = 'A new update is available for: ';
  sUpdateBy = 'An update is available by: ';
  sUpdateChanges = 'Update Changes: ';
  sUpdateNotNeeded = 'Your application is up to date.';
  sUpdaterErrorReport = 'Updater Error Report';
  sUpdaterRunning = 'Updater is running... ';
  sUpdateVersion = ' trying to locate the update version number!';
  sUpdatingControls = 'Updating Application Controls';
  sXmlAppName = 'ApplicationName';
  sXmlData = ' (XML Data Section)';
  sXMLError = ' trying to parse the XML file!';
  sXmlErrorParsingData = ('Error parsing XML data');
  sXMLFileError = 'An error has been found in the update XML file in the ';
  sXmlHead = '(XML Head Section)';
  sXMLInformation = 'XML Information: ';
  sXmlParsingData = 'Parsing XML data';
  sXmlParsingHead = 'Parsing XML Head section';
  sXmlParsingTag = 'Parsing XML tag: ';
  sXmlUpdatesSection = 'Updates Section';

   //Do not translate the forward strings unless you do so also
   // in the remote site XML file and you know what you are doing.
   //(Read the AppWebUpdater code!)
  sDestination = 'Destination';
  sNo = 'no';
  sTerminate = 'Terminate';
  sXmlAuthor = 'Author';
  sXmlChangeLog = 'ChangeLog';
  sXmlCompany = 'Company';
  sXmlDestination = 'Destination';
  sXmlDetails = 'Details';
  sXmlFile = 'File';
  sXmlInfo = 'Info';
  sXmlInstructions = 'Instructions';
  sXmlName = 'Name';
  sXmlText = 'Text';
  sXmlUpdates = 'Updates';
  sXmlVersion = 'Version';
  sYes = 'yes';
  sXmlMajorVer = 'MajorVersion';
  sXmlMinorVer= 'MinorVersion';
  sXmlReleaseVer= 'ReleaseVersion';
  sXmlBuildVer= 'BuildVersion';

implementation

end.

