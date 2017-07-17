//*************************************************************
//                     IEDownload strings                     *
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

unit IEDownloadStrings;

interface

resourcestring

  Bind_To_St = 'MonikerBindToStorage: ';
  Create_BSCB = 'Creating IBindStatusCallback: ';
  Create_Moniker = 'CreateMoniker: ';
  CreateABindCtx = 'CreateAsyncBindCtx: ';
  CreateURLMEx = 'CreateURLMonikerEx: ';
  DL_DIR = 'Downloads\';
  done = 'Done.';
  DL_ToFile = 'UrlDownloadToFile';
  DL_ToCacheFile = 'UrlDownloadToCacheFile';
  Err_AsyncBindCtx = 'Error while CreateAsyncBindCtx: ';
  Err_BindToSt = 'Error while MonikerBindToStorage ';
  Err_CreateMoniker = 'Error while CreateMoniker ';
  Err_Creating_Dir = ' while creating download folder';
  Err_Folder = 'Error while opening folder';
  Err_Proc_DL = 'ProcessEvents fails to wait until complete downloading';
  Err_Proc_Ev = 'ProcessEvents fails to wait until complete.';
  Err_RegBSCB = 'Error while RegisterBindStatusCallback: ';
  Err_Revoke = 'Error while RevokeBindStatusCallback. ';
  Err_ToFile = 'Error while UrlDownloadToFile';
  Err_ToCacheFile = 'Error while UrlDownloadToCacheFile';
  Err_TimeOut = 'Error: Timeout.';
  Err_URLMEx = 'Error while CreateURLMonikerEx: ';
  Frmt_Time = 'mm:ss.zzz';
  kb_sec = 'kb/sec';
  Reg_BSCB = 'RegisterBindStatusCallback: ';
  Registering_new_moniker = 'Registering new moniker: ';
  Revoke_BSCB = 'RevokeBindStatusCallback: ';
  Err_Doc_AsAdvise = 'Error while getting ConnectionPoint.Advise: ';
  Doc_AsAdvise = 'Getting ConnectionPoint.Advise ';
  Err_Doc_AsPointContainer = 'Error while getting IConnectionPointContainer: ';
  Doc_AsPointContainer = 'Getting IConnectionPointContainer: ';
  Err_Doc_AsAmbientPropertyChange = 'Error while getting AmbientPropertyChang: ';
  Doc_AsAmbientPropertyChange = 'Getting Ambient Property Change: ';
  Err_Doc_AsSetClientSite = 'Error while getting Set Client Site: ';
  Doc_AsSetClientSite = 'Getting Set Client Site: ';
  Err_CoCreateInstance = 'Error while CoCreateInstance: ';
  Succ_CoCreateInstance = 'Getting CoCreateInstance: ';
  Err_IpersistMoniker_Load = 'Error while loading as IpersistMoniker: ';
  Succ_IpersistMoniker_Load = 'Loading as IpersistMoniker ';
  Err_Load_Str = 'Error while loading from string: ';
  Succ_Load_Str = 'Loading from string: ';
  Err_Load_Mon = 'Error while loading from moniker: ';
  Succ_Load_Mon = 'Loading from moniker: ';
  P_Wait = 'Please wait... ';
  General_Error = 'An error occured while ';
  Doc_Error = 'Bad document or bad address.';

implementation

end.
