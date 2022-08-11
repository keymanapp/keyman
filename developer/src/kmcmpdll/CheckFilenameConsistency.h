
#include <windows.h>
#include <Compfile.h>

// _S2 commented out functionality because I have no idea how to change to use with char16_t on non-windows platforms
KMX_DWORD CheckFilenameConsistencyForCalls(PFILE_KEYBOARD fk);

// _S2 changed Version to call CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile)
KMX_DWORD CheckFilenameConsistency(KMX_CHAR const * Filename, BOOL ReportMissingFile);

//  _S2 we want to use char16_t in kmcompx so this version using wchar_t should not be needed 
//KMX_DWORD CheckFilenameConsistency(KMX_WCHART const * Filename, BOOL ReportMissingFile);

// _S2 this version uses char16_t* ( but is not able to use _wfinddata_t,_wfindfirst,_wsplitpath_s, _wmakepath_s )
KMX_DWORD CheckFilenameConsistency(KMX_WCHAR const * Filename, bool ReportMissingFile) ;

// _S2 added to use with char* but not able to use _wsplitpath_s, _wmakepath_s for adding warning of CHINT_FilenameHasDifferingCase
KMX_DWORD CheckFilenameConsistencyCHAR(KMX_CHAR const * Filename, bool ReportMissingFile) ;             

bool FileExists(KMX_CHAR const * filename);
bool IsRelativePath(KMX_CHAR const * p);
bool IsRelativePath(KMX_WCHART const * p);

// _S2 added to use in CheckFilenameConsistencyCHAR
bool IsRelativePath(KMX_WCHAR const * p) ;
