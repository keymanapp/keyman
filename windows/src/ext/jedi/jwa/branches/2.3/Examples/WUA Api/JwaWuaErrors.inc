{******************************************************************}
{                                                                  }
{       Windows Update Api                                         }
{       Windows Update Api Error & Result & Success Codes          }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) Microsoft Corporation.  All rights reserved.       }
{                                                                  }
{ Portions created by stOrM! and Christian Wimmer                  }
{ Copyright (C) 2008 stOrM! and Christian Wimmer.                  }
{                                                                  }
{ Contributor(s): stOrM! and Christian Wimmer.                     }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

const

//  The following table shows WUA success codes.

//Result String                             Result Code   Description

  WU_S_SERVICE_STOP	                        = $240001;	  // Windows Update Agent was stopped successfully.
  WU_S_SELFUPDATE                           = $240002;	  // Windows Update Agent updated itself.
  WU_S_UPDATE_ERROR                         = $240003;	  // Operation completed successfully but there were errors applying the updates..
  WU_S_MARKED_FOR_DISCONNECT	              = $240004;	  // A callback was marked to be disconnected later because the request to disconnect the operation came while a callback was executing.
  WU_S_REBOOT_REQUIRED	                    = $240005;	  // The system must be restarted to complete installation of the update.
  WU_S_ALREADY_INSTALLED	                  = $240006;	  // The update to be installed is already installed on the system.
  WU_S_ALREADY_UNINSTALLED              	  = $240007;	  // The update to be removed is not installed on the system.
  WU_S_ALREADY_DOWNLOADED               	  = $240008;	  // The update to be downloaded has already been downloaded.


//  The following table shows WUA error codes.

//Result String                             Result Code     Description

  WU_E_NO_SERVICE	                          = $80240001;	// Windows Update Agent was unable to provide the service.
  WU_E_MAX_CAPACITY_REACHED                 = $80240002;	// The maximum capacity of the service was exceeded.
  WU_E_UNKNOWN_ID	                          = $80240003;	// An ID cannot be found.
  WU_E_NOT_INITIALIZED                      = $80240004;	// The object could not be initialized.
  WU_E_RANGEOVERLAP                         = $80240005;	// The update handler requested a byte range overlapping a previously requested range.
  WU_E_TOOMANYRANGES                        = $80240006;	// The requested number of byte ranges exceeds the maximum number (2^31 - 1).
  WU_E_INVALIDINDEX                         = $80240007;	// The index to a collection was invalid.
  WU_E_ITEMNOTFOUND                         = $80240008;	// The key for the item queried could not be found.
  WU_E_OPERATIONINPROGRESS                  = $80240009;	// Another conflicting operation was in progress. Some operations such as installation cannot be performed twice simultaneously.
  WU_E_COULDNOTCANCEL                       = $8024000A;	// Cancellation of the operation was not allowed.
  WU_E_CALL_CANCELLED                       = $8024000B;	// Operation was cancelled.
  WU_E_NOOP                                 = $8024000C;	// No operation was required.
  WU_E_XML_MISSINGDATA                      = $8024000D;	// Windows Update Agent could not find required information in the update's XML data.
  WU_E_XML_INVALID                          = $8024000E;	// Windows Update Agent found invalid information in the update's XML data.
  WU_E_CYCLE_DETECTED                       = $8024000F;	// Circular update relationships were detected in the metadata.
  WU_E_TOO_DEEP_RELATION	                  = $80240010;	// Update relationships too deep to evaluate were evaluated.
  WU_E_INVALID_RELATIONSHIP                 = $80240011;	// An invalid update relationship was detected.
  WU_E_REG_VALUE_INVALID	                  = $80240012;	// An invalid registry value was read.
  WU_E_DUPLICATE_ITEM	                      = $80240013;	// Operation tried to add a duplicate item to a list.
  WU_E_INSTALL_NOT_ALLOWED	                = $80240016;	// Operation tried to install while another installation was in progress or the system was pending a mandatory restart.
  WU_E_NOT_APPLICABLE	                      = $80240017;	// Operation was not performed because there are no applicable updates.
  WU_E_NO_USERTOKEN	                        = $80240018;	// Operation failed because a required user token is missing.
  WU_E_EXCLUSIVE_INSTALL_CONFLICT           = $80240019;	// An exclusive update cannot be installed with other updates at the same time.
  WU_E_POLICY_NOT_SET	                      = $8024001A;	// A policy value was not set.
  WU_E_SELFUPDATE_IN_PROGRESS	              = $8024001B;	// The operation could not be performed because the Windows Update Agent is self-updating.
  WU_E_INVALID_UPDATE	                      = $8024001D;	// An update contains invalid metadata.
  WU_E_SERVICE_STOP	                        = $8024001E;	// Operation did not complete because the service or system was being shut down.
  WU_E_NO_CONNECTION	                      = $8024001F;	// Operation did not complete because the network connection was unavailable.
  WU_E_NO_INTERACTIVE_USER	                = $80240020;	// Operation did not complete because there is no logged-on interactive user.
  WU_E_TIME_OUT	                            = $80240021;	// Operation did not complete because it timed out.
  WU_E_ALL_UPDATES_FAILED                   = $80240022;	// Operation failed for all the updates.
  WU_E_EULAS_DECLINED	                      = $80240023;	// The license terms for all updates were declined.
  WU_E_NO_UPDATE	                          = $80240024;	// There are no updates.
  WU_E_USER_ACCESS_DISABLED       	        = $80240025;	// Group Policy settings prevented access to Windows Update.
  WU_E_INVALID_UPDATE_TYPE	                = $80240026;	// The type of update is invalid.
  WU_E_URL_TOO_LONG	                        = $80240027;	// The URL exceeded the maximum length.
  WU_E_UNINSTALL_NOT_ALLOWED	              = $80240028;	// The update could not be uninstalled because the request did not originate from a WSUS server.
  WU_E_INVALID_PRODUCT_LICENSE	            = $80240029;	// Search may have missed some updates before there is an unlicensed application on the system.
  WU_E_MISSING_HANDLER	                    = $8024002A;	// A component required to detect applicable updates was missing.
  WU_E_LEGACYSERVER	                        = $8024002B;	// An operation did not complete because it requires a newer version of server.
  WU_E_BIN_SOURCE_ABSENT                    = $8024002C;	// A delta-compressed update could not be installed because it required the source.
  WU_E_SOURCE_ABSENT	                      = $8024002D;	// A full-file update could not be installed because it required the source.
  WU_E_WU_DISABLED	                        = $8024002E;	// Access to an unmanaged server is not allowed.
  WU_E_CALL_CANCELLED_BY_POLICY	            = $8024002F;	// Operation did not complete because the DisableWindowsUpdateAccess policy was set.
  WU_E_INVALID_PROXY_SERVER	                = $80240030;	// The format of the proxy list was invalid.
  WU_E_INVALID_FILE	                        = $80240031;	// The file is in the wrong format.
  WU_E_INVALID_CRITERIA	                    = $80240032;	// The search criteria string was invalid.
  WU_E_EULA_UNAVAILABLE	                    = $80240033;	// License terms could not be downloaded.
  WU_E_DOWNLOAD_FAILED	                    = $80240034;	// Update failed to download.
  WU_E_UPDATE_NOT_PROCESSED	                = $80240035;	// The update was not processed.
  WU_E_INVALID_OPERATION	                  = $80240036;	// The object's current state did not allow the operation.
  WU_E_NOT_SUPPORTED	                      = $80240037;	// The functionality for the operation is not supported.
  WU_E_WINHTTP_INVALID_FILE	                = $80240038;	// The downloaded file has an unexpected content type.
  WU_E_TOO_MANY_RESYNC	                    = $80240039;	// Agent is asked by server to resync too many times.
  WU_E_NO_SERVER_CORE_SUPPORT	              = $80240040;	// WUA API method does not run on Server Core installation.
  WU_E_SYSPREP_IN_PROGRESS	                = $80240041;	// Service is not available while sysprep is running.
  WU_E_UNKNOWN_SERVICE	                    = $80240042;	// The update service is no longer registered with AU.
  WU_E_UNEXPECTED	                          = $80240FFF;	// An operation failed due to reasons not covered by another error code.
  WU_E_MSI_WRONG_VERSION	                  = $80241001;	// Search may have missed some updates because the Windows Installer is less than version 3.1.
  WU_E_MSI_NOT_CONFIGURED	                  = $80241002;	// Search may have missed some updates because the Windows Installer is not configured.
  WU_E_MSP_DISABLED	                        = $80241003;	// Search may have missed some updates because policy has disabled Windows Installer patching.
  WU_E_MSI_WRONG_APP_CONTEXT	              = $80241004;	// An update could not be applied because the application is installed per-user.
  WU_E_MSP_UNEXPECTED	                      = $80241FFF;	// Search may have missed some updates because there was a failure of the Windows Installer.
  WU_E_UH_REMOTEUNAVAILABLE	                = $80242000;	// A request for a remote update handler could not be completed because no remote process is available.
  WU_E_UH_LOCALONLY	                        = $80242001;	// A request for a remote update handler could not be completed because the handler is local only.
  WU_E_UH_UNKNOWNHANDLER	                  = $80242002;	// A request for an update handler could not be completed because the handler could not be recognized.
  WU_E_UH_REMOTEALREADYACTIVE	              = $80242003;	// A remote update handler could not be created because one already exists.
  WU_E_UH_DOESNOTSUPPORTACTION	            = $80242004;	// A request for the handler to install (uninstall) an update could not be completed because the update does not support install (uninstall).
  WU_E_UH_WRONGHANDLER	                    = $80242005;	// An operation did not complete because the wrong handler was specified.
  WU_E_UH_INVALIDMETADATA	                  = $80242006;	// A handler operation could not be completed because the update contains invalid metadata.
  WU_E_UH_INSTALLERHUNG	                    = $80242007;	// An operation could not be completed because the installer exceeded the time limit.
  WU_E_UH_OPERATIONCANCELLED	              = $80242008;	// An operation being done by the update handler was cancelled.
  WU_E_UH_BADHANDLERXML	                    = $80242009;	// An operation could not be completed because the handler-specific metadata is invalid.
  WU_E_UH_CANREQUIREINPUT	                  = $8024200A;	// A request to the handler to install an update could not be completed because the update requires user input.
  WU_E_UH_INSTALLERFAILURE	                = $8024200B;	// The installer failed to install (uninstall) one or more updates.
  WU_E_UH_FALLBACKTOSELFCONTAINED	          = $8024200C;	// The update handler should download self-contained content rather than delta-compressed content for the update.
  WU_E_UH_NEEDANOTHERDOWNLOAD	              = $8024200D;	// The update handler did not install the update because it needs to be downloaded again.
  WU_E_UH_NOTIFYFAILURE	                    = $8024200E;	// The update handler failed to send notification of the status of the install (uninstall) operation.
  WU_E_UH_INCONSISTENT_FILE_NAMES	          = $8024200F;	// The file names contained in the update metadata and in the update package are inconsistent.
  WU_E_UH_FALLBACKERROR	                    = $80242010;	// The update handler failed to fall back to the self-contained content.
  WU_E_UH_TOOMANYDOWNLOADREQUESTS	          = $80242011;	// The update handler has exceeded the maximum number of download requests.
  WU_E_UH_UNEXPECTEDCBSRESPONSE	            = $80242012;	// The update handler has received an unexpected response from CBS.
  WU_E_UH_BADCBSPACKAGEID	                  = $80242013;	// The update metadata contains an invalid CBS package identifier.
  WU_E_UH_POSTREBOOTSTILLPENDING	          = $80242014;	// he post-reboot operation for the update is still in progress.
  WU_E_UH_POSTREBOOTRESULTUNKNOWN	          = $80242015;	// The result of the post-reboot operation for the update could not be determined.
  WU_E_UH_POSTREBOOTUNEXPECTEDSTATE	        = $80242016;	// The state of the update after its post-reboot operation has completed is unexpected.
  WU_E_UH_NEW_SERVICING_STACK_REQUIRED	    = $80242017;	// The operating system servicing stack must be updated before this update is downloaded or installed.
  WU_E_UH_UNEXPECTED	                      = $80242FFF;	// An update handler error not covered by another WU_E_UH_* code.
  WU_E_INSTALLATION_RESULTS_UNKNOWN_VERSION	= $80243001;	// The results of download and installation could not be read from the registry due to an unrecognized data format version.
  WU_E_INSTALLATION_RESULTS_INVALID_DATA    = $80243002;	// The results of download and installation could not be read from the registry due to an invalid data format.
  WU_E_INSTALLATION_RESULTS_NOT_FOUND     	= $80243003;	// The results of download and installation are not available; the operation may have failed to start.
  WU_E_TRAYICON_FAILURE	                    = $80243004;	// A failure occurred when trying to create an icon in the taskbar notification area.
  WU_E_NON_UI_MODE	                        = $80243FFD;	// Unable to show UI when in non-UI mode; WU client UI modules may not be installed.
  WU_E_WUCLTUI_UNSUPPORTED_VERSION        	= $80243FFE;	// Unsupported version of WU client UI exported functions.
  WU_E_AUCLIENT_UNEXPECTED                  = $80243FFF;	// There was a user interface error not covered by another WU_E_AUCLIENT_* error code.
  WU_E_PT_SOAPCLIENT_BASE	                  = $80244000;	// WU_E_PT_SOAPCLIENT_* error codes map to the SOAPCLIENT_ERROR enum of the ATL Server Library.
  WU_E_PT_SOAPCLIENT_INITIALIZE           	= $80244001;	// SOAPCLIENT_INITIALIZE_ERROR - initialization of the SOAP client failed, possibly because of an MSXML installation failure.
  WU_E_PT_SOAPCLIENT_OUTOFMEMORY	          = $80244002;	// SOAPCLIENT_OUTOFMEMORY - SOAP client failed because it ran out of memory.
  WU_E_PT_SOAPCLIENT_GENERATE	              = $80244003;	// SOAPCLIENT_GENERATE_ERROR - SOAP client failed to generate the request.
  WU_E_PT_SOAPCLIENT_CONNECT	              = $80244004;	// SOAPCLIENT_CONNECT_ERROR - SOAP client failed to connect to the server.
  WU_E_PT_SOAPCLIENT_SEND	                  = $80244005;	// SOAPCLIENT_SEND_ERROR - SOAP client failed to send a message for reasons of WU_E_WINHTTP_* error codes.
  WU_E_PT_SOAPCLIENT_SERVER               	= $80244006;	// SOAPCLIENT_SERVER_ERROR - SOAP client failed because there was a server error.
  WU_E_PT_SOAPCLIENT_SOAPFAULT	            = $80244007;	// SOAPCLIENT_SOAPFAULT - SOAP client failed because there was a SOAP fault for reasons of WU_E_PT_SOAP_* error codes.
  WU_E_PT_SOAPCLIENT_PARSEFAULT           	= $80244008;	// SOAPCLIENT_PARSEFAULT_ERROR - SOAP client failed to parse a SOAP fault.
  WU_E_PT_SOAPCLIENT_READ	                  = $80244009;	// SOAPCLIENT_READ_ERROR - SOAP client failed while reading the response from the server.
  WU_E_PT_SOAPCLIENT_PARSE                	= $8024400A;	// SOAPCLIENT_PARSE_ERROR - SOAP client failed to parse the response from the server.
  WU_E_PT_SOAP_VERSION	                    = $8024400B;	// SOAP_E_VERSION_MISMATCH - SOAP client found an unrecognizable namespace for the SOAP envelope.
  WU_E_PT_SOAP_MUST_UNDERSTAND	            = $8024400C;	// SOAP_E_MUST_UNDERSTAND - SOAP client was unable to understand a header.
  WU_E_PT_SOAP_CLIENT	                      = $8024400D;	// SOAP_E_CLIENT - SOAP client found the message was malformed; fix before resending.
  WU_E_PT_SOAP_SERVER                     	= $8024400E;	// SOAP_E_SERVER - The SOAP message could not be processed due to a server error; resend later.
  WU_E_PT_WMI_ERROR                       	= $8024400F;	// There was an unspecified Windows Management Instrumentation (WMI) error.
  WU_E_PT_EXCEEDED_MAX_SERVER_TRIPS       	= $80244010;	// The number of round trips to the server exceeded the maximum limit.
  WU_E_PT_SUS_SERVER_NOT_SET	              = $80244011;	// WUServer policy value is missing in the registry.
  WU_E_PT_DOUBLE_INITIALIZATION	            = $80244012;	// Initialization failed because the object was already initialized.
  WU_E_PT_INVALID_COMPUTER_NAME	            = $80244013;	// The computer name could not be determined.
  WU_E_PT_REFRESH_CACHE_REQUIRED	          = $80244015;	// The reply from the server indicates that the server was changed or the cookie was invalid; refresh the state of the internal cache and retry.
  WU_E_PT_HTTP_STATUS_BAD_REQUEST	          = $80244016;	// HTTP 400 - the server could not process the request due to invalid syntax.
  WU_E_PT_HTTP_STATUS_DENIED	              = $80244017;	// HTTP 401 - the requested resource requires user authentication.
  WU_E_PT_HTTP_STATUS_FORBIDDEN	            = $80244018;	// HTTP 403 - server understood the request, but declined to fulfill it.
  WU_E_PT_HTTP_STATUS_NOT_FOUND	            = $80244019;	// HTTP 404 - the server cannot find the requested URI (Uniform Resource Identifier).
  WU_E_PT_HTTP_STATUS_BAD_METHOD	          = $8024401A;	// HTTP 405 - the HTTP method is not allowed.
  WU_E_PT_HTTP_STATUS_PROXY_AUTH_REQ      	= $8024401B;	// HTTP 407 - proxy authentication is required.
  WU_E_PT_HTTP_STATUS_REQUEST_TIMEOUT	      = $8024401C;	// HTTP 408 - the server timed out waiting for the request.
  WU_E_PT_HTTP_STATUS_CONFLICT	            = $8024401D;	// HTTP 409 - the request was not completed due to a conflict with the current state of the resource.
  WU_E_PT_HTTP_STATUS_GONE	                = $8024401E;	// HTTP 410 - requested resource is no longer available at the server.
  WU_E_PT_HTTP_STATUS_SERVER_ERROR        	= $8024401F;	// HTTP 500 - an error internal to the server prevented fulfilling the request.
  WU_E_PT_HTTP_STATUS_NOT_SUPPORTED       	= $80244020;	// HTTP 501 - server does not support the functionality required to fulfill the request.
  WU_E_PT_HTTP_STATUS_BAD_GATEWAY	          = $80244021;	// HTTP 502 - the server, while acting as a gateway or proxy, received an invalid response from the upstream server it accessed in attempting to fulfill the request.
  WU_E_PT_HTTP_STATUS_SERVICE_UNAVAIL	      = $80244022;	// HTTP 503 - the service is temporarily overloaded.
  WU_E_PT_HTTP_STATUS_GATEWAY_TIMEOUT	      = $80244023;	// HTTP 504 - the request was timed out waiting for a gateway.
  WU_E_PT_HTTP_STATUS_VERSION_NOT_SUP	      = $80244024;	// HTTP 505 - the server does not support the HTTP protocol version used for the request.
  WU_E_PT_FILE_LOCATIONS_CHANGED	          = $80244025;	// Operation failed due to a changed file location; refresh internal state and resend.
  WU_E_PT_REGISTRATION_NOT_SUPPORTED      	= $80244026;	// Operation failed because Windows Update Agent does not support registration with a non-WSUS server.
  WU_E_PT_NO_AUTH_PLUGINS_REQUESTED         = $80244027;	// The server returned an empty authentication information list.
  WU_E_PT_NO_AUTH_COOKIES_CREATED	          = $80244028;	// Windows Update Agent was unable to create any valid authentication cookies.
  WU_E_PT_INVALID_CONFIG_PROP             	= $80244029;	// A configuration property value was wrong.
  WU_E_PT_CONFIG_PROP_MISSING             	= $8024402A;	// A configuration property value was missing.
  WU_E_PT_HTTP_STATUS_NOT_MAPPED	          = $8024402B;	// The HTTP request could not be completed and the reason did not correspond to any of the WU_E_PT_HTTP_* error codes.
  WU_E_PT_WINHTTP_NAME_NOT_RESOLVED       	= $8024402C;	// ERROR_WINHTTP_NAME_NOT_RESOLVED - the proxy server or target server name cannot be resolved.
  WU_E_PT_ECP_SUCCEEDED_WITH_ERRORS       	= $8024402F;	// External cab file processing completed with some errors.
  WU_E_PT_ECP_INIT_FAILED	                  = $80244030;	// The external cab processor initialization did not complete.
  WU_E_PT_ECP_INVALID_FILE_FORMAT	          = $80244031;	// The format of a metadata file was invalid.
  WU_E_PT_ECP_INVALID_METADATA	            = $80244032;	// External cab processor found invalid metadata.
  WU_E_PT_ECP_FAILURE_TO_EXTRACT_DIGEST   	= $80244033;	// The file digest could not be extracted from an external cab file.
  WU_E_PT_ECP_FAILURE_TO_DECOMPRESS_CAB_FILE= $80244034;	// An external cab file could not be decompressed.
  WU_E_PT_ECP_FILE_LOCATION_ERROR	          = $80244035;	// External cab processor was unable to get file locations.
  WU_E_PT_UNEXPECTED                      	= $80244FFF;	// A communication error not covered by another WU_E_PT_* error code
  WU_E_REDIRECTOR_LOAD_XML                	= $80245001;	// The redirector XML document could not be loaded into the DOM class.
  WU_E_REDIRECTOR_S_FALSE                 	= $80245002;	// The redirector XML document is missing some required information.
  WU_E_REDIRECTOR_ID_SMALLER              	= $80245003;	// The redirector ID in the downloaded redirector cab is less than in the cached cab.
  WU_E_PT_SAME_REDIR_ID                   	= $8024502D;	// Windows Update Agent failed to download a redirector cabinet file with a new redirector ID value from the server during the recovery.
  WU_E_PT_NO_MANAGED_RECOVER              	= $8024502E;	// A redirector recovery action did not complete because the server is managed.
  WU_E_REDIRECTOR_UNEXPECTED              	= $80245FFF;	// The redirector failed for reasons not covered by another WU_E_REDIRECTOR_* error code.
  WU_E_DM_URLNOTAVAILABLE	                  = $80246001;	// A download manager operation could not be completed because the requested file does not have a URL.
  WU_E_DM_INCORRECTFILEHASH               	= $80246002;	// A download manager operation could not be completed because the file digest was not recognized.
  WU_E_DM_UNKNOWNALGORITHM	                = $80246003;	// A download manager operation could not be completed because the file metadata requested an unrecognized hash algorithm.
  WU_E_DM_NEEDDOWNLOADREQUEST             	= $80246004;	// An operation could not be completed because a download request is required from the download handler.
  WU_E_DM_NONETWORK                       	= $80246005;	// A download manager operation could not be completed because the network connection was unavailable.
  WU_E_DM_WRONGBITSVERSION                	= $80246006;	// A download manager operation could not be completed because the version of Background Intelligent Transfer Service (BITS) is incompatible.
  WU_E_DM_NOTDOWNLOADED	                    = $80246007;	// The update has not been downloaded.
  WU_E_DM_FAILTOCONNECTTOBITS	              = $80246008;	// A download manager operation failed because the download manager was unable to connect the Background Intelligent Transfer Service (BITS).
  WU_E_DM_BITSTRANSFERERROR	                = $80246009;	// A download manager operation failed because there was an unspecified Background Intelligent Transfer Service (BITS) transfer error.
  WU_E_DM_DOWNLOADLOCATIONCHANGED	          = $8024600a;	// A download must be restarted because the location of the source of the download has changed.
  WU_E_DM_CONTENTCHANGED	                  = $8024600B;	// A download must be restarted because the update content changed in a new revision.
  WU_E_DM_UNEXPECTED	                      = $80246FFF;	// There was a download manager error not covered by another WU_E_DM_* error code.
  WU_E_OL_INVALID_SCANFILE                	= $80247001;	// An operation could not be completed because the scan package was invalid.
  WU_E_OL_NEWCLIENT_REQUIRED              	= $80247002;	// An operation could not be completed because the scan package requires a greater version of the Windows Update Agent.
  WU_E_OL_UNEXPECTED                      	= $80247FFF;	// Search using the scan package failed.
  WU_E_DS_SHUTDOWN                          = $80248000;	// An operation failed because Windows Update Agent is shutting down.
  WU_E_DS_INUSE	                            = $80248001;	// An operation failed because the data store was in use.
  WU_E_DS_INVALID	                          = $80248002;	// The current and expected states of the data store do not match.
  WU_E_DS_TABLEMISSING	                    = $80248003;	// The data store is missing a table.
  WU_E_DS_TABLEINCORRECT	                  = $80248004;	// The data store contains a table with unexpected columns.
  WU_E_DS_INVALIDTABLENAME                	= $80248005;	// A table could not be opened because the table is not in the data store.
  WU_E_DS_BADVERSION	                      = $80248006;	// The current and expected versions of the data store do not match.
  WU_E_DS_NODATA	                          = $80248007;	// The information requested is not in the data store.
  WU_E_DS_MISSINGDATA	                      = $80248008;	// The data store is missing required information or has a NULL in a table column that requires a non-null value.
  WU_E_DS_MISSINGREF	                      = $80248009;	// The data store is missing required information or has a reference to missing license terms, file, localized property or linked row.
  WU_E_DS_UNKNOWNHANDLER	                  = $8024800A;	// The update was not processed because its update handler could not be recognized.
  WU_E_DS_CANTDELETE	                      = $8024800B;	// The update was not deleted because it is still referenced by one or more services.
  WU_E_DS_LOCKTIMEOUTEXPIRED	              = $8024800C;	// The data store section could not be locked within the allotted time.
  WU_E_DS_NOCATEGORIES	                    = $8024800D;	// The category was not added because it contains no parent categories and is not a top-level category itself.
  WU_E_DS_ROWEXISTS	                        = $8024800E;	// The row was not added because an existing row has the same primary key.
  WU_E_DS_STOREFILELOCKED	                  = $8024800F;	// The data store could not be initialized because it was locked by another process.
  WU_E_DS_CANNOTREGISTER                  	= $80248010;	// The data store is not allowed to be registered with COM in the current process.
  WU_E_DS_UNABLETOSTART	                    = $80248011;	// Could not create a data store object in another process.
  WU_E_DS_DUPLICATEUPDATEID               	= $80248013;	// The server sent the same update to the client with two different revision IDs.
  WU_E_DS_UNKNOWNSERVICE	                  = $80248014;	// An operation did not complete because the service is not in the data store.
  WU_E_DS_SERVICEEXPIRED                  	= $80248015;	// An operation did not complete because the registration of the service has expired.
  WU_E_DS_DECLINENOTALLOWED               	= $80248016;	// A request to hide an update was declined because it is a mandatory update or because it was deployed with a deadline.
  WU_E_DS_TABLESESSIONMISMATCH            	= $80248017;	// A table was not closed because it is not associated with the session.
  WU_E_DS_SESSIONLOCKMISMATCH             	= $80248018;	// A table was not closed because it is not associated with the session.
  WU_E_DS_NEEDWINDOWSSERVICE	              = $80248019;	// A request to remove the Windows Update service or to unregister it with Automatic Updates was declined because it is a built-in service and/or Automatic Updates cannot fall back to another service.
  WU_E_DS_INVALIDOPERATION	                = $8024801A;	// A request was declined because the operation is not allowed.
  WU_E_DS_SCHEMAMISMATCH	                  = $8024801B;	// The schema of the current data store and the schema of a table in a backup XML document do not match.
  WU_E_DS_RESETREQUIRED	                    = $8024801C;	// The data store requires a session reset; release the session and retry with a new session.
  WU_E_DS_IMPERSONATED	                    = $8024801D;	// A data store operation did not complete because it was requested with an impersonated identity.
  WU_E_DS_UNEXPECTED	                      = $80248FFF;	// A data store error not covered by another WU_E_DS_* code.
  WU_E_INVENTORY_PARSEFAILED	              = $80249001;	// Parsing of the rule file failed.
  WU_E_INVENTORY_GET_INVENTORY_TYPE_FAILED	= $80249002;	// Failed to get the requested inventory type from the server.
  WU_E_INVENTORY_RESULT_UPLOAD_FAILED     	= $80249003;	// Failed to upload inventory result to the server.
  WU_E_INVENTORY_UNEXPECTED               	= $80249004;	// There was an inventory error not covered by another error code.
  WU_E_INVENTORY_WMI_ERROR	                = $80249005;	// A WMI error occurred when enumerating the instances for a particular class.
  WU_E_AU_NOSERVICE	                        = $8024A000;	// Automatic Updates was unable to service incoming requests.
  WU_E_AU_NONLEGACYSERVER                 	= $8024A002;	// The old version of the Automatic Updates client has stopped because the WSUS server has been upgraded.
  WU_E_AU_LEGACYCLIENTDISABLED            	= $8024A003;	// The old version of the Automatic Updates client was disabled.
  WU_E_AU_PAUSED	                          = $8024A004;	// Automatic Updates was unable to process incoming requests because it was paused.
  WU_E_AU_NO_REGISTERED_SERVICE	            = $8024A005;	// No unmanaged service is registered with AU.
  WU_E_AU_UNEXPECTED	                      = $8024AFFF;	// An Automatic Updates error not covered by another WU_E_AU * code.
  WU_E_DRV_PRUNED	                          = $8024C001;	// A driver was skipped.
  WU_E_DRV_NOPROP_OR_LEGACY               	= $8024C002;	// A property for the driver could not be found. It may not conform with required specifications.
  WU_E_DRV_REG_MISMATCH                   	= $8024C003;	// The registry type read for the driver does not match the expected type.
  WU_E_DRV_NO_METADATA	                    = $8024C004;	// The driver update is missing metadata.
  WU_E_DRV_MISSING_ATTRIBUTE              	= $8024C005;	// The driver update is missing a required attribute.
  WU_E_DRV_SYNC_FAILED                    	= $8024C006;	// Driver synchronization failed.
  WU_E_DRV_NO_PRINTER_CONTENT	              = $8024C007;	// Information required for the synchronization of applicable printers is missing.
  WU_E_DRV_UNEXPECTED	                      = $8024CFFF;	// A driver error not covered by another WU_E_DRV_* code.
  WU_E_SETUP_INVALID_INFDATA              	= $8024D001;	// Windows Update Agent could not be updated because an INF file contains invalid information.
  WU_E_SETUP_INVALID_IDENTDATA            	= $8024D002;	// Windows Update Agent could not be updated because the wuident.cab file contains invalid information.
  WU_E_SETUP_ALREADY_INITIALIZED          	= $8024D003;	// Windows Update Agent could not be updated because of an internal error that caused setup initialization to be performed twice.
  WU_E_SETUP_NOT_INITIALIZED              	= $8024D004;	// Windows Update Agent could not be updated because setup initialization never completed successfully.
  WU_E_SETUP_SOURCE_VERSION_MISMATCH        = $8024D005;	// Windows Update Agent could not be updated because the versions specified in the INF do not match the actual source file versions.
  WU_E_SETUP_TARGET_VERSION_GREATER       	= $8024D006;	// Windows Update Agent could not be updated because a WUA file on the target system is newer than the corresponding source file.
  WU_E_SETUP_REGISTRATION_FAILED	          = $8024D007;	// Windows Update Agent could not be updated because regsvr32.exe returned an error.
  WU_E_SELFUPDATE_SKIP_ON_FAILURE	          = $8024D008;	// An update to the Windows Update Agent was skipped because previous attempts to update have failed.
  WU_E_SETUP_SKIP_UPDATE	                  = $8024D009;	// An update to the Windows Update Agent was skipped due to a directive in the wuident.cab file.
  WU_E_SETUP_UNSUPPORTED_CONFIGURATION    	= $8024D00A;	// Windows Update Agent could not be updated because the current system configuration is not supported.
  WU_E_SETUP_BLOCKED_CONFIGURATION        	= $8024D00B;	// Windows Update Agent could not be updated because the system is configured to block the update.
  WU_E_SETUP_REBOOT_TO_FIX                	= $8024D00C;	// Windows Update Agent could not be updated because a restart of the system is required.
  WU_E_SETUP_ALREADYRUNNING               	= $8024D00D;	// Windows Update Agent setup is already running.
  WU_E_SETUP_REBOOTREQUIRED               	= $8024D00E;	// Windows Update Agent setup package requires a reboot to complete installation.
  WU_E_SETUP_HANDLER_EXEC_FAILURE	          = $8024D00F;	// Windows Update Agent could not be updated because the setup handler failed during execution.
  WU_E_SETUP_INVALID_REGISTRY_DATA        	= $8024D010;	// Windows Update Agent could not be updated because the registry contains invalid information.
  WU_E_SELFUPDATE_REQUIRED                	= $8024D011;	// Windows Update Agent must be updated before search can continue.
  WU_E_SELFUPDATE_REQUIRED_ADMIN	          = $8024D012;	// Windows Update Agent must be updated before search can continue. An administrator is required to perform the operation.
  WU_E_SETUP_WRONG_SERVER_VERSION         	= $8024D013;	// Windows Update Agent could not be updated because the server does not contain update information for this version.
  WU_E_SETUP_UNEXPECTED	                    = $8024DFFF;	// Windows Update Agent could not be updated because of an error not covered by another WU_E_SETUP_* error code.
  WU_E_EE_UNKNOWN_EXPRESSION              	= $8024E001;	// An expression evaluator operation could not be completed because an expression was unrecognized.
  WU_E_EE_INVALID_EXPRESSION              	= $8024E002;	// An expression evaluator operation could not be completed because an expression was invalid.
  WU_E_EE_MISSING_METADATA                	= $8024E003;	// An expression evaluator operation could not be completed because an expression contains an incorrect number of metadata nodes.
  WU_E_EE_INVALID_VERSION	                  = $8024E004;	// An expression evaluator operation could not be completed because the version of the serialized expression data is invalid.
  WU_E_EE_NOT_INITIALIZED	                  = $8024E005;	// The expression evaluator could not be initialized.
  WU_E_EE_INVALID_ATTRIBUTEDATA           	= $8024E006;	// An expression evaluator operation could not be completed because there was an invalid attribute.
  WU_E_EE_CLUSTER_ERROR	                    = $8024E007;	// An expression evaluator operation could not be completed because the cluster state of the computer could not be determined.
  WU_E_EE_UNEXPECTED                      	= $8024EFFF;	// There was an expression evaluator error not covered by another WU_E_EE_* error code.
  WU_E_REPORTER_EVENTCACHECORRUPT	          = $8024F001;	// The event cache file was defective.
  WU_E_REPORTER_                            = $8024F002;	// The XML in the event namespace descriptor could not be parsed.

// EVENTNAMESPACEPARSEFAILED	                =	   ;	// nicht dokumentiert....

  WU_E_INVALID_EVENT                      	= $8024F003;	// The XML in the event namespace descriptor could not be parsed.
  WU_E_SERVER_BUSY                        	= $8024F004;	// The server rejected an event because the server was too busy.
  WU_E_REPORTER_UNEXPECTED	                = $8024FFFF;	// There was a reporter error not covered by another error code.

// Operation Result Codes

{
typedef enum  (
  orcNotStarted            = 0,
  orcInProgress            = 1,
  orcSucceeded             = 2,
  orcSucceededWithErrors   = 3,
  orcFailed                = 4,
  orcAborted               = 5
) OperationResultCode;
}
