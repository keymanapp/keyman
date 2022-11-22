{*
 scarderr.mc

   Error message codes from the Smart Card Resource Manager
   These messages must be reconciled with winerror.w
   They exist here to provide error messages on pre-Win2K systems.

*}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaCardErr;

interface
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  // =============================
  // Facility SCARD Error Messages
  // =============================
  //
  SCARD_S_SUCCESS = 0;
  //
  //  Values are 32 bit values layed out as follows:
  //
  //   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  //   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  //  +---+-+-+-----------------------+-------------------------------+
  //  |Sev|C|R|     Facility          |               Code            |
  //  +---+-+-+-----------------------+-------------------------------+
  //
  //  where
  //
  //      Sev - is the severity code
  //
  //          00 - Success
  //          01 - Informational
  //          10 - Warning
  //          11 - Error
  //
  //      C - is the Customer code flag
  //
  //      R - is a reserved bit
  //
  //      Facility - is the facility code
  //
  //      Code - is the facility's status code
  //
  //
  // Define the facility codes
  //
  FACILITY_SYSTEM                  = $0;
  FACILITY_SCARD                   = $10;


  //
  // Define the severity codes
  //
  STATUS_SEVERITY_WARNING          = $2;
  STATUS_SEVERITY_INFORMATIONAL    = $1;
  STATUS_SEVERITY_ERROR            = $3;


  //
  // MessageId: SCARD_F_INTERNAL_ERROR
  //
  // MessageText:
  //
  //  An internal consistency check failed.
  //
  SCARD_F_INTERNAL_ERROR           = $80100001;

  //
  // MessageId: SCARD_E_CANCELLED
  //
  // MessageText:
  //
  //  The action was cancelled by an SCardCancel request.
  //
  SCARD_E_CANCELLED                = $80100002;

  //
  // MessageId: SCARD_E_INVALID_HANDLE
  //
  // MessageText:
  //
  //  The supplied handle was invalid.
  //
  SCARD_E_INVALID_HANDLE           = $80100003;

  //
  // MessageId: SCARD_E_INVALID_PARAMETER
  //
  // MessageText:
  //
  //  One or more of the supplied parameters could not be properly interpreted.
  //
  SCARD_E_INVALID_PARAMETER        = $80100004;

  //
  // MessageId: SCARD_E_INVALID_TARGET
  //
  // MessageText:
  //
  //  Registry startup information is missing or invalid.
  //
  SCARD_E_INVALID_TARGET           = $80100005;

  //
  // MessageId: SCARD_E_NO_MEMORY
  //
  // MessageText:
  //
  //  Not enough memory available to complete this command.
  //
  SCARD_E_NO_MEMORY                = $80100006;

  //
  // MessageId: SCARD_F_WAITED_TOO_LONG
  //
  // MessageText:
  //
  //  An internal consistency timer has expired.
  //
  SCARD_F_WAITED_TOO_LONG          = $80100007;

  //
  // MessageId: SCARD_E_INSUFFICIENT_BUFFER
  //
  // MessageText:
  //
  //  The data buffer to receive returned data is too small for the returned data.
  //
  SCARD_E_INSUFFICIENT_BUFFER      = $80100008;

  //
  // MessageId: SCARD_E_UNKNOWN_READER
  //
  // MessageText:
  //
  //  The specified reader name is not recognized.
  //
  SCARD_E_UNKNOWN_READER           = $80100009;

  //
  // MessageId: SCARD_E_TIMEOUT
  //
  // MessageText:
  //
  //  The user-specified timeout value has expired.
  //
  SCARD_E_TIMEOUT                  = $8010000A;

  //
  // MessageId: SCARD_E_SHARING_VIOLATION
  //
  // MessageText:
  //
  //  The smart card cannot be accessed because of other connections outstanding.
  //
  SCARD_E_SHARING_VIOLATION        = $8010000B;

  //
  // MessageId: SCARD_E_NO_SMARTCARD
  //
  // MessageText:
  //
  //  The operation requires a Smart Card, but no Smart Card is currently in the device.
  //
  SCARD_E_NO_SMARTCARD             = $8010000C;

  //
  // MessageId: SCARD_E_UNKNOWN_CARD
  //
  // MessageText:
  //
  //  The specified smart card name is not recognized.
  //
  SCARD_E_UNKNOWN_CARD             = $8010000D;

  //
  // MessageId: SCARD_E_CANT_DISPOSE
  //
  // MessageText:
  //
  //  The system could not dispose of the media in the requested manner.
  //
  SCARD_E_CANT_DISPOSE             = $8010000E;

  //
  // MessageId: SCARD_E_PROTO_MISMATCH
  //
  // MessageText:
  //
  //  The requested protocols are incompatible with the protocol currently in use with the smart card.
  //
  SCARD_E_PROTO_MISMATCH           = $8010000F;

  //
  // MessageId: SCARD_E_NOT_READY
  //
  // MessageText:
  //
  //  The reader or smart card is not ready to accept commands.
  //
  SCARD_E_NOT_READY                = $80100010;

  //
  // MessageId: SCARD_E_INVALID_VALUE
  //
  // MessageText:
  //
  //  One or more of the supplied parameters values could not be properly interpreted.
  //
  SCARD_E_INVALID_VALUE            = $80100011;

  //
  // MessageId: SCARD_E_SYSTEM_CANCELLED
  //
  // MessageText:
  //
  //  The action was cancelled by the system, presumably to log off or shut down.
  //
  SCARD_E_SYSTEM_CANCELLED         = $80100012;

  //
  // MessageId: SCARD_F_COMM_ERROR
  //
  // MessageText:
  //
  //  An internal communications error has been detected.
  //
  SCARD_F_COMM_ERROR               = $80100013;

  //
  // MessageId: SCARD_F_UNKNOWN_ERROR
  //
  // MessageText:
  //
  //  An internal error has been detected, but the source is unknown.
  //
  SCARD_F_UNKNOWN_ERROR            = $80100014;

  //
  // MessageId: SCARD_E_INVALID_ATR
  //
  // MessageText:
  //
  //  An ATR obtained from the registry is not a valid ATR string.
  //
  SCARD_E_INVALID_ATR              = $80100015;

  //
  // MessageId: SCARD_E_NOT_TRANSACTED
  //
  // MessageText:
  //
  //  An attempt was made to end a non-existent transaction.
  //
  SCARD_E_NOT_TRANSACTED           = $80100016;

  //
  // MessageId: SCARD_E_READER_UNAVAILABLE
  //
  // MessageText:
  //
  //  The specified reader is not currently available for use.
  //
  SCARD_E_READER_UNAVAILABLE       = $80100017;

  //
  // MessageId: SCARD_P_SHUTDOWN
  //
  // MessageText:
  //
  //  The operation has been aborted to allow the server application to exit.
  //
  SCARD_P_SHUTDOWN                 = $80100018;

  //
  // MessageId: SCARD_E_PCI_TOO_SMALL
  //
  // MessageText:
  //
  //  The PCI Receive buffer was too small.
  //
  SCARD_E_PCI_TOO_SMALL            = $80100019;

  //
  // MessageId: SCARD_E_READER_UNSUPPORTED
  //
  // MessageText:
  //
  //  The reader driver does not meet minimal requirements for support.
  //
  SCARD_E_READER_UNSUPPORTED       = $8010001A;

  //
  // MessageId: SCARD_E_DUPLICATE_READER
  //
  // MessageText:
  //
  //  The reader driver did not produce a unique reader name.
  //
  SCARD_E_DUPLICATE_READER         = $8010001B;

  //
  // MessageId: SCARD_E_CARD_UNSUPPORTED
  //
  // MessageText:
  //
  //  The smart card does not meet minimal requirements for support.
  //
  SCARD_E_CARD_UNSUPPORTED         = $8010001C;

  //
  // MessageId: SCARD_E_NO_SERVICE
  //
  // MessageText:
  //
  //  The Smart card resource manager is not running.
  //
  SCARD_E_NO_SERVICE               = $8010001D;

  //
  // MessageId: SCARD_E_SERVICE_STOPPED
  //
  // MessageText:
  //
  //  The Smart card resource manager has shut down.
  //
  SCARD_E_SERVICE_STOPPED          = $8010001E;

  //
  // MessageId: SCARD_E_UNEXPECTED
  //
  // MessageText:
  //
  //  An unexpected card error has occurred.
  //
  SCARD_E_UNEXPECTED               = $8010001F;

  //
  // MessageId: SCARD_E_ICC_INSTALLATION
  //
  // MessageText:
  //
  //  No Primary Provider can be found for the smart card.
  //
  SCARD_E_ICC_INSTALLATION         = $80100020;

  //
  // MessageId: SCARD_E_ICC_CREATEORDER
  //
  // MessageText:
  //
  //  The requested order of object creation is not supported.
  //
  SCARD_E_ICC_CREATEORDER          = $80100021;

  //
  // MessageId: SCARD_E_UNSUPPORTED_FEATURE
  //
  // MessageText:
  //
  //  This smart card does not support the requested feature.
  //
  SCARD_E_UNSUPPORTED_FEATURE      = $80100022;

  //
  // MessageId: SCARD_E_DIR_NOT_FOUND
  //
  // MessageText:
  //
  //  The identified directory does not exist in the smart card.
  //
  SCARD_E_DIR_NOT_FOUND            = $80100023;

  //
  // MessageId: SCARD_E_FILE_NOT_FOUND
  //
  // MessageText:
  //
  //  The identified file does not exist in the smart card.
  //
  SCARD_E_FILE_NOT_FOUND           = $80100024;

  //
  // MessageId: SCARD_E_NO_DIR
  //
  // MessageText:
  //
  //  The supplied path does not represent a smart card directory.
  //
  SCARD_E_NO_DIR                   = $80100025;

  //
  // MessageId: SCARD_E_NO_FILE
  //
  // MessageText:
  //
  //  The supplied path does not represent a smart card file.
  //
  SCARD_E_NO_FILE                  = $80100026;

  //
  // MessageId: SCARD_E_NO_ACCESS
  //
  // MessageText:
  //
  //  Access is denied to this file.
  //
  SCARD_E_NO_ACCESS                = $80100027;

  //
  // MessageId: SCARD_E_WRITE_TOO_MANY
  //
  // MessageText:
  //
  //  The smartcard does not have enough memory to store the information.
  //
  SCARD_E_WRITE_TOO_MANY           = $80100028;

  //
  // MessageId: SCARD_E_BAD_SEEK
  //
  // MessageText:
  //
  //  There was an error trying to set the smart card file object pointer.
  //
  SCARD_E_BAD_SEEK                 = $80100029;

  //
  // MessageId: SCARD_E_INVALID_CHV
  //
  // MessageText:
  //
  //  The supplied PIN is incorrect.
  //
  SCARD_E_INVALID_CHV              = $8010002A;

  //
  // MessageId: SCARD_E_UNKNOWN_RES_MNG
  //
  // MessageText:
  //
  //  An unrecognized error code was returned from a layered component.
  //
  SCARD_E_UNKNOWN_RES_MNG          = $8010002B;

  //
  // MessageId: SCARD_E_NO_SUCH_CERTIFICATE
  //
  // MessageText:
  //
  //  The requested certificate does not exist.
  //
  SCARD_E_NO_SUCH_CERTIFICATE      = $8010002C;

  //
  // MessageId: SCARD_E_CERTIFICATE_UNAVAILABLE
  //
  // MessageText:
  //
  //  The requested certificate could not be obtained.
  //
  SCARD_E_CERTIFICATE_UNAVAILABLE  = $8010002D;

  //
  // MessageId: SCARD_E_NO_READERS_AVAILABLE
  //
  // MessageText:
  //
  //  Cannot find a smart card reader.
  //
  SCARD_E_NO_READERS_AVAILABLE     = $8010002E;

  //
  // MessageId: SCARD_E_COMM_DATA_LOST
  //
  // MessageText:
  //
  //  A communications error with the smart card has been detected.  Retry the operation.
  //
  SCARD_E_COMM_DATA_LOST           = $8010002F;

  //
  // MessageId: SCARD_E_NO_KEY_CONTAINER
  //
  // MessageText:
  //
  //  The requested key container does not exist on the smart card.
  //
  SCARD_E_NO_KEY_CONTAINER         = $80100030;

  //
  // MessageId: SCARD_E_SERVER_TOO_BUSY
  //
  // MessageText:
  //
  //  The Smart card resource manager is too busy to complete this operation.
  //
  SCARD_E_SERVER_TOO_BUSY          = $80100031;

  //
  // These are warning codes.
  //
  //
  // MessageId: SCARD_W_UNSUPPORTED_CARD
  //
  // MessageText:
  //
  //  The reader cannot communicate with the smart card, due to ATR configuration conflicts.
  //
  SCARD_W_UNSUPPORTED_CARD         = $80100065;

  //
  // MessageId: SCARD_W_UNRESPONSIVE_CARD
  //
  // MessageText:
  //
  //  The smart card is not responding to a reset.
  //
  SCARD_W_UNRESPONSIVE_CARD        = $80100066;

  //
  // MessageId: SCARD_W_UNPOWERED_CARD
  //
  // MessageText:
  //
  //  Power has been removed from the smart card, so that further communication is not possible.
  //
  SCARD_W_UNPOWERED_CARD           = $80100067;

  //
  // MessageId: SCARD_W_RESET_CARD
  //
  // MessageText:
  //
  //  The smart card has been reset, so any shared state information is invalid.
  //
  SCARD_W_RESET_CARD               = $80100068;

  //
  // MessageId: SCARD_W_REMOVED_CARD
  //
  // MessageText:
  //
  //  The smart card has been removed, so that further communication is not possible.
  //
  SCARD_W_REMOVED_CARD             = $80100069;

  //
  // MessageId: SCARD_W_SECURITY_VIOLATION
  //
  // MessageText:
  //
  //  Access was denied because of a security violation.
  //
  SCARD_W_SECURITY_VIOLATION       = $8010006A;

  //
  // MessageId: SCARD_W_WRONG_CHV
  //
  // MessageText:
  //
  //  The card cannot be accessed because the wrong PIN was presented.
  //
  SCARD_W_WRONG_CHV                = $8010006B;

  //
  // MessageId: SCARD_W_CHV_BLOCKED
  //
  // MessageText:
  //
  //  The card cannot be accessed because the maximum number of PIN entry attempts has been reached.
  //
  SCARD_W_CHV_BLOCKED              = $8010006C;

  //
  // MessageId: SCARD_W_EOF
  //
  // MessageText:
  //
  //  The end of the smart card file has been reached.
  //
  SCARD_W_EOF                      = $8010006D;

  //
  // MessageId: SCARD_W_CANCELLED_BY_USER
  //
  // MessageText:
  //
  //  The action was cancelled by the user.
  //
  SCARD_W_CANCELLED_BY_USER        = $8010006E;

  //
  // MessageId: SCARD_W_CARD_NOT_AUTHENTICATED
  //
  // MessageText:
  //
  //  No PIN was presented to the smart card.
  //
  SCARD_W_CARD_NOT_AUTHENTICATED   = $8010006F;
{$ENDIF JWA_INCLUDEMODE}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}
//your implementation here
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
