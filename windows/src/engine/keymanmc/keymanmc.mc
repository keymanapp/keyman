; // Keymanmc.mc 

; // This is the header section.


SeverityNames=(Success=0x0:STATUS_SEVERITY_SUCCESS
               Informational=0x1:STATUS_SEVERITY_INFORMATIONAL
               Warning=0x2:STATUS_SEVERITY_WARNING
               Error=0x3:STATUS_SEVERITY_ERROR
              )


FacilityNames=(System=0x0:FACILITY_SYSTEM
               Runtime=0x2:FACILITY_RUNTIME
               Stubs=0x3:FACILITY_STUBS
               Io=0x4:FACILITY_IO_ERROR_CODE
              )

LanguageNames=(English=0x409:MSG00409)


; // The following are the categories of events.

MessageIdTypedef=WORD

MessageId=0x1
SymbolicName=SIMPLE_CATEGORY
Language=English
Default
.

; // The following are the message definitions.

MessageIdTypedef=DWORD

MessageId=0x100
Severity=Informational
Facility=Runtime
SymbolicName=MSG_START_KEYMAN_ON_DEMAND
Language=English
A Keyman input method has been activated while Keyman was not running; now attempting to start Keyman.
.
