{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Debug user interface status enumeration
}
unit Keyman.System.Debug.DebugUIStatus;

interface

type
  TDebugUIStatus = (
    duiInvalid,
    duiPaused,
    duiFocusedForInput,
    duiReadyForInput,
    duiDebugging,
    duiClosing,
    duiTest);

implementation

end.
