{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTConst.PAS, released on 2002-09-24.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-09-25


Known Issues:
-----------------------------------------------------------------------------}

unit MTConst;

interface
uses
  SysUtils, Windows, Classes;

type
  TMTTicket = Integer;

type
  EMTThread = class(EThread);
  EMTTerminate = class(EAbort);

type
  TMTThreadStatus = (tsInitializing, tsWaiting, tsRunning, tsTerminating,
    tsFinished);

type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;

const
  MTDefaultBufferSize = 32;

implementation


end.
