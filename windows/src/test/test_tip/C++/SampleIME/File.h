/*
  Name:             File
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      31 Dec 2014

  Modified Date:    31 Dec 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
*/
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//
// Copyright (c) Microsoft Corporation. All rights reserved

#pragma once

class CFile
{
public:
    CFile(UINT codePage = CP_ACP);
    virtual ~CFile();

    BOOL CreateFile(_In_ PCWSTR pFileName, DWORD desiredAccess, DWORD creationDisposition,
        DWORD sharedMode = 0, _Inout_opt_ LPSECURITY_ATTRIBUTES lpSecurityAttributes = nullptr, DWORD flagsAndAttributes = 0, _Inout_opt_ HANDLE templateFileHandle = nullptr);

    BOOL IsEndOfFile();
    VOID NextLine();

    _Ret_maybenull_
    const WCHAR *GetReadBufferPointer()
    {
        if (!_pReadBuffer)
        {
            if (!SetupReadBuffer())
            {
                return nullptr;
            }
        }
        return _pReadBuffer;
    }

    DWORD_PTR GetFileSize() { return _fileSize;}

    LPCWSTR GetFileName() { return _pFileName;}

protected:
    virtual BOOL SetupReadBuffer();

protected:
    const WCHAR* _pReadBuffer;   // read buffer memory.
    DWORD_PTR _fileSize;         // in byte.
    HANDLE _fileHandle;          // file handle for CreateFile

private:
    DWORD_PTR GetBufferInWCharLength()
    {
        if (_filePosPointer == 0 && _fileSize > 0)
        {
            // skip Unicode byte order mark
            GetReadBufferPointer();
        }
        return(_fileSize - _filePosPointer) / sizeof(WCHAR);    // in char count as a returned length.
    }

    const WCHAR *GetBufferInWChar()
    {
        const WCHAR *pwch = GetReadBufferPointer();
        return(const WCHAR*)((BYTE*)pwch + _filePosPointer);
    }

private:
    UINT _codePage;             // used for MultiByteToWideChar
    DWORD_PTR _filePosPointer;  // in byte. Always point start of line.
    LPWSTR _pFileName;
};
