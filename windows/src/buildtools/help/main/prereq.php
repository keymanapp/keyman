<?php include( "../inc/kmbuild.php" );
FORMAT( <<<ENDFORMAT

HTML:       <style>span.dir { font-weight: bold; color: #700000 }</style>

PAGE:       Tavultesoft Keyman 6.0 Build Help

TITLE:      Build Prerequisites

BODY

SECTION:      Development Tools
TEXT:         The following developmenet tools are required to build Keyman:
BULLETLIST:   Windows 2000 SP 2 or later
              Delphi 2006 Professional Update 2, installed in <span class="dir">c:\program files\borland\bds\4.0</span>
              Visual Studio 2005 (C++), installed in <span class="dir">c:\program files\microsoft visual studio 8</span>
              --PHP for Win32, 4.1.2 or later, installed in <span class="dir">c:\php</span>
              Microsoft HTML Help Compiler, version 4.74.8702 or later, installed in <span class="dir">c:\program files\HTML Help Workshop</span>
              WinZip 8.1, installed in <span class="dir">c:\program files\winzip</span>
              WinZipCL, installed with winzip 8.1
              Microsoft Windows Server 2003 R3 Platform SDK or later, installed in <span class="dir">c:\program files\microsoft platform sdk</span>
              WinCVS, with CVSROOT=:pserver:mcdurdin@cvs-keyman:/home/tavultesoft/keyman
              TortoiseCVS
              .NET framework v1.1 SDK (contains signcode.exe and tools)
	      WiX in <span class="dir">c:\program files\wix</span>
              
ENDSECTION

SECTION:      Environment
BULLETLIST:   	@call "c:\program files\microsoft visual studio 8\vc\bin\vcvars32.bat"
		@call "c:\program files\microsoft platform sdk\setenv.cmd"
		@cd /d c:\keyman\7.0\src
		@set PATH=C:\Program Files\Microsoft Platform SDK\Bin;C:\Program Files\Microsoft Platform SDK\Bin\WinNT;
			C:\Program Files\Microsoft Visual Studio 8\Common7\IDE;C:\Program Files\Microsoft Visual Studio 8\VC\BIN;
			C:\Program Files\Microsoft Visual Studio 8\Common7\Tools;C:\Program Files\Microsoft Visual Studio 8\Common7\Tools\bin;
			C:\Program Files\Microsoft Visual Studio 8\VC\PlatformSDK\bin;C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0\bin;
			C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727;C:\Program Files\Microsoft Visual Studio 8\VC\VCPackages;
			C:\Program Files\Microsoft Visual Studio 8\Common7\IDE;C:\Program Files\Microsoft Visual Studio 8\VC\BIN;
			C:\Program Files\Microsoft Visual Studio 8\Common7\Tools;C:\Program Files\Microsoft Visual Studio 8\Common7\Tools\bin;
			C:\Program Files\Microsoft Visual Studio 8\VC\PlatformSDK\bin;C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0\bin;
			C:\WINDOWS\Microsoft.NET\Framework\v2.0.50727;C:\Program Files\Microsoft Visual Studio 8\VC\VCPackages;
			C:\Program Files\Windows Resource Kits\Tools\;C:\Perl\bin\;c:\bin;C:\WINDOWS\Microsoft.NET\Framework\v1.1.4322\;
			C:\Program Files\Borland\BDS\4.0\Bin;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;
			c:\Program Files\Microsoft SQL Server\90\Tools\binn\;C:\Program Files\Microsoft SQL Server\80\Tools\Binn\;
			C:\Program Files\Microsoft SQL Server\90\DTS\Binn\;C:\Program Files\Microsoft SQL Server\90\Tools\Binn\VSShell\Common7\IDE\;
			C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\PrivateAssemblies\;c:\program files\WiX;
			C:\Documents and Settings\mcdurdin\My Documents\Borland Studio Projects\Bpl
		@cmd

ENDSECTION

SECTION:      Other files
BULLETLIST:   Code signing requires:<ul><li>Certificate c:\tavultesoft\cert\tavultesoft.spc<li>Private key c:\tavultesoft\cert\tavultesoft.pvk</ul>
ENDSECTION

SECTION:      Knowledge
BULLETLIST:   Always use MAKE, not NMAKE.  NMAKE is called internally in the Makefiles for C++ programs
              Type MAKE&lt;enter&gt; to find a list of targets in most directories.
ENDSECTION

ENDBODY

PAGENAV:      "Table of Contents", "contents.html", "Build", "build.html"

FOOTER

ENDPAGE
ENDFORMAT
);
?>
