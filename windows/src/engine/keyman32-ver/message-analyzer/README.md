# Event tracing

keyman-debug-etw.man will be installed with Keyman Engine so that debug log files can be analyzed in Event Viewer or
Microsoft Message Analyzer. You can import the layout file keyman-debug-etw.asset into Microsoft Message Analyzer
to provide a good default layout for Keyman log files.

To manually install the provider, run:

    wevtutil im keyman-debug-etw.man

To uninstall it (a good idea often before installing):

    wevtutil um keyman-debug-etw.man

You may need to manually edit the file to add the paths for keyman32-ver.dll, e.g.:

    <provider name="Keyman-Debug-ETWProvider" guid="{DA621615-E08B-4283-918E-D2502D3757AE}" symbol="ProviderGuid" message="$(string.Provider.Name)" messageFileName="C:\Program Files (x86)\Common Files\Keyman\Keyman Engine\keyman32-ver.dll" resourceFileName="C:\Program Files (x86)\Common Files\Keyman\Keyman Engine\keyman32-ver.dll">

Debug log files are written to %LOCAL_APP_DATA%\Keyman\Diag.
