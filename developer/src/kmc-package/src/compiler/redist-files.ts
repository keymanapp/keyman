
/**
 * This is a set of known redistributable files for Keyman for Windows that
 * should not be included in packages. It is not critical that this list matches
 * the current deployment; it is just for warning against accidental inclusion
 * of these files by package authors. Some redistributable files have been
 * intentionally excluded because they could legitimately be a different file
 * with the same name.
 *
 * This matches behaviour from the legacy package compiler; we may want to
 * reconsider how this is done in the future.
 *
 * These lists have been constructed from 17.0.109 alpha build. Filenames
 * intentionally in lower case.
 */

export const
  keymanForWindowsInstallerFiles: string[] = [
    'keymandesktop.msi',
    'keymanengine.msm'
  ];


export const
  keymanEngineForWindowsFiles: string[] = [
    'base.xslt',
    'crashpad_handler.exe',
    'keyman-debug-etw.man',
    'keyman.exe',
    'keyman32.dll',
    'keyman64.dll',
    'keymanmc.dll',
    'keymanx64.exe',
    'kmcomapi.dll',
    'kmcomapi.x64.dll',
    'kmrefresh.x64.exe',
    'kmrefresh.x86.exe',
    'kmtip.dll',
    'kmtip64.dll',
    'mcompile.exe',
    'sentry.dll',
    'sentry.x64.dll',
    'si_browsers.xslt',
    'si_fonts.xslt',
    'si_hookdlls.xslt',
    'si_keyman.xslt',
    'si_language.xslt',
    'si_office.xslt',
    'si_overview.xslt',
    'si_processes.xslt',
    'si_processes_x64.xslt',
    'si_startup.xslt',
    'tsysinfo.exe',
  ];

export const
  keymanForWindowsRedistFiles: string[] = [
    'desktop_resources.dll',
    'keymandesktop.chm',
    'kmbrowserhost.exe',
    'kmconfig.exe',
    'kmshell.exe',
    'unicodedata.mdb',
  ];
