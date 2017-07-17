program tsysinfox64;

uses
  fileversioninfo in 'fileversioninfo.pas',
  si_base in 'si_base.pas',
  si_processes in 'si_processes.pas',
  sysinfo_util in 'sysinfo_util.pas',
  main in 'main.pas';

{$R *.res}
{$R manifest.res}
{$R version.res}

begin
  Run;
end.
