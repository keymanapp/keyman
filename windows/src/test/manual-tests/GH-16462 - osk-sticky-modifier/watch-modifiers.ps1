# GH-8064 continuous modifier watcher. Logs every CHANGE in modifier state, so no
# keyboard interaction is needed during the test (the whole point: a stuck Ctrl
# makes the keyboard unusable). Run in background; read the log afterwards.
param([int]$Minutes = 45)
Add-Type -Name KmW -Namespace Watch -MemberDefinition '[DllImport("user32.dll")] public static extern short GetAsyncKeyState(int vKey);'
$log = Join-Path $PSScriptRoot 'osk-watch.txt'
$m = [ordered]@{ SHIFT=0x10; CTRL=0x11; ALT=0x12; LSHIFT=0xA0; RSHIFT=0xA1; LCTRL=0xA2; RCTRL=0xA3; LALT=0xA4; RALT=0xA5 }
"=== watcher started $(Get-Date -f 'yyyy-MM-dd HH:mm:ss') ===" | Add-Content $log
$prev = $null
$end = (Get-Date).AddMinutes($Minutes)
while ((Get-Date) -lt $end) {
  $down = @()
  foreach ($k in $m.GetEnumerator()) { if ([Watch.KmW]::GetAsyncKeyState($k.Value) -lt 0) { $down += $k.Key } }
  $now = ($down -join ',')
  if ($now -ne $prev) {
    $line = "[{0}] {1}" -f (Get-Date -f 'HH:mm:ss.fff'), $(if ($down.Count) { "HELD: $now" } else { 'ALL CLEAR' })
    $line | Add-Content $log
    Write-Host $line
    $prev = $now
  }
  Start-Sleep -Milliseconds 60
}
"=== watcher ended $(Get-Date -f 'HH:mm:ss') ===" | Add-Content $log
