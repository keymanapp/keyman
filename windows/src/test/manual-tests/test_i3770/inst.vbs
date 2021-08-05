dim k 

set k = CreateObject("kmcomapi.TavultesoftKeyman")

k.keyboards.index.install "turkish_q.kmx", false, false




dim kmcom, package
Set kmcom = CreateObject("kmcomapi.TavultesoftKeyman")
n = kmcom.Packages.IndexOf("SamplePackage")
if n > 0 then
  Set package = kmcom.Packages(n)
  if msgbox("Uninstall package "+package.Description+"?", vbOKCancel, "Keyman Desktop") = vbOK then
    package.Uninstall(True)
  end if
else
  msgbox "The package SamplePackage could not be found."
end if