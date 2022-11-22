!include ..\..\Defines.mak

default:
    @echo The following make targets are available:
    @echo .   test-certificate      create a test certificate + CA (and install test CA)

test-certificate:
    cd $(COMMON_ROOT)\tools\certificates
    -del KeymanTestCA-sha1.*
    -del KeymanTest-sha1.*
    -del KeymanTest-sha256.*
    -del KeymanTestCA-sha256.*

    makecert -r -pe -n "CN=Keyman Test CA SHA1" -ss CA -sr CurrentUser -a sha1 -cy authority -sky signature -sv KeymanTestCA-sha1.pvk KeymanTestCA-sha1.cer
    certutil -user -addstore Root KeymanTestCA-sha1.cer
    makecert -pe -n "CN=Keyman Test Certificate SHA1" -a sha1 -cy end -sky signature -ic KeymanTestCA-sha1.cer -iv KeymanTestCA-sha1.pvk -sv KeymanTest-sha1.pvk KeymanTest-sha1.cer
    pvk2pfx -pvk KeymanTest-sha1.pvk -spc KeymanTest-sha1.cer -pfx KeymanTest-sha1.pfx

    makecert -r -pe -n "CN=Keyman Test CA" -ss CA -sr CurrentUser -a sha256 -cy authority -sky signature -sv KeymanTestCA-sha256.pvk KeymanTestCA-sha256.cer
    certutil -user -addstore Root KeymanTestCA-sha256.cer
    makecert -pe -n "CN=Keyman Test Certificate" -a sha256 -cy end -sky signature -ic KeymanTestCA-sha256.cer -iv KeymanTestCA-sha256.pvk -sv KeymanTest-sha256.pvk KeymanTest-sha256.cer
    pvk2pfx -pvk KeymanTest-sha256.pvk -spc KeymanTest-sha256.cer -pfx KeymanTest-sha256.pfx
