# IIS local host secure website setup

The setup-iis-https.sh script in this folder automates much of the certificate
setup required to host secure versions of the Keyman sites on your development
machine, such as https://keyman.com.local/.

The script must be run as Administrator and can safely be run multiple times.

Once configured, the certificates will work for the following sites (configured in v3.ext):
- `localhost`
- `keyman.com.local`
- `*.keyman.com.local`
- `keymanweb.com.local`
- `keymankeyboards.com.local`
- `keyman.dev.local`
- `*.keymanweb.com.local`
