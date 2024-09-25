# How to Set Up a Local Web Server for the Keyman Web Pages

Currently, most of the Keyman websites (*.keyman.com, *.keyman-staging.com) are running PHP 7.4 via Apache.

## Pre-requisite Installs
* [Docker Desktop](https://www.docker.com/products/docker-desktop/)

On Windows, Docker will need either:
* Hyper-V or
* WSL 2.0 - Install with this guide:
https://learn.microsoft.com/en-us/windows/wsl/tutorials/wsl-containers
    * WSL will then need a Linux image (e.g. Ubuntu app) from the Microsoft Store

#### Other Docker Notes
Docker tends to throttle Docker image downloads, so some developer offices may want to set up a proxy server. If the proxy server is set up, carefully edit the JSON file per in Docker Settings -> Docker Engine https://docs.docker.com/registry/recipes/mirror/#configure-the-docker-daemon and click 'Apply & Restart'. Note the example (lingnet) is for running inside the Linguistics Institute (Chiang Mai)

```json
 "registry-mirrors": ["https://docker.io.registry.lingnet/"],
  "insecure-registries" : [
    "docker.io.registry.lingnet",
    "registry.lingnet"
  ]
```

### Using Website-Local-Proxy
Rather than remembering localhost port values below, you can clone and run [website-local-proxy](https://github.com/keymanapp/website-local-proxy).

Refer to the [port lookup table](#port-lookup-table) to access the local websites at
http://*keyman.com.localhost

## Builder BASH Script Actions

#### Stop the Docker container
1. Run `./build.sh stop`

This stops the Docker container for the site.

#### Build the Docker image
1. Run `./build.sh build`

This downloads and builds the Docker images needed for the site.

#### Configure
1. Run `./build.sh configure`

This step typically downloads _common/ website files from [shared-sites](https://github.com/keymanapp/shared-sites/tree/main/_common).

#### Start the Docker container
1. Run `./build.sh start`

This maps the local directory to the the Docker image.

For sites that use Composer dependencies, this step also creates a link in the Docker image from /var/www/vendor/ to /var/www/html/vendor.
The link file also appears locally.

Some sites have assets in `/cdn/dev/` used to generate CDN in `/dev/deploy/`. To avoid confusion during development, you can skip generating CDN with `./build.sh start --debug`.

##### Port lookup table
After this, you can access the website at the following ports:

| Website        |          URL          |  with website-local-proxy running |
|----------------|-----------------------|-----------------------------------|
| keyman.com     | http://localhost:8053 | http://keyman.com.localhost       |
| s.keyman.com   | http://localhost:8054 | http://s.keyman.com.localhost     |
| help.keyman    | http://localhost:8055 | http://help.keyman.com.localhost  |
| keymanweb.com  | http://localhost:8057 | http://keymanweb.com.localhost    |
|                                  |                                       | http://web.keyman.com.localhost   |
| api.keyman.com | http://localhost:8058 | http://api.keyman.com.localhost   |

#### Remove the Docker container and image
1. Run `./build.sh clean`.

#### Running tests
You might need to install the broken-link-checker first

`npm install broken-link-checker`

Checks for broken links
1. Run `./build.sh test`

---------

## Kubernetes Deployment
For production, the websites are deployed with Kubernetes.

Note: the following section is outdated as it's replaced with Rancher/Fleet.

### How to run help.keyman.com locally with Docker Desktop's Kubernetes singlenode cluster

For testing Kubernetes deployment, there are yaml files under the corresponding website's repo: `/resources/kubectl`, that cover local developer testing.

### Pre-requisites
On the host machine, install [Docker](https://docs.docker.com/get-docker/), then enable Kubernetes in the settings. Ensure you have built a help-keyman-app Docker image, and either tag it `docker.dallas.languagetechnology.org/keyman/help-keyman-app` or modify the `app-php` containers `image:` value to match you local copy's name.

### Deploying to a desktop cluster
To deploy the dev version to the cluster do the following:
1. Ensure your `kubectl` context is set to `docker-desktop`, though the Docker Desktop systray icon or by running:
```bash
$> kubectl config use-context docker-desktop
```
2. Create a keyman namespace if it does not already exist:
```bash
$> kubectl create ns keyman
```
3. Apply the configs for the resources and start the pod:
```bash
$> kubectl --namespace keyman apply \
       -f resources/kubectl/help-kubectl-dev.yaml \
       -f resources/kubectl/help-kubectl.yaml
```
### Testing the site and `/api/deploy` webhook endpoint
The site can be reached on http://localhost:30080/ via web browser, and the deploy api is on http://localhost:30900/api/deploy, and can be activated like so:
```bash
$> curl -v --request POST \
    -H "Content-Type: application/json" \
    -H "X-Hub-Signature-256: sha256=49af8531106a369bfee369f91dadec597e8ea3992ec2802bbe655be0ece17f15" \
    --data '{"action":"push","ref":"refs/heads/staging"}' \
    http://localhost:30900/api/deploy
```
This simulates enough of a GitHub webhook push event to pass validation on the responder.

### Clean up after testing

To remove the k8s pod and resources, and delete everything do:
```bash
$> kubectl --namespace=keyman delete {pod,cm,svc,secret,pvc}/help-keyman-com
```
Or just delete the pod and keep the resources for further testing:
```bash
$> kubectl --namespace=keyman delete pod/help-keyman-com
```
