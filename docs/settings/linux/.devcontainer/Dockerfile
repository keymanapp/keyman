# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.2/containers/ubuntu/.devcontainer/base.Dockerfile

# VARIANT: Ubuntu version: ubuntu-22.04, jammy, ubuntu-20.04, focal, ubuntu-18.04, bionic
# Note: on local arm64/Apple Silicon use ubuntu-22.04 or ubuntu-18.04
ARG VARIANT="jammy"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN export DEBIAN_FRONTEND=noninteractive \
    && apt-get update \
    && apt-get -y install --no-install-recommends software-properties-common \
        cdbs debhelper libx11-dev autotools-dev build-essential \
        dh-autoreconf flex bison libibus-1.0-dev python3-setuptools meson \
        libjson-glib-dev libgtk-3-dev libxml2-utils help2man python3-lxml \
        python3-magic python3-numpy python3-pil python3-pip python3-qrcode \
        python3-requests python3-requests-cache python3 python3-gi dconf-cli \
        dconf-editor cargo python3-dbus gir1.2-webkit2-4.1 ibus libglib2.0-bin \
        liblocale-gettext-perl xvfb xserver-xephyr metacity mutter \
    && sudo add-apt-repository --yes ppa:keymanapp/keyman \
    && sudo apt upgrade --yes
