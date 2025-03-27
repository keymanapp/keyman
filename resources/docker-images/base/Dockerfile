# Keyman is copyright (C) SIL Global. MIT License.

ARG DISTRO=ubuntu
ARG DISTRO_VERSION=latest
FROM ${DISTRO}:${DISTRO_VERSION}

LABEL org.opencontainers.image.authors="SIL Global."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Build Base Image"

# We will switch to a build user after some installation
USER root
ENV HOME=/home/build
RUN grep ubuntu /etc/passwd && userdel ubuntu || true && \
  rm -rf /home/ubuntu && \
  useradd -c "Build user" --uid 1000 --home-dir $HOME --create-home --shell /usr/bin/bashwrapper build

ENV DEBIAN_FRONTEND=noninteractive
ENV DEBIAN_PRIORITY=critical
ENV DEBCONF_NOWARNINGS=yes

# Update to the latest
RUN apt-get -q -y update &&  \
  apt-get -q -y install ca-certificates curl gnupg meson software-properties-common sudo && \
  if [[ "$(lsb_release -is)" == "Ubuntu" ]]; then \
    add-apt-repository ppa:keymanapp/keyman && \
    add-apt-repository ppa:keymanapp/keyman-alpha ; \
  fi
RUN apt-get -q -y update &&  \
  apt-get -q -y upgrade

# Allow build user to use `sudo`
RUN echo "build ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN <<EOF cat > /usr/bin/bashwrapper
#!/bin/bash
export KEYMAN_USE_NVM=1
export DOCKER_RUNNING=true
EOF

# Install NVM
RUN NVM_RELEASE=$(curl -s https://api.github.com/repos/nvm-sh/nvm/releases/latest | grep tag_name | cut -d : -f 2 | cut -d '"' -f 2) && \
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_RELEASE}/install.sh | bash
RUN <<EOF cat >> /usr/bin/bashwrapper
PATH=/home/build/.keyman/node:\$PATH
export NVM_DIR="$HOME/.nvm"
. /home/build/.nvm/nvm.sh
EOF

RUN chmod +x /usr/bin/bashwrapper && \
  chown -R build:build $HOME

USER build
# Pre-install node
ARG REQUIRED_NODE_VERSION=unset
RUN echo "HOME=\${HOME}; REQUIRED_NODE_VERSION=${REQUIRED_NODE_VERSION}" && \
  export NVM_DIR="/home/build/.nvm" && \
  . /home/build/.nvm/nvm.sh && \
  nvm install "${REQUIRED_NODE_VERSION}" && \
  nvm use "${REQUIRED_NODE_VERSION}"

USER root
