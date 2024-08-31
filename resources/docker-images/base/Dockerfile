# Copyright (c) 2024 SIL International. All rights reserved.

ARG UBUNTU_VERSION=latest
FROM ubuntu:${UBUNTU_VERSION}

LABEL org.opencontainers.image.authors="SIL International."
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
  add-apt-repository ppa:keymanapp/keyman && \
  add-apt-repository ppa:keymanapp/keyman-alpha
RUN apt-get -q -y update &&  \
  apt-get -q -y upgrade

# Allow build user to use `sudo`
RUN echo "build ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN <<EOF cat > /usr/bin/bashwrapper
#!/bin/bash
export KEYMAN_USE_NVM=1
EOF
