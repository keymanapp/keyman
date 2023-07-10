# Copyright (c) 2022 SIL International. All rights reserved.
#
# builder image for a linux build
# see ../docs/build/linux-ubuntu.md

FROM --platform=amd64 ubuntu:latest
LABEL org.opencontainers.image.authors="SIL International."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Linux Build Image"
# We will switch to a build user after some installation
USER root
RUN useradd -c "Build user" -d $HOME -m build
ENV HOME /home/build
VOLUME /home/build
WORKDIR /home/build
ENV DEBIAN_FRONTEND noninteractive
ENV DEBIAN_PRIORITY critical
ENV DEBCONF_NOWARNINGS yes

# Update to the latest
RUN apt-get -q -y update &&  \
  apt-get -q -y install devscripts equivs meson python3 python3-setuptools software-properties-common && \
  add-apt-repository ppa:keymanapp/keyman && \
  add-apt-repository ppa:keymanapp/keyman-alpha && \
  apt-get -q -y upgrade

ADD debian/control /tmp/control
# Answer 'yes' to install questions
RUN (yes | mk-build-deps --install /tmp/control) || true
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash
RUN apt-get -q -y install nodejs
# now, switch to build user
USER build
