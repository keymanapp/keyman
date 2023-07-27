# Copyright (c) 2022-2023 SIL International. All rights reserved.
#
# builder image for a linux build
# see ../docs/build/linux-ubuntu.md

FROM --platform=amd64 ubuntu:latest
LABEL org.opencontainers.image.authors="SIL International."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Linux Build Image"

# We will switch to a build user after some installation
USER root
ENV HOME /home/build
RUN useradd -c "Build user" --home-dir $HOME --create-home --shell /usr/bin/bash build
VOLUME /home/build/build
WORKDIR /home/build/build
ENV DEBIAN_FRONTEND noninteractive
ENV DEBIAN_PRIORITY critical
ENV DEBCONF_NOWARNINGS yes

# Update to the latest
RUN apt-get -q -y update &&  \
  apt-get -q -y install devscripts equivs meson python3 python3-setuptools software-properties-common curl && \
  add-apt-repository ppa:keymanapp/keyman && \
  add-apt-repository ppa:keymanapp/keyman-alpha
RUN apt-get -q -y update &&  \
  apt-get -q -y upgrade

# Install dependencies
ADD debian/control /tmp/control
# Answer 'yes' to install questions
RUN (yes | mk-build-deps --install /tmp/control) || true

# Install Node
RUN curl -sL https://deb.nodesource.com/setup_18.x | bash
RUN apt-get -q -y install nodejs

# Install emscripten
RUN cd /usr/share && \
  git clone https://github.com/emscripten-core/emsdk.git && \
  cd emsdk && \
  ./emsdk install latest && \
  ./emsdk activate latest && \
  echo "#!/bin/bash" > /usr/bin/wrapper && \
  echo "export EMSCRIPTEN_BASE=/usr/share/emsdk/upstream/emscripten" >> /usr/bin/wrapper && \
  echo "bash -c \"\${@:-bash}\"">> /usr/bin/wrapper && \
  chmod +x /usr/bin/wrapper

# now, switch to build user
USER build
