# Copyright (c) 2024 SIL International. All rights reserved.

ARG BASE_VERSION
FROM --platform=amd64 keymanapp/keyman-base-ci:${BASE_VERSION}

LABEL org.opencontainers.image.authors="SIL International."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Core Build Image"

USER root
RUN apt-get install -qy git jq llvm meson pkgconf \
  xvfb xserver-xephyr metacity

#### TMP until we properly figure out the dependencies needed for Core
#### We should not need to install /tmp/control
# Install dependencies
ADD control /tmp/control
RUN apt-get install -qy python3 python3-setuptools python3-coverage \
  devscripts equivs libdatetime-perl meson pkgconf lcov gcovr xvfb \
  xserver-xephyr metacity mutter dbus-x11 weston xwayland && \
  (yes | mk-build-deps --install /tmp/control) || true && \
  rm /tmp/control
#### TMP END

# Install node
ARG NODE_MAJOR
RUN set -eu; \
  NODE_VERSION=$(curl -sSL https://unofficial-builds.nodejs.org/download/release/ | cut -d'>' -f2 | cut -d'/' -f1 | grep v${NODE_MAJOR} | sort -V | tail -1) && \
  echo "Installing node version ${NODE_VERSION}" && \
  curl -fsSLO --compressed "https://unofficial-builds.nodejs.org/download/release/${NODE_VERSION}/node-${NODE_VERSION}-linux-x64-glibc-217.tar.xz" && \
  tar -xJf "node-${NODE_VERSION}-linux-x64-glibc-217.tar.xz" -C /usr/local --strip-components=1 --no-same-owner && \
  ln -s /usr/local/bin/node /usr/local/bin/nodejs

# Install emscripten
ARG EMSCRIPTEN_VERSION
RUN echo "Installing emscripten version ${EMSCRIPTEN_VERSION}" && \
  cd /usr/share && \
  git clone https://github.com/emscripten-core/emsdk.git && \
  cd emsdk && \
  ./emsdk install ${EMSCRIPTEN_VERSION} && \
  ./emsdk activate ${EMSCRIPTEN_VERSION} && \
  echo "#!/bin/bash" > /usr/bin/bashwrapper && \
  echo "export EMSCRIPTEN_BASE=/usr/share/emsdk/upstream/emscripten" >> /usr/bin/bashwrapper

# Finish bashwrapper script and adjust permissions
RUN <<EOF cat >> /usr/bin/bashwrapper

if [[ "\$@" =~ test ]] && [ -f /usr/bin/run-tests.sh ]; then
  /usr/bin/run-tests.sh "\${@:-bash}"
else
  "\${@:-bash}"
fi
EOF

RUN chmod +x /usr/bin/bashwrapper && \
  chown -R build:build $HOME

# now, switch to build user
USER build

VOLUME /home/build/build
WORKDIR /home/build/build

ENTRYPOINT [ "/usr/bin/bashwrapper" ]
