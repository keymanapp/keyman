# Copyright (c) 2024 SIL International. All rights reserved.
# ARGS used in this file:
# - ARG BASE_VERSION=default
# - ARG REQUIRED_NODE_VERSION=18

ARG BASE_VERSION=default
FROM keymanapp/keyman-base-ci:${BASE_VERSION}

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

# Pre-install emscripten
USER build
ARG REQUIRED_EMSCRIPTEN_VERSION=1.0
RUN echo "Installing emscripten version ${REQUIRED_EMSCRIPTEN_VERSION}" && \
  export EMSDK_KEEP_DOWNLOADS=1 && \
  cd /home/build/ && \
  git clone https://github.com/emscripten-core/emsdk.git && \
  cd emsdk && \
  ./emsdk install ${REQUIRED_EMSCRIPTEN_VERSION} && \
  ./emsdk activate ${REQUIRED_EMSCRIPTEN_VERSION}
USER root
RUN echo "export EMSCRIPTEN_BASE=/home/build/emsdk/upstream/emscripten" >> /usr/bin/bashwrapper && \
  echo "export KEYMAN_USE_EMSDK=1" >> /usr/bin/bashwrapper

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

# Pre-install node
RUN export NVM_DIR="/home/build/.nvm" && \
  . /home/build/.nvm/nvm.sh && \
  cd /home/build/emsdk/upstream/emscripten && \
  npm install

VOLUME /home/build/build
WORKDIR /home/build/build

ENTRYPOINT [ "/usr/bin/bashwrapper" ]
