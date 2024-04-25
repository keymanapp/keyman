# Copyright (c) 2024 SIL International. All rights reserved.

ARG BASE_VERSION
FROM --platform=amd64 keymanapp/keyman-base-ci:${BASE_VERSION}

LABEL org.opencontainers.image.authors="SIL International."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman for Web Build Image"

USER root
RUN apt-get install -qy git jq xvfb xserver-xephyr metacity

COPY run-tests.sh /usr/bin/run-tests.sh

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

# Keyman Web
RUN curl --output google-chrome-stable_current_amd64.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
  apt-get -qy install ./google-chrome-stable_current_amd64.deb && \
  rm google-chrome-stable_current_amd64.deb && \
  echo "export CHROME_BIN=/opt/google/chrome/chrome" >> /usr/bin/bashwrapper

RUN <<EOF cat > /etc/apt/preferences.d/mozilla
Package: *
Pin: origin packages.mozilla.org
Pin-Priority: 1000
EOF
RUN curl https://packages.mozilla.org/apt/repo-signing-key.gpg > /etc/apt/keyrings/packages.mozilla.org.asc && \
  echo "deb [signed-by=/etc/apt/keyrings/packages.mozilla.org.asc] https://packages.mozilla.org/apt mozilla main" >> /etc/apt/sources.list.d/mozilla.list && \
  apt-get update && \
  apt-get -qy install firefox && \
  echo "export FIREFOX_BIN=/usr/bin/firefox" >> /usr/bin/bashwrapper

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
