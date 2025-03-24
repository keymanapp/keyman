# Keyman is copyright (C) SIL Global. MIT License.
#
# ARGS used in this file:
# - ARG BASE_VERSION=default
# - ARG REQUIRED_EMSCRIPTEN_VERSION=unset

ARG BASE_VERSION=default
FROM keymanapp/keyman-base-ci:${BASE_VERSION}

LABEL org.opencontainers.image.authors="SIL Global."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman for Web Build Image"

USER root
RUN apt-get update && \
  apt-get install -qy git jq xvfb xserver-xephyr metacity
# For playwright:
RUN apt-get install -qy  libevent-2.1-7t64 libxslt1.1 libwoff1 libvpx9 \
  libgstreamer-plugins-bad1.0-0 libwebpdemux2 libharfbuzz-icu0 \
  libenchant-2-2 libsecret-1-0 libhyphen0 libmanette-0.2-0 libflite1 \
  gstreamer1.0-libav

COPY run-tests.sh /usr/bin/run-tests.sh

# Pre-install emscripten
USER build
ARG REQUIRED_EMSCRIPTEN_VERSION=unset
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
