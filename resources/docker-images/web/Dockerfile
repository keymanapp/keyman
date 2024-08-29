# Copyright (c) 2024 SIL International. All rights reserved.
# ARGS used in this file:
# - ARG BASE_VERSION=default
# - ARG REQUIRED_NODE_VERSION=18

ARG BASE_VERSION=default
FROM keymanapp/keyman-base-ci:${BASE_VERSION}

LABEL org.opencontainers.image.authors="SIL International."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman for Web Build Image"

USER root
RUN apt-get install -qy git jq xvfb xserver-xephyr metacity libevent-2.1-7t64

COPY run-tests.sh /usr/bin/run-tests.sh

# Install NVM
RUN NVM_RELEASE=$(curl -s https://api.github.com/repos/nvm-sh/nvm/releases/latest | grep tag_name | cut -d : -f 2 | cut -d '"' -f 2) && \
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_RELEASE}/install.sh | bash
RUN <<EOF cat >> /usr/bin/bashwrapper
PATH=/home/build/.keyman/node:\$PATH
export NVM_DIR="$HOME/.nvm"
. /home/build/.nvm/nvm.sh
EOF

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

# Pre-install node
ARG REQUIRED_NODE_VERSION=18
RUN echo "HOME=\${HOME}; REQUIRED_NODE_VERSION=${REQUIRED_NODE_VERSION}" && \
  export NVM_DIR="/home/build/.nvm" && \
  . /home/build/.nvm/nvm.sh && \
  nvm install "${REQUIRED_NODE_VERSION}" && \
  nvm use "${REQUIRED_NODE_VERSION}"

VOLUME /home/build/build
WORKDIR /home/build/build

ENTRYPOINT [ "/usr/bin/bashwrapper" ]
