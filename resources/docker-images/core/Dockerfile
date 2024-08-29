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

# Install NVM
RUN NVM_RELEASE=$(curl -s https://api.github.com/repos/nvm-sh/nvm/releases/latest | grep tag_name | cut -d : -f 2 | cut -d '"' -f 2) && \
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_RELEASE}/install.sh | bash
RUN <<EOF cat >> /usr/bin/bashwrapper
PATH=/home/build/.keyman/node:\$PATH
export NVM_DIR="$HOME/.nvm"
. /home/build/.nvm/nvm.sh
EOF

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
