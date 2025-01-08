# Keyman is copyright (C) SIL Global. MIT License.

ARG BASE_VERSION=default
FROM keymanapp/keyman-base-ci:${BASE_VERSION}
LABEL org.opencontainers.image.authors="SIL Global."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Linux Build Image"

# Install dependencies
ADD control /tmp/control
# Answer 'yes' to install questions
RUN apt-get install -qy python3 python3-setuptools python3-coverage \
      devscripts equivs libdatetime-perl lcov gcovr xvfb \
      xserver-xephyr metacity mutter dbus-x11 weston xwayland && \
  (yes | mk-build-deps --install /tmp/control) || true && \
  rm /tmp/control

# Install lcov for code coverage
# Update to the latest and install packages needed for Linux coverage reporting
# and integration tests. We need at least version 2.0 of lcov. However,
# version 2.0-4 from Noble doesn't work either on Jammy. So we use
# version 2.0-1 from Mantic.
RUN LCOV_VERSION=$(dpkg -s lcov | grep Version | cut -d' ' -f2) && \
  if dpkg --compare-versions "${LCOV_VERSION}" lt 2.0; then \
    curl -sS -o /tmp/lcov.deb --location http://mirrors.kernel.org/ubuntu/pool/universe/l/lcov/lcov_2.0-1_all.deb && \
    apt-get -qy install /tmp/lcov.deb && \
    rm /tmp/lcov.deb ; \
  fi

RUN mkdir -p /var/run/1000 && \
  chown build:build /var/run/1000 && \
  echo "export XDG_RUNTIME_DIR=/var/run/1000" >> /usr/bin/bashwrapper

COPY run-tests.sh /usr/bin/run-tests.sh

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

VOLUME /home/build/build
WORKDIR /home/build/build

ENTRYPOINT [ "/usr/bin/bashwrapper" ]
