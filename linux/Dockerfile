# Copyright (c) 2022-2023 SIL Global. All rights reserved.
#
# builder image for a linux build
# see ../docs/build/linux-ubuntu.md

ARG OS_VERSION=latest
ARG OS_PLATFORM=amd64

FROM --platform=${OS_PLATFORM} ubuntu:${OS_VERSION}
LABEL org.opencontainers.image.authors="SIL Global."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Linux Build Image"

# We will switch to a build user after some installation
USER root
ENV HOME /home/build
RUN useradd -c "Build user" --home-dir $HOME --create-home --shell /usr/bin/bashwrapper build
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
RUN (yes | mk-build-deps --install /tmp/control) || true && \
  rm /tmp/control

# Install Node
ARG NODE_MAJOR=18
RUN apt-get install -q -y ca-certificates curl gnupg && curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_"${NODE_MAJOR}".x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list && apt-get update && apt-get install nodejs -y

ARG EMSCRIPTEN_VERSION=3.1.44
# Install emscripten
RUN cd /usr/share && \
  git clone https://github.com/emscripten-core/emsdk.git && \
  cd emsdk && \
  ./emsdk install ${EMSCRIPTEN_VERSION} && \
  ./emsdk activate ${EMSCRIPTEN_VERSION} && \
  echo "#!/bin/bash" > /usr/bin/bashwrapper && \
  echo "export EMSCRIPTEN_BASE=/usr/share/emsdk/upstream/emscripten" >> /usr/bin/bashwrapper

# Keyman Web
RUN curl --output google-chrome-stable_current_amd64.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
  apt-get -q -y install ./google-chrome-stable_current_amd64.deb && \
  rm google-chrome-stable_current_amd64.deb && \
  echo "export CHROME_BIN=/opt/google/chrome/chrome" >> /usr/bin/bashwrapper

# Keyman for Android
RUN apt-get -q -y install gradle maven pandoc sdkmanager jq && \
  sdkmanager platform-tools && \
  yes | sdkmanager --licenses && \
  chown -R build:build /opt/android-sdk/ && \
  echo "export ANDROID_HOME=/opt/android-sdk" >> /usr/bin/bashwrapper && \
  echo "export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64" >> /usr/bin/bashwrapper

# Finish bashwrapper script and adjust permissions
RUN echo "\${@:-bash}" >> /usr/bin/bashwrapper && \
  chmod +x /usr/bin/bashwrapper && \
  chown -R build:build $HOME

# now, switch to build user
USER build

# Pre-install gradle. This will put files in ~/.gradle which will speed up builds.
RUN mkdir -p $HOME/tmp/gradle/wrapper && \
  # KMEA uses gradle-7.5.1-bin
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.jar https://raw.githubusercontent.com/keymanapp/keyman/master/android/KMEA/gradle/wrapper/gradle-wrapper.jar && \
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.properties https://raw.githubusercontent.com/keymanapp/keyman/master/android/KMEA/gradle/wrapper/gradle-wrapper.properties && \
  curl --location --output $HOME/tmp/gradlew https://raw.githubusercontent.com/keymanapp/keyman/master/android/KMEA/gradlew && \
  chmod +x $HOME/tmp/gradlew && \
  $HOME/tmp/gradlew --quiet && \
  # Some projects use gradle-7.5.1-all, so we pre-install that as well
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.jar https://raw.githubusercontent.com/keymanapp/keyman/master/android/Samples/KMSample1/gradle/wrapper/gradle-wrapper.jar && \
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.properties https://raw.githubusercontent.com/keymanapp/keyman/master/android/Samples/KMSample1/gradle/wrapper/gradle-wrapper.properties && \
  curl --location --output $HOME/tmp/gradlew https://raw.githubusercontent.com/keymanapp/keyman/master/android/Samples/KMSample1/gradlew && \
  chmod +x $HOME/tmp/gradlew && \
  $HOME/tmp/gradlew --quiet && \
  rm -rf $HOME/tmp

ENTRYPOINT [ "/usr/bin/bashwrapper" ]
