# Copyright (c) 2024 SIL Global. All rights reserved.

ARG BASE_VERSION=default
FROM keymanapp/keyman-base-ci:${BASE_VERSION}
LABEL org.opencontainers.image.authors="SIL Global."
LABEL org.opencontainers.image.url="https://github.com/keymanapp/keyman.git"
LABEL org.opencontainers.image.title="Keyman Android Build Image"

# Keyman for Android
SHELL ["/bin/bash", "-c"]

# Starting with Ubuntu 24.04 sdkmanager is no longer available, instead
# a version dependent package allows to install the cmdline tools
ARG JAVA_VERSION=11
RUN <<EOF
  OS_VER=$(lsb_release -r -s 2>/dev/null)
  echo "OS_VER=${OS_VER}"
  if (( ${OS_VER%%.*} > 22 )); then
    PKG_SDKMANAGER=google-android-cmdline-tools-13.0-installer
    DIR_SDK=/usr/lib/android-sdk
  else
    PKG_SDKMANAGER=sdkmanager
    DIR_SDK=/opt/android-sdk
  fi
  apt-get -q -y install gradle maven pandoc $PKG_SDKMANAGER jq openjdk-${JAVA_VERSION}-jdk
  sdkmanager platform-tools
  yes | sdkmanager --licenses
  chown -R build:build $DIR_SDK
  echo "export ANDROID_HOME=$DIR_SDK" >> /usr/bin/bashwrapper
  echo "export JAVA_HOME_${JAVA_VERSION}=/usr/lib/jvm/java-${JAVA_VERSION}-openjdk-amd64" >> /usr/bin/bashwrapper
EOF

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

# Pre-install gradle. This will put files in ~/.gradle which will speed up builds.
RUN mkdir -p $HOME/tmp/gradle/wrapper && \
  # KMEA uses gradle-7.6.4-bin
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.jar https://raw.githubusercontent.com/keymanapp/keyman/master/android/KMEA/gradle/wrapper/gradle-wrapper.jar && \
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.properties https://raw.githubusercontent.com/keymanapp/keyman/master/android/KMEA/gradle/wrapper/gradle-wrapper.properties && \
  curl --location --output $HOME/tmp/gradlew https://raw.githubusercontent.com/keymanapp/keyman/master/android/KMEA/gradlew && \
  chmod +x $HOME/tmp/gradlew && \
  $HOME/tmp/gradlew --quiet && \
  # Some projects use gradle-7.6.4-all, so we pre-install that as well
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.jar https://raw.githubusercontent.com/keymanapp/keyman/master/android/Samples/KMSample1/gradle/wrapper/gradle-wrapper.jar && \
  curl --location --output $HOME/tmp/gradle/wrapper/gradle-wrapper.properties https://raw.githubusercontent.com/keymanapp/keyman/master/android/Samples/KMSample1/gradle/wrapper/gradle-wrapper.properties && \
  curl --location --output $HOME/tmp/gradlew https://raw.githubusercontent.com/keymanapp/keyman/master/android/Samples/KMSample1/gradlew && \
  chmod +x $HOME/tmp/gradlew && \
  $HOME/tmp/gradlew --quiet && \
  rm -rf $HOME/tmp

ENTRYPOINT [ "/usr/bin/bashwrapper" ]
