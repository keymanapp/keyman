# source $HOME/.cargo/env
#echo 'export PATH="/usr/local/opt/openjdk@8/bin:$PATH"' >> ~/.zshrc
#echo 'export PATH="/usr/local/opt/openjdk@8/bin:$PATH"' >> ~/.bashrc
export ANT_HOME=/usr/local/opt/ant
export MAVEN_HOME=/usr/local/opt/maven
export GRADLE_HOME=/usr/local/opt/gradle
export ANDROID_HOME=~/.android
export ANDROID_SDK_ROOT="$HOME/Library/Android/sdk"

export PATH=$ANT_HOME/bin:$PATH
export PATH=$MAVEN_HOME/bin:$PATH
export PATH=$GRADLE_HOME/bin:$PATH
export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/tools/bin:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH
export PATH=$ANDROID_HOME/build-tools/30.0.3:$PATH
export PATH="/usr/local/opt/openjdk@8/bin:$PATH"
export PATH="$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH"
export JAVA_HOME=/usr/local/opt/openjdk@8

# Python 2.7
eval "$(pyenv init --path)"
