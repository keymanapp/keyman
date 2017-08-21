#!/bin/sh
# Build KMAPro

display_usage ( ) {
    echo "build.sh [-no-daemon]"
    echo
    echo "Build Keyman for Android"
    echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
    exit 1
}

echo Build KMAPro

NO_DAEMON=false

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -no-daemon)
            NO_DAEMON=true
            ;;
        -h|-?)
            display_usage
            ;;
    esac
    shift # past argument
done

echo
echo "NO_DAEMON: $NO_DAEMON"
echo

if [ "$NO_DAEMON" = true ]; then
  DAEMON_FLAG=--no-daemon
else
  DAEMON_FLAG=
fi

./gradlew $DAEMON_FLAG clean build
