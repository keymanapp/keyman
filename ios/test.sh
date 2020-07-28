if [ "$#" -ge 1 ]; then
  DEVICE=$1
else
  DEVICE="iPhone X"
fi

echo "Running tests on simulated $DEVICE, iOS $OS_VERSION"

# Allows for easy re-running against different device specs if desired.
function run_test() {
  if [ "$#" -lt 1 ]; then
    echo "Error in test-run configuration:  run_test was given no arguments, needs 1"
    return
  fi

  DEVICE=$1

  # a bit of thanks to https://stackoverflow.com/a/37971495.
  #
  # Note: we don't _have_ to specify the OS version; the Simulator will
  # auto-pick what's most current if we don't.  Makes CI maintenance
  # much less problematic.
  xcodebuild \
    -workspace keymanios.xcworkspace \
    -scheme Keyman \
    -sdk iphonesimulator \
    -destination "platform=iOS Simulator,name=$DEVICE" \
    -only-testing KeymanEngineTests \
    SKIP_UNSTABLE_TESTS=true \
    test
}

# Example use.  It's a start.
run_test "$DEVICE"