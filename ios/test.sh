# Allows for easy re-running against different device specs if desired.
function run_test() {
  if [ "$#" -lt 2 ]; then
    echo "Error in test-run configuration:  run_test was only given $# arguments, needs 2"
    return
  fi

  DEVICE=$1
  VERSION=$2

  # a bit of thanks to https://stackoverflow.com/a/37971495.
  xcodebuild \
    -workspace keymanios.xcworkspace \
    -scheme KeymanEngine \
    -sdk iphonesimulator \
    -destination "platform=iOS Simulator,name=$DEVICE,OS=$VERSION" \
    -only-testing KeymanEngineTests \
    test
}

# Example use.  It's a start.
run_test "iPhone SE" "13.2.2"