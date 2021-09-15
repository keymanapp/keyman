# Build textinputsource for multiple architectures
textinputsource: textinputsource.x86_64 textinputsource.arm64
	lipo -create -output $@ $<

textinputsource.x86_64: main.c
	xcrun -sdk macosx $(CC) $< -framework Carbon -o $@ -target x86_64-apple-macos10.12

textinputsource.arm64: main.c
	xcrun -sdk macosx $(CC) $< -framework Carbon -o $@ -target arm64-apple-macos11
