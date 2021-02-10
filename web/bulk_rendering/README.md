# Bulk renderer

This renderer loads all the cloud keyboards from api.keyman.com and renders each of them to the document, saving each render as an inline (data-url) PNG for easy comparison.

## Using the bulk renderer

1. `build.sh` to build the renderer (must have already built KeymanWeb in ../source/, `./build.sh -debug`).
2. Open `index.html` (via http, not via file, as otherwise fonts will not be accessible).
3. If you want a touch layout, use Developer Tools to select the appropriate emulation and then reload the page.
3. Choose whether you wish to render all layers or just the default layer for each keyboard.
4. If desired, filter out keyboards by id (regex).
5. Run the render.
6. Save the result to a .html file, either before.html or after.html.
7. When swapping versions, don't forget to rebuild.

## compare.html

Compare allows you to load two runs of the renderer side-by-side (simply save the run to either before.html or after.html in this folder). It defaults to a width of 20% which you might need to tweak depending on the size of your display and the width of the OSK images.
