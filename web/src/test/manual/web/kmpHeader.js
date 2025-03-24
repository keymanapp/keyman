
/*
  This script allows its host page to add a keyboard via KMP file.

  Expectations for the host page:
  - `<p> <input type="file" id="package-file-select" accept=".kmp"/> </p>`
  - <script src="../../../../build/tools/testing/bulk_rendering/lib/zip.min.js" type="application/javascript"></script>
*/

window.addEventListener('load', () => {
  const packageSelector = document.getElementById('package-file-select');
  packageSelector.addEventListener('change', async (ev) => {
    let file = ev.target.files[0];
  
    if(!file) {
      // The user probably canceled file selection; abort.
      return;
    }
  
    // Load the package.
    // https://gildas-lormeau.github.io/zip.js/api/classes/ZipReader.html
    const packageReader = new zip.ZipReader(new zip.BlobReader(file));
    const packageEntries = await packageReader.getEntries();
  
    if(packageEntries.length == 0) {
      alert('Could not decompress the KMP; package loading failed.');
      return;
    } else {
      const metadataFile = packageEntries.find((entry) => entry.filename == 'kmp.json');
      if(!metadataFile) {
        alert('Could not extract kmp.json; aborting');
      }
      const rawMetadata = await metadataFile.getData(new zip.TextWriter());
  
      // Full kmp.json load!
      const packageMetadata = JSON.parse(rawMetadata);
      const keyboards = packageMetadata.keyboards
  
      for(let keyboard of keyboards) {
        const keyboardFile = `${keyboard.id}.js`;
        const displayFont = keyboard.displayFont;
        const oskFont = keyboard.oskFont;
  
        async function dataFor(filename, type) {
          if(type == 'font') {
            const ext = filename.substring(filename.lastIndexOf('.')+1);
            type = type + '/' + ext;
          }
          const rawEntry = packageEntries.find((entry) => entry.filename == filename);
          if(!rawEntry) {
            alert(`Could not extract ${filename} from the keyboard package.`);
            return null;
          }
          return await rawEntry.getData(new zip.Data64URIWriter(type));
        }
  
        const kbdData = await dataFor(keyboardFile, 'text/javascript');
        const dFontData = displayFont ? await dataFor(displayFont, 'font') : null;
        const oFontData = oskFont ? await dataFor(oskFont, 'font') : null;
  
        keyboard.languages.forEach((language) => {
          if(dFontData) {
            language.font = {
              family: keyboard.id,
              source: [dFontData]
            };
          }
  
          if(oFontData) {
            language.oskFont = {
              family: keyboard.id,
              source: [oFontData]
            };
          }
        })
  
        const stub = {
          id: keyboard.id,
          name: keyboard.name,
          languages: keyboard.languages,
          filename: kbdData
        };
  
        // Load 'er up!
        keyman.addKeyboards(stub).then(() => document.getElementById('ta1').focus());
      }
    }
  });  
});