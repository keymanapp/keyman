import JSZip from 'jszip';

const startVersion = 1;
const pdbVersion = 163;
const revertedVersion = 171;

// get the setup-redist.zip file, look at byte size of setup.exe
// https://downloads.keyman.com/windows/alpha/19.0.1/setup-redist.zip
// 19.0.1 = 4,014,080, known-good

const jszip = JSZip();

let minVersion = startVersion, maxVersion = pdbVersion - 1;
while(minVersion < maxVersion) {
  let version = Math.round((maxVersion - minVersion) / 2) + minVersion;
  const path = `https://downloads.keyman.com/windows/alpha/19.0.${version}/setup-redist.zip`;
  console.log('Downloading '+path);
  const http = await fetch(path);
  const blob = await (await http.blob()).arrayBuffer();
  console.log('  Examining zip');
  const zip = await jszip.loadAsync(blob);
  const file = zip.file('setup-redist.exe');
  const sz = file._data.uncompressedSize;
  if(sz % 512 == 0) {
    console.log('  File size is '+sz+', multiple of 512')
    minVersion = version;
  } else {
    console.log('  File size is '+sz+', NOT multiple of 512!');
    maxVersion = version;
  }
}

