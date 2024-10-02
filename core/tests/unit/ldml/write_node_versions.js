// Simple utility to dump process.versions to argument 1

const { argv, versions } = require('process');
const { writeFileSync } = require('fs');

const [ f ] = argv.slice(2);
writeFileSync(f, JSON.stringify(versions, null, ' '));
console.log('Wrote:', f);
