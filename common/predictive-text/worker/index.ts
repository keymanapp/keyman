// Dummy module for getting environment setup.
let EXPORTS = {
    hello() {
        return 'hello';
    }
}

// The technique below allows code to work both in-browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = EXPORTS;
} else {
    //@ts-ignore
    window.worker = EXPORTS;
}