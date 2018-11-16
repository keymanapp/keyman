// Dummy module for getting environment setup.
class MyWorker {
    public hello() {
        return 'hello';
    }
}

// The technique below allows code to work both in-browser and in Node.
if(typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = new MyWorker();
} else {
    //@ts-ignore
    window.worker = new MyWorker();
}