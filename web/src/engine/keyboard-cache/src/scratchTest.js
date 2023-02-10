import CloudNodeRequester from '../../../../build/engine/keyboard-cache/obj/nodeCloudRequester.js';
import CloudQueryEngine from '../../../../build/engine/keyboard-cache/obj/cloud/queryEngine.js';

let requester = new CloudNodeRequester();
let queryEngine = new CloudQueryEngine(requester);

requester.link(queryEngine);

// We may now query.

let promise = queryEngine.fetchCloudStubs(["sil_cameroon_azerty@pny", "khmer_angkor"]);
let ret = await promise;

console.log(ret);