// receive a message and execute its uri
onmessage = function (e) {
  let uri = e.data.uri;
  importScripts(uri);
};
