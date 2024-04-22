"use strict";

/* Websocket listening for updated resource notifications */

WebSockHop.disable.mobile = false;
if(WebSockHop.isAvailable()) {
  const ws = new WebSockHop((window.location.protocol == 'http:' ? 'ws://' : 'wss://')+window.location.host);

  ws.formatter = new WebSockHop.StringFormatter();
  ws.formatter.pingMessage = 'ping';
  ws.formatter.handlePong = function (message) {
    return message == 'pong';
  };

  ws.on('message', (message) => {
    console.log('WebSocket: '+message);
    if(message == 'refresh') {
      checkKeyboardsAndModels(true);
      checkPackages();
    }
  });
}
