import React from 'react';
import './App.css';

const vsCode = (global as any).acquireVsCodeApi();

function Keyboard() {
  const [kmxPlus, setKmxPlus] = React.useState({loaded: false, text: null});

  window.addEventListener('message', event => {
    const message = event.data;
    switch (message.type) {
        case 'update':
            const { kmxPlus, text } = message;
            console.log('Got Data');
            setKmxPlus({ loaded: true, text });
            break;
        default:
            console.error(`Unknown message ${message.type}`);
    }
  });

  const loaded = (kmxPlus as any)?.loaded;

  if (!loaded) {
    return (
      <div>
        <h4>LDML Keyboard</h4>
        <i>no KMX+ yet</i>
      </div>
    );
  }

  return (
    <div>
      <h4>LDML Keyboard</h4>
      <pre>{ kmxPlus?.text }</pre>
    </div>
  );
}

function App() {
  return (
    <div className="App">
      <h4>App</h4>
      <Keyboard />
    </div>
  );
}

export default App;
