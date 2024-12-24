import React from 'react';
import './App.css';

let a : number = 3;

function count() : number {
  return (++a);
}

let kmxplus = null;
let text = null;

window.addEventListener('message', event => {
  const message = event.data;
  switch (message.command) {
      case 'update':
          kmxplus = message.kmxplus;
          text = message.text;
          break;
  }
});


function App() {
  return (
    <div className="App">
      Hello world!! 2+2 = ${count()}
    </div>
  );
}

export default App;
