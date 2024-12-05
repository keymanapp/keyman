import React from 'react';
import './App.css';

let a : number = 3;

function count() : number {
  return (++a);
}

function App() {
  return (
    <div className="App">
      Hello world!! 2+2 = ${count()}
    </div>
  );
}

export default App;
