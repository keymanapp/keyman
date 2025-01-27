/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * This is top level JS code used to bootstrap the React app
 */

import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';

/** find the id="root" element from ./index.html */
const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
