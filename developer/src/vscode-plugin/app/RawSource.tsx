/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * the Raw Source view
 */

import React from 'react';
import { Button } from 'antd';
/** Ant's Card had an import problem, so we use this workaround */
import { FixedCard as Card } from './FixedCard.js';


/** The "show raw source" panel */
export function RawSource({ text }) {
    const [shown, setShown] = React.useState(false);
    function show() {
      setShown(true);
    }
    if (!text) return;
    if (!shown) return (
      <Card title="Raw XML Source" >
        <Button type="primary" onClick={() => setShown(true)}>Show</Button>
      </Card>
    );
    return (
      <Card title="Raw XML Source" bordered={true}>
        <Button type="primary" onClick={() => setShown(false)}>Hide</Button>
        <pre className="code">{text}</pre>
      </Card>
    );
  }
