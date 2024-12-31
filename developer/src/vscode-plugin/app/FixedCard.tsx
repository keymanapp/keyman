/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Visual editor app
 */

import { Card } from 'antd';
import React from 'react';

/** workaround Card TS linkage (and also make the border always set) */
export function FixedCard({ title, bordered, children } : { title: string, bordered?: boolean, children?: any }) {
    return (
      // TODO-EPIC-LDML: why the red squiggle on the next line?
      <Card title={title} bordered={bordered}>
        {children}
      </Card>
    );
  }
