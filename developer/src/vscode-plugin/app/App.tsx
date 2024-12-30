/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Visual editor app
 */

import React from 'react';
import './App.css';
import { KMXPlus } from '@keymanapp/common-types';
import KMXPlusFile = KMXPlus.KMXPlusFile;
import { Button, Card, Input, InputNumber, Checkbox, Segmented, Skeleton, Alert } from 'antd';
import { isGapKey, layerTitle, listTitle, touchWidth, WIDTH_HARDWARE } from './utils.js';
/**
 * Used to get the VSCode global. Can only call this once.
 */
const vsCode = (global as any).acquireVsCodeApi();

/** This is used to track data as it is loaded.  */
interface LoadedData {
  /** true if we think we're loaded */
  loaded: boolean;
  /** full XML source text */
  text?: string;
  /** the entire KMXPlusFile */
  kmxPlus?: KMXPlusFile;
  /** messages serialized to strings */
  messages?: string[];
}

/** this context will have the KMXPlusFile. We won't use it until the KMXPlusFile is valid. */
const KmxPlusContext = React.createContext(undefined as unknown as KMXPlusFile)

/** The "show raw source" panel */
function RawSource({ text }) {
  const [shown, setShown] = React.useState(false);
  function show() {
    setShown(true);
  }
  if (!text) return;
  if (!shown) return (
    <Card title="Raw XML Source" bordered={true}>
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

/** An individual key in the keybag. */
function Key({ k, chosenKey, setChosenKey }: {
  k: KMXPlus.KeysKeys,
  chosenKey: string,
  setChosenKey: any, /* setter */
  key: string, /* special prop */
}) {
  const id = k.id.value;
  const chosen = (chosenKey == id);
  const className = chosen ? "Key chosenKey" : "Key unchosenKey";
  // const onClick = chosen ? () => setChosenKey(k.id.value) : setChosenKey(id);
  const onClick = () => { };
  if (k.to.value === '') {
    return (
      <i key={id} onClick={() => setChosenKey(id)} className={className} title={id}>{id}</i>
    )
  }
  return (
    <kbd key={id} onClick={() => setChosenKey(id)} className={className} title={id}>{k.to.value}</kbd>
  );
}

/** The list of keys in the keybag */
function KeyList({ keys, chosenKey, setChosenKey }:
  { keys: KMXPlus.KeysKeys[], chosenKey: string, setChosenKey: any /* setter */ }) {
  return (
    <div className="keyList">
      {keys?.map((k) => (<Key key={k.id.value} k={k} chosenKey={chosenKey} setChosenKey={setChosenKey} />))}
    </div>
  );
}

/** keybag detail editor */
function KeyDetails({ chosenKey }: { chosenKey: string }) {
  const kmxPlus = React.useContext(KmxPlusContext) as KMXPlus.KMXPlusFile;
  if (!chosenKey) return;
  const chosenKeys = kmxPlus.kmxplus.keys?.keys.filter(({ id }) => id.value == chosenKey);
  if (!chosenKeys?.length) {
    return; // no key selecte4d
  } else if (chosenKeys?.length > 1) {
    throw Error(`more than one key with id ${chosenKey}`);
  }

  const k = chosenKeys[0];

  const { flags } = k;
  const isGap = isGapKey(k);

  return (
    <Card title="Details" bordered={true}>
      <h5>Details</h5>
      <b>ID:</b> <Input value={k.id.value} /> <br />
      <b>To:</b> <Input value={k.to.value} /> <br />
      <b>Width:</b> <InputNumber min={0.1} max={99.0} step={0.1} value={k.width / 10} /> <br />
      <Checkbox checked={isGap}>Gap</Checkbox> <br />
      <p>flags? 0x{k.flags.toString(16)}</p>
    </Card>
  );
}

/** the main editor for keys */
function KeyBag() {
  const kmxPlus = React.useContext(KmxPlusContext);
  const keys = kmxPlus?.kmxplus?.keys?.keys || [];
  /** string id of selected key */
  const [chosenKey, setChosenKey] = React.useState(keys[0]?.id?.value);
  if (!kmxPlus) return; // get out
  return (
    <Card title="Key Bag" bordered={true}>
      <div className="keyListContainer">
        <KeyList keys={keys} chosenKey={chosenKey} setChosenKey={setChosenKey} />
      </div>
      <KeyDetails chosenKey={chosenKey} />
    </Card>
  );
}

/** row in the layer list */
function Row({ row }: {
  row: KMXPlus.LayrRow,
  key: string,
}) {
  return (
    <>
      {row.keys.map(({ value }) => (
        <kbd key={value}>{value}</kbd>
      ))}
      <br />
    </>
  )
}

/** single layer */
function Layer({ layer }: {
  layer: KMXPlus.LayrEntry,
  key: string,
}) {
  return (
    <>
      <h5>{layerTitle(layer)}</h5>
      {layer.rows.map((row, index) => (<Row row={row} key={index.toString()} />))}
    </>
  );
}

/** a list of layouts (i.e. a form type or width) */
function LayoutList({ curWidth, setCurWidth, list }:
  {
    curWidth: number,
    setCurWidth: any,
    list: KMXPlus.LayrList,
    key: string,
  }
) {
  const myWidth = touchWidth(list);
  const selected = (curWidth === myWidth);
  if (!selected) return; // for now: hide collapsed layouts
  return (
    <div className="layoutList">
      {list.layers.map((layer) => (<Layer key={layerTitle(layer)} layer={layer} />))}
    </div>
  );
}

/** The list of Layouts */
function KeyLayouts() {
  const kmxPlus = React.useContext(KmxPlusContext);
  const lists = [...(kmxPlus.kmxplus.layr?.lists || [])]; // copy the list
  const [curWidth, setCurWidth] = React.useState(WIDTH_HARDWARE);
  lists.sort((a, b) => touchWidth(a) - touchWidth(b)); // sort by width
  const listWidthAndTitle = lists.map((list) => [listTitle(list), touchWidth(list)]);
  const listTitles = listWidthAndTitle.map(([t]) => t); // just titles
  // search listWidthAndTitle for the width
  function findWidth(title) {
    return listWidthAndTitle.filter(([t]) => t == title)[0][1];
  }
  return (
    <div className="keyLayouts">
      <Card title="Layouts" bordered={true}>
        <Segmented<string>
          options={listTitles}
          onChange={(value) => setCurWidth(findWidth(value))} />
        {lists.map((list) => (<LayoutList key={touchWidth(list).toString()} curWidth={curWidth} setCurWidth={setCurWidth} list={list} />))}
      </Card>
    </div>
  );
}

/** this shows any error or warning messages from the compiler */
function Messages({ messages }: { messages: string[] }) {
  if (!messages) return;
  return (
    <ul>
      {messages.map((msg, key) => (<li key={key}>{msg}</li>))}
    </ul>
  );
}

/** This is the main Keyboard File object. */
function KeyboardFile() {
  const initialData: LoadedData = {
    loaded: false,
    text: undefined,
    kmxPlus: undefined,
    messages: [],
  };
  const [data, setData] = React.useState(initialData);

  window.addEventListener('message', event => {
    const message = event.data;
    switch (message.type) {
      case 'update':
        const { kmxPlus, text, messages } = message;
        console.log('Got Data');
        const newData: LoadedData = {
          loaded: true, kmxPlus, text, messages
        };
        setData(newData);
        break;
      default:
        console.error(`Unknown message ${message.type}`);
    }
  });

  const loaded = (data as any)?.loaded;

  if (!loaded) {
    return (
      <div>
        <h4>LDML Keyboard</h4>
        <Skeleton />
      </div>
    );
  }

  if (!data.kmxPlus) {
    return (
      <div>
        <Alert type="error" showIcon message="Could not read keyboard file. Correct these issues and reload the file." />
        <Messages messages={data.messages} />
      </div>
    )
  }

  return (
    <>
      <Messages messages={data.messages} />
      <KmxPlusContext.Provider value={data.kmxPlus}>
        <KeyBag />
        <KeyLayouts />
      </KmxPlusContext.Provider>
      <hr />
      <RawSource text={data?.text} />
    </>
  );
}

/** The outer App. TODO-EPIC-LDML this may be  */
function App() {
  return (
    <div className="App">
      <KeyboardFile />
    </div>
  );
}

export default App;
