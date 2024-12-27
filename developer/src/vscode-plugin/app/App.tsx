/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import React from 'react';
import './App.css';
import { KMXPlus } from '@keymanapp/common-types';
import KMXPlusFile = KMXPlus.KMXPlusFile;
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { Button, Input, InputNumber, Checkbox, Card, Segmented, Skeleton, Alert } from 'antd';

interface LoadedData {
  loaded: boolean;
  text?: string;
  kmxPlus?: KMXPlusFile;
  messages?: string[];
}

const noKmxPlusFile : KMXPlusFile = undefined as unknown as KMXPlusFile;

const KmxPlusContext = React.createContext(noKmxPlusFile);

const vsCode = (global as any).acquireVsCodeApi();

function RawSource({ text }) {
  const [shown, setShown] = React.useState(false);
  function show() {
    setShown(true);
  }
  if (!text) return;
  if (!shown) return (
    <Button type="primary" onClick={() => setShown(true)}>Show Raw Source</Button>
  );
  return (
    <div>
      <Button type="primary" onClick={() => setShown(false)}>Hide Raw Source</Button>
      <h4>Raw Source</h4>
      <pre className="code">{text}</pre>
    </div>
  );
}

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
  const onClick = () => {};
  if (k.to.value === '') {
    return (
      <i key={id} onClick={() => setChosenKey(id)} className={className} title={id}>{id}</i>
    )
  }
  return (
    <kbd key={id} onClick={() => setChosenKey(id)} className={className} title={id}>{k.to.value}</kbd>
  );
}

function KeyList({ keys, chosenKey, setChosenKey }:
  { keys: KMXPlus.KeysKeys[], chosenKey: string, setChosenKey: any /* setter */ }) {
  return (
    <div className="keyList">
      {keys?.map((k) => (<Key key={k.id.value} k={k} chosenKey={chosenKey} setChosenKey={setChosenKey} />))}
    </div>
  );
}

/** detail editor */
function KeyDetails({ chosenKey } : { chosenKey : string }) {
  const kmxPlus = React.useContext(KmxPlusContext);
  if (!chosenKey) return;
  const chosenKeys = kmxPlus.kmxplus.keys?.keys.filter(({id}) => id.value == chosenKey);
  if (!chosenKeys?.length) {
    return; // no key selecte4d
  } else if (chosenKeys?.length > 1) {
    throw Error(`more than one key with id ${chosenKey}`);
  }

  const k = chosenKeys[0];

  const {flags} = k;
  const isGap = flags & constants.keys_key_flags_gap;

  return (
    <Card title="Details" bordered={true}>
      <h5>Details</h5>
      <b>ID:</b> <Input value={k.id.value} /> <br/>
      <b>To:</b> <Input value={k.to.value} /> <br/>
      <b>Width:</b> <InputNumber min={0.1} max={99.0} step={0.1} value={k.width / 10} /> <br/>
      <Checkbox checked={isGap}>Gap</Checkbox> <br/>
      <p>flags? 0x{k.flags.toString(16)}</p>
    </Card>
  );
}

/** the main editor for keys */
function KeyBag() {
  const kmxPlus = React.useContext(KmxPlusContext);
  const keys = kmxPlus?.kmxplus?.keys?.keys || [];
  /** string id of selected key */
  const [ chosenKey, setChosenKey ] = React.useState(keys[0]?.id?.value);
  if (!kmxPlus) return; // get out
  return (
    <>
      <h4>Key Bag</h4>
      <div className="keyListContainer">
        <KeyList keys={keys} chosenKey={chosenKey} setChosenKey={setChosenKey} />
      </div>
      <KeyDetails chosenKey={chosenKey}/>
    </>
  );
}

/** treat hardware as width -1 for sorting */
const WIDTH_HARDWARE = -1;

/** get an index for a LayrList */
function touchWidth( list: KMXPlus.LayrList ) : number {
  if (list.hardware.value === constants.layr_list_hardware_touch) {
    return list.minDeviceWidth;
  } else {
    return -1;
  }
}

function LayoutListButton({curWidth, setCurWidth, list} :
  {
    curWidth: number,
    setCurWidth: any,
    list: KMXPlus.LayrList,
    key: string,
  }
) {
  const myWidth = touchWidth(list);
  const selected = (curWidth === myWidth);
  const isTouch = (list.hardware.value === constants.layr_list_hardware_touch);
  const title = isTouch ? `Touch>${list.minDeviceWidth}px` : `${list.hardware.value}`;
  const className = selected ? 'layoutListButton selectedLayoutListButton' : 'layoutListButton unselectedLayoutListButton';
  return (
    <button className={className} onClick={() => setCurWidth(myWidth)}>
      {title}
    </button>
  );
}

function modToStr(mod : number) {
  if (mod === constants.keys_mod_none) {
    return 'none';
  }
  let ret : string[] = [];
  constants.keys_mod_map.forEach((mask, name) => {
    if(mod & mask) {
      ret.push(name);
    }
  });
  ret.sort(); // make it deterministic
  return ret.join(',');
}

function layerTitle(layer : KMXPlus.LayrEntry) {
  if (layer.id.value) return layer.id.value + modToStr(layer.mod);
  return modToStr(layer.mod);
}

function Row({row} : {
  row: KMXPlus.LayrRow,
  key: string,
}) {
  return (
    <>
      {row.keys.map(({value})=>(
        <kbd key={value}>{value}</kbd>
      ))}
      <br/>
    </>
  )
}

function Layer({layer} : {
  layer: KMXPlus.LayrEntry,
  key: string,
}) {
  return (
    <>
      <h5>{layerTitle(layer)}</h5>
      {layer.rows.map((row,index)=> (<Row row={row} key={index.toString()}/>))}
    </>
  );
}

function listTitle(list : KMXPlus.LayrList) {
  const myWidth = touchWidth(list);
  const isTouch = (list.hardware.value === constants.layr_list_hardware_touch);
  const title = isTouch ? `Touch>${list.minDeviceWidth}px` : `${list.hardware.value}`;
  return title;
}

function LayoutList({curWidth, setCurWidth, list} :
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
      {list.layers.map((layer)=> (<Layer key={layerTitle(layer)} layer={layer}/>))}
    </div>
  );
}

function KeyLayouts() {
  const kmxPlus = React.useContext(KmxPlusContext);
  const lists = [...(kmxPlus.kmxplus.layr?.lists || [])]; // copy the list
  const [curWidth, setCurWidth] = React.useState(WIDTH_HARDWARE);
  lists.sort((a,b)=>touchWidth(a)-touchWidth(b)); // sort by width
  const listWidthAndTitle = lists.map((list) => [listTitle(list),touchWidth(list)]);
  const listTitles = listWidthAndTitle.map(([t])=>t); // just titles
  // search listWidthAndTitle for the width
  function findWidth(title) {
    return listWidthAndTitle.filter(([t])=>t == title)[0][1];
  }
  return (
    <div className="keyLayouts">
      <h4>Layouts</h4>
      <Segmented<string>
          options={listTitles}
          onChange={(value)=>setCurWidth(findWidth(value))} />
      {lists.map((list) => (<LayoutList key={touchWidth(list).toString()} curWidth={curWidth} setCurWidth={setCurWidth} list={list}/>))}
    </div>
  );
}

function Messages({messages} : {messages: string[]}) {
  if (!messages) return;
  return (
    <ul>
      {messages.map((msg,key) => (<li key={key}>{msg}</li>))}
    </ul>
  );
}

function Keyboard() {
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
        <Skeleton/>
      </div>
    );
  }

  if (!data.kmxPlus) {
    return (
      <div>
        <Alert type="error" showIcon message="Could not read keyboard file. Correct these issues and reload the file." />
        <Messages messages={data.messages}/>
      </div>
    )
  }

  return (
    <div>
        <Messages messages={data.messages}/>
        <KmxPlusContext.Provider value={data.kmxPlus}>
        <KeyBag />
        <KeyLayouts />
      </KmxPlusContext.Provider>
      <hr />
      <RawSource text={data?.text} />
    </div>
  );
}

function App() {
  return (
    <div className="App">
      <h4>LDML Editor</h4>
      <Keyboard />
    </div>
  );
}

export default App;
