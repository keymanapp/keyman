/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Visual editor app
 */

import React from 'react';
import './App.css';
import { KMXPlus } from '@keymanapp/common-types';
import KMXPlusFile = KMXPlus.KMXPlusFile;
import { Button, Input, InputNumber, Checkbox, Segmented, Skeleton, Alert, Collapse, TabsProps, Tabs, Spin, Space } from 'antd';
/** Ant's Card had an import problem, so we use this workaround */
import { FixedCard as Card } from './FixedCard.js';
import { isGapKey, layerTitle, listTitle, touchWidth, WIDTH_HARDWARE } from './utils.js';
import { FakeRepertoire, RepertoireEntry, SAMPLE_REPERTOIRE } from './fakerepertoire.js';
// import { RawSource } from './RawSource';

/**
 * Used to get the VSCode global. Can only call this once.
 */
const vsCode = (global as any).acquireVsCodeApi();

/** this context will have the KMXPlusFile. We won't use it until the KMXPlusFile is valid. */
const KmxPlusContext = React.createContext(undefined as unknown as KMXPlusFile)

// -------- info -------------

const names = new Intl.DisplayNames(['en'], {type: 'language'});

function langName(l : string) {
  try {
    return names.of(l);
  } catch(e) {
    return '';
  }
}

function LanguageList({ list }: { list: string[] }) {
  const [langs, setLangs] = React.useState(list);
  const [addingLang, setAddingLang] = React.useState('');
  function addLanguage(l : string) {
    if (!l) return;
    setLangs((langs) => [...(langs.filter((ll: string) => ll !== l)), l].sort());
  }
  function removeLanguage(l : string) {
    if (!l) return;
    if (langs.length <= 1) return; // cannot delete last
    // set the list to all but this one
    setLangs(langs => langs.filter((lang: string) => lang !== l));
  }
  const addInput = <Input onPressEnter={(e) => addLanguage(e.target.value)} onChange={(e) => setAddingLang(e.target.value)} placeholder="locale code" />;
  return (
    <>
      <Space.Compact>
        { addInput }
        <Button onClick={() => addLanguage(addingLang)}>Add</Button>
      </Space.Compact>
      <ul>
        {(langs as string[]).map((lang, key) => (
          <li key={key}>
            {lang} — {langName(lang)}
            {(langs.length) && <button onClick={() => removeLanguage(lang)}>×</button>}
          </li>
        ))}
      </ul>
    </>
  );
}

function KeyboardInfo() {
  const kmxPlus = React.useContext(KmxPlusContext) as KMXPlus.KMXPlusFile;

  const { meta, loca } = kmxPlus.kmxplus;
  // modelled after Keyman Developer
  return (
    <>
      <b>Keyboard Name:</b>
        <Input value={meta?.name?.value}/>
        <br/>
      <b>Author:</b>
        <Input value={meta?.author?.value}/>
        <br/>
      <b>Version:</b>
        <Input value={meta?.version?.value}/>
        <br/>
      <b>Supported Languages:</b>
        <LanguageList list={[...(loca?.locales || []).map(l => l.value)]}/>
        <br/>
    </>
  );
}

// -------- repertoire -------------

function RepertoireItem({ item } : { item : RepertoireEntry, key?: any}) {
  return (
    <span className="repertoireItem">{item}</span>
  );
}

function AddCldrSldr() {
  const [ loading, setLoading ] = React.useState(false);

  return (
    <>
      Locale: <Input value="mt"/> <br/>
      <Button type="primary" onClick={() => setLoading((v) => !v)}>Load</Button>
      {loading && (<Spin/>)}
    </>
  )
}

function AddUnicodeRange() {
  return (
    <>
      <InputNumber value="U+1A00"/>
      …
      <InputNumber value="U+1AFF"/>
      <br/>
      <Button type="primary">Add</Button>
    </>
  );
}

function AddRepertoire() {
  const items : TabsProps['items'] = [
    {
      key: 'sldr',
      label: 'SLDR',
      children: (<AddCldrSldr/>),
    },
    {
      key: 'cldr',
      label: 'CLDR',
      children: (<AddCldrSldr/>),
    },
    {
      key: 'range',
      label: 'Unicode Range',
      children: (<AddUnicodeRange/>),
    },
    {
      key: 'corpus',
      label: 'Corpus',
      children: (<Skeleton/>),
    },
  ];
  return (<Tabs defaultActiveKey='range' items={items} />);
}

function Repertoire({ repertoire }: { repertoire : FakeRepertoire}) {

  // the 'add' panel
  const addItems = ([
    {
      key: 'add',
      label: 'Add…',
      children: (
        <AddRepertoire/>
      ),
    }
  ]);

  return (
    <>
      <div className="repertoire">
        {repertoire.map((s, key) => (
          <RepertoireItem key={key} item={s}/>
        ))}
      </div>
      <Collapse items={addItems} />
    </>
  );
}

// -------- keybag -------------

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
  const clickToChose = chosen ? '' : id;
  if (k.to.value === '') {
    return (
      <i key={id} onClick={() => setChosenKey(clickToChose)} className={className} title={id}>{id}</i>
    )
  }
  return (
    <kbd key={id} onClick={() => setChosenKey(clickToChose)} className={className} title={id}>{k.to.value}</kbd>
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
  const kmxPlus = React.useContext(KmxPlusContext) as KMXPlus.KMXPlusFile;
  const keys = kmxPlus?.kmxplus?.keys?.keys || [];
  /** string id of selected key */
  const [chosenKey, setChosenKey] = React.useState('' /*keys[0]?.id?.value*/);
  if (!kmxPlus) return; // get out
  return (
    <>
      <div className="keyListContainer">
        <KeyList keys={keys} chosenKey={chosenKey} setChosenKey={setChosenKey} />
      </div>
      <KeyDetails chosenKey={chosenKey} />
    </>
  );
}

// -------- layer -------------


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
  const kmxPlus = React.useContext(KmxPlusContext) as KMXPlus.KMXPlusFile;
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
        <Segmented<string>
          options={listTitles}
          onChange={(value) => setCurWidth(findWidth(value))} />
        {lists.map((list) => (<LayoutList key={touchWidth(list).toString()} curWidth={curWidth} setCurWidth={setCurWidth} list={list} />))}
    </div>
  );
}

// -------- err messages -------------

/** this shows any error or warning messages from the compiler */
function Messages({ messages }: { messages: string[] }) {
  if (!messages) return;
  return (
    <ul>
      {messages.map((msg, key) => (<li key={key}>{msg}</li>))}
    </ul>
  );
}

// -------- main editor -------------

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
        {/* show something while waiting */}
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

  const items = [
    {
      key: 'info',
      label: 'Info',
      children: (<KeyboardInfo />),
    },
    {
      key: 'repertoire',
      label: 'Character Repertoire',
      children: (<Repertoire repertoire={SAMPLE_REPERTOIRE} />),
    },
    {
      key: 'key',
      label: 'Key Bag',
      children: (<KeyBag />),
    },
    {
      key: 'layouts',
      label: 'Layouts',
      children: (<KeyLayouts />),
    },
  ];

  return (
    <>
      <Messages messages={data.messages} />
      <KmxPlusContext.Provider value={data.kmxPlus}>
        <Collapse items={items} defaultActiveKey={[
          // default items visible in the editor

          // 'info',
          'repertoire',
          // 'key',
          // 'layouts',
        ]} />
      </KmxPlusContext.Provider>
      {/*
        TODO-EPIC-LDML: raw source isn't really needed.
        Users can use the system editor view if needed.
        <hr />
      <RawSource text={data?.text} /> */}
    </>
  );
}

/** The outer App. TODO-EPIC-LDML this may be unneeded */
function App() {
  return (
    <div className="App">
      <KeyboardFile />
    </div>
  );
}

export default App;
