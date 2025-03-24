---
title: Layout Specifications
---

Touch-screen layouts for *KeymanWeb 17* are specified as JSON objects containing a member object for each specified device type. Currently supported device types are *tablet* and *phone*. Layouts for *desktop*  computers may also be specified but desktop on-screen keyboard design is normally managed by the
standard *Keyman Developer* on-screen keyboard tool rather than the keyboard layout designer. If the same
layout is appropriate for both *tablet* and *phone* devices only one need be specified, and will be used for either type of device.

File encoding for manually-created layout files may use either UTF-8 or 7-bit ANSI coding, but must not include a BOM. For easier editing and management without requiring special fonts, embedded Unicode characters with values above 127 may use the *\uXXXX* notation.

For details of the JSON specification, see [The JSON Data Interchange Format(ECMA-404)](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf).

<table data-border="1">
    <thead>
        <tr>
            <th style="text-align: left;">Object name or description</th>
            <th style="text-align: left;">Object or array element members</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td class="rowhead" style="text-align: left;">(container object)</td>
            <td class="members" style="text-align: left;">
                <table width="90%" data-border="0">
                    <thead>
                        <tr>
                            <th style="text-align: left;">Name</th>
                            <th style="text-align: left;">Type</th>
                            <th style="text-align: left;">Description</th>
                            <th style="text-align: left;">Required</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="rowhead" style="text-align: left;">tablet</td>
                            <td style="text-align: left;">Object</td>
                            <td style="text-align: left;">Tablet layout specification</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">phone</td>
                            <td style="text-align: left;">Object</td>
                            <td style="text-align: left;">Phone layout specification</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">desktop</td>
                            <td style="text-align: left;">Object</td>
                            <td style="text-align: left;">Desktop OSK layout specification</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                    </tbody>
                </table>
            </td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">(layout specification)</td>
            <td class="members" style="text-align: left;">
                <table width="90%" data-border="0">
                    <thead>
                        <tr>
                            <th style="text-align: left;">Name</th>
                            <th style="text-align: left;">Type</th>
                            <th style="text-align: left;">Description</th>
                            <th style="text-align: left;">Required</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="rowhead" style="text-align: left;">font</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Key label default font</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">fontsize</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Key label default font size (in <i>em</i>)</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">layer</td>
                            <td style="text-align: left;">Object array</td>
                            <td style="text-align: left;">Array of layer specifications</td>
                            <td style="text-align: center;">Yes</td>
                        </tr>
                    </tbody>
                </table>
            </td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">layer</td>
            <td class="members" style="text-align: left;">
                <table width="90%" data-border="0">
                    <thead>
                        <tr>
                            <th style="text-align: left;">Name</th>
                            <th style="text-align: left;">Type</th>
                            <th style="text-align: left;">Description</th>
                            <th style="text-align: left;">Required</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="rowhead" style="text-align: left;">id</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Layer name</td>
                            <td style="text-align: center;">Yes</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">row</td>
                            <td style="text-align: left;">Object array</td>
                            <td style="text-align: left;">Array of keyboard row specifications</td>
                            <td style="text-align: center;">Yes</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">nextlayer</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Keyboard layer to display after processing keystroke</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                    </tbody>
                </table>
            </td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">row</td>
            <td class="members" style="text-align: left;">
                <table width="90%" data-border="0">
                    <thead>
                        <tr>
                            <th style="text-align: left;">Name</th>
                            <th style="text-align: left;">Type</th>
                            <th style="text-align: left;">Description</th>
                            <th style="text-align: left;">Required</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="rowhead" style="text-align: left;">id</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Row number</td>
                            <td style="text-align: center;">Yes</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">key</td>
                            <td style="text-align: left;">Object array</td>
                            <td style="text-align: left;">Array of keyboard key specifications</td>
                            <td style="text-align: center;">Yes</td>
                        </tr>
                    </tbody>
                </table>
            </td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">key</td>
            <td class="members" style="text-align: left;">
                <table width="90%" data-border="0">
                    <thead>
                        <tr>
                            <th style="text-align: left;">Name</th>
                            <th style="text-align: left;">Type</th>
                            <th style="text-align: left;">Description</th>
                            <th style="text-align: left;">Required</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="rowhead" style="text-align: left;">id</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Key identifier</td>
                            <td style="text-align: center;">Yes*</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">text</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Text on key</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">font</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Font name if different from default key label font</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">fontsize</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Font size if different from default key label font size</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">sp</td>
                            <td style="text-align: left;">number</td>
                            <td style="text-align: left;">Key style</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">width</td>
                            <td style="text-align: left;">number</td>
                            <td style="text-align: left;">Key width</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">pad</td>
                            <td style="text-align: left;">number</td>
                            <td style="text-align: left;">Space before key</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">dk</td>
                            <td style="text-align: left;">number</td>
                            <td style="text-align: left;">Deadkey (1) or normal key (0 or omitted)</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">layer</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Modifier key or keys assumed applied when
                            mapping this key</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">nextlayer</td>
                            <td style="text-align: left;">string</td>
                            <td style="text-align: left;">Name of keyboard layer to be displayed after this key is processed</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">sk</td>
                            <td style="text-align: left;">Object array</td>
                            <td style="text-align: left;">Array of pop-up key specifications</td>
                            <td style="text-align: center;">No</td>
                        </tr>
                    </tbody>
                </table>
            </td>
        </tr>
    </tbody>
</table>

Details of key member specifications are given below:

<table data-border="1">
    <thead>
        <tr>
            <th style="text-align: left;">Member name</th>
            <th style="text-align: left;">Details</th>
        </tr>
    </thead>
    <tbody>
        <tr>    
            <td class="rowhead" style="text-align: left;">id</td>
            <td style="text-align: left;">Each key id must start with <i>>K_</i> , for keys mapped to standard Keyman virtual key names, e.g.  <i>K_HYPHEN</i> , or either  <i>U_</i>  or <i>T_</i>  for user-defined names. Keys identified as  <i>U_xxxx[_xxxx...]</i> specify one or more Unicode characters in hex format, e.g. <i>U_1363</i>  for the Ethiopic Comma character, and will insert those characters if the key id is not matched by a rule. Other user-defined keys, such as <i>T_ZZZ</i> , will be ignored unless matched by a rule. The key id is required except for key styles 9 or 10 (blank or spacer keys).</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">text</td>
            <td style="text-align: left;">Where a key id is of the form <i>K_XXXX</i> , the text on the key will be the indicated Unicode character if this member is omitted. However, what the keystroke output will depend on any rules that process the keystroke, and may or may not be the same as the key text.</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">sp</td>
            <td class="members" style="text-align: left;">
                <table width="90%" data-border="0">
                    <thead>
                        <tr>
                            <th style="text-align: left;">Value</th>
                            <th style="text-align: left;">Key style</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="rowhead" style="text-align: left;">0</td>
                            <td style="text-align: left;">Normal key (default)</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">1</td>
                            <td style="text-align: left;">Shift key</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">2</td>
                            <td style="text-align: left;">Active shift key</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">8</td>
                            <td style="text-align: left;">Deadkey</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">9</td>
                            <td style="text-align: left;">Blank key</td>
                        </tr>
                        <tr>
                            <td class="rowhead" style="text-align: left;">10</td>
                            <td style="text-align: left;">Spacer</td>
                        </tr>
                    </tbody>
                </table>
            </td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">font</td>
            <td style="text-align: left;">The font for the key label may be specified if it is necessary to use a different font for that key.</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">fontsize</td>
            <td style="text-align: left;">The font size for the key label may be specified (in <i>em</i> units) if necessary.</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">width</td>
            <td style="text-align: left;">By default, each key has a width of 100
            units, with rows then scaled to make the widest row fit on the screen.
            Set the width value if a key should be narrower or wider than the
            standard width.</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">pad</td>
            <td style="text-align: left;">Space before key, in same units as width (default key width=100 units).</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">layer</td>
            <td style="text-align: left;">Override the default layer type for processing key rules. For example, use <i>'layer':'shift'</i> in any layer to process a key rule as if it were the <i>shift</i> layer, i.e. as if the Shift key was down.</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">nextlayer</td>
            <td style="text-align: left;">Select the key layer to be displayed after processing the current keystroke. This will usually be used to implement custom layer selection, but can also be used to select a different layer after a deadkey or normal key, and will override any <i>nextlayer</i>  value specifed for the entire layer. The layer to be displayed may also be specified using the setlayer() statement in a rule in the keyboard source.</td>
        </tr>
        <tr>
            <td class="rowhead" style="text-align: left;">sk</td>
            <td style="text-align: left;">Pop-up key specifications are identical to normal key specifications except that they cannot be nested. (Key size and spacing members <i>width</i> and <i>pad</i> are ignored for pop-up keys.)</td>
        </tr>
    </tbody>
</table>

For many keyboards, it is helpful to associate some keyboard layers with physical keyboard modifier states. This is reflected in the layer name, where a layer name of *shift* means that when a key in that layer is touched, the keystroke will be processed as if the keyboard Shift key is held.

The special layer names of:  
   *leftalt* ,
 *rightalt* ,
 *alt* ,  
   *leftctrl* ,
 *rightctrl* ,
 *ctrl* ,
 *ctrlalt* ,  
   *leftctrl-leftalt* ,
 *rightctrl-rightalt* ,
 *leftctrl-leftalt-shift* ,
 *rightctrl-rightalt-shift* ,  
   *shift* ,
 *shift-alt* ,
 *shift-ctrl* ,
 *shift-ctrl-alt* ,  
   *leftalt-shift* ,
 *rightalt-shift* ,
 *leftctrl-shift* , and
 *rightctrl-shift*   
each apply the appropriate modifier keys when processing rules unless overridden by a *layer* value in the key specification. The layer names *default* , *numeric* , *symbol* and *currency* are also recognized, but no modifier key is applied for keys in these layers.

The following special key identifiers have been added to simplify layer selection:

| Identifier   | Value |
|:-------------|:------|
| K_NUMERALS   | 261   |
| K_SYMBOLS    | 262   |
| K_CURRENCIES | 263   |
| K_SHIFTED    | 264   |
| K_ALTGR      | 265   |

A special font is used to provide easily recognizable key graphics for various special purpose keys. The following key text strings will be recognized and cause the appropriate graphic to be used for the key cap instead of the actual text:

| Text string       | Key purpose                                             |
|:------------------|:--------------------------------------------------------|
| \*Shift\*         | Shift key image (inactive)                              |
| \*Enter\*         | Return                                                  |
| \*Tab\*           | Move to next input element in tab order                 |
| \*BkSp\*          | Backspace                                               |
| \*Menu\*          | Display the language menu                               |
| \*Hide\*          | Hide the virtual keyboard                               |
| \*Alt\*           | Alt key caption                                         |
| \*Ctrl\*          | Control key caption                                     |
| \*Caps\*          | Caps Lock caption                                       |
| \*ABC\*           | Select the upper case alphabetic layer                  |
| \*abc\*           | Return to the default (alphabetic) keyboard layer       |
| \*123\*           | Select a numeric keyboard layer                         |
| \*Symbol\*        | Select a layer with various non-letter symbols          |
| \*Currency\*      | Select a layer with currency symbols                    |
| \*Shifted\*       | Active Shift key image                                  |
| \*AltGr\*         | Select Control + Alt (AltGr) modifier state             |
| \*TabLeft\*       | Go back to previous input element in tab order          |
| \*LAlt\*          | Select left Alt modifier state                          |
| \*RAlt\*          | Select right Alt modifier state                         |
| \*LCtrl\*         | Select left Control modifier state                      |
| \*RCtrl\*         | Select right Control modifier state                     |
| \*LAltCtrl\*      | Select left Alt + left Control modifier state           |
| \*RAltCtrl\*      | Select right Alt + right Control modifier state         |
| \*LAltCtrlShift\* | Select left Alt + left Control + Shift modifier state   |
| \*RAltCtrlShift\* | Select right Alt + right Control + Shift modifier state |
| \*AltShift\*      | Select Alt + Shift modifier state                       |
| \*CtrlShift\*     | Select Control + Shift modifier state                   |
| \*AltCtrlShift\*  | Select Alt + Control + Shift modifier state             |
| \*LAltShift\*     | Select left Alt + Shift modifier state                  |
| \*RAltShift\*     | Select right Alt + Shift modifier state                 |
| \*LCtrlShift\*    | Select left Control + Shift modifier state              |
| \*RCtrlShift\*    | Select right Control + Shift modifier state             |
