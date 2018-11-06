/****************************** Module Header ******************************\
* Module Name: kbd.h
*
* Copyright (c) 1985-91, Microsoft Corporation
*
* Keyboard table values that form the basis for languages and keyboard types.
* The basis is US, kbd type 4 - all others are a variation on this.
* This file is included by all kbd**.h files.
*
* History:
* 10-Jan-1991 GregoryW
* 23-Apr-1991 IanJa         VSC_TO_VK _* macros from oemtab.c
\***************************************************************************/

#ifndef _KBD_
#define _KBD_

/****************************************************************************\
*
* Keyboard Layers.   Used in kdb??.dll and in usersrv.dll
*
\****************************************************************************/

/*
 * Key Event (KE) structure
 * Stores a Virtual Key event
 */
typedef struct tagKE {
    BYTE   bScanCode;   // Virtual Scan Code (Set 1)
    USHORT usFlaggedVk; // Vk | Flags
} KE, *PKE;

typedef BOOL (* KEPROC)(PKE pKe);

/*
 * KE.usFlaggedVk values, also used in the keyboard layer tables.
 */
#define KBDEXT     (USHORT)0x0100
#define KBDMULTIVK (USHORT)0x0200
#define KBDSPECIAL (USHORT)0x0400
#define KBDNUMPAD  (USHORT)0x0800
#define KBDBREAK   (USHORT)0x8000

/*
 * Key message lParam bits
 */
#define EXTENDED_BIT   0x01000000
#define DONTCARE_BIT   0x02000000
#define FAKE_KEYSTROKE 0x02000000

/*
 * Keyboard Shift State defines. These correspond to the bit mask defined
 * by the VkKeyScan() API.
 */
#define KBDBASE        0
#define KBDSHIFT       1
#define KBDCTRL        2
#define KBDALT         4
#define KBDGRPSELTAP   0x80

/*
 * Handy diacritics
 */
#define GRAVE           0x0300
#define ACUTE           0x0301
#define CIRCUMFLEX      0x0302
#define TILDE           0x0303
#define MACRON          0x0304
#define OVERSCORE       0x0305
#define BREVE           0x0306
#define DOT_ABOVE       0x0307
#define UMLAUT          0x0308
#define DIARESIS        UMLAUT
#define HOOK_ABOVE      0x0309
#define RING            0x030A
#define DOUBLE_ACUTE    0x030B
#define HACEK           0x030C

#define CEDILLA         0x0327
#define OGONEK          0x0328
#define TONOS           0x0384
#define DIARESIS_TONOS  0x0385


#define wszGRAVE           L"\x0300"
#define wszACUTE           L"\x0301"
#define wszCIRCUMFLEX      L"\x0302"
#define wszTILDE           L"\x0303"
#define wszMACRON          L"\x0304"
#define wszOVERSCORE       L"\x0305"
#define wszBREVE           L"\x0306"
#define wszDOT_ABOVE       L"\x0307"
#define wszUMLAUT          L"\x0308"
#define wszHOOK_ABOVE      L"\x0309"
#define wszRING            L"\x030A"
#define wszDOUBLE_ACUTE    L"\x030B"
#define wszHACEK           L"\x030C"

#define wszCEDILLA         L"\x0327"
#define wszOGONEK          L"\x0328"
#define wszTONOS           L"\x0384"
#define wszDIARESIS_TONOS  L"\x0385"

/*
 * function prototypes
 */
VOID    VSCFromSC(PKE pke);
BYTE    VKFromVSC(PKE pke, BYTE bPrefix, LPBYTE afKeyState);
BOOL    KEOEMProcs(PKE pke);
BOOL    KELocaleProcs(PKE pke);
VOID    _KeyEvent(USHORT usVk, WORD wScanCode, DWORD ExtraInfo);

/***************************************************************************\
* MODIFIER KEYS
*
* All keyboards have "Modifier" keys which are used to alter the behaviour of
* some of the other keys.  These shifter keys are usually:
*   Shift  (left and/or right Shift key)
*   Ctrl   (left and/or right Ctrl key)
*   Alt    (left and/or right Alt key)
*   AltGr  (right Alt key only)
*
* NOTE:
*   All keyboards use the Shift key.
*   All keyboards use a Ctrl key to generate ASCII control characters.
*   All keyboards with a number pad use the Alt key and the NumPad to
*     generate characters by number.
*   Keyboards using AltGr as a Modifier Key usually translate the Virtual
*     ScanCode to Virtual Keys VK_CTRL + VK_ALT at input time: the Modifier
*     tables should be written to treat Ctrl + Alt as a valid shifter
*     key combination in these cases.
*
* By holding down 0 or more of these Modifier keys, a "shift state" is
* obtained : the shift state may affect the translation of Virtual Scancodes
* to Virtual Keys and/or the translation of Virtuals Key to Characters.
*
* EXAMPLES:
*
* Each key on a particular keyboard may be marked with up to five different
* characters in five different positions:
*
*              .-------.
*             /|       |\
*            : | 2   4 | :
*            | |       | |
*            | |       | |
*            | | 1   3 | |
*            | |_______| |
*            | /       \ |
*            |/    5    \|
*            `-----------'
*
* A key may also be able to generate a character that is not marked on it:
* these are ASCII Control chars, lower-case letters and/or "invisible keys".
*                                                  .-------.
*      An example of an "Invisible Key":          /|       |\
*                                                : | >     | :
*  The German M24 keyboard 2 should produce the  | |       | |
*  '|' character when ALT SHIFT is is held down  | |       | |
*  while the '<' key (shown here) is pressed:    | | <   \ | |
*  This keyboard has four other invisible        | |_______| |
*  characters.  France, Italy and Spain also     | /       \ |
*  support invisible characters on the M24       |/         \|
*  Keyboard 2 with ALT SHIFT depressed.          `-----------'
*
* The keyboard table must list the keys that contribute to it's shift state,
* and indicate which combinations are valid.  This is done with
*    aCharModifiers[]  - convert combinations of Modifier Keys to Bitmasks.
* and
*    aModification[];  - convert Modifier Bitmasks to enumerated Modifications
*
* AN EXAMPLE OF VALID AND INVALID MODIFIER KEY COMBINATIONS
*
*    The US English keyboard has 3 Modifier keys:
*      Shift (left or right); Ctrl (left or right); and Alt (left or right).
*
*    The only valid combinations of these Modifier Keys are:
*      none pressed      : Character at position (1) on the key.
*      Shift             : Character at position (2) on the key.
*      Ctrl              : Ascii Control characters
*      Shift + Ctrl      : Ascii Control characters
*      Alt               : Character-by-number on the numpad
*
*    The invalid combinations (that do not generate any characters) are:
*      Shift + Alt
*      Alt + Ctrl
*      Shift + Alt + Ctrl
*
* Something (???) :
* -----------------
*      Modifier keys              Character produced
*      -------------------------  ------------------
*   0  No shifter key depressed   position 1
*   1  Shift key is depressed     position 2
*   2  AltGr (r.h. Alt) depressed position 4 or 5 (whichever is marked)
*
* However, note that 3 shifter keys (SHIFT, can be combined in a
* characters, depending on the Keyboards
* Consider the following keyboards:
*
*     .-------.            STRANGE KBD         PECULIAR KBD
*    /|       |\           ==================  ==================
*   : | 2   4 | :    1   -
*   | |       | |    2   - SHIFT               SHIFT
*   | |       | |    3   - MENU                MENU
*   | | 1   3 | |    4   - SHIFT + MENU        SHIFT + MENU
*   | |_______| |    5   -    no such keys     CTRL  + MENU
*   | /       \ |
*   |/    5    \|
*   `-----------'
* Both STRANGE and PECULIAR keyboards could have aVkToBits[] =
*   { VK_SHIFT  , KBDSHIFT }, // 0x01
*   { VK_CONTROL, KBDCTRL  }, // 0x02
*   { VK_MENU   , KBDALT   }, // 0x04
*   { 0,          0        }
*
* The STRANGE keyboard has 4 distinct shift states, while the PECULIAR kbd
* has 5.  However, note that 3 shifter bits can be combined in a
* total of 2^3 == 8 ways.  Each such combination must be related to one (or
* none) of the enumerated shift states.
* Each shifter key combination can be represented by three binary bits:
*  Bit 0  is set if VK_SHIFT is down
*  Bit 1  is set if VK_CONTROL is down
*  Bit 2  is set if VK_MENU is down
*
* Example: If the STRANGE keyboard generates no characters in combination
* when just the ALT key is held down, nor when the SHIFT, CTRL and ALT keys
* are all held down, then the tables might look like this:
*
*                                VK_MENU,
*                        VK_CTRL,                    0
*    };
*    aModification[] = {
*        0,            //   0       0       0     = 000  <none>
*        1,            //   0       0       1     = 001  SHIFT
*        SHFT_INVALID, //   0       1       0     = 010  ALT
*        2,            //   0       1       1     = 011  SHIFT ALT
*        3,            //   1       0       0     = 100  CTRL
*        4,            //   1       0       1     = 101  SHIFT CTRL
*        5,            //   1       1       0     = 110  CTRL ALT
*        SHFT_INVALID  //   1       1       1     = 111  SHIFT CTRL ALT
*    };
*
*
\***************************************************************************/

/***************************************************************************\
* VK_TO_BIT - associate a Virtual Key with a Modifier bitmask.
*
* Vk        - the Virtual key (eg: VK_SHIFT, VK_RMENU, VK_CONTROL etc.)
*             Special Values:
*                0        null terminator
* ModBits   - a combination of KBDALT, KBDCTRL, KBDSHIFT and kbd-specific bits
*             Any kbd-specific shift bits must be the lowest-order bits other
*             than KBDSHIFT, KBDCTRL and KBDALT (0, 1 & 2)
*
* Those languages that use AltGr (VK_RMENU) to shift keys convert it to
* CTRL+ALT with the KBDSPECIAL bit in the ausVK[] entry for VK_RMENU
* and by having an entry in aVkToPfnOem[] to simulate the right Vk sequence.
*
\***************************************************************************/
typedef struct {
    BYTE Vk;
    BYTE ModBits;
} VK_TO_BIT, *PVK_TO_BIT;

/***************************************************************************\
* pModNumber  - a table to map shift bits to enumerated shift states
*
* Table attributes: Ordered table
*
* Maps all possible shifter key combinations to an enumerated shift state.
* The size of the table depends on the value of the highest order bit used
* in aCharModifiers[*].ModBits
*
* Special values for aModification[*]
*   SHFT_INVALID - no characters produced with this shift state.
LATER: (ianja) no SHFT_CTRL - control characters encoded in tables like others
*   SHFT_CTRL    - standard control character production (all keyboards must
*                  be able to produce CTRL-C == 0x0003 etc.)
*   Other        - enumerated shift state (not less than 0)
*
* This table is indexed by the Modifier Bits to obtain an Modification Number.
*
*                        CONTROL MENU SHIFT
*
*    aModification[] = {
*        0,            //   0     0     0     = 000  <none>
*        1,            //   0     0     1     = 001  SHIFT
*        SHFT_INVALID, //   0     1     0     = 010  ALT
*        2,            //   0     1     1     = 011  SHIFT ALT
*        3,            //   1     0     0     = 100  CTRL
*        4,            //   1     0     1     = 101  SHIFT CTRL
*        5,            //   1     1     0     = 110  CTRL ALT
*        SHFT_INVALID  //   1     1     1     = 111  SHIFT CTRL ALT
*    };
*
\***************************************************************************/

#pragma warning ( disable : 4200 )

typedef struct {
    PVK_TO_BIT pVkToBit;     // Virtual Keys -> Mod bits
    WORD       wMaxModBits;  // max Modification bit combination value
    BYTE       ModNumber[];  // Mod bits -> Modification Number
} MODIFIERS, *PMODIFIERS;

#pragma warning ( default : 4200 )

WORD GetModifierBits(PMODIFIERS pModifiers, LPBYTE afKeyState);
WORD GetModificationNumber(PMODIFIERS pModifiers, WORD wModBits);

#define SHFT_INVALID 0x0F

/***************************************************************************\
* apulCvt_VK[] - obtain VK translation table from shift state
*     A VK translation table is used to change the value of the Virtual Key
*     according to the shift state.   OEM only (not locale-specific)
\***************************************************************************/
extern PULONG *gapulCvt_VK;
extern ULONG *gapulCvt_VK_101[];
extern ULONG *gapulCvt_VK_84[];

/***************************************************************************\
* awNumPadCvt[]   - Translate cursor movement keys to numpad keys
\***************************************************************************/
extern MODIFIERS Modifiers_VK;
extern BYTE aVkNumpad[];

/***************************************************************************\
* VSC_VK     - Associate a Virtual Scancode with a Virtual Key
*  Vsc - Virtual Scancode
*  Vk  - Virtual Key | flags
* Used by VKFromVSC() for scancodes prefixed 0xE0 or 0xE1
\***************************************************************************/
typedef struct _VSC_VK {
    BYTE Vsc;
    USHORT Vk;
} VSC_VK, *PVSC_VK;

/***************************************************************************\
* VK_VSC     - Associate a Virtual Key with a Virtual Scancode
*  Vk  - Virtual Key
*  Vsc - Virtual Scancode
* Used by MapVirtualKey for Virtual Keys not appearing in ausVK[]
\***************************************************************************/
typedef struct _VK_VSC {
    BYTE Vk;
    BYTE Vsc;
} VK_VSC, *PVK_VSC;

/***************************************************************************\
*
* VK_TO_WCHARS<n> - Associate a Virtual Key with <n> UNICODE characters
*
* VirtualKey  - The Virtual Key.
* wch[]       - An array of characters, one for each shift state that
*               applies to the specified Virtual Key.
*
* Special values for VirtualKey:
*    -1        - This entry contains dead chars for the previous entry
*    0         - Terminates a VK_TO_WCHARS[] table
*
* Special values for Attributes:
*    CAPLOK    - The CAPS-LOCK key affects this key like SHIFT
*    SGCAPS    - CapsLock uppercases the unshifted char (Swiss-German)
*
* Special values for wch[*]:
*    WCH_NONE  - No character is generated by pressing this key with the
*                current shift state.
*    WCH_DEAD  - The character is a dead-key: the next VK_TO_WCHARS[] entry
*                will contain the values of the dead characters (diaresis)
*                that can be produced by the Virtual Key.
*
\***************************************************************************/
#define WCH_NONE 0xF000
#define WCH_DEAD 0xF001

#define CAPLOK      0x01
#define SGCAPS      0x02
#define CAPLOKALTGR 0x04
#define GRPSELTAP   0x80

/*
 * Macro for VK to WCHAR with "n" shift states
 */
#define TYPEDEF_VK_TO_WCHARS(n) typedef struct _VK_TO_WCHARS##n {  \
                                    BYTE  VirtualKey;      \
                                    BYTE  Attributes;      \
                                    WCHAR wch[n];          \
                                } VK_TO_WCHARS##n, *PVK_TO_WCHARS##n;

/*
 * To facilitate coding the table scanning routine.
 */

/*
 * Table element types (for various numbers of shift states), used
 * to facilitate static initializations of tables.
 * VK_TO_WCHARS1 and PVK_TO_WCHARS1 may be used as the generic type
 */
TYPEDEF_VK_TO_WCHARS(1) // VK_TO_WCHARS1, *PVK_TO_WCHARS1;
TYPEDEF_VK_TO_WCHARS(2) // VK_TO_WCHARS2, *PVK_TO_WCHARS2;
TYPEDEF_VK_TO_WCHARS(3) // VK_TO_WCHARS3, *PVK_TO_WCHARS3;
TYPEDEF_VK_TO_WCHARS(4) // VK_TO_WCHARS4, *PVK_TO_WCHARS4;
TYPEDEF_VK_TO_WCHARS(5) // VK_TO_WCHARS5, *PVK_TO_WCHARS5;
TYPEDEF_VK_TO_WCHARS(6) // VK_TO_WCHARS6, *PVK_TO_WCHARS5;

/***************************************************************************\
*
* VK_TO_WCHAR_TABLE - Describe a table of VK_TO_WCHARS1
*
* pVkToWchars     - points to the table.
* nModifications  - the number of shift-states supported by this table.
*                   (this is the number of elements in pVkToWchars[*].wch[])
*
* A keyboard may have several such tables: all keys with the same number of
*    shift-states are grouped together in one table.
*
* Special values for pVktoWchars:
*     NULL     - Terminates a VK_TO_WCHAR_TABLE[] list.
*
\***************************************************************************/

typedef struct _VK_TO_WCHAR_TABLE {
    PVK_TO_WCHARS1 pVkToWchars;
    BYTE           nModifications;
    BYTE           cbSize;
} VK_TO_WCHAR_TABLE, *PVK_TO_WCHAR_TABLE;

/***************************************************************************\
*
* Dead Key (diaresis) tables
*
* LATER IanJa: supplant by an NLS API that composes Diacritic+Base -> WCHAR
*
\***************************************************************************/
typedef struct {
    DWORD  dwBoth;  // diacritic & char
    WCHAR  wchComposed;
} DEADKEY, *PDEADKEY;

#define DEADTRANS(ch, accent, comp) { MAKELONG(ch, accent), comp}

/***************************************************************************\
* VSC_LPWSTR - associate a Virtual Scancode with a Text string
*
* Uses:
*   GetKeyNameText(), aKeyNames[]  Map virtual scancode to name of key
*
\***************************************************************************/
typedef struct {
    BYTE   vsc;
    LPWSTR pwsz;
} VSC_LPWSTR, *PVSC_LPWSTR;

#define KLLF_ALTGR 0x00000001

/***************************************************************************\
* KBDTABLES
*
* This structure describes all the tables that implement the keyboard layer.
*
* When switching to a new layer, we get a new KBDTABLES structure: all key
* processing tables are accessed indirectly through this structure.
*
\***************************************************************************/

typedef struct tagKbdLayer {
    /*
     * Modifier keys
     */
    PMODIFIERS pCharModifiers;

    /*
     * Characters
     */
    VK_TO_WCHAR_TABLE *pVkToWcharTable;  // ptr to tbl of ptrs to tbl

    /*
     * Diacritics
     */
    PDEADKEY pDeadKey;

    /*
     * Names of Keys
     */
    VSC_LPWSTR *pKeyNames;
    VSC_LPWSTR *pKeyNamesExt;
    LPWSTR     *pKeyNamesDead;

    /*
     * Scan codes to Virtual Keys
     */
    USHORT *pusVSCtoVK;
    BYTE    bMaxVSCtoVK;
    PVSC_VK pVSCtoVK_E0;  // Scancode has E0 prefix
    PVSC_VK pVSCtoVK_E1;  // Scancode has E1 prefix

    /*
     * Locale-specific special processing
     */
    DWORD fLocaleFlags;
} KBDTABLES, *PKBDTABLES;

/*
 * OEM-specific special processing (keystroke simulators and filters)
 */
extern KEPROC aKEProcOEM[];

/***************************************************************************\
* Macros for ausVK[] values (used below)
*
* These macros prefix each argument with VK_ to produce the name of a Virtual
* Key defined in "winuser.h" (eg: ESCAPE becomes VK_ESCAPE).
\***************************************************************************/
#ifndef KBD_TYPE
#define KBD_TYPE 4
#endif

/*
 * _NE() selects the Virtual Key according to keyboard type
 */
#if   (KBD_TYPE == 1)
#define _NE(v1,v2,v3,v4,v5,v6) (VK_##v1)
#elif (KBD_TYPE == 2)
#define _NE(v1,v2,v3,v4,v5,v6) (VK_##v2)
#elif (KBD_TYPE == 3)
#define _NE(v1,v2,v3,v4,v5,v6) (VK_##v3)
#elif (KBD_TYPE == 4)
#define _NE(v1,v2,v3,v4,v5,v6) (VK_##v4)
#elif (KBD_TYPE == 5)
#define _NE(v1,v2,v3,v4,v5,v6) (VK_##v5)
#elif (KBD_TYPE == 6)
#define _NE(v1,v2,v3,v4,v5,v6) (VK_##v6)
#endif

/*
 * _EQ() selects the same Virtual Key for all keyboard types
 */
#define _EQ(         v4      ) (VK_##v4)

/*
 * A bit of trickery for virtual key names 'A' to 'Z' and '0' to '9' so
 * that they are not converted to a VK_* name.
 * With this macro, VK_'A' equates to 'A' etc.
 */
#define VK_
#define VK__none_   0xFF
#define VK_ABNT_C1  0xC1
#define VK_ABNT_C2  0xC2

/***************************************************************************\
* T** - Values for ausVK[] (Virtual Scan Code to Virtual Key conversion)
*
* These values are for Scancode Set 3 and the USA.
* Other languages substitute their own values where required (files kbd**.h)
*
* Six sets of keyboards are supported, according to KBD_TYPE:
*
* KBD_TYPE   Keyboard (examples)
* ========   =======================================================
*    1       AT&T '301' & '302'; Olivetti 83-key; PC-XT 84-key; etc.
*    2       Olivetti M24 102-key
*    3       HP Vectra (DIN); Olivetti 86-key; etc.
*    4 *     Enhanced 101/102-key; Olivetti A; etc.
*    5       Nokia (Ericsson) type 5 (1050, etc.)
*    6       Nokia (Ericsson) type 6 (9140)
*
* * If KBD_TYPE is not defined, the default is type 4.
*
* KB3270 comments refers to KB 3270 keyboards in native emulation mode (DIP
* switches all OFF), and the Scancode Map used to convert its scancodes to
* standard scancode set 1.
*    KB3270 <= 57      - this entry is reached by mapping from scancode 0x57
*                        to an arbitrary scancode: the VK is what counts
*    KB3270 => HOME    - this scancode is mapped to the scancode for VK_HOME
*    KB3270            - no mapping involved, a scancode for KB3270 only
*
* _EQ() : all keyboard types have the same virtual key for this scancode
* _NE() : different virtual keys for this scancode, depending on kbd type
*
*     +------+ +--------+--------+--------+--------+--------+--------+
*     | Scan | |  kbd   |  kbd   |  kbd   |  kbd   |  kbd   |  kbd   |
*     | code | | type 1 | type 2 | type 3 | type 4 | type 5 | type 6 |
\****+-------+-+--------+--------+--------+--------+--------+--------+******/

#define T00 _EQ(                           _none_                    )
#define T01 _EQ(                           ESCAPE                    )
#define T02 _EQ(                           '1'                       )
#define T03 _EQ(                           '2'                       )
#define T04 _EQ(                           '3'                       )
#define T05 _EQ(                           '4'                       )
#define T06 _EQ(                           '5'                       )
#define T07 _EQ(                           '6'                       )
#define T08 _EQ(                           '7'                       )
#define T09 _EQ(                           '8'                       )
#define T0A _EQ(                           '9'                       )
#define T0B _EQ(                           '0'                       )
#define T0C _EQ(                           OEM_MINUS                 )
#define T0D _NE(OEM_PLUS,OEM_4,   OEM_PLUS,OEM_PLUS,OEM_PLUS,OEM_PLUS)
#define T0E _EQ(                           BACK                      )
#define T0F _EQ(                           TAB                       )
#define T10 _EQ(                           'Q'                       )
#define T11 _EQ(                           'W'                       )
#define T12 _EQ(                           'E'                       )
#define T13 _EQ(                           'R'                       )
#define T14 _EQ(                           'T'                       )
#define T15 _EQ(                           'Y'                       )
#define T16 _EQ(                           'U'                       )
#define T17 _EQ(                           'I'                       )
#define T18 _EQ(                           'O'                       )
#define T19 _EQ(                           'P'                       )
#define T1A _NE(OEM_4,   OEM_6,   OEM_4,   OEM_4,   OEM_4,   OEM_4   )
#define T1B _NE(OEM_6,   OEM_1,   OEM_6,   OEM_6,   OEM_6,   OEM_6   )
#define T1C _EQ(                           RETURN                    )
#define T1D _EQ(                           LCONTROL                  )
#define T1E _EQ(                           'A'                       )
#define T1F _EQ(                           'S'                       )
#define T20 _EQ(                           'D'                       )
#define T21 _EQ(                           'F'                       )
#define T22 _EQ(                           'G'                       )
#define T23 _EQ(                           'H'                       )
#define T24 _EQ(                           'J'                       )
#define T25 _EQ(                           'K'                       )
#define T26 _EQ(                           'L'                       )
#define T27 _NE(OEM_1,   OEM_PLUS,OEM_1,   OEM_1,   OEM_1,   OEM_1   )
#define T28 _NE(OEM_7,   OEM_3,   OEM_7,   OEM_7,   OEM_3,   OEM_3   )
#define T29 _NE(OEM_3,   OEM_7,   OEM_3,   OEM_3,   OEM_7,   OEM_7   )
#define T2A _EQ(                           LSHIFT                    )
#define T2B _EQ(                           OEM_5                     )
#define T2C _EQ(                           'Z'                       )
#define T2D _EQ(                           'X'                       )
#define T2E _EQ(                           'C'                       )
#define T2F _EQ(                           'V'                       )
#define T30 _EQ(                           'B'                       )
#define T31 _EQ(                           'N'                       )
#define T32 _EQ(                           'M'                       )
#define T33 _EQ(                           OEM_COMMA                 )
#define T34 _EQ(                           OEM_PERIOD                )
#define T35 _EQ(                           OEM_2                     )
#define T36 _EQ(                           RSHIFT                    )
#define T37 _EQ(                           MULTIPLY                  )
#define T38 _EQ(                           LMENU                     )
#define T39 _EQ(                           ' '                       )
#define T3A _EQ(                           CAPITAL                   )
#define T3B _EQ(                           F1                        )
#define T3C _EQ(                           F2                        )
#define T3D _EQ(                           F3                        )
#define T3E _EQ(                           F4                        )
#define T3F _EQ(                           F5                        )
#define T40 _EQ(                           F6                        )
#define T41 _EQ(                           F7                        )
#define T42 _EQ(                           F8                        )
#define T43 _EQ(                           F9                        )
#define T44 _EQ(                           F10                       )
#define T45 _EQ(                           NUMLOCK                   )
#define T46 _EQ(                           OEM_SCROLL                )
#define T47 _EQ(                           HOME                      )
#define T48 _EQ(                           UP                        )
#define T49 _EQ(                           PRIOR                     )
#define T4A _EQ(                           SUBTRACT                  )
#define T4B _EQ(                           LEFT                      )
#define T4C _EQ(                           CLEAR                     )
#define T4D _EQ(                           RIGHT                     )
#define T4E _EQ(                           ADD                       )
#define T4F _EQ(                           END                       )
#define T50 _EQ(                           DOWN                      )
#define T51 _EQ(                           NEXT                      )
#define T52 _EQ(                           INSERT                    )
#define T53 _EQ(                           DELETE                    )
#define T54 _EQ(                           SNAPSHOT                  )
#define T55 _EQ(                           _none_                    ) // KB3270 => DOWN
#define T56 _NE(OEM_102, HELP,    OEM_102, OEM_102, _none_,  OEM_PA2 ) // KB3270 => LEFT
#define T57 _NE(F11,     RETURN,  F11,     F11,     _none_,  HELP    ) // KB3270 => ZOOM
#define T58 _NE(F12,     LEFT,    F12,     F12,     _none_,  OEM_102 ) // KB3270 => HELP
#define T59 _EQ(                           CLEAR                     )
#define T5A _EQ(                           OEM_WSCTRL                )// WSCtrl
#define T5B _EQ(                           OEM_FINISH                )// Finish
#define T5C _EQ(                           OEM_JUMP                  )// Jump
#define T5D _EQ(                           EREOF                     )
#define T5E _EQ(                           OEM_BACKTAB               ) // KB3270 <= 7E
#define T5F _EQ(                           OEM_AUTO                  ) // KB3270
#define T60 _EQ(                           _none_                    )
#define T61 _EQ(                           _none_                    )
#define T62 _EQ(                           ZOOM                      ) // KB3270 <= 57
#define T63 _EQ(                           HELP                      ) // KB3270 <= 58
#define T64 _EQ(                           F13                       )
#define T65 _EQ(                           F14                       )
#define T66 _EQ(                           F15                       )
#define T67 _EQ(                           F16                       )
#define T68 _EQ(                           F17                       )
#define T69 _EQ(                           F18                       )
#define T6A _EQ(                           F19                       )
#define T6B _EQ(                           F20                       )
#define T6C _EQ(                           F21                       )
#define T6D _EQ(                           F22                       )
#define T6E _EQ(                           F23                       )
#define T6F _EQ(                           OEM_PA3                   ) // KB3270
#define T70 _EQ(                           _none_                    )
#define T71 _EQ(                           OEM_RESET                 ) // KB3270
#define T72 _EQ(                           _none_                    )
#define T73 _EQ(                           ABNT_C1                   )
#define T74 _EQ(                           _none_                    )
#define T75 _EQ(                           _none_                    ) // KB3270 => RETURN
#define T76 _EQ(                           F24                       )
#define T77 _EQ(                           _none_                    ) // KB3270 => HOME
#define T78 _EQ(                           _none_                    ) // KB3270 => UP
#define T79 _EQ(                           _none_                    ) // KB3270 => DELETE
#define T7A _EQ(                           _none_                    ) // KB3270 => INSERT
#define T7B _EQ(                           OEM_PA1                   ) // KB3270
#define T7C _EQ(                           TAB                       ) // KB3270 => TAB
#define T7D _EQ(                           _none_                    ) // KB3270 => RIGHT
#define T7E _EQ(                           ABNT_C2                   ) // KB3270 => BACKTAB
#define T7F _EQ(                           OEM_PA2                   ) // KB3270

#define X1C _EQ(                           RETURN                    )
#define X1D _EQ(                           RCONTROL                  )
#define X35 _EQ(                           DIVIDE                    )
#define X37 _EQ(                           SNAPSHOT                  )
#define X38 _EQ(                           RMENU                     )
#define X46 _EQ(                           CANCEL                    )
#define X47 _EQ(                           HOME                      )
#define X48 _EQ(                           UP                        )
#define X49 _EQ(                           PRIOR                     )
#define X4B _EQ(                           LEFT                      )
#define X4D _EQ(                           RIGHT                     )
#define X4F _EQ(                           END                       )
#define X50 _EQ(                           DOWN                      )
#define X51 _NE(NEXT,    F1,      NEXT,    NEXT,    _none_, OEM_PA2  )
#define X52 _EQ(                           INSERT                    )
#define X53 _EQ(                           DELETE                    )
#define X5B _EQ(                           LWIN                      )
#define X5C _EQ(                           RWIN                      )
#define X5D _EQ(                           APPS                      )

        /*
         * The break key is sent to us as E1,LCtrl,NumLock
         * We must convert the E1+LCtrl to BREAK, then ignore the Numlock
         */
#define Y1D _EQ(                           PAUSE                     )

#define SCANCODE_LSHIFT      0x2A
#define SCANCODE_RSHIFT      0x36
#define SCANCODE_SIMULATED   (FAKE_KEYSTROKE >> 16)

#define SCANCODE_NUMPAD_FIRST 0x47
#define SCANCODE_NUMPAD_LAST  0x52

#endif // _KBD_
