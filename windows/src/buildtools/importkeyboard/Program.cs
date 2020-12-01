using System;
using System.Text;
using System.IO;
using System.Collections;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Collections.Specialized;
using Microsoft.Win32;

namespace KeyboardLayouts
{

  public enum KeysEx : byte
  {
    None = 0x00,
    VK_LBUTTON = Keys.LButton,             // 0x01
    VK_RBUTTON = Keys.RButton,             // 0x02
    VK_CANCEL = Keys.Cancel,              // 0x03
    VK_MBUTTON = Keys.MButton,             // 0x04
    VK_XBUTTON1 = Keys.XButton1,            // 0x05
    VK_XBUTTON2 = Keys.XButton2,            // 0x06
    /*
     * 0x07 : unassigned
     */
    VK_BACK = Keys.Back,                // 0x08
    VK_TAB = Keys.Tab,                 // 0x09
    /*
     * 0x0A - 0x0B : reserved
     */
    VK_CLEAR = Keys.Clear,               // 0x0C
    VK_RETURN = Keys.Return,              // 0x0D, Keys.Enter
    VK_SHIFT = Keys.ShiftKey,            // 0x10
    VK_CONTROL = Keys.ControlKey,          // 0x11
    VK_MENU = Keys.Menu,                // 0x12
    VK_PAUSE = Keys.Pause,               // 0x13
    VK_CAPITAL = Keys.Capital,             // 0x14, Keys.CapsLock
    VK_KANA = Keys.KanaMode,            // 0x15
    VK_HANGEUL = Keys.HanguelMode,         // 0x15, Keys.HangulMode
    VK_JUNJA = Keys.JunjaMode,           // 0x17
    VK_FINAL = Keys.FinalMode,           // 0x18
    VK_HANJA = Keys.HanjaMode,           // 0x19
    VK_KANJI = Keys.KanjiMode,           // 0x19
    VK_ESCAPE = Keys.Escape,              // 0x1B
    VK_CONVERT = Keys.IMEConvert,          // 0x1C
    VK_NONCONVERT = Keys.IMENonconvert,       // 0x1D
    VK_ACCEPT = Keys.IMEAceept,           // 0x1E, Keys.IMEAccept
    VK_MODECHANGE = Keys.IMEModeChange,       // 0x1F
    VK_SPACE = Keys.Space,               // 0x20
    VK_PRIOR = Keys.Prior,               // 0x21, Keys.PageUp
    VK_NEXT = Keys.Next,                // 0x22, Keys.PageDown
    VK_END = Keys.End,                 // 0x23
    VK_HOME = Keys.Home,                // 0x24
    VK_LEFT = Keys.Left,                // 0x25
    VK_UP = Keys.Up,                  // 0x26
    VK_RIGHT = Keys.Right,               // 0x27
    VK_DOWN = Keys.Down,                // 0x28
    VK_SELECT = Keys.Select,              // 0x29
    VK_PRINT = Keys.Print,               // 0x2A
    VK_EXECUTE = Keys.Execute,             // 0x2B
    VK_SNAPSHOT = Keys.Snapshot,            // 0x2C, Keys.PrintScreen
    VK_INSERT = Keys.Insert,              // 0x2D
    VK_DELETE = Keys.Delete,              // 0x2E
    VK_HELP = Keys.Help,                // 0x2F
    VK_0 = Keys.D0,                  // 0x30
    VK_1 = Keys.D1,                  // 0x31
    VK_2 = Keys.D2,                  // 0x32
    VK_3 = Keys.D3,                  // 0x33
    VK_4 = Keys.D4,                  // 0x34
    VK_5 = Keys.D5,                  // 0x35
    VK_6 = Keys.D6,                  // 0x36
    VK_7 = Keys.D7,                  // 0x37
    VK_8 = Keys.D8,                  // 0x38
    VK_9 = Keys.D9,                  // 0x39
    /*
     * 0x40 : unassigned
     */
    VK_A = Keys.A,                   // 0x41
    VK_B = Keys.B,                   // 0x42
    VK_C = Keys.C,                   // 0x43
    VK_D = Keys.D,                   // 0x44
    VK_E = Keys.E,                   // 0x45
    VK_F = Keys.F,                   // 0x46
    VK_G = Keys.G,                   // 0x47
    VK_H = Keys.H,                   // 0x48
    VK_I = Keys.I,                   // 0x49
    VK_J = Keys.J,                   // 0x4A
    VK_K = Keys.K,                   // 0x4B
    VK_L = Keys.L,                   // 0x4C
    VK_M = Keys.M,                   // 0x4D
    VK_N = Keys.N,                   // 0x4E
    VK_O = Keys.O,                   // 0x4F
    VK_P = Keys.P,                   // 0x50
    VK_Q = Keys.Q,                   // 0x51
    VK_R = Keys.R,                   // 0x52
    VK_S = Keys.S,                   // 0x53
    VK_T = Keys.T,                   // 0x54
    VK_U = Keys.U,                   // 0x55
    VK_V = Keys.V,                   // 0x56
    VK_W = Keys.W,                   // 0x57
    VK_X = Keys.X,                   // 0x58
    VK_Y = Keys.Y,                   // 0x59
    VK_Z = Keys.Z,                   // 0x5A
    VK_LWIN = Keys.LWin,                // 0x5B
    VK_RWIN = Keys.RWin,                // 0x5C
    VK_APPS = Keys.Apps,                // 0x5D
    /*
     * 0x5E : reserved
     */
    VK_SLEEP = 0x5f,                     // 0x5f, Keys.Sleep
    VK_NUMPAD0 = Keys.NumPad0,             // 0x60
    VK_NUMPAD1 = Keys.NumPad1,             // 0x61
    VK_NUMPAD2 = Keys.NumPad2,             // 0x62
    VK_NUMPAD3 = Keys.NumPad3,             // 0x63
    VK_NUMPAD4 = Keys.NumPad4,             // 0x64
    VK_NUMPAD5 = Keys.NumPad5,             // 0x65
    VK_NUMPAD6 = Keys.NumPad6,             // 0x66
    VK_NUMPAD7 = Keys.NumPad7,             // 0x67
    VK_NUMPAD8 = Keys.NumPad8,             // 0x68
    VK_NUMPAD9 = Keys.NumPad9,             // 0x69
    VK_MULTIPLY = Keys.Multiply,            // 0x6A
    VK_ADD = Keys.Add,                 // 0x6B
    VK_SEPARATOR = Keys.Separator,           // 0x6C
    VK_SUBTRACT = Keys.Subtract,            // 0x6D
    VK_DECIMAL = Keys.Decimal,             // 0x6E
    VK_DIVIDE = Keys.Divide,              // 0x6F
    VK_F1 = Keys.F1,                  // 0x70
    VK_F2 = Keys.F2,                  // 0x71
    VK_F3 = Keys.F3,                  // 0x72
    VK_F4 = Keys.F4,                  // 0x73
    VK_F5 = Keys.F5,                  // 0x74
    VK_F6 = Keys.F6,                  // 0x75
    VK_F7 = Keys.F7,                  // 0x76
    VK_F8 = Keys.F8,                  // 0x77
    VK_F9 = Keys.F9,                  // 0x78
    VK_F10 = Keys.F10,                 // 0x79
    VK_F11 = Keys.F11,                 // 0x7A
    VK_F12 = Keys.F12,                 // 0x7B
    VK_F13 = Keys.F13,                 // 0x7C
    VK_F14 = Keys.F14,                 // 0x7D
    VK_F15 = Keys.F15,                 // 0x7E
    VK_F16 = Keys.F16,                 // 0x7F
    VK_F17 = Keys.F17,                 // 0x80
    VK_F18 = Keys.F18,                 // 0x81
    VK_F19 = Keys.F19,                 // 0x82
    VK_F20 = Keys.F20,                 // 0x83
    VK_F21 = Keys.F21,                 // 0x84
    VK_F22 = Keys.F22,                 // 0x85
    VK_F23 = Keys.F23,                 // 0x86
    VK_F24 = Keys.F24,                 // 0x87
    /*
     * 0x88 - 0x8F : unassigned
     */
    VK_NUMLOCK = Keys.NumLock,             // 0x90
    VK_SCROLL = Keys.Scroll,              // 0x91
    VK_OEM_NEC_EQUAL = 0x92,                     // 0x92, NEC PC-9800 kbd definition
    VK_OEM_FJ_JISHO = 0x92,                     // 0x92, Fujitsu/OASYS kbd definition
    VK_OEM_FJ_MASSHOU = 0x93,                     // 0x93, Fujitsu/OASYS kbd definition
    VK_OEM_FJ_TOUROKU = 0x94,                     // 0x94, Fujitsu/OASYS kbd definition
    VK_OEM_FJ_LOYA = 0x95,                     // 0x95, Fujitsu/OASYS kbd definition
    VK_OEM_FJ_ROYA = 0x96,                     // 0x96, Fujitsu/OASYS kbd definition
    /*
     * 0x97 - 0x9F : unassigned
     */
    VK_LSHIFT = Keys.LShiftKey,           // 0xA0
    VK_RSHIFT = Keys.RShiftKey,           // 0xA1
    VK_LCONTROL = Keys.LControlKey,         // 0xA2
    VK_RCONTROL = Keys.RControlKey,         // 0xA3
    VK_LMENU = Keys.LMenu,               // 0xA4
    VK_RMENU = Keys.RMenu,               // 0xA5
    VK_BROWSER_BACK = Keys.BrowserBack,         // 0xA6
    VK_BROWSER_FORWARD = Keys.BrowserForward,      // 0xA7
    VK_BROWSER_REFRESH = Keys.BrowserRefresh,      // 0xA8
    VK_BROWSER_STOP = Keys.BrowserStop,         // 0xA9
    VK_BROWSER_SEARCH = Keys.BrowserSearch,       // 0xAA
    VK_BROWSER_FAVORITES = Keys.BrowserFavorites,    // 0xAB
    VK_BROWSER_HOME = Keys.BrowserHome,         // 0xAC
    VK_VOLUME_MUTE = Keys.VolumeMute,          // 0xAD
    VK_VOLUME_DOWN = Keys.VolumeDown,          // 0xAE
    VK_VOLUME_UP = Keys.VolumeUp,            // 0xAF
    VK_MEDIA_NEXT_TRACK = Keys.MediaNextTrack,      // 0xB0
    VK_MEDIA_PREV_TRACK = Keys.MediaPreviousTrack,  // 0xB1
    VK_MEDIA_STOP = Keys.MediaStop,           // 0xB2
    VK_MEDIA_PLAY_PAUSE = Keys.MediaPlayPause,      // 0xB3
    VK_LAUNCH_MAIL = Keys.LaunchMail,          // 0xB4
    VK_LAUNCH_MEDIA_SELECT = Keys.SelectMedia,         // 0xB5
    VK_LAUNCH_APP1 = Keys.LaunchApplication1,  // 0xB6
    VK_LAUNCH_APP2 = Keys.LaunchApplication2,  // 0xB7
    /*
     * 0xB8 - 0xB9 : reserved
     */
    VK_OEM_1 = Keys.OemSemicolon,        // 0xBA, Keys.Oem1
    VK_OEM_PLUS = Keys.Oemplus,             // 0xBB
    VK_OEM_COMMA = Keys.Oemcomma,            // 0xBC
    VK_OEM_MINUS = Keys.OemMinus,            // 0xBD
    VK_OEM_PERIOD = Keys.OemPeriod,           // 0xBE
    VK_OEM_2 = Keys.OemQuestion,         // 0xBF, Keys.Oem2
    VK_OEM_3 = Keys.Oemtilde,            // 0xC0, Keys.Oem3
    /*
     * 0xC1 - 0xD7 : reserved
     */
    /*
     * 0xD8 - 0xDA : unassigned
     */
    VK_OEM_4 = Keys.OemOpenBrackets,     // 0xDB, Keys.Oem4
    VK_OEM_5 = Keys.OemPipe,             // 0xDC, Keys.Oem5
    VK_OEM_6 = Keys.OemCloseBrackets,    // 0xDD, Keys.Oem6
    VK_OEM_7 = Keys.OemQuotes,           // 0xDE, Keys.Oem7
    VK_OEM_8 = Keys.Oem8,                // 0xDF
    /*
     * 0xE0 : reserved
     */
    VK_OEM_AX = 0xE1,                     // 0xE1, 'AX' key on Japanese AX kbd
    VK_OEM_102 = Keys.OemBackslash,        // 0xE2, Keys.Oem102
    VK_ICO_HELP = 0xE3,                     // 0xE3, Help key on ICO
    VK_ICO_00 = 0xE4,                     // 0xE4, 00 key on ICO
    VK_PROCESSKEY = Keys.ProcessKey,          // 0xE5
    VK_ICO_CLEAR = 0xE6,                     // 0xE6
    VK_PACKET = 0xE7,                     // 0xE7, Keys.Packet
    /*
     * 0xE8 : unassigned
     */
    VK_OEM_RESET = 0xE9,                     // 0xE9, Nokia/Ericsson definition
    VK_OEM_JUMP = 0xEA,                     // 0xEA, Nokia/Ericsson definition
    VK_OEM_PA1 = 0xEB,                     // 0xEB, Nokia/Ericsson definition
    VK_OEM_PA2 = 0xEC,                     // 0xEC, Nokia/Ericsson definition
    VK_OEM_PA3 = 0xED,                     // 0xED, Nokia/Ericsson definition
    VK_OEM_WSCTRL = 0xEE,                     // 0xEE, Nokia/Ericsson definition
    VK_OEM_CUSEL = 0xEF,                     // 0xEF, Nokia/Ericsson definition
    VK_OEM_ATTN = 0xF0,                     // 0xF0, Nokia/Ericsson definition
    VK_OEM_FINISH = 0xF1,                     // 0xF1, Nokia/Ericsson definition
    VK_OEM_COPY = 0xF2,                     // 0xF2, Nokia/Ericsson definition
    VK_OEM_AUTO = 0xF3,                     // 0xF3, Nokia/Ericsson definition
    VK_OEM_ENLW = 0xF4,                     // 0xF4, Nokia/Ericsson definition
    VK_OEM_BACKTAB = 0xF5,                     // 0xF5, Nokia/Ericsson definition
    VK_ATTN = Keys.Attn,                // 0xF6
    VK_CRSEL = Keys.Crsel,               // 0xF7
    VK_EXSEL = Keys.Exsel,               // 0xF8
    VK_EREOF = Keys.EraseEof,            // 0xF9
    VK_PLAY = Keys.Play,                // 0xFA
    VK_ZOOM = Keys.Zoom,                // 0xFB
    VK_NONAME = Keys.NoName,              // 0xFC
    VK_PA1 = Keys.Pa1,                 // 0xFD
    VK_OEM_CLEAR = Keys.OemClear,            // 0xFE
    /*
     * 0xFF : reserved
     */
  }

  public enum ShiftState : int
  {
    Base = 0,                    // 0
    Shft = 1,                    // 1
    Ctrl = 2,                    // 2
    ShftCtrl = Shft | Ctrl,          // 3
    Menu = 4,                    // 4 -- NOT USED
    ShftMenu = Shft | Menu,          // 5 -- NOT USED
    MenuCtrl = Menu | Ctrl,          // 6
    ShftMenuCtrl = Shft | Menu | Ctrl,   // 7
    Xxxx = 8,                    // 8
    ShftXxxx = Shft | Xxxx,          // 9
  }

  public class DeadKey
  {
    private char m_deadchar;
    private ArrayList m_rgbasechar = new ArrayList();
    private ArrayList m_rgcombchar = new ArrayList();

    public DeadKey(char deadCharacter)
    {
      this.m_deadchar = deadCharacter;
    }

    public char DeadCharacter
    {
      get
      {
        return this.m_deadchar;
      }
    }

    public void AddDeadKeyRow(char baseCharacter, char combinedCharacter)
    {
      this.m_rgbasechar.Add(baseCharacter);
      this.m_rgcombchar.Add(combinedCharacter);
    }

    public int Count
    {
      get
      {
        return this.m_rgbasechar.Count;
      }
    }

    public char GetBaseCharacter(int index)
    {
      return (char)this.m_rgbasechar[index];
    }

    public char GetCombinedCharacter(int index)
    {
      return (char)this.m_rgcombchar[index];
    }

    public bool ContainsBaseCharacter(char baseCharacter)
    {
      return this.m_rgbasechar.Contains(baseCharacter);
    }
  }

  public class VirtualKey
  {
    [DllImport("user32.dll", CharSet = CharSet.Unicode, EntryPoint = "MapVirtualKeyExW", ExactSpelling = true)]
    internal static extern uint MapVirtualKeyEx(
        uint uCode,
        uint uMapType,
        IntPtr dwhkl);

    private IntPtr m_hkl;
    private uint m_vk;
    private uint m_sc;
    private bool[,] m_rgfDeadKey = new bool[(int)ShiftState.ShftXxxx + 1, 2];
    private string[,] m_rgss = new string[(int)ShiftState.ShftXxxx + 1, 2];

    public VirtualKey(IntPtr hkl, KeysEx virtualKey)
    {
      this.m_sc = MapVirtualKeyEx((uint)virtualKey, 0, hkl);
      this.m_hkl = hkl;
      this.m_vk = (uint)virtualKey;
    }

    public VirtualKey(IntPtr hkl, uint scanCode)
    {
      this.m_vk = MapVirtualKeyEx(scanCode, 1, hkl);
      this.m_hkl = hkl;
      this.m_sc = scanCode;
    }

    public KeysEx VK
    {
      get { return (KeysEx)this.m_vk; }
    }

    public uint SC
    {
      get { return this.m_sc; }
    }

    public string GetShiftState(ShiftState shiftState, bool capsLock)
    {
      if (this.m_rgss[(uint)shiftState, (capsLock ? 1 : 0)] == null)
      {
        return ("");
      }
      return (this.m_rgss[(uint)shiftState, (capsLock ? 1 : 0)]);
    }

    public void SetShiftState(ShiftState shiftState, string value, bool isDeadKey, bool capsLock)
    {
      this.m_rgfDeadKey[(uint)shiftState, (capsLock ? 1 : 0)] = isDeadKey;
      this.m_rgss[(uint)shiftState, (capsLock ? 1 : 0)] = value;
    }

    public bool IsSGCAPS
    {
      get
      {
        string stBase = this.GetShiftState(ShiftState.Base, false);
        string stShift = this.GetShiftState(ShiftState.Shft, false);
        string stCaps = this.GetShiftState(ShiftState.Base, true);
        string stShiftCaps = this.GetShiftState(ShiftState.Shft, true);
        return (
            ((stCaps.Length > 0) &&
            (!stBase.Equals(stCaps)) &&
            (!stShift.Equals(stCaps))) ||
            ((stShiftCaps.Length > 0) &&
            (!stBase.Equals(stShiftCaps)) &&
            (!stShift.Equals(stShiftCaps))));
      }
    }

    public bool IsCapsEqualToShift
    {
      get
      {
        string stBase = this.GetShiftState(ShiftState.Base, false);
        string stShift = this.GetShiftState(ShiftState.Shft, false);
        string stCaps = this.GetShiftState(ShiftState.Base, true);
        return (
            (stBase.Length > 0) &&
            (stShift.Length > 0) &&
            (!stBase.Equals(stShift)) &&
            (stShift.Equals(stCaps)));
      }
    }

    public bool IsAltGrCapsEqualToAltGrShift
    {
      get
      {
        string stBase = this.GetShiftState(ShiftState.MenuCtrl, false);
        string stShift = this.GetShiftState(ShiftState.ShftMenuCtrl, false);
        string stCaps = this.GetShiftState(ShiftState.MenuCtrl, true);
        return (
            (stBase.Length > 0) &&
            (stShift.Length > 0) &&
            (!stBase.Equals(stShift)) &&
            (stShift.Equals(stCaps)));
      }
    }

    public bool IsXxxxGrCapsEqualToXxxxShift
    {
      get
      {
        string stBase = this.GetShiftState(ShiftState.Xxxx, false);
        string stShift = this.GetShiftState(ShiftState.ShftXxxx, false);
        string stCaps = this.GetShiftState(ShiftState.Xxxx, true);
        return (
            (stBase.Length > 0) &&
            (stShift.Length > 0) &&
            (!stBase.Equals(stShift)) &&
            (stShift.Equals(stCaps)));
      }
    }

    public bool IsEmpty
    {
      get
      {
        for (int i = 0; i < this.m_rgss.GetUpperBound(0); i++)
        {
          for (int j = 0; j <= 1; j++)
          {
            if (this.GetShiftState((ShiftState)i, (j == 1)).Length > 0)
            {
              return (false);
            }
          }
        }
        return true;
      }
    }

    public bool IsKeymanUsedKey
    {
      get
      {
        return (this.m_vk >= 0x20 && this.m_vk <= 0x5F) || (this.m_vk >= 0x88);
      }
    }

    public string GetShiftStateName(int capslock, int caps, ShiftState ss, Boolean IsKMW)
    {
      StringBuilder sbState = new StringBuilder();
      if(capslock != 0)
        if(caps == 1)  sbState.Append("CAPS ");
        else sbState.Append("NCAPS ");

      switch(ss)
      {
        case ShiftState.Base: break;
        case ShiftState.Shft:  sbState.Append( "SHIFT "); break;
        case ShiftState.Ctrl:  sbState.Append( "CTRL "); break;
        case ShiftState.ShftCtrl:  sbState.Append( "SHIFT CTRL "); break;
        case ShiftState.Menu:  sbState.Append( "ALT "); break;
        case ShiftState.ShftMenu:  sbState.Append( "SHIFT ALT "); break;
        case ShiftState.MenuCtrl:  sbState.Append( IsKMW ? "CTRL ALT " : "RALT "); break;
        case ShiftState.ShftMenuCtrl:  sbState.Append( IsKMW ? "SHIFT CTRL ALT " : "SHIFT RALT "); break;
        case ShiftState.Xxxx:  sbState.Append( "XXXX "); break;
        case ShiftState.ShftXxxx: sbState.Append("SHIFT XXXX "); break;
        default: break;
      }
      return sbState.ToString();
    }

  private string[] VKeyNames = {
  // Key Codes
	  "K_?00",				// &H0
	  "K_LBUTTON",			// &H1
	  "K_RBUTTON",			// &H2
	  "K_CANCEL",		   	// &H3
	  "K_MBUTTON",			// &H4
	  "K_?05",				// &H5
	  "K_?06",				// &H6
	  "K_?07",				// &H7
	  "K_BKSP",	    		// &H8
	  "K_TAB",	    		// &H9
	  "K_?0A",				// &HA
	  "K_?0B",				// &HB
	  "K_KP5",		    	// &HC
	  "K_ENTER",				// &HD
	  "K_?0E",				// &HE
	  "K_?0F",				// &HF
	  "K_SHIFT",				// &H10
	  "K_CONTROL",			// &H11
	  "K_ALT",				// &H12
	  "K_PAUSE",				// &H13
	  "K_CAPS",				// &H14
	  "K_KANJI?15",			// &H15
	  "K_KANJI?16",			// &H16
	  "K_KANJI?17",			// &H17
	  "K_KANJI?18",			// &H18
	  "K_KANJI?19",			// &H19
	  "K_?1A",				// &H1A
	  "K_ESC",				// &H1B
	  "K_KANJI?1C",			// &H1C
	  "K_KANJI?1D",			// &H1D
	  "K_KANJI?1E",			// &H1E
	  "K_KANJI?1F",			// &H1F
	  "K_SPACE",				// &H20
	  "K_PGUP",				// &H21
	  "K_PGDN",				// &H22
	  "K_END",				// &H23
	  "K_HOME",				// &H24
	  "K_LEFT",				// &H25
	  "K_UP",				// &H26
	  "K_RIGHT",				// &H27
	  "K_DOWN",				// &H28
	  "K_SEL",				// &H29
	  "K_PRINT",				// &H2A
	  "K_EXEC",				// &H2B
	  "K_PRTSCN",			// &H2C
	  "K_INS",				// &H2D
	  "K_DEL",				// &H2E
	  "K_HELP",				// &H2F
	  "K_0",					// &H30
	  "K_1",					// &H31
	  "K_2",					// &H32
	  "K_3",					// &H33
	  "K_4",					// &H34
	  "K_5",					// &H35
	  "K_6",					// &H36
	  "K_7",					// &H37
	  "K_8",					// &H38
	  "K_9",					// &H39
	  "K_?3A",				// &H3A
	  "K_?3B",				// &H3B
	  "K_?3C",				// &H3C
	  "K_?3D",				// &H3D
	  "K_?3E",				// &H3E
	  "K_?3F",				// &H3F
	  "K_?40",				// &H40

	  "K_A",					// &H41
	  "K_B",					// &H42
	  "K_C",					// &H43
	  "K_D",					// &H44
	  "K_E",					// &H45
	  "K_F",					// &H46
	  "K_G",					// &H47
	  "K_H",					// &H48
	  "K_I",					// &H49
	  "K_J",					// &H4A
	  "K_K",					// &H4B
	  "K_L",					// &H4C
	  "K_M",					// &H4D
	  "K_N",					// &H4E
	  "K_O",					// &H4F
	  "K_P",					// &H50
	  "K_Q",					// &H51
	  "K_R",					// &H52
	  "K_S",					// &H53
	  "K_T",					// &H54
	  "K_U",					// &H55
	  "K_V",					// &H56
	  "K_W",					// &H57
	  "K_X",					// &H58
	  "K_Y",					// &H59
	  "K_Z",					// &H5A
	  "K_?5B",				// &H5B
	  "K_?5C",				// &H5C
	  "K_?5D",				// &H5D
	  "K_?5E",				// &H5E
	  "K_?5F",				// &H5F
	  "K_NP0",				// &H60
	  "K_NP1",				// &H61
	  "K_NP2",				// &H62
	  "K_NP3",				// &H63
	  "K_NP4",				// &H64
	  "K_NP5",				// &H65
	  "K_NP6",				// &H66
	  "K_NP7",				// &H67
	  "K_NP8",				// &H68
	  "K_NP9",				// &H69
	  "K_NPSTAR",			// &H6A
	  "K_NPPLUS",			// &H6B
	  "K_SEPARATOR",			// &H6C
	  "K_NPMINUS",			// &H6D
	  "K_NPDOT",				// &H6E
	  "K_NPSLASH",			// &H6F
	  "K_F1",				// &H70
	  "K_F2",				// &H71
	  "K_F3",				// &H72
	  "K_F4",				// &H73
	  "K_F5",				// &H74
	  "K_F6",				// &H75
	  "K_F7",				// &H76
	  "K_F8",				// &H77
	  "K_F9",				// &H78
	  "K_F10",				// &H79
	  "K_F11",				// &H7A
	  "K_F12",				// &H7B
	  "K_F13",				// &H7C
	  "K_F14",				// &H7D
	  "K_F15",				// &H7E
	  "K_F16",				// &H7F
	  "K_F17",				// &H80
	  "K_F18",				// &H81
	  "K_F19",				// &H82
	  "K_F20",				// &H83
	  "K_F21",				// &H84
	  "K_F22",				// &H85
	  "K_F23",				// &H86
	  "K_F24",				// &H87

	  "K_?88",				// &H88
	  "K_?89",				// &H89
	  "K_?8A",				// &H8A
	  "K_?8B",				// &H8B
	  "K_?8C",				// &H8C
	  "K_?8D",				// &H8D
	  "K_?8E",				// &H8E
	  "K_?8F",				// &H8F

	  "K_NUMLOCK",			// &H90
	  "K_SCROLL",			// &H91

	  "K_?92",				// &H92
	  "K_?93",				// &H93
	  "K_?94",				// &H94
	  "K_?95",				// &H95
	  "K_?96",				// &H96
	  "K_?97",				// &H97
	  "K_?98",				// &H98
	  "K_?99",				// &H99
	  "K_?9A",				// &H9A
	  "K_?9B",				// &H9B
	  "K_?9C",				// &H9C
	  "K_?9D",				// &H9D
	  "K_?9E",				// &H9E
	  "K_?9F",				// &H9F
	  "K_?A0",				// &HA0
	  "K_?A1",				// &HA1
	  "K_?A2",				// &HA2
	  "K_?A3",				// &HA3
	  "K_?A4",				// &HA4
	  "K_?A5",				// &HA5
	  "K_?A6",				// &HA6
	  "K_?A7",				// &HA7
	  "K_?A8",				// &HA8
	  "K_?A9",				// &HA9
	  "K_?AA",				// &HAA
	  "K_?AB",				// &HAB
	  "K_?AC",				// &HAC
	  "K_?AD",				// &HAD
	  "K_?AE",				// &HAE
	  "K_?AF",				// &HAF
	  "K_?B0",				// &HB0
	  "K_?B1",				// &HB1
	  "K_?B2",				// &HB2
	  "K_?B3",				// &HB3
	  "K_?B4",				// &HB4
	  "K_?B5",				// &HB5
	  "K_?B6",				// &HB6
	  "K_?B7",				// &HB7
	  "K_?B8",				// &HB8
	  "K_?B9",				// &HB9

	  "K_COLON",				// &HBA
	  "K_EQUAL",				// &HBB
	  "K_COMMA",				// &HBC
	  "K_HYPHEN",			// &HBD
	  "K_PERIOD",			// &HBE
	  "K_SLASH",				// &HBF
	  "K_BKQUOTE",			// &HC0

	  "K_?C1",				// &HC1
	  "K_?C2",				// &HC2
	  "K_?C3",				// &HC3
	  "K_?C4",				// &HC4
	  "K_?C5",				// &HC5
	  "K_?C6",				// &HC6
	  "K_?C7",				// &HC7
	  "K_?C8",				// &HC8
	  "K_?C9",				// &HC9
	  "K_?CA",				// &HCA
	  "K_?CB",				// &HCB
	  "K_?CC",				// &HCC
	  "K_?CD",				// &HCD
	  "K_?CE",				// &HCE
	  "K_?CF",				// &HCF
	  "K_?D0",				// &HD0
	  "K_?D1",				// &HD1
	  "K_?D2",				// &HD2
	  "K_?D3",				// &HD3
	  "K_?D4",				// &HD4
	  "K_?D5",				// &HD5
	  "K_?D6",				// &HD6
	  "K_?D7",				// &HD7
	  "K_?D8",				// &HD8
	  "K_?D9",				// &HD9
	  "K_?DA",				// &HDA

	  "K_LBRKT",				// &HDB
	  "K_BKSLASH",			// &HDC
	  "K_RBRKT",				// &HDD
	  "K_QUOTE",				// &HDE
	  "K_oDF",				// &HDF
	  "K_oE0",				// &HE0
	  "K_oE1",				// &HE1
	  "K_oE2",				// &HE2
	  "K_oE3",				// &HE3
	  "K_oE4",				// &HE4

	  "K_?E5",				// &HE5

	  "K_oE6",				// &HE6

	  "K_?E7",				// &HE7
	  "K_?E8",				// &HE8

	  "K_oE9",				// &HE9
	  "K_oEA",				// &HEA
	  "K_oEB",				// &HEB
	  "K_oEC",				// &HEC
	  "K_oED",				// &HED
	  "K_oEE",				// &HEE
	  "K_oEF",				// &HEF
	  "K_oF0",				// &HF0
	  "K_oF1",				// &HF1
	  "K_oF2",				// &HF2
	  "K_oF3",				// &HF3
	  "K_oF4",				// &HF4
	  "K_oF5",				// &HF5

	  "K_?F6",				// &HF6
	  "K_?F7",				// &HF7
	  "K_?F8",				// &HF8
	  "K_?F9",				// &HF9
	  "K_?FA",				// &HFA
	  "K_?FB",				// &HFB
	  "K_?FC",				// &HFC
	  "K_?FD",				// &HFD
	  "K_?FE",				// &HFE
	  "K_?FF"};				// &HFF

  private uint[] USVirtualKeyToScanCode =
{
	0x00, // L"K_?00",				// &H0
	0x00, // L"K_LBUTTON",			// &H1
	0x00, // L"K_RBUTTON",			// &H2
	0x46, // L"K_CANCE0x00, // L",		   	// &H3
	0x00, // L"K_MBUTTON",			// &H4
	0x00, // L"K_?05",				// &H5
	0x00, // L"K_?06",				// &H6
	0x00, // L"K_?07",				// &H7
	0x0E, // L"K_BKSP",	    		// &H8
	0x0F, // L"K_TAB",	    		// &H9
	0x00, // L"K_?0A",				// &HA
	0x00, // L"K_?0B",				// &HB
	0x4C, // L"K_KP5",		    	// &HC
	0x1C, // L"K_ENTER",				// &HD
	0x00, // L"K_?0E",				// &HE
	0x00, // L"K_?0F",				// &HF
	0x2A, // L"K_SHIFT",				// &H10
	0x1D, // L"K_CONTRO0x00, // L",			// &H11
	0x38, // L"K_ALT",				// &H12
	0x00, // L"K_PAUSE",				// &H13
	0x3A, // L"K_CAPS",				// &H14
	0x00, // L"K_KANJI?15",			// &H15
	0x00, // L"K_KANJI?16",			// &H16
	0x00, // L"K_KANJI?17",			// &H17
	0x00, // L"K_KANJI?18",			// &H18
	0x00, // L"K_KANJI?19",			// &H19
	0x00, // L"K_?1A",				// &H1A
	0x01, // L"K_ESC",				// &H1B
	0x00, // L"K_KANJI?1C",			// &H1C
	0x00, // L"K_KANJI?1D",			// &H1D
	0x00, // L"K_KANJI?1E",			// &H1E
	0x00, // L"K_KANJI?1F",			// &H1F
	0x39, // L"K_SPACE",				// &H20
	0x49, // L"K_PGUP",				// &H21
	0x51, // L"K_PGDN",				// &H22
	0x4F, // L"K_END",				// &H23
	0x47, // L"K_HOME",				// &H24
	0x4B, // L"K_LEFT",				// &H25
	0x48, // L"K_UP",				// &H26
	0x4D, // L"K_RIGHT",				// &H27
	0x50, // L"K_DOWN",				// &H28
	0x00, // L"K_SEL",				// &H29
	0x00, // L"K_PRINT",				// &H2A
	0x00, // L"K_EXEC",				// &H2B
	0x54, // L"K_PRTSCN",			// &H2C
	0x52, // L"K_INS",				// &H2D
	0x53, // L"K_DEL",				// &H2E
	0x63, // L"K_HELP",				// &H2F
	0x0B, // L"K_0",					// &H30
	0x02, // L"K_1",					// &H31
	0x03, // L"K_2",					// &H32
	0x04, // L"K_3",					// &H33
	0x05, // L"K_4",					// &H34
	0x06, // L"K_5",					// &H35
	0x07, // L"K_6",					// &H36
	0x08, // L"K_7",					// &H37
	0x09, // L"K_8",					// &H38
	0x0A, // L"K_9",					// &H39
	0x00, // L"K_?3A",				// &H3A
	0x00, // L"K_?3B",				// &H3B
	0x00, // L"K_?3C",				// &H3C
	0x00, // L"K_?3D",				// &H3D
	0x00, // L"K_?3E",				// &H3E
	0x00, // L"K_?3F",				// &H3F
	0x00, // L"K_?40",				// &H40

	0x1E, // L"K_A",					// &H41
	0x30, // L"K_B",					// &H42
	0x2E, // L"K_C",					// &H43
	0x20, // L"K_D",					// &H44
	0x12, // L"K_E",					// &H45
	0x21, // L"K_F",					// &H46
	0x22, // L"K_G",					// &H47
	0x23, // L"K_H",					// &H48
	0x17, // L"K_I",					// &H49
	0x24, // L"K_J",					// &H4A
	0x25, // L"K_K",					// &H4B
	0x26, // L"K_L",					// &H4C
	0x32, // L"K_M",					// &H4D
	0x31, // L"K_N",					// &H4E
	0x18, // L"K_O",					// &H4F
	0x19, // L"K_P",					// &H50
	0x10, // L"K_Q",					// &H51
	0x13, // L"K_R",					// &H52
	0x1F, // L"K_S",					// &H53
	0x14, // L"K_T",					// &H54
	0x16, // L"K_U",					// &H55
	0x2F, // L"K_V",					// &H56
	0x11, // L"K_W",					// &H57
	0x2D, // L"K_X",					// &H58
	0x15, // L"K_Y",					// &H59
	0x2C, // L"K_Z",					// &H5A
	0x5B, // L"K_?5B",				// &H5B
	0x5C, // L"K_?5C",				// &H5C
	0x5D, // L"K_?5D",				// &H5D
	0x00, // L"K_?5E",				// &H5E
	0x5F, // L"K_?5F",				// &H5F
	0x52, // L"K_NP0",				// &H60
	0x4F, // L"K_NP1",				// &H61
	0x50, // L"K_NP2",				// &H62
	0x51, // L"K_NP3",				// &H63
	0x4B, // L"K_NP4",				// &H64
	0x4C, // L"K_NP5",				// &H65
	0x4D, // L"K_NP6",				// &H66
	0x47, // L"K_NP7",				// &H67
	0x48, // L"K_NP8",				// &H68
	0x49, // L"K_NP9",				// &H69
	0x37, // L"K_NPSTAR",			// &H6A
	0x4E, // L"K_NPPLUS",			// &H6B
	0x7E, // L"K_SEPARATOR",			// &H6C		// MCD 01-11-02: Brazilian Fix, 00 -> 7E
	0x4A, // L"K_NPMINUS",			// &H6D
	0x53, // L"K_NPDOT",				// &H6E
	0x135, // L"K_NPSLASH",			// &H6F
	0x3B, // L"K_F1",				// &H70
	0x3C, // L"K_F2",				// &H71
	0x3D, // L"K_F3",				// &H72
	0x3E, // L"K_F4",				// &H73
	0x3F, // L"K_F5",				// &H74
	0x40, // L"K_F6",				// &H75
	0x41, // L"K_F7",				// &H76
	0x42, // L"K_F8",				// &H77
	0x43, // L"K_F9",				// &H78
	0x44, // L"K_F10",				// &H79
	0x57, // L"K_F11",				// &H7A
	0x58, // L"K_F12",				// &H7B
	0x64, // L"K_F13",				// &H7C
	0x65, // L"K_F14",				// &H7D
	0x66, // L"K_F15",				// &H7E
	0x67, // L"K_F16",				// &H7F
	0x68, // L"K_F17",				// &H80
	0x69, // L"K_F18",				// &H81
	0x6A, // L"K_F19",				// &H82
	0x6B, // L"K_F20",				// &H83
	0x6C, // L"K_F21",				// &H84
	0x6D, // L"K_F22",				// &H85
	0x6E, // L"K_F23",				// &H86
	0x76, // L"K_F24",				// &H87

	0x00, // L"K_?88",				// &H88
	0x00, // L"K_?89",				// &H89
	0x00, // L"K_?8A",				// &H8A
	0x00, // L"K_?8B",				// &H8B
	0x00, // L"K_?8C",				// &H8C
	0x00, // L"K_?8D",				// &H8D
	0x00, // L"K_?8E",				// &H8E
	0x00, // L"K_?8F",				// &H8F

	0x45, // L"K_NUMLOCK",			// &H90
	0x46, // L"K_SCROL0x00, // L",			// &H91

	0x00, // L"K_?92",				// &H92
	0x00, // L"K_?93",				// &H93
	0x00, // L"K_?94",				// &H94
	0x00, // L"K_?95",				// &H95
	0x00, // L"K_?96",				// &H96
	0x00, // L"K_?97",				// &H97
	0x00, // L"K_?98",				// &H98
	0x00, // L"K_?99",				// &H99
	0x00, // L"K_?9A",				// &H9A
	0x00, // L"K_?9B",				// &H9B
	0x00, // L"K_?9C",				// &H9C
	0x00, // L"K_?9D",				// &H9D
	0x00, // L"K_?9E",				// &H9E
	0x00, // L"K_?9F",				// &H9F
	0x2A, // L"K_?A0",				// &HA0
	0x36, // L"K_?A1",				// &HA1
	0x1D, // L"K_?A2",				// &HA2
	0x1D, // L"K_?A3",				// &HA3
	0x38, // L"K_?A4",				// &HA4
	0x38, // L"K_?A5",				// &HA5
	0x6A, // L"K_?A6",				// &HA6
	0x69, // L"K_?A7",				// &HA7
	0x67, // L"K_?A8",				// &HA8
	0x68, // L"K_?A9",				// &HA9
	0x65, // L"K_?AA",				// &HAA
	0x66, // L"K_?AB",				// &HAB
	0x32, // L"K_?AC",				// &HAC
	0x20, // L"K_?AD",				// &HAD
	0x2E, // L"K_?AE",				// &HAE
	0x30, // L"K_?AF",				// &HAF
	0x19, // L"K_?B0",				// &HB0
	0x10, // L"K_?B1",				// &HB1
	0x24, // L"K_?B2",				// &HB2
	0x22, // L"K_?B3",				// &HB3
	0x6C, // L"K_?B4",				// &HB4
	0x6D, // L"K_?B5",				// &HB5
	0x6B, // L"K_?B6",				// &HB6
	0x21, // L"K_?B7",				// &HB7
	0x00, // L"K_?B8",				// &HB8
	0x00, // L"K_?B9",				// &HB9

	0x27, // L"K_COLON",				// &HBA
	0x0D, // L"K_EQUA0x00, // L",				// &HBB
	0x33, // L"K_COMMA",				// &HBC
	0x0C, // L"K_HYPHEN",			// &HBD
	0x34, // L"K_PERIOD",			// &HBE
	0x35, // L"K_SLASH",				// &HBF
	0x29, // L"K_BKQUOTE",			// &HC0

	0x73, // L"K_?C1",				// &HC1
	0x7E, // L"K_?C2",				// &HC2
	0x00, // L"K_?C3",				// &HC3
	0x00, // L"K_?C4",				// &HC4
	0x00, // L"K_?C5",				// &HC5
	0x00, // L"K_?C6",				// &HC6
	0x00, // L"K_?C7",				// &HC7
	0x00, // L"K_?C8",				// &HC8
	0x00, // L"K_?C9",				// &HC9
	0x00, // L"K_?CA",				// &HCA
	0x00, // L"K_?CB",				// &HCB
	0x00, // L"K_?CC",				// &HCC
	0x00, // L"K_?CD",				// &HCD
	0x00, // L"K_?CE",				// &HCE
	0x00, // L"K_?CF",				// &HCF
	0x00, // L"K_?D0",				// &HD0
	0x00, // L"K_?D1",				// &HD1
	0x00, // L"K_?D2",				// &HD2
	0x00, // L"K_?D3",				// &HD3
	0x00, // L"K_?D4",				// &HD4
	0x00, // L"K_?D5",				// &HD5
	0x00, // L"K_?D6",				// &HD6
	0x00, // L"K_?D7",				// &HD7
	0x00, // L"K_?D8",				// &HD8
	0x00, // L"K_?D9",				// &HD9
	0x00, // L"K_?DA",				// &HDA

	0x1A, // L"K_LBRKT",				// &HDB
	0x2B, // L"K_BKSLASH",			// &HDC
	0x1B, // L"K_RBRKT",				// &HDD
	0x28, // L"K_QUOTE",				// &HDE
	0x73, // L"K_oDF",				// &HDF			// MCD 01-11-02: Brazilian fix: 00 -> 73
	0x00, // L"K_oE0",				// &HE0
	0x00, // L"K_oE1",				// &HE1
	0x56, // L"K_oE2",				// &HE2
	0x00, // L"K_oE3",				// &HE3
	0x00, // L"K_oE4",				// &HE4

	0x00, // L"K_?E5",				// &HE5

	0x00, // L"K_oE6",				// &HE6

	0x00, // L"K_?E7",				// &HE7
	0x00, // L"K_?E8",				// &HE8

	0x71, // L"K_oE9",				// &HE9
	0x5C, // L"K_oEA",				// &HEA
	0x7B, // L"K_oEB",				// &HEB
	0x00, // L"K_oEC",				// &HEC
	0x6F, // L"K_oED",				// &HED
	0x5A, // L"K_oEE",				// &HEE
	0x00, // L"K_oEF",				// &HEF
	0x00, // L"K_oF0",				// &HF0
	0x5B, // L"K_oF1",				// &HF1
	0x00, // L"K_oF2",				// &HF2
	0x5F, // L"K_oF3",				// &HF3
	0x00, // L"K_oF4",				// &HF4
	0x5E, // L"K_oF5",				// &HF5

	0x00, // L"K_?F6",				// &HF6
	0x00, // L"K_?F7",				// &HF7
	0x00, // L"K_?F8",				// &HF8
	0x5D, // L"K_?F9",				// &HF9
	0x00, // L"K_?FA",				// &HFA
	0x62, // L"K_?FB",				// &HFB
	0x00, // L"K_?FC",				// &HFC
	0x00, // L"K_?FD",				// &HFD
	0x00, // L"K_?FE",				// &HFE
	0x00  // L"K_?FF"				// &HFF
	};

    private string GetVKeyName(int vk)
    {
      for(int i = 0; i < 256; i++)
        if(this.USVirtualKeyToScanCode[i] == this.m_sc) return this.VKeyNames[i];
      return "K_???";
    }

    public string LayoutRow(Boolean IsKMW)
    {
      StringBuilder sbRow = new StringBuilder();

      // First, get the SC/VK info stored
      //sbRow.Append(string.Format("{0:x2}\t{1:x2} - {2}", this.SC, (byte)this.VK, ((KeysEx)this.VK).ToString().PadRight(13)));

      // Now the CAPSLOCK value
      int capslock =
          (IsKMW ? 0 : 0xFF) &
          (0 |
          (this.IsCapsEqualToShift ? 1 : 0) |
          (this.IsSGCAPS ? 2 : 0) |
          (this.IsAltGrCapsEqualToAltGrShift ? 4 : 0) |
          (this.IsXxxxGrCapsEqualToXxxxShift ? 8 : 0));
      //sbRow.Append(string.Format("\t{0}\t", capslock));


      for (ShiftState ss = 0; ss <= Loader.MaxShiftState; ss++)
      {
        if (ss == ShiftState.Menu || ss == ShiftState.ShftMenu)
        {
          // Alt and Shift+Alt don't work, so skip them
          continue;
        }
        for (int caps = 0; caps <= 1; caps++)
        {
          string st = this.GetShiftState(ss, (caps == 1));

          if (st.Length == 0)
          {
            // No character assigned here, put in -1.
            //sbRow.Append("\t  -1");
          }
          else if ((caps == 1) && st == (this.GetShiftState(ss, (caps == 0))))
          {
            // Its a CAPS LOCK state and the assigned character(s) are
            // identical to the non-CAPS LOCK state. Put in a MIDDLE DOT.
            //sbRow.Append("\t   \u00b7");
          }
          else if (this.m_rgfDeadKey[(int)ss, caps])
          {
            sbRow.Append(string.Format("+ [{0}{1}] > dk({2:x4})\r\n", this.GetShiftStateName(capslock, caps, ss, IsKMW), this.GetVKeyName((int)this.VK), (ushort)st[0]));
            // It's a dead key, append an @ sign.
            //sbRow.Append(string.Format("\t{0:x4}@", ((ushort)st[0])));
          }
          else
          {
            bool isvalid = true;
            for (int ich = 0; ich < st.Length; ich++)
            {
              if(st[ich] < 0x20 || st[ich] == 0x7F) { isvalid=false; break; }
            }

            if(isvalid)
            {
              sbRow.Append(string.Format("+ [{0}{1}] > ", this.GetShiftStateName(capslock, caps, ss, IsKMW), this.GetVKeyName((int)this.VK)));

              // It's some characters; put 'em in there.
              StringBuilder sbChar = new StringBuilder((5 * st.Length) + 1);
              for (int ich = 0; ich < st.Length; ich++)
              {
                sbChar.Append(string.Format("U+{0} ", ((ushort)st[ich]).ToString("x4")));
              }
              sbRow.Append(string.Format("{0}\r\n", sbChar.ToString(0, sbChar.Length - 1)));
            }
          }
        }
      }

      return sbRow.ToString();
    }
  }

  public class Loader
  {

    private const uint KLF_NOTELLSHELL = 0x00000080;

    internal static KeysEx[] lpKeyStateNull = new KeysEx[256];

    [DllImport("user32.dll", CharSet = CharSet.Unicode, EntryPoint = "LoadKeyboardLayoutW", ExactSpelling = true)]
    private static extern IntPtr LoadKeyboardLayout(string pwszKLID, uint Flags);

    [DllImport("user32.dll", ExactSpelling = true)]
    private static extern bool UnloadKeyboardLayout(IntPtr hkl);

    [DllImport("user32.dll", CharSet = CharSet.Unicode, ExactSpelling = true)]
    private static extern int ToUnicodeEx(
        uint wVirtKey,
        uint wScanCode,
        KeysEx[] lpKeyState,
        StringBuilder pwszBuff,
        int cchBuff,
        uint wFlags,
        IntPtr dwhkl);

    [DllImport("user32.dll", CharSet = CharSet.Unicode, EntryPoint = "VkKeyScanExW", ExactSpelling = true)]
    private static extern ushort VkKeyScanEx(char ch, IntPtr dwhkl);

    [DllImport("user32.dll", ExactSpelling = true)]
    private static extern int GetKeyboardLayoutList(int nBuff, [Out, MarshalAs(UnmanagedType.LPArray)] IntPtr[] lpList);

    private static KeysEx m_XxxxVk = KeysEx.None;
    public static KeysEx XxxxVk
    {
      get
      {
        return m_XxxxVk;
      }
      set
      {
        m_XxxxVk = value;
      }
    }

    public static ShiftState MaxShiftState
    {
      get
      {
        return (Loader.XxxxVk == KeysEx.None ? ShiftState.ShftMenuCtrl : ShiftState.ShftXxxx);
      }
    }


    private static void FillKeyState(KeysEx[] lpKeyState, ShiftState ss, bool fCapsLock)
    {
      lpKeyState[(int)KeysEx.VK_SHIFT] = (((ss & ShiftState.Shft) != 0) ? (KeysEx)0x80 : (KeysEx)0x00);
      lpKeyState[(int)KeysEx.VK_CONTROL] = (((ss & ShiftState.Ctrl) != 0) ? (KeysEx)0x80 : (KeysEx)0x00);
      lpKeyState[(int)KeysEx.VK_MENU] = (((ss & ShiftState.Menu) != 0) ? (KeysEx)0x80 : (KeysEx)0x00);
      if (Loader.XxxxVk != KeysEx.None)
      {
        // The Xxxx key has been assigned, so let's include it
        lpKeyState[(int)Loader.XxxxVk] = (((ss & ShiftState.Xxxx) != 0) ? (KeysEx)0x80 : (KeysEx)0x00);
      }
      lpKeyState[(int)KeysEx.VK_CAPITAL] = (fCapsLock ? (KeysEx)0x01 : (KeysEx)0x00);
    }

    private static DeadKey ProcessDeadKey(
        uint iKeyDead,              // The index into the VirtualKey of the dead key
        ShiftState shiftStateDead,  // The shiftstate that contains the dead key
        KeysEx[] lpKeyStateDead,    // The key state for the dead key
        VirtualKey[] rgKey,         // Our array of dead keys
        bool fCapsLock,             // Was the caps lock key pressed?
        IntPtr hkl)
    {               // The keyboard layout

      KeysEx[] lpKeyState = new KeysEx[256];
      DeadKey deadKey = new DeadKey(rgKey[iKeyDead].GetShiftState(shiftStateDead, fCapsLock)[0]);

      for (uint iKey = 0; iKey < rgKey.Length; iKey++)
      {
        if (rgKey[iKey] != null)
        {
          StringBuilder sbBuffer = new StringBuilder(10);     // Scratchpad we use many places

          for (ShiftState ss = ShiftState.Base; ss <= Loader.MaxShiftState; ss++)
          {
            int rc = 0;
            if (ss == ShiftState.Menu || ss == ShiftState.ShftMenu)
            {
              // Alt and Shift+Alt don't work, so skip them
              continue;
            }

            for (int caps = 0; caps <= 1; caps++)
            {
              // First the dead key
              while (rc >= 0)
              {
                // We know that this is a dead key coming up, otherwise
                // this function would never have been called. If we do
                // *not* get a dead key then that means the state is
                // messed up so we run again and again to clear it up.
                // Risk is technically an infinite loop but per Hiroyama
                // that should be impossible here.
                rc = ToUnicodeEx((uint)rgKey[iKeyDead].VK, rgKey[iKeyDead].SC, lpKeyStateDead, sbBuffer, sbBuffer.Capacity, 0, hkl);
              }

              // Now fill the key state for the potential base character
              FillKeyState(lpKeyState, ss, (caps != 0));

              sbBuffer = new StringBuilder(10);
              rc = ToUnicodeEx((uint)rgKey[iKey].VK, rgKey[iKey].SC, lpKeyState, sbBuffer, sbBuffer.Capacity, 0, hkl);
              if (rc == 1)
              {
                // That was indeed a base character for our dead key.
                // And we now have a composite character. Let's run
                // through one more time to get the actual base
                // character that made it all possible?
                char combchar = sbBuffer[0];
                sbBuffer = new StringBuilder(10);
                rc = ToUnicodeEx((uint)rgKey[iKey].VK, rgKey[iKey].SC, lpKeyState, sbBuffer, sbBuffer.Capacity, 0, hkl);

                char basechar = sbBuffer[0];

                if (deadKey.DeadCharacter == combchar)
                {
                  // Since the combined character is the same as the dead key,
                  // we must clear out the keyboard buffer.
                  ClearKeyboardBuffer((uint)KeysEx.VK_DECIMAL, rgKey[(uint)KeysEx.VK_DECIMAL].SC, hkl);
                }

                if ((((ss == ShiftState.Ctrl) || (ss == ShiftState.ShftCtrl)) &&
                    (char.IsControl(basechar))) ||
                    (basechar.Equals(combchar)))
                {
                  // ToUnicodeEx has an internal knowledge about those
                  // VK_A ~ VK_Z keys to produce the control characters,
                  // when the conversion rule is not provided in keyboard
                  // layout files

                  // Additionally, dead key state is lost for some of these
                  // character combinations, for unknown reasons.

                  // Therefore, if the base character and combining are equal,
                  // and its a CTRL or CTRL+SHIFT state, and a control character
                  // is returned, then we do not add this "dead key" (which
                  // is not really a dead key).
                  continue;
                }

                if (!deadKey.ContainsBaseCharacter(basechar))
                {
                  deadKey.AddDeadKeyRow(basechar, combchar);
                }
              }
              else if (rc > 1)
              {
                // Not a valid dead key combination, sorry! We just ignore it.
              }
              else if (rc < 0)
              {
                // It's another dead key, so we ignore it (other than to flush it from the state)
                ClearKeyboardBuffer((uint)KeysEx.VK_DECIMAL, rgKey[(uint)KeysEx.VK_DECIMAL].SC, hkl);
              }
            }
          }
        }
      }
      return deadKey;
    }

    private static void ClearKeyboardBuffer(uint vk, uint sc, IntPtr hkl)
    {
      StringBuilder sb = new StringBuilder(10);
      int rc = 0;
      while (rc != 1)
      {
        rc = ToUnicodeEx(vk, sc, lpKeyStateNull, sb, sb.Capacity, 0, hkl);
      }
    }

    private static void ShowKeyboardList()
    {
      Console.WriteLine("     ID   \tFilename\tKeyboard Name");
      using (RegistryKey key = Registry.LocalMachine.OpenSubKey("System\\CurrentControlSet\\Control\\Keyboard Layouts"))
      {
        string[] keyboardIds = key.GetSubKeyNames();
        foreach (string keyboardId in keyboardIds)
        {
          using (RegistryKey subKey = key.OpenSubKey(keyboardId))
          {
            if (subKey.GetValue("keyman install") == null)
            {
              string layoutFile = (string) subKey.GetValue("layout file");
              string layoutText = (string) subKey.GetValue("layout text");
              if (layoutFile != null && layoutText != null)
              {
                Console.WriteLine("  " + keyboardId + "\t" + layoutFile + "\t" + layoutText);
              }
            }
          }
        }
      }
    }

    [STAThread]
    static void Main(string[] args) {

            if(args.Length == 0)
            {
              Console.WriteLine("Usage: importkeyboard /list | [/kmw] hkl [output.kmn] ");
              Console.WriteLine(" /list shows a list of keyboards available on your system.\n");
              Console.WriteLine(" hkl should be an 8 hex digit Keyboard ID.  See /list to enumerate these.");
              Console.WriteLine(" /kmw should be specified to target KeymanWeb.  When using KeymanWeb, the imported keyboard will:");
              Console.WriteLine("    * Ignore CAPS/NCAPS");
              Console.WriteLine("    * Convert RALT to CTRL+ALT\n");
              Console.WriteLine(" If output.kmn is not specified, the filename of the source keyboard will be used; e.g. 00000409 will produce kbdus.kmn\n");
              return;
            }

            if (args[0].Equals("/list", StringComparison.CurrentCultureIgnoreCase))
            {
              ShowKeyboardList();
              return;
            }

            string outputFileName = "", inputHKL;
            Boolean IsKMW = args[0].Equals("/kmw", StringComparison.CurrentCultureIgnoreCase);
            if (IsKMW)
            {
              if (args.Length < 2)
              {
                Console.WriteLine("Invalid command line.");
                return;
              }
              inputHKL = args[1];
              if(args.Length > 2) outputFileName = args[2];
            }
            else
            {
              inputHKL = args[0];
              if(args.Length > 1) outputFileName = args[1];
            }

            inputHKL = int.Parse(inputHKL, System.Globalization.NumberStyles.HexNumber).ToString("x8");

            int cKeyboards = GetKeyboardLayoutList(0, null);
            IntPtr[] rghkl = new IntPtr[cKeyboards];
            GetKeyboardLayoutList(cKeyboards, rghkl);
            IntPtr hkl = LoadKeyboardLayout(inputHKL, KLF_NOTELLSHELL);
            if(hkl == IntPtr.Zero) {
                Console.WriteLine("Sorry, that keyboard does not seem to be valid.");
                return;
            }

            KeysEx[] lpKeyState = new KeysEx[256];
            VirtualKey[] rgKey = new VirtualKey[256];
            ArrayList alDead = new ArrayList();

            // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
            // values in it. Then, store the SC in each valid VK so it can act as both a
            // flag that the VK is valid, and it can store the SC value.
            for(uint sc = 0x01; sc <= 0x7f; sc++) {
                VirtualKey key = new VirtualKey(hkl, sc);
                if(key.VK != 0) {
                    rgKey[(uint)key.VK] = key;
                }
            }

            // add the special keys that do not get added from the code above
            for(KeysEx ke = KeysEx.VK_NUMPAD0; ke <= KeysEx.VK_NUMPAD9; ke++) {
                rgKey[(uint)ke] = new VirtualKey(hkl, ke);
            }
            rgKey[(uint)KeysEx.VK_DIVIDE] = new VirtualKey(hkl, KeysEx.VK_DIVIDE);
            rgKey[(uint)KeysEx.VK_CANCEL] = new VirtualKey(hkl, KeysEx.VK_CANCEL);
            rgKey[(uint)KeysEx.VK_DECIMAL] = new VirtualKey(hkl, KeysEx.VK_DECIMAL);

            // See if there is a special shift state added
            for(KeysEx vk = KeysEx.None; vk <= KeysEx.VK_OEM_CLEAR; vk++) {
                uint sc = VirtualKey.MapVirtualKeyEx((uint)vk, 0, hkl);
                uint vkL = VirtualKey.MapVirtualKeyEx(sc, 1, hkl);
                uint vkR = VirtualKey.MapVirtualKeyEx(sc, 3, hkl);
                if((vkL != vkR) &&
                    ((uint)vk != vkL)) {
                    switch(vk) {
                        case KeysEx.VK_LCONTROL:
                        case KeysEx.VK_RCONTROL:
                        case KeysEx.VK_LSHIFT:
                        case KeysEx.VK_RSHIFT:
                        case KeysEx.VK_LMENU:
                        case KeysEx.VK_RMENU:
                            break;

                        default:
                            Loader.XxxxVk = vk;
                            break;
                    }
                }
            }

            for(uint iKey = 0; iKey < rgKey.Length; iKey++) {
                if(rgKey[iKey] != null) {
                    StringBuilder sbBuffer;     // Scratchpad we use many places

                    for(ShiftState ss = ShiftState.Base; ss <= Loader.MaxShiftState; ss++) {
                        if(ss == ShiftState.Menu || ss == ShiftState.ShftMenu) {
                            // Alt and Shift+Alt don't work, so skip them
                            continue;
                        }

                        for(int caps = (IsKMW ? 1 : 0); caps <= 1; caps++) {
                            ClearKeyboardBuffer((uint)KeysEx.VK_DECIMAL, rgKey[(uint)KeysEx.VK_DECIMAL].SC, hkl);
                            ////FillKeyState(lpKeyState, ss, (caps != 0)); //http://blogs.msdn.com/michkap/archive/2006/04/18/578557.aspx
                            FillKeyState(lpKeyState, ss, (caps == 0));
                            sbBuffer = new StringBuilder(10);
                            int rc = ToUnicodeEx((uint)rgKey[iKey].VK, rgKey[iKey].SC, lpKeyState, sbBuffer, sbBuffer.Capacity, 0, hkl);
                            if(rc > 0) {
                                if(sbBuffer.Length == 0) {
                                    // Someone defined NULL on the keyboard; let's coddle them
                                    ////rgKey[iKey].SetShiftState(ss, "\u0000", false, (caps != 0));
                                    rgKey[iKey].SetShiftState(ss, "\u0000", false, (caps == 0));
                                }
                                else {
                                    if((rc == 1) &&
                                        (ss == ShiftState.Ctrl || ss == ShiftState.ShftCtrl) &&
                                        ((int)rgKey[iKey].VK == ((uint)sbBuffer[0] + 0x40))) {
                                        // ToUnicodeEx has an internal knowledge about those
                                        // VK_A ~ VK_Z keys to produce the control characters,
                                        // when the conversion rule is not provided in keyboard
                                        // layout files
                                        continue;
                                    }
                                    //rgKey[iKey].SetShiftState(ss, sbBuffer.ToString().Substring(0, rc), false, (caps != 0));
                                    rgKey[iKey].SetShiftState(ss, sbBuffer.ToString().Substring(0, rc), false, (caps == 0));

                                }
                            }
                            else if(rc < 0) {
                                //rgKey[iKey].SetShiftState(ss, sbBuffer.ToString().Substring(0, 1), true, (caps != 0));
                                rgKey[iKey].SetShiftState(ss, sbBuffer.ToString().Substring(0, 1), true, (caps == 0));

                                // It's a dead key; let's flush out whats stored in the keyboard state.
                                ClearKeyboardBuffer((uint)KeysEx.VK_DECIMAL, rgKey[(uint)KeysEx.VK_DECIMAL].SC, hkl);
                                DeadKey dk = null;
                                for(int iDead = 0; iDead < alDead.Count; iDead++) {
                                    dk = (DeadKey)alDead[iDead];
                                    if(dk.DeadCharacter == rgKey[iKey].GetShiftState(ss, caps == 0)[0]) {
                                        break;
                                    }
                                    dk = null;
                                }
                                if(dk == null) {
                                    alDead.Add(ProcessDeadKey(iKey, ss, lpKeyState, rgKey, caps == 0, hkl));
                                }
                            }
                        }
                    }
                }
            }

            foreach(IntPtr i in rghkl) {
                if(hkl == i) {
                    hkl = IntPtr.Zero;
                    break;
                }
            }

            if(hkl != IntPtr.Zero) {
                UnloadKeyboardLayout(hkl);
            }

            // Okay, now we can dump the layout
            //Console.Write("\nSC\tVK  \t\t\tCAPS\t_\t_C\ts\tsC\tc\tcC\tsc\tscC\t\tca\tcaC\tsca\tscaC");
            //if(Loader.XxxxVk != KeysEx.None) {
                //Console.Write("\tx\txC\tsx\tsxC");
            //}
            //Console.WriteLine();
            //Console.Write("==\t==========\t\t====\t====\t====\t====\t====\t====\t====\t====\t====\t====\t====\t====\t====");
            //if(Loader.XxxxVk != KeysEx.None) {
                //Console.Write("\t====\t====\t====\t====");
            //}
            //Console.WriteLine();

            RegistryKey rk = Registry.LocalMachine;
            rk = rk.OpenSubKey(string.Format("System\\CurrentControlSet\\Control\\Keyboard Layouts\\{0}", inputHKL), false);
            if(rk == null)
            {
              Console.WriteLine("Could not find {0} in registry", inputHKL);
              return;
            }

            if(outputFileName == "")
            {
              outputFileName = (string) rk.GetValue("Layout File");
              outputFileName = outputFileName.ToLower().Replace(".dll", ".kmn");
            }

            Console.WriteLine("Importing Windows system keyboard {0} to Keyman keyboard {1}", inputHKL, outputFileName);

            using (StreamWriter outFile = File.CreateText(outputFileName))
            {
              outFile.WriteLine("c");
              outFile.WriteLine("c Keyman keyboard generated by ImportKeyboard");
              outFile.WriteLine("c Imported: {0}", DateTime.Now.ToString());
              outFile.WriteLine("c");
              outFile.WriteLine("c Source Keyboard File: {0}", rk.GetValue("Layout File"));
              outFile.WriteLine("c Source KeyboardID: {0}", inputHKL);
              if (IsKMW)
                outFile.WriteLine("c Target: KeymanWeb");
              else
                outFile.WriteLine("c Target: Keyman for Windows");
              outFile.WriteLine("c");


              outFile.WriteLine("c \r\n");
              outFile.WriteLine("store(&Version) \"8.0\"");
              outFile.WriteLine(string.Format("store(&Name) \"{0}\"", rk.GetValue("Layout Text")));
              //outFile.WriteLine("store(&Copyright) \"(C) 2007 Tavultesoft Pty Ltd\"");
              outFile.WriteLine();
              outFile.WriteLine("begin Unicode > use(main)\r\n");
              outFile.WriteLine("group(main) using keys\r\n");


              //for(uint i = 0; i < rk.SubKeyCount; i++0

              for (uint iKey = 0; iKey < rgKey.Length; iKey++)
              {
                if ((rgKey[iKey] != null) && rgKey[iKey].IsKeymanUsedKey &&
                    (!rgKey[iKey].IsEmpty))
                {
                  outFile.WriteLine(rgKey[iKey].LayoutRow(IsKMW));
                }
              }

              if (alDead.Count > 0)
              {
                outFile.WriteLine("\r\nmatch > use(deadkeys)\r\n");
                outFile.WriteLine("group(deadkeys)\r\n");
              }
              foreach (DeadKey dk in alDead)
              {

                StringBuilder sbDKFrom = new StringBuilder(), sbDKTo = new StringBuilder();
                sbDKFrom.Append(string.Format("store(dkf{0:x4})", ((ushort)dk.DeadCharacter)));
                sbDKTo.Append(string.Format("store(dkt{0:x4})", ((ushort)dk.DeadCharacter)));
                for (int id = 0; id < dk.Count; id++)
                {
                  sbDKFrom.Append(string.Format(" U+{0:x4}", ((ushort)dk.GetBaseCharacter(id))));
                  sbDKTo.Append(string.Format(" U+{0:x4}", ((ushort)dk.GetCombinedCharacter(id))));
                }

                outFile.WriteLine(sbDKFrom.ToString());
                outFile.WriteLine(sbDKTo.ToString());
                outFile.WriteLine("dk({0:x4}) any(dkf{0:x4}) > index(dkt{0:x4}, 2)\r\n", ((ushort)dk.DeadCharacter));
              }
              outFile.WriteLine();

            }
        }
  }
}

