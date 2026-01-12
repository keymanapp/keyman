if (typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if (typeof tavultesoft !== 'undefined') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
  KeymanWeb.KR(new Keyboard_platformtest());
}

function Keyboard_platformtest() {
  this._v = (typeof keyman != "undefined" && typeof keyman.version == "string") ? parseInt(keyman.version, 10) : 9;
  this.KI = "Keyboard_platformtest";
  this.KN = "PlatformTest";
  this.KMINVER = "10.0";
  this.KV = null;
  this.KDU = 0;
  this.KH = '';
  this.KM = 0;
  this.KBVER = "1.0";
  this.KMBM = 0x0;
  this.s20 = "touch";
  this.s21 = "hardware";
  this.s24 = "windows";
  this.s25 = "android";
  this.s26 = "ios";
  this.s27 = "macosx";
  this.s28 = "linux";
  this.s31 = "desktop";
  this.s32 = "tablet";
  this.s33 = "phone";
  this.s36 = "native";
  this.s37 = "web";
  this.s40 = "ie";
  this.s41 = "chrome";
  this.s42 = "edge";
  this.s43 = "firefox";
  this.s44 = "safari";
  this.s45 = "opera";
  this.s48 = "touch";
  this.s49 = "hardware";
  this.s52 = "platform-x";
  this.s55 = "WinDOWS";
  this.s56 = "ANDroid";
  this.s57 = "iOS";
  this.s58 = "macOSX";
  this.s59 = "LINUX";
  this.s61 = "touch";
  this.s62 = "hardware";
  this.s63 = "windows";
  this.s64 = "android";
  this.s65 = "ios";
  this.s66 = "macosx";
  this.s67 = "linux";
  this.s68 = "desktop";
  this.s69 = "tablet";
  this.s70 = "phone";
  this.s71 = "native";
  this.s72 = "web";
  this.s73 = "ie";
  this.s74 = "chrome";
  this.s75 = "edge";
  this.s76 = "firefox";
  this.s77 = "safari";
  this.s78 = "opera";
  this.s79 = "touch";
  this.s80 = "hardware";
  this.s81 = "platform-x";
  this.s82 = "WinDOWS";
  this.s83 = "ANDroid";
  this.s84 = "iOS";
  this.s85 = "macOSX";
  this.s86 = "LINUX";
  this.KVER = "18.0.241.0";
  this.KVS = [];
  this.gs = function(t, e) {
    return this.g0(t, e);
  };
  this.gs = function(t, e) {
    return this.g0(t, e);
  };
  this.g0 = function(t, e) {
    var k = KeymanWeb,
      r = 0,
      m = 0;
    if (k.KKM(e, 16384, 65)) {
      if (1) {
        r = m = 1;
        k.KDC(0, t);
        r = this.g1(t, e);
        m = 2;
      }
    }
    if (!m && k.KIK(e)) {
      r = 1;
      k.KDC(-1, t);
      r = this.g9(t, e);
      m = 2;
    }
    return r;
  };
  this.g1 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s20, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, "touch");
    } else if (k.KIFS(31, this.s21, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, "hardware");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g2(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, "undefined");
      r = this.g2(t, e);
      m = 2;
    }
    return r;
  };
  this.g2 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s24, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " windows");
    } else if (k.KIFS(31, this.s25, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " android");
    } else if (k.KIFS(31, this.s26, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " ios");
    } else if (k.KIFS(31, this.s27, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " macosx");
    } else if (k.KIFS(31, this.s28, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " linux");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g3(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
      r = this.g3(t, e);
      m = 2;
    }
    return r;
  };
  this.g3 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s31, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " desktop");
    } else if (k.KIFS(31, this.s32, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " tablet");
    } else if (k.KIFS(31, this.s33, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " phone");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g4(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
      r = this.g4(t, e);
      m = 2;
    }
    return r;
  };
  this.g4 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s36, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " native");
    } else if (k.KIFS(31, this.s37, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " web");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g5(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
      r = this.g5(t, e);
      m = 2;
    }
    return r;
  };
  this.g5 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s40, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " ie");
    } else if (k.KIFS(31, this.s41, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " chrome");
    } else if (k.KIFS(31, this.s42, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " edge");
    } else if (k.KIFS(31, this.s43, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " firefox");
    } else if (k.KIFS(31, this.s44, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " safari");
    } else if (k.KIFS(31, this.s45, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " opera");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g6(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
      r = this.g6(t, e);
      m = 2;
    }
    return r;
  };
  this.g6 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (!k.KIFS(31, this.s48, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " !touch");
    } else if (!k.KIFS(31, this.s49, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " !hardware");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g7(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
      r = this.g7(t, e);
      m = 2;
    }
    return r;
  };
  this.g7 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s52, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " platform-x");
    }
    if (m == 1) {
      k.KDC(-1, t);
      r = this.g8(t, e);
      m = 2;
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
      r = this.g8(t, e);
      m = 2;
    }
    return r;
  };
  this.g8 = function(t, e) {
    var k = KeymanWeb,
      r = 1,
      m = 0;
    if (k.KIFS(31, this.s55, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " windows");
    } else if (k.KIFS(31, this.s56, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " android");
    } else if (k.KIFS(31, this.s57, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " ios");
    } else if (k.KIFS(31, this.s58, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " macosx");
    } else if (k.KIFS(31, this.s59, t)) {
      m = 1;
      k.KDC(0, t);
      k.KO(-1, t, " linux");
    }
    if (!m) {
      k.KDC(-1, t);
      k.KO(-1, t, " undefined");
    }
    return r;
  };
  this.g9 = function(t, e) {
    var k = KeymanWeb,
      r = 0,
      m = 0;
    if (k.KKM(e, 16384, 69)) {
      if (k.KIFS(31, this.s82, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Windows");
      } else if (k.KIFS(31, this.s83, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Android");
      } else if (k.KIFS(31, this.s84, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " iOS");
      } else if (k.KIFS(31, this.s85, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " macOS");
      } else if (k.KIFS(31, this.s86, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Linux");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [OS Undefined (case insensitive test)]");
      }
    } else if (k.KKM(e, 16384, 73)) {
      if (k.KIFS(31, this.s68, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Desktop");
      } else if (k.KIFS(31, this.s69, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Tablet");
      } else if (k.KIFS(31, this.s70, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Phone");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [FF Undefined]");
      }
    } else if (k.KKM(e, 16384, 79)) {
      if (k.KIFS(31, this.s63, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Windows");
      } else if (k.KIFS(31, this.s64, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Android");
      } else if (k.KIFS(31, this.s65, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " iOS");
      } else if (k.KIFS(31, this.s66, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " OSX");
      } else if (k.KIFS(31, this.s67, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Linux");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [OS Undefined]");
      }
    } else if (k.KKM(e, 16384, 80)) {
      if (k.KIFS(31, this.s61, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, "Touch");
      } else if (k.KIFS(31, this.s62, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, "Hardware");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, "[UI Undefined]");
      }
    } else if (k.KKM(e, 16384, 82)) {
      if (k.KIFS(31, this.s81, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Platform-X");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [Platform-X Undefined]");
      }
    } else if (k.KKM(e, 16384, 84)) {
      if (!k.KIFS(31, this.s79, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " !Touch");
      } else if (!k.KIFS(31, this.s80, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " !Hardware");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [Inverted OS Undefined]");
      }
    } else if (k.KKM(e, 16384, 85)) {
      if (k.KIFS(31, this.s71, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Native");
      } else if (k.KIFS(31, this.s72, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Web");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [Nativeness Undefined]");
      }
    } else if (k.KKM(e, 16384, 89)) {
      if (k.KIFS(31, this.s73, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " IE");
      } else if (k.KIFS(31, this.s74, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Chrome");
      } else if (k.KIFS(31, this.s75, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Edge");
      } else if (k.KIFS(31, this.s76, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Firefox");
      } else if (k.KIFS(31, this.s77, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Safari");
      } else if (k.KIFS(31, this.s78, t)) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " Opera");
      } else if (1) {
        r = m = 1;
        k.KDC(0, t);
        k.KO(-1, t, " [Browser Undefined]");
      }
    }
    return r;
  };
}
