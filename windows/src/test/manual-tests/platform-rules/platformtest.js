
KeymanWeb.KR(new Keyboard_platformtest());

function Keyboard_platformtest()
{

    this.KI = "Keyboard_platformtest";
    this.KN = "PlatformTest";
    this.KMINVER = "9.0";
    this.KV = null;
    this.KH = '';
    this.KM = 0;
    this.KBVER = "1.0";
    this.KMBM = 0x0000;
    this.s17 = "touch";
    this.s18 = "hardware";
    this.s21 = "windows";
    this.s22 = "android";
    this.s23 = "ios";
    this.s24 = "macosx";
    this.s25 = "linux";
    this.s28 = "desktop";
    this.s29 = "tablet";
    this.s30 = "phone";
    this.s33 = "native";
    this.s34 = "web";
    this.s37 = "ie";
    this.s38 = "chrome";
    this.s39 = "edge";
    this.s40 = "firefox";
    this.s41 = "safari";
    this.s42 = "opera";
    this.s45 = "touch";
    this.s46 = "hardware";
    this.s49 = "platform-x";
    this.s52 = "WinDOWS";
    this.s53 = "ANDroid";
    this.s54 = "iOS";
    this.s55 = "macOSX";
    this.s56 = "LINUX";
    this.s58 = "touch";
    this.s59 = "hardware";
    this.s60 = "windows";
    this.s61 = "android";
    this.s62 = "ios";
    this.s63 = "macosx";
    this.s64 = "linux";
    this.s65 = "desktop";
    this.s66 = "tablet";
    this.s67 = "phone";
    this.s68 = "native";
    this.s69 = "web";
    this.s70 = "ie";
    this.s71 = "chrome";
    this.s72 = "edge";
    this.s73 = "firefox";
    this.s74 = "safari";
    this.s75 = "opera";
    this.s76 = "touch";
    this.s77 = "hardware";
    this.s78 = "platform-x";
    this.s79 = "WinDOWS";
    this.s80 = "ANDroid";
    this.s81 = "iOS";
    this.s82 = "macOSX";
    this.s83 = "LINUX";
    this.KVER = "10.0.1109.0";
    this.gs = function(t, e) {
        return this.g0(t, e);
    };
    this.g0 = function(t, e) {
        var k = KeymanWeb,
            r = 0,
            m = 0;
        if (k.KKM(e, 16384, 65)) {
            r = m = 1;
            r = this.g1(t, e);
        }
        if (!m && k.KIK(e)) {
            r = 1;
            r = this.g9(t, e);
        }
        return r;
    };
    this.g1 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s17, t)) {
            m = 1;
            k.KO(0, t, "touch");
        } else if (k.KIFS(31, this.s18, t)) {
            m = 1;
            k.KO(0, t, "hardware");
        }
        if (m) {
            r = this.g2(t, e);
        }
        if (!m) {
            k.KO(-1, t, "undefined");
            r = this.g2(t, e);
        }
        return r;
    };
    this.g2 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s21, t)) {
            m = 1;
            k.KO(0, t, " windows");
        } else if (k.KIFS(31, this.s22, t)) {
            m = 1;
            k.KO(0, t, " android");
        } else if (k.KIFS(31, this.s23, t)) {
            m = 1;
            k.KO(0, t, " ios");
        } else if (k.KIFS(31, this.s24, t)) {
            m = 1;
            k.KO(0, t, " macosx");
        } else if (k.KIFS(31, this.s25, t)) {
            m = 1;
            k.KO(0, t, " linux");
        }
        if (m) {
            r = this.g3(t, e);
        }
        if (!m) {
            k.KO(-1, t, " undefined");
            r = this.g3(t, e);
        }
        return r;
    };
    this.g3 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s28, t)) {
            m = 1;
            k.KO(0, t, " desktop");
        } else if (k.KIFS(31, this.s29, t)) {
            m = 1;
            k.KO(0, t, " tablet");
        } else if (k.KIFS(31, this.s30, t)) {
            m = 1;
            k.KO(0, t, " phone");
        }
        if (m) {
            r = this.g4(t, e);
        }
        if (!m) {
            k.KO(-1, t, " undefined");
            r = this.g4(t, e);
        }
        return r;
    };
    this.g4 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s33, t)) {
            m = 1;
            k.KO(0, t, " native");
        } else if (k.KIFS(31, this.s34, t)) {
            m = 1;
            k.KO(0, t, " web");
        }
        if (m) {
            r = this.g5(t, e);
        }
        if (!m) {
            k.KO(-1, t, " undefined");
            r = this.g5(t, e);
        }
        return r;
    };
    this.g5 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s37, t)) {
            m = 1;
            k.KO(0, t, " ie");
        } else if (k.KIFS(31, this.s38, t)) {
            m = 1;
            k.KO(0, t, " chrome");
        } else if (k.KIFS(31, this.s39, t)) {
            m = 1;
            k.KO(0, t, " edge");
        } else if (k.KIFS(31, this.s40, t)) {
            m = 1;
            k.KO(0, t, " firefox");
        } else if (k.KIFS(31, this.s41, t)) {
            m = 1;
            k.KO(0, t, " safari");
        } else if (k.KIFS(31, this.s42, t)) {
            m = 1;
            k.KO(0, t, " opera");
        }
        if (m) {
            r = this.g6(t, e);
        }
        if (!m) {
            k.KO(-1, t, " undefined");
            r = this.g6(t, e);
        }
        return r;
    };
    this.g6 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (!k.KIFS(31, this.s45, t)) {
            m = 1;
            k.KO(0, t, " !touch");
        } else if (!k.KIFS(31, this.s46, t)) {
            m = 1;
            k.KO(0, t, " !hardware");
        }
        if (m) {
            r = this.g7(t, e);
        }
        if (!m) {
            k.KO(-1, t, " undefined");
            r = this.g7(t, e);
        }
        return r;
    };
    this.g7 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s49, t)) {
            m = 1;
            k.KO(0, t, " platform-x");
        }
        if (m) {
            r = this.g8(t, e);
        }
        if (!m) {
            k.KO(-1, t, " undefined");
            r = this.g8(t, e);
        }
        return r;
    };
    this.g8 = function(t, e) {
        var k = KeymanWeb,
            r = 1,
            m = 0;
        if (k.KIFS(31, this.s52, t)) {
            m = 1;
            k.KO(0, t, " windows");
        } else if (k.KIFS(31, this.s53, t)) {
            m = 1;
            k.KO(0, t, " android");
        } else if (k.KIFS(31, this.s54, t)) {
            m = 1;
            k.KO(0, t, " ios");
        } else if (k.KIFS(31, this.s55, t)) {
            m = 1;
            k.KO(0, t, " macosx");
        } else if (k.KIFS(31, this.s56, t)) {
            m = 1;
            k.KO(0, t, " linux");
        }
        if (!m) {
            k.KO(-1, t, " undefined");
        }
        return r;
    };
    this.g9 = function(t, e) {
        var k = KeymanWeb,
            r = 0,
            m = 0;
        if (k.KKM(e, 16384, 69) && k.KIFS(31, this.s79, t)) {
            r = m = 1;
            k.KO(0, t, " Windows");
        } else if (k.KKM(e, 16384, 69) && k.KIFS(31, this.s80, t)) {
            r = m = 1;
            k.KO(0, t, " Android");
        } else if (k.KKM(e, 16384, 69) && k.KIFS(31, this.s81, t)) {
            r = m = 1;
            k.KO(0, t, " iOS");
        } else if (k.KKM(e, 16384, 69) && k.KIFS(31, this.s82, t)) {
            r = m = 1;
            k.KO(0, t, " macOS");
        } else if (k.KKM(e, 16384, 69) && k.KIFS(31, this.s83, t)) {
            r = m = 1;
            k.KO(0, t, " Linux");
        } else if (k.KKM(e, 16384, 69)) {
            r = m = 1;
            k.KO(0, t, " [OS Undefined (case insensitive test)]");
        } else if (k.KKM(e, 16384, 73) && k.KIFS(31, this.s65, t)) {
            r = m = 1;
            k.KO(0, t, " Desktop");
        } else if (k.KKM(e, 16384, 73) && k.KIFS(31, this.s66, t)) {
            r = m = 1;
            k.KO(0, t, " Tablet");
        } else if (k.KKM(e, 16384, 73) && k.KIFS(31, this.s67, t)) {
            r = m = 1;
            k.KO(0, t, " Phone");
        } else if (k.KKM(e, 16384, 73)) {
            r = m = 1;
            k.KO(0, t, " [FF Undefined]");
        } else if (k.KKM(e, 16384, 79) && k.KIFS(31, this.s60, t)) {
            r = m = 1;
            k.KO(0, t, " Windows");
        } else if (k.KKM(e, 16384, 79) && k.KIFS(31, this.s61, t)) {
            r = m = 1;
            k.KO(0, t, " Android");
        } else if (k.KKM(e, 16384, 79) && k.KIFS(31, this.s62, t)) {
            r = m = 1;
            k.KO(0, t, " iOS");
        } else if (k.KKM(e, 16384, 79) && k.KIFS(31, this.s63, t)) {
            r = m = 1;
            k.KO(0, t, " OSX");
        } else if (k.KKM(e, 16384, 79) && k.KIFS(31, this.s64, t)) {
            r = m = 1;
            k.KO(0, t, " Linux");
        } else if (k.KKM(e, 16384, 79)) {
            r = m = 1;
            k.KO(0, t, " [OS Undefined]");
        } else if (k.KKM(e, 16384, 80) && k.KIFS(31, this.s58, t)) {
            r = m = 1;
            k.KO(0, t, "Touch");
        } else if (k.KKM(e, 16384, 80) && k.KIFS(31, this.s59, t)) {
            r = m = 1;
            k.KO(0, t, "Hardware");
        } else if (k.KKM(e, 16384, 80)) {
            r = m = 1;
            k.KO(0, t, "[UI Undefined]");
        } else if (k.KKM(e, 16384, 82) && k.KIFS(31, this.s78, t)) {
            r = m = 1;
            k.KO(0, t, " Platform-X");
        } else if (k.KKM(e, 16384, 82)) {
            r = m = 1;
            k.KO(0, t, " [Platform-X Undefined]");
        } else if (k.KKM(e, 16384, 84) && !k.KIFS(31, this.s76, t)) {
            r = m = 1;
            k.KO(0, t, " !Touch");
        } else if (k.KKM(e, 16384, 84) && !k.KIFS(31, this.s77, t)) {
            r = m = 1;
            k.KO(0, t, " !Hardware");
        } else if (k.KKM(e, 16384, 84)) {
            r = m = 1;
            k.KO(0, t, " [Inverted OS Undefined]");
        } else if (k.KKM(e, 16384, 85) && k.KIFS(31, this.s68, t)) {
            r = m = 1;
            k.KO(0, t, " Native");
        } else if (k.KKM(e, 16384, 85) && k.KIFS(31, this.s69, t)) {
            r = m = 1;
            k.KO(0, t, " Web");
        } else if (k.KKM(e, 16384, 85)) {
            r = m = 1;
            k.KO(0, t, " [Nativeness Undefined]");
        } else if (k.KKM(e, 16384, 89) && k.KIFS(31, this.s70, t)) {
            r = m = 1;
            k.KO(0, t, " IE");
        } else if (k.KKM(e, 16384, 89) && k.KIFS(31, this.s71, t)) {
            r = m = 1;
            k.KO(0, t, " Chrome");
        } else if (k.KKM(e, 16384, 89) && k.KIFS(31, this.s72, t)) {
            r = m = 1;
            k.KO(0, t, " Edge");
        } else if (k.KKM(e, 16384, 89) && k.KIFS(31, this.s73, t)) {
            r = m = 1;
            k.KO(0, t, " Firefox");
        } else if (k.KKM(e, 16384, 89) && k.KIFS(31, this.s74, t)) {
            r = m = 1;
            k.KO(0, t, " Safari");
        } else if (k.KKM(e, 16384, 89) && k.KIFS(31, this.s75, t)) {
            r = m = 1;
            k.KO(0, t, " Opera");
        } else if (k.KKM(e, 16384, 89)) {
            r = m = 1;
            k.KO(0, t, " [Browser Undefined]");
        }
        return r;
    };
}