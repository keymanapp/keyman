if (typeof keyman === 'undefined') {
  console.log('Keyboard requires KeymanWeb 10.0 or later');
  if (typeof tavultesoft !== 'undefined')
      tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");
} else {
  KeymanWeb.KR(new Keyboard_galaxie_hebrew_positional());
}
function Keyboard_galaxie_hebrew_positional() {
  this.KI = "Keyboard_galaxie_hebrew_positional";
  this.KN = "Galaxie Hebrew (Positional)";
  this.KMINVER = "10.0";
  this.KV = {
      F: ' 1em "Arial"',
      K102: 0
  };
  this.KDU = 0;
  this.KV.KLS = {
      "default": ["", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "־", "◌ְ", "", "", "", "ק", "ו", "ךֵ", "ר", "ת", "י", "ע", "ך", "וֹ", "פ", "ף", "", "◌ּ", "", "", "", "א", "ס", "ד", "ט", "ג", "ה", "ח", "כ", "ל", "ךָ", "'", "", "", "", "", "", "", "ז", "צ", "שׂ", "שׁ", "ב", "נ", "מ", ",", "׃", "ן", "", "", "", "", "", ""],
      "shift": ["~", "◌ִ", "◌ֵ", "◌ֶ", "◌ֱ", "◌ֻ", "◌ַ", "◌ֲ", "◌ָ", "◌ֳ", "◌ֹ", "◌ֿ", "+", "", "", "", "קּ", "וּ", "ךְ", "רּ", "תּ", "יּ", "עּ", "ךּ", "וֹּ", "פּ", "ףּ", "שּּ", "|", "", "", "", "אּ", "סּ", "דּ", "טּ", "גּ", "ץ", "חּ", "כּ", "לּ", "ךֶ", "\"", "", "", "", "", "", "", "זּ", "צּ", "שּׂ", "שּׁ", "בּ", "נּ", "מּ", "ם", ".", "ש", "", "", "", "", "", ""]
  };
  this.KV.BK = (function(x) {
      var e = Array.apply(null, Array(65)).map(String.prototype.valueOf, ""), r = [], v, i, m = ['default', 'shift', 'ctrl', 'shift-ctrl', 'alt', 'shift-alt', 'ctrl-alt', 'shift-ctrl-alt'];
      for (i = m.length - 1; i >= 0; i--)
          if ((v = x[m[i]]) || r.length)
              r = (v ? v : e).slice().concat(r);
      return r
  }
  )(this.KV.KLS);
  this.KH = '';
  this.KM = 0;
  this.KBVER = "2.2";
  this.KMBM = 0x0010;
  this.KRTL = 1;
  this.KVER = "10.0.1099.0";
  this.gs = function(t, e) {
      return this.g0(t, e);
  }
  ;
  this.g0 = function(t, e) {
      var k = KeymanWeb
        , r = 0
        , m = 0;
      if (k.KKM(e, 16384, 9) && k.KFCM(1, t, ['מ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ם");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 9) && k.KFCM(1, t, ['פ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ף");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 9) && k.KFCM(1, t, ['נ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ן");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 9) && k.KFCM(1, t, ['צ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ץ");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 13) && k.KFCM(1, t, ['מ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ם");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 13) && k.KFCM(1, t, ['פ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ף");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 13) && k.KFCM(1, t, ['נ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ן");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 13) && k.KFCM(1, t, ['צ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ץ");
          r = this.g1(t, e);
      } else if (k.KKM(e, 16384, 9) && k.KFCM(1, t, ['כ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ך");
      } else if (k.KKM(e, 16384, 13) && k.KFCM(1, t, ['כ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ך");
      } else if (k.KKM(e, 16384, 32) && k.KFCM(1, t, ['מ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ם ");
      } else if (k.KKM(e, 16384, 32) && k.KFCM(1, t, ['פ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ף ");
      } else if (k.KKM(e, 16384, 32) && k.KFCM(1, t, ['נ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ן ");
      } else if (k.KKM(e, 16384, 32) && k.KFCM(1, t, ['צ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ץ ");
      } else if (k.KKM(e, 16384, 32) && k.KFCM(1, t, ['כ'])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ך");
      } else if (k.KKM(e, 16400, 49)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ִ");
      } else if (k.KKM(e, 16400, 222)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "\"");
      } else if (k.KKM(e, 16400, 51)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֶ");
      } else if (k.KKM(e, 16400, 52)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֱ");
      } else if (k.KKM(e, 16400, 53)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֻ");
      } else if (k.KKM(e, 16400, 55)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֲ");
      } else if (k.KKM(e, 16384, 222)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "'");
      } else if (k.KKM(e, 16400, 57)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֳ");
      } else if (k.KKM(e, 16400, 48)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֹ");
      } else if (k.KKM(e, 16400, 56)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ָ");
      } else if (k.KKM(e, 16400, 187)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "+");
      } else if (k.KKM(e, 16384, 188) && k.KFCM(1, t, [{
          t: 'd',
          d: 0
      }])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "֤");
      } else if (k.KKM(e, 16384, 189)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "־");
      } else if (k.KKM(e, 16384, 190)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "׃");
      } else if (k.KKM(e, 16384, 191)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ן");
      } else if (k.KKM(e, 16384, 48)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "0");
      } else if (k.KKM(e, 16384, 49)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "1");
      } else if (k.KKM(e, 16384, 50)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "2");
      } else if (k.KKM(e, 16384, 51)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "3");
      } else if (k.KKM(e, 16384, 52)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "4");
      } else if (k.KKM(e, 16384, 53)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "5");
      } else if (k.KKM(e, 16384, 54)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "6");
      } else if (k.KKM(e, 16384, 55)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "7");
      } else if (k.KKM(e, 16384, 56)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "8");
      } else if (k.KKM(e, 16384, 57)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "9");
      } else if (k.KKM(e, 16400, 186)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ךֶ");
      } else if (k.KKM(e, 16384, 186)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ךָ");
      } else if (k.KKM(e, 16400, 188) && k.KFCM(1, t, [{
          t: 'd',
          d: 0
      }])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "֫");
      } else if (k.KKM(e, 16400, 188)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ם");
      } else if (k.KKM(e, 16384, 187) && k.KFCM(1, t, [{
          t: 'd',
          d: 0
      }])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "=");
      } else if (k.KKM(e, 16384, 187)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ְ");
      } else if (k.KKM(e, 16400, 190)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, ".");
      } else if (k.KKM(e, 16400, 191)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ש");
      } else if (k.KKM(e, 16400, 50)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֵ");
      } else if (k.KKM(e, 16400, 65)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "אּ");
      } else if (k.KKM(e, 16400, 66)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "בּ");
      } else if (k.KKM(e, 16400, 67)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "שּׂ");
      } else if (k.KKM(e, 16400, 68)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "דּ");
      } else if (k.KKM(e, 16400, 69)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ךְ");
      } else if (k.KKM(e, 16400, 70)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "טּ");
      } else if (k.KKM(e, 16400, 71)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "גּ");
      } else if (k.KKM(e, 16400, 72)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ץ");
      } else if (k.KKM(e, 16400, 73)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ךּ");
      } else if (k.KKM(e, 16400, 74)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "חּ");
      } else if (k.KKM(e, 16400, 75)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "כּ");
      } else if (k.KKM(e, 16400, 76)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "לּ");
      } else if (k.KKM(e, 16400, 77)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "מּ");
      } else if (k.KKM(e, 16400, 78)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "נּ");
      } else if (k.KKM(e, 16400, 79)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "וֹּ");
      } else if (k.KKM(e, 16400, 80)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "פּ");
      } else if (k.KKM(e, 16400, 81)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "קּ");
      } else if (k.KKM(e, 16400, 82)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "רּ");
      } else if (k.KKM(e, 16400, 83)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "סּ");
      } else if (k.KKM(e, 16400, 84)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "תּ");
      } else if (k.KKM(e, 16400, 85)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "עּ");
      } else if (k.KKM(e, 16400, 86)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "שּׁ");
      } else if (k.KKM(e, 16400, 87)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "וּ");
      } else if (k.KKM(e, 16400, 88)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "צּ");
      } else if (k.KKM(e, 16400, 89)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "יּ");
      } else if (k.KKM(e, 16400, 90)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "זּ");
      } else if (k.KKM(e, 16384, 219)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ף");
      } else if (k.KKM(e, 16384, 220)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ּ");
      } else if (k.KKM(e, 16400, 54)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ַ");
      } else if (k.KKM(e, 16400, 189)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ֿ");
      } else if (k.KKM(e, 16384, 192)) {
          r = m = 1;
          k.KDC(0, t);
          k.KDO(-1, t, 0);
      } else if (k.KKM(e, 16384, 65)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "א");
      } else if (k.KKM(e, 16384, 66)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ב");
      } else if (k.KKM(e, 16384, 67)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "שׂ");
      } else if (k.KKM(e, 16384, 68)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ד");
      } else if (k.KKM(e, 16384, 69)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ךֵ");
      } else if (k.KKM(e, 16384, 70)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ט");
      } else if (k.KKM(e, 16384, 71)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ג");
      } else if (k.KKM(e, 16384, 72)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ה");
      } else if (k.KKM(e, 16384, 73)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ך");
      } else if (k.KKM(e, 16384, 74)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ח");
      } else if (k.KKM(e, 16384, 75)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "כ");
      } else if (k.KKM(e, 16384, 76)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ל");
      } else if (k.KKM(e, 16384, 77)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "מ");
      } else if (k.KKM(e, 16384, 78)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "נ");
      } else if (k.KKM(e, 16384, 79)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "וֹ");
      } else if (k.KKM(e, 16384, 80)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "פ");
      } else if (k.KKM(e, 16384, 81)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ק");
      } else if (k.KKM(e, 16384, 82)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ר");
      } else if (k.KKM(e, 16384, 83)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ס");
      } else if (k.KKM(e, 16384, 84)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ת");
      } else if (k.KKM(e, 16384, 85)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ע");
      } else if (k.KKM(e, 16384, 86)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "שׁ");
      } else if (k.KKM(e, 16384, 87)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ו");
      } else if (k.KKM(e, 16384, 88)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "צ");
      } else if (k.KKM(e, 16384, 89)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "י");
      } else if (k.KKM(e, 16384, 90)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ז");
      } else if (k.KKM(e, 16400, 219)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "ףּ");
      } else if (k.KKM(e, 16400, 220) && k.KFCM(1, t, [{
          t: 'd',
          d: 0
      }])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "ֽ");
      } else if (k.KKM(e, 16400, 220)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "|");
      } else if (k.KKM(e, 16400, 221)) {
          r = m = 1;
          k.KDC(0, t);
          k.KO(-1, t, "שּּ");
      } else if (k.KKM(e, 16400, 192) && k.KFCM(1, t, [{
          t: 'd',
          d: 0
      }])) {
          r = m = 1;
          k.KDC(1, t);
          k.KO(-1, t, "~");
      }
      return r;
  }
  ;
  this.g1 = function(t, e) {
      var k = KeymanWeb
        , r = 0
        , m = 0;
      return r;
  }
  ;
}