// The Device object definition -------------------------------------------------
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var Device = /** @class */ (function () {
            // Generates a default Device value.
            function Device() {
                this.touchable = !!('ontouchstart' in window);
                this.OS = '';
                this.formFactor = 'desktop';
                this.dyPortrait = 0;
                this.dyLandscape = 0;
                this.version = '0';
                this.orientation = window.orientation;
                this.browser = '';
            }
            /**
             * Get device horizontal DPI for touch devices, to set actual size of active regions
             * Note that the actual physical DPI may be somewhat different.
             *
             * @return      {number}
             */
            Device.prototype.getDPI = function () {
                var t = document.createElement('DIV'), s = t.style, dpi = 96;
                if (document.readyState !== 'complete') {
                    return dpi;
                }
                t.id = 'calculateDPI';
                s.position = 'absolute';
                s.display = 'block';
                s.visibility = 'hidden';
                s.left = '10px';
                s.top = '10px';
                s.width = '1in';
                s.height = '10px';
                document.body.appendChild(t);
                dpi = (typeof window.devicePixelRatio == 'undefined') ? t.offsetWidth : t.offsetWidth * window.devicePixelRatio;
                document.body.removeChild(t);
                return dpi;
            };
            Device.prototype.detect = function () {
                var IEVersion = Device._GetIEVersion();
                if (navigator && navigator.userAgent) {
                    var agent = navigator.userAgent;
                    if (agent.indexOf('iPad') >= 0) {
                        this.OS = 'iOS';
                        this.formFactor = 'tablet';
                        this.dyPortrait = this.dyLandscape = 0;
                    }
                    if (agent.indexOf('iPhone') >= 0) {
                        this.OS = 'iOS';
                        this.formFactor = 'phone';
                        this.dyPortrait = this.dyLandscape = 25;
                    }
                    if (agent.indexOf('Android') >= 0) {
                        this.OS = 'Android';
                        this.formFactor = 'phone'; // form factor may be redefined on initialization
                        this.dyPortrait = 75;
                        this.dyLandscape = 25;
                        try {
                            var rx = new RegExp("(?:Android\\s+)(\\d+\\.\\d+\\.\\d+)");
                            this.version = agent.match(rx)[1];
                        }
                        catch (ex) { }
                    }
                    if (agent.indexOf('Windows NT') >= 0) {
                        this.OS = 'Windows';
                        if (agent.indexOf('Touch') >= 0) {
                            this.formFactor = 'phone'; // will be redefined as tablet if resolution high enough
                        }
                        // Windows Phone and Tablet PC
                        if (typeof navigator.msMaxTouchPoints == 'number' && navigator.msMaxTouchPoints > 0) {
                            this.touchable = true;
                        }
                    }
                }
                // var sxx=device.formFactor;
                // Check and possibly revise form factor according to actual screen size (adjusted for Galaxy S, maybe OK generally?)
                if (this.formFactor == 'tablet' && Math.min(screen.width, screen.height) < 400) {
                    this.formFactor = 'phone';
                }
                if (this.formFactor == 'phone' && Math.max(screen.width, screen.height) > 720) {
                    this.formFactor = 'tablet';
                }
                //                           alert(sxx+'->'+device.formFactor);
                // Check for phony iOS devices (Win32 test excludes Chrome touch emulation on Windows)!
                if (this.OS == 'iOS' && !('ongesturestart' in window) && navigator.platform != 'Win32') {
                    this.OS = 'Android';
                }
                // Determine application or browser
                this.browser = 'web';
                if (IEVersion < 999) {
                    this.browser = 'ie';
                }
                else {
                    if (this.OS == 'iOS' || this.OS.toLowerCase() == 'macosx') {
                        this.browser = 'safari';
                    }
                    var bMatch = /Firefox|Chrome|OPR/;
                    if (bMatch.test(navigator.userAgent)) {
                        if ((navigator.userAgent.indexOf('Firefox') >= 0) && ('onmozorientationchange' in screen)) {
                            this.browser = 'firefox';
                        }
                        else if (navigator.userAgent.indexOf('OPR') >= 0) {
                            this.browser = 'opera';
                        }
                        else if (navigator.userAgent.indexOf('Chrome') >= 0) {
                            this.browser = 'chrome';
                        }
                    }
                }
            };
            Device._GetIEVersion = function () {
                var n, agent = '';
                if ('userAgent' in navigator) {
                    agent = navigator.userAgent;
                }
                // Test first for old versions
                if ('selection' in document) {
                    var appVer = navigator.appVersion;
                    n = appVer.indexOf('MSIE ');
                    if (n >= 0) {
                        // Check for quirks mode page, always return 6 if so
                        if (document.compatMode == 'BackCompat') {
                            return 6;
                        }
                        appVer = appVer.substr(n + 5);
                        n = appVer.indexOf('.');
                        if (n > 0) {
                            return parseInt(appVer.substr(0, n), 10);
                        }
                    }
                }
                // Finally test for IE 11 (and later?)
                n = agent.indexOf('Trident/');
                if (n < 0) {
                    return 999;
                }
                agent = agent.substr(n + 8);
                n = agent.indexOf('.');
                if (n > 0) {
                    return parseInt(agent.substr(0, n), 10) + 4;
                }
                return 999;
            };
            return Device;
        }());
        keyman.Device = Device;
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
// Includes KeymanWeb's Device class, as it's quite useful for ensuring that we target our tests correctly
// to each device.
/// <reference path="kmwdevice.ts" />
var __extends = (this && this.__extends) || (function () {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var KMWRecorder;
(function (KMWRecorder) {
    var Device = com.keyman.Device;
    var InputEvent = /** @class */ (function () {
        function InputEvent() {
        }
        InputEvent.fromJSONObject = function (obj) {
            if (obj && obj.type) {
                if (obj.type == "key") {
                    return new PhysicalInputEvent(obj);
                }
                else if (obj.type == "osk") {
                    return new OSKInputEvent(obj);
                }
            }
            else {
                throw new SyntaxError("Error in JSON format corresponding to an InputEvent!");
            }
        };
        InputEvent.prototype.toPrettyJSON = function () {
            // We want the default, non-spaced JSON for this class, even when otherwise adding whitespace.
            var str = JSON.stringify(this);
            return str;
        };
        return InputEvent;
    }());
    KMWRecorder.InputEvent = InputEvent;
    var PhysicalInputEvent = /** @class */ (function (_super) {
        __extends(PhysicalInputEvent, _super);
        function PhysicalInputEvent(e) {
            var _this = _super.call(this) || this;
            // KeyboardEvent properties
            _this.type = "key";
            // We condition upon newly-generated events, as a PhysicalInputEvent from JSON
            // will lack its proper prototype, etc.
            if (e instanceof KeyboardEvent) {
                _this.key = e.key;
                _this.code = e.code;
                _this.keyCode = e.keyCode;
                _this.modifierSet = _this.compileModifierState(e);
                _this.location = e.location;
            }
            else {
                _this.key = e.key;
                _this.code = e.code;
                _this.keyCode = e.keyCode;
                _this.modifierSet = e.modifierSet;
                _this.location = e.location;
            }
            return _this;
        }
        PhysicalInputEvent.prototype.compileModifierState = function (e) {
            var flagSet = 0;
            for (var key in PhysicalInputEvent.modifierCodes) {
                if (e.getModifierState(key)) {
                    flagSet |= PhysicalInputEvent.modifierCodes[key];
                }
            }
            return flagSet;
        };
        PhysicalInputEvent.prototype.getModifierState = function (key) {
            return (PhysicalInputEvent.modifierCodes[key] & this.modifierSet) != 0;
        };
        PhysicalInputEvent.prototype.generateModifierString = function () {
            var list = "";
            for (var key in PhysicalInputEvent.modifierCodes) {
                if (this.getModifierState(key)) {
                    list += ((list != "" ? " " : "") + key);
                }
            }
            return list;
        };
        PhysicalInputEvent.prototype.simulateEventOn = function (ele) {
            var event;
            // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
            var downEvent;
            if (typeof Event == 'function') {
                event = new Event(PhysicalInputEvent.eventType);
                event['key'] = this.key;
                event['code'] = this.code;
                event['keyCode'] = this.keyCode;
                event['location'] = this.location;
                event['getModifierState'] = this.getModifierState.bind(this);
            }
            else {
                event = document.createEvent(PhysicalInputEvent.eventClass);
                // An override to ensure that IE's method gets called.
                // Many thanks to https://gist.github.com/termi/4654819, line 142 at the time of writing this.
                var success = event.initKeyboardEvent(PhysicalInputEvent.eventType, false, true, null, this.key, /*this.code,*/ this.location, this.generateModifierString(), 0, 0);
            }
            ele.dispatchEvent(event);
        };
        PhysicalInputEvent.eventClass = "KeyboardEvent";
        PhysicalInputEvent.eventType = "keydown";
        PhysicalInputEvent.modifierCodes = {
            "Shift": 0x0001,
            "Control": 0x0002,
            "Alt": 0x0004,
            "Meta": 0x0008,
            "CapsLock": 0x0010,
            "NumLock": 0x0020,
            "ScrollLock": 0x0040
        };
        return PhysicalInputEvent;
    }(InputEvent));
    KMWRecorder.PhysicalInputEvent = PhysicalInputEvent;
    var OSKInputEvent = /** @class */ (function (_super) {
        __extends(OSKInputEvent, _super);
        // osk.clickKey receives the element clicked or touched in OSK interactions.
        function OSKInputEvent(ele) {
            var _this = _super.call(this) || this;
            _this.type = "osk";
            if (typeof ele == 'string') {
                _this.keyID = ele;
            }
            else if (ele instanceof HTMLDivElement) {
                _this.keyID = ele.id;
            }
            else {
                _this.keyID = ele.keyID;
            }
            return _this;
        }
        OSKInputEvent.prototype.simulateEventOn = function (target) {
            var oskKeyElement = document.getElementById(this.keyID);
            if (!oskKeyElement) {
                console.error('Could not find OSK key "' + this.keyID + '"!');
                // The following lines will throw an appropriate-enough error.
                return;
            }
            // To be safe, we replicate the MouseEvent similarly to the keystroke event.
            var downEvent;
            var upEvent;
            if (typeof Event == 'function') {
                if (window['keyman'].isEmbedded || (target['base'] && target instanceof HTMLDivElement)) {
                    downEvent = new Event(OSKInputEvent.downTouchType);
                    upEvent = new Event(OSKInputEvent.upTouchType);
                    downEvent['touches'] = [{ "target": oskKeyElement }];
                    upEvent['touches'] = [{ "target": oskKeyElement }];
                    downEvent['changedTouches'] = [{ "target": oskKeyElement }];
                    upEvent['changedTouches'] = [{ "target": oskKeyElement }];
                }
                else {
                    downEvent = new Event(OSKInputEvent.downMouseType);
                    upEvent = new Event(OSKInputEvent.upMouseType);
                    downEvent['relatedTarget'] = target;
                    upEvent['relatedTarget'] = target;
                }
            }
            else {
                downEvent = document.createEvent(OSKInputEvent.eventClass);
                downEvent.initMouseEvent(OSKInputEvent.downMouseType, false, true, null, null, 0, 0, 0, 0, false, false, false, false, 0, oskKeyElement);
                upEvent = document.createEvent(OSKInputEvent.eventClass);
                upEvent.initMouseEvent(OSKInputEvent.upMouseType, false, true, null, null, 0, 0, 0, 0, false, false, false, false, 0, oskKeyElement);
            }
            oskKeyElement.dispatchEvent(downEvent);
            oskKeyElement.dispatchEvent(upEvent);
        };
        OSKInputEvent.eventClass = "MouseEvent";
        OSKInputEvent.downMouseType = "mousedown";
        OSKInputEvent.upMouseType = "mouseup";
        OSKInputEvent.downTouchType = "touchstart";
        OSKInputEvent.upTouchType = "touchend";
        return OSKInputEvent;
    }(InputEvent));
    KMWRecorder.OSKInputEvent = OSKInputEvent;
    var resetElement = function (ele) {
        if (ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
            window['keyman'].resetContext();
            ele.value = "";
        }
        else {
            window['keyman'].resetContext();
            if (ele['base']) {
                // Gotta be extra-careful with the simulated touch fields!
                window['keyman'].touchAliasing.setText(ele, "", 0);
            }
            else {
                ele.textContent = "";
            }
        }
    };
    var InputTestSequence = /** @class */ (function () {
        function InputTestSequence(ins, outs, msg) {
            if (ins) {
                if (ins instanceof Array) {
                    this.inputs = [].concat(ins);
                }
                else {
                    // We're constructing from existing JSON.
                    this.inputs = [];
                    for (var ie = 0; ie < ins.inputs.length; ie++) {
                        this.inputs.push(InputEvent.fromJSONObject(ins.inputs[ie]));
                    }
                    this.output = ins.output;
                    this.msg = ins.msg;
                    return;
                }
            }
            else {
                this.inputs = [];
            }
            if (outs) {
                this.output = outs;
            }
            if (msg) {
                this.msg = msg;
            }
        }
        InputTestSequence.prototype.addInput = function (event, output) {
            this.inputs.push(event);
            this.output = output;
        };
        InputTestSequence.prototype.simulateSequenceOn = function (ele, assertCallback) {
            resetElement(ele);
            for (var i = 0; i < this.inputs.length; i++) {
                this.inputs[i].simulateEventOn(ele);
            }
            var result;
            if (ele instanceof HTMLInputElement || ele instanceof HTMLTextAreaElement) {
                result = ele.value;
            }
            else {
                result = ele.textContent;
            }
            if (assertCallback) {
                assertCallback(result, this.output, this.msg);
            }
            return { success: (result == this.output), result: result };
        };
        InputTestSequence.prototype.toPrettyJSON = function () {
            var str = "{ ";
            if (this.output) {
                str += "\"output\": \"" + this.output + "\", ";
            }
            str += "\"inputs\": [\n";
            for (var i = 0; i < this.inputs.length; i++) {
                str += "  " + this.inputs[i].toPrettyJSON() + ((i == this.inputs.length - 1) ? "\n" : ",\n");
            }
            if (this.msg) {
                str += "], \"message\": \"" + this.msg + "\" }";
            }
            else {
                str += "]}";
            }
            return str;
        };
        InputTestSequence.prototype.hasOSKInteraction = function () {
            for (var i = 0; i < this.inputs.length; i++) {
                if (this.inputs[i] instanceof OSKInputEvent) {
                    return true;
                }
            }
            return false;
        };
        return InputTestSequence;
    }());
    KMWRecorder.InputTestSequence = InputTestSequence;
    var FontStubForLanguage = /** @class */ (function () {
        function FontStubForLanguage(activeStubEntry) {
            this.family = activeStubEntry.family;
            var src = activeStubEntry.files;
            if (!(src instanceof Array)) {
                src = [src];
            }
            this.source = [];
            for (var i = 0; i < src.length; i++) {
                this.source.push(activeStubEntry.path + src[i]);
            }
        }
        return FontStubForLanguage;
    }());
    var LanguageStubForKeyboard = /** @class */ (function () {
        function LanguageStubForKeyboard(activeStub) {
            if (activeStub.KLC) {
                this.id = activeStub.KLC;
                this.name = activeStub.KL;
                this.region = activeStub.KR;
                // Fonts.
                if (activeStub.KFont) {
                    this.font = new FontStubForLanguage(activeStub.KFont);
                }
                if (activeStub.KOskFont) {
                    this.oskFont = new FontStubForLanguage(activeStub.KOskFont);
                }
            }
            else {
                this.id = activeStub.id;
                this.name = activeStub.name;
                this.region = activeStub.region;
                // If we end up adding functionality to FontStubForLanguage, we'll need to properly reconstruct these.
                this.font = activeStub.font;
                this.oskFont = activeStub.oskFont;
            }
        }
        return LanguageStubForKeyboard;
    }());
    var KeyboardStub = /** @class */ (function () {
        // Constructs a stub usable with KeymanWeb's addKeyboards() API function from
        // the internally-tracked ActiveStub value for that keyboard.
        function KeyboardStub(activeStub) {
            if (activeStub.KI) {
                this.id = activeStub.KI;
                this.id = this.id.replace('Keyboard_', '');
                this.name = activeStub.KN;
                this.filename = activeStub.KF;
                this.languages = [new LanguageStubForKeyboard(activeStub)];
            }
            else {
                this.id = activeStub.id;
                this.name = activeStub.name;
                this.filename = activeStub.filename;
                if (activeStub.languages instanceof Object) {
                    this.languages = new LanguageStubForKeyboard(activeStub.languages);
                }
                else {
                    this.languages = [];
                    for (var i = 0; i < activeStub.languages.length; i++) {
                        this.languages.push(new LanguageStubForKeyboard(activeStub.languages[i]));
                    }
                }
            }
        }
        KeyboardStub.prototype.setBasePath = function (filePath, force) {
            var linkParser = document.createElement("a");
            linkParser.href = filePath;
            if (force === undefined) {
                force = true;
            }
            if (force || (this.filename.indexOf(linkParser.protocol) < 0 && this.filename.indexOf('/') != 0)) {
                var file = this.filename.substr(this.filename.lastIndexOf('/') + 1);
                this.filename = filePath + '/' + file;
            }
        };
        KeyboardStub.prototype.getFirstLanguage = function () {
            if (this.languages instanceof LanguageStubForKeyboard) {
                return this.languages.id;
            }
            else {
                return this.languages[0].id;
            }
        };
        return KeyboardStub;
    }());
    KMWRecorder.KeyboardStub = KeyboardStub;
    var Constraint = /** @class */ (function () {
        function Constraint(target, validOSList, validBrowsers) {
            if (typeof (target) == 'string') {
                this.target = target;
                this.validOSList = validOSList;
                this.validBrowsers = validBrowsers;
            }
            else {
                var json = target;
                this.target = json.target;
                this.validOSList = json.validOSList;
                this.validBrowsers = json.validBrowsers;
            }
        }
        Constraint.prototype.matchesClient = function (device, usingOSK) {
            // #1:  Platform check.
            if (usingOSK === true) {
                if (this.target != device.formFactor) {
                    return false;
                }
            }
            else if (usingOSK === false) {
                if (this.target != 'hardware') {
                    return false;
                }
            }
            else if (this.target != device.formFactor && this.target != 'hardware') {
                return false;
            }
            if (this.validOSList) {
                if (this.validOSList.indexOf(device.OS) == -1) {
                    return false;
                }
            }
            if (this.validBrowsers) {
                if (this.validBrowsers.indexOf(device.browser) == -1) {
                    return false;
                }
            }
            return true;
        };
        // Checks if another Constraint instance is functionally identical to this one.
        Constraint.prototype.equals = function (other) {
            if (this.target != other.target) {
                return false;
            }
            var list1 = this.validOSList ? this.validOSList : ['any'];
            var list2 = other.validOSList ? other.validOSList : ['any'];
            if (list1.sort().join(',') != list2.sort().join(',')) {
                return false;
            }
            list1 = this.validBrowsers ? this.validBrowsers : ['web'];
            list2 = other.validBrowsers ? other.validBrowsers : ['web'];
            if (list1.sort().join(',') != list2.sort().join(',')) {
                return false;
            }
            return true;
        };
        return Constraint;
    }());
    KMWRecorder.Constraint = Constraint;
    var TestFailure = /** @class */ (function () {
        function TestFailure(constraint, test, output) {
            this.constraint = constraint;
            this.test = test;
            this.result = output;
        }
        return TestFailure;
    }());
    var InputTestSet = /** @class */ (function () {
        function InputTestSet(constraint) {
            if ("target" in constraint) {
                this.constraint = constraint;
                this.testSet = [];
            }
            else {
                var json = constraint;
                this.constraint = new Constraint(json.constraint);
                this.testSet = [];
                // Clone each test sequence / reconstruct from methodless JSON object.
                for (var i = 0; i < json.testSet.length; i++) {
                    this.testSet.push(new InputTestSequence(json.testSet[i]));
                }
            }
        }
        InputTestSet.prototype.addTest = function (seq) {
            this.testSet.push(seq);
        };
        // Validity should be checked before calling this method.
        InputTestSet.prototype.run = function (ele, assertCallback) {
            var failures = [];
            for (var i = 0; i < this.testSet.length; i++) {
                var testSeq = this.testSet[i];
                var simResult = testSeq.simulateSequenceOn(ele, assertCallback);
                if (!simResult.success) {
                    // Failed test!
                    failures.push(new TestFailure(this.constraint, testSeq, simResult.result));
                }
            }
            return failures.length > 0 ? failures : null;
        };
        // Used to determine if the current InputTestSet is applicable to be run on a device.
        InputTestSet.prototype.isValidForCurrentClient = function (usingOSK) {
            var device = new Device();
            device.detect();
            return this.constraint.matchesClient(device, usingOSK);
        };
        return InputTestSet;
    }());
    var KeyboardTest = /** @class */ (function () {
        /**
         * Reconstructs a KeyboardTest object from its JSON representation, restoring its methods.
         * @param fromJSON
         */
        function KeyboardTest(fromJSON) {
            if (!fromJSON) {
                this.keyboard = null;
                this.inputTestSets = [];
                return;
            }
            else if (typeof (fromJSON) == 'string') {
                fromJSON = JSON.parse(fromJSON);
            }
            else if (fromJSON instanceof KeyboardStub) {
                this.keyboard = fromJSON;
                this.inputTestSets = [];
                return;
            }
            fromJSON = fromJSON;
            this.keyboard = new KeyboardStub(fromJSON.keyboard);
            this.inputTestSets = [];
            for (var i = 0; i < fromJSON.inputTestSets.length; i++) {
                this.inputTestSets[i] = new InputTestSet(fromJSON.inputTestSets[i]);
            }
        }
        KeyboardTest.prototype.addTest = function (constraint, seq) {
            for (var i = 0; i < this.inputTestSets.length; i++) {
                if (this.inputTestSets[i].constraint.equals(constraint)) {
                    this.inputTestSets[i].addTest(seq);
                    return;
                }
            }
            var newSet = new InputTestSet(new Constraint(constraint));
            this.inputTestSets.push(newSet);
            newSet.addTest(seq);
        };
        KeyboardTest.prototype.run = function (ele, usingOSK, assertCallback) {
            var setHasRun = false;
            var failures = [];
            window['keyman'].setActiveElement(ele['base'] ? ele['base'] : ele);
            for (var i = 0; i < this.inputTestSets.length; i++) {
                var testSet = this.inputTestSets[i];
                if (testSet.isValidForCurrentClient(usingOSK)) {
                    var testFailures = testSet.run(ele, assertCallback);
                    if (testFailures) {
                        failures = failures.concat(testFailures);
                    }
                    setHasRun = true;
                }
            }
            if (!setHasRun) {
                // The sets CAN be empty, allowing silent failure if/when we actually want that.
                console.warn("No test sets for this keyboard were applicable for this device!");
            }
            // Allow the method's caller to trigger a 'fail'.
            if (failures.length > 0) {
                return failures;
            }
            else {
                return null;
            }
        };
        KeyboardTest.prototype.isEmpty = function () {
            return this.inputTestSets.length == 0;
        };
        return KeyboardTest;
    }());
    KMWRecorder.KeyboardTest = KeyboardTest;
})(KMWRecorder || (KMWRecorder = {}));
//# sourceMappingURL=recorder_InputEvents.js.map