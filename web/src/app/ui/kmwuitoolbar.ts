/***
   KeymanWeb 17.0
   Copyright 2019-2023 SIL International
***/

import type { KeymanEngine, KeyboardCookie, UIModule } from 'keyman/app/browser';

declare var keyman: KeymanEngine

type MapRegion = {
  /** The title of the region */
  t: string,

  /** The geometrical specification, in string form, for the region of the map to highlight */
  m: string
}

type ToolbarCookie = {
  /**
   * The two-letter code for the currently-selected map region
   */
  region: string;

  /**
   * The number of encoded 'recent'-keyboard entries enumerated within the cookie.
   */
  maxrecent: number;
} & Record<`recent${number}`, string>;

type KeyboardDetail = ReturnType<KeymanEngine['_GetKeyboardDetail']>;

type LanguageEntry = {
  /** Language id */
  id: string;

  /** Language ID */
  name: string;

  /**
   * A rich list of keyboard metadata for keyboards matching this language's language code (`id`).
   */
  keyboards: KeyboardDetail[];
}

// If a UI module has been loaded, we can rely on the publically-published 'name' property
// having been set as a way to short-out a UI reload.  Its parent object always exists by
// this point in the build process.
if(!keyman?.ui?.name) {
  /********************************/
  /*                              */
  /* Toolbar User Interface       */
  /*                              */
  /********************************/

  /**
   * Do not enclose in an anonymous function, as the compiler may create
   * global scope variables to replace true, false, null, which can collide
   * with other variables.
   * Instead, use the --output-wrapper command during optimization, which will
   * add the anonymous function to enclose all code, including those optimized
   * variables which would otherwise have global scope.
   **/

  try {
    // Declare KeymanWeb, OnScreen keyboard and Util objects
    const keymanweb=keyman;
    const util=keymanweb.util;

    // Disable UI for touch devices
    if(util.isTouchDevice()) {
      throw '';
    }

    // User interface local variables
    class ToolbarUI implements UIModule {
      init = false;

      toolbarNode: HTMLDivElement = null;
      backgroundNode = null;
      browseMapNode: HTMLDivElement = null;
      keyboardsButtonNode: HTMLDivElement = null;
      languageButtonsNode: HTMLDivElement = null;
      offButtonNode: HTMLDivElement = null;
      offBarNode: HTMLDivElement = null;
      oskButtonNode: HTMLDivElement = null;
      oskBarNode: HTMLDivElement = null;
      selectorNode: HTMLDivElement = null;
      regionLanguageListNodes: Record<string, HTMLDivElement> = {};
      regionsNode: HTMLDivElement = null;
      regionNodes = null;
      langKeyboardNodes = [];
      langKeyboardListNodes = [];
      selectedRegion = 'as';

      /**
       * Tracks a list of recently-selected keyboards.
       *
       * This list may be used to pick the most-recent **and** still-available keyboard upon
       * page refresh.  (If either condition isn't met, it's ignored and the next in line
       * is checked until one is found that meets both conditions.)
       */
      listedKeyboards = [];
      catchAllRegion = 'un';

      /**
       * A seed value for tracking listedKeyboards entry 'age'; smaller values = older, as this
       * seed is incremented on each keyboard change.
       */
      keyboardListPriority = 0;

      /**
       * The maximum length of the `listedKeyboards` array; once this length is reached, addition
       * of a new entry will result in pruning the oldest in the list, as with a cache.
       */
      maxListedKeyboards = 1;
      lastActiveControl = null;
      selectedKeyboard: KeyboardDetail = null;
      selectedLanguage = '';
      helpOffsetX = 0;
      helpOffsetY = 0;

      keyboardsForLangPopup = null;
      lastSelectedKeyboard = null;

      regions: Record<string, MapRegion>;

      /**
       * If no prior KeymanWebControl element exists, the Toolbar UI will generate its own
       * during initialization and insert it.  That reference is stored here so that we may
       * remove it later if/when `.shutdown()` is called.
       */
      _insertedElem: HTMLElement;

      /**
       * associative-array of objects { id: <langid>, name: <name>, keyboards: <array of KeymanWeb keyboards> }
       */
      languages: Record<string, LanguageEntry> = {};

      /**
       * flag to control re-initialization of keyboard map
       */
      updateMap = false;

      /**
       * initialization timer - wait 2 seconds after last stub installed then initialize map
       */
      startTimer = 0;

      /**
       * language name in toolbar
       */
      lgText = ''

      readonly name = 'toolbar';

      /**
       * Appears to be the i18n/l10n setup for the KMW Toolbar UI.
       */
      ToolBar_Text = {
        Keyboards: 'Languages',
        OffTitle: 'Turn off KeymanWeb keyboards',
        Off: 'Off',
        ShowOSK: 'Show On Screen Keyboard',
        LanguageSelector: 'Select language',

        SelectKeyboardPre: 'Select ',
        SelectKeyboardSuf: 'keyboard',
        AltKeyboardsPre: 'Alternate keyboards for ',
        AltKeyboardsSuf: '',

        ca: 'Central America',
        sa: 'South America',
        na: 'Americas',
        eu: 'Europe',
        af: 'Africa',
        un: 'Undetermined',
        as: 'Asia',
        oc: 'Oceania'
      };

      /**
       * Create some of the controls but don't insert them into the document yet
       * This does not need or want to prevent loss of focus, so uses document.createElement
       * rather than util.createElement.
       *
       * @param       {string}  tag
       * @param       {?string=}  id
       * @param       {?string=}  className
       * @param       {string=}  innerHTML
       * @return      {Node|null}
       */
      private createNode<E extends keyof HTMLElementTagNameMap>(tag: E, id?: string, className?: string, innerHTML?: string) {
        const node = document.createElement(tag);
        if(id) {
          node.id = id;
        }
        if(className) {
          node.className = className;
        }
        if(innerHTML) {
          node.innerHTML = innerHTML;
        }

        // The following is OK for IE and for F3.5 and later - should simply be ignored for others
        // first tried preventDefault() as suggested by many on web, which worked but interfered with keyboard and OSK enabling
        if(tag == 'a' || tag == 'area' || tag == 'map') {
          node.ondragstart = () => false;
        }
        return node;
      }

      /**
       * Initialize toolbar UI
       */
      initialize() {
        if(!keymanweb.initialized || this.init) {
          return;
        }

        // Find the controller DIV, insert at top of body if undefined
        let e = document.getElementById('KeymanWebControl');
        if(!e) {
          if(document.body == null) {
            return;
          } else {
            e = document.createElement('div');
            e.id = 'KeymanWebControl';
            document.body.insertBefore(e, document.body.firstChild);
            this._insertedElem = e;
          }
        }

        // Hide toolbar until elements fully drawn, to prevent spurious text displays
        e.style.visibility='hidden';
        e.style.maxHeight='35px';

        this.init = true;

        if(util.isTouchDevice()) {
          return;
        }

        this.regions = {};
        //ui.regions['ca'] = {t: ui.ToolBar_Text['ca'], m: '49,52,65,54,68,57,71,56,73,59,75,60,93,61,94,58,97,58,101,59,107,60,114,64,115,68,114,77,104,74,98,75,96,78,95,82,90,81,85,80,82,76,78,74,74,73,65,68,57,61' },
        //ui.regions['sa'] = {t: ui.ToolBar_Text['sa'], m: '82,82,95,82,96,78,98,75,104,74,114,77,120,79,124,83,126,87,141,90,142,97,138,103,135,113,127,116,123,124,115,131,112,132,109,138,117,139,140,141,141,146,134,148,114,145,109,148,100,148,91,143,91,130,96,111,89,102,83,95,77,89' },
        this.regions['na'] = {t: this.ToolBar_Text['na'], m: '0,3,0,37,24,32,35,37,43,47,49,52,65,54,68,57,71,56,73,59,75,60,93,61,93,57,103,49,118,41,126,41,136,23,148,17,156,14,164,5,164,0,57,0,35,5,25,9,5,8,49,52,65,54,68,57,71,56,73,59,75,60,93,61,94,58,97,58,101,59,107,60,114,64,115,68,114,77,104,74,98,75,96,78,95,82,90,81,85,80,82,76,78,74,74,73,65,68,57,61,82,82,95,82,96,78,98,75,104,74,114,77,120,79,124,83,126,87,141,90,142,97,138,103,135,113,127,116,123,124,115,131,112,132,109,138,117,139,140,141,141,146,134,148,114,145,109,148,100,148,91,143,91,130,96,111,89,102,83,95,77,89' },
        this.regions['eu'] = {t: this.ToolBar_Text['eu'], m: '145,29,146,19,158,14,171,6,187,2,206,1,217,4,227,11,231,16,231,33,227,34,225,35,225,37,227,39,228,44,228,47,227,48,223,46,218,44,215,43,208,43,203,45,202,48,205,52,201,52,195,49,189,50,187,48,177,48,175,49,166,50,147,33' },
        this.regions['af'] = {t: this.ToolBar_Text['af'], m: '150,58,158,50,166,50,175,49,177,48,187,48,189,50,195,49,201,52,205,52,207,53,221,75,229,75,231,77,231,85,227,92,232,101,237,106,237,112,227,115,222,118,206,125,199,127,193,127,185,111,183,104,180,87,168,89,153,85,143,71,147,60' },
        this.regions['as'] = {t: this.ToolBar_Text['as'], m: '219,1,221,6,228,12,231,16,231,33,227,34,225,35,225,37,227,39,229,45,232,48,239,48,240,49,239,53,242,60,243,65,249,70,252,81,259,87,271,87,278,95,289,100,303,101,311,98,320,98,323,98,323,84,311,81,308,73,307,65,317,57,330,50,334,44,348,36,364,38,375,34,375,8,355,8,336,5,292,1,285,0,219,0' },
        this.regions['oc'] = {t: this.ToolBar_Text['oc'], m: '288,117,289,107,303,101,311,98,323,98,323,84,333,77,344,73,362,80,369,88,375,96,375,141,352,143,323,142,316,136,310,130,291,130' }
        this.regions['un'] = {t: this.ToolBar_Text['un'], m: '205,52,202,48,203,45,208,43,215,43,218,44,223,46,227,48,232,48,239,48,240,49,239,53,242,60,243,65,237,76,231,77,229,75,221,75,207,53' },

        this.toolbarNode = this.createNode('div', 'kmw_controls');
        this.toolbarNode.style.display='block';

        util.linkStyleSheet(util.getOption('resources')+'ui/toolbar/kmwuitoolbar.css');

        const tbNode = this.createNode('a', 'kmw_controls_start', null, ' ');
        tbNode.href = "https://keyman.com/developer/keymanweb/";
        tbNode.target="_blank";
        this.toolbarNode.appendChild(tbNode);

        /* Keyboards button */
        this.keyboardsButtonNode = this.createNode('div','kmw_btn_keyboards','kmw_button');
        this.keyboardsButtonNode.title=this.ToolBar_Text.LanguageSelector;
        let aNode = this.createNode('a', null, 'kmw_button_a');
        aNode.href='#';
        aNode.onclick = this.showKeyboardsPopup;
        aNode.appendChild(this.createNode('div', 'kmw_img_keyboards', 'kmw_img'));
        aNode.appendChild(this.createNode('div', null, 'kmw_a', this.ToolBar_Text.Keyboards));
        aNode.appendChild(this.createNode('div', null, 'kmw_drop'));
        this.keyboardsButtonNode.appendChild(aNode);

        /* Keyboards popup */
        this.selectorNode = this.createNode('div', 'kmw_selector');
        this.regionsNode = this.createNode('div', 'kmw_selector_regions');

        this.browseMapNode = this.createNode('div', 'kmw_browsemap');
        const imgNode = this.createNode('img', 'kmw_region_browsemap');
        imgNode.src= util.getOption('resources')+'ui/toolbar/blank.gif';
        imgNode.useMap = '#kmw_worldgrey16';
        this.browseMapNode.appendChild(imgNode);

        const mapNode = this.createNode('map', 'kmw_worldgrey16');
        mapNode.name='kmw_worldgrey16';
        for(let i in this.regions) {
          const areaNode = this.createNode('area');
          areaNode.shape = 'poly';
          areaNode.alt = '';
          areaNode.href = '#';
          areaNode.title = this.regions[i].t;
          areaNode['hidefocus'] = 'true';
          areaNode.onclick =     ((i) => { return (event) => this.selectRegion(event, i);  })(i);
          areaNode.onmouseover = ((i) => { return (event) => this.hoverRegion(event, i);   })(i);
          areaNode.onmouseout =  ((i) => { return (event) => this.unhoverRegion(event, i); })(i);
          areaNode.coords = this.regions[i].m;
          mapNode.appendChild(areaNode);
        }

        const areaNode = this.createNode('area');
        areaNode.shape = 'default';
        areaNode.noHref = true;
        areaNode.alt = '';
        areaNode.onclick=this.eventCapture;   // do not close map when clicking on ocean!
        mapNode.appendChild(areaNode);
        this.browseMapNode.appendChild(mapNode);
        this.regionsNode.appendChild(this.browseMapNode);
        this.regionNodes = {};

        const listNode = this.createNode('ul');
        for(let i in this.regions) {
          const itemNode = this.createNode('li');
          this.regionNodes[i] = this.createNode('a', null, null, this.regions[i].t);
          this.regionNodes[i].href='#';
          this.regionNodes[i].onclick = ((i) =>     { return (event) => this.selectRegion(event, i);  })(i);
          this.regionNodes[i].onmouseover = ((i) => { return (event) => this.hoverRegion(event, i);   })(i);
          this.regionNodes[i].onmouseout = ((i) =>  { return (event) => this.unhoverRegion(event, i); })(i);
          itemNode.appendChild(this.regionNodes[i]);
          listNode.appendChild(itemNode);
        }
        this.regionsNode.appendChild(listNode);
        this.selectorNode.appendChild(this.regionsNode);
        this.keyboardsButtonNode.appendChild(this.selectorNode);
        this.toolbarNode.appendChild(this.keyboardsButtonNode);

        // Separator and Keyboard Off Button
        this.toolbarNode.appendChild(this.offBarNode = this.createNode('div', 'kmw_bar_off', 'kmw_bar'));

        this.offButtonNode = this.createNode('div', 'kmw_btn_off', 'kmw_button_selected');

        // Re-used variable!
        aNode = this.createNode('a', null, 'kmw_button_a');
        aNode.href = '#';
        aNode.onclick = this.offButtonClickEvent;
        aNode.title = this.ToolBar_Text.OffTitle;
        aNode.appendChild(this.createNode('div', 'kmw_img_off', 'kmw_img'));
        aNode.appendChild(this.createNode('div', null, 'kmw_a', this.ToolBar_Text.Off));
        this.offButtonNode.appendChild(aNode);
        this.toolbarNode.appendChild(this.offButtonNode);

        // Keyboard buttons
        this.toolbarNode.appendChild(this.languageButtonsNode = this.createNode('div', 'kmw_control_keyboards', 'kmw_button'));

        // Separator and On Screen Keyboard Button
        this.toolbarNode.appendChild(this.oskBarNode = this.createNode('div', 'kmw_bar_osk', 'kmw_bar'));

        this.oskButtonNode = this.createNode('div', 'kmw_btn_osk', 'kmw_button');//was 'kmw_button_selected'

        // Re-used variable!
        aNode = this.createNode('a', null, 'kmw_button_a');
        aNode.href = '#';
        aNode.onclick = this.showOSK;
        aNode.onmousedown = function() {
          keymanweb.activatingUI(true);
        };
        aNode.title = this.ToolBar_Text.ShowOSK;
        aNode.appendChild(this.createNode('div', 'kmw_img_osk', 'kmw_img'));
        //aNode.appendChild(ui.createNode('div', null, 'kmw_a', 'On Screen Keyboard'));
        this.oskButtonNode.appendChild(aNode);
        this.toolbarNode.appendChild(this.oskButtonNode);

        this.toolbarNode.appendChild(this.createNode('div', 'kmw_controls_end', null, ' '));

        const img = this.createNode('div');
        img.id = 'kmw_map_preload';
        this.toolbarNode.appendChild(img);

        this.toolbarNode.appendChild(this.createNode('br', null, 'kmw_clear'));

        // Append toolbar node to controller
        e.appendChild(this.toolbarNode);

        // Initialize map array, using a timer to allow restarting if necessary after keyboards loaded
        // Note that toolbar will be displayed and enabled on completion of timeout routine
        this.updateMap = true;
        if(this.startTimer) {
          clearTimeout(this.startTimer);
        }
        this.startTimer = window.setTimeout(this.addKeyboardsToMap, 0);

        // Ensure that popups are hidden by clicking elsewhere on document
        util.attachDOMEvent(document.body,'click', this.hideAllPopups, false);

        // Set Europe to be the default region
        this.selectedRegion = 'eu';

        this.registerEvents();

        // Restore focus
        keymanweb.focusLastActiveElement();
      }

      shutdown() {
        let root: HTMLElement = this.toolbarNode;
        if(root) {
          root.parentNode.removeChild(root);
        }

        root = this._insertedElem;
        if(root) {
          root.parentNode.removeChild(root);
        }
      }

      /**
       * Fill the map with available keyboards when the Keyboards selector is clicked
       *    after all keyboard stubs have been registered
      **/
      readonly addKeyboardsToMap = () => {
        // Do nothing unless a keyboard has been installed since map created
        if(this.updateMap) {
          this.updateMap=false;
        } else {
          return;
        }

        this.regionLanguageListNodes = {};

        // Build list of keyboards by region and language
        let Keyboards = keymanweb.getKeyboards();

        // Sort the keyboards by region and language
        Keyboards.sort(this.sortKeyboards);

        // Always rebuild the map, so remove any previously created language lists
        let n=0;
        for(n=this.regionsNode.children.length; n>0; n--) {
          if(this.regionsNode.children[n-1].className == 'kmw_selector_region') {
            this.regionsNode.removeChild(this.regionsNode.childNodes[n-1]);
          }
        }

        for(let i in this.regions) {
          this.regionLanguageListNodes[i] = this.createNode('div', null, 'kmw_selector_region');
          let colNode = this.createNode('div', null, 'kmw_keyboard_col');
          var max = 0, count = 0, languageCode = '';

          // Get number of languages for the region
          for(let j=0; j<Keyboards.length; j++) {
            // REVERT:  ensures that keyboards without visible map region get displayed SOMEWHERE.
            var kbdRegion = Keyboards[j].RegionCode;
            if(!this.regions[kbdRegion]) {
              // For now, we'll display them within the 'middle-east' region.
              if(i != this.catchAllRegion) {
                continue;
              }
            } else if(kbdRegion != i) {
              continue; // Not this region
            }

            // Get JUST the language code for this section.  BCP-47 codes can include more!
            var bcpSubtags: string[] = keymanweb.util.getLanguageCodes(Keyboards[j].LanguageCode);
            if(bcpSubtags[0] == languageCode) {
              continue; // Same language as previous keyboard
            }
            languageCode = bcpSubtags[0];

            max++;
          }
          max = Number(((max+3)/4).toFixed(0));   // Get number of entries per column

          // Add language list to columns for the region
          languageCode='';
          for(let j=0; j<Keyboards.length; j++) {
            // REVERT:  ensures that keyboards without visible map region get displayed SOMEWHERE.
            const kbdRegion = Keyboards[j].RegionCode;
            if(!this.regions[kbdRegion]) {
              // For now, we'll display them within the 'middle-east' region.
              if(i != this.catchAllRegion) {
                continue;
              }
            } else if(kbdRegion != i) {
              continue; // Not this region
            }

            var bcpSubtags: string[] = keymanweb.util.getLanguageCodes(Keyboards[j].LanguageCode);
            if(bcpSubtags[0] == languageCode) {  // Same language as previous keyboard, so add it to that entry
              var x = this.languages[languageCode].keyboards;

              // While we could avoid duplicating keyboard entries that occur for multiple regions, we'll instead
              // allow them to display while distinguishing them more directly.  (That part is handled later.)
              if(x.push) {
                x.push(Keyboards[j]);
              } else {
                this.languages[languageCode].keyboards = x.concat(Keyboards[j]);
              }

              continue;
            }

            // Add a new language entry
            languageCode = bcpSubtags[0];
            this.languages[languageCode] = {
              id: Keyboards[j].LanguageCode,
              name: Keyboards[j].LanguageName,
              keyboards: [Keyboards[j]]
            };

            // Start a new column if necessary
            if(count % max == 0 && count > 0) {
              this.regionLanguageListNodes[i].appendChild(colNode);
              colNode = this.createNode('div', null, count/max == 3 ? 'kmw_keyboard_col_right' : 'kmw_keyboard_col');
            }
            count++;

            // Add the language to the column
            const langNode = this.createNode('div', null, 'kmw_language');
            const aNode = this.createNode('a', null, null, Keyboards[j].LanguageName);
            aNode.href='#';
            aNode.onclick = ((lang) => {
              return (event) => this.selectLanguage(event, lang);
            }) (this.languages[languageCode]);

            langNode.appendChild(aNode);
            colNode.appendChild(langNode);

            // This is almost certainly an irrelevant code line; nothing seems to reference it here.
            n++;
          }
          // Finish the last column and close the list
          this.regionLanguageListNodes[i].appendChild(colNode);
          this.regionLanguageListNodes[i].appendChild(this.createNode('div', null, 'kmw_clear'));
          this.regionsNode.appendChild(this.regionLanguageListNodes[i]);
        }
        this.loadCookie();

        // Ensure that the correct region has been selected
        this.selectRegion(null, this.selectedRegion);
        this.enableControls();

        // When a keyboard is activated before init is complete,
        // the toolbar needs to refresh the loaded keyboard
        if(this.lastSelectedKeyboard) {
          this.changeKeyboardEvent(this.lastSelectedKeyboard);
        }

        // Restore focus
        keymanweb.focusLastActiveElement();
      }

      /**
       * Sort keyboards array returned from keymanweb by region and language
       *
       * @param       {Object}  a
       * @param       {Object}  b
       * @return      {number}
       **/
      readonly sortKeyboards = function(a: KeyboardDetail, b: KeyboardDetail) {
        if(a.RegionCode < b.RegionCode) {
          return -2;
        }

        if(a.RegionCode > b.RegionCode) {
          return 2;
        }

        if(a.LanguageName < b.LanguageName) {
          return -1;
        }

        if(a.LanguageName > b.LanguageName) {
          return 1;
        }

        return 0;
      }

      /* KeymanWeb Interfaces */

      /**
       * Function     findListedKeyboard
       * Scope        Private
       * @param       {Object}  lang
       * @return      {?number}
       * Description  Test if a keyboard is still shown on the toolbar.  Returns index of keyboard in listedKeyboards or null if not found
       */
      findListedKeyboard(lang: LanguageEntry | string) {
        if(typeof lang != 'string') {
          lang = lang.id;
        }
        for(let i = 0; i < this.listedKeyboards.length; i++) {
          if(this.listedKeyboards[i].lang.id == lang) {
            return i;
          }
        }

        return null;
      }

      /**
       * Add a keyboard to the list of keyboards available for a language
       *
       * @param       {Object}  lang
       * @param       {Object}  kbd
       **/
      addKeyboardToList(lang: LanguageEntry, kbd: KeyboardDetail) {
        let found = this.findListedKeyboard(lang);
        if(found == null) {
          // Add the button
          if(this.listedKeyboards.length >= this.maxListedKeyboards) {
            let oldestPriority = 0x7fffffff, oldestFound = null;

            for(let i = 0; i < this.listedKeyboards.length; i++) {
              if(this.listedKeyboards[i].priority < oldestPriority) {
                oldestFound = i;
                oldestPriority = this.listedKeyboards[i].priority;
              }
            }

            // delete the oldest used control
            if(oldestFound != null) {
              let rk = this.listedKeyboards[oldestFound];
              this.langKeyboardListNodes[rk.lang.id] = null;
              this.langKeyboardNodes[rk.lang.id] = null;
              this.languageButtonsNode.removeChild(rk.buttonNode);
              if(oldestFound == 0) {
                this.listedKeyboards = this.listedKeyboards.slice(oldestFound + 1);
              } else if(oldestFound == this.listedKeyboards.length - 1) {
                this.listedKeyboards = this.listedKeyboards.slice(0, oldestFound);
              } else {
                this.listedKeyboards = this.listedKeyboards.slice(0, oldestFound).concat(this.listedKeyboards.slice(oldestFound+1));
              }
            }
          }
          let buttonNode = this.createNode('div', null/*'kmw_button_keyboard_'+lang.id*/, 'kmw_button');
          let aNode = this.createNode('a', null, 'kmw_button_a'+(lang.keyboards.length>1 ? ' kmw_norightgap' : ''));
          aNode.href='#';

          let p1=this.ToolBar_Text['SelectKeyboardPre'] + kbd.Name;
          let p2=this.ToolBar_Text['SelectKeyboardSuf'];
          if(p1.toLowerCase().indexOf(p2.toLowerCase()) < 0) {
            p1=p1+' '+ p2;
          }
          aNode.title = p1;
          aNode.onclick = (event) => this.selectLanguage(event, lang);
          aNode.appendChild(this.createNode('div', 'kmw_img_kbd', 'kmw_img'));

          this.lgText=this.truncate(lang.name,28);
          aNode.appendChild(this.createNode('div', null, 'kmw_a', this.lgText));
          buttonNode.appendChild(aNode);

          let thisANode = aNode;

          if(lang.keyboards.length > 1) {
            aNode = this.createNode('a', null, 'kmw_button_a kmw_noleftgap');
            aNode.href = '#';
            aNode.title = this.ToolBar_Text['AltKeyboardsPre']+lang.name + this.ToolBar_Text['AltKeyboardsSuf'];
            aNode.onclick = (event) => this.showKeyboardsForLanguage(event, lang);
            let divNode = this.createNode('div', null, 'kmw_a');
            let kbdText = this.truncate(kbd.Name.replace(/\s?keyboard/i,''),40-this.lgText.length);
            divNode.appendChild(this.langKeyboardNodes[lang.id] = this.createNode('span', null, 'kmw_kbd', kbdText));
            aNode.appendChild(divNode);
            aNode.appendChild(this.createNode('div', null, 'kmw_drop'));
            buttonNode.appendChild(aNode);

            this.langKeyboardListNodes[lang.id] = this.createNode('ul', null, 'kmw_selector_kbd');
            this.langKeyboardListNodes[lang.id].style.display='none';

            for(let n in lang.keyboards) {
              let itemNode = this.createNode('li');
              kbdText = lang.keyboards[n].Name.replace(/\s?keyboard/i,'');
              // We append the full BCP-47 code here for disambiguation when regional and/or script variants exist.
              kbdText = kbdText + " [" + lang.keyboards[n].LanguageCode + "]";
              aNode = this.createNode('a', null, null, kbdText);
              aNode.href = '#';
              aNode.title = '';
              aNode.onclick = ((lang, kbd) => {
                return (event) => this.selectKeyboard(event, lang, kbd, true);
              })(lang, lang.keyboards[n]);

              itemNode.appendChild(aNode);
              this.langKeyboardListNodes[lang.id].appendChild(itemNode);
            }
            buttonNode.appendChild(this.langKeyboardListNodes[lang.id]);
          }

          this.languageButtonsNode.appendChild(buttonNode);

          let thisLang = lang, thisButtonNode = buttonNode;
          this.listedKeyboards.push({priority: this.keyboardListPriority++, lang:thisLang, keyboard:kbd, buttonNode:thisButtonNode, aNode:thisANode});
        } else {
          this.listedKeyboards[found].priority = this.keyboardListPriority++;
          this.listedKeyboards[found].keyboard = kbd;
          let e = this.langKeyboardNodes[lang.id];
          if(e) {
            var kbdText=kbd.Name.replace(/\s?keyboard/i,'');
            e.innerHTML = this.truncate(kbdText,40-this.lgText.length);
          }

          if(this.listedKeyboards[found].aNode) {
            let p1 = this.ToolBar_Text['SelectKeyboardPre']+kbd.Name;
            let p2 = this.ToolBar_Text['SelectKeyboardSuf'];
            if(p1.toLowerCase().indexOf(p2.toLowerCase()) < 0) {
              p1=p1+' '+ p2;
            }

            this.listedKeyboards[found].aNode.title = p1;
          }
        }
      }

      /**
       *  Truncate a long name and add an ellipsis
       *
       *  @param  {string}  PName   string that may need to be truncated
       *  @param  {number}  PLen    max non-truncated length
       *  @return {string}          string, truncated with ellipsis if necessary
       *
       **/
      truncate(PName: string, PLen: number) {
        if(PName.length <=PLen) {
          return PName;
        }

        return PName.substr(0,PLen-1)+'\u2026';
      }

      /**
       * Rebuild the entire keyboard list whenever a keyboard is installed
       * (without necessarily loading the keyboard)
       **/
      readonly registerKeyboard = () => {
        this.updateMap = true;
        if(this.startTimer) {
          clearTimeout(this.startTimer);
        }

        this.startTimer = window.setTimeout(this.addKeyboardsToMap, 0);
      }

      /**
       * Function     hideKeyboardsForLanguage
       * Scope        Private
       * @param       {Object}  event
       * @return      {boolean}
       * Description  Hide the list of keyboards for this language
       **/
      readonly hideKeyboardsForLanguage = (event: Event) => {
        var e = this.keyboardsForLangPopup;
        if(e) {
          e.style.display='none';
        }
        this.CancelPopupDismissal(this.hideKeyboardsForLanguage);
        return this.eventCapture(event);
      }

      /**
       * Display the list of keyboards for this language
       *
       * @param       {Object}  event
       * @param       {Object}  lang
       * @return      {boolean}
       *
       **/
      readonly showKeyboardsForLanguage = (event: Event, lang: LanguageEntry) => {
        this.hideKeyboardsPopup(event);
        const e = this.langKeyboardListNodes[lang.id];
        if(e) {
          if(e.style.display=='block') {
            return this.hideKeyboardsForLanguage(event);
          }

          e.style.display='block';
          this.keyboardsForLangPopup = e;
          this.SetupPopupDismissal(e, this.hideKeyboardsForLanguage);
        }
        return this.eventCapture(event);
      }

      /**
       * Select the language, and either select the keyboard (if unique) or display the list of keyboards
       * available for this language
       *
       * @param       {Object}  event
       * @param       {Object}  lang
       * @return      {boolean}
       **/
      selectLanguage(event: Event, lang: LanguageEntry) {
        let found = this.findListedKeyboard(lang);
        let kbd: KeyboardDetail = null;

        if(found == null) {
          kbd = lang.keyboards[0];
        } else {
          kbd = this.listedKeyboards[found].keyboard;
        }

        if(!kbd) {
          return false;
        }

        return this.selectKeyboard(event, lang, kbd, true);
      }


      /**
       * Enable a selected keyboard
       *
       * @param       {Object}  event
       * @param       {Object}  lang
       * @param       {Object}  kbd
       * @param       {boolean} updateKeyman
       * @return      {boolean}
       **/
      selectKeyboard(event: Event, lang: LanguageEntry, kbd: KeyboardDetail, updateKeyman: boolean) {
        keymanweb.activatingUI(true);

        if(this.selectedLanguage) {
          let found = this.findListedKeyboard(this.selectedLanguage);
          if(found != null) {
            this.listedKeyboards[found].buttonNode.className = 'kmw_button';
          }
        }

        this.offButtonNode.className = 'kmw_button';
        this.selectedKeyboard = kbd;

        // In 12.0, this UI class has only been partially converted to BCP-47.
        // `lang.id` refers to the base language identifier and will NOT include
        // any subtags.  We want the FULL language identifier here, with subtags.
        this.selectedLanguage = kbd.LanguageCode;

        // Return focus to input area and activate the selected keyboard
        this.addKeyboardToList(lang, kbd);
        if(updateKeyman) {
          keymanweb.setActiveKeyboard(kbd.InternalName, kbd.LanguageCode).then(() => {
            // Restore focus _after_ the keyboard finishes loading.
            this.setLastFocus();
          });
        }
        this.listedKeyboards[this.findListedKeyboard(lang)].buttonNode.className = 'kmw_button_selected';

        // Always save current state when selecting a keyboard
        this.saveCookie();
        this.enableControls();

        keymanweb.activatingUI(false);

        return this.hideKeyboardsPopup(event) || this.hideKeyboardsForLanguage(event);
      }

      /**
       * Enable all UI controls
       *
       * @return      {boolean}
       **/
      enableControls(): boolean {
        let elems = [
          this.offButtonNode,
          this.offBarNode,
          this.oskButtonNode,
          this.oskBarNode
        ];
        let hideOskButton=false;

        if(keymanweb.isCJK(this.selectedKeyboard)) {
          hideOskButton = true;
        } else if(this.selectedKeyboard == null) {
          hideOskButton = (elems[2].style.display == 'none');
        }

        if(this.selectedKeyboard != null || this.listedKeyboards.length > 0) {
          for(let i = 0; i < elems.length; i++) {
            elems[i].style.display='';
          }
        } else {
          for(let i = 0; i < elems.length; i++) {
            elems[i].style.display='none';
          }
        }

        if(hideOskButton) {
          this.oskButtonNode.style.display = this.oskBarNode.style.display = 'none';
        } else if(this.selectedKeyboard == null) {
          this.oskButtonNode.className='kmw_button_disabled';
        }
        // else {
        //   ui.oskButtonNode.className=(osk && osk.isEnabled() ? 'kmw_button_selected' : 'kmw_button');
        // }

        // Display the toolbar if still hidden
        this.toolbarNode.parentElement.style.visibility='visible';
        return true;
      }

      /**
       * Restore the focus to the last focused element
       **/
      setLastFocus() {
        keymanweb.focusLastActiveElement();
      }

      /**
       * Display or hide the OSK according to user control. This will always force
       * the focus to the last active element if currently unfocused, which is
       * preferable to not having the OSK appear when the OSK button is clicked.
       *
       * @param       {Object}  event
       * @return      {boolean}
       **/
      readonly showOSK = (event: Event) => {
        let osk = keymanweb.osk;
        if(!osk) {
          return;
        }
        keymanweb.activatingUI(true);
        //Toggle OSK on or off
        if(osk && keymanweb.getActiveKeyboard() != '') {
          if(osk.isEnabled()) {
            osk.hide();
           } else {
            osk.show(true);
           }
        }
        this.setLastFocus();
        keymanweb.activatingUI(false);
        return this.eventCapture(event);
      }


      /**
       * Function     offButtonClickEvent
       * Scope        Private
       * @param       {Object}  event
       * @return      {boolean}
       * Description  Update the UI when all keyboards disabled by user
       **/
      readonly offButtonClickEvent = (event: Event) => {
        if(this.toolbarNode.className != 'kmw_controls_disabled') {
          this.hideKeyboardsForLanguage(null);
          if(this.selectedLanguage) {
            var found = this.findListedKeyboard(this.selectedLanguage);
            if(found != null) {
              this.listedKeyboards[found].buttonNode.className = 'kmw_button';
            }
          }
          this.selectedKeyboard = null;
          this.selectedLanguage = null;
          this.offButtonNode.className = 'kmw_button_selected';
        }

        // Return the focus to the input area and set the active keyboard to nothing
        this.setLastFocus();
        keymanweb.setActiveKeyboard('','');

        //Save current state when deselecting a keyboard (may not be needed)
        this.saveCookie();
        this.enableControls();
        return this.eventCapture(event);
      }

      /**
       * Function     eventCapture
       * Scope        Private
       * @param       {Object}  event
       * @return      {boolean}
       * Description  Browser-independent event capture
       **/
      readonly eventCapture = (event?: Event) => {
        if(!event) {
          event = window.event;
        }
        if(window.event) {
          window.event.returnValue = false;
        }
        if(event) {
          event.cancelBubble = true;
        }

        return false;
      }

      /**
       * Function     selectRegion
       * Scope        Private
       * @param       {Object}  event
       * @param       {string}  region
       * @return      {boolean}
       * Description  Select the region for which to list languages
       **/
      readonly selectRegion = (event: Event, region: string) => {
        let e = this.browseMapNode;

        if(!e) {
          return this.eventCapture(event);
        }

        e.className = 'kmw_browsemap_'+region;
        if(typeof(this.regionLanguageListNodes[region]) == 'undefined') {
          this.updateMap = true;
          this.addKeyboardsToMap();
        }

        this.regionLanguageListNodes[region].style.display='block';
        this.regionNodes[region].className='selected';
        if(this.selectedRegion != null && this.selectedRegion != region) {
          this.regionLanguageListNodes[this.selectedRegion].style.display='none';
          this.regionNodes[this.selectedRegion].className='';
        }
        this.selectedRegion = region;
        //this.saveCookie();
        return this.eventCapture(event);
      }

      /**
       * Function     unhoverRegion
       * Scope        Private
       * @param       {Object}  event
       * @param       {string}  region
       * @return      {boolean}
       * Description  Remove highlighting from a region
       **/
      readonly unhoverRegion = (event: Event, region: string) => {
        this.browseMapNode.className = (this.selectedRegion == null ? '' : 'kmw_browsemap_' + this.selectedRegion);
        this.regionNodes[region].className=(this.selectedRegion==region?'selected':'');
        return this.eventCapture(event);
      }

      /**
       * Function     hoverRegion
       * Scope        Private
       * @param       {Object}  event
       * @param       {string}  region
       * @return      {boolean}
       * Description  Highlight a hovered region
       **/
      readonly hoverRegion = (event: Event, region: string) => {
        this.browseMapNode.className = 'kmw_browsemap_'+region+'_sel';
        this.regionNodes[region].className='hover';
        return this.eventCapture(event);
      }

      /**
       * Function     pluck
       * Scope        Private
       * @param       {Object}  elem
       * @param       {string}  property
       * @return      {*}
       * Description  Get the value of an element property
       **/
      pluck(elem, property) {
        return elem.getAttribute ? elem.getAttribute(property) || elem[property] : elem[property];
      };

      /**
       * Function     focusControlEvent
       * Scope        Private
       * @param       {Object}  params    Object containing Target element (or frame)
       * @return      {boolean}
       * Description  UI code to be executed on receiving focus
       */
      readonly focusControlEvent = (params: {target: HTMLElement, activeControl: HTMLElement}) => {
        if(!this.init) {
          return true;
        }

        let t=params.target;
        if(t.tagName.toLowerCase() == 'textarea' ||
          (t.tagName.toLowerCase() == 'input' && (t as HTMLInputElement).type.toLowerCase() == 'text')
        ) {
          this.lastActiveControl = t;
          if(this.pluck(t, 'kmw_disable')) {
            if(this.toolbarNode.className != 'kmw_controls_disabled') {
              this.toolbarNode.className = 'kmw_controls_disabled';
            }
          } else {
            if(this.selectedKeyboard != null) {
              if(keymanweb.isCJK()) {
                this.oskButtonNode.style.display = this.oskBarNode.style.display = 'none';
              } else {
                let osk = keymanweb.osk;
                this.oskButtonNode.className = (osk && osk.isEnabled()) ? 'kmw_button_selected' : 'kmw_button';
              }
            }

            if(this.toolbarNode.className != '') {
              this.toolbarNode.className = '';
            }

            // NOTE:  as best as I can tell, this is to be specified by the site developer, not KMW.
            // I don't see documentation for it on first glance, though.
            let offsetX, offsetY;
            if(t['KMW_HelpOffsetX']) {
              offsetX = t['KMW_HelpOffsetX'];
            } else {
              offsetX = 64;
            }

            if(t['KMW_HelpOffsetY']) {
              offsetY = t['KMW_HelpOffsetY'];
            } else {
              offsetY = 0;
            }

            this.helpOffsetX = util.getAbsoluteX(t)+offsetX;
            this.helpOffsetY = util.getAbsoluteY(t)+t.offsetHeight+offsetY;
          }
        }
        return true;
      }

      /**
       * Function     oncontrolblurred
       * Scope        Private
       * Parameters   {Object}  params  Object containing event
       * @return      {boolean}
       * Description  UI code to be executed on losing focus
       */
      readonly blurControlEvent = () => {
        if(!this.init) {
          return true;
        }

        // Must disable OSK button when not focused
        if(this.oskButtonNode.style.display != 'none') {
          this.oskButtonNode.className='kmw_button_disabled';
        }

        return true;
      }

      /**
       * UI action required when keyboard changed indirectly
       *
       * @param       {Object}  p   keyboard selection object
       * @return      {boolean}
       **/
      readonly changeKeyboardEvent = (p: {
        internalName: string,
        languageCode: string
      }) => {  // Uses a different format than .getKeyboards(), b/c why not?
        // https://help.keyman.com/developer/engine/web/16.0/reference/core/getKeyboards vs
        // https://help.keyman.com/developer/engine/web/16.0/reference/events/kmw.keyboardchange
        this.lastSelectedKeyboard = null;
        let kbName=p.internalName,
            lgName=keymanweb.util.getLanguageCodes(p.languageCode)[0];
        if(lgName != '' && kbName != '') {
          let lg = this.languages[lgName];
          if(lg != null) {
            for(let j=0; j<lg.keyboards.length; j++) {
              if(lg.keyboards[j].InternalName == kbName) {
                this.selectKeyboard(null, lg, lg.keyboards[j], false);
                return true;
              }
            }
          }
          this.lastSelectedKeyboard = {...p};
        }
        return true;
      }

      /**
       * UI action when OSK displayed: restore to or update the saved OSK position
       *
       * @param       {Object}  oskPosition
       * @return      {Object}
       **/
      readonly onShowOSK = (oskPosition) => {
        if(this.init) {
          this.oskButtonNode.className = 'kmw_button_selected';
        }

        //The return value is not currently useful, but allows for the possibility of the OSK restricting or limiting
        //a UI-set position, and returning the corrected position to the UI
        return oskPosition;
      }

      /**
       * Update appearance of OSK button whenever the OSK is hidden by the user
       *
       * @param       {Object}  p
       **/
      readonly onHideOSK = (p: {
        HiddenByUser: boolean
      }) => {
        if(this.init && p.HiddenByUser) {
          this.oskButtonNode.className = 'kmw_button';
        }
      }

      registerEvents() {
        let osk = keymanweb.osk;
        if(!osk) {
          return;
        }

        osk.addEventListener('show', this.onShowOSK);
        osk.addEventListener('hide', this.onHideOSK);
      }

      /**
       * Function     showKeyboardsPopup
       * Scope        Private
       * @param       {Object}  event
       * @return      {boolean}
       * Description  Update the map when displayed
       **/
      readonly showKeyboardsPopup = (event) => {
        // Add any newly available keyboards to the map
        this.addKeyboardsToMap();

        if(this.toolbarNode.className == 'kmw_controls_disabled') {
          return this.eventCapture(event);
        }
        this.hideKeyboardsForLanguage(null);
        if(this.selectorNode.className=='kmw_over') {
          return this.hideKeyboardsPopup(event);
        }
        this.selectorNode.className='kmw_over';
        this.keyboardsButtonNode.className='kmw_button_selected';

        this.SetupPopupDismissal(this.selectorNode, this.hideKeyboardsPopup);
        return this.eventCapture(event);
      }

      /**
       * Function     hideKeyboardsPopup
       * Scope        Private
       * @param       {Object}  event
       * @return      {boolean}
       * Description  Hide the list of keyboards for this language
       **/
      readonly hideKeyboardsPopup = (event: Event) => {
        this.selectorNode.className='';
        this.keyboardsButtonNode.className='kmw_button';
        this.CancelPopupDismissal(this.hideKeyboardsPopup);
        return this.eventCapture(event);
      }

      /**
       * Hide both language and keyboard selection popups on body click
       * @param   {Object}  event
       * @return  {boolean}
      **/
      readonly hideAllPopups = (event: Event) => {
        var e = this.keyboardsForLangPopup;
        if((!e || (e.style.display =='none')) && (this.selectorNode.className == '')) {
          return true;
        }
        this.hideKeyboardsPopup(event);
        this.hideKeyboardsForLanguage(event);
        return this.eventCapture(event);
      }

      // Fields only used for the three Popup-related methods that follow.

      /**
       * A closure to be evaluated upon dismissal of a modal popup generated
       * by this UI module.
       */
      dismissalCallback: (event: Event) => any = null;

      /**
       * The root element associated with an active modal popup; related
       * events will check to see if they are a descendant (or this element).
       */
      popupElement: HTMLElement = null;

      /**
       * As we replace any `onclick` event listener when acting modally
       * for the popup, we stash the old event listener here and restore
       * it when we're done.
       */
      lastDismissalCallback: (event: Event) => any = null;

      /**
       * Function     PopupDismissal
       * Scope        Private
       * @param       {Object}  event
       * @return      {Object}
       * Description  Carry out action when popup dismissed
       **/
      readonly PopupDismissal = (event: Event) => {
        let t = ((event && event.target) || (window.event && window.event.srcElement)) as HTMLElement;
        if(t) {
          while(t.parentElement) {
            if (t == this.popupElement) {
              return null;
            }
            t = t.parentElement;
          }
        }
        if(t.nodeName == '#document') {
          this.hideAllPopups(event); //KMEW-41, fixed for build 356.
        }
        return this.dismissalCallback;
      }

      /**
       * Function     SetupPopupDismissal
       * Scope        Private
       * @param       {Object}            element
       * @param       {function(Object)}  callback
       * Description  Prepare for callback dismissal
       **/
      SetupPopupDismissal(element, callback: (event: Event) => any) {
        if(this.PopupDismissal == document.onclick) {
          this.CancelPopupDismissal(this.dismissalCallback);
        }
        this.dismissalCallback = callback;
        this.popupElement = element;
        this.lastDismissalCallback = document.onclick;
        document.onclick = this.PopupDismissal;
      }

      /**
       * Function     CancelPopupDismissal
       * Scope        Private
       * @param       {?function(Object)}  callback
       * Description  Cancel callback dismissal
       **/
      CancelPopupDismissal(callback) {
        if(this.PopupDismissal == document.onclick) {
          document.onclick = this.lastDismissalCallback;
          this.lastDismissalCallback = null;
          this.dismissalCallback = null;
          this.popupElement = null;
        }
      }




      /**
       * Load the previous state from the KeymanWeb_Keyboard and KeymanWeb_Toolbar cookies
       **/
      loadCookie() {
        let currentKeyboard='';
        const kc=util.loadCookie<KeyboardCookie>("KeymanWeb_Keyboard");
        if(kc.current != undefined) {
          currentKeyboard = kc.current.split(':')[0];
        }

        const c=util.loadCookie<ToolbarCookie>("KeymanWeb_Toolbar");

        if(c.region != undefined) {
          this.selectedRegion = c.region;
        }

        // If there's a defined length for the cookie's keyboard count...
        if(c.maxrecent != undefined) {

          // Iterate over the implicit array
          for(let i=0; i < c.maxrecent; i++) {
            // Is the entry actually defined?
            if(c['recent'+i] != undefined) {
              var r=c['recent'+i].split(',');
              // Does its definition have the expected format?  That is: langID,keyboardID
              if(r.length == 2) {
                var k = this.languages[r[0]];
                // If the language has a defined entry in our list of loaded languages...
                if(k != null) {
                  // Then, for our loaded keyboards for said loaded language...
                  // find the most recent match that is available in our current list.
                  for(var j=0; j<k.keyboards.length; j++) {
                    if(k.keyboards[j].InternalName == r[1]) {
                      this.addKeyboardToList(k, k.keyboards[j]);
                      if(k.keyboards[j].InternalName == currentKeyboard) {
                        this.selectKeyboard(null, k, k.keyboards[j], true);
                        window.focus();
                        this.setLastFocus();
                        break;
                      }
                      break;
                    }
                  }
                }
              }
            }
          }
        } else {
          // If language list and keyboard list have not yet been saved as a cookie,
          // initialize to the current (default) language and keyboard, if set by KMW
          let kbName=keymanweb.getActiveKeyboard();
          let lgName=keymanweb.getActiveLanguage();
          if(lgName != '' && kbName != '') {
            var lg = this.languages[lgName];
            if(lg != null) {
              for(var j=0; j<lg.keyboards.length; j++) {
                if(lg.keyboards[j].InternalName == kbName) {
                  this.selectKeyboard(null, lg, lg.keyboards[j], true);
                  window.focus();
                  this.setLastFocus();
                }
              }
            }
          }
        }
      }

      /**
       * Save the current UI state in the KeymanWeb_Toolbar cookie
       **/
      saveCookie() {
        let vs: ToolbarCookie ={
          region: this.selectedRegion,
          maxrecent: this.listedKeyboards.length
        };
        vs.region = this.selectedRegion;
        vs.maxrecent = this.listedKeyboards.length;

        for(var i=0; i<this.listedKeyboards.length; i++) {
          vs['recent'+i] = this.listedKeyboards[i].lang.id+","+this.listedKeyboards[i].keyboard.InternalName;
        }

        util.saveCookie<ToolbarCookie>('KeymanWeb_Toolbar',vs);
      }

    }

    const ui = keymanweb.ui = new ToolbarUI();

    keymanweb.addEventListener('keyboardregistered', ui.registerKeyboard);
    keymanweb.addEventListener('controlfocused', ui.focusControlEvent);
    keymanweb.addEventListener('controlblurred', ui.blurControlEvent);
    keymanweb.addEventListener('keyboardchange', ui.changeKeyboardEvent);

    // Initialize when everything defined (replaces unreliable onload event handler)
    // In case the toolbar script loads a bit later than the main KMW script
    // (may happen in unit testing)
    ui.initialize(); // equivalent to ui.Initialize() from the other UI modules
  } catch(ex){}
}