/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

// If a UI module has been loaded, we can rely on the publically-published 'name' property
// having been set as a way to short-out a UI reload.  Its parent object always exists by
// this point in the build process.
if(!window['keyman']['ui']['name']) {
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
    var keymanweb=window['keyman'],osk=keymanweb['osk'],util=keymanweb['util'],dbg=keymanweb['debug'];

    // Disable UI for touch devices
    if(util['isTouchDevice']()) throw '';

    // User interface local variables
    keymanweb['ui'] = {
      init: false,
      toolbarNode: null,
      backgroundNode: null,
      browseMapNode: null,
      keyboardsButtonNode: null,
      languageButtonsNode: null,
      offButtonNode: null,
      offBarNode: null,
      oskButtonNode: null,
      oskBarNode: null,
      selectorNode: null,
      regionLanguageListNodes: [],
      regionNodes: null,
      langKeyboardNodes: [],
      langKeyboardListNodes: [],
      selectedRegion: 'as',
      listedKeyboards: [],
      catchAllRegion: 'un',
      keyboardListPriority: 0,
      maxListedKeyboards: 1,
      lastActiveControl: null,
      selectedKeyboard: null,
      selectedLanguage: '',
      helpOffsetX: 0,
      helpOffsetY: 0,
      languages: [],      // array of objects { id: <langid>, name: <name>, keyboards: <array of KeymanWeb keyboards> }
      updateMap: false,   // flag to control re-initialization of keyboard map
      startTimer: 0,      // initialization timer - wait 2 seconds after last stub installed then initialize map
      lgText: ''          // language name in toolbar
    }

    // User Interface object
    var ui=keymanweb['ui'];
    ui['name'] = 'toolbar';

    ui.ToolBar_Text = {
      Keyboards: 'Languages',
      OffTitle: 'Turn off KeymanWeb keyboards',
      Off: 'Off',
      ShowOSK: 'Show On Screen Keyboard',
      LanguageSelector: 'Select language'
      };

    ui.ToolBar_Text['SelectKeyboardPre']='Select ';
    ui.ToolBar_Text['SelectKeyboardSuf']='keyboard';
    ui.ToolBar_Text['AltKeyboardsPre']='Alternate keyboards for ';
    ui.ToolBar_Text['AltKeyboardsSuf']='';

    ui.ToolBar_Text['ca']='Central America';
    ui.ToolBar_Text['sa']='South America';
    ui.ToolBar_Text['na']='Americas';
    ui.ToolBar_Text['eu']='Europe';
    ui.ToolBar_Text['af']='Africa';
    ui.ToolBar_Text['un']='Undetermined';
    ui.ToolBar_Text['as']='Asia';
    ui.ToolBar_Text['oc']='Oceania';


    var controller=document.getElementById('KeymanWebControl');
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
    ui.createNode = function(tag, id, className, innerHTML)
    {
      //var node = util['createElement'](tag);
      var node = document.createElement(tag);
      if(id) node.id = id;
      if(className) node.className = className;
      if(innerHTML) node.innerHTML = innerHTML;

      // The following is OK for IE and for F3.5 and later - should simply be ignored for others
      // first tried preventDefault() as suggested by many on web, which worked but interfered with keyboard and OSK enabling
      if(tag == 'a' || tag == 'area' || tag == 'map') node.ondragstart = function(){return false;}
      return node;
    }

    /**
     * Initialize toolbar UI
     */
    ui['initialize'] = ui.initToolbarUI = function()
    {
      if(!keymanweb['initialized'] || ui.init) return;

      // Find the controller DIV, insert at top of body if undefined
      var e = document.getElementById('KeymanWebControl');
      if(!e)
      {
        if(document.body == null)
          return;
        else
        {
          e=document.createElement('DIV');
          e.id='KeymanWebControl';
          document.body.insertBefore(e,document.body.firstChild);
          ui._insertedElem = e;
        }
      }

      // Hide toolbar until elements fully drawn, to prevent spurious text displays
      e.style.visibility='hidden'; e.style.maxHeight='35px';

      ui.init = true;

      if(util['isTouchDevice']()) return;

      util['linkStyleSheet'](util['getOption']('resources')+'ui/toolbar/kmwuitoolbar.css');

      ui.regions = {};
      //ui.regions['ca'] = {t: ui.ToolBar_Text['ca'], m: '49,52,65,54,68,57,71,56,73,59,75,60,93,61,94,58,97,58,101,59,107,60,114,64,115,68,114,77,104,74,98,75,96,78,95,82,90,81,85,80,82,76,78,74,74,73,65,68,57,61' },
      //ui.regions['sa'] = {t: ui.ToolBar_Text['sa'], m: '82,82,95,82,96,78,98,75,104,74,114,77,120,79,124,83,126,87,141,90,142,97,138,103,135,113,127,116,123,124,115,131,112,132,109,138,117,139,140,141,141,146,134,148,114,145,109,148,100,148,91,143,91,130,96,111,89,102,83,95,77,89' },
      ui.regions['na'] = {t: ui.ToolBar_Text['na'], m: '0,3,0,37,24,32,35,37,43,47,49,52,65,54,68,57,71,56,73,59,75,60,93,61,93,57,103,49,118,41,126,41,136,23,148,17,156,14,164,5,164,0,57,0,35,5,25,9,5,8,49,52,65,54,68,57,71,56,73,59,75,60,93,61,94,58,97,58,101,59,107,60,114,64,115,68,114,77,104,74,98,75,96,78,95,82,90,81,85,80,82,76,78,74,74,73,65,68,57,61,82,82,95,82,96,78,98,75,104,74,114,77,120,79,124,83,126,87,141,90,142,97,138,103,135,113,127,116,123,124,115,131,112,132,109,138,117,139,140,141,141,146,134,148,114,145,109,148,100,148,91,143,91,130,96,111,89,102,83,95,77,89' },
      ui.regions['eu'] = {t: ui.ToolBar_Text['eu'], m: '145,29,146,19,158,14,171,6,187,2,206,1,217,4,227,11,231,16,231,33,227,34,225,35,225,37,227,39,228,44,228,47,227,48,223,46,218,44,215,43,208,43,203,45,202,48,205,52,201,52,195,49,189,50,187,48,177,48,175,49,166,50,147,33' },
      ui.regions['af'] = {t: ui.ToolBar_Text['af'], m: '150,58,158,50,166,50,175,49,177,48,187,48,189,50,195,49,201,52,205,52,207,53,221,75,229,75,231,77,231,85,227,92,232,101,237,106,237,112,227,115,222,118,206,125,199,127,193,127,185,111,183,104,180,87,168,89,153,85,143,71,147,60' },
      ui.regions['as'] = {t: ui.ToolBar_Text['as'], m: '219,1,221,6,228,12,231,16,231,33,227,34,225,35,225,37,227,39,229,45,232,48,239,48,240,49,239,53,242,60,243,65,249,70,252,81,259,87,271,87,278,95,289,100,303,101,311,98,320,98,323,98,323,84,311,81,308,73,307,65,317,57,330,50,334,44,348,36,364,38,375,34,375,8,355,8,336,5,292,1,285,0,219,0' },
      ui.regions['oc'] = {t: ui.ToolBar_Text['oc'], m: '288,117,289,107,303,101,311,98,323,98,323,84,333,77,344,73,362,80,369,88,375,96,375,141,352,143,323,142,316,136,310,130,291,130' }
      ui.regions['un'] = {t: ui.ToolBar_Text['un'], m: '205,52,202,48,203,45,208,43,215,43,218,44,223,46,227,48,232,48,239,48,240,49,239,53,242,60,243,65,237,76,231,77,229,75,221,75,207,53' },

      ui.toolbarNode = ui.createNode('div', 'kmw_controls');
      ui.toolbarNode.style.display='block';

      var tbNode = ui.createNode('a', 'kmw_controls_start', null, ' ');
      tbNode.href = "https://keyman.com/developer/keymanweb/";
      tbNode.target="_blank";
      ui.toolbarNode.appendChild(tbNode);

      /* Keyboards button */
      ui.keyboardsButtonNode = ui.createNode('div','kmw_btn_keyboards','kmw_button');
      ui.keyboardsButtonNode.title=ui.ToolBar_Text.LanguageSelector;
      var aNode = ui.createNode('a', null, 'kmw_button_a');
      aNode.href='#';
      aNode.onclick = ui.showKeyboardsPopup;
      aNode.appendChild(ui.createNode('div', 'kmw_img_keyboards', 'kmw_img'));
      aNode.appendChild(ui.createNode('div', null, 'kmw_a', ui.ToolBar_Text.Keyboards));
      aNode.appendChild(ui.createNode('div', null, 'kmw_drop'));
      ui.keyboardsButtonNode.appendChild(aNode);

      /* Keyboards popup */
      ui.selectorNode = ui.createNode('div', 'kmw_selector');
      ui.regionsNode = ui.createNode('div', 'kmw_selector_regions');
      var imgNode;
      ui.browseMapNode = ui.createNode('div', 'kmw_browsemap');
      imgNode = ui.createNode('img', 'kmw_region_browsemap');
      imgNode.src= util['getOption']('resources')+'ui/toolbar/blank.gif';
      imgNode.useMap = '#kmw_worldgrey16';
      ui.browseMapNode.appendChild(imgNode);

      var areaNode, mapNode = ui.createNode('map', 'kmw_worldgrey16');
      mapNode.name='kmw_worldgrey16';
      for(var i in ui.regions)
      {
        areaNode = ui.createNode('area');
        areaNode.shape = 'poly';
        areaNode.alt = '';
        areaNode.href = '#';
        areaNode.title = ui.regions[i].t;
        areaNode.hidefocus = 'true';
        areaNode.onclick = (function(i) { return function(event) { return ui.selectRegion(event, i); } })(i);
        areaNode.onmouseover = (function(i) { return function(event) { return ui.hoverRegion(event, i); } })(i);
        areaNode.onmouseout = (function(i) { return function(event) { return ui.unhoverRegion(event, i); } })(i);
        areaNode.coords = ui.regions[i].m;
        mapNode.appendChild(areaNode);
      }

      areaNode = ui.createNode('area');
      areaNode.shape = 'default';
      areaNode.nohref = 'true';
      areaNode.alt = '';
      areaNode.onclick=ui.eventCapture;   // do not close map when clicking on ocean!
      mapNode.appendChild(areaNode);
      ui.browseMapNode.appendChild(mapNode);
      ui.regionsNode.appendChild(ui.browseMapNode);
      ui.regionNodes = {};

      var listNode = ui.createNode('ul');
      for(i in ui.regions)
      {
        var itemNode = ui.createNode('li');
        ui.regionNodes[i] = ui.createNode('a', null, null, ui.regions[i].t);
        ui.regionNodes[i].href='#';
        ui.regionNodes[i].onclick = (function(i) { return function(event) { return ui.selectRegion(event, i); } })(i);
        ui.regionNodes[i].onmouseover = (function(i) { return function(event) { return ui.hoverRegion(event, i); } })(i);
        ui.regionNodes[i].onmouseout = (function(i) { return function(event) { return ui.unhoverRegion(event, i); } })(i);
        itemNode.appendChild(ui.regionNodes[i]);
        listNode.appendChild(itemNode);
      }
      ui.regionsNode.appendChild(listNode);
      ui.selectorNode.appendChild(ui.regionsNode);
      ui.keyboardsButtonNode.appendChild(ui.selectorNode);
      ui.toolbarNode.appendChild(ui.keyboardsButtonNode);

      // Separator and Keyboard Off Button
      ui.toolbarNode.appendChild(ui.offBarNode = ui.createNode('div', 'kmw_bar_off', 'kmw_bar'));

      ui.offButtonNode = ui.createNode('div', 'kmw_btn_off', 'kmw_button_selected');
      aNode = ui.createNode('a', null, 'kmw_button_a');
      aNode.href = '#';
      aNode.onclick = ui.offButtonClickEvent;
      aNode.title = ui.ToolBar_Text.OffTitle;
      aNode.appendChild(ui.createNode('div', 'kmw_img_off', 'kmw_img'));
      aNode.appendChild(ui.createNode('div', null, 'kmw_a', ui.ToolBar_Text.Off));
      ui.offButtonNode.appendChild(aNode);
      ui.toolbarNode.appendChild(ui.offButtonNode);

      // Keyboard buttons
      ui.toolbarNode.appendChild(ui.languageButtonsNode = ui.createNode('div', 'kmw_control_keyboards', 'kmw_button'));

      // Separator and On Screen Keyboard Button
      ui.toolbarNode.appendChild(ui.oskBarNode = ui.createNode('div', 'kmw_bar_osk', 'kmw_bar'));

      ui.oskButtonNode = ui.createNode('div', 'kmw_btn_osk', 'kmw_button');//was 'kmw_button_selected'
      aNode = ui.createNode('a', null, 'kmw_button_a');
      aNode.href = '#';
      aNode.onclick = ui.showOSK;
      aNode.title = ui.ToolBar_Text.ShowOSK;
      aNode.appendChild(ui.createNode('div', 'kmw_img_osk', 'kmw_img'));
      //aNode.appendChild(ui.createNode('div', null, 'kmw_a', 'On Screen Keyboard'));
      ui.oskButtonNode.appendChild(aNode);
      ui.toolbarNode.appendChild(ui.oskButtonNode);

      ui.toolbarNode.appendChild(ui.createNode('div', 'kmw_controls_end', null, ' '));

      var img = ui.createNode('div');
      img.id = 'kmw_map_preload';
      ui.toolbarNode.appendChild(img);

      ui.toolbarNode.appendChild(ui.createNode('br', null, 'kmw_clear'));

      // Append toolbar node to controller
      e.appendChild(ui.toolbarNode);

      // Initialize map array, using a timer to allow restarting if necessary after keyboards loaded
      // Note that toolbar will be displayed and enabled on completion of timeout routine
      ui.updateMap = true;
      if(ui.startTimer) clearTimeout(ui.startTimer);
      ui.startTimer = setTimeout(ui.addKeyboardsToMap,0);

      // Ensure that popups are hidden by clicking elsewhere on document
      util['attachDOMEvent'](document.body,'click',ui.hideAllPopups,false);

      // Set Europe to be the default region
      ui.selectedRegion = 'eu';

      // Restore focus
      keymanweb['focusLastActiveElement']();
    }

    ui.shutdown = function() {
      var root = ui.toolbarNode;
      if(root) {
        root.parentNode.removeChild(root);
      }

      root = ui._insertedElem;
      if(root) {
        root.parentNode.removeChild(root);
      }
    }

    /**
     * Fill the map with available keyboards when the Keyboards selector is clicked
     *    after all keyboard stubs have been registered
    **/
    ui.addKeyboardsToMap = function()
    {
      // Do nothing unless a keyboard has been installed since map created
      if(ui.updateMap) ui.updateMap=false; else return;

      var n = 0;
      ui.regionLanguageListNodes = {};

      // Build list of keyboards by region and language
      var Keyboards = keymanweb['getKeyboards']();

      // Sort the keyboards by region and language
      Keyboards.sort(ui.sortKeyboards);

      // Always rebuild the map, so remove any previously created language lists
      for(n=ui.regionsNode.childNodes.length; n>0; n--)
      {
        if(ui.regionsNode.childNodes[n-1].className == 'kmw_selector_region')
        {
          ui.regionsNode.removeChild(ui.regionsNode.childNodes[n-1]);
        }
      }

      for(var i in ui.regions)
      {

        ui.regionLanguageListNodes[i] = ui.createNode('div', null, 'kmw_selector_region');
        var colNode = ui.createNode('div', null, 'kmw_keyboard_col');
        var max = 0, count = 0, languageCode = '';

        // Get number of languages for the region
        for(var j=0; j<Keyboards.length; j++) {
          // REVERT:  ensures that keyboards without visible map region get displayed SOMEWHERE.
          var kbdRegion = Keyboards[j]['RegionCode'];
          if(!ui.regions[kbdRegion]) {
            // For now, we'll display them within the 'middle-east' region.
            if(i != ui.catchAllRegion) {
              continue;
            }
          } else if(kbdRegion != i) {
            continue; // Not this region
          }

          // Get JUST the language code for this section.  BCP-47 codes can include more!
          var bcpSubtags: string[] = keymanweb['util']['getLanguageCodes'](Keyboards[j]['LanguageCode']);
          if(bcpSubtags[0] == languageCode) continue; // Same language as previous keyboard
          languageCode = bcpSubtags[0];

          max++;
        }
        max = Number(((max+3)/4).toFixed(0));   // Get number of entries per column

        // Add language list to columns for the region
        languageCode='';
        for(var j=0; j<Keyboards.length; j++) {
          // REVERT:  ensures that keyboards without visible map region get displayed SOMEWHERE.
          var kbdRegion = Keyboards[j]['RegionCode'];
          if(!ui.regions[kbdRegion]) {
            // For now, we'll display them within the 'middle-east' region.
            if(i != ui.catchAllRegion) {
              continue;
            }
          } else if(kbdRegion != i) {
            continue; // Not this region
          }

          var bcpSubtags: string[] = keymanweb['util']['getLanguageCodes'](Keyboards[j]['LanguageCode']);
          if(bcpSubtags[0] == languageCode) {  // Same language as previous keyboard, so add it to that entry
            var x = ui.languages[languageCode].keyboards;

            // While we could avoid duplicating keyboard entries that occur for multiple regions, we'll instead
            // allow them to display while distinguishing them more directly.  (That part is handled later.)
            if(x.push) {
              x.push(Keyboards[j]);
            } else {
              ui.languages[languageCode].keyboards = x.concat(Keyboards[j]);
            }

            continue;
          }

          // Add a new language entry
          languageCode = bcpSubtags[0];
          ui.languages[languageCode] = { id: Keyboards[j]['LanguageCode'], name: Keyboards[j]['LanguageName'], keyboards: [Keyboards[j]] };

          // Start a new column if necessary
          if(count % max == 0 && count > 0)
          {
            ui.regionLanguageListNodes[i].appendChild(colNode);
            colNode = ui.createNode('div', null, count/max == 3 ? 'kmw_keyboard_col_right' : 'kmw_keyboard_col');
          }
          count++;

          // Add the language to the column
          var langNode = ui.createNode('div', null, 'kmw_language');
          var aNode = ui.createNode('a', null, null, Keyboards[j]['LanguageName']);
          aNode.href='#';
          aNode.onclick = (function(lang) { return function(event) { return ui.selectLanguage(event, lang); }; })(ui.languages[languageCode]);
          langNode.appendChild(aNode);
          colNode.appendChild(langNode);

          n++;
        }
        // Finish the last column and close the list
        ui.regionLanguageListNodes[i].appendChild(colNode);
        ui.regionLanguageListNodes[i].appendChild(ui.createNode('div', null, 'kmw_clear'));
        ui.regionsNode.appendChild(ui.regionLanguageListNodes[i]);
      }
      ui.loadCookie();

      // Ensure that the correct region has been selected
      ui.selectRegion(null, ui.selectedRegion);
      ui.enableControls();

      // When a keyboard is activated before init is complete,
      // the toolbar needs to refresh the loaded keyboard
      if(ui.lastSelectedKeyboard) {
        ui.changeKeyboardEvent(ui.lastSelectedKeyboard);
      }

      // Restore focus
      keymanweb['focusLastActiveElement']();
    }

    /**
     * Sort keyboards array returned from keymanweb by region and language
     *
     * @param       {Object}  a
     * @param       {Object}  b
     * @return      {number}
     **/
    ui.sortKeyboards = function(a,b)
    {
      if(a['RegionCode'] < b['RegionCode']) return -2;
      if(a['RegionCode'] > b['RegionCode']) return 2;
      if(a['LanguageName'] < b['LanguageName']) return -1;
      if(a['LanguageName'] > b['LanguageName']) return 1;
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
    ui.findListedKeyboard = function(lang)
    {
      for(var i = 0; i < ui.listedKeyboards.length; i++)
        if(ui.listedKeyboards[i].lang.id == lang.id) return i;
      return null;
    }

    /**
     * Add a keyboard to the list of keyboards available for a language
     *
     * @param       {Object}  lang
     * @param       {Object}  kbd
     **/
    ui.addKeyboardToList = function(lang,kbd)
    {
      var found = ui.findListedKeyboard(lang);
      if(found == null)
      {
        // Add the button
        if(ui.listedKeyboards.length >= ui.maxListedKeyboards)
        {
          var oldestPriority = 0x7fffffff, oldestFound = null;

          for(var i = 0; i < ui.listedKeyboards.length; i++)
            if(ui.listedKeyboards[i].priority < oldestPriority)
            {
              oldestFound = i;
              oldestPriority = ui.listedKeyboards[i].priority;
            }

          // delete the oldest used control
          if(oldestFound != null)
          {
            var rk = ui.listedKeyboards[oldestFound];
            ui.langKeyboardListNodes[rk.lang.id] = null;
            ui.langKeyboardNodes[rk.lang.id] = null;
            ui.languageButtonsNode.removeChild(rk.buttonNode);
            if(oldestFound == 0)
              ui.listedKeyboards = ui.listedKeyboards.slice(oldestFound + 1);
            else if(oldestFound == ui.listedKeyboards.length - 1)
              ui.listedKeyboards = ui.listedKeyboards.slice(0, oldestFound);
            else
              ui.listedKeyboards = ui.listedKeyboards.slice(0, oldestFound).concat(ui.listedKeyboards.slice(oldestFound+1));
          }
        }
        var buttonNode = ui.createNode('div', null/*'kmw_button_keyboard_'+lang.id*/, 'kmw_button');
        var aNode = ui.createNode('a', null, 'kmw_button_a'+(lang.keyboards.length>1 ? ' kmw_norightgap' : ''));
        aNode.href='#';

        var p1=ui.ToolBar_Text['SelectKeyboardPre']+kbd['Name'],p2=ui.ToolBar_Text['SelectKeyboardSuf'];
        if(p1.toLowerCase().indexOf(p2.toLowerCase()) < 0) p1=p1+' '+ p2;
        aNode.title = p1;
        aNode.onclick = function(event) { return ui.selectLanguage(event, lang) };
        aNode.appendChild(ui.createNode('div', 'kmw_img_kbd', 'kmw_img'));

        ui.lgText=ui.truncate(lang.name,28);
        aNode.appendChild(ui.createNode('div', null, 'kmw_a', ui.lgText));
        buttonNode.appendChild(aNode);

        var thisANode = aNode;

        if(lang.keyboards.length > 1) {
          aNode = ui.createNode('a', null, 'kmw_button_a kmw_noleftgap');
          aNode.href = '#';
          aNode.title = ui.ToolBar_Text['AltKeyboardsPre']+lang.name+ui.ToolBar_Text['AltKeyboardsSuf'];
          aNode.onclick = function(event) { return ui.showKeyboardsForLanguage(event, lang) };
          var divNode = ui.createNode('div', null, 'kmw_a');
          var kbdText=ui.truncate(kbd['Name'].replace(/\s?keyboard/i,''),40-ui.lgText.length);
          divNode.appendChild(ui.langKeyboardNodes[lang.id] = ui.createNode('span', null, 'kmw_kbd', kbdText));
          aNode.appendChild(divNode);
          aNode.appendChild(ui.createNode('div', null, 'kmw_drop'));
          buttonNode.appendChild(aNode);

          ui.langKeyboardListNodes[lang.id] = ui.createNode('ul', null, 'kmw_selector_kbd');
          ui.langKeyboardListNodes[lang.id].style.display='none';

          for(var n in lang.keyboards) {
            var itemNode = ui.createNode('li');
            kbdText = lang.keyboards[n]['Name'].replace(/\s?keyboard/i,'');
            // We append the full BCP-47 code here for disambiguation when regional and/or script variants exist.
            kbdText = kbdText + " [" + lang.keyboards[n].LanguageCode + "]";
            aNode = ui.createNode('a', null, null, kbdText);
            aNode.href = '#';
            aNode.title = '';
            aNode.onclick = (function(lang,kbd) { return function(event) { return ui.selectKeyboard(event, lang, kbd, true); } })(lang, lang.keyboards[n]);

            itemNode.appendChild(aNode);
            ui.langKeyboardListNodes[lang.id].appendChild(itemNode);
          }
          buttonNode.appendChild(ui.langKeyboardListNodes[lang.id]);
        }

        ui.languageButtonsNode.appendChild(buttonNode);

        var thisLang = lang, thisButtonNode = buttonNode;
        ui.listedKeyboards.push({priority: ui.keyboardListPriority++, lang:thisLang, keyboard:kbd, buttonNode:thisButtonNode, aNode:thisANode});
      }
      else
      {
        ui.listedKeyboards[found].priority = ui.keyboardListPriority++;
        ui.listedKeyboards[found].keyboard = kbd;
        var e = ui.langKeyboardNodes[lang.id];
        if(e)
        {
          var kbdText=kbd['Name'].replace(/\s?keyboard/i,'');
          e.innerHTML = ui.truncate(kbdText,40-ui.lgText.length);
        }
        if(ui.listedKeyboards[found].aNode)
        {
          var p1=ui.ToolBar_Text['SelectKeyboardPre']+kbd['Name'],p2=ui.ToolBar_Text['SelectKeyboardSuf'];
          if(p1.toLowerCase().indexOf(p2.toLowerCase()) < 0) p1=p1+' '+ p2;
          ui.listedKeyboards[found].aNode.title = p1;
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
    ui.truncate = function(PName,PLen)
    {
      if(PName.length <=PLen) return PName;
      return PName.substr(0,PLen-1)+'\u2026';
    }

    /**
     * Rebuild the entire keyboard list whenever a keyboard is installed
     * (without necessarily loading the keyboard)
     *
     * @param       {Object}  p Keyboard metadata object
     **/
    ui.registerKeyboard = function(p)
    {
      ui.updateMap = true;
      if(ui.startTimer) clearTimeout(ui.startTimer);
      ui.startTimer = setTimeout(ui.addKeyboardsToMap,0);
    }
    keymanweb['addEventListener']('keyboardregistered',ui.registerKeyboard);

    var keyboardsForLangPopup = null;

    /**
     * Function     hideKeyboardsForLanguage
     * Scope        Private
     * @param       {Object}  event
     * @return      {boolean}
     * Description  Hide the list of keyboards for this language
     **/
    ui.hideKeyboardsForLanguage = function(event)
    {
      var e = ui.keyboardsForLangPopup;
      if(e) e.style.display='none';
      ui.CancelPopupDismissal(ui.hideKeyboardsForLanguage);
      return ui.eventCapture(event);
    }

    /**
     * Display the list of keyboards for this language
     *
     * @param       {Object}  event
     * @param       {Object}  lang
     * @return      {boolean}
     *
     **/
    ui.showKeyboardsForLanguage = function(event, lang)
    {
      ui.hideKeyboardsPopup(event);
      var e = ui.langKeyboardListNodes[lang.id];
      if(e)
      {
        if(e.style.display=='block') return ui.hideKeyboardsForLanguage(event);
        e.style.display='block';
        ui.keyboardsForLangPopup = e;
        ui.SetupPopupDismissal(e, ui.hideKeyboardsForLanguage);
      }
      return ui.eventCapture(event);
    }

    /**
     * Select the language, and either select the keyboard (if unique) or display the list of keyboards
     * available for this language
     *
     * @param       {Object}  event
     * @param       {Object}  lang
     * @return      {boolean}
     **/
    ui.selectLanguage = function(event, lang)
    {
      var found=ui.findListedKeyboard(lang), kbd = null;

      if(found == null)
        kbd = lang.keyboards[0];
      else
        kbd = ui.listedKeyboards[found].keyboard;
      if(!kbd) return false;

      return ui.selectKeyboard(event,lang,kbd,true);
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
    ui.selectKeyboard = function(event,lang,kbd,updateKeyman:boolean) {
      if(ui.selectedLanguage) {
        var found = ui.findListedKeyboard(ui.selectedLanguage);
        if(found != null) {
          ui.listedKeyboards[found].buttonNode.className = 'kmw_button';
        }
      }

      ui.offButtonNode.className = 'kmw_button';
      ui.selectedLanguage = lang;
      ui.selectedKeyboard = kbd;

      // In 12.0, this UI class has only been partially converted to BCP-47.
      // `lang.id` refers to the base language identifier and will NOT include
      // any subtags.  We want the FULL language identifier here, with subtags.
      ui.SelectedLanguage = kbd.LanguageCode;

      // Return focus to input area and activate the selected keyboard
      ui.setLastFocus(); //*****this seems out of sequence???
      ui.addKeyboardToList(lang, kbd);
      if(updateKeyman) {
        keymanweb['setActiveKeyboard'](kbd['InternalName'], kbd['LanguageCode']);
      }
      ui.listedKeyboards[ui.findListedKeyboard(lang)].buttonNode.className = 'kmw_button_selected';

      // Always save current state when selecting a keyboard
      ui.saveCookie();
      ui.enableControls();
      return ui.hideKeyboardsPopup(event) || ui.hideKeyboardsForLanguage(event);
    }

    /**
     * Enable all UI controls
     *
     * @return      {boolean}
     **/
    ui.enableControls = function()
    {
      var elems=[ui.offButtonNode, ui.offBarNode, ui.oskButtonNode, ui.oskBarNode],hideOskButton=false;

      if(keymanweb['isCJK'](ui.selectedKeyboard)) hideOskButton = true;
      else if(ui.selectedKeyboard == null) hideOskButton = (elems[2].style.display == 'none')

      if(ui.selectedKeyboard != null || ui.listedKeyboards.length > 0)
      {
        for(var i = 0; i < elems.length; i++)
          elems[i].style.display='';
      }
      else
      {
        for(var i = 0; i < elems.length; i++)
          elems[i].style.display='none';
      }

      if(hideOskButton)
        ui.oskButtonNode.style.display = ui.oskBarNode.style.display = 'none';

      else if(ui.selectedKeyboard == null)
        ui.oskButtonNode.className='kmw_button_disabled';
      //else
      //  ui.oskButtonNode.className=(osk && osk['isEnabled']() ? 'kmw_button_selected' : 'kmw_button');

      // Display the toolbar if still hidden
      ui.toolbarNode.parentNode.style.visibility='visible';
      return true;
    }

    /**
     * Restore the focus to the last focused element
     **/
    ui.setLastFocus = function()
    {
      keymanweb['focusLastActiveElement']();
    }

    /**
     * Display or hide the OSK according to user control. This will always force
     * the focus to the last active element if currently unfocused, which is
     * preferable to not having the OSK appear when the OSK button is clicked.
     *
     * @param       {Object}  event
     * @return      {boolean}
     **/
    ui.showOSK = function(event)
    {
      //Toggle OSK on or off
      if(osk && keymanweb['getActiveKeyboard']() != '')
      {
        if(osk['isEnabled']()) osk['hide'](); else osk['show'](true);
      }
      ui.setLastFocus();
      return ui.eventCapture(event);
    }

    /**
     * Function     offButtonClickEvent
     * Scope        Private
     * @param       {Object}  event
     * @return      {boolean}
     * Description  Update the UI when all keyboards disabled by user
     **/
    ui.offButtonClickEvent = function(event)
    {
      if(ui.toolbarNode.className != 'kmw_controls_disabled')
      {
        ui.hideKeyboardsForLanguage(null);
        if(ui.selectedLanguage)
        {
          var found = ui.findListedKeyboard(ui.selectedLanguage);
          if(found != null) ui.listedKeyboards[found].buttonNode.className = 'kmw_button';
        }
        ui.selectedKeyboard = null;
        ui.selectedLanguage = null;
        ui.SelectedLanguage = '';
        ui.offButtonNode.className = 'kmw_button_selected';
      }
      // Return the focus to the input area and set the active keyboard to nothing
      ui.setLastFocus();
      keymanweb['setActiveKeyboard']('','');

      //Save current state when deselecting a keyboard (may not be needed)
      ui.saveCookie();
      ui.enableControls();
      return ui.eventCapture(event);
    }

    /**
     * Function     eventCapture
     * Scope        Private
     * @param       {Object}  event
     * @return      {boolean}
     * Description  Browser-independent event capture
     **/
    ui.eventCapture = function(event)
    {
      if(!event) event = window.event;
      if(window.event) window.event.returnValue = false;
      if(event) event.cancelBubble = true;

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
    ui.selectRegion = function(event, region)
    {
      var e = ui.browseMapNode;
      if(!e) return ui.eventCapture(event);
      e.className = 'kmw_browsemap_'+region;
      if(typeof(ui.regionLanguageListNodes[region]) == 'undefined')
      {
        ui.updateMap = true; ui.addKeyboardsToMap();
      }
      ui.regionLanguageListNodes[region].style.display='block';
      ui.regionNodes[region].className='selected';
      if(ui.selectedRegion != null && ui.selectedRegion != region)
      {
        ui.regionLanguageListNodes[ui.selectedRegion].style.display='none';
        ui.regionNodes[ui.selectedRegion].className='';
      }
      ui.selectedRegion = region;
      //ui.saveCookie();
      return ui.eventCapture(event);
    }

    /**
     * Function     unhoverRegion
     * Scope        Private
     * @param       {Object}  event
     * @param       {string}  region
     * @return      {boolean}
     * Description  Remove highlighting from a region
     **/
    ui.unhoverRegion = function(event, region)
    {
      ui.browseMapNode.className = (ui.selectedRegion == null ? '' : 'kmw_browsemap_'+ui.selectedRegion);
      ui.regionNodes[region].className=(ui.selectedRegion==region?'selected':'');
      return ui.eventCapture(event);
    }

    /**
     * Function     hoverRegion
     * Scope        Private
     * @param       {Object}  event
     * @param       {string}  region
     * @return      {boolean}
     * Description  Highlight a hovered region
     **/
    ui.hoverRegion = function(event, region)
    {
      ui.browseMapNode.className = 'kmw_browsemap_'+region+'_sel';
      ui.regionNodes[region].className='hover';
      return ui.eventCapture(event);
    }

    /**
     * Function     pluck
     * Scope        Private
     * @param       {Object}  elem
     * @param       {string}  property
     * @return      {*}
     * Description  Get the value of an element property
     **/
    ui.pluck = function(elem, property) {
        return elem.getAttribute ? elem.getAttribute(property) || elem[property] : elem[property];
      };

    /**
     * Function     focusControlEvent
     * Scope        Private
     * @param       {Object}  params    Object containing Target element (or frame)
     * @return      {boolean}
     * Description  UI code to be executed on receiving focus
     */
    ui.focusControlEvent = function(params)
    {
      if(!ui.init) return true;

      var t=params.target;
      if(t.tagName.toLowerCase() == 'textarea' || (t.tagName.toLowerCase() == 'input' && t.type.toLowerCase() == 'text'))
      {
        ui.lastActiveControl = t;
        if(ui.pluck(t, 'kmw_disable'))
        {
          if(ui.toolbarNode.className != 'kmw_controls_disabled')
            ui.toolbarNode.className = 'kmw_controls_disabled';
        }
        else
        {
          if(ui.selectedKeyboard != null)
          {
            if(keymanweb['isCJK']())
            {
              ui.oskButtonNode.style.display = ui.oskBarNode.style.display = 'none';
            }
            else
            {
              ui.oskButtonNode.className = (osk && osk['isEnabled']()) ? 'kmw_button_selected' : 'kmw_button';
            }
          }

          if(ui.toolbarNode.className != '') ui.toolbarNode.className = '';

          var offsetX, offsetY;
          if(t.KMW_HelpOffsetX) offsetX = t.KMW_HelpOffsetX; else offsetX = 64;
          if(t.KMW_HelpOffsetY) offsetY = t.KMW_HelpOffsetY; else offsetY = 0;
          ui.helpOffsetX = util['getAbsoluteX'](t)+offsetX;
          ui.helpOffsetY = util['getAbsoluteY'](t)+t.offsetHeight+offsetY;
        }
      }
      return true;
    }
    keymanweb['addEventListener']('controlfocused',ui.focusControlEvent);

    /**
     * Function     oncontrolblurred
     * Scope        Private
     * Parameters   {Object}  params  Object containing event
     * @return      {boolean}
     * Description  UI code to be executed on losing focus
     */
    ui.blurControlEvent = function(params)
    {
      if(!ui.init) return true;

      // Must disable OSK button when not focused
      if(ui.oskButtonNode.style.display != 'none') ui.oskButtonNode.className='kmw_button_disabled';

      if(!params.event) return true;   // I2404 - Manage IE events in IFRAMEs
      return true;
    }
    keymanweb['addEventListener']('controlblurred',ui.blurControlEvent);


    /**
     * UI action required when keyboard changed indirectly
     *
     * @param       {Object}  p   keyboard selection object
     * @return      {boolean}
     **/

    ui.lastSelectedKeyboard = null;

    ui.changeKeyboardEvent = function(p)
    {
      ui.lastSelectedKeyboard = null;
      var kbName=p['internalName'],
          lgName=keymanweb['util']['getLanguageCodes'](p['languageCode'])[0];
      if(lgName != '' && kbName != '')
      {
        var lg = ui.languages[lgName];
        if(lg != null)
        {
          for(var j=0; j<lg.keyboards.length; j++)
          {
            if(lg.keyboards[j]['InternalName'] == kbName)
            {
              ui.selectKeyboard(null, lg, lg.keyboards[j], false);
              return true;
            }
          }
        }
        ui.lastSelectedKeyboard = {...p};
      }
      return true;
    }
    keymanweb['addEventListener']('keyboardchange',ui.changeKeyboardEvent);


    /**
     * UI action when OSK displayed: restore to or update the saved OSK position
     *
     * @param       {Object}  oskPosition
     * @return      {Object}
     **/
    ui.onShowOSK = function(oskPosition)
    {
      if(ui.init) ui.oskButtonNode.className = 'kmw_button_selected';
      //The return valu is not currently useful, but allows for the possibility of the OSK restricting or limiting
      //a UI-set position, and returning the corrected position to the UI
      return oskPosition;
    }
    osk['addEventListener']('show',ui.onShowOSK);

    /**
     * Update appearance of OSK button whenever the OSK is hidden by the user
     *
     * @param       {Object}  p
     **/
    ui.onHideOSK = function(p)
    {
      if(ui.init && p['HiddenByUser']) ui.oskButtonNode.className = 'kmw_button';
    }
    osk['addEventListener']('hide',ui.onHideOSK);

    /**
     * Function     showKeyboardsPopup
     * Scope        Private
     * @param       {Object}  event
     * @return      {boolean}
     * Description  Update the map when displayed
     **/
    ui.showKeyboardsPopup = function(event)
    {
      // Add any newly available keyboards to the map
      ui.addKeyboardsToMap();

      if(ui.toolbarNode.className == 'kmw_controls_disabled') return ui.eventCapture(event);
      ui.hideKeyboardsForLanguage(null);
      if(ui.selectorNode.className=='kmw_over') return ui.hideKeyboardsPopup(event);
      ui.selectorNode.className='kmw_over';
      ui.keyboardsButtonNode.className='kmw_button_selected';

      ui.SetupPopupDismissal(ui.selectorNode, ui.hideKeyboardsPopup);
      return ui.eventCapture(event);
    }

    /**
     * Function     hideKeyboardsPopup
     * Scope        Private
     * @param       {Object}  event
     * @return      {boolean}
     * Description  Hide the list of keyboards for this language
     **/
    ui.hideKeyboardsPopup = function(event)
    {
      ui.selectorNode.className='';
      ui.keyboardsButtonNode.className='kmw_button';
      ui.CancelPopupDismissal(ui.hideKeyboardsPopup);
      return ui.eventCapture(event);
    }

    /**
     * Hide both language and keyboard selection popups on body click
     * @param   {Object}  event
     * @return  {boolean}
    **/
    ui.hideAllPopups = function(event)
    {
      var e = ui.keyboardsForLangPopup;
      if((!e || (e.style.display =='none')) && (ui.selectorNode.className == '')) return true;
      ui.hideKeyboardsPopup(event);
      ui.hideKeyboardsForLanguage(event);
      return ui.eventCapture(event);
    }

    var dismissalCallback = null, popupElement = null, lastDismissalCallback = null;

    /**
     * Function     PopupDismissal
     * Scope        Private
     * @param       {Object}  event
     * @return      {Object}
     * Description  Carry out action when popup dismissed
     **/
    ui.PopupDismissal = function(event)
    {
      var t = (event && event.target) || (window.event && window.event.srcElement);
      if(t)
      {
        while(t.parentNode)
        {
          if (t == popupElement) return null;
          t = t.parentNode;
        }
      }
      if(t.nodeName == '#document') ui.hideAllPopups(event); //KMEW-41, fixed for build 356.
      return dismissalCallback;
    }

    /**
     * Function     SetupPopupDismissal
     * Scope        Private
     * @param       {Object}            element
     * @param       {function(Object)}  callback
     * Description  Prepare for callback dismissal
     **/
    ui.SetupPopupDismissal = function(element, callback)
    {
      if(ui.PopupDismissal == document.onclick)
        ui.CancelPopupDismissal(dismissalCallback);
      dismissalCallback = callback;
      popupElement = element;
      lastDismissalCallback = document.onclick;
      document.onclick = ui.PopupDismissal;
    }

    /**
     * Function     CancelPopupDismissal
     * Scope        Private
     * @param       {?function(Object)}  callback
     * Description  Cancel callback dismissal
     **/
    ui.CancelPopupDismissal = function(callback)
    {
      if(ui.PopupDismissal == document.onclick)
      {
        document.onclick = lastDismissalCallback;
        lastDismissalCallback = null;
        dismissalCallback = null;
        popupElement = null;
      }
    }

    /**
     * Load the previous state from the KeymanWeb_Keyboard and KeymanWeb_Toolbar cookies
     **/
    ui.loadCookie = function()
    {
      var currentKeyboard='',currentLgCode='',c=util['loadCookie']("KeymanWeb_Keyboard");
      if(c['current'] != undefined) currentKeyboard = c['current'].split(':')[0];
      c=util['loadCookie']("KeymanWeb_Toolbar");

      if(c['region'] != undefined) ui.selectedRegion = c['region'];
      if(c['maxrecent'] != undefined)
      {
        for(var i=0; i<c['maxrecent']; i++)
        {
          if(c['recent'+i] != undefined)
          {
            var r=c['recent'+i].split(',');
            if(r.length == 2)
            {
              var k = ui.languages[r[0]];
              if(k != null)
              {
                for(var j=0; j<k.keyboards.length; j++)
                {
                  if(k.keyboards[j]['InternalName'] == r[1])
                  {
                    ui.addKeyboardToList(k, k.keyboards[j]);
                    if(k.keyboards[j]['InternalName'] == currentKeyboard)
                    {
                      ui.selectKeyboard(null, k, k.keyboards[j], true);
                      window.focus(); ui.setLastFocus();
                      break;
                    }
                    break;
                  }
                }
              }
            }
          }
        }
      }
      // If language list and keyboard list have not yet been saved as a cookie,
      // initialize to the current (default) language and keyboard, if set by KMW
      else
      {
        var kbName=keymanweb['getActiveKeyboard'](),
            lgName=keymanweb['getActiveLanguage']();
        if(lgName != '' && kbName != '')
        {
          var lg = ui.languages[lgName];
          if(lg != null)
          {
            for(var j=0; j<lg.keyboards.length; j++)
            {
              if(lg.keyboards[j]['InternalName'] == kbName)
              {
                ui.selectKeyboard(null, lg, lg.keyboards[j], true);
                window.focus(); ui.setLastFocus();
              }
            }
          }
        }
      }
    }

    /**
     * Save the current UI state in the KeymanWeb_Toolbar cookie
     **/
    ui.saveCookie = function()
    {
      var vs={};
      vs['region'] = ui.selectedRegion;
      vs['maxrecent'] = ''+ui.listedKeyboards.length;
      for(var i=0; i<ui.listedKeyboards.length; i++)
        vs['recent'+i] = ui.listedKeyboards[i].lang.id+","+ui.listedKeyboards[i].keyboard['InternalName'];
      util['saveCookie']('KeymanWeb_Toolbar',vs);
    }

    // Initialize when everything defined (replaces unreliable onload event handler)
    // ui.initToolbarUI();
    // No, initialization must not be done before KMW is ready!
    // ui.initialize() is now called by keymanweb when document is ready and kmw initialization completed

  } catch(ex){}
}