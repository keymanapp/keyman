// Includes version-related functionality
///<reference path="utils/styleConstants.ts" />

// The Device object definition -------------------------------------------------

namespace com.keyman {
  export class Device {
    touchable: boolean;
    OS: string;
    formFactor: string;
    dyPortrait: number;
    dyLandscape: number;
    version: string;
    orientation: string|number;
    browser: string;
    colorScheme: 'light' | 'dark';

    private detected: boolean = false;
    private _styles: utils.StyleConstants;

    // Generates a default Device value.
    constructor() {
      this.touchable = !!('ontouchstart' in window);
      this.OS = '';
      this.formFactor='desktop';
      this.dyPortrait=0;
      this.dyLandscape=0;
      this.version='0';
      this.orientation=window.orientation;
      this.browser='';
    }

    /**
     * Get device horizontal DPI for touch devices, to set actual size of active regions
     * Note that the actual physical DPI may be somewhat different.
     * 
     * @return      {number}               
     */       
    getDPI(): number {
      var t=document.createElement('DIV') ,s=t.style,dpi=96;
      if(document.readyState !== 'complete') {
        return dpi;
      }
      
      t.id='calculateDPI';
      s.position='absolute'; s.display='block';s.visibility='hidden';
      s.left='10px'; s.top='10px'; s.width='1in'; s.height='10px';
      document.body.appendChild(t);
      dpi=(typeof window.devicePixelRatio == 'undefined') ? t.offsetWidth : t.offsetWidth * window.devicePixelRatio;
      document.body.removeChild(t);
      return dpi;    
    }

    detect() : void {
      var IEVersion = Device._GetIEVersion();
      var possMacSpoof = false;
      
      if(navigator && navigator.userAgent) {
        var agent=navigator.userAgent;

        if(agent.indexOf('iPad') >= 0) {
          this.OS='iOS';
          this.formFactor='tablet';
          this.dyPortrait=this.dyLandscape=0;
        } else if(agent.indexOf('iPhone') >= 0) { 
          this.OS='iOS';
          this.formFactor='phone';
          this.dyPortrait=this.dyLandscape=25;
        } else if(agent.indexOf('Android') >= 0) {
          this.OS='Android';
          this.formFactor='phone';    // form factor may be redefined on initialization
          this.dyPortrait=75;
          this.dyLandscape=25;
          try {
            var rx=new RegExp("(?:Android\\s+)(\\d+\\.\\d+\\.\\d+)");
            this.version=agent.match(rx)[1];
          } catch(ex) {}
        } else if(agent.indexOf('Linux') >= 0) {
          this.OS='Linux';
        } else if(agent.indexOf('Macintosh') >= 0) {
          // Starting with 13.1, "Macintosh" can reflect iPads (by default) or iPhones 
          // (by user setting); a new "Request Desktop Website" setting for Safari will
          // change the user agent string to match a desktop Mac.
          //
          // Firefox uses '.' between version components, while Chrome and Safari use
          // '_' instead.  So, we have to check for both.  Yay.
          let regex = /Intel Mac OS X (\d+(?:[_\.]\d+)+)/i;
          let results = regex.exec(agent);
          
          // Match result:  a version string with components separated by underscores.
          if(!results) {
            console.warn("KMW could not properly parse the user agent string."
              + "A suboptimal keyboard layout may result.");
            this.OS='MacOSX';
          } else if(results.length > 1 && results[1]) {
            // Convert version string into a usable form.
            let versionString = results[1].replace('_', '.');
            let version = new utils.Version(versionString);

            possMacSpoof = utils.Version.MAC_POSSIBLE_IPAD_ALIAS.compareTo(version) <= 0;
            this.OS='MacOSX';
          }
        } else if(agent.indexOf('Windows NT') >= 0) {
          this.OS='Windows';
          if(agent.indexOf('Touch') >= 0) {
            this.formFactor='phone';   // will be redefined as tablet if resolution high enough
          }
          
          // Windows Phone and Tablet PC
          if(typeof navigator.msMaxTouchPoints == 'number' && navigator.msMaxTouchPoints > 0) {
            this.touchable=true;
          }
        }
      }

      // var sxx=device.formFactor;
      // Check and possibly revise form factor according to actual screen size (adjusted for Galaxy S, maybe OK generally?)
      if(this.formFactor == 'tablet' && Math.min(screen.width,screen.height) < 400) {
        this.formFactor='phone';
      }

      // Trust what iOS tells us for phone vs tablet.
      if(this.formFactor == 'phone'  && Math.max(screen.width,screen.height) > 720 && this.OS != 'iOS') {
        this.formFactor='tablet';
      }

      // Test for potential Chrome emulation on Windows or macOS X (used only in next if-check)
      let possibleChromeEmulation = navigator.platform == 'Win32' || navigator.platform == 'MacIntel'

      //                           alert(sxx+'->'+device.formFactor);
      // Check for phony iOS devices (but don't undo for Chrome emulation used during development)
      if(this.OS == 'iOS' && !('ongesturestart' in window) && !possibleChromeEmulation) {
        this.OS='Android';
      }
    
      // Determine application or browser
      this.browser='web';
      if(IEVersion < 999) {
        this.browser='ie';
      } else {
        if(this.OS == 'iOS' || this.OS.toLowerCase() == 'macosx') {
          this.browser='safari';
        }

        var bMatch=/Firefox|Chrome|OPR|Safari|Edge/;
        if(bMatch.test(navigator.userAgent)) {
          if((navigator.userAgent.indexOf('Firefox') >= 0) && ('onmozorientationchange' in screen)) {
            this.browser='firefox';
          } else if(navigator.userAgent.indexOf('OPR') >= 0) {
            this.browser='opera';
          } else if(navigator.userAgent.indexOf(' Edge/') >= 0) {
            // Edge is too common a word, so test for Edge/ :)
            // Must come before Chrome and Safari test because
            // Edge pretends to be both
            this.browser='edge';
          } else if(navigator.userAgent.indexOf('Chrome') >= 0) {
            // This test must come before Safari test because on macOS,
            // Chrome also reports "Safari"
            this.browser='chrome';
          } else if(navigator.userAgent.indexOf('Safari') >= 0) {
            this.browser='safari';
          }
        } 
      }

      if(possMacSpoof && this.browser == 'safari') {
        // Indistinguishable user agent string!  We need a different test; fortunately, true macOS
        // Safari doesn't support TouchEvents.  (Chrome does, though!  Hence the filter above.)
        if(window['TouchEvent']) {
          this.OS='iOS';
          this.formFactor='tablet';
          this.dyPortrait=this.dyLandscape=0;

          // It's currently impossible to differentiate between iPhone and iPad here
          // except for by screen dimensions.
          let aspectRatio = screen.height / screen.width;
          if(aspectRatio < 1) {
            aspectRatio = 1 / aspectRatio;
          }

          // iPhones usually have a ratio of 16:9 (or 1.778) or higher, while iPads use 4:3 (or 1.333)
          if(aspectRatio > 1.6) {
            // Override - we'll treat this device as an iPhone.
            this.formFactor = 'phone';
            this.dyPortrait=this.dyLandscape=25;
          }
        }
      }

      this.colorScheme = this.prefersDarkMode() ? 'dark' : 'light';
      this.detected = true;
    }

    static _GetIEVersion() {
      var n, agent='';
      
      if('userAgent' in navigator) {
        agent=navigator.userAgent;
      }
      
      // Test first for old versions
      if('selection' in document) {         // only defined for IE and not for IE 11!!!       
        var appVer=navigator.appVersion;
        n=appVer.indexOf('MSIE ');
        if(n >= 0) {
          // Check for quirks mode page, always return 6 if so
          if((document as Document).compatMode == 'BackCompat') {
            return 6;
          }
          
          appVer=appVer.substr(n+5);
          n=appVer.indexOf('.');
          if(n > 0) {
            return parseInt(appVer.substr(0,n),10);
          }
        }              
      }
        
      // Finally test for IE 11 (and later?)
      n=agent.indexOf('Trident/');
      if(n < 0) {
        return 999;
      }
      
      agent=agent.substr(n+8);
      n=agent.indexOf('.');
      if(n > 0){
        return parseInt(agent.substr(0,n),10)+4;
      }
    
      return 999;
    }

    /**
     * Checks is a user's browser is in dark mode, if the feature is supported.  Returns false otherwise.
     * 
     * Thanks to https://stackoverflow.com/a/57795518 for this code.
     */
    private prefersDarkMode(): boolean {
      // Ensure the detector exists (otherwise, returns false)
      return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    }

    public get styles(): utils.StyleConstants {
      if(!this._styles) {
        if(!this.detected) {
          this.detect();
        }
        
        this._styles = new utils.StyleConstants(this);
      }

      return this._styles;
    }

    /**
     * Returns a slimmer, web-core compatible version of this object.
     */
    public get coreSpec(): utils.DeviceSpec {
      return new utils.DeviceSpec(this.browser, this.formFactor, this.OS, this.touchable);
    }
  }
}