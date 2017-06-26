// JavaScript Document samplehdr.js: Keyboard management for KeymanWeb demonstration pages

/* 
    This script is designed to test KeymanWeb error message handling.
*/

  function loadKeyboards() 
  { 
    var kmw=tavultesoft.keymanweb;
    
    // We start by adding a keyboard correctly.  It's best to include a 'control' in our experiment.
    kmw.addKeyboards({id:'us',name:'English',language:{id:'eng',name:'English'},
      filename:'./us-1.0.js'});
      
    // Insert a keyboard that cannot be found.
    kmw.addKeyboards({id:'lao_2008_basic',name:'wrong-filename',
      language:{
        id:'lao',name:'debugging',region:'Asia',
        font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
        },
      filename:'./missing_file.js' // Intentional error - the file doesn't exist, so the <script> tag will raise an error event.
      });   
	  
	// Insert a keyboard that will generate a timing error.  
    kmw.addKeyboards({id:'unparsable',name:'non-parsable',
      language:{
        id:'lao',name:'debugging',region:'Asia',
        font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
        },
      filename:'./unparsable.js' // Intentional error - the file doesn't exist, so the <script> tag will raise an error event.
      }); 	  
	  
	// Insert a keyboard that will generate a timing error.  
    kmw.addKeyboards({id:'timeout',name:'timeout',
      language:{
        id:'lao',name:'debugging',region:'Asia',
        font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
        },
      filename:'./timeout.js.php' // Intentional error - the file doesn't exist, so the <script> tag will raise an error event.
      }); 

    // The following two optional calls should be delayed until language menus are fully loaded:
    //  (a) a specific mapped input element input is focused, to ensure that the OSK appears
    //  (b) a specific keyboard is loaded, rather than the keyboard last used.         
  //window.setTimeout(function(){kmw.setActiveElement('ta1',true);},2500);
  //window.setTimeout(function(){kmw.setActiveKeyboard('Keyboard_french','fra');},3000);
  
    // Note that locally specified keyboards will be listed before keyboards 
    // requested from the remote server by user interfaces that do not order
    // keyboards alphabetically by language.
  }
