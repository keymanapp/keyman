// JavaScript Document samplehdr.js: Keyboard management for KeymanWeb demonstration pages

/* 
    This script is designed to test KeymanWeb error message handling.
*/

function loadKeyboards() { 
  var kmw=keyman;
  
  // We start by adding a keyboard correctly.  It's best to include a 'control' in our experiment.
  kmw.addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
    filename:'../us-1.0.js'});
    
  // Insert a keyboard that cannot be found.
  kmw.addKeyboards({id:'lao_2008_basic',name:'wrong-filename',
    languages:{
      id:'lo',name:'debugging',region:'Asia',
      font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
      },
    filename:'./missing_file.js' // Intentional error - the file doesn't exist, so the <script> tag will raise an error event.
    });   
  
// Insert a keyboard that will generate a timing error.  
  kmw.addKeyboards({id:'unparsable',name:'non-parsable',
    languages:{
      id:'lo',name:'debugging',region:'Asia',
      font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
      },
    filename:'./unparsable.js' // Intentional error - the file has no parsable keyboard, so while the <script> tag will load,
      // registration will fail.
    });

// Insert a keyboard that will generate a timing error.  
  kmw.addKeyboards({id:'timeout',name:'timeout',
    languages:{
      id:'lo',name:'debugging',region:'Asia',
      font:{family:'LaoWeb',source:['../font/saysettha_web.ttf','../font/saysettha_web.woff','../font/saysettha_web.eot']}
      },
    filename:'./timeout.js' // Intentional (simulated) error - the file never loads, simulating a server timeout.
    }); 
}

function badStubRequest(entry) {
  switch(entry) {
    case 1:
      kmw.addKeyboards('1nval1d5tub@reque5t'); 
      break;
    case 2:
      keyman.addKeyboardsForLanguage(['PigLatn','1337sp34k']);
      break;
  } 
}