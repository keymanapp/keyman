function loadKeyboards() 
{ 
  var kmw=keyman;

  // A testing keyboard handwritten with chirality information.
  kmw.addKeyboards({id:'chirality',name:'Chirality Testing',
    languages:{
      id:'en',name:'English',region:'North America'
    },
    filename:'chirality.js'
  });
    
  // A testing keyboard using 10.0 format and the KLS layout specifier
  // without the 'shift' layer properly defined.
  // Add a fully-specified, locally-sourced, keyboard with custom font  
  kmw.addKeyboards({id:'halfDefined',name:'Undefined Shift Layer Keyboard',
    languages:{
      id:'en',name:'English',region:'North America'
    },
    filename:'halfDefined.js'
  });
}