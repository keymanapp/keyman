function loadKeyboards() 
{ 
  var kmw=keyman;

  kmw.addKeyboards({id:'platformtest',name:'Platform Testing',
    languages:{
      id:'en',name:'English',region:'North America'
    },
    filename: '../../../../../build/test-resources/keyboards/platformtest.js'
  });
}