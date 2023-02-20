function loadKeyboards() 
{ 
  var kmw=keyman;

    // Add a fully-specified, locally-sourced, keyboard with custom font  
    kmw.addKeyboards({id:'sil_cameroon_qwerty',name:'Cameroon QWERTY (SIL)',
      languages:{
        id:'aal-Latn',name:'Afade (Latin)',region:'Africa',
        font:{family:'AndikaAfr',source:['./ANDIKAAFR-R.TTF']}
        },
      filename:'./sil_cameroon_qwerty.js'
      });   
}
