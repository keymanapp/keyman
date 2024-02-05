function loadKeyboards()
{
  var kmw=keyman;
  kmw.addKeyboards({id:'us',name:'English',languages:{id:'en',name:'English'},
    filename:('../us-1.0.js')});

  // Add a fully-specified, locally-sourced, keyboard with custom font
  kmw.addKeyboards({id:'dotty_keys',name:'Dotty Keys',
    languages:{
      id:'und',name:'Special',region:'World',
      font:{family:'DejaVu Dots',source:['./DejaVu-Dots.ttf']}
      },
    filename:'./dotty_keys.js'
  });
}
