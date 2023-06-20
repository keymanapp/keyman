keyman.init({
  attachType: 'auto'
});

window.addEventListener('load', function() {
  keyman.addKeyboards({
    id: 'testvariable',
    name: 'testvariable',
    languages: [{
      id:'en', name:'English'
    }],
    filename: './testvariable.js'
  });

  keyman.addKeyboards({
    id: 'oldtestvariable',
    name: 'oldtestvariable',
    languages: [{
      id:'en', name:'English'
    }],
    filename: './oldtestvariable.js'
  });

  keyman.addKeyboards({
    id: 'us',
    name: 'English US',
    languages: [{
      id: 'en',
      name: 'English'
    }],
    filename: '../us-1.0.js'
  });

  const pageRef = (window.location.protocol == 'file:')
    ? window.location.href.substr(0, window.location.href.lastIndexOf('/')+1)
    : window.location.href;

  keyman.addModel({
    id: 'nrc.en.mtnt',
    languages: ['en'],
    path: pageRef + 'dmg.dv.test.model.js'
  });
}, false);
