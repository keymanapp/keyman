/**
 * @type {Object.<number, string>}
 */
const tpl = {
  0: 'Copyright',
  1: 'Font Family',
  2: 'Font Subfamily',
  3: 'Unique identifier',
  4: 'Full name',
  5: 'Version',
  6: 'Postscript name',
  7: 'Note',
  8: 'Company',
  9: 'Author',
  10: 'Description',
  11: 'URL',
  12: 'URL',
  13: 'License',
  14: 'URL',
  // 15: '',
  16: 'Name'
  // 17: ''
};

const tagName = (text='') => /^[^a-z]*$/.test(text)?text.split(' ').length>4?'paragraph':'title':'paragraph';
/**
 * format meta.tables.name, property description license, reference
 * @param {{[k: string]: string}} e
 */
export default function (e){
  var meta={
    /**
     * @type {{name:string,text:string}[]}
     */
    property:[],
    /**
     * @type {{name:string,text:string}[]}
     */
    description:[],
    /**
     * @type {{name:string,text:string}[]}
     */
    license:[],
    /**
     * @type {{name:string,text:string}[]}
     */
    reference:[]
  };

  for (const key in e) {
    if (e.hasOwnProperty(key)) {
      const i = parseInt(key);
      /**
       * @type {keyof typeof tpl}
       */
      var tplId = (i);
      const context = e[i].trim();
      var pA = context.replace('~\r\n?~', "\n").split('\n').map(i=>i.trim()).filter( i => i);
      if (pA.length > 1) {
        /**
         * @type {keyof typeof meta}
         */
        var id = (i == 10)?'description':'license';
        meta[id]=[];
        for (const eP in pA) {
          if (pA.hasOwnProperty(eP)) {
            var text = pA[eP].trim();
            meta[id].push({name:tagName(text),text:text});
          }
        }
      } else if(context) {
        if (/^s?https?:\/\/[-_.!~*'()a-zA-Z0-9;\/?:\@&=+\$,%#]+$/.test(context)){
          var has = meta.reference.findIndex(a => a.text == context);
          if (has < 0) {
            meta.reference.push({name:'url',text:context});
          }
        } else if (i > 0 && i < 6) {
          var name = tpl[tplId].replace(' ','-').toLowerCase();
          meta.property.push({name: name, text: context});
        } else {
          if (tpl.hasOwnProperty(i)){
            if (i == 0 || i == 7) {
              var pA = context.replace(/---+/, "\n").split('\n').map(i=>i.trim()).filter( i => i);
              for (const eP in pA) {
                if (pA.hasOwnProperty(eP)) {
                  var text = pA[eP].trim();
                  meta.description.push({name:tagName(text),text:text});
                }
              }
            } else if (i == 13) {
              meta.license.push({name:tagName(context), text: context});
            } else {

              var name = tpl[tplId].replace(' ','-').toLowerCase();
              meta.property.push({name:name, text: context});
            }
          }
        }
      }

      // if (i == 1) {
      //   meta.title =context.replace('_',' ');
      //   meta.keywords = context.replace('_',',');
      //   meta.description = context;
      // } else if (i == 7 && context) {
      //   meta.description = context;
      // } else if (i == 4 && context) {
      //   meta.description = context;
      // }
    }
  }
  return meta;
}