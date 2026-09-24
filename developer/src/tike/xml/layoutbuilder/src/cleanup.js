$(function() {

  builder.cleanupKVKL = function() {
    let cleanupEmptyArrays = function() {
      for (let platform in KVKL) {
        for (let layer of KVKL[platform].layer) {
          let newRow = [], n = 1;
          for(let row of layer.row) {
            if(row.key && Array.isArray(row.key)) {
              for(let key of row.key) {
                if(key.sk && (!Array.isArray(key.sk) || key.sk.length == 0)) {
                  delete key.sk;
                }
              }
              if(row.key.length > 0) {
                row.id = n;
                n++;
                newRow.push(row);
              }
            }
          }
          layer.row = newRow;
        }
      }
      return true;
    };

    let cleanupNumericField = function(obj, prop) {
      if(typeof obj[prop] == 'number') {
        return true;
      }
      if(typeof obj[prop] == 'string') {
        let v = obj[prop].trim();
        if(v != '') {
          v = parseInt(v, 10);
          if(isFinite(v)) {
            obj[prop] = v;
            return true;
          }
        }
      }
      delete obj[prop];
      return true;
    }

    let cleanupKeyTypes = function(key) {
      // Delete empty strings for id
      if(typeof key.id != 'undefined') {
        if(typeof key.id != 'string') delete key.id;
        else if(key.id.trim() == '') delete key.id;
      }
      // Enforce numeric types for sp, pad, width
      return cleanupNumericField(key, 'sp') &&
        cleanupNumericField(key, 'pad') &&
        cleanupNumericField(key, 'width');
    }

    let cleanupTypes = function() {
      for(let platform in KVKL) {
        for(let layer of KVKL[platform].layer) {
          for(let row of layer.row) {
            for(let key of row.key) {
              cleanupKeyTypes(key);
              if(key.sk) {
                for(let sk of key.sk) {
                  cleanupKeyTypes(sk);
                }
              }
            }
          }
        }
      }
      return true;
    }

    return cleanupEmptyArrays() && cleanupTypes();
  };

}.bind(builder));
