// https://stackoverflow.com/questions/53308396/how-to-polyfill-array-prototype-includes-for-ie8
if(!Array.prototype.includes){
  //or use Object.defineProperty
  Array.prototype.includes = function(search){
   return !!~this.indexOf(search);
 }
}
