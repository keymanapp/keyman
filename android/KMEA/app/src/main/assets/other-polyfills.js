// Needed by the KMW's gesture engine until Chrome 61.
var DOMRect = DOMRect || function (x, y, width, height) {
  this.x = this.left = x;
  this.y = this.top = y;
  this.width = width;
  this.height = height;
  this.bottom = y + height;
  this.right = x + width;
};

// https://stackoverflow.com/questions/53308396/how-to-polyfill-array-prototype-includes-for-ie8
if(!Array.prototype.includes){
  //or use Object.defineProperty
  Array.prototype.includes = function(search){
   return !!~this.indexOf(search);
 }
}