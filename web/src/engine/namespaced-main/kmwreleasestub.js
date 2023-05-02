/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

/**
 * External references to separately compiled string prototype extensions
 */  

/**
 * Prototypes for SMP string function extensions 
 *
 * @param   {number}  cp0
 * @return  {string}
 **/   
String.kmwFromCharCode = function(cp0) {};

/**
 * @param   {number}  codePointIndex
 * @return  {number}
 **/   
String.prototype.kmwCharCodeAt = function(codePointIndex) {};

/**
 * @param   {string}  searchValue
 * @param   {number}  fromIndex
 * @return  {number}
 **/   
String.prototype.kmwIndexOf = function(searchValue, fromIndex) {};

/**
 * @param   {string}  searchValue
 * @param   {number}  fromIndex
 * @return  {number}
 **/   
String.prototype.kmwLastIndexOf = function(searchValue, fromIndex) {};

/**
 * @return  {number}
 **/   
String.prototype.kmwLength = function() {};

/**
 * @param   {number}  beginSlice
 * @param   {number}  endSlice
 * @return  {string}
 **/   
String.prototype.kmwSlice = function(beginSlice, endSlice) {};

/**
 * @param   {number}  start
 * @param   {number=} length
 * @return  {string}
 **/   
String.prototype.kmwSubstr = function(start, length) {};

/**
 * @param   {number}  indexA
 * @param   {number=} indexB
 * @return  {string}
 **/   
String.prototype.kmwSubstring = function(indexA, indexB) {};

/**
 * @param   {number}  codeUnitIndex
 * @return  {number}
 **/   
String.prototype.kmwNextChar = function(codeUnitIndex) {};

/**
 * @param   {number}  codeUnitIndex
 * @return  {number}
 **/   
String.prototype.kmwPrevChar = function(codeUnitIndex) {};

/**
 * @param   {number}  codePointIndex
 * @return  {number}
 **/   
String.prototype.kmwCodePointToCodeUnit = function(codePointIndex) {};

/**
 * @param   {number}  codeUnitIndex
 * @return  {number}
 **/   
String.prototype.kmwCodeUnitToCodePoint = function(codeUnitIndex) {};

/**
 * @param   {number}  codePointIndex
 * @return  {string}
 **/   
String.prototype.kmwCharAt = function(codePointIndex) {};



/**
 * Prototypes for string function extensions that can be either BMP or SMP
 * 
 * @param   {number}  cp0
 * @return  {string}
 **/   
String._kmwFromCharCode = function(cp0) {};

/**
 * @param   {number}  codePointIndex
 * @return  {number}
 **/   
String.prototype._kmwCharCodeAt = function(codePointIndex) {};

/**
 * @param   {string}  searchValue
 * @param   {number}  [fromIndex]
 * @return  {number}
 **/   
String.prototype._kmwIndexOf = function(searchValue, fromIndex) {};

/**
 * @param   {string}  searchValue
 * @param   {number}  fromIndex
 * @return  {number}
 **/   
String.prototype._kmwLastIndexOf = function(searchValue, fromIndex) {};

/**
 * @return  {number}
 **/   
String.prototype._kmwLength = function() {};

/**
 * @param   {string}  beginSlice
 * @param   {string}  endSlice
 * @return  {string}
 **/   
String.prototype._kmwSlice = function(beginSlice, endSlice) {};

/**
 * @param   {number}  start
 * @param   {number=} length
 * @return  {string}
 **/   
String.prototype._kmwSubstr = function(start, length) {};

/**
 * @param   {number}  indexA
 * @param   {number=} indexB
 * @return  {string}
 **/   
String.prototype._kmwSubstring = function(indexA, indexB) {};

/**
 * @param   {number}  codeUnitIndex
 * @return  {number}
 **/   
String.prototype._kmwNextChar = function(codeUnitIndex) {};

/**
 * @param   {number}  codeUnitIndex
 * @return  {number}
 **/   
String.prototype._kmwPrevChar = function(codeUnitIndex) {};

/**
 * @param   {number}  codePointIndex
 * @return  {number}
 **/   
String.prototype._kmwCodePointToCodeUnit = function(codePointIndex) {};

/**
 * @param   {number}  codeUnitIndex
 * @return  {number}
 **/   
String.prototype._kmwCodeUnitToCodePoint = function(codeUnitIndex) {};

/**
 * @param   {number}  codePointIndex
 * @return  {string}
 **/   
String.prototype._kmwCharAt = function(codePointIndex) {};


/**
 * String extension to enable SMP handling only as required
 *  
 * @param   {(boolean|number)}  bEnable
 **/   
String.kmwEnableSupplementaryPlane = function(bEnable) {};

/**
 * External debug routines are stubbed out, allowing the compiler to remove all references 
 */
 
/** @nosideeffects */
function _Debug(t){};

/** @nosideeffects */
function _DebugEnter(t){};

/** @nosideeffects */
function _DebugExit(t){};

/** @nosideeffects */
function _DebugDeadKeys(t,u){};
