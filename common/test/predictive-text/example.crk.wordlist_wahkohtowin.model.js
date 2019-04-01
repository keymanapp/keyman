(function() {
'use strict';
var model = {};
model.backingData = [["nimosôm",1],["nohkom",1],["nohtâwiy",1],["nikâwiy",1],["nohcâwîs",1],["nikâwîs",1],["nisis",1],["nisikos",1],["nistês",1],["nimis",1],["nisîmis",1],["nikisos",1],["nitânis",1],["nôsisim",1],["ninâpêm",1],["nitiskwêm",1],["niwîkimâkan",1]];
LMLayerWorker.loadModel(new models.WordListModel(model.backingData));
LMLayerWorker.loadWordBreaker(new DefaultWordBreaker({"allowedCharacters":{"initials":"abcdefghijklmnopqrstuvwxyz","medials":"abcdefghijklmnopqrstuvwxyz","finals":"abcdefghijklmnopqrstuvwxyz"},"defaultBreakCharacter":" "}));
})();
