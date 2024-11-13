/*(C) Copyright 2021 SIL International. All Rights Reserved. Details: keymanweb.com*/
function loadKeyboards() {
    // Uses an older type of keyboard stub definition.
    var KWK={
        "devanagari_inscript":{KN:"Devanagari (INSCRIPT)", KLC:"hi", KL:"Hindi"},
        "european2":{KN:"EuroLatin2", KLC:"en", KL:"English"},
        "hebrew":{KN:"Hebrew", KLC:"he", KL:"Hebrew"},
        "korean_korda":{KN:"Korean (KORDA) - 30 Day Evaluation", KLC:"ko", KL:"Korean"},
        "korean_morse":{KN:"Korean (Morse) - 30 Day Evaluation", KLC:"ko", KL:"Korean"},
        "laokeys":{KN:"Lao (Phonetic)", KLC:"lo", KL:"Lao"}
    };

    for(var n in KWK) {
        KeymanWeb.registerStub({KF:"./js/" + n+".js", KI:"Keyboard_"+n, KN:KWK[n].KN, KL:KWK[n].KL, KLC:KWK[n].KLC});
    }
}
