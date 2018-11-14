@echo off

echo --Baseline test--
debug\kmxtest -kmx khmer_angkor.kmx -keys "xEjmr" -expected-output "\u1781\u17d2\u1798\u17c2\u179a"

echo --jmr with no context--
debug\kmxtest -kmx khmer_angkor.kmx -keys "jmr" -expected-output "\u17d2\u1798\u179a"

echo --jmr with xE context--
debug\kmxtest -kmx khmer_angkor.kmx -keys "jmr" -context "\u1781\u17c2" -expected-output "\u1781\u17d2\u1798\u17c2\u179a"
