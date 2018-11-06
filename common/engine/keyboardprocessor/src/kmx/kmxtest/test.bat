@echo off

echo --Baseline test--
debug\kmxtest -kmx khmer_angkor.kmx -keys "xEjmr"

echo --jmr with no context--
debug\kmxtest -kmx khmer_angkor.kmx -keys "jmr"

echo --jmr with xE context--
debug\kmxtest -kmx khmer_angkor.kmx -keys "jmr" -context "\u1781\u17c2"
