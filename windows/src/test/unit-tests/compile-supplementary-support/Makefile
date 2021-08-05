#
# test if KS=1 is in the right files, both in debug and non-debug versions
#

KMCOMP="..\..\..\..\bin\developer\kmcomp.exe"
KMCOMPD=$(KMCOMP) -d

test: test-nodebug test-debug

test-nodebug:
	$(KMCOMP) i3317_nosupp.kmn i3317_nosupp-1.0.js
	grep -vl "KS=1" i3317_nosupp-1.0.js
	$(KMCOMP) i3317_withsupp.kmn i3317_withsupp-1.0.js
	grep -l "KS=1" i3317_withsupp-1.0.js
	$(KMCOMP) i3317_withsupp_incontext.kmn i3317_withsupp_incontext-1.0.js
	grep -l "KS=1" i3317_withsupp_incontext-1.0.js
	$(KMCOMP) i3317_withsupp_inmatch.kmn i3317_withsupp_inmatch-1.0.js
	grep -l "KS=1" i3317_withsupp_inmatch-1.0.js
	$(KMCOMP) i3317_withsupp_innomatch.kmn i3317_withsupp_innomatch-1.0.js
	grep -l "KS=1" i3317_withsupp_innomatch-1.0.js
	$(KMCOMP) i3317_withsupp_instore.kmn i3317_withsupp_instore-1.0.js
	grep -l "KS=1" i3317_withsupp_instore-1.0.js

test-debug:
	$(KMCOMPD) i3317_nosupp.kmn i3317_nosupp-1.0.js
	grep -vl "KS=1" i3317_nosupp-1.0.js
	$(KMCOMPD) i3317_withsupp.kmn i3317_withsupp-1.0.js
	grep "KS=1" i3317_withsupp-1.0.js
	$(KMCOMPD) i3317_withsupp_incontext.kmn i3317_withsupp_incontext-1.0.js
	grep "KS=1" i3317_withsupp_incontext-1.0.js
	$(KMCOMPD) i3317_withsupp_inmatch.kmn i3317_withsupp_inmatch-1.0.js
	grep "KS=1" i3317_withsupp_inmatch-1.0.js
	$(KMCOMPD) i3317_withsupp_innomatch.kmn i3317_withsupp_innomatch-1.0.js
	grep "KS=1" i3317_withsupp_innomatch-1.0.js
	$(KMCOMPD) i3317_withsupp_instore.kmn i3317_withsupp_instore-1.0.js
	grep "KS=1" i3317_withsupp_instore-1.0.js

clean:
	-del *.js
	-del *.json
