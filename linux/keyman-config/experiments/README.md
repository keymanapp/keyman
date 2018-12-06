# Experimental scripts not for release

### list_kmp.py

`./list_kmp.py`

Find all known keyboard packages in the api and sort the ids into files according to:

* goodjsonkmp.txt - kmp with kmp.json
* goodinfkmp.txt - kmp with kmp.inf but no kmp.json
* brokeninf.txt - kmp with kmp.inf but can't process it
* infnokeyboard.txt - kmp with kmp.info but no [Keyboard]
* nokmp.txt - no kmp
* nodata.txt - no data in the api for the keyboard
