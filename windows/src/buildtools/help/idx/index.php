<?php include( "../../../global/help/hhdefs.php" );

/*
  keyword [; { *keyword | htmlfile } ...]

  'keyword' is a keyword name. A semicolon precedes all information that applies to a single
  keyword. You can split the information over several lines, as long as a semicolon is the
  first character on all lines after the first. If there is no information following a keyword,
  it specifies a keyword which has only subtopics, and no topics of its own.
  An asterisk preceding a keyword in the information part is a "see also" which jumps to
  another keyword in the index.
  Note that the index is not sorted: it is presented in the order in which it is written.
*/

INDEX( <<<INDEX

no keyword                  ; main/contents.html

INDEX
);

?>