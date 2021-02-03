<?php
require_once('includes/template.php');
require_once('includes/__docheader.php');

$baselanguage = "language-bash";

head([
  'title' => "Command: km-kvk2ldml"
]);

?>

<!-- TODO: Auto-generate from https://manpages.debian.org/unstable/keyman/km-kvk2ldml.1.en.html -->
<div class="body_text">
  <h1 class="title" id="kmKVK2LDML">km-kvk2ldml</h1>

  <h2 id="Summary" name="Summary">Summary</h2>

  <p>Convert a Keyman on-screen keyboard file to LDML</p>

  <?php
  syntax("km-kvk2ldml [-h] [-p] [-k] [-o LDMLFILE] [--version] [-v] [-vv] kvkfile");
  params("kvkfile", "String", "kvk file",
    "-h, --help", "optional", "show this help message and exit",
    "-p, --print", "optional", "print kvk details",
    "-k, --keys", "optional", "if printing, also print all keys",
    "-o LDMLFILE, --output LDMLFILE", "optional", "output <code>LDML</code> file location",
    "--version", "optional", "show program's version number and exit",
    "-v, --verbose ", "optional", "verbose logging",
    "-vv, --veryverbose", "optional", "very verbose logging");
  ?>

  <h2 id="Description" name="Description">Description</h2>
  <p>km-kvk2ldml converts a Keyman kvk on-screen keyboard file to an LDML file. Optionally print the details of the kvk file.
  </p>

  <h2 id="See_also" name="See_also">See also</h2>
  <ul>
    <li><a href="km-config.php" title="km-config"><code>km-config</code></a></li>
  </ul>

</div>
