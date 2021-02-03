<?php
require_once('includes/template.php');
require_once('includes/__docheader.php');

$baselanguage = "language-bash";

head([
  'title' => "Command: km-package-get"
]);

?>

<!-- TODO: Auto-generate from https://manpages.debian.org/unstable/keyman/km-package-get.1.en.html -->
<div class="body_text">
  <h1 class="title" id="kmPackageGet">km-package-get</h1>

  <h2 id="Summary" name="Summary">Summary</h2>

  <p>Download a Keyman keyboard package</p>

  <?php
  syntax("km-package-get [-h] [--version] [-v] [-vv] id");
  params("id", "String", "Keyman keyboard id",
    "-h, --help", "optional", "show this help message and exit",
    "--version", "optional", "show program's version number and exit",
    "-v, --verbose ", "optional", "verbose logging",
    "-vv, --veryverbose", "optional", "very verbose logging");
  ?>

  <h2 id="Description" name="Description">Description</h2>
  <p>km-package-get downloads Keyman keyboard package to <code>~/.cache/keyman</code>.
  </p>

  <h2 id="See_also" name="See_also">See also</h2>
  <ul>
    <li><a href="km-package-install.php" title="km-package-install"><code>km-package-install</code></a></li>
    <li><a href="km-config.php" title="km-config"><code>km-config</code></a></li>
    <li><a href="km-package-list-installed.php" title="km-package-list-installed"><code>km-package-list-installed</code></a></li>
    <li><a href="km-package-uninstall.php" title="km-package-uninstall"><code>km-package-uninstall</code></a></li>
  </ul>

</div>
