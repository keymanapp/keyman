<?php
  require_once('includes/template.php');
  require_once('includes/__docheader.php');

  $baselanguage = "language-bash";

  head([
    'title' => "Command: km-config"
  ]);

?>

<!-- TODO: Auto-generate from https://manpages.debian.org/unstable/keyman/km-config.1.en.html -->
<div class="body_text">
  <h1 class="title" id="kmConfig">km-config</h1>

  <h2 id="Summary" name="Summary">Summary</h2>

  <p>Launches Keyman Configuration for installing and showing information about Keyman keyboards</p>

  <?php
  syntax("km-config [-h] [--version] [-v] [-vv] [-i <INSTALL>]");
  params("-h, --help", "optional", "show this help message and exit",
    "--version", "optional", "show program's version number and exit",
    "-v, --verbose ", "optional", "verbose logging",
    "-vv, --veryverbose", "optional", "very verbose logging",
    "-i keyboard.kmp, -install keyboard.kmp", "optional", "install .kmp package");
  ?>

  <h2 id="Description" name="Description">Description</h2>
  <p>km-config shows the currently installed Keyman keyboard packages and allows you to view information about them.
    It enables you to download new keyboard packages from the website or install from local files.
  </p>

  <h2 id="See_also" name="See_also">See also</h2>
  <ul>
    <li><a href="km-package-install.php" title="km-package-install"><code>km-package-install</code></a></li>
    <li><a href="km-package-list-installed.php" title="km-package-list-installed"><code>km-package-list-installed</code></a></li>
    <li><a href="km-package-uninstall.php" title="km-package-uninstall"><code>km-package-uninstall</code></a></li>
  </ul>

</div>
