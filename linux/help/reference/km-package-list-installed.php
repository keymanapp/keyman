<?php
require_once('includes/template.php');
require_once('includes/__docheader.php');

$baselanguage = "language-bash";

head([
  'title' => "Command: km-package-list-installed"
]);

?>

<!-- TODO: Auto-generate from https://manpages.debian.org/unstable/keyman/km-package-list-installed.1.en.html -->
<div class="body_text">
  <h1 class="title" id="kmPackageListInstalled">km-package-list-installed</h1>

  <h2 id="Summary" name="Summary">Summary</h2>

  <p>List installed Keyman keyboard packages</p>

  <?php
  syntax("km-package-list-installed [-h] [-l] [-k] [-s] [-o] [-u] [--version] [-v] [-vv]");
  params("-h, --help", "optional", "show this help message and exit",
    "-l, --long", "optional", "long format also shows description",
    "-s, --shared", "optional", "show those installed in shared areas",
    "-o, --os", "optional", "show those installed by the OS",
    "-u, --user", "optional", "show those installed in user areas",
    "--version", "optional", "show program's version number and exit",
    "-v, --verbose ", "optional", "verbose logging",
    "-vv, --veryverbose", "optional", "very verbose logging");
  ?>

  <h2 id="Description" name="Description">Description</h2>
  <p>km-package-list-installed shows installed Keyman keyboards with name, version, id.
  </p>

  <h2 id="See_also" name="See_also">See also</h2>
  <ul>
    <li><a href="km-package-install.php" title="km-package-install"><code>km-package-install</code></a></li>
    <li><a href="km-config.php" title="km-config"><code>km-config</code></a></li>
    <li><a href="km-package-uninstall.php" title="km-package-uninstall"><code>km-package-uninstall</code></a></li>
  </ul>

</div>
