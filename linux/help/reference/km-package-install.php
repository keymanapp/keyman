<?php
require_once('includes/template.php');
require_once('includes/__docheader.php');

$baselanguage = "language-bash";

head([
  'title' => "Command: km-package-install"
]);

?>

<!-- TODO: Auto-generate from https://manpages.debian.org/unstable/keyman/km-package-install.1.en.html -->
<div class="body_text">
  <h1 class="title" id="kmPackageInstall">km-package-install</h1>

  <h2 id="Summary" name="Summary">Summary</h2>

  <p>Install a Keyman keyboard package</p>

  <?php
  syntax("km-package-install [-h] [-s] [-f <kmpfile>] [-p <packageID>] [--version] [-v] [-vv]");
  params("-h, --help", "optional", "show this help message and exit",
    "-s, --shared", "optional", "install to shared area <code>/usr/local</code>",
    "-f &lt;kmpfile&gt;, --file &lt;kmpfile&gt;", "optional", "Keyman kmp file",
    "-p &lt;packageID&gt;, --package &lt;packageID&gt;", "optional", "Keyman package id",
    "--version", "optional", "show program's version number and exit",
    "-v, --verbose ", "optional", "verbose logging",
    "-vv, --veryverbose", "optional", "very verbose logging");
  ?>

  <h2 id="Description" name="Description">Description</h2>
  <p>km-package-install installs a Keyman keyboard. Provide either a local .kmp file or specify a package id
    to download and install.
  </p>
  <h3>Tab Completion</h3>
  <p>
    You may use tab completion to find keyboard packages to install. The list comes from the network,
    so there may be a small delay before tab completion will work the first time you use it. The list of packages
    is cached for one week.
  </p>
  <p>
    The list is the full list including keyboards that do not work on Linux, so they won't ncecessarily install.
  </p>

  <h2 id="See_also" name="See_also">See also</h2>
  <ul>
    <li><a href="km-config.php" title="km-config"><code>km-config</code></a></li>
    <li><a href="km-package-list-installed.php" title="km-package-list-installed"><code>km-package-list-installed</code></a></li>
    <li><a href="km-package-uninstall.php" title="km-package-uninstall"><code>km-package-uninstall</code></a></li>
  </ul>

</div>
