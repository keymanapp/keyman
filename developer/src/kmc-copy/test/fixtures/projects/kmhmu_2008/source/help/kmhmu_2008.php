<?php
  $pagename = 'Kmhmu 2008 (deprecated) Keyboard Help';
  $pagetitle = $pagename;
  $style = <<<EXTRA
  p { font: 10pt Tahoma }
  h1 { font: bold 16pt Tahoma; color: #4444cc;margin-bottom: 2px; }
  h2 { font: bold 12pt Tahoma; color: #4444cc }
  span.kmhmu {font-family: "Leelawadee UI","Kmhmu MX","Kmhmu OT"; font-size: 12pt }
  table.display tr td { font: 10pt Tahoma; border: solid 1px #ccccff; padding: 4px; text-align: center}
  table.display tr th { font: bold 10pt Tahoma; border: solid 1px #ccccff; padding: 4px; text-align: center}
  table.display { border-collapse: collapse; width:640px;}
  img {width:100% ;max-width:750px;}
  th.narrow {width:40px;}
  th.medium {width:100px;}
EXTRA;
  require_once('header.php');
?>





<h2>Keyboard Layout Notes</h2>
<p>Keyboard layout (as shown below) follows the Lao standard, with the following minor differences to simplify entry of Kmhmu text:</p>

<table class='display'>
<tr>
  <th class="narrow">Shift</th>
  <th class="medium">Key</th>
  <th class="medium">Result</th>
  <th>Notes</th></tr>
<tr>
  <td>●</td>
  <td>Y</td>
  <td class='kmhmu'> ກ໌ </td>
  <td></td>
</tr>
<tr>
  <td>●</td>
  <td>U</td>
  <td class='kmhmu'> ຍ໌ </td>
  <td></td>
</tr>
<tr>
  <td></td>
  <td>i</td>
  <td class='kmhmu'> ຣ </td>
  <td rowspan='2' class='note'>This follows the pre-2005 Lao typewriter convention, <br />
  since <span class='kmhmu'>ຣ</span> is widely used in Kmhmu while <span class='kmhmu'>ຮ</span> is rarely if ever used.</td>
</tr>
<tr>
  <td>●</td>
  <td>I</td>
  <td class='kmhmu'>ຮ</td>
</tr>
<tr>
  <td></td>
  <td>space bar<br />(pressed once)</td>
  <td>thin space</td>
  <td rowspan='2' class='note'>To allow narrow spacing between words, the space bar inserts a (wrapping) thin-space when pressed once, or normal spaces if pressed more than once.</td>
</tr>
<tr>
  <td></td>
  <td>space bar<br />(twice or more)</td>
  <td>normal space</td>
</tr>
<tr>
  <td>●</td>
  <td>|</td>
  <td>ZWSP</td>
  <td class='note'>Zero-width (wrapping) space character</td>
</tr>
<tr>
  <td>●</td>
  <td>%</td>
  <td class='kmhmu'> ໌ </td>
  <td class='note'>This mark used to form modified characters found in Kmhmu (but not in Lao) replaces the "Cancellation mark" (thanthakhat) found in normal Lao fonts, so can be entered using the same keystroke.  But in most cases, the Y and U keys would be used instead. </td>
</tr>
</table>

<h2 style="clear: left; page-break-before: always">Keyboard layout - Unshifted</h2>
<p><img alt="Kmhmu 2008 keyboard layout: normal (default) state" src="kmhmu_2008U_.png"/></p>
<h2 style="clear: left; page-break-before: always">Keyboard layout - with Shift</h2>
<p><img alt="Kmhmu 2008 keyboard layout: shift state" src="kmhmu_2008U_S.png"/></p>
<h2 style="clear: left; page-break-before: always">Keyboard layout - with Right Alt</h2>
<p><img alt="Kmhmu 2008 keyboard layout: AltGr state" src="kmhmu_2008U_RA.png"/></p>

<h2>Fonts</h2>
  <p>The font <b>Kmhmu MX</b> has been included with the keyboard layout.</p>
