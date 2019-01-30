<?php
  $pagename = 'Hieroglyphic Keyboard Help';
  $pagetitle = 'Hieroglyphic Keyboard Help';
  $pagestyle = <<<END
  p {font-size: 11pt; margin:5px 0px 0px;}
  .example {margin-left:40px}
  section {clear:both;}
  article{clear:left; margin-bottom:60px;}
  samp{font-family: Gardiner, Aegyptus; color:blue;}
  kbd {color:black;}
  .break {clear:both !important; page-break-before:always !important;}
  aside {background-color:#eee;border:2px solid #ddd;-webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px;margin:10px 0px; padding:0px 10px 5px;font-size:10pt;}
  aside h4 {color:#AD4A28; text-align:left;padding:0px 0px 5px;margin-top:5px;font-size:11pt;}
  aside p {font-size:10pt;}
  aside kbd {color:black; font:9pt Arial; border:solid 1px grey; background:#ccc; margin:0px 1px;  padding:2px 4px; vertical-align:middle; line-height:2em; -webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px;}
  table {border-collapse:collapse !important; margin:10px 0px 20px 40px;}
  .grid col {text-align:center;  background-color:#ffffff;}
  .grid th {text-align:center; font-weight:bold}
  .grid td, .grid th {padding:2px 5px;border-bottom:1px solid #ddd;}
  .grid tr {text-align:center; font-weight:normal; height:80px;}
  .grid thead tr,.grid .headrow{height:30px;}
  .grid .headrow {font-weight:bold; color:#AD4A28; background-color:#eee;}
  .gardiner {font-family:Gardiner, Aegyptus; font-size:24pt;line-height:20px;vertical-align:-2px;}
  col.trans {width:60px;}
  col.keys  {width:50px;}
  col.signs {width:85px;}
  #guidemdc col.keys {width:120px;}
  #guidemdc col.signs{width:465px;}
  #guidegardiner col.signs{width:125px;}
  #guidegardiner col.keys {width:115px;}
  tr.halfrow {border-bottom:2px solid white !important;}
  td samp {font-size:24pt}
  td samp.hiero {vertical-align:middle;font-size:49pt;}
  table kbd {color:black; font:12pt Arial; border:solid 1px grey; background:#ccc; margin:0px 1px;  padding:2px 4px; vertical-align:middle; -webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px;}
  .columns  {width:375px; -webkit-column-count: 2; -webkit-column-gap: 10px; -moz-column-count: 2; -moz-column-gap: 10px; column-count: 2; column-gap: 10px;}
  .colleft, .colright {position:relative; float:left;}
  .colright {padding-left:40px;}
  p samp {font-size:22pt; line-height:.8em;}
  p kbd {color:black; font:9pt Arial; border:solid 1px grey; background:#ccc; margin:0px 1px; padding:0px 3px; -webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px;}
  samp.hiero {font-size:44pt; line-height:43px; font-family:Gardiner, Aegyptus;}
  samp .cyc {font-size:30pt; color:#2D2C2C;}
  .key { float: left;display: block; position: relative;overflow: hidden;height: 35px;margin: 2px 0px 0px 2px;border: solid 1px grey;-webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px;     }
  .plain {background-repeat: no-repeat;width: 34px;}
  .empty {background:#eee;}
  .K_BKQUOTE {width:34px;}
  .K_BKSP {width: 60px;}
  .K_TAB {width: 52px;}
  .K_BKSLASH {width: 42px;}
  .K_CAPS {width: 62px;}
  .K_ENTER {width: 70px; }
  .K_ENTER-EU1 {width: 42px; -webkit-border-radius: 3px 3px 0px; -moz-border-radius: 3px 3px 0px; border-radius: 3px 3px 0px;}
  .K_ENTER-EU1 .key kbd {left:3px !important;}
  .K_ENTER-EU2 {width: 32px; margin-top:-1px !important; border-top:none !important;height:39px !important;-webkit-border-radius: 0px 0px 3px 3px; -moz-border-radius: 0px 0px 3px 3px; border-radius: 0px 0px 3px 3px;}
  .K_SHIFTL {  width: 85px;}
  .K_SHIFTL-EU {  width: 47px;}
  .K_oE2 {  display:none;        }
  .K_SHIFTR {  width: 85px;}
  .K_CTRLL {width:60px;margin-right: 40px; }
  .K_ALTL {  width: 40px;}
  .K_SPACE , .K_SPACE samp{  width: 224px;  }
  .K_ALTR {  width: 40px;}
  .K_CTRLR {  width: 60px;margin-left: 76px;}
  .special {  background-repeat: no-repeat;background: #c0c0c0;}
  .key kbd {font: bold 7pt Arial; position: absolute; left: 3px; top: 3px;}
  .key kbd {          display: block;}
  .special kbd {  display: block;}
  .key samp {font-size: 19pt;position: absolute; display: block;right: 5px;bottom: 0px;color: blue;}
  .LTN {font-family:Arial !important;font-size:12px !important;color: #AD4A28 !important;right:2px !important;bottom:2px !important; font-variant:small-caps;}
END;
  require_once('header.php');
?>

  <p>Created by Christian Casey for Right Said Languages</p>
  <section id='overview'>
	<h2>Overview</h2>
    <p>This keyboard provides a quick and easy way to enter Ancient Egyptian
    hieroglyphs as Unicode text. It is designed to bridge the gap between the
    available hieroglyphic editors, which encode hieroglyphs using the <a href='http://en.wikipedia.org/wiki/Manuel_de_Codage'>Manuel de Codage</a> standard (<a href="http://www.catchpenny.org/codage/">MdC</a>), and the websites and programs designed for basic fonts, which will almost certainly never support the complex
    formatting needed to properly display Ancient Egyptian. This keyboard emulates MdC editors as much as possible in order to be accessible to users familiar with other programs. Most of
    these programs are intuitive, so new users should find the keyboard
    easy to learn and use as well (see <a href="#methods">Methods for Typing Hieroglyphs</a> for more about available typing methods).</p>
	<h4>Basic Function</h4>
    <p>The Unicode Hieroglyphic keyboard lets you type transliterated text and hieroglyphs. The keyboard outputs transliterated text using the Manuel de Codage (MdC) standard. It creates hieroglyphs from MdC transliteration <b>or</b> <a href='http://en.wikipedia.org/wiki/Gardiner%27s_sign_list'>Gardiner sign list</a> numbers each time you press <kbd>Spacebar</kbd>. Repeat pressing <kbd>Spacebar</kbd> on signs output from MdC transliteration will cycle through phonetically identical signs. Pressing <kbd>Ctrl</kbd>+<kbd>Spacebar</kbd> instead outputs a space, which will also prevent transliteration from becoming hieroglyphs.</p>
	<h4>On Screen Keyboard</h4>
	<p>This keyboard includes an On Screen Keyboard view of the MdC transliteration standard for easy reference. The On Screen Keyboard works best when associated with a QWERTY US layout.</p>
	<h4>Fonts</h4>
	<p>This is a Unicode keyboard and works with any Unicode font which has support for Ancient Egyptian transliteration and hieroglyphs. Fonts which work with this keyboard include Aegyptus and Gardiner, which install with the keyboard. To see if other fonts on your computer support the keyboard, use the Keyman Desktop Font Helper.</p>

  </section>

  <section id='layout' class='break'>
    <h2>Keyboard Layout</h2>
	<article id='unshifted'>
		<h3 style="clear:left">Unshifted</h3>
		<div class="key plain K_BKQUOTE">
			<kbd>`</kbd>
			<samp class='LTN'>‹ltn›</samp>
		</div>
		<div class="key plain K_1">
			<kbd>1</kbd>
			<samp>1</samp>
		</div>
		<div class="key plain K_2">
			<kbd>2</kbd>
			<samp>2</samp>
		</div>
		<div class="key plain K_3">
			<kbd>3</kbd>
			<samp>3</samp>
		</div>
		<div class="key plain K_4">
			<kbd>4</kbd>
			<samp>4</samp>
		</div>
		<div class="key plain K_5">
			<kbd>5</kbd>
			<samp>5</samp>
		</div>
		<div class="key plain K_6">
			<kbd>6</kbd>
			<samp>6</samp>
		</div>
		<div class="key plain K_7">
			<kbd>7</kbd>
			<samp>7</samp>
		</div>
		<div class="key plain K_8">
			<kbd>8</kbd>
			<samp>8</samp>
		</div>
		<div class="key plain K_9">
			<kbd>9</kbd>
			<samp>9</samp>
		</div>
		<div class="key plain K_0">
			<kbd>0</kbd>
			<samp>0</samp>
		</div>
		<div class="key plain K_HYPHEN">
			<kbd>-</kbd>
			<samp>-</samp>
		</div>
		<div class="key plain K_EQUAL">
			<kbd>=</kbd>
			<samp>=</samp>
		</div>
		<div class="key special K_BKSP">
			<kbd>Backspace</kbd>
		</div>
		<br style="clear: left">
		<div class="key special K_TAB">
			<kbd>Tab</kbd>
		</div>
		<div class="key plain K_Q">
			<kbd>q</kbd>
			<samp>ḳ</samp>
		</div>
		<div class="key plain K_W">
			<kbd>w</kbd>
			<samp>w</samp>
		</div>
		<div class="key plain K_E">
			<kbd>e</kbd>
			<samp>e</samp>
		</div>
		<div class="key plain K_R">
			<kbd>r</kbd>
			<samp>r</samp>
		</div>
		<div class="key plain K_T">
			<kbd>t</kbd>
			<samp>t</samp>
		</div>
		<div class="key plain K_Y">
			<kbd>y</kbd>
			<samp>y</samp>
		</div>
		<div class="key plain K_U">
			<kbd>u</kbd>
			<samp>u</samp>
		</div>
		<div class="key plain K_I">
			<kbd>i</kbd>
			<samp>i</samp>
		</div>
		<div class="key plain K_O">
			<kbd>o</kbd>
			<samp>o</samp>
		</div>
		<div class="key plain K_P">
			<kbd>p</kbd>
			<samp>p</samp>
		</div>
		<div class="key plain K_LBRKT">
			<kbd>[</kbd>
			<samp>[</samp>
		</div>
		<div class="key plain K_RBRKT">
			<kbd>]</kbd>
			<samp>]</samp>
		</div>
		<div class="key plain K_BKSLASH">
			<kbd>\</kbd>
			<samp>\</samp>
		</div>
		<br style="clear: left">
		<div class="key special K_CAPS">
			<kbd>Caps Lock</kbd>
		</div>
		<div class="key plain K_A">
			<kbd>a</kbd>
			<samp>ꜥ</samp>
		</div>
		<div class="key plain K_S">
			<kbd>s</kbd>
			<samp>s</samp>
		</div>
		<div class="key plain K_D">
			<kbd>d</kbd>
			<samp>d</samp>
		</div>
		<div class="key plain K_F">
			<kbd>f</kbd>
			<samp>f</samp>
		</div>
		<div class="key plain K_G">
			<kbd>g</kbd>
			<samp>g</samp>
		</div>
		<div class="key plain K_H">
			<kbd>h</kbd>
			<samp>h</samp>
		</div>
		<div class="key plain K_J">
			<kbd>j</kbd>
			<samp>j</samp>
		</div>
		<div class="key plain K_K">
			<kbd>k</kbd>
			<samp>k</samp>
		</div>
		<div class="key plain K_L">
			<kbd>l</kbd>
			<samp>l</samp>
		</div>
		<div class="key plain K_COLON">
			<kbd>;</kbd>
			<samp>;</samp>
		</div>
		<div class="key plain K_QUOTE">
			<kbd>'</kbd>
			<samp>'</samp>
		</div>
		<div class="key special K_ENTER">
			<kbd>Enter</kbd>
		</div>
		<br style="clear: left">
		<div class="key special K_SHIFTL">
			<kbd>Shift</kbd>
		</div>
		<div class="key plain K_oE2">
			<kbd>\</kbd>
		</div>
		<div class="key plain K_Z">
			<kbd>z</kbd>
			<samp>z</samp>
		</div>
		<div class="key plain K_X">
			<kbd>x</kbd>
			<samp>ḫ</samp>
		</div>
		<div class="key plain K_C">
			<kbd>c</kbd>
			<samp>c</samp>
		</div>
		<div class="key plain K_V">
			<kbd>v</kbd>
			<samp>v</samp>
		</div>
		<div class="key plain K_B">
			<kbd>b</kbd>
			<samp>b</samp>
		</div>
		<div class="key plain K_N">
			<kbd>n</kbd>
			<samp>n</samp>
		</div>
		<div class="key plain K_M">
			<kbd>m</kbd>
			<samp>m</samp>
		</div>
		<div class="key plain K_COMMA">
			<kbd>,</kbd>
			<samp>,</samp>
		</div>
		<div class="key plain K_PERIOD">
			<kbd>.</kbd>
			<samp>.</samp>
		</div>
		<div class="key plain K_SLASH">
			<kbd>/</kbd>
			<samp>/</samp>
		</div>
		<div class="key special K_SHIFTR">
			<kbd>Shift</kbd>
		</div>
		<br style="clear: left">
		<div class="key special K_CTRLL">
			<kbd>Ctrl</kbd>
		</div>
		<div class="key special K_ALTL">
			<kbd>Alt</kbd>
		</div>
		<div class="key plain K_SPACE">
			<kbd> </kbd>
			<samp> </samp>
		</div>
		<div class="key special K_ALTR">
			<kbd>Alt</kbd>
		</div>
		<div class="key special K_CTRLR">
			<kbd>Ctrl</kbd>
		</div>
	</article>
	<article id='shift'>
		<h3 style="clear:left">Shift</h3>
		<div class="key plain K_BKQUOTE">
			<kbd>~</kbd>
			<samp>~</samp>
		</div>
		<div class="key plain K_1">
			<kbd>!</kbd>
			<samp>!</samp>
		</div>
		<div class="key plain K_2">
			<kbd>@</kbd>
			<samp>@</samp>
		</div>
		<div class="key plain K_3">
			<kbd>#</kbd>
			<samp>#</samp>
		</div>
		<div class="key plain K_4">
			<kbd>$</kbd>
			<samp>$</samp>
		</div>
		<div class="key plain K_5">
			<kbd>%</kbd>
			<samp>%</samp>
		</div>
		<div class="key plain K_6">
			<kbd>^</kbd>
			<samp>^</samp>
		</div>
		<div class="key plain K_7">
			<kbd>&amp;</kbd>
			<samp>&amp;</samp>
		</div>
		<div class="key plain K_8">
			<kbd>*</kbd>
			<samp>*</samp>
		</div>
		<div class="key plain K_9">
			<kbd>(</kbd>
			<samp>(</samp>
		</div>
		<div class="key plain K_0">
			<kbd>)</kbd>
			<samp>)</samp>
		</div>
		<div class="key plain K_HYPHEN">
			<kbd>_</kbd>
			<samp>_</samp>
		</div>
		<div class="key plain K_EQUAL">
			<kbd>+</kbd>
			<samp>+</samp>
		</div>
		<div class="key special K_BKSP">
			<kbd>Backspace</kbd>
		</div>
		<br style="clear: left">
		<div class="key special K_TAB">
			<kbd>Tab</kbd>
		</div>
		<div class="key plain K_Q">
			<kbd>Q</kbd>
			<samp>Q</samp>
		</div>
		<div class="key plain K_W">
			<kbd>W</kbd>
			<samp>W</samp>
		</div>
		<div class="key plain K_E">
			<kbd>E</kbd>
			<samp>E</samp>
		</div>
		<div class="key plain K_R">
			<kbd>R</kbd>
			<samp>R</samp>
		</div>
		<div class="key plain K_T">
			<kbd>T</kbd>
			<samp>ṯ</samp>
		</div>
		<div class="key plain K_Y">
			<kbd>Y</kbd>
			<samp>Y</samp>
		</div>
		<div class="key plain K_U">
			<kbd>U</kbd>
			<samp>U</samp>
		</div>
		<div class="key plain K_I">
			<kbd>I</kbd>
			<samp>I</samp>
		</div>
		<div class="key plain K_O">
			<kbd>O</kbd>
			<samp>O</samp>
		</div>
		<div class="key plain K_P">
			<kbd>P</kbd>
			<samp>P</samp>
		</div>
		<div class="key plain K_LBRKT">
			<kbd>{</kbd>
			<samp>{</samp>
		</div>
		<div class="key plain K_RBRKT">
			<kbd>}</kbd>
			<samp>}</samp>
		</div>
		<div class="key plain K_BKSLASH">
			<kbd>|</kbd>
			<samp>|</samp>
		</div>
		<br style="clear: left">
		<div class="key special K_CAPS">
			<kbd>Caps Lock</kbd>
		</div>
		<div class="key plain K_A">
			<kbd>A</kbd>
			<samp>ꜣ</samp>
		</div>
		<div class="key plain K_S">
			<kbd>S</kbd>
			<samp>š</samp>
		</div>
		<div class="key plain K_D">
			<kbd>D</kbd>
			<samp>ḏ</samp>
		</div>
		<div class="key plain K_F">
			<kbd>F</kbd>
			<samp>F</samp>
		</div>
		<div class="key plain K_G">
			<kbd>G</kbd>
			<samp>G</samp>
		</div>
		<div class="key plain K_H">
			<kbd>H</kbd>
			<samp>ḥ</samp>
		</div>
		<div class="key plain K_J">
			<kbd>J</kbd>
			<samp>J</samp>
		</div>
		<div class="key plain K_K">
			<kbd>K</kbd>
			<samp>K</samp>
		</div>
		<div class="key plain K_L">
			<kbd>L</kbd>
			<samp>L</samp>
		</div>
		<div class="key plain K_COLON">
			<kbd>:</kbd>
			<samp>:</samp>
		</div>
		<div class="key plain K_QUOTE">
			<kbd>"</kbd>
			<samp>"</samp>
		</div>
		<div class="key special K_ENTER">
			<kbd>Enter</kbd>
		</div>
		<br style="clear: left">
		<div class="key special K_SHIFTL">
			<kbd>Shift</kbd>
		</div>
		<div class="key plain K_oE2">
			<kbd>\</kbd>
		</div>
		<div class="key plain K_Z">
			<kbd>Z</kbd>
			<samp>Z</samp>
		</div>
		<div class="key plain K_X">
			<kbd>X</kbd>
			<samp>ẖ</samp>
		</div>
		<div class="key plain K_C">
			<kbd>C</kbd>
			<samp>C</samp>
		</div>
		<div class="key plain K_V">
			<kbd>V</kbd>
			<samp>V</samp>
		</div>
		<div class="key plain K_B">
			<kbd>B</kbd>
			<samp>B</samp>
		</div>
		<div class="key plain K_N">
			<kbd>N</kbd>
			<samp>N</samp>
		</div>
		<div class="key plain K_M">
			<kbd>M</kbd>
			<samp>M</samp>
		</div>
		<div class="key plain K_COMMA">
			<kbd>&lt;</kbd>
			<samp>&lt;</samp>
		</div>
		<div class="key plain K_PERIOD">
			<kbd>&gt;</kbd>
			<samp>&gt;</samp>
		</div>
		<div class="key plain K_SLASH">
			<kbd>?</kbd>
			<samp>?</samp>
		</div>
		<div class="key special K_SHIFTR">
			<kbd>Shift</kbd>
		</div>
		<br style="clear: left">
		<div class="key special K_CTRLL">
			<kbd>Ctrl</kbd>
		</div>
		<div class="key special K_ALTL">
			<kbd>Alt</kbd>
		</div>
		<div class="key plain K_SPACE">
			<kbd> </kbd>
			<samp> </samp>
		</div>
		<div class="key special K_ALTR">
			<kbd>Alt</kbd>
		</div>
		<div class="key special K_CTRLR">
			<kbd>Ctrl</kbd>
		</div>
	</article>
  </section>
  <section id='details'>
	<h2>Keyboard Details</h2>
	<p>This keyboard can be used to type in three ways:</p>
	<ul>
		<li>Transliteration using MdC</li>
		<li>Hieroglyphs using MdC</li>
		<li>Hieroglyphs using Gardiner numbers</li>
	</ul>
	<h3>Typing Transliteration</h3>
	<p>The keyboard automatically outputs the correct MdC transliteration when typing.</p>
	<p class='example'><b>Example:</b> type <kbd>a</kbd><kbd>n</kbd><kbd>x</kbd> to see <samp>ꜥ</samp><samp>n</samp><samp>ḫ</samp>.</p>

	<p>To add a space between transliterated words use <kbd>Ctrl</kbd>+<kbd>Spacebar</kbd>. This prevents transliteration from converting into hieroglyphs.</p>

	<p>To get the Latin letter value of a key instead of MdC transliteration, type <kbd>`</kbd>  before the key.</p>
	<p class='example'><b>Example:</b> type <kbd>`</kbd><kbd>A</kbd> to get <samp>A</samp> instead of <samp>ꜣ</samp>.</p>

	<p>To output <samp>`</samp>, type <kbd>`</kbd><kbd>`</kbd>.</p>

	<h3 class='break'>Typing Hieroglyphs Using MdC</h3>
	<p>MdC transliteration converts to hieroglyphs automatically when you press <kbd>Spacebar</kbd>.</p>
	<p class='example'><b>Example:</b> type <kbd>H</kbd><kbd>t</kbd><kbd>p</kbd> to see <samp>ḥtp</samp>. Press <kbd>Spacebar</kbd> to get <samp class='hiero'>𓊵</samp>.</p>

	<p>Some phonetic values match more than one sign. Pressing <kbd>Spacebar</kbd> repeatedly cycles through signs with identical phonetic values.</p>

	<p class='example'><b>Example:</b> type <kbd>d</kbd><kbd>i</kbd><kbd>Spacebar</kbd> to get <samp class='hiero'>𓏙</samp>. Press <kbd>Spacebar</kbd> again to get <samp class='hiero'>𓂞</samp>.</p>

	<p>To add a space after a sign use <kbd>Ctrl</kbd>+<kbd>Spacebar</kbd>.</p>

	<h3>Typing Hieroglyphs Using Gardiner Numbers</h3>
	<p>This keyboard also converts Gardiner numbers into hieroglyphs when you press <kbd>Spacebar</kbd>.</p>
	<p class='example'><b>Example:</b> type <kbd>a</kbd><kbd>3</kbd><kbd>9</kbd> to get <samp class='hiero'>𓀬</samp>.</p>
	<p>The keyboard relies on the keys you press not the transliteration output in converting Gardiner numbers into hieroglyphs.</p>
    <p class='example'><b>Example:</b> typing <kbd>X</kbd><kbd>3</kbd> gives <samp>ẖ</samp><samp>3</samp>. Pressing <kbd>Spacebar</kbd> gives <samp class='hiero'>𓏑</samp>, which is the correct output for Gardiner number X3.</p>
	<p>Either upper or lowercase letters can be used to begin a Gardiner number, but the second letter in the AA sign list must be lowercase to prevent confusion with the A sign list.</p>
	<p class='example'><b>Example:</b> type <kbd>A</kbd><kbd>a</kbd><kbd>6</kbd> or <kbd>a</kbd><kbd>a</kbd><kbd>6</kbd> to get <samp class='hiero'>𓐒</samp>.</p>
	<p>Pressing <kbd>Spacebar</kbd> repeatedly <b>will not</b> cycle through signs created from Gardiner numbers.</p>
	<p>To add a space after a sign use <kbd>Ctrl</kbd>+<kbd>Spacebar</kbd>.</p>
  </section>
  <section id='guides'>
	<h2 class='break'>Typing Guides</h2>
	<div id='guidetrans'>
		<h3>Transliteration</h3>
		<p>This keyboard uses the MdC transliteration standard as follows:</p>
		<table class='grid colleft'>
			<col class='trans' />
			<col class='keys' />
			<col class='signs' />
		<thead>
			<tr>
				<th>Trans.</th><th>Key</th><th>Signs</th>
			</tr>
		</thead>
		<tbody>
			<tr>
				<td><samp>ꜣ</samp></td><td><kbd>A</kbd></td><td><samp class='hiero'>𓄿</samp></td>
			</tr>
			<tr>
				<td><samp>i</samp></td><td><kbd>i</kbd></td><td><samp class='hiero'>𓇋</samp></td>
			</tr>
			<tr>
				<td><samp>y</samp></td><td><kbd>y</kbd></td><td><samp class='hiero'>𓏭 </samp></td>
			</tr>
			<tr>
				<td><samp>ꜥ</samp></td><td><kbd>a</kbd></td><td><samp class='hiero'>𓂝</samp></td>
			</tr>
			<tr>
				<td><samp>w</samp></td><td><kbd>w</kbd></td><td><samp class='hiero'>𓅱</samp></td>
			</tr>
			<tr>
				<td><samp>W</samp></td><td><kbd>W</kbd></td><td><samp class='hiero'>𓏲</samp></td>
			</tr>
			<tr>
				<td><samp>b</samp></td><td><kbd>b</kbd></td><td><samp class='hiero'>𓃀</samp></td>
			</tr>
			<tr>
				<td><samp>p</samp></td><td><kbd>p</kbd></td><td><samp class='hiero'>𓊪</samp></td>
			</tr>
			<tr>
				<td><samp>f</samp></td><td><kbd>f</kbd></td><td><samp class='hiero'>𓆑</samp></td>
			</tr>
			<tr>
				<td><samp>m</samp></td><td><kbd>m</kbd></td><td><samp class='hiero'>𓅓</samp></td>
			</tr>
		</tbody>
		</table>

		<table class='grid colright'>
			<col class='trans' />
			<col class='keys' />
			<col class='signs' />
		<thead>
			<tr>
				<th>Trans.</th><th>Key</th><th>Signs</th>
			</tr>
		</thead>
		<tbody>
			<tr>
				<td><samp>M</samp></td><td><kbd>M</kbd></td><td><samp class='hiero'>𓐝</samp></td>
			</tr>
			<tr>
				<td><samp>n</samp></td><td><kbd>n</kbd></td><td><samp class='hiero'>𓈖</samp></td>
			</tr>
			<tr>
				<td><samp>N</samp></td><td><kbd>N</kbd></td><td><samp class='hiero'>𓋔</samp></td>
			</tr>
			<tr>
				<td><samp>r</samp></td><td><kbd>r</kbd></td><td><samp class='hiero'>𓂋</samp></td>
			</tr>
			<tr>
				<td><samp>l</samp></td><td><kbd>l</kbd></td><td><samp class='hiero'>𓃭</samp></td>
			</tr>
			<tr>
				<td><samp>h</samp></td><td><kbd>h</kbd></td><td><samp class='hiero'>𓉔</samp></td>
			</tr>
			<tr>
				<td><samp>ḥ</samp></td><td><kbd>H</kbd></td><td><samp class='hiero'>𓎛</samp></td>
			</tr>
			<tr>
				<td><samp>ḫ</samp></td><td><kbd>x</kbd></td><td><samp class='hiero'>𓐍</samp></td>
			</tr>
			<tr>
				<td><samp>ẖ</samp></td><td><kbd>X</kbd></td><td><samp class='hiero'>𓄡</samp></td>
			</tr>
		</tbody>
		</table>

		<table class='grid colright'>
			<col class='trans' />
			<col class='keys' />
			<col class='signs' />
		<thead>
			<tr>
				<th>Trans.</th><th>Key</th><th>Signs</th>
			</tr>
		</thead>
		<tbody>
			<tr>
				<td><samp>z</samp></td><td><kbd>z</kbd></td><td><samp class='hiero'>𓊃</samp></td>
			</tr>
			<tr>
				<td><samp>s</samp></td><td><kbd>s</kbd></td><td><samp class='hiero'>𓋴</samp></td>
			</tr>
			<tr>
				<td><samp>š</samp></td><td><kbd>S</kbd></td><td><samp class='hiero'>𓈙</samp></td>
			</tr>
			<tr>
				<td><samp>ḳ</samp></td><td><kbd>q</kbd></td><td><samp class='hiero'>𓈎</samp></td>
			</tr>
			<tr>
				<td><samp>k</samp></td><td><kbd>k</kbd></td><td><samp class='hiero'>𓎡</samp></td>
			</tr>
			<tr>
				<td><samp>g</samp></td><td><kbd>g</kbd></td><td><samp class='hiero'>𓎼</samp></td>
			</tr>
			<tr>
				<td><samp>t</samp></td><td><kbd>t</kbd></td><td><samp class='hiero'>𓏏</samp></td>
			</tr>
			<tr>
				<td><samp>ṯ</samp></td><td><kbd>T</kbd></td><td><samp class='hiero'>𓍿</samp></td>
			</tr>
			<tr>
				<td><samp>d</samp></td><td><kbd>d</kbd></td><td><samp class='hiero'>𓂧</samp></td>
			</tr>
			<tr>
				<td><samp>ḏ</samp></td><td><kbd>D</kbd></td><td><samp class='hiero'>𓆓</samp></td>
			</tr>
		</tbody>
		</table>
	</div>
	<div id='guidemdc' class='break'>
		<h3>Hieroglyphs Using MdC</h3>
		<p>This keyboard cycles through the following hieroglyphs when you type a <kbd>Spacebar</kbd> after the transliteration/keys shown:</p>
		<table class='grid colleft'>
			<col class='keys' />
			<col class='trans' />
			<col class='signs' />
		<thead>
			<tr>
				<th>Keys</th><th>Trans.</th><th>Signs</th>
			</tr>
		</thead>
		<tbody>
			<tr class='headrow'>
				<td colspan='3'>1</td>
			</tr>
			<tr>
				<td><kbd>1</kbd></td><td><samp>1</samp></td><td><samp class='hiero'>𓏤</samp></td>
			</tr>
			<tr>
				<td><kbd>1</kbd><kbd>0</kbd></td><td><samp>10</samp></td><td><samp class='hiero'>𓎆</samp></td>
			</tr>
			<tr>
				<td><kbd>1</kbd><kbd>0</kbd><kbd>0</kbd></td><td><samp>100</samp></td><td><samp class='hiero'>𓍢</samp></td>
			</tr>
			<tr>
				<td><kbd>1</kbd><kbd>0</kbd><kbd>0</kbd><kbd>0</kbd></td><td><samp>1000</samp></td><td><samp class='hiero'>𓆼</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>3</td>
			</tr>
			<tr>
				<td><kbd>3</kbd></td><td><samp>3</samp></td><td><samp class='hiero'>𓏥<span class='cyc'>&rarr;</span>𓈓<span class='cyc'>&rarr;</span>𓏪<span class='cyc'>&rarr;</span>𓏫</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'><span class='gardiner'>ꜣ</span> (A)</td>
			</tr>
			<tr>
				<td><kbd>A</kbd></td><td><samp>&#xa723;</samp></td><td><samp class='hiero'>&#x1313F;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>A</kbd></td><td><samp>&#xa723;&#xa723;</samp></td><td><samp class='hiero'>&#x13130;<span class='cyc'>&rarr;</span>&#x13140;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>T</kbd><kbd>p</kbd></td><td><samp>&#xa723;&#x1e6f;p</samp></td><td><samp class='hiero'>&#x1300B;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>b</kbd></td><td><samp>&#xa723;b</samp></td><td><samp class='hiero'>&#x1311C;<span class='cyc'>&rarr;</span>&#x132C1;<span class='cyc'>&rarr;</span>&#x1334B;<span class='cyc'>&rarr;</span>&#x133B6;<span class='cyc'>&rarr;</span>&#x133B7;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>b</kbd><kbd>d</kbd></td><td><samp>&#xa723;bd</samp></td><td><samp class='hiero'>&#x131F9;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>b</kbd><kbd>y</kbd></td><td><samp>&#xa723;by</samp></td><td><samp class='hiero'>&#x130EE;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>i</kbd></td><td><samp>&#xa723;i</samp></td><td><samp class='hiero'>&#x13317;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>m</kbd><kbd>m</kbd></td><td><samp>&#xa723;mm</samp></td><td><samp class='hiero'>&#x130AC;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>m</kbd><kbd>s</kbd></td><td><samp>&#xa723;ms</samp></td><td><samp class='hiero'>&#x130D6;<span class='cyc'>&rarr;</span>&#x13304;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>q</kbd><kbd>H</kbd><kbd>w</kbd></td><td><samp>&#xa723;&#x1e33;&#x1e25;w</samp></td><td><samp class='hiero'>&#x1330F;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>r</kbd></td><td><samp>&#xa723;r</samp></td><td><samp class='hiero'>&#x13317;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>t</kbd><kbd>f</kbd></td><td><samp>&#xa723;tf</samp></td><td><samp class='hiero'>&#x132DA;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>t</kbd><kbd>p</kbd></td><td><samp>&#xa723;tp</samp></td><td><samp class='hiero'>&#x1300B;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>w</kbd></td><td><samp>&#xa723;w</samp></td><td><samp class='hiero'>&#x1312B;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>x</kbd></td><td><samp>&#xa723;&#x1e2b;</samp></td><td><samp class='hiero'>&#x1315C;<span class='cyc'>&rarr;</span>&#x131C7;</samp></td>
			</tr>
			<tr>
				<td><kbd>A</kbd><kbd>x</kbd><kbd>t</kbd></td><td><samp>&#xa723;&#x1e2b;t</samp></td><td><samp class='hiero'>&#x131B7;<span class='cyc'>&rarr;</span>&#x1320C;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'><span class='gardiner'>ꜥ</span> (a)</td>
			</tr>
			<tr>
				<td><kbd>a</kbd></td><td><samp>&#xa725;</samp></td><td><samp class='hiero'>&#x1309D;<span class='cyc'>&rarr;</span>&#x1327B;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>A</kbd></td><td><samp>&#xa725;&#xa723;</samp></td><td><samp class='hiero'>&#x130D8;<span class='cyc'>&rarr;</span>&#x1327B;<span class='cyc'>&rarr;</span>&#x1327F;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>A</kbd><kbd>m</kbd><kbd>w</kbd></td><td><samp>&#xa725;&#xa723;mw</samp></td><td><samp class='hiero'>&#x1303A;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>D</kbd></td><td><samp>&#xa725;&#x1e0f;</samp></td><td><samp class='hiero'>&#x1319D;<span class='cyc'>&rarr;</span>&#x13399;<span class='cyc'>&rarr;</span>&#x1339A;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>H</kbd></td><td><samp>&#xa725;&#x1e25;</samp></td><td><samp class='hiero'>&#x13265;<span class='cyc'>&rarr;</span>&#x13266;<span class='cyc'>&rarr;</span>&#x13324;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>H</kbd><kbd>A</kbd></td><td><samp>&#xa725;&#x1e25;&#xa723;</samp></td><td><samp class='hiero'>&#x1309A;<span class='cyc'>&rarr;</span>&#x1309B;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>H</kbd><kbd>a</kbd></td><td><samp>&#xa725;&#x1e25;&#xa725;</samp></td><td><samp class='hiero'>&#x132A2;<span class='cyc'>&rarr;</span>&#x132A3;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>S</kbd></td><td><samp>&#xa725;&#x0161;</samp></td><td><samp class='hiero'>&#x1301E;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>S</kbd><kbd>A</kbd></td><td><samp>&#xa725;&#x0161;&#xa723;</samp></td><td><samp class='hiero'>&#x13188;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>b</kbd></td><td><samp>&#xa725;b</samp></td><td><samp class='hiero'>&#x130C1;<span class='cyc'>&rarr;</span>&#x13410;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>b</kbd><kbd>A</kbd></td><td><samp>&#xa725;b&#xa723;</samp></td><td><samp class='hiero'>&#x13302;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>d</kbd></td><td><samp>&#xa725;d</samp></td><td><samp class='hiero'>&#x1319D;<span class='cyc'>&rarr;</span>&#x13399;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>f</kbd><kbd>t</kbd><kbd>i</kbd></td><td><samp>&#xa725;fti</samp></td><td><samp class='hiero'>&#x13029;<span class='cyc'>&rarr;</span>&#x1302A;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>n</kbd></td><td><samp>&#xa725;n</samp></td><td><samp class='hiero'>&#x1307C;<span class='cyc'>&rarr;</span>&#x1307D;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>n</kbd><kbd>n</kbd></td><td><samp>&#xa725;nn</samp></td><td><samp class='hiero'>&#x130BD;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>n</kbd><kbd>t</kbd></td><td><samp>&#xa725;nt</samp></td><td><samp class='hiero'>&#x130B7;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>n</kbd><kbd>w</kbd></td><td><samp>&#xa725;nw</samp></td><td><samp class='hiero'>&#x13023;<span class='cyc'>&rarr;</span>&#x1307D;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>n</kbd><kbd>x</kbd></td><td><samp>&#xa725;n&#x1e2b;</samp></td><td><samp class='hiero'>&#x13114;<span class='cyc'>&rarr;</span>&#x132F9;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>p</kbd><kbd>r</kbd></td><td><samp>&#xa725;pr</samp></td><td><samp class='hiero'>&#x13422;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>q</kbd></td><td><samp>&#xa725;&#x1e33;</samp></td><td><samp class='hiero'>&#x13167;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>r</kbd><kbd>q</kbd></td><td><samp>&#xa725;r&#x1e33;</samp></td><td><samp class='hiero'>&#x1337C;</samp></td>
			</tr>
			<tr>
				<td><kbd>a</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>&#xa725;wt</samp></td><td><samp class='hiero'>&#x132FF;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>b</td>
			</tr>
			<tr>
				<td><kbd>b</kbd></td><td><samp>b</samp></td><td><samp class='hiero'>&#x130C0;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>b</kbd><kbd>A</kbd></td><td rowspan='2'><samp>b&#xa723;</samp></td><td><samp class='hiero'>&#x130DD;<span class='cyc'>&rarr;</span>&#x130DE;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13161;<span class='cyc'>&rarr;</span>&#x1317D;<span class='cyc'>&rarr;</span>&#x133BB;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>A</kbd><kbd>H</kbd></td><td><samp>b&#xa723;&#x1e25;</samp></td><td><samp class='hiero'>&#x130BA;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>A</kbd><kbd>s</kbd></td><td><samp>b&#xa723;s</samp></td><td><samp class='hiero'>&#x133B0;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>H</kbd></td><td><samp>b&#x1e25;</samp></td><td><samp class='hiero'>&#x13111;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>H</kbd><kbd>s</kbd></td><td><samp>b&#x1e25;s</samp></td><td><samp class='hiero'>&#x130D4;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>S</kbd><kbd>i</kbd></td><td><samp>b&#x0161;i</samp></td><td><samp class='hiero'>&#x13090;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>a</kbd><kbd>H</kbd><kbd>i</kbd></td><td><samp>b&#xa725;&#x1e25;i</samp></td><td><samp class='hiero'>&#x13164;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>d</kbd></td><td><samp>bd</samp></td><td><samp class='hiero'>&#x132BA;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>d</kbd><kbd>t</kbd></td><td><samp>bdt</samp></td><td><samp class='hiero'>&#x131E3;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>i</kbd><kbd>A</kbd></td><td><samp>bi&#xa723;</samp></td><td><samp class='hiero'>&#x13111;<span class='cyc'>&rarr;</span>&#x13214;<span class='cyc'>&rarr;</span>&#x1321E;<span class='cyc'>&rarr;</span>&#x13344;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>i</kbd><kbd>t</kbd></td><td><samp>bit</samp></td><td><samp class='hiero'>&#x131A4;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>i</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>bity</samp></td><td><samp class='hiero'>&#x13035;<span class='cyc'>&rarr;</span>&#x13037;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>n</kbd><kbd>r</kbd></td><td><samp>bnr</samp></td><td><samp class='hiero'>&#x131DC;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>w</kbd></td><td><samp>bw</samp></td><td><samp class='hiero'>&#x130C0;<span class='cyc'>&rarr;</span>&#x1319C;</samp></td>
			</tr>
			<tr>
				<td><kbd>b</kbd><kbd>z</kbd></td><td><samp>bz</samp></td><td><samp class='hiero'>&#x1319F;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd></td><td><samp>d</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x130A7;<span class='cyc'>&rarr;</span>&#x13193;<span class='cyc'>&rarr;</span>&#x133D9;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>D</td>
			</tr>
			<tr>
				<td><kbd>D</kbd></td><td><samp>&#x1e0f;</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x13193;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>A</kbd></td><td><samp>&#x1e0f;&#xa723;</samp></td><td><samp class='hiero'>&#x1329E;<span class='cyc'>&rarr;</span>&#x13351;<span class='cyc'>&rarr;</span>&#x13352;<span class='cyc'>&rarr;</span>&#x13416;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>D</kbd></td><td><samp>&#x1e0f;&#x1e0f;</samp></td><td><samp class='hiero'>&#x13195;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>H</kbd><kbd>w</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>&#x1e0f;&#x1e25;wty</samp></td><td><samp class='hiero'>&#x1305F;<span class='cyc'>&rarr;</span>&#x1315D;<span class='cyc'>&rarr;</span>&#x1315E;<span class='cyc'>&rarr;</span>&#x133D0;<span class='cyc'>&rarr;</span>&#x133D1;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>a</kbd><kbd>m</kbd></td><td><samp>&#x1e0f;&#xa725;m</samp></td><td><samp class='hiero'>&#x132E1;<span class='cyc'>&rarr;</span>&#x13300;<span class='cyc'>&rarr;</span>&#x13301;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>b</kbd></td><td><samp>&#x1e0f;b</samp></td><td><samp class='hiero'>&#x13159;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>b</kbd><kbd>A</kbd></td><td><samp>&#x1e0f;b&#xa723;</samp></td><td><samp class='hiero'>&#x13325;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>b</kbd><kbd>a</kbd></td><td><samp>&#x1e0f;b&#xa725;</samp></td><td><samp class='hiero'>&#x130AD;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>d</kbd></td><td><samp>&#x1e0f;d</samp></td><td><samp class='hiero'>&#x132BD;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>f</kbd><kbd>D</kbd></td><td><samp>&#x1e0f;f&#x1e0f;</samp></td><td><samp class='hiero'>&#x13082;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>r</kbd></td><td><samp>&#x1e0f;r</samp></td><td><samp class='hiero'>&#x131E5;<span class='cyc'>&rarr;</span>&#x131E6;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>r</kbd><kbd>D</kbd></td><td><samp>&#x1e0f;r&#x1e0f;</samp></td><td><samp class='hiero'>&#x13114;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>r</kbd><kbd>t</kbd></td><td><samp>&#x1e0f;rt</samp></td><td><samp class='hiero'>&#x130A7;<span class='cyc'>&rarr;</span>&#x130A9;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>s</kbd><kbd>r</kbd></td><td><samp>&#x1e0f;sr</samp></td><td><samp class='hiero'>&#x130A6;</samp></td>
			</tr>
			<tr>
				<td><kbd>D</kbd><kbd>w</kbd></td><td><samp>&#x1e0f;w</samp></td><td><samp class='hiero'>&#x1320B;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>d</td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>S</kbd><kbd>r</kbd></td><td><samp>d&#x0161;r</samp></td><td><samp class='hiero'>&#x1315F;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>S</kbd><kbd>r</kbd><kbd>t</kbd></td><td><samp>d&#x0161;rt</samp></td><td><samp class='hiero'>&#x132D4;<span class='cyc'>&rarr;</span>&#x133BE;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>b</kbd></td><td><samp>db</samp></td><td><samp class='hiero'>&#x130EF;<span class='cyc'>&rarr;</span>&#x1310F;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>d</kbd></td><td><samp>dd</samp></td><td><samp class='hiero'>&#x132BD;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>i</kbd></td><td><samp>di</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x133D9;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>m</kbd><kbd>D</kbd></td><td><samp>dm&#x1e0f;</samp></td><td><samp class='hiero'>&#x132EC;<span class='cyc'>&rarr;</span>&#x133DB;<span class='cyc'>&rarr;</span>&#x133DD;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>q</kbd><kbd>r</kbd></td><td><samp>d&#x1e33;r</samp></td><td><samp class='hiero'>&#x130B7;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>w</kbd><kbd>A</kbd></td><td><samp>dw&#xa723;</samp></td><td><samp class='hiero'>&#x13003;<span class='cyc'>&rarr;</span>&#x13022;<span class='cyc'>&rarr;</span>&#x131FC;</samp></td>
			</tr>
			<tr>
				<td><kbd>d</kbd><kbd>w</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>dw&#xa723;t</samp></td><td><samp class='hiero'>&#x131FD;</samp></td>
			</tr>


			<tr class='headrow'>
				<td colspan='3'>f</td>
			</tr>
			<tr>
				<td><kbd>f</kbd></td><td><samp>f</samp></td><td><samp class='hiero'>&#x13191;</samp></td>
			</tr>
			<tr>
				<td><kbd>f</kbd><kbd>A</kbd></td><td><samp>f&#xa723;</samp></td><td><samp class='hiero'>&#x1300B;</samp></td>
			</tr>
			<tr>
				<td><kbd>f</kbd><kbd>A</kbd><kbd>i</kbd></td><td><samp>f&#xa723;i</samp></td><td><samp class='hiero'>&#x1300B;</samp></td>
			</tr>
			<tr>
				<td><kbd>f</kbd><kbd>n</kbd><kbd>D</kbd></td><td><samp>fn&#x1e0f;</samp></td><td><samp class='hiero'>&#x13089;<span class='cyc'>&rarr;</span>&#x1308A;</samp></td>
			</tr>
			<tr>
				<td><kbd>f</kbd><kbd>n</kbd><kbd>d</kbd></td><td><samp>fnd</samp></td><td><samp class='hiero'>&#x13089;<span class='cyc'>&rarr;</span>&#x1308A;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>g</td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>g</kbd></td><td rowspan='2'><samp>g</samp></td><td><samp class='hiero'>&#x13371;<span class='cyc'>&rarr;</span>&#x133A4;<span class='cyc'>&rarr;</span>&#x133A6;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x133A7;<span class='cyc'>&rarr;</span>&#x133BC;<span class='cyc'>&rarr;</span>&#x133BD;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>H</kbd></td><td><samp>g&#x1e25;</samp></td><td><samp class='hiero'>&#x130BE;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>H</kbd><kbd>s</kbd></td><td><samp>g&#x1e25;s</samp></td><td><samp class='hiero'>&#x130BE;<span class='cyc'>&rarr;</span>&#x130F4;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>b</kbd></td><td><samp>gb</samp></td><td><samp class='hiero'>&#x1316C;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>b</kbd><kbd>A</kbd></td><td><samp>gb&#xa723;</samp></td><td><samp class='hiero'>&#x130A2;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>m</kbd></td><td><samp>gm</samp></td><td><samp class='hiero'>&#x13160;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>r</kbd><kbd>g</kbd></td><td><samp>grg</samp></td><td><samp class='hiero'>&#x13345;<span class='cyc'>&rarr;</span>&#x13346;</samp></td>
			</tr>
			<tr>
				<td><kbd>g</kbd><kbd>s</kbd></td><td><samp>gs</samp></td><td><samp class='hiero'>&#x1341B;<span class='cyc'>&rarr;</span>&#x1341C;<span class='cyc'>&rarr;</span>&#x1341D;<span class='cyc'>&rarr;</span>&#x1341E;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>ḥ (H)</td>
			</tr>
			<tr>
				<td><kbd>H</kbd></td><td><samp>&#x1e25;</samp></td><td><samp class='hiero'>&#x1339B;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>A</kbd></td><td><samp>&#x1e25;&#xa723;</samp></td><td><samp class='hiero'>&#x131C9;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>A</kbd><kbd>i</kbd></td><td><samp>&#x1e25;&#xa723;i</samp></td><td><samp class='hiero'>&#x13020;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>&#x1e25;&#xa723;t</samp></td><td><samp class='hiero'>&#x13102;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>D</kbd></td><td><samp>&#x1e25;&#x1e0f;</samp></td><td><samp class='hiero'>&#x132E0;<span class='cyc'>&rarr;</span>&#x13309;<span class='cyc'>&rarr;</span>&#x1330B;<span class='cyc'>&rarr;</span>&#x1330C;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>D</kbd><kbd>D</kbd></td><td><samp>&#x1e25;&#x1e0f;&#x1e0f;</samp></td><td><samp class='hiero'>&#x1330D;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>D</kbd><kbd>t</kbd></td><td><samp>&#x1e25;&#x1e0f;t</samp></td><td><samp class='hiero'>&#x132D1;<span class='cyc'>&rarr;</span>&#x132D2;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>H</kbd></td><td><samp>&#x1e25;&#x1e25;</samp></td><td><samp class='hiero'>&#x13068;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>a</kbd><kbd>i</kbd></td><td><samp>&#x1e25;&#xa725;i</samp></td><td><samp class='hiero'>&#x13020;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>b</kbd></td><td><samp>&#x1e25;b</samp></td><td><samp class='hiero'>&#x133B1;<span class='cyc'>&rarr;</span>&#x133B3;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>f</kbd><kbd>n</kbd></td><td><samp>&#x1e25;fn</samp></td><td><samp class='hiero'>&#x13190;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>m</kbd></td><td><samp>&#x1e25;m</samp></td><td><samp class='hiero'>&#x1302F;<span class='cyc'>&rarr;</span>&#x13030;<span class='cyc'>&rarr;</span>&#x1321E;<span class='cyc'>&rarr;</span>&#x1321F;<span class='cyc'>&rarr;</span>&#x1335B;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>m</kbd><kbd>s</kbd></td><td><samp>&#x1e25;ms</samp></td><td><samp class='hiero'>&#x13002;<span class='cyc'>&rarr;</span>&#x13015;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>m</kbd><kbd>s</kbd><kbd>i</kbd></td><td><samp>&#x1e25;msi</samp></td><td><samp class='hiero'>&#x13002;<span class='cyc'>&rarr;</span>&#x13015;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>m</kbd><kbd>t</kbd></td><td><samp>&#x1e25;mt</samp></td><td><samp class='hiero'>&#x13214;<span class='cyc'>&rarr;</span>&#x1334D;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>n</kbd></td><td><samp>&#x1e25;n</samp></td><td><samp class='hiero'>&#x131B0;<span class='cyc'>&rarr;</span>&#x1333C;<span class='cyc'>&rarr;</span>&#x133A8;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>n</kbd><kbd>k</kbd></td><td><samp>&#x1e25;nk</samp></td><td><samp class='hiero'>&#x130A0;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>n</kbd><kbd>m</kbd><kbd>m</kbd><kbd>t</kbd></td><td><samp>&#x1e25;nmmt</samp></td><td><samp class='hiero'>&#x131F6;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>n</kbd><kbd>q</kbd><kbd>t</kbd></td><td><samp>&#x1e25;n&#x1e33;t</samp></td><td><samp class='hiero'>&#x133CA;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>p</kbd></td><td><samp>&#x1e25;p</samp></td><td><samp class='hiero'>&#x13411;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>p</kbd><kbd>t</kbd></td><td><samp>&#x1e25;pt</samp></td><td><samp class='hiero'>&#x13098;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>q</kbd><kbd>A</kbd></td><td><samp>&#x1e25;&#x1e33;&#xa723;</samp></td><td><samp class='hiero'>&#x132FE;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>q</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>&#x1e25;&#x1e33;&#xa723;t</samp></td><td><samp class='hiero'>&#x132FE;<span class='cyc'>&rarr;</span>&#x1333F;<span class='cyc'>&rarr;</span>&#x13340;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>r</kbd></td><td><samp>&#x1e25;r</samp></td><td><samp class='hiero'>&#x13077;<span class='cyc'>&rarr;</span>&#x13421;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>s</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>&#x1e25;s&#xa723;t</samp></td><td><samp class='hiero'>&#x130D5;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>t</kbd><kbd>p</kbd></td><td><samp>&#x1e25;tp</samp></td><td><samp class='hiero'>&#x132B5;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>w</kbd></td><td><samp>&#x1e25;w</samp></td><td><samp class='hiero'>&#x13111;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>w</kbd><kbd>i</kbd></td><td><samp>&#x1e25;wi</samp></td><td><samp class='hiero'>&#x1301C;<span class='cyc'>&rarr;</span>&#x1301D;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>&#x1e25;wt</samp></td><td><samp class='hiero'>&#x13257;<span class='cyc'>&rarr;</span>&#x1325E;</samp></td>
			</tr>
			<tr>
				<td><kbd>H</kbd><kbd>z</kbd></td><td><samp>&#x1e25;z</samp></td><td><samp class='hiero'>&#x133BF;</samp></td>
			</tr>


			<tr class='headrow'>
				<td colspan='3'>h</td>
			</tr>
			<tr>
				<td><kbd>h</kbd></td><td><samp>h</samp></td><td><samp class='hiero'>&#x13254;</samp></td>
			</tr>
			<tr>
				<td><kbd>h</kbd><kbd>b</kbd></td><td><samp>hb</samp></td><td><samp class='hiero'>&#x1315D;<span class='cyc'>&rarr;</span>&#x1315E;<span class='cyc'>&rarr;</span>&#x13341;</samp></td>
			</tr>
			<tr>
				<td><kbd>h</kbd><kbd>n</kbd><kbd>w</kbd></td><td><samp>hnw</samp></td><td><samp class='hiero'>&#x1300A;</samp></td>
			</tr>
			<tr>
				<td><kbd>h</kbd><kbd>r</kbd><kbd>w</kbd></td><td><samp>hrw</samp></td><td><samp class='hiero'>&#x131F3;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>i</td>
			</tr>
			<tr>
				<td><kbd>i</kbd></td><td><samp>i</samp></td><td><samp class='hiero'>&#x13000;<span class='cyc'>&rarr;</span>&#x1301E;<span class='cyc'>&rarr;</span>&#x131B0;<span class='cyc'>&rarr;</span>&#x131CB;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>T</kbd></td><td><samp>i&#xa723;&#x1e6f;</samp></td><td><samp class='hiero'>&#x130BF;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>b</kbd></td><td><samp>i&#xa723;b</samp></td><td><samp class='hiero'>&#x132C1;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>d</kbd><kbd>t</kbd></td><td><samp>i&#xa723;dt</samp></td><td><samp class='hiero'>&#x131F2;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>m</kbd></td><td><samp>i&#xa723;m</samp></td><td><samp class='hiero'>&#x131AD;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>s</kbd></td><td><samp>i&#xa723;s</samp></td><td><samp class='hiero'>&#x13020;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>i&#xa723;t</samp></td><td><samp class='hiero'>&#x130BF;<span class='cyc'>&rarr;</span>&#x13126;<span class='cyc'>&rarr;</span>&#x1320F;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>i&#xa723;w</samp></td><td><samp class='hiero'>&#x13017;<span class='cyc'>&rarr;</span>&#x13022;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>H</kbd></td><td><samp>i&#x1e25;</samp></td><td><samp class='hiero'>&#x130D2;<span class='cyc'>&rarr;</span>&#x130FE;<span class='cyc'>&rarr;</span>&#x13324;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>T</kbd><kbd>i</kbd></td><td><samp>i&#x1e6f;i</samp></td><td><samp class='hiero'>&#x13381;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>a</kbd><kbd>H</kbd></td><td><samp>i&#xa725;&#x1e25;</samp></td><td><samp class='hiero'>&#x131F9;<span class='cyc'>&rarr;</span>&#x131FA;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>a</kbd><kbd>b</kbd></td><td><samp>i&#xa725;b</samp></td><td><samp class='hiero'>&#x133BA;<span class='cyc'>&rarr;</span>&#x133BB;<span class='cyc'>&rarr;</span>&#x13410;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>b</kbd></td><td><samp>ib</samp></td><td><samp class='hiero'>&#x130D9;<span class='cyc'>&rarr;</span>&#x130DA;<span class='cyc'>&rarr;</span>&#x13123;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>b</kbd><kbd>A</kbd></td><td><samp>ib&#xa723;</samp></td><td><samp class='hiero'>&#x133E1;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>d</kbd></td><td><samp>id</samp></td><td><samp class='hiero'>&#x13130;<span class='cyc'>&rarr;</span>&#x1321E;<span class='cyc'>&rarr;</span>&#x1321F;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>d</kbd><kbd>n</kbd></td><td><samp>idn</samp></td><td><samp class='hiero'>&#x13114;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>d</kbd><kbd>r</kbd></td><td><samp>idr</samp></td><td><samp class='hiero'>&#x133A9;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>d</kbd><kbd>t</kbd></td><td><samp>idt</samp></td><td><samp class='hiero'>&#x130A8;<span class='cyc'>&rarr;</span>&#x13130;<span class='cyc'>&rarr;</span>&#x131F2;<span class='cyc'>&rarr;</span>&#x1321E;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>i</kbd></td><td><samp>ii</samp></td><td><samp class='hiero'>&#x131CD;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>k</kbd><kbd>w</kbd></td><td><samp>ikw</samp></td><td><samp class='hiero'>&#x13017;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>m</kbd></td><td><samp>im</samp></td><td><samp class='hiero'>&#x131AD;<span class='cyc'>&rarr;</span>&#x1341B;<span class='cyc'>&rarr;</span>&#x1341C;<span class='cyc'>&rarr;</span>&#x1341D;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>m</kbd><kbd>A</kbd><kbd>x</kbd></td><td><samp>im&#xa723;&#x1e2b;</samp></td><td><samp class='hiero'>&#x1312A;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>m</kbd><kbd>i</kbd></td><td><samp>imi</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x133F6;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>m</kbd><kbd>n</kbd></td><td><samp>imn</samp></td><td><samp class='hiero'>&#x13003;<span class='cyc'>&rarr;</span>&#x13004;<span class='cyc'>&rarr;</span>&#x13069;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>m</kbd><kbd>n</kbd><kbd>t</kbd></td><td><samp>imnt</samp></td><td><samp class='hiero'>&#x132BF;<span class='cyc'>&rarr;</span>&#x132C0;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>n</kbd></td><td><samp>in</samp></td><td><samp class='hiero'>&#x1301F;<span class='cyc'>&rarr;</span>&#x1319B;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>n</kbd><kbd>b</kbd></td><td><samp>inb</samp></td><td><samp class='hiero'>&#x13285;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>n</kbd><kbd>i</kbd></td><td><samp>ini</samp></td><td><samp class='hiero'>&#x133CE;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>n</kbd><kbd>p</kbd></td><td><samp>inp</samp></td><td><samp class='hiero'>&#x13016;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>n</kbd><kbd>p</kbd><kbd>w</kbd></td><td><samp>inpw</samp></td><td><samp class='hiero'>&#x13062;<span class='cyc'>&rarr;</span>&#x130E2;<span class='cyc'>&rarr;</span>&#x130E3;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>p</kbd><kbd>t</kbd></td><td><samp>ipt</samp></td><td><samp class='hiero'>&#x13292;<span class='cyc'>&rarr;</span>&#x13293;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>r</kbd></td><td><samp>ir</samp></td><td><samp class='hiero'>&#x13079;<span class='cyc'>&rarr;</span>&#x13082;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>r</kbd><kbd>t</kbd></td><td><samp>irt</samp></td><td><samp class='hiero'>&#x13079;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>r</kbd><kbd>y</kbd></td><td><samp>iry</samp></td><td><samp class='hiero'>&#x13038;<span class='cyc'>&rarr;</span>&#x13039;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>isw</samp></td><td><samp class='hiero'>&#x1312F;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>i</kbd><kbd>t</kbd></td><td rowspan='2'><samp>it</samp></td><td><samp class='hiero'>&#x1318A;<span class='cyc'>&rarr;</span>&#x13191;<span class='cyc'>&rarr;</span>&#x131E0;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x1333E;<span class='cyc'>&rarr;</span>&#x133D0;<span class='cyc'>&rarr;</span>&#x133D1;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>ity</samp></td><td><samp class='hiero'>&#x1301B;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>w</kbd></td><td><samp>iw</samp></td><td><samp class='hiero'>&#x130BB;<span class='cyc'>&rarr;</span>&#x130DB;<span class='cyc'>&rarr;</span>&#x130E1;<span class='cyc'>&rarr;</span>&#x13139;<span class='cyc'>&rarr;</span>&#x13200;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>w</kbd><kbd>a</kbd></td><td><samp>iw&#xa725;</samp></td><td><samp class='hiero'>&#x1312F;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>w</kbd><kbd>f</kbd></td><td><samp>iwf</samp></td><td><samp class='hiero'>&#x13139;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>w</kbd><kbd>n</kbd></td><td><samp>iwn</samp></td><td><samp class='hiero'>&#x1327A;</samp></td>
			</tr>
			<tr>
				<td><kbd>i</kbd><kbd>z</kbd></td><td><samp>iz</samp></td><td><samp class='hiero'>&#x131E9;<span class='cyc'>&rarr;</span>&#x13284;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>k</td>
			</tr>
			<tr>
				<td><kbd>k</kbd></td><td><samp>k</samp></td><td><samp class='hiero'>&#x133A1;<span class='cyc'>&rarr;</span>&#x133A2;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>A</kbd></td><td><samp>k&#xa723;</samp></td><td><samp class='hiero'>&#x13093;<span class='cyc'>&rarr;</span>&#x13094;<span class='cyc'>&rarr;</span>&#x130D2;<span class='cyc'>&rarr;</span>&#x130D3;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>A</kbd><kbd>p</kbd></td><td><samp>k&#xa723;p</samp></td><td><samp class='hiero'>&#x132B6;<span class='cyc'>&rarr;</span>&#x132B7;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>A</kbd><kbd>r</kbd></td><td><samp>k&#xa723;r</samp></td><td><samp class='hiero'>&#x1326C;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>k&#xa723;t</samp></td><td><samp class='hiero'>&#x1300B;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>f</kbd><kbd>A</kbd></td><td><samp>kf&#xa723;</samp></td><td><samp class='hiero'>&#x13116;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>m</kbd></td><td><samp>km</samp></td><td><samp class='hiero'>&#x1318E;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>p</kbd></td><td><samp>kp</samp></td><td><samp class='hiero'>&#x132B6;<span class='cyc'>&rarr;</span>&#x132B7;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>s</kbd></td><td><samp>ks</samp></td><td><samp class='hiero'>&#x13013;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>s</kbd><kbd>i</kbd></td><td><samp>ksi</samp></td><td><samp class='hiero'>&#x13013;</samp></td>
			</tr>
			<tr>
				<td><kbd>k</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>ksw</samp></td><td><samp class='hiero'>&#x13013;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>l</td>
			</tr>
			<tr>
				<td><kbd>l</kbd></td><td><samp>l</samp></td><td><samp class='hiero'>&#x130ED;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>M</td>
			</tr>
			<tr>
				<td><kbd>M</kbd></td><td><samp>M</samp></td><td><samp class='hiero'>&#x1341D;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>m</td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>m</kbd></td><td rowspan='2'><samp>m</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x1309F;<span class='cyc'>&rarr;</span>&#x13153;<span class='cyc'>&rarr;</span>&#x13155;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp  class='hiero'>&#x13156;<span class='cyc'>&rarr;</span>&#x1341B;<span class='cyc'>&rarr;</span>&#x1341C;<span class='cyc'>&rarr;</span>&#x1341D;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>m</kbd><kbd>A</kbd></td><td rowspan='2'><samp>m&#xa723;</samp></td><td><samp class='hiero'>&#x13079;<span class='cyc'>&rarr;</span>&#x13082;<span class='cyc'>&rarr;</span>&#x13141;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13333;<span class='cyc'>&rarr;</span>&#x13334;<span class='cyc'>&rarr;</span>&#x13335;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>m</kbd><kbd>A</kbd><kbd>a</kbd></td><td rowspan='2'><samp>m&#xa723;&#xa725;</samp></td><td><samp class='hiero'>&#x13180;<span class='cyc'>&rarr;</span>&#x13336;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13337;<span class='cyc'>&rarr;</span>&#x13419;<span class='cyc'>&rarr;</span>&#x1341A;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>A</kbd><kbd>a</kbd><kbd>t</kbd></td><td><samp>m&#xa723;&#xa725;t</samp></td><td><samp class='hiero'>&#x13066;<span class='cyc'>&rarr;</span>&#x13067;<span class='cyc'>&rarr;</span>&#x13184;<span class='cyc'>&rarr;</span>&#x13185;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>A</kbd><kbd>i</kbd></td><td><samp>m&#xa723;i</samp></td><td><samp class='hiero'>&#x130EC;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>m&#xa723;w</samp></td><td><samp class='hiero'>&#x13176;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>D</kbd></td><td><samp>m&#x1e0f;</samp></td><td><samp class='hiero'>&#x13386;<span class='cyc'>&rarr;</span>&#x13393;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>D</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>m&#x1e0f;&#xa723;t</samp></td><td><samp class='hiero'>&#x133DB;<span class='cyc'>&rarr;</span>&#x133DD;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>D</kbd><kbd>H</kbd></td><td><samp>m&#x1e0f;&#x1e25;</samp></td><td><samp class='hiero'>&#x132DC;<span class='cyc'>&rarr;</span>&#x1330E;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>D</kbd><kbd>t</kbd></td><td><samp>m&#x1e0f;t</samp></td><td><samp class='hiero'>&#x13385;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>H</kbd></td><td><samp>m&#x1e25;</samp></td><td><samp class='hiero'>&#x130A3;<span class='cyc'>&rarr;</span>&#x13394;<span class='cyc'>&rarr;</span>&#x13395;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>S</kbd><kbd>a</kbd></td><td><samp>m&#x0161;&#xa725;</samp></td><td><samp class='hiero'>&#x1300E;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>d</kbd></td><td><samp>md</samp></td><td><samp class='hiero'>&#x13303;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>m</kbd><kbd>i</kbd></td><td rowspan='2'><samp>mi</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x1309F;<span class='cyc'>&rarr;</span>&#x13155;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13156;<span class='cyc'>&rarr;</span>&#x13218;<span class='cyc'>&rarr;</span>&#x133C7;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>m</kbd></td><td><samp>mm</samp></td><td><samp class='hiero'>&#x13154;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd></td><td><samp>mn</samp></td><td><samp class='hiero'>&#x13307;<span class='cyc'>&rarr;</span>&#x133E0;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>D</kbd></td><td><samp>mn&#x1e0f;</samp></td><td><samp class='hiero'>&#x13091;<span class='cyc'>&rarr;</span>&#x13092;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>T</kbd><kbd>w</kbd></td><td><samp>mn&#x1e6f;w</samp></td><td><samp class='hiero'>&#x1306E;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>a</kbd><kbd>i</kbd></td><td><samp>mn&#xa725;i</samp></td><td><samp class='hiero'>&#x13091;<span class='cyc'>&rarr;</span>&#x13092;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>a</kbd><kbd>t</kbd></td><td><samp>mn&#xa725;t</samp></td><td><samp class='hiero'>&#x13054;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>h</kbd><kbd>d</kbd></td><td><samp>mnhd</samp></td><td><samp class='hiero'>&#x133DE;<span class='cyc'>&rarr;</span>&#x133DF;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>i</kbd></td><td><samp>mni</samp></td><td><samp class='hiero'>&#x1303F;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>i</kbd><kbd>t</kbd></td><td><samp>mnit</samp></td><td><samp class='hiero'>&#x132E7;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>i</kbd><kbd>w</kbd></td><td><samp>mniw</samp></td><td><samp class='hiero'>&#x13026;<span class='cyc'>&rarr;</span>&#x13038;<span class='cyc'>&rarr;</span>&#x13039;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>w</kbd></td><td><samp>mnw</samp></td><td><samp class='hiero'>&#x13064;<span class='cyc'>&rarr;</span>&#x132C9;<span class='cyc'>&rarr;</span>&#x132CA;<span class='cyc'>&rarr;</span>&#x13307;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>x</kbd></td><td><samp>mn&#x1e2b;</samp></td><td><samp class='hiero'>&#x1334A;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>n</kbd><kbd>x</kbd><kbd>t</kbd></td><td><samp>mn&#x1e2b;t</samp></td><td><samp class='hiero'>&#x132F2;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>m</kbd><kbd>r</kbd></td><td rowspan='2'><samp>mr</samp></td><td><samp class='hiero'>&#x13113;<span class='cyc'>&rarr;</span>&#x13218;<span class='cyc'>&rarr;</span>&#x13255;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13274;<span class='cyc'>&rarr;</span>&#x13338;<span class='cyc'>&rarr;</span>&#x1333B;<span class='cyc'>&rarr;</span>&#x1334B;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>s</kbd></td><td><samp>ms</samp></td><td><samp class='hiero'>&#x13052;<span class='cyc'>&rarr;</span>&#x13053;<span class='cyc'>&rarr;</span>&#x1311F;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>s</kbd><kbd>D</kbd><kbd>r</kbd></td><td><samp>ms&#x1e0f;r</samp></td><td><samp class='hiero'>&#x13114;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>s</kbd><kbd>i</kbd></td><td><samp>msi</samp></td><td><samp class='hiero'>&#x13052;<span class='cyc'>&rarr;</span>&#x13053;<span class='cyc'>&rarr;</span>&#x1311F;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>s</kbd><kbd>n</kbd></td><td><samp>msn</samp></td><td><samp class='hiero'>&#x133A3;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>msw</samp></td><td><samp class='hiero'>&#x13015;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>t</kbd></td><td><samp>mt</samp></td><td><samp class='hiero'>&#x130B8;<span class='cyc'>&rarr;</span>&#x130BA;<span class='cyc'>&rarr;</span>&#x13150;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>w</kbd></td><td><samp>mw</samp></td><td><samp class='hiero'>&#x13217;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>mwt</samp></td><td><samp class='hiero'>&#x13010;<span class='cyc'>&rarr;</span>&#x13011;<span class='cyc'>&rarr;</span>&#x13150;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>x</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>m&#x1e2b;&#xa723;t</samp></td><td><samp class='hiero'>&#x1335D;</samp></td>
			</tr>
			<tr>
				<td><kbd>m</kbd><kbd>z</kbd><kbd>H</kbd></td><td><samp>mz&#x1e25;</samp></td><td><samp class='hiero'>&#x1318A;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>N</td>
			</tr>
			<tr>
				<td><kbd>N</kbd></td><td><samp>N</samp></td><td><samp class='hiero'>&#x132D4;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>n</td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>n</kbd></td><td rowspan='2'><samp>n</samp></td><td><samp class='hiero'>&#x1309C;<span class='cyc'>&rarr;</span>&#x131D1;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13216;<span class='cyc'>&rarr;</span>&#x132D4;<span class='cyc'>&rarr;</span>&#x132D5;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>D</kbd></td><td><samp>n&#x1e0f;</samp></td><td><samp class='hiero'>&#x13429;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>D</kbd><kbd>m</kbd></td><td><samp>n&#x1e0f;m</samp></td><td><samp class='hiero'>&#x131DB;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>D</kbd><kbd>s</kbd></td><td><samp>n&#x1e0f;s</samp></td><td><samp class='hiero'>&#x1316A;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>H</kbd></td><td><samp>n&#x1e25;</samp></td><td><samp class='hiero'>&#x13158;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>S</kbd><kbd>m</kbd><kbd>t</kbd></td><td><samp>n&#x0161;mt</samp></td><td><samp class='hiero'>&#x131A0;<span class='cyc'>&rarr;</span>&#x1329E;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>T</kbd><kbd>r</kbd></td><td><samp>n&#x1e6f;r</samp></td><td><samp class='hiero'>&#x1302D;<span class='cyc'>&rarr;</span>&#x13146;<span class='cyc'>&rarr;</span>&#x132B9;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>b</kbd></td><td><samp>nb</samp></td><td><samp class='hiero'>&#x1339F;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>b</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>nbty</samp></td><td><samp class='hiero'>&#x13152;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>b</kbd><kbd>w</kbd></td><td><samp>nbw</samp></td><td><samp class='hiero'>&#x13212;<span class='cyc'>&rarr;</span>&#x13213;<span class='cyc'>&rarr;</span>&#x132DE;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>f</kbd><kbd>r</kbd></td><td><samp>nfr</samp></td><td><samp class='hiero'>&#x13124;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>f</kbd><kbd>w</kbd></td><td><samp>nfw</samp></td><td><samp class='hiero'>&#x132A1;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>i</kbd></td><td><samp>ni</samp></td><td><samp class='hiero'>&#x130A2;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>i</kbd><kbd>s</kbd></td><td><samp>nis</samp></td><td><samp class='hiero'>&#x1301E;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>i</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>niwt</samp></td><td><samp class='hiero'>&#x13296;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>m</kbd></td><td><samp>nm</samp></td><td><samp class='hiero'>&#x13255;<span class='cyc'>&rarr;</span>&#x13330;<span class='cyc'>&rarr;</span>&#x13331;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>m</kbd><kbd>H</kbd></td><td><samp>nm&#x1e25;</samp></td><td><samp class='hiero'>&#x13014;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>m</kbd><kbd>t</kbd></td><td><samp>nmt</samp></td><td><samp class='hiero'>&#x13329;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>m</kbd><kbd>t</kbd><kbd>i</kbd></td><td><samp>nmti</samp></td><td><samp class='hiero'>&#x13147;<span class='cyc'>&rarr;</span>&#x13148;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>m</kbd><kbd>t</kbd><kbd>t</kbd></td><td><samp>nmtt</samp></td><td><samp class='hiero'>&#x130BB;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>n</kbd></td><td><samp>nn</samp></td><td><samp class='hiero'>&#x131D2;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>n</kbd><kbd>i</kbd></td><td><samp>nni</samp></td><td><samp class='hiero'>&#x13014;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>r</kbd></td><td><samp>nr</samp></td><td><samp class='hiero'>&#x13150;<span class='cyc'>&rarr;</span>&#x13182;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>s</kbd></td><td><samp>ns</samp></td><td><samp class='hiero'>&#x13113;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>s</kbd><kbd>t</kbd></td><td><samp>nst</samp></td><td><samp class='hiero'>&#x133BC;<span class='cyc'>&rarr;</span>&#x133BD;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>nsw</samp></td><td><samp class='hiero'>&#x1302F;<span class='cyc'>&rarr;</span>&#x13030;<span class='cyc'>&rarr;</span>&#x13032;<span class='cyc'>&rarr;</span>&#x13034;<span class='cyc'>&rarr;</span>&#x13146;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>w</kbd></td><td><samp>nw</samp></td><td><samp class='hiero'>&#x13347;<span class='cyc'>&rarr;</span>&#x13348;<span class='cyc'>&rarr;</span>&#x133CC;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>x</kbd><kbd>n</kbd></td><td><samp>n&#x1e2b;n</samp></td><td><samp class='hiero'>&#x13294;<span class='cyc'>&rarr;</span>&#x13295;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>x</kbd><kbd>t</kbd></td><td><samp>n&#x1e2b;t</samp></td><td><samp class='hiero'>&#x1301C;<span class='cyc'>&rarr;</span>&#x130A1;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>x</kbd><kbd>x</kbd><kbd>w</kbd></td><td><samp>n&#x1e2b;&#x1e2b;w</samp></td><td><samp class='hiero'>&#x13305;</samp></td>
			</tr>
			<tr>
				<td><kbd>n</kbd><kbd>z</kbd><kbd>t</kbd></td><td><samp>nzt</samp></td><td><samp class='hiero'>&#x133BC;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>p</td>
			</tr>
			<tr>
				<td><kbd>p</kbd></td><td><samp>p</samp></td><td><samp class='hiero'>&#x13250;<span class='cyc'>&rarr;</span>&#x132AA;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>A</kbd></td><td><samp>p&#xa723;</samp></td><td><samp class='hiero'>&#x1316E;<span class='cyc'>&rarr;</span>&#x1316F;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>A</kbd><kbd>q</kbd></td><td><samp>p&#xa723;&#x1e33;</samp></td><td><samp class='hiero'>&#x13180;<span class='cyc'>&rarr;</span>&#x13181;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>D</kbd></td><td><samp>p&#x1e0f;</samp></td><td><samp class='hiero'>&#x13312;<span class='cyc'>&rarr;</span>&#x13313;<span class='cyc'>&rarr;</span>&#x13314;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>H</kbd></td><td><samp>p&#x1e25;</samp></td><td><samp class='hiero'>&#x13116;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>X</kbd><kbd>r</kbd></td><td><samp>p&#x1e96;r</samp></td><td><samp class='hiero'>&#x13132;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>d</kbd></td><td><samp>pd</samp></td><td><samp class='hiero'>&#x13312;<span class='cyc'>&rarr;</span>&#x13313;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>g</kbd><kbd>A</kbd></td><td><samp>pg&#xa723;</samp></td><td><samp class='hiero'>&#x13098;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>q</kbd></td><td><samp>p&#x1e33;</samp></td><td><samp class='hiero'>&#x13180;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>r</kbd></td><td><samp>pr</samp></td><td><samp class='hiero'>&#x13250;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>s</kbd><kbd>g</kbd></td><td><samp>psg</samp></td><td><samp class='hiero'>&#x13090;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>t</kbd></td><td><samp>pt</samp></td><td><samp class='hiero'>&#x131EF;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>t</kbd><kbd>H</kbd></td><td><samp>pt&#x1e25;</samp></td><td><samp class='hiero'>&#x13070;<span class='cyc'>&rarr;</span>&#x13071;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>t</kbd><kbd>i</kbd></td><td><samp>pti</samp></td><td><samp class='hiero'>&#x1307A;<span class='cyc'>&rarr;</span>&#x1307B;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>t</kbd><kbd>r</kbd></td><td><samp>ptr</samp></td><td><samp class='hiero'>&#x1307A;<span class='cyc'>&rarr;</span>&#x1307B;<span class='cyc'>&rarr;</span>&#x1307C;</samp></td>
			</tr>
			<tr>
				<td><kbd>p</kbd><kbd>z</kbd><kbd>D</kbd></td><td><samp>pz&#x1e0f;</samp></td><td><samp class='hiero'>&#x131F7;<span class='cyc'>&rarr;</span>&#x131F8;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>ḳ (q)</td>
			</tr>
			<tr>
				<td><kbd>q</kbd></td><td><samp>&#x1e33;</samp></td><td><samp class='hiero'>&#x1320E;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>A</kbd></td><td><samp>&#x1e33;&#xa723;</samp></td><td><samp class='hiero'>&#x13020;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>A</kbd><kbd>b</kbd></td><td><samp>&#x1e33;&#xa723;b</samp></td><td><samp class='hiero'>&#x13132;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>A</kbd><kbd>i</kbd></td><td><samp>&#x1e33;&#xa723;i</samp></td><td><samp class='hiero'>&#x13020;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>d</kbd></td><td><samp>&#x1e33;d</samp></td><td><samp class='hiero'>&#x13028;<span class='cyc'>&rarr;</span>&#x1342A;<span class='cyc'>&rarr;</span>&#x1342B;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>i</kbd></td><td><samp>&#x1e33;i</samp></td><td><samp class='hiero'>&#x1303E;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>i</kbd><kbd>s</kbd></td><td><samp>&#x1e33;is</samp></td><td><samp class='hiero'>&#x1302B;<span class='cyc'>&rarr;</span>&#x1302C;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>i</kbd><kbd>z</kbd></td><td><samp>&#x1e33;iz</samp></td><td><samp class='hiero'>&#x1302B;<span class='cyc'>&rarr;</span>&#x1302C;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>m</kbd><kbd>A</kbd></td><td><samp>&#x1e33;m&#xa723;</samp></td><td><samp class='hiero'>&#x1316F;<span class='cyc'>&rarr;</span>&#x13319;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>n</kbd></td><td><samp>&#x1e33;n</samp></td><td><samp class='hiero'>&#x13416;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>r</kbd><kbd>s</kbd></td><td><samp>&#x1e33;rs</samp></td><td><samp class='hiero'>&#x132AD;<span class='cyc'>&rarr;</span>&#x1331F;<span class='cyc'>&rarr;</span>&#x13320;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>r</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>&#x1e33;rsw</samp></td><td><samp class='hiero'>&#x132AD;</samp></td>
			</tr>
			<tr>
				<td><kbd>q</kbd><kbd>s</kbd></td><td><samp>&#x1e33;s</samp></td><td><samp class='hiero'>&#x1331F;<span class='cyc'>&rarr;</span>&#x13320;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>r</td>
			</tr>
			<tr>
				<td><kbd>r</kbd></td><td><samp>r</samp></td><td><samp class='hiero'>&#x1308B;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>A</kbd></td><td><samp>r&#xa723;</samp></td><td><samp class='hiero'>&#x1308B;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>A</kbd><kbd>w</kbd><kbd>i</kbd></td><td><samp>r&#xa723;wi</samp></td><td><samp class='hiero'>&#x1308C;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>a</kbd></td><td><samp>r&#xa725;</samp></td><td><samp class='hiero'>&#x1305A;<span class='cyc'>&rarr;</span>&#x1305B;<span class='cyc'>&rarr;</span>&#x1314A;<span class='cyc'>&rarr;</span>&#x131F3;<span class='cyc'>&rarr;</span>&#x131F4;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>d</kbd></td><td><samp>rd</samp></td><td><samp class='hiero'>&#x130BE;<span class='cyc'>&rarr;</span>&#x131DD;<span class='cyc'>&rarr;</span>&#x131DF;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>d</kbd><kbd>i</kbd></td><td><samp>rdi</samp></td><td><samp class='hiero'>&#x1309E;<span class='cyc'>&rarr;</span>&#x133D9;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>h</kbd><kbd>n</kbd></td><td><samp>rhn</samp></td><td><samp class='hiero'>&#x13017;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>m</kbd><kbd>i</kbd></td><td><samp>rmi</samp></td><td><samp class='hiero'>&#x1307F;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>m</kbd><kbd>n</kbd></td><td><samp>rmn</samp></td><td><samp class='hiero'>&#x130A2;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>n</kbd><kbd>n</kbd></td><td><samp>rnn</samp></td><td><samp class='hiero'>&#x13056;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>n</kbd><kbd>p</kbd></td><td><samp>rnp</samp></td><td><samp class='hiero'>&#x131B3;<span class='cyc'>&rarr;</span>&#x131B6;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>s</kbd></td><td><samp>rs</samp></td><td><samp class='hiero'>&#x131D4;<span class='cyc'>&rarr;</span>&#x131D6;<span class='cyc'>&rarr;</span>&#x13318;<span class='cyc'>&rarr;</span>&#x1335F;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>rsw</samp></td><td><samp class='hiero'>&#x131D4;<span class='cyc'>&rarr;</span>&#x131D6;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>t</kbd><kbd>H</kbd></td><td><samp>rt&#x1e25;</samp></td><td><samp class='hiero'>&#x13355;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>w</kbd></td><td><samp>rw</samp></td><td><samp class='hiero'>&#x130ED;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>w</kbd><kbd>D</kbd></td><td><samp>rw&#x1e0f;</samp></td><td><samp class='hiero'>&#x13317;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>w</kbd><kbd>d</kbd></td><td><samp>rwd</samp></td><td><samp class='hiero'>&#x1328D;<span class='cyc'>&rarr;</span>&#x13317;</samp></td>
			</tr>
			<tr>
				<td><kbd>r</kbd><kbd>x</kbd><kbd>y</kbd><kbd>t</kbd></td><td><samp>r&#x1e2b;yt</samp></td><td><samp class='hiero'>&#x1315A;<span class='cyc'>&rarr;</span>&#x1315B;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>š (S)</td>
			</tr>
			<tr>
				<td><kbd>S</kbd></td><td><samp>&#x0161;</samp></td><td><samp class='hiero'>&#x13219;<span class='cyc'>&rarr;</span>&#x1321B;<span class='cyc'>&rarr;</span>&#x1321C;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>A</kbd></td><td><samp>&#x0161;&#xa723;</samp></td><td><samp class='hiero'>&#x13186;<span class='cyc'>&rarr;</span>&#x131B7;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>d</kbd></td><td><samp>&#x0161;d</samp></td><td><samp class='hiero'>&#x1311E;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>m</kbd></td><td><samp>&#x0161;m</samp></td><td><samp class='hiero'>&#x1321D;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>m</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>&#x0161;m&#xa723;w</samp></td><td><samp class='hiero'>&#x13026;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>m</kbd><kbd>a</kbd></td><td><samp>&#x0161;m&#xa725;</samp></td><td><samp class='hiero'>&#x131D7;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>m</kbd><kbd>s</kbd></td><td><samp>&#x0161;ms</samp></td><td><samp class='hiero'>&#x1331E;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd></td><td><samp>&#x0161;n</samp></td><td><samp class='hiero'>&#x13372;<span class='cyc'>&rarr;</span>&#x13375;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd><kbd>a</kbd></td><td><samp>&#x0161;n&#xa725;</samp></td><td><samp class='hiero'>&#x13341;<span class='cyc'>&rarr;</span>&#x13342;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd><kbd>d</kbd><kbd>y</kbd><kbd>t</kbd></td><td><samp>&#x0161;ndyt</samp></td><td><samp class='hiero'>&#x132EF;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd><kbd>i</kbd></td><td><samp>&#x0161;ni</samp></td><td><samp class='hiero'>&#x13078;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd><kbd>t</kbd></td><td><samp>&#x0161;nt</samp></td><td><samp class='hiero'>&#x13362;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>&#x0161;nwt</samp></td><td><samp class='hiero'>&#x1329A;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>n</kbd><kbd>y</kbd></td><td><samp>&#x0161;ny</samp></td><td><samp class='hiero'>&#x13078;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>p</kbd></td><td><samp>&#x0161;p</samp></td><td><samp class='hiero'>&#x1307A;<span class='cyc'>&rarr;</span>&#x1307B;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>p</kbd><kbd>s</kbd></td><td><samp>&#x0161;ps</samp></td><td><samp class='hiero'>&#x1303B;<span class='cyc'>&rarr;</span>&#x1303C;<span class='cyc'>&rarr;</span>&#x1303D;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>p</kbd><kbd>s</kbd><kbd>i</kbd></td><td><samp>&#x0161;psi</samp></td><td><samp class='hiero'>&#x1303B;<span class='cyc'>&rarr;</span>&#x1303C;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>r</kbd><kbd>i</kbd></td><td><samp>&#x0161;ri</samp></td><td><samp class='hiero'>&#x13014;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>s</kbd><kbd>A</kbd></td><td><samp>&#x0161;s&#xa723;</samp></td><td><samp class='hiero'>&#x13103;<span class='cyc'>&rarr;</span>&#x13104;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>s</kbd><kbd>p</kbd></td><td><samp>&#x0161;sp</samp></td><td><samp class='hiero'>&#x130AA;<span class='cyc'>&rarr;</span>&#x131F9;<span class='cyc'>&rarr;</span>&#x1328F;<span class='cyc'>&rarr;</span>&#x13290;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>t</kbd></td><td><samp>&#x0161;t</samp></td><td><samp class='hiero'>&#x13362;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>t</kbd><kbd>y</kbd><kbd>w</kbd></td><td><samp>&#x0161;tyw</samp></td><td><samp class='hiero'>&#x13189;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>w</kbd></td><td><samp>&#x0161;w</samp></td><td><samp class='hiero'>&#x13184;<span class='cyc'>&rarr;</span>&#x13185;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>&#x0161;wt</samp></td><td><samp class='hiero'>&#x132FA;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>w</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>&#x0161;wty</samp></td><td><samp class='hiero'>&#x132DB;</samp></td>
			</tr>
			<tr>
				<td><kbd>S</kbd><kbd>z</kbd><kbd>p</kbd></td><td><samp>&#x0161;zp</samp></td><td><samp class='hiero'>&#x1328F;<span class='cyc'>&rarr;</span>&#x13290;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>s</td>
			</tr>
			<tr>
				<td><kbd>s</kbd></td><td><samp>s</samp></td><td><samp class='hiero'>&#x13283;<span class='cyc'>&rarr;</span>&#x132F4;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>s</kbd><kbd>A</kbd></td><td rowspan='2'><samp>s&#xa723;</samp></td><td><samp class='hiero'>&#x1316D;<span class='cyc'>&rarr;</span>&#x13187;<span class='cyc'>&rarr;</span>&#x13382;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13383;<span class='cyc'>&rarr;</span>&#x13384;<span class='cyc'>&rarr;</span>&#x1341F;<span class='cyc'>&rarr;</span>&#x13420;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>A</kbd><kbd>H</kbd></td><td><samp>s&#xa723;&#x1e25;</samp></td><td><samp class='hiero'>&#x130C3;<span class='cyc'>&rarr;</span>&#x130C4;<span class='cyc'>&rarr;</span>&#x130C5;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>A</kbd><kbd>q</kbd></td><td><samp>s&#xa723;&#x1e33;</samp></td><td><samp class='hiero'>&#x1318C;<span class='cyc'>&rarr;</span>&#x1318D;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>s&#xa723;w</samp></td><td><samp class='hiero'>&#x13038;<span class='cyc'>&rarr;</span>&#x13039;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>D</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>s&#x1e0f;&#xa723;w</samp></td><td><samp class='hiero'>&#x132E8;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>D</kbd><kbd>m</kbd></td><td><samp>s&#x1e0f;m</samp></td><td><samp class='hiero'>&#x13114;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>D</kbd><kbd>r</kbd></td><td><samp>s&#x1e0f;r</samp></td><td><samp class='hiero'>&#x13040;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>H</kbd><kbd>r</kbd></td><td><samp>s&#x1e25;r</samp></td><td><samp class='hiero'>&#x13044;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>S</kbd></td><td><samp>s&#x0161;</samp></td><td><samp class='hiero'>&#x13371;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>S</kbd><kbd>m</kbd></td><td><samp>s&#x0161;m</samp></td><td><samp class='hiero'>&#x1332B;<span class='cyc'>&rarr;</span>&#x1332C;<span class='cyc'>&rarr;</span>&#x1332E;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>S</kbd><kbd>r</kbd></td><td><samp>s&#x0161;r</samp></td><td><samp class='hiero'>&#x13315;<span class='cyc'>&rarr;</span>&#x133A4;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>T</kbd></td><td><samp>s&#x1e6f;</samp></td><td><samp class='hiero'>&#x1311D;<span class='cyc'>&rarr;</span>&#x132EB;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>T</kbd><kbd>A</kbd></td><td><samp>s&#x1e6f;&#xa723;</samp></td><td><samp class='hiero'>&#x1336C;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>T</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>s&#x1e6f;&#xa723;w</samp></td><td><samp class='hiero'>&#x1336E;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>X</kbd><kbd>r</kbd></td><td><samp>s&#x1e96;r</samp></td><td><samp class='hiero'>&#x13315;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>a</kbd><kbd>H</kbd></td><td><samp>s&#xa725;&#x1e25;</samp></td><td><samp class='hiero'>&#x1303D;<span class='cyc'>&rarr;</span>&#x130F6;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>b</kbd><kbd>A</kbd></td><td><samp>sb&#xa723;</samp></td><td><samp class='hiero'>&#x1301C;<span class='cyc'>&rarr;</span>&#x131FC;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>b</kbd><kbd>i</kbd></td><td><samp>sbi</samp></td><td><samp class='hiero'>&#x1300F;<span class='cyc'>&rarr;</span>&#x13010;<span class='cyc'>&rarr;</span>&#x13011;<span class='cyc'>&rarr;</span>&#x13428;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>b</kbd><kbd>k</kbd></td><td><samp>sbk</samp></td><td><samp class='hiero'>&#x1318B;<span class='cyc'>&rarr;</span>&#x1318C;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>b</kbd><kbd>q</kbd></td><td><samp>sb&#x1e33;</samp></td><td><samp class='hiero'>&#x130BE;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>d</kbd></td><td><samp>sd</samp></td><td><samp class='hiero'>&#x13122;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>f</kbd></td><td><samp>sf</samp></td><td><samp class='hiero'>&#x132F5;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>i</kbd><kbd>A</kbd></td><td><samp>si&#xa723;</samp></td><td><samp class='hiero'>&#x132F7;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>i</kbd><kbd>A</kbd><kbd>T</kbd></td><td><samp>si&#xa723;&#x1e6f;</samp></td><td><samp class='hiero'>&#x130BF;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>k</kbd></td><td><samp>sk</samp></td><td><samp class='hiero'>&#x1339D;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>m</kbd></td><td><samp>sm</samp></td><td><samp class='hiero'>&#x1309C;<span class='cyc'>&rarr;</span>&#x131D0;<span class='cyc'>&rarr;</span>&#x132C9;<span class='cyc'>&rarr;</span>&#x132CA;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>m</kbd><kbd>A</kbd></td><td><samp>sm&#xa723;</samp></td><td><samp class='hiero'>&#x130D3;<span class='cyc'>&rarr;</span>&#x13125;<span class='cyc'>&rarr;</span>&#x132F6;<span class='cyc'>&rarr;</span>&#x13427;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>m</kbd><kbd>r</kbd></td><td><samp>smr</samp></td><td><samp class='hiero'>&#x13019;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>m</kbd><kbd>s</kbd><kbd>w</kbd></td><td><samp>smsw</samp></td><td><samp class='hiero'>&#x13017;<span class='cyc'>&rarr;</span>&#x13018;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>m</kbd><kbd>x</kbd></td><td><samp>sm&#x1e2b;</samp></td><td><samp class='hiero'>&#x1309C;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>s</kbd><kbd>n</kbd></td><td rowspan='2'><samp>sn</samp></td><td><samp class='hiero'>&#x13219;<span class='cyc'>&rarr;</span>&#x1321B;<span class='cyc'>&rarr;</span>&#x1321C;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13322;<span class='cyc'>&rarr;</span>&#x13323;<span class='cyc'>&rarr;</span>&#x133D2;<span class='cyc'>&rarr;</span>&#x133D5;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>n</kbd><kbd>D</kbd></td><td><samp>sn&#x1e0f;</samp></td><td><samp class='hiero'>&#x1317E;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>n</kbd><kbd>T</kbd></td><td><samp>sn&#x1e6f;</samp></td><td><samp class='hiero'>&#x13370;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>n</kbd><kbd>T</kbd><kbd>r</kbd></td><td><samp>sn&#x1e6f;r</samp></td><td><samp class='hiero'>&#x132B8;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>n</kbd><kbd>f</kbd></td><td><samp>snf</samp></td><td><samp class='hiero'>&#x13090;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>n</kbd><kbd>q</kbd></td><td><samp>sn&#x1e33;</samp></td><td><samp class='hiero'>&#x13091;<span class='cyc'>&rarr;</span>&#x13092;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>p</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>sp&#xa723;t</samp></td><td><samp class='hiero'>&#x13208;<span class='cyc'>&rarr;</span>&#x13416;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>p</kbd><kbd>r</kbd></td><td><samp>spr</samp></td><td><samp class='hiero'>&#x1312D;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>p</kbd><kbd>t</kbd></td><td><samp>spt</samp></td><td><samp class='hiero'>&#x1308E;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>p</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>spty</samp></td><td><samp class='hiero'>&#x1308F;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>q</kbd><kbd>d</kbd></td><td><samp>s&#x1e33;d</samp></td><td><samp class='hiero'>&#x1300C;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>q</kbd><kbd>d</kbd><kbd>w</kbd></td><td><samp>s&#x1e33;dw</samp></td><td><samp class='hiero'>&#x1300C;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>r</kbd></td><td><samp>sr</samp></td><td><samp class='hiero'>&#x13019;<span class='cyc'>&rarr;</span>&#x130F1;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>r</kbd><kbd>q</kbd><kbd>t</kbd></td><td><samp>sr&#x1e33;t</samp></td><td><samp class='hiero'>&#x131AB;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>t</kbd></td><td><samp>st</samp></td><td><samp class='hiero'>&#x13050;<span class='cyc'>&rarr;</span>&#x1311D;<span class='cyc'>&rarr;</span>&#x132A8;<span class='cyc'>&rarr;</span>&#x132A9;<span class='cyc'>&rarr;</span>&#x132EB;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>t</kbd><kbd>X</kbd></td><td><samp>st&#x1e96;</samp></td><td><samp class='hiero'>&#x13063;<span class='cyc'>&rarr;</span>&#x130E9;<span class='cyc'>&rarr;</span>&#x130EB;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>t</kbd><kbd>i</kbd></td><td><samp>sti</samp></td><td><samp class='hiero'>&#x1311D;<span class='cyc'>&rarr;</span>&#x1342E;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>t</kbd><kbd>p</kbd></td><td><samp>stp</samp></td><td><samp class='hiero'>&#x13349;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>w</kbd></td><td><samp>sw</samp></td><td><samp class='hiero'>&#x131D3;<span class='cyc'>&rarr;</span>&#x131F3;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>x</kbd><kbd>m</kbd></td><td><samp>s&#x1e2b;m</samp></td><td><samp class='hiero'>&#x13302;<span class='cyc'>&rarr;</span>&#x133E3;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>x</kbd><kbd>m</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>s&#x1e2b;mty</samp></td><td><samp class='hiero'>&#x132D6;<span class='cyc'>&rarr;</span>&#x132D7;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>x</kbd><kbd>r</kbd></td><td><samp>s&#x1e2b;r</samp></td><td><samp class='hiero'>&#x13021;</samp></td>
			</tr>
			<tr>
				<td><kbd>s</kbd><kbd>x</kbd><kbd>t</kbd></td><td><samp>s&#x1e2b;t</samp></td><td><samp class='hiero'>&#x131CF;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>ṯ (T)</td>
			</tr>
			<tr>
				<td><kbd>T</kbd></td><td><samp>&#x1e6f;</samp></td><td><samp class='hiero'>&#x1337F;<span class='cyc'>&rarr;</span>&#x13380;</samp></td>
			</tr>
			<tr>
				<td><kbd>T</kbd><kbd>A</kbd></td><td><samp>&#x1e6f;&#xa723;</samp></td><td><samp class='hiero'>&#x13177;</samp></td>
			</tr>
			<tr>
				<td><kbd>T</kbd><kbd>A</kbd><kbd>w</kbd></td><td><samp>&#x1e6f;&#xa723;w</samp></td><td><samp class='hiero'>&#x132A1;</samp></td>
			</tr>
			<tr>
				<td><kbd>T</kbd><kbd>H</kbd><kbd>n</kbd></td><td><samp>&#x1e6f;&#x1e25;n</samp></td><td><samp class='hiero'>&#x132E3;<span class='cyc'>&rarr;</span>&#x132E4;<span class='cyc'>&rarr;</span>&#x132E5;</samp></td>
			</tr>
			<tr>
				<td><kbd>T</kbd><kbd>b</kbd></td><td><samp>&#x1e6f;b</samp></td><td><samp class='hiero'>&#x132F8;</samp></td>
			</tr>
			<tr>
				<td><kbd>T</kbd><kbd>m</kbd><kbd>A</kbd></td><td><samp>&#x1e6f;m&#xa723;</samp></td><td><samp class='hiero'>&#x13385;<span class='cyc'>&rarr;</span>&#x13412;</samp></td>
			</tr>
			<tr>
				<td><kbd>T</kbd><kbd>z</kbd></td><td><samp>&#x1e6f;z</samp></td><td><samp class='hiero'>&#x132ED;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>t</td>
			</tr>
			<tr>
				<td><kbd>t</kbd></td><td><samp>t</samp></td><td><samp class='hiero'>&#x133CF;<span class='cyc'>&rarr;</span>&#x133D0;<span class='cyc'>&rarr;</span>&#x133D1;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>t</kbd><kbd>A</kbd></td><td rowspan='2'><samp>t&#xa723;</samp></td><td><samp class='hiero'>&#x131FE;<span class='cyc'>&rarr;</span>&#x131FF;<span class='cyc'>&rarr;</span>&#x13207;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x1326A;<span class='cyc'>&rarr;</span>&#x1326B;<span class='cyc'>&rarr;</span>&#x132AE;<span class='cyc'>&rarr;</span>&#x13354;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>H</kbd><kbd>n</kbd></td><td><samp>t&#x1e25;n</samp></td><td><samp class='hiero'>&#x132E3;<span class='cyc'>&rarr;</span>&#x132E4;<span class='cyc'>&rarr;</span>&#x132E5;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>i</kbd></td><td><samp>ti</samp></td><td><samp class='hiero'>&#x131B4;<span class='cyc'>&rarr;</span>&#x131B5;<span class='cyc'>&rarr;</span>&#x13358;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>i</kbd><kbd>t</kbd></td><td><samp>tit</samp></td><td><samp class='hiero'>&#x13087;<span class='cyc'>&rarr;</span>&#x13358;<span class='cyc'>&rarr;</span>&#x133AC;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>m</kbd></td><td><samp>tm</samp></td><td><samp class='hiero'>&#x1328B;<span class='cyc'>&rarr;</span>&#x13343;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>n</kbd><kbd>i</kbd></td><td><samp>tni</samp></td><td><samp class='hiero'>&#x13017;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>p</kbd></td><td><samp>tp</samp></td><td><samp class='hiero'>&#x13076;<span class='cyc'>&rarr;</span>&#x13310;<span class='cyc'>&rarr;</span>&#x13311;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>r</kbd></td><td><samp>tr</samp></td><td><samp class='hiero'>&#x13022;<span class='cyc'>&rarr;</span>&#x131B3;<span class='cyc'>&rarr;</span>&#x131B4;<span class='cyc'>&rarr;</span>&#x131B5;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>w</kbd><kbd>A</kbd></td><td><samp>tw&#xa723;</samp></td><td><samp class='hiero'>&#x13022;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>twt</samp></td><td><samp class='hiero'>&#x1301A;<span class='cyc'>&rarr;</span>&#x1303E;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>x</kbd><kbd>n</kbd></td><td><samp>t&#x1e2b;n</samp></td><td><samp class='hiero'>&#x13276;</samp></td>
			</tr>
			<tr>
				<td><kbd>t</kbd><kbd>y</kbd><kbd>w</kbd></td><td><samp>tyw</samp></td><td><samp class='hiero'>&#x13142;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>W</td>
			</tr>
			<tr>
				<td><kbd>W</kbd></td><td><samp>W</samp></td><td><samp class='hiero'>&#x133F2;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>w</td>
			</tr>
			<tr>
				<td><kbd>w</kbd></td><td><samp>w</samp></td><td><samp class='hiero'>&#x13171;<span class='cyc'>&rarr;</span>&#x133F2;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>A</kbd></td><td><samp>w&#xa723;</samp></td><td><samp class='hiero'>&#x1336F;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>A</kbd><kbd>D</kbd></td><td><samp>w&#xa723;&#x1e0f;</samp></td><td><samp class='hiero'>&#x131C5;<span class='cyc'>&rarr;</span>&#x131C6;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>A</kbd><kbd>H</kbd></td><td><samp>w&#xa723;&#x1e25;</samp></td><td><samp class='hiero'>&#x1339D;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>A</kbd><kbd>s</kbd></td><td><samp>w&#xa723;s</samp></td><td><samp class='hiero'>&#x132C6;<span class='cyc'>&rarr;</span>&#x13300;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>D</kbd></td><td><samp>w&#x1e0f;</samp></td><td><samp class='hiero'>&#x13278;<span class='cyc'>&rarr;</span>&#x13397;<span class='cyc'>&rarr;</span>&#x13398;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>D</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>w&#x1e0f;&#xa723;t</samp></td><td><samp class='hiero'>&#x13080;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>D</kbd><kbd>a</kbd></td><td><samp>w&#x1e0f;&#xa725;</samp></td><td><samp class='hiero'>&#x13423;<span class='cyc'>&rarr;</span>&#x13424;</samp></td>
			</tr>
			<tr class='halfrow'>
				<td rowspan='2'><kbd>w</kbd><kbd>D</kbd><kbd>b</kbd></td><td rowspan='2'><samp>w&#x1e0f;b</samp></td><td><samp class='hiero'>&#x13132;<span class='cyc'>&rarr;</span>&#x13134;<span class='cyc'>&rarr;</span></samp></td>
			</tr>
			<tr>
				<td><samp class='hiero'>&#x13136;<span class='cyc'>&rarr;</span>&#x13137;<span class='cyc'>&rarr;</span>&#x13204;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>H</kbd><kbd>a</kbd></td><td><samp>w&#x1e25;&#xa725;</samp></td><td><samp class='hiero'>&#x132A0;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>H</kbd><kbd>m</kbd></td><td><samp>w&#x1e25;m</samp></td><td><samp class='hiero'>&#x13119;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>S</kbd></td><td><samp>w&#x0161;</samp></td><td><samp class='hiero'>&#x13078;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>S</kbd><kbd>A</kbd></td><td><samp>w&#x0161;&#xa723;</samp></td><td><samp class='hiero'>&#x13170;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>S</kbd><kbd>m</kbd></td><td><samp>w&#x0161;m</samp></td><td><samp class='hiero'>&#x13180;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>a</kbd></td><td><samp>w&#xa725;</samp></td><td><samp class='hiero'>&#x13174;<span class='cyc'>&rarr;</span>&#x13210;<span class='cyc'>&rarr;</span>&#x13321;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>a</kbd><kbd>b</kbd></td><td><samp>w&#xa725;b</samp></td><td><samp class='hiero'>&#x13006;<span class='cyc'>&rarr;</span>&#x130C2;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>b</kbd><kbd>A</kbd></td><td><samp>wb&#xa723;</samp></td><td><samp class='hiero'>&#x1334F;<span class='cyc'>&rarr;</span>&#x13350;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>d</kbd><kbd>n</kbd></td><td><samp>wdn</samp></td><td><samp class='hiero'>&#x131BB;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>i</kbd></td><td><samp>wi</samp></td><td><samp class='hiero'>&#x1303E;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>n</kbd></td><td><samp>wn</samp></td><td><samp class='hiero'>&#x130F9;<span class='cyc'>&rarr;</span>&#x131EC;<span class='cyc'>&rarr;</span>&#x1327F;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>n</kbd><kbd>m</kbd></td><td><samp>wnm</samp></td><td><samp class='hiero'>&#x131EC;<span class='cyc'>&rarr;</span>&#x133F6;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>p</kbd></td><td><samp>wp</samp></td><td><samp class='hiero'>&#x1310B;<span class='cyc'>&rarr;</span>&#x133F4;<span class='cyc'>&rarr;</span>&#x133F5;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>r</kbd></td><td><samp>wr</samp></td><td><samp class='hiero'>&#x13017;<span class='cyc'>&rarr;</span>&#x13168;<span class='cyc'>&rarr;</span>&#x133F4;<span class='cyc'>&rarr;</span>&#x133F5;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>r</kbd><kbd>d</kbd></td><td><samp>wrd</samp></td><td><samp class='hiero'>&#x13009;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>r</kbd><kbd>r</kbd><kbd>t</kbd></td><td><samp>wrrt</samp></td><td><samp class='hiero'>&#x132D7;<span class='cyc'>&rarr;</span>&#x1331D;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>s</kbd><kbd>i</kbd><kbd>r</kbd></td><td><samp>wsir</samp></td><td><samp class='hiero'>&#x13032;<span class='cyc'>&rarr;</span>&#x13034;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>s</kbd><kbd>r</kbd></td><td><samp>wsr</samp></td><td><samp class='hiero'>&#x1310A;<span class='cyc'>&rarr;</span>&#x132A4;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>s</kbd><kbd>x</kbd></td><td><samp>ws&#x1e2b;</samp></td><td><samp class='hiero'>&#x132DD;<span class='cyc'>&rarr;</span>&#x13410;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>s</kbd><kbd>x</kbd><kbd>t</kbd></td><td><samp>ws&#x1e2b;t</samp></td><td><samp class='hiero'>&#x13269;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>w</kbd></td><td><samp>ww</samp></td><td><samp class='hiero'>&#x13173;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>x</kbd></td><td><samp>w&#x1e2b;</samp></td><td><samp class='hiero'>&#x132C2;</samp></td>
			</tr>
			<tr>
				<td><kbd>w</kbd><kbd>z</kbd></td><td><samp>wz</samp></td><td><samp class='hiero'>&#x132A8;<span class='cyc'>&rarr;</span>&#x132A9;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>ẖ (X)</td>
			</tr>
			<tr>
				<td><kbd>X</kbd></td><td><samp>&#x1e96;</samp></td><td><samp class='hiero'>&#x13121;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>A</kbd></td><td><samp>&#x1e96;&#xa723;</samp></td><td><samp class='hiero'>&#x1319E;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>A</kbd><kbd>r</kbd></td><td><samp>&#x1e96;&#xa723;r</samp></td><td><samp class='hiero'>&#x13385;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>&#x1e96;&#xa723;t</samp></td><td><samp class='hiero'>&#x13040;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>k</kbd><kbd>r</kbd></td><td><samp>&#x1e96;kr</samp></td><td><samp class='hiero'>&#x1342C;<span class='cyc'>&rarr;</span>&#x1342D;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>n</kbd></td><td><samp>&#x1e96;n</samp></td><td><samp class='hiero'>&#x13099;<span class='cyc'>&rarr;</span>&#x1311A;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>n</kbd><kbd>i</kbd></td><td><samp>&#x1e96;ni</samp></td><td><samp class='hiero'>&#x13099;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>n</kbd><kbd>m</kbd></td><td><samp>&#x1e96;nm</samp></td><td><samp class='hiero'>&#x133B8;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>n</kbd><kbd>m</kbd><kbd>w</kbd></td><td><samp>&#x1e96;nmw</samp></td><td><samp class='hiero'>&#x13060;<span class='cyc'>&rarr;</span>&#x13061;<span class='cyc'>&rarr;</span>&#x130DD;<span class='cyc'>&rarr;</span>&#x130DE;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>n</kbd><kbd>t</kbd><kbd>i</kbd></td><td><samp>&#x1e96;nti</samp></td><td><samp class='hiero'>&#x1301A;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>r</kbd></td><td><samp>&#x1e96;r</samp></td><td><samp class='hiero'>&#x13328;</samp></td>
			</tr>
			<tr>
				<td><kbd>X</kbd><kbd>r</kbd><kbd>d</kbd></td><td><samp>&#x1e96;rd</samp></td><td><samp class='hiero'>&#x13014;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>ḫ (x)</td>
			</tr>
			<tr>
				<td><kbd>x</kbd></td><td><samp>&#x1e2b;</samp></td><td><samp class='hiero'>&#x1340D;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>A</kbd></td><td><samp>&#x1e2b;&#xa723;</samp></td><td><samp class='hiero'>&#x131A9;<span class='cyc'>&rarr;</span>&#x131BC;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>A</kbd><kbd>i</kbd></td><td><samp>&#x1e2b;&#xa723;i</samp></td><td><samp class='hiero'>&#x130A1;<span class='cyc'>&rarr;</span>&#x1333D;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>A</kbd><kbd>s</kbd><kbd>t</kbd></td><td><samp>&#x1e2b;&#xa723;st</samp></td><td><samp class='hiero'>&#x13209;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>A</kbd><kbd>t</kbd></td><td><samp>&#x1e2b;&#xa723;t</samp></td><td><samp class='hiero'>&#x132AF;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>A</kbd><kbd>w</kbd><kbd>t</kbd></td><td><samp>&#x1e2b;&#xa723;wt</samp></td><td><samp class='hiero'>&#x132AF;<span class='cyc'>&rarr;</span>&#x132B0;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>a</kbd></td><td><samp>&#x1e2b;&#xa725;</samp></td><td><samp class='hiero'>&#x1320D;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>b</kbd><kbd>i</kbd></td><td><samp>&#x1e2b;bi</samp></td><td><samp class='hiero'>&#x13024;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>f</kbd><kbd>t</kbd><kbd>y</kbd></td><td><samp>&#x1e2b;fty</samp></td><td><samp class='hiero'>&#x1300F;<span class='cyc'>&rarr;</span>&#x13010;<span class='cyc'>&rarr;</span>&#x13011;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>m</kbd></td><td><samp>&#x1e2b;m</samp></td><td><samp class='hiero'>&#x1309C;<span class='cyc'>&rarr;</span>&#x132C9;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>n</kbd></td><td><samp>&#x1e2b;n</samp></td><td><samp class='hiero'>&#x1316F;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>n</kbd><kbd>m</kbd><kbd>s</kbd></td><td><samp>&#x1e2b;nms</samp></td><td><samp class='hiero'>&#x1300D;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>n</kbd><kbd>t</kbd></td><td><samp>&#x1e2b;nt</samp></td><td><samp class='hiero'>&#x13089;<span class='cyc'>&rarr;</span>&#x1308A;<span class='cyc'>&rarr;</span>&#x1329D;<span class='cyc'>&rarr;</span>&#x133C3;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>p</kbd><kbd>S</kbd></td><td><samp>&#x1e2b;p&#x0161;</samp></td><td><samp class='hiero'>&#x13117;<span class='cyc'>&rarr;</span>&#x13118;<span class='cyc'>&rarr;</span>&#x1331B;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>p</kbd><kbd>r</kbd></td><td><samp>&#x1e2b;pr</samp></td><td><samp class='hiero'>&#x131A3;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>p</kbd><kbd>r</kbd><kbd>S</kbd></td><td><samp>&#x1e2b;pr&#x0161;</samp></td><td><samp class='hiero'>&#x132D9;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>r</kbd></td><td><samp>&#x1e2b;r</samp></td><td><samp class='hiero'>&#x13012;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>r</kbd><kbd>p</kbd></td><td><samp>&#x1e2b;rp</samp></td><td><samp class='hiero'>&#x130A5;<span class='cyc'>&rarr;</span>&#x13302;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>r</kbd><kbd>w</kbd></td><td><samp>&#x1e2b;rw</samp></td><td><samp class='hiero'>&#x132A4;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>s</kbd><kbd>f</kbd></td><td><samp>&#x1e2b;sf</samp></td><td><samp class='hiero'>&#x13359;<span class='cyc'>&rarr;</span>&#x1335A;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>t</kbd></td><td><samp>&#x1e2b;t</samp></td><td><samp class='hiero'>&#x131B1;<span class='cyc'>&rarr;</span>&#x132AE;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>t</kbd><kbd>m</kbd></td><td><samp>&#x1e2b;tm</samp></td><td><samp class='hiero'>&#x132E8;<span class='cyc'>&rarr;</span>&#x132E9;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>w</kbd></td><td><samp>&#x1e2b;w</samp></td><td><samp class='hiero'>&#x130A4;<span class='cyc'>&rarr;</span>&#x132FD;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>w</kbd><kbd>i</kbd></td><td><samp>&#x1e2b;wi</samp></td><td><samp class='hiero'>&#x130A4;</samp></td>
			</tr>
			<tr>
				<td><kbd>x</kbd><kbd>w</kbd><kbd>s</kbd><kbd>i</kbd></td><td><samp>&#x1e2b;wsi</samp></td><td><samp class='hiero'>&#x13027;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>y</td>
			</tr>
			<tr>
				<td><kbd>y</kbd></td><td><samp>y</samp></td><td><samp class='hiero'>&#x133ED;</samp></td>
			</tr>

			<tr class='headrow'>
				<td colspan='3'>z</td>
			</tr>
			<tr>
				<td><kbd>z</kbd></td><td><samp>z</samp></td><td><samp class='hiero'>&#x13283;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>A</kbd></td><td><samp>z&#xa723;</samp></td><td><samp class='hiero'>&#x1316D;<span class='cyc'>&rarr;</span>&#x1341F;<span class='cyc'>&rarr;</span>&#x13420;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>A</kbd><kbd>b</kbd></td><td><samp>z&#xa723;b</samp></td><td><samp class='hiero'>&#x130E5;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>H</kbd></td><td><samp>z&#x1e25;</samp></td><td><samp class='hiero'>&#x13271;<span class='cyc'>&rarr;</span>&#x13272;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>S</kbd></td><td><samp>z&#x0161;</samp></td><td><samp class='hiero'>&#x133DE;<span class='cyc'>&rarr;</span>&#x133DF;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>S</kbd><kbd>S</kbd><kbd>t</kbd></td><td><samp>z&#x0161;&#x0161;t</samp></td><td><samp class='hiero'>&#x133E3;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>S</kbd><kbd>n</kbd></td><td><samp>z&#x0161;n</samp></td><td><samp class='hiero'>&#x131B8;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>b</kbd></td><td><samp>zb</samp></td><td><samp class='hiero'>&#x13284;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>i</kbd><kbd>n</kbd></td><td><samp>zin</samp></td><td><samp class='hiero'>&#x13315;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>m</kbd><kbd>A</kbd></td><td><samp>zm&#xa723;</samp></td><td><samp class='hiero'>&#x13125;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>m</kbd><kbd>n</kbd></td><td><samp>zmn</samp></td><td><samp class='hiero'>&#x13356;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>p</kbd></td><td><samp>zp</samp></td><td><samp class='hiero'>&#x13297;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>w</kbd></td><td><samp>zw</samp></td><td><samp class='hiero'>&#x131F3;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>w</kbd><kbd>n</kbd></td><td><samp>zwn</samp></td><td><samp class='hiero'>&#x13315;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>x</kbd><kbd>n</kbd><kbd>t</kbd></td><td><samp>z&#x1e2b;nt</samp></td><td><samp class='hiero'>&#x1327D;</samp></td>
			</tr>
			<tr>
				<td><kbd>z</kbd><kbd>z</kbd><kbd>m</kbd><kbd>t</kbd></td><td><samp>zzmt</samp></td><td><samp class='hiero'>&#x130D7;</samp></td>
			</tr>
		</tbody>
		</table>
	</div>
	<div id='guidegardiner' class='break'>
		<h3>Hieroglyphs Using Gardiner Numbers</h3>
		<p>This keyboard converts the following Gardiner numbers to hieroglyphs when you press <kbd>Spacebar</kbd>:</p>
		<table class='grid colleft'>
			<col class='signs' />
			<col class='keys' />
		<thead>
			<tr>
				<th>Sign</th><th>Number/Key</th>
			</tr>
		</thead>
		<tbody>
			<tr class='headrow'>
				<td colspan='2'>A<br/>Man + His Occupations</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13000;</samp></td><td><kbd>a</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13001;</samp></td><td><kbd>a</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13002;</samp></td><td><kbd>a</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13003;</samp></td><td><kbd>a</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13004;</samp></td><td><kbd>a</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13005;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13006;</samp></td><td><kbd>a</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13007;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13008;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13009;</samp></td><td><kbd>a</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1300A;</samp></td><td><kbd>a</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1300B;</samp></td><td><kbd>a</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1300C;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1300D;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1300E;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1300F;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13010;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13011;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13012;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13013;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13014;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13015;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13016;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13017;</samp></td><td><kbd>a</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13018;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13019;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1301A;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1301B;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1301C;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1301D;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1301E;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1301F;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13020;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13021;</samp></td><td><kbd>a</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13022;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13023;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13024;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13025;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13026;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13027;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13028;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13029;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1302A;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1302B;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1302C;</samp></td><td><kbd>a</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1302D;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1302E;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1302F;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13030;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13031;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13032;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13033;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13034;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13035;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13036;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13037;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13038;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13039;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1303A;</samp></td><td><kbd>a</kbd><kbd>4</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1303B;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1303C;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1303D;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1303E;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1303F;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13040;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13041;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13042;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13043;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13044;</samp></td><td><kbd>a</kbd><kbd>5</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13045;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13046;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13047;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13048;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13049;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1304A;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1304B;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1304C;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1304D;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1304E;</samp></td><td><kbd>a</kbd><kbd>6</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1304F;</samp></td><td><kbd>a</kbd><kbd>7</kbd><kbd>0</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>B<br/>Woman + Her Occupations</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13050;</samp></td><td><kbd>b</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13051;</samp></td><td><kbd>b</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13052;</samp></td><td><kbd>b</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13053;</samp></td><td><kbd>b</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13054;</samp></td><td><kbd>b</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13055;</samp></td><td><kbd>b</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13056;</samp></td><td><kbd>b</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13057;</samp></td><td><kbd>b</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13058;</samp></td><td><kbd>b</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13059;</samp></td><td><kbd>b</kbd><kbd>9</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>C<br/>Anthropomorphic Deities</td>
			</tr>
			<tr><td><samp class='hiero'>&#x1305A;</samp></td><td><kbd>c</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1305B;</samp></td><td><kbd>c</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1305C;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1305D;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1305E;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1305F;</samp></td><td><kbd>c</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13060;</samp></td><td><kbd>c</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13061;</samp></td><td><kbd>c</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13062;</samp></td><td><kbd>c</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13063;</samp></td><td><kbd>c</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13064;</samp></td><td><kbd>c</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13065;</samp></td><td><kbd>c</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13066;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13067;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13068;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13069;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1306A;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1306B;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1306C;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1306D;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1306E;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1306F;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13070;</samp></td><td><kbd>c</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13071;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13072;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13073;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13074;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13075;</samp></td><td><kbd>c</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>D<br/>Parts of the Human Body</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13076;</samp></td><td><kbd>d</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13077;</samp></td><td><kbd>d</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13078;</samp></td><td><kbd>d</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13079;</samp></td><td><kbd>d</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1307A;</samp></td><td><kbd>d</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1307B;</samp></td><td><kbd>d</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1307C;</samp></td><td><kbd>d</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1307D;</samp></td><td><kbd>d</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1307E;</samp></td><td><kbd>d</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1307F;</samp></td><td><kbd>d</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13080;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13081;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13082;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13083;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13084;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13085;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13086;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13087;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13088;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13089;</samp></td><td><kbd>d</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1308A;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1308B;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1308C;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1308D;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1308E;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1308F;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13090;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13091;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13092;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13093;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13094;</samp></td><td><kbd>d</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13095;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13096;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13097;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13098;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13099;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1309A;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1309B;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1309C;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1309D;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1309E;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1309F;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A0;</samp></td><td><kbd>d</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A1;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A2;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A3;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A4;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A5;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A6;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A7;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A8;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130A9;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130AA;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130AB;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130AC;</samp></td><td><kbd>d</kbd><kbd>4</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130AD;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130AE;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130AF;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B0;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B1;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B2;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B3;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B4;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B5;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>h</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B6;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>0</kbd><kbd>i</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B7;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B8;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130B9;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130BA;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130BB;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130BC;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130BD;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130BE;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130BF;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C0;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C1;</samp></td><td><kbd>d</kbd><kbd>5</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C2;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C3;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C4;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C5;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C6;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C7;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C8;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130C9;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130CA;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130CB;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130CC;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130CD;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130CE;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130CF;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D0;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D1;</samp></td><td><kbd>d</kbd><kbd>6</kbd><kbd>7</kbd><kbd>h</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>E<br/>Mammals</td>
			</tr>
			<tr><td><samp class='hiero'>&#x130D2;</samp></td><td><kbd>e</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D3;</samp></td><td><kbd>e</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D4;</samp></td><td><kbd>e</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D5;</samp></td><td><kbd>e</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D6;</samp></td><td><kbd>e</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D7;</samp></td><td><kbd>e</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D8;</samp></td><td><kbd>e</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130D9;</samp></td><td><kbd>e</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130DA;</samp></td><td><kbd>e</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130DB;</samp></td><td><kbd>e</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130DC;</samp></td><td><kbd>e</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130DD;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130DE;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130DF;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E0;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E1;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E2;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E3;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E4;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E5;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E6;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E7;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E8;</samp></td><td><kbd>e</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130E9;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130EA;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130EB;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130EC;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130ED;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130EE;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130EF;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F0;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F1;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F2;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F3;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F4;</samp></td><td><kbd>e</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F5;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F6;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F7;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F8;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130F9;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130FA;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130FB;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130FC;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130FD;</samp></td><td><kbd>e</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>F<br/>Parts of Mammals</td>
			</tr>
			<tr><td><samp class='hiero'>&#x130FE;</samp></td><td><kbd>f</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x130FF;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13100;</samp></td><td><kbd>f</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13101;</samp></td><td><kbd>f</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13102;</samp></td><td><kbd>f</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13103;</samp></td><td><kbd>f</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13104;</samp></td><td><kbd>f</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13105;</samp></td><td><kbd>f</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13106;</samp></td><td><kbd>f</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13107;</samp></td><td><kbd>f</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13108;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13109;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1310A;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1310B;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1310C;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1310D;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1310E;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1310F;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13110;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13111;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13112;</samp></td><td><kbd>f</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13113;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13114;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13115;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13116;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13117;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13118;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13119;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1311A;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1311B;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1311C;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1311D;</samp></td><td><kbd>f</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1311E;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1311F;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13120;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13121;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13122;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13123;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13124;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13125;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13126;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13127;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13128;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13129;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1312A;</samp></td><td><kbd>f</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1312B;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1312C;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1312D;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1312E;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1312F;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13130;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13131;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13132;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13133;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13134;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13135;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13136;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13137;</samp></td><td><kbd>f</kbd><kbd>4</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13138;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13139;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1313A;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1313B;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>1</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1313C;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>1</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1313D;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1313E;</samp></td><td><kbd>f</kbd><kbd>5</kbd><kbd>3</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>G<br/>Birds</td>
			</tr>
			<tr><td><samp class='hiero'>&#x1313F;</samp></td><td><kbd>g</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13140;</samp></td><td><kbd>g</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13141;</samp></td><td><kbd>g</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13142;</samp></td><td><kbd>g</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13143;</samp></td><td><kbd>g</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13144;</samp></td><td><kbd>g</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13145;</samp></td><td><kbd>g</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13146;</samp></td><td><kbd>g</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13147;</samp></td><td><kbd>g</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13148;</samp></td><td><kbd>g</kbd><kbd>7</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13149;</samp></td><td><kbd>g</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1314A;</samp></td><td><kbd>g</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1314B;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1314C;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1314D;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1314E;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1314F;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13150;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13151;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13152;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13153;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13154;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13155;</samp></td><td><kbd>g</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13156;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13157;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13158;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13159;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1315A;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1315B;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1315C;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1315D;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1315E;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1315F;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13160;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13161;</samp></td><td><kbd>g</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13162;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13163;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13164;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13165;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13166;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13167;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13168;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13169;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1316A;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1316B;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1316C;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1316D;</samp></td><td><kbd>g</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1316E;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1316F;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13170;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13171;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13172;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13173;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13174;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13175;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13176;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13177;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13178;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13179;</samp></td><td><kbd>g</kbd><kbd>4</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1317A;</samp></td><td><kbd>g</kbd><kbd>5</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1317B;</samp></td><td><kbd>g</kbd><kbd>5</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1317C;</samp></td><td><kbd>g</kbd><kbd>5</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1317D;</samp></td><td><kbd>g</kbd><kbd>5</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1317E;</samp></td><td><kbd>g</kbd><kbd>5</kbd><kbd>4</kbd></td></tr>
		</tbody>
		</table>

		<table class='grid colright'>
			<col class='signs' />
			<col class='keys' />
		<thead>
			<tr>
				<th>Sign</th><th>Number/Key</th>
			</tr>
		</thead>
		<tbody>
			<tr class='headrow'>
				<td colspan='2'>H<br/>Parts of Birds</td>
			</tr>
			<tr><td><samp class='hiero'>&#x1317F;</samp></td><td><kbd>h</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13180;</samp></td><td><kbd>h</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13181;</samp></td><td><kbd>h</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13182;</samp></td><td><kbd>h</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13183;</samp></td><td><kbd>h</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13184;</samp></td><td><kbd>h</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13185;</samp></td><td><kbd>h</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13186;</samp></td><td><kbd>h</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13187;</samp></td><td><kbd>h</kbd><kbd>8</kbd></td></tr>


			<tr class='headrow'>
				<td colspan='2'>I<br/>Amphibians + Reptiles</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13188;</samp></td><td><kbd>i</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13189;</samp></td><td><kbd>i</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1318A;</samp></td><td><kbd>i</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1318B;</samp></td><td><kbd>i</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1318C;</samp></td><td><kbd>i</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1318D;</samp></td><td><kbd>i</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1318E;</samp></td><td><kbd>i</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1318F;</samp></td><td><kbd>i</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13190;</samp></td><td><kbd>i</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13191;</samp></td><td><kbd>i</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13192;</samp></td><td><kbd>i</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13193;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13194;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13195;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13196;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13197;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13198;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13199;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1319A;</samp></td><td><kbd>i</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>K<br/>Fish + Parts of Fish</td>
			</tr>
			<tr><td><samp class='hiero'>&#x1319B;</samp></td><td><kbd>k</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1319C;</samp></td><td><kbd>k</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1319D;</samp></td><td><kbd>k</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1319E;</samp></td><td><kbd>k</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1319F;</samp></td><td><kbd>k</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A0;</samp></td><td><kbd>k</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A1;</samp></td><td><kbd>k</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A2;</samp></td><td><kbd>k</kbd><kbd>8</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>L<br/>Invertebrates +<br/>Other Animals</td>
			</tr>
			<tr><td><samp class='hiero'>&#x131A3;</samp></td><td><kbd>l</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A4;</samp></td><td><kbd>l</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A5;</samp></td><td><kbd>l</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A6;</samp></td><td><kbd>l</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A7;</samp></td><td><kbd>l</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A8;</samp></td><td><kbd>l</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131A9;</samp></td><td><kbd>l</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131AA;</samp></td><td><kbd>l</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131AB;</samp></td><td><kbd>l</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131AC;</samp></td><td><kbd>l</kbd><kbd>8</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>M<br/>Trees + Other Plants</td>
			</tr>
			<tr><td><samp class='hiero'>&#x131AD;</samp></td><td><kbd>m</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131AE;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131AF;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B0;</samp></td><td><kbd>m</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B1;</samp></td><td><kbd>m</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B2;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B3;</samp></td><td><kbd>m</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B4;</samp></td><td><kbd>m</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B5;</samp></td><td><kbd>m</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B6;</samp></td><td><kbd>m</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B7;</samp></td><td><kbd>m</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B8;</samp></td><td><kbd>m</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131B9;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131BA;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131BB;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131BC;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131BD;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131BE;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131BF;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C0;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C1;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C2;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C3;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C4;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>2</kbd><kbd>h</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C5;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C6;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C7;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C8;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131C9;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131CA;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131CB;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131CC;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131CD;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131CE;</samp></td><td><kbd>m</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131CF;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D0;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D1;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D2;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D3;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D4;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D5;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D6;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D7;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D8;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131D9;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131DA;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131DB;</samp></td><td><kbd>m</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131DC;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131DD;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131DE;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131DF;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E0;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E1;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E2;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>3</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E3;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E4;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E5;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E6;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E7;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E8;</samp></td><td><kbd>m</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131E9;</samp></td><td><kbd>m</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131EA;</samp></td><td><kbd>m</kbd><kbd>4</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131EB;</samp></td><td><kbd>m</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131EC;</samp></td><td><kbd>m</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131ED;</samp></td><td><kbd>m</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131EE;</samp></td><td><kbd>m</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>N<br/>Sky, Earth + Water</td>
			</tr>
			<tr><td><samp class='hiero'>&#x131EF;</samp></td><td><kbd>n</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F0;</samp></td><td><kbd>n</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F1;</samp></td><td><kbd>n</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F2;</samp></td><td><kbd>n</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F3;</samp></td><td><kbd>n</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F4;</samp></td><td><kbd>n</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F5;</samp></td><td><kbd>n</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F6;</samp></td><td><kbd>n</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F7;</samp></td><td><kbd>n</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F8;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131F9;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131FA;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131FB;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131FC;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131FD;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131FE;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x131FF;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13200;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13201;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13202;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>8</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13203;</samp></td><td><kbd>n</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13204;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13205;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13206;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13207;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13208;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13209;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1320A;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1320B;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1320C;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1320D;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1320E;</samp></td><td><kbd>n</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1320F;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13210;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13211;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13212;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13213;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13214;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13215;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13216;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13217;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13218;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13219;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1321A;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1321B;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1321C;</samp></td><td><kbd>n</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1321D;</samp></td><td><kbd>n</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1321E;</samp></td><td><kbd>n</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1321F;</samp></td><td><kbd>n</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13220;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13221;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13222;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13223;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13224;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13225;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13226;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13227;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13228;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13229;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1322A;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1322B;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1322C;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1322D;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1322E;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1322F;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13230;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13231;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13232;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13233;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13234;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13235;</samp></td><td><kbd>n</kbd><kbd>l</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13236;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13237;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13238;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13239;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1323A;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1323B;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1323C;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1323D;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1323E;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1323F;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13240;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13241;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13242;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13243;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13244;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13245;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13246;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13247;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13248;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13249;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1324A;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1324B;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1324C;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1324D;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1324E;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1324F;</samp></td><td><kbd>n</kbd><kbd>u</kbd><kbd>2</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>O<br/>Buildings + <br/>Parts of Buildings</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13250;</samp></td><td><kbd>o</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13251;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13252;</samp></td><td><kbd>o</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13253;</samp></td><td><kbd>o</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13254;</samp></td><td><kbd>o</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13255;</samp></td><td><kbd>o</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13256;</samp></td><td><kbd>o</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13257;</samp></td><td><kbd>o</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13258;</samp></td><td><kbd>o</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13259;</samp></td><td><kbd>o</kbd><kbd>6</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1325A;</samp></td><td><kbd>o</kbd><kbd>6</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1325B;</samp></td><td><kbd>o</kbd><kbd>6</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1325C;</samp></td><td><kbd>o</kbd><kbd>6</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1325D;</samp></td><td><kbd>o</kbd><kbd>6</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1325E;</samp></td><td><kbd>o</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1325F;</samp></td><td><kbd>o</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13260;</samp></td><td><kbd>o</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13261;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13262;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13263;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>0</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13264;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>0</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13265;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13266;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13267;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13268;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13269;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1326A;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1326B;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1326C;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1326D;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1326E;</samp></td><td><kbd>o</kbd><kbd>1</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1326F;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13270;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13271;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13272;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13273;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13274;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13275;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13276;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13277;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13278;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13279;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1327A;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1327B;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1327C;</samp></td><td><kbd>o</kbd><kbd>2</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1327D;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1327E;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1327F;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13280;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13281;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13282;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13283;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13284;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13285;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13286;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13287;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>6</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13288;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>6</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13289;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>6</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1328A;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1328B;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1328C;</samp></td><td><kbd>o</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1328D;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1328E;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1328F;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13290;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13291;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13292;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13293;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13294;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13295;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13296;</samp></td><td><kbd>o</kbd><kbd>4</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13297;</samp></td><td><kbd>o</kbd><kbd>5</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13298;</samp></td><td><kbd>o</kbd><kbd>5</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13299;</samp></td><td><kbd>o</kbd><kbd>5</kbd><kbd>0</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1329A;</samp></td><td><kbd>o</kbd><kbd>5</kbd><kbd>1</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>P<br/>Ships + Parts of Ships</td>
			</tr>
			<tr><td><samp class='hiero'>&#x1329B;</samp></td><td><kbd>p</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1329C;</samp></td><td><kbd>p</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1329D;</samp></td><td><kbd>p</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1329E;</samp></td><td><kbd>p</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1329F;</samp></td><td><kbd>p</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A0;</samp></td><td><kbd>p</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A1;</samp></td><td><kbd>p</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A2;</samp></td><td><kbd>p</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A3;</samp></td><td><kbd>p</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A4;</samp></td><td><kbd>p</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A5;</samp></td><td><kbd>p</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A6;</samp></td><td><kbd>p</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A7;</samp></td><td><kbd>p</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>Q<br/>Furniture</td>
			</tr>
			<tr><td><samp class='hiero'>&#x132A8;</samp></td><td><kbd>q</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132A9;</samp></td><td><kbd>q</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132AA;</samp></td><td><kbd>q</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132AB;</samp></td><td><kbd>q</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132AC;</samp></td><td><kbd>q</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132AD;</samp></td><td><kbd>q</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132AE;</samp></td><td><kbd>q</kbd><kbd>7</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>R<br/>Temple</td>
			</tr>
			<tr><td><samp class='hiero'>&#x132AF;</samp></td><td><kbd>r</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B0;</samp></td><td><kbd>r</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B1;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B2;</samp></td><td><kbd>r</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B3;</samp></td><td><kbd>r</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B4;</samp></td><td><kbd>r</kbd><kbd>3</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B5;</samp></td><td><kbd>r</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B6;</samp></td><td><kbd>r</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B7;</samp></td><td><kbd>r</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B8;</samp></td><td><kbd>r</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132B9;</samp></td><td><kbd>r</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132BA;</samp></td><td><kbd>r</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132BB;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132BC;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132BD;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132BE;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132BF;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C0;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C1;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C2;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C3;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C4;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C5;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C6;</samp></td><td><kbd>r</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C7;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C8;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132C9;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132CA;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132CB;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132CC;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132CD;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132CE;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132CF;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D0;</samp></td><td><kbd>r</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
		</tbody>
		</table>

		<table class='grid colright'>
			<col class='signs' />
			<col class='keys' />
		<thead>
			<tr>
				<th>Sign</th><th>Number/Key</th>
			</tr>
		</thead>
		<tbody>
			<tr class='headrow'>
				<td colspan='2'>S<br/>Crowns + Staffs</td>
			</tr>
			<tr><td><samp class='hiero'>&#x132D1;</samp></td><td><kbd>s</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D2;</samp></td><td><kbd>s</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D3;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D4;</samp></td><td><kbd>s</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D5;</samp></td><td><kbd>s</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D6;</samp></td><td><kbd>s</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D7;</samp></td><td><kbd>s</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D8;</samp></td><td><kbd>s</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132D9;</samp></td><td><kbd>s</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132DA;</samp></td><td><kbd>s</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132DB;</samp></td><td><kbd>s</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132DC;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132DD;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132DE;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132DF;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E0;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E1;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E2;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>4</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E3;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E4;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E5;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E6;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E7;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E8;</samp></td><td><kbd>s</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132E9;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132EA;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132EB;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132EC;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132ED;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132EE;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132EF;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F0;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F1;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>6</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F2;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F3;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F4;</samp></td><td><kbd>s</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F5;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F6;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F7;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F8;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132F9;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132FA;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132FB;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132FC;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132FD;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132FE;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x132FF;</samp></td><td><kbd>s</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13300;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13301;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13302;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13303;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13304;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13305;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13306;</samp></td><td><kbd>s</kbd><kbd>4</kbd><kbd>6</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>T<br/>War, Hunting + Butchery</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13307;</samp></td><td><kbd>t</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13308;</samp></td><td><kbd>t</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13309;</samp></td><td><kbd>t</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1330A;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1330B;</samp></td><td><kbd>t</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1330C;</samp></td><td><kbd>t</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1330D;</samp></td><td><kbd>t</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1330E;</samp></td><td><kbd>t</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1330F;</samp></td><td><kbd>t</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13310;</samp></td><td><kbd>t</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13311;</samp></td><td><kbd>t</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13312;</samp></td><td><kbd>t</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13313;</samp></td><td><kbd>t</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13314;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13315;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13316;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13317;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13318;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13319;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1331A;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1331B;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1331C;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1331D;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1331E;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1331F;</samp></td><td><kbd>t</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13320;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13321;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13322;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13323;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13324;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13325;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13326;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13327;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13328;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13329;</samp></td><td><kbd>t</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1332A;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1332B;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1332C;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1332D;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1332E;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1332F;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13330;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13331;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13332;</samp></td><td><kbd>t</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>U<br/>Agriculture + Crafts</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13333;</samp></td><td><kbd>u</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13334;</samp></td><td><kbd>u</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13335;</samp></td><td><kbd>u</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13336;</samp></td><td><kbd>u</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13337;</samp></td><td><kbd>u</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13338;</samp></td><td><kbd>u</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13339;</samp></td><td><kbd>u</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1333A;</samp></td><td><kbd>u</kbd><kbd>6</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1333B;</samp></td><td><kbd>u</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1333C;</samp></td><td><kbd>u</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1333D;</samp></td><td><kbd>u</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1333E;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1333F;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13340;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13341;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13342;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13343;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13344;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13345;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13346;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13347;</samp></td><td><kbd>u</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13348;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13349;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1334A;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1334B;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1334C;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1334D;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1334E;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1334F;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13350;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13351;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13352;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13353;</samp></td><td><kbd>u</kbd><kbd>2</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13354;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13355;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13356;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13357;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13358;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13359;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1335A;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1335B;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1335C;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1335D;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1335E;</samp></td><td><kbd>u</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1335F;</samp></td><td><kbd>u</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13360;</samp></td><td><kbd>u</kbd><kbd>4</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13361;</samp></td><td><kbd>u</kbd><kbd>4</kbd><kbd>2</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>V<br/>Rope, Fiber, Baskets, etc</td>
			</tr>
			<tr><td><samp class='hiero'>&#x13362;</samp></td><td><kbd>v</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13363;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13364;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13365;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13366;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13367;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13368;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13369;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1336A;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>h</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1336B;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>i</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1336C;</samp></td><td><kbd>v</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1336D;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1336E;</samp></td><td><kbd>v</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1336F;</samp></td><td><kbd>v</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13370;</samp></td><td><kbd>v</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13371;</samp></td><td><kbd>v</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13372;</samp></td><td><kbd>v</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13373;</samp></td><td><kbd>v</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13374;</samp></td><td><kbd>v</kbd><kbd>7</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13375;</samp></td><td><kbd>v</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13376;</samp></td><td><kbd>v</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13377;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13378;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13379;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1337A;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>1</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1337B;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>1</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1337C;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1337D;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1337E;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>2</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1337F;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13380;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13381;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13382;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13383;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13384;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13385;</samp></td><td><kbd>v</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13386;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13387;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13388;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13389;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1338A;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1338B;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1338C;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1338D;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1338E;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>h</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1338F;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>i</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13390;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>j</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13391;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>k</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13392;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>0</kbd><kbd>l</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13393;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13394;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13395;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13396;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13397;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13398;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13399;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1339A;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1339B;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1339C;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1339D;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1339E;</samp></td><td><kbd>v</kbd><kbd>2</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1339F;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A0;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A1;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A2;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A3;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A4;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A5;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A6;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A7;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A8;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133A9;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133AA;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133AB;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133AC;</samp></td><td><kbd>v</kbd><kbd>3</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133AD;</samp></td><td><kbd>v</kbd><kbd>4</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133AE;</samp></td><td><kbd>v</kbd><kbd>4</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>W<br/>Stoneware + Pottery</td>
			</tr>
			<tr><td><samp class='hiero'>&#x133AF;</samp></td><td><kbd>w</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B0;</samp></td><td><kbd>w</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B1;</samp></td><td><kbd>w</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B2;</samp></td><td><kbd>w</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B3;</samp></td><td><kbd>w</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B4;</samp></td><td><kbd>w</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B5;</samp></td><td><kbd>w</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B6;</samp></td><td><kbd>w</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B7;</samp></td><td><kbd>w</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B8;</samp></td><td><kbd>w</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133B9;</samp></td><td><kbd>w</kbd><kbd>9</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133BA;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133BB;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>0</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133BC;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133BD;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133BE;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133BF;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C0;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C1;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C2;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C3;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C4;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C5;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C6;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C7;</samp></td><td><kbd>w</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C8;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133C9;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133CA;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133CB;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133CC;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133CD;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133CE;</samp></td><td><kbd>w</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>


			<tr class='headrow'>
				<td colspan='2'>X<br/>Loaves + Cakes</td>
			</tr>
			<tr><td><samp class='hiero'>&#x133CF;</samp></td><td><kbd>x</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D0;</samp></td><td><kbd>x</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D1;</samp></td><td><kbd>x</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D2;</samp></td><td><kbd>x</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D3;</samp></td><td><kbd>x</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D4;</samp></td><td><kbd>x</kbd><kbd>4</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D5;</samp></td><td><kbd>x</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D6;</samp></td><td><kbd>x</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D7;</samp></td><td><kbd>x</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D8;</samp></td><td><kbd>x</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133D9;</samp></td><td><kbd>x</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133DA;</samp></td><td><kbd>x</kbd><kbd>8</kbd><kbd>a</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>Y<br/>Writing, Games + Music</td>
			</tr>
			<tr><td><samp class='hiero'>&#x133DB;</samp></td><td><kbd>y</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133DC;</samp></td><td><kbd>y</kbd><kbd>1</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133DD;</samp></td><td><kbd>y</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133DE;</samp></td><td><kbd>y</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133DF;</samp></td><td><kbd>y</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E0;</samp></td><td><kbd>y</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E1;</samp></td><td><kbd>y</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E2;</samp></td><td><kbd>y</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E3;</samp></td><td><kbd>y</kbd><kbd>8</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>Z<br/>Geometric Figures</td>
			</tr>
			<tr><td><samp class='hiero'>&#x133E4;</samp></td><td><kbd>z</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E5;</samp></td><td><kbd>z</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E6;</samp></td><td><kbd>z</kbd><kbd>2</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E7;</samp></td><td><kbd>z</kbd><kbd>2</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E8;</samp></td><td><kbd>z</kbd><kbd>2</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133E9;</samp></td><td><kbd>z</kbd><kbd>2</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133EA;</samp></td><td><kbd>z</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133EB;</samp></td><td><kbd>z</kbd><kbd>3</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133EC;</samp></td><td><kbd>z</kbd><kbd>3</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133ED;</samp></td><td><kbd>z</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133EE;</samp></td><td><kbd>z</kbd><kbd>4</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133EF;</samp></td><td><kbd>z</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F0;</samp></td><td><kbd>z</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F1;</samp></td><td><kbd>z</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F2;</samp></td><td><kbd>z</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F3;</samp></td><td><kbd>z</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F4;</samp></td><td><kbd>z</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F5;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F6;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F7;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F8;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133F9;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133FA;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133FB;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133FC;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133FD;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133FE;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x133FF;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13400;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13401;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13402;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>h</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13403;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>5</kbd><kbd>i</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13404;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13405;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13406;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13407;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>c</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13408;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>d</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13409;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>e</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1340A;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>f</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1340B;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>g</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1340C;</samp></td><td><kbd>z</kbd><kbd>1</kbd><kbd>6</kbd><kbd>h</kbd></td></tr>

			<tr class='headrow'>
				<td colspan='2'>AA<br/>Unclassified</td>
			</tr>
			<tr><td><samp class='hiero'>&#x1340D;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1340E;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1340F;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13410;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13411;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13412;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13413;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13414;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>7</kbd><kbd>a</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13415;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>7</kbd><kbd>b</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13416;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13417;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13418;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13419;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1341A;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1341B;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1341C;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1341D;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1341E;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1341F;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13420;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13421;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>1</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13422;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13423;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13424;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>2</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13425;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>3</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13426;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>4</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13427;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>5</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13428;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>6</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x13429;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>7</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1342A;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>8</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1342B;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>2</kbd><kbd>9</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1342C;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>3</kbd><kbd>0</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1342D;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>3</kbd><kbd>1</kbd></td></tr>
			<tr><td><samp class='hiero'>&#x1342E;</samp></td><td><kbd>a</kbd><kbd>a</kbd><kbd>3</kbd><kbd>2</kbd></td></tr>
		</tbody>
		</table>
	</div>
  </section>
  <section id='additional' class='break'>
	<h2>Additional Information</h2>
    <div id="methods">
		<h3>Available Methods for Typing Hieroglyphs</h3>
		<p>There are three ways to display Ancient Egyptian on a computer:</p>

				<h4>Unicode</h4>
				<p>This keyboard uses <a href='http://unicode.org/standard/standard.html' target='blank'>Unicode</a>, the international standard for computer encoding all the world's languages. Hieroglyphs written in Unicode can be treated just like Latin text. They can be searched, sorted, copied and pasted, entered into forms on websites, and typed into word processors.	The unfortunate consequence of this versatility is that Unicode text cannot be displayed like real Ancient Egyptian without a great deal of effort. In standard use, anything typed using Unicode will display as a single-file line of characters like a modern alphabet.</p>
				<p>This isn't a problem for many situations. If you're sending a short bit of text to a colleague, and you're more concerned with content than formatting, Unicode allows you to type hieroglyphs directly into an email. It also makes algorithmic processing of Ancient Egyptian texts considerably easier. On the other hand, if you need the glyphs to be stacked and aligned properly, you should consider the next option.</p>

			<h4>A Hieroglyphic Text Editor</h4>
				<p>This is the best way to input hieroglyphs when formatting is a primary concern, i.e. when the text has to look exactly as it would have on a temple wall. These programs provide full control over the placement, size, and orientation of each glyph. The downside is that any text made in a program like this has to be rendered as a graphic before it can be used anywhere else. Once it becomes a graphic, it is impossible to make any changes to the text. It also cannot be entered into text boxes, searched, processed by a computer program, etc.</p>
				<p>If proper formatting is an important issue, try using JSesh instead of this keyboard. JSesh is a free, open-source hieroglyphic text editor that provides an extraordinary level of control over formatting.</p>
				<p><a href="http://jsesh.qenherkhopeshef.org/">Download JSesh</a></p>

				<h4>An Image</h4>
				<p>Although it is possible to display hieroglyphs by putting them all in a jpeg, and though many websites still do this, this approach should be avoided at all costs. It is a terrible way to present information. The availability of the first two options should preclude the need to ever display Ancient Egyptian this way ever again.</p>

	</div>
	<div id='author'>
		<h3>Keyboard Authorship</h3>
		<p>This keyboard was created by Christian Casey for Right Said Languages, a
		project designed to provide free resources to students of Ancient Egyptian
		languages.</p>
	</div>
  </section>
