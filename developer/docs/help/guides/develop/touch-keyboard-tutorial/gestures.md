---
title: Using Gestures in Keyman Developer
---

Keyman Developer offers **Longpress** keys, **Flicks**, and **Multitaps** for touch keyboard layout. To allow gestures onto your keyboard, go to the Touch layout tab and define the keys as follows:

<img src="../../../images/touch-layout-gestures.jpg" alt="Keyman Developer touch layout gestures" width="70%"/>

## Tips

***Flicks** and **long presses** can both be used in a single keyboard. In this scenario, it is recommended to use flicks in only one direction—south (down). Long presses work well alongside this pattern. See, for example, [sil_euro_latin](../../../../keyboard/sil_euro_latin/3.0.3/sil_euro_latin#toc-mobile-keyboard-layout). 
    * Another helpful tip seen in the sil_euro_latin keyboard is using south-flicks on the base and shift layers corresponding to the numeric/symbol layer, this aids in both memorization and discovery.
* **Flicks** in multiple directions are possible, but currently Keyman does not support hints for multiple directions, so they are not very discoverable.
* **Long presses** are more discoverable, but **flicks** are faster. We suggest using south flicks along with long presses, and including the flick output in the long press for maximum discoverability and optimum ergonomics.

## Caution

* **Flicks** near screen edges tend to be problematic--once you drag off the screen, then Keyman can no longer tell that you are flicking (or you may end up activating system controls on the bottom edge):
  * On the bottom row, south flicks are not recommended.
  * On the top row, we have the banner space reserved in order to be able to support flicks and long presses up into that region of the screen.
  * Left/right screen edges, horizontal flicks are not the best practice.

## See also

* The [Winchus keyboard](../../../../keyboard/winchus/1.4/winchus#guia-rapida-celular)
* The [GFF Amharic keyboard](../../../../keyboard/gff_amharic/3.2.1/gff_amharic#toc-using-this-keyboard)