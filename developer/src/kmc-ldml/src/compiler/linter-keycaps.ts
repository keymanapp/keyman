import { MarkerParser } from "@keymanapp/common-types";
import { Linter } from "./linter.js";
import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";

/** A linter concerned with the display of keycaps */
export class LinterKeycaps extends Linter {

    private findDisp(keyId: string, output: string) {
        for (const disp of this.kmx.kmxplus.disp.disps) {
            const {id, to} = disp;
            if (id.value !== '' && id.value === keyId) {
                return disp;
            }

            if (to.value !== '' && to.value === output) {
                return disp;
            }
        }
        return null;
    }

    public async lint(): Promise<boolean> {
        const ANY_MARKER_REGEX = new RegExp(MarkerParser.ANY_MARKER_MATCH,'g');
        // Are there any keys which:
        // 0. Are present in the layout, flicks or gestures, AND
        // 1. Consist entirely of marker output, AND
        // 2. Are not matched by any display id= OR display output=

        for (const key of this.kmx.kmxplus.keys.keys) {
            const { id, to } = key;
            const nonMarkerOutput = to.value.replaceAll(ANY_MARKER_REGEX, '');

            if (key['switch'].value !== '' && to.value === '') {
                // switch with no output
                const disp = this.findDisp(id.value, to.value);

                if (!disp) {
                    this.callbacks.reportMessage(LdmlCompilerMessages.Hint_NoDisplayForSwitch({ id: id.value }));
                }
            } else if (to.value !== '' && nonMarkerOutput === '') {
                // has output, but only markers
                const disp = this.findDisp(id.value, to.value);
                if (!disp) {
                    this.callbacks.reportMessage(LdmlCompilerMessages.Hint_NoDisplayForMarker({ id: id.value }));
                }
            }
        }

        return true;
    }
}
