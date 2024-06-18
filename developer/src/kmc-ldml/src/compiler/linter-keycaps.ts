import { Linter } from "./linter.js";

/** A linter concerned with the display of keycaps */
export class LinterKeycaps extends Linter {
    public async lint(): Promise<boolean> {
        return true;
    }
}
