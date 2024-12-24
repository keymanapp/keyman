/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

/**
 * interface for class to manage the compiler.
 * This is abstract because the ES module needs to be loaded asynchronously.
 */
export interface LDMLCompilerManager {
    /** validate that everything loaded properly. idempotent */
    init() : Promise<any>;
    /** Compile a file. Returns KMXPlus file. */
    compile(filename: string) : Promise<any>;
};

/** load concrete instance from module */
export async function getLDMLCompilerManager() : Promise<LDMLCompilerManager>  {
    const { KmcLdmlManager } = await import('./kmcLdmlCompilerManager.mjs');
    return new KmcLdmlManager();
}
