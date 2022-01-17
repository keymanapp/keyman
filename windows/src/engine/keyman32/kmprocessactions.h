
/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#ifndef _KMPROCESSACTIONS_H
#define _KMPROCESSACTIONS_h

/**
 * Process the actions queued in the core processor
 *
 * @param  [in, out] emitKeyStroke  is set to true if requested by the core action queue
 * @return BOOL  True if actions were successfully processed
 */
BOOL ProcessActions(BOOL* emitKeyStroke);

/**
 * This function process the actions queued in the core processor in
 * the non updateable parse of a keystroke.
 * Emit keystroke and capslock are required to be processed in this phase.
 *
 * @param  [in, out] emitKeyStroke  is set to true if requested by the core action queue
 * @return BOOL  True if actions were successfully processed
 */
BOOL ProcessActionsTestParse(BOOL* emitKeyStroke);

#endif
