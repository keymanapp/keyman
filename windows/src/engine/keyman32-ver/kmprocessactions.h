
/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Description: Used for apply core processor actions to
 *              Keyman for Windows engine.
 */
#ifndef _KMPROCESSACTIONS_H
#define _KMPROCESSACTIONS_H

/**
 * Process the actions queued in the core processor
 *
 * @param  [in, out] emitKeyStroke  is set to true if requested by the core action queue
 * @return BOOL  True if actions were successfully processed
 */
BOOL ProcessActions(BOOL* emitKeyStroke);

/**
 * This function process the actions queued in the core processor in
 * the non-updateable parse of a keystroke.
 * Emit keystroke, capslock and possibly invalidate key stroke are required to be processed in this parse.
 *
 * @param  [in, out] emitKeyStroke  is set to true if requested by the core action queue
 * @return BOOL  True if actions were successfully processed
 */
BOOL ProcessActionsNonUpdatableParse(BOOL* emitKeyStroke);

#endif
