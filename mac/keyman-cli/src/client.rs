/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * The `KeymanClient` trait is the seam between the CLI dispatcher and
 * the per-platform backend. Stage 1 only has a macOS implementation;
 * the Windows and Linux backends will land later behind the same trait.
 *
 * Keyboard-id resolution (free-form string -> canonical id) is a
 * CLI-level concern handled by `resolver.rs`. This trait deals only in
 * already-resolved `KeyboardId` values.
 */

use crate::error::Result;
use crate::keyboard::Keyboard;
use crate::keyboard::{ActivateOutcome, KeyboardId, SelectOutcome, Status};

pub trait KeymanClient {
    /// Every keyboard listed in Keyman's `KMActiveKeyboardsKey`, with
    /// the currently selected one (per `KMSelectedKeyboardKey`) marked.
    fn list_keyboards(&self) -> Result<Vec<Keyboard>>;

    /// Combined OS-level + Keyman-level state snapshot.
    fn status(&self) -> Result<Status>;

    /// Ensure Keyman is the currently active OS-level input source.
    /// Idempotent; reports what changed.
    fn activate(&self) -> Result<ActivateOutcome>;

    /// Switch Keyman's internally-selected keyboard to `id`. Always
    /// also activates Keyman as the OS-level input source, because
    /// selecting a Keyman keyboard while another IM is active is
    /// meaningless.
    fn select_keyboard(&self, id: &KeyboardId) -> Result<SelectOutcome>;
}
