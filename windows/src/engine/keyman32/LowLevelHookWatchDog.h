/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-17
 */

/**
 * @brief Watch for scenarios where the low level keyboard hook may be
 * uninstalled by Windows, e.g. when there is heavy system load, and
 * reinstall it.
 */
class LowLevelHookWatchDog {
public:
  /**
   * @brief Update the watchdog timestamp to current time, because the low
   *        level hook is receiving messages successfully
   */
  static void HookIsAlive();

  /**
   * @brief Update the watchdog GetMessageProc timestamp to current time;
   *        this is called by the Keyman_WatchDogKeyEvent function, when
   *        a message posted from the GetMessageProc hook is received by
   *        keyman.exe.
   */
  static void KeyEventReceivedInGetMessageProc();

private:
  static bool CheckIfHookIsAlive();
  static void ReinstallHook();
};

