#[no_mangle]
pub extern "C" fn rust_mock_process_event(vk: u16, modifier: u16, is_key_down: u8) -> u32 {
  return keyman_keyboard_processor::rust_mock_process_event(vk, modifier, is_key_down);
}
