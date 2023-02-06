export default interface FloatingOSKCookie {
  visible: '0' | '1';
  userSet: '0' | '1';
  left: string;
  top: string;
  width?: string;
  height?: string;

  _version: string;
}