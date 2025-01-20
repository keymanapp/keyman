// A separate build-product needed to support KMW when running in legacy ES5 mode for
// non-updated Chrome WebViews (like with Android API 21 / 5.0)
// @ts-ignore
import { default as Map } from "core-js/stable/map";
// @ts-ignore
import { default as Symbol } from "core-js/stable/symbol";

// @ts-ignore
Window['Symbol'] = Symbol;
// @ts-ignore
Window['Map'] = Map;