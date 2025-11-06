/**
 * Yields a type matching the (likely) JSON representation of the object after a JSON parse.
 */
export type JSONObject<Type> = {
    [Property in keyof Type]: Type[Property];
};