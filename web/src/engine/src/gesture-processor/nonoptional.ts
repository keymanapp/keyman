export type Nonoptional<Type> = {
  [Property in keyof Type]-?: Type[Property];
};