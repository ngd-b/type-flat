import type { flatten as TSFlatten } from "../pkg/index.d.ts";
import { loadBinding } from "@node-rs/helper";

type T = {
  flatten: typeof TSFlatten;
};

const binding = loadBinding(__dirname, "type-flat", "type_flat") as {
  [k in keyof T]: T[k];
};

export const { flatten } = binding;
