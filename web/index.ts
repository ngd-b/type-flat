import type { flatten as TSFlatten } from "../pkg/index.d.ts";
import { loadBinding } from "@node-rs/helper";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

type T = {
  flatten: typeof TSFlatten;
};

const binding = loadBinding(__dirname, "type-flat", "type_flat") as {
  [k in keyof T]: T[k];
};

export const { flatten } = binding;
