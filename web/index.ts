import type { flatten as TSFlatten } from "../pkg/index.d.ts";
import { loadBinding } from "@node-rs/helper";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

type T = {
  flatten: typeof TSFlatten;
};

// Node version
const nodeABI = process.versions.modules;

const binding_path = path.join(__dirname, "node", nodeABI);
const binding = loadBinding(binding_path, "type-flat", "type-flat") as {
  [k in keyof T]: T[k];
};

export const { flatten } = binding;
