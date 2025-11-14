import type { flatten as TSFlatten } from "../pkg/index.d.ts";
// import { loadBinding } from "@node-rs/helper";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const require = createRequire(import.meta.url);

type T = {
  flatten: typeof TSFlatten;
};

// Node version
const nodeABI = process.versions.modules;
// Platform
const platform = process.platform;
const arch = process.arch;

const binding_path = path.join(
  __dirname,
  "node",
  nodeABI,
  `type-flat.${platform}-${arch}.node`
);
const binding = require(binding_path) as {
  [k in keyof T]: T[k];
};

export const { flatten } = binding;
