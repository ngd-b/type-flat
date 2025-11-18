import type { Flatten as TSFlatten } from "../pkg/index.d.ts";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const require = createRequire(import.meta.url);

// Node version
// const nodeABI = process.versions.modules;
// Platform
const platform = process.platform;
const arch = process.arch;

let suffix: string[] = [platform, arch];

switch (platform) {
  case "win32":
    suffix.push("msvc");
    break;
  case "linux":
    suffix.push("gnu");
    break;
}

const binding_path = path.join(
  __dirname,
  "node",
  `type-flat.${suffix.join("-")}.node`
);

const binding = require(binding_path);

export const { Flatten } = binding;

export type Flatten = TSFlatten;
