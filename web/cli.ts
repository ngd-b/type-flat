#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";
import fs from "node:fs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const platform = process.platform;

const binName = platform === "win32" ? "type-flat.exe" : "type-flat";

const binPath = path.join(__dirname, "cli", platform, binName);

console.log("Spawning binary:", binPath);

try {
  fs.accessSync(binPath, fs.constants.X_OK);
} catch (e) {
  console.error("Binary is not executable, fixing permission...", binPath);

  try {
    fs.chmodSync(binPath, 0o755);
  } catch (e) {
    console.error("[type-flat] Failed to fix permission", e);
    process.exit(1);
  }
}

const result = spawnSync(binPath, process.argv.slice(2), { stdio: "inherit" });
process.exit(result.status ?? 1);
