#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const platform = process.platform;

const binName = platform === "win32" ? "type-flat.exe" : "type-flat";

const binPath = path.join(__dirname, "cli", platform, binName);

console.log("Spawning binary:", binPath);

const result = spawnSync(binPath, process.argv.slice(2), { stdio: "inherit" });
process.exit(result.status ?? 1);
