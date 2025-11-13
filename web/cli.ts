#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import path from "node:path";

const binPath = path.join(
  __dirname,
  "cli",
  process.platform === "win32" ? "type-flat.exe" : "type-flat"
);
const result = spawnSync(binPath, process.argv.slice(2), { stdio: "inherit" });
process.exit(result.status ?? 1);
