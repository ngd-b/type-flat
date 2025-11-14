#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

let platformDir = "";
switch (process.platform) {
  case "win32":
    platformDir = "windows";
    break;
  case "darwin":
    platformDir = "macos";
    break;
  case "linux":
    platformDir = "ubuntu"; // 或者 "linux" 看你目录名字
    break;
  default:
    console.error("Unsupported platform:", process.platform);
    process.exit(1);
}

const binName = process.platform === "win32" ? "type-flat.exe" : "type-flat";
const binPath = path.join(__dirname, "cli", platformDir, binName);

console.log("Spawning binary:", binPath);

const result = spawnSync(binPath, process.argv.slice(2), { stdio: "inherit" });
process.exit(result.status ?? 1);
