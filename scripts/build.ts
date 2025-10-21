import { execSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT = path.resolve(__dirname, "../");
const PKG = path.join(ROOT, "pkg");
const DIST = path.join(ROOT, "dist");

fs.rmSync(DIST, { recursive: true, force: true });
fs.mkdirSync(DIST, { recursive: true });
fs.rmSync(PKG, { recursive: true, force: true });
fs.mkdirSync(PKG, { recursive: true });

console.log("🚀 Build wasm module...");
execSync(
  "wasm-pack build --release --target nodejs --out-dir pkg --out-name index --no-opt",
  {
    cwd: ROOT,
    stdio: "inherit",
  }
);

// 再使用rollup进行二次编译
console.log("📦 Run Rollup build...");
execSync("npm run build:es", {
  cwd: ROOT,
  stdio: "inherit",
});

console.log("✅ Build finished.");
