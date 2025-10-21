import { execSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

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

console.log("⚙️ Build Node CLI...");
const CLI_PATH = path.join(DIST, "cli.js");
const RUST_BIN = path.join(ROOT, "target", "release", "type-flat");

fs.writeFileSync(
  CLI_PATH,
  `#!/usr/bin/env node
import { execSync } from "child_process";
import path from "path";

const args = process.argv.slice(2);
const rustBin = path.join("${RUST_BIN.replace(/\\/g, "\\\\")}");
execSync(\`\${rustBin} \${args.join(" ")}\`, { stdio: "inherit" });
`
);
fs.chmodSync(CLI_PATH, 0o755);

console.log("✅ Build complete!");
