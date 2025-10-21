#!/usr/bin/env node

import tf from "./index.js";
import process from "process";
import fs from "node:fs";

// 命令行参数
const args = process.argv.slice(2);

if (args.length < 2) {
  console.error("Usage:npx type-flat <TypeFilePath> <TypeName>");
  process.exit(1);
}

const [filePath, typeName] = args;

const content = fs.readFileSync(filePath, { encoding: "utf-8" });
try {
  const result = tf.flatten(content, typeName);
  console.log(result);
} catch (err) {
  console.error(err);
  process.exit(1);
}
