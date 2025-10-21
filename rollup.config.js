import { defineConfig } from "rollup";
import PluginNodeResolve from "@rollup/plugin-node-resolve";
import PluginCommonjs from "@rollup/plugin-commonjs";
import PluginWasm from "@rollup/plugin-wasm";
import PluginCopy from "rollup-plugin-copy";
import PluginReplace from "@rollup/plugin-replace";

import path from "node:path";

export default defineConfig({
  input: "pkg/index.js",
  output: {
    dir: "dist",
    format: "es",
  },
  plugins: [
    PluginNodeResolve(),
    PluginCommonjs(),
    PluginWasm({ inline: true }),
    PluginCopy({
      targets: [
        { src: "pkg/index_bg.wasm", dest: "dist" },
        { src: "scripts/cli.js", dest: "dist" },
      ],
    }),
    PluginReplace({
      preventAssignment: true,
      values: {
        // 在 ESM 中模拟 __dirname
        __dirname: JSON.stringify(path.resolve("./dist")),
      },
    }),
  ],
});
