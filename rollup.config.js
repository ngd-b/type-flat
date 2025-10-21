import { defineConfig } from "rollup";
import PluginNodeResolve from "@rollup/plugin-node-resolve";
import PluginCommonjs from "@rollup/plugin-commonjs";
import PluginWasm from "@rollup/plugin-wasm";
import PluginCopy from "rollup-plugin-copy";
import PluginReplace from "@rollup/plugin-replace";
import PluginDTS from "rollup-plugin-dts";

export default defineConfig([
  {
    input: ["pkg/index.js", "scripts/cli.ts"],
    output: {
      dir: "dist",
      format: "es",
    },
    plugins: [
      PluginNodeResolve(),
      PluginCommonjs(),
      PluginWasm({ inline: true }),
      PluginCopy({
        targets: [{ src: "pkg/index_bg.wasm", dest: "dist" }],
      }),
      PluginReplace({
        preventAssignment: true,
        values: {
          // 在 ESM 中模拟 __dirname
          __dirname: 'new URL(".", import.meta.url).pathname',
        },
      }),
    ],
  },
  {
    input: "pkg/index.d.ts",
    output: [
      {
        file: "dist/index.d.ts",
        format: "es",
      },
    ],
    plugins: [PluginDTS()],
  },
]);
