import { defineConfig } from "rollup";
import PluginNodeResolve from "@rollup/plugin-node-resolve";
import PluginCommonjs from "@rollup/plugin-commonjs";
import PluginTypescript from "@rollup/plugin-typescript";
// import PluginWasm from "@rollup/plugin-wasm";
import PluginCopy from "rollup-plugin-copy";
// import PluginReplace from "@rollup/plugin-replace";
import PluginDTS from "rollup-plugin-dts";

export default defineConfig([
  {
    input: { index: "web/index.ts", cli: "web/cli.ts" },
    output: {
      dir: "dist",
      format: "es",
      entryFileNames: "[name].js",
    },
    plugins: [
      PluginNodeResolve(),
      PluginCommonjs(),
      // PluginWasm({ inline: true }),
      // PluginCopy({
      //   targets: [{ src: "pkg/type-flat.node", dest: "dist" }],
      // }),
      // PluginReplace({
      //   preventAssignment: true,
      //   values: {
      //     // 在 ESM 中模拟 __dirname
      //     __dirname: 'new URL(".", import.meta.url).pathname',
      //   },
      // }),
      PluginTypescript(),
    ],
  },
  {
    input: {
      index: "web/index.ts",
    },
    output: {
      dir: "dist",
      entryFileNames: "[name].d.ts", // 输出 dist/index.d.ts、dist/cli.d.ts
    },
    plugins: [PluginDTS()],
  },
]);
