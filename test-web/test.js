import test from "ava";

import * as tf from "../dist/index.js";

function run(content, typeNames, excludes) {
  let result = tf.Flatten.flatten(content, typeNames, excludes);

  return result
    .replace(/[\n|\t|\r]/g, " ")
    .replace(/\s+/g, " ")
    .replace("< ", "<")
    .replace(" >", ">")
    .replace("( ", "(")
    .replace(" )", ")")
    .trim();
}

test("Basic test", (t) => {
  let content = `
    interface Empty {}
  `;
  let result = run(content, "Empty");

  t.assert(result.includes("interface Empty {}"));
});

test("Multiple flatten type names", (t) => {
  let content = `
    interface A { a: number }
    interface B extends A { b: string }
    interface C extends B { c: boolean }
  `;
  let result = run(content, ["C"]);

  console.log(result);
  t.assert(
    result.includes("interface C { c: boolean; b: string; a: number; }")
  );
});
