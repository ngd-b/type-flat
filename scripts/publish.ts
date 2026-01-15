import { execSync } from "node:child_process";
import pkg from "../package.json" with { type: "json" };

const { version } = pkg;

let command = "pnpm publish --provenance --access public --no-git-checks";

if (version.includes("beta")) command += " --tag beta";

execSync(command, { stdio: "inherit" });
