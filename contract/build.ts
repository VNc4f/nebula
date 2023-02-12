import {build, emptyDir} from "https://deno.land/x/dnt@0.30.0/mod.ts";
import * as esbuild from "https://deno.land/x/esbuild@v0.14.45/mod.js";
import packageInfo from "../package.json" assert {type: "json"};

function currentPath(path: string): string {
  return new URL(path, import.meta.url).pathname;
}

await emptyDir(currentPath("./dist"));

const deps_before = await Deno.readTextFile("../deps.ts");
await Deno.writeTextFile(
  "../deps.ts",
  deps_before.replaceAll('from "./lucid-cardano/mod.ts";', 'from "https://deno.land/x/lucid@0.9.1/mod.ts";')
);

//** NPM ES Module for Node.js and Browser */

await build({
  entryPoints: [currentPath("./mod.ts")],
  outDir: currentPath("./dist"),
  test: false,
  scriptModule: false,
  typeCheck: false,
  shims: {},
  package: {
    ...packageInfo,
    engines: {
      node: ">=14",
    },
    main: "./esm/mod.js",
    type: "module",
  },
  mappings: {
    "https://deno.land/x/lucid@0.9.1/mod.ts": {
      name: "lucid-cardano",
      version: "^0.9.1",
      peerDependency: true,
    },
  },
});

Deno.copyFileSync(currentPath("../LICENSE"), currentPath("dist/LICENSE"));
Deno.copyFileSync(currentPath("README.md"), currentPath("dist/README.md"));

await esbuild.build({
  bundle: true,
  format: "esm",
  entryPoints: ["./dist/esm/contract/mod.js"],
  outfile: "./dist/web/mod.js",
  minify: true,
  external: [
    "lucid-cardano",
  ],
});
esbuild.stop();

const deps = await Deno.readTextFile("../deps.ts");
await Deno.writeTextFile(
  "../deps.ts",
  deps.replaceAll('from "https://deno.land/x/lucid@0.9.1/mod.ts";', 'from "./lucid-cardano/mod.ts";')
);

const text = await Deno.readTextFile("./dist/web/mod.js");
await Deno.writeTextFile(
  "./dist/web/mod.js",
  text.replaceAll('from"lucid-cardano";', 'from"./lucid-cardano/mod.js";')
    .replaceAll('from "lucid-cardano";', 'from "./lucid-cardano/mod.js";')
);

Deno.removeSync("../../nebula-deploy/mod.js");
Deno.copyFileSync(
  "./dist/web/mod.js",
  "../../nebula-deploy/mod.js",
);

// await esbuild.build({
//   bundle: true,
//   format: "esm",
//   entryPoints: ["../index.js"],
//   outfile: "./dist/web/index.js",
//   // minify: true,
//   external: [
//     "lucid-cardano",
//     "./mod.js",
//     "./support_wallets.js"
//   ],
// });
// esbuild.stop();