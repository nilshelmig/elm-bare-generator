import { Elm } from "../build/elm.js";
import fs from "fs";

if (process.argv.length != 5) {
  console.error(
    "Usage: npx elm-bare-generator <input schema> <output file> <module name>"
  );
  process.exit(1);
}

const schemaPath = process.argv[2];
const outputFile = process.argv[3];
const moduleName = process.argv[4];

console.log(`[1/4] Reading schema from ${schemaPath}...`);
const schema = fs.readFileSync(schemaPath, "utf8");

const program = Elm.Main.init({ flags: { moduleName, outputFile, schema } });
program.ports.notifyStepParsing.subscribe(() => {
  console.log("[2/4] Parsing schema...");
  program.ports.notified.send({});
});
program.ports.notifyStepTransforming.subscribe(() => {
  console.log("[3/4] Transform to elm code...");
  program.ports.notified.send({});
});
program.ports.notifyStepWriteFile.subscribe(() => {
  console.log(`[4/4] Write content to '${outputFile}'...`);
  program.ports.notified.send({});
});
program.ports.writeFile.subscribe((data) => {
  fs.writeFileSync(outputFile, data);
  program.ports.writeFileDone.send({});
});
program.ports.exit.subscribe(() => {
  console.log(`You can now use the module '${moduleName}'`);
  process.exit(0);
});
program.ports.exitWithError.subscribe((message) => {
  console.error(message);
  process.exit(1);
});
