import { Elm } from "./src/Main.elm";
import "./scss/style.scss";
import { readFileSync } from "fs";

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}

const flags = {};

const app = Elm.Main.init({ flags });
