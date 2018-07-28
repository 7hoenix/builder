import "bulma/css/bulma.css";
import Chess from "chess.js/chess";
import { builder } from "./builder.js";
import registerServiceWorker from "./registerServiceWorker";

const chess = new Chess();
builder(chess);
