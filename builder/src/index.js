import "bulma/css/bulma.css";
import Chess from "chess.js/chess";
import { builder } from "./builder.js";
import registerServiceWorker from "./registerServiceWorker";

const chess = new Chess();
const port = 3001;
const initialSeed = 1;
builder(chess, port, initialSeed);
