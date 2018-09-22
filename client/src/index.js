import "bulma/css/bulma.css";
import Chess from "chess.js/chess";
import { builder } from "./builder.js";
import registerServiceWorker from "./registerServiceWorker";

const chess = new Chess();
const endpoint = process.env.ELM_APP_API_ENDPOINT;
const apiEndpoint = endpoint
  ? endpoint
  : "https://trained-backend.herokuapp.com";
const initialSeed = 1;
builder(chess, apiEndpoint, initialSeed);

// : "https://young-meadow-51179.herokuapp.com/api";
