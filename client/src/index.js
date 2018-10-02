import "bulma/css/bulma.css";
import Chess from "chess.js/chess";
import { builder } from "./builder.js";
import registerServiceWorker from "./registerServiceWorker";

const chess = new Chess();
const endpoint = process.env.ELM_APP_API_ENDPOINT;
const apiEndpoint = endpoint
  ? endpoint
  : "https://trained-backend.herokuapp.com";
const initialSeed = Math.floor(Math.random() * 10 + 1);
builder(chess, apiEndpoint, initialSeed);
