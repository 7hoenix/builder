import "bulma/css/bulma.css";
import Chess from "chess.js/chess";
import { builder } from "./builder.js";
import registerServiceWorker from "./registerServiceWorker";
console.log("starting");

const chess = new Chess();
console.log("chess", chess);
const endpoint = process.env.ELM_APP_API_ENDPOINT;
const engineUrl = process.env.ELM_APP_BASE_ENGINE_URL;
const apiEndpoint = endpoint
  ? endpoint
  : "https://trained-backend.herokuapp.com";
const baseEngineUrl = engineUrl
  ? engineUrl
  : "https://3gdfbpe678.execute-api.us-east-1.amazonaws.com/dev";
const initialSeed = Math.floor(Math.random() * 10 + 1);
console.log("asdf");
console.log("hi cake", chess, apiEndpoint, baseEngineUrl, initialSeed);
builder(chess, apiEndpoint, baseEngineUrl, initialSeed);
