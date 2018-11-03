// import "./main.css";
import "bulma/css/bulma.css";
import Chess from "chess.js/chess";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const chess = new Chess();

var flags = {
  initialSeed: 123,
  baseEngineUrl: "https://3gdfbpe678.execute-api.us-east-1.amazonaws.com/dev",
  apiEndpoint: "https://trained-backend.herokuapp.com"
};
var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: flags
});

app.ports.fromElm.subscribe(msg => {
  switch (msg.tag) {
    case "SEND_PLACEMENTS":
      chess.clear();
      const valid = msg.placements.every(placement =>
        chess.put(
          { type: placement.piece, color: placement.team },
          placement.square
        )
      );
      app.ports.fromJs.send({
        tag: "VALIDATED_POSITION",
        position: chess.fen()
      });
      return;
    default:
      console.log("msg not tagged", msg);
  }
});

registerServiceWorker();
