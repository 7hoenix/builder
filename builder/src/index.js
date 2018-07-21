import "bulma/css/bulma.css";
import "./main.css";
import Chess from "chess.js/chess";
import { Main } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const app = Main.embed(document.getElementById("root"));

app.ports.fromElm.subscribe(msg => {
  switch (msg.tag) {
    case "SEND_PLACEMENTS":
      const chess = new Chess();
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
