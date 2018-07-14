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
      console.log(valid);
      console.log(chess.fen());
      // app.ports.toElm.send({ tag: "VALID_MOVES", valid: valid });
      return;
    default:
      console.log("msg not tagged", msg);
  }
});

registerServiceWorker();
