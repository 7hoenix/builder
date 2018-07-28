import { Builder } from "./Builder.elm";

export function builder(chess) {
  const app = Builder.embed(document.getElementById("root"));

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
}
