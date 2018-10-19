import { Builder } from "./Builder.elm";
console.log('builder', Builder)

export function builder(chess, apiEndpoint, baseEngineUrl, seed) {
  const app = Builder.embed(document.getElementById("root"), {
    apiEndpoint: apiEndpoint,
    baseEngineUrl: baseEngineUrl,
    initialSeed: seed
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
}
