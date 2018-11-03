// // // import { Builder } from "./Builder.elm";
// // console.log('builder', Builder)
// // import { Main } from
// import { Main } from "./Main.elm";

// export function builder(chess, apiEndpoint, baseEngineUrl, seed) {
//   // const app = Builder.embed(document.getElementById("root"), {
//   //   apiEndpoint: apiEndpoint,
//   //   baseEngineUrl: baseEngineUrl,
//   //   initialSeed: seed
//   // });
//   var flags = {
//     initialSeed: 123,
//     baseEngineUrl: "https://3gdfbpe678.execute-api.us-east-1.amazonaws.com/dev",
//     apiEndpoint: "https://trained-backend.herokuapp.com"
//   };
//   var app = Elm.Main.init({ flags: flags });

//   app.ports.fromElm.subscribe(msg => {
//     switch (msg.tag) {
//       case "SEND_PLACEMENTS":
//         chess.clear();
//         const valid = msg.placements.every(placement =>
//           chess.put(
//             { type: placement.piece, color: placement.team },
//             placement.square
//           )
//         );
//         app.ports.fromJs.send({
//           tag: "VALIDATED_POSITION",
//           position: chess.fen()
//         });
//         return;
//       default:
//         console.log("msg not tagged", msg);
//     }
//   });
// }
