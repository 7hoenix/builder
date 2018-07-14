import './main.css';
import Chess from 'chess.js/chess';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));

app.ports.fromElm.subscribe(msg => {
  switch (msg.tag) {
    case "SEND_MOVES":
      const board = new Chess();
      const valid = msg.moves.every(move => board.put(move.piece, move.location))
      app.ports.toElm.send({ tag: "VALID_MOVES", valid: valid });
      return;
  }
});

registerServiceWorker();
