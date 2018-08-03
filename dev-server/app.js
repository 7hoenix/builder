const express = require("express");
// var cors = require("cors");

var app = express();

let lessons = [];
let randomSeed = () => {
  return Math.floor(Math.random() * 100);
}

// app.use(cors()); // TODO: handle CORS better
app.use(express.urlencoded({ extended: true }));
app.use(express.json());

app.post("/api/lesson", (req, res, next) => {
  console.log('We made it! ')
  const fen = req.body.fen;
  const seed = req.body.seed;
  if (!!fen) {
  // if (!!fen && !!seed) {
    const id = lessons.length; // TODO: use UUID
    const newLesson = {
      id: id,
      seed: seed,
      lesson: fen
    };
    lessons.push(newLesson);
    res.status(204).send("No Content");
  } else {
    res.status(400).send("Bad Request");
  }
});

app.get("/api/seed", (req, res, next) => {
  res.json({"seed": randomSeed()});
});

app.listen(3001, () => console.log("Listening"));
