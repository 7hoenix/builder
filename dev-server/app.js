const express = require("express");
var morgan = require('morgan');
var cors = require("cors");

var app = express();

let lessons = [];
let randomSeed = () => {
  return Math.floor(Math.random() * 100);
}

app.use(morgan('dev'));

app.use(express.urlencoded({ extended: true }));
app.use(express.json());

var corsOptions = {
  origin: 'http://localhost:3000',
  methods: ['GET', 'POST'],
  optionsSuccessStatus: 200 // some legacy browsers (IE11, various SmartTVs) choke on 204
}

const configuredCors = cors(corsOptions)

app.options("/api/lesson", configuredCors)
app.post("/api/lesson", configuredCors, (req, res, next) => {
  const fen = req.body.fen;
  const seed = req.body.seed;
  if (!!fen) {
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

app.get("/api/seed", configuredCors, (req, res, next) => {
  res.json({"seed": randomSeed()});
});

app.listen(3001, () => console.log("Listening"));
