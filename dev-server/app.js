const express = require("express");
var cors = require("cors");

var app = express();

let lessons = [];

app.use(cors()); // TODO: handle CORS better
app.use(express.urlencoded({ extended: true }));
app.use(express.json());

app.post("/api/lesson", (req, res, next) => {
  const fen = req.body.fen;
  if (!!fen) {
    const id = lessons.length;
    const newLesson = { id: id, lesson: fen };
    lessons.push(newLesson);
    res.status(204).send("No Content");
  } else {
    res.status(400).send("Bad Request");
  }
});

app.get("/api/lesson/:id", (req, res, next) => {
  const id = req.params.id;
  const lesson = lessons.find(lesson => lesson.id == id);
  if (lesson) {
    res.json({
      msg: "lesson content",
      data: { lesson: lesson.lesson }
    });
  } else {
    res.status(404).send("Not Found");
  }
});

app.listen(3001, () => console.log("Listening"));
