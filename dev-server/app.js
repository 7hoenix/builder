const express = require("express");
var cors = require("cors");
var app = express();

app.get("/api/lesson", cors(), (req, res, next) => {
  console.log(req);
  res.json({
    msg: "lesson content",
    data: { lesson: "some lesson" }
  });
  res.send("hello world");
});

app.listen(3001, () => console.log("Listening"));
