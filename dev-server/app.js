const express = require("express");
var cors = require("cors");
var port = process.env.PORT || 3001;

var app = express();

const lessons = [
  {
    id: 1,
    title: "The net",
    state: "5k2/8/8/5K2/8/8/3R4/8 w - - 0 0",
    store: {
      "5k2/8/8/5K2/8/8/3R4/8 w": {
        squares: {
          d7:
            "This square will completely restrict the enemy monarchs movement to just the back rank."
        },
        defaultMessage: "We are going to learn the net"
      },
      "4k3/3R4/8/5K2/8/8/8/8 w": {
        squares: {
          e6:
            "Monarchs move one square in any direction. However they can't move into danger. So if you move here then the enemy monarch cannot capture your rook"
        },
        defaultMessage:
          "The enemy monarch is in range to capture. We need to defend the rook."
      },
      "5k2/3R4/4K3/8/8/8/8/8 w": {
        squares: {
          f6:
            "If you move here... then the enemy monarch can just move back to the square it was just in (impasse)",
          c7: "You may buy a move with a stutter step"
        },
        defaultMessage: "Think carefully."
      },
      "6k1/2R5/4K3/8/8/8/8/8 w": {
        squares: {
          f6: "Bounce the monarch out of the corner."
        },
        defaultMessage: "Pursue"
      },
      "7k/2R5/5K2/8/8/8/8/8 w": {
        squares: {
          g6: "Line up the mirror."
        },
        defaultMessage: "The enemy monarch only has one move."
      },
      "6k1/2R5/6K1/8/8/8/8/8 w": {
        squares: {
          c8: "Check mate"
        },
        defaultMessage: "Controlling the mirror."
      }
    }
  }
];

app.use(cors());
app.use(express.urlencoded({ extended: true }));
app.use(express.json());

app.post("/api/lessons", (req, res, next) => {
  if (!!req.body.state) {
    const newLesson = {
      id: lessons.length + 1,
      state: req.body.state,
      title: req.body.title,
      store: req.body.store
    };
    console.log("posting lesson: ", newLesson);
    lessons.push(newLesson);
    res.status(204).send("No Content");
  } else {
    res.status(400).send("Bad Request");
  }
});

app.get("/api/lessons", (req, res, next) => {
  res.json(lessons);
});

app.listen(port, () => console.log("Listening"));
