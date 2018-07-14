const express = require('express')
var app = express()

app.get('/', (req, res) => {
  console.log(req);
  res.send('hello world')
})

app.listen(3001, () => console.log('example app listening'))
