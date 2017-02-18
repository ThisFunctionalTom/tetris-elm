require('purecss');
require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

Elm.Main.embed(root, [Date.now(), [window.innerWidth, window.innerHeight]]);
