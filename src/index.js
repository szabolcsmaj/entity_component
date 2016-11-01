require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

require('./index.html');

let Elm = require('./Main.elm');
let mountNode = document.getElementById('main');

let app = Elm.Main.embed(mountNode);
