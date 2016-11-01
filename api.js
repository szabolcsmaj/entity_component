let jsonServer = require('json-server');

let server = jsonServer.create();

server.use(jsonServer.defaults());

let router = jsonServer.router('entity.json');
server.use(router);

console.log('Listening at 4000');
server.listen(4000);
