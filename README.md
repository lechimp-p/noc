# NoC

This is a small and straightforward approach to a social network, using 
angular.js for the frontend and a server written in haskell for the 
backend.

To try it:

* install docker.io, grunt-cli and bower
* go to ./HTML
* run npm install
* run grunt setup 
* go to ./docker
* run ./make-images.sh
* Watch out, at some point you have to type "yes" to initialize
  the connection to the containers via ssh correctly. This setup
  is for dev, mkay?
* run ./run-dev-server.sh
* visit http://$DOCKER_HOST and login as admin/admin.
* use the python interface to add users and do modifications
* yes, there will be a gui for that later
* have fun and give me feedback

This is certainly not production ready, so use it at your own risk.
