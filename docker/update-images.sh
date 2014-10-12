#!/bin/bash

docker run -t -i --name noc-build -v $(pwd)/..:/opt/NoC-Server-git noc-installed /bin/sh /opt/update-noc-server.sh 
docker commit noc-build noc-installed
docker stop noc-build
docker rm noc-build
