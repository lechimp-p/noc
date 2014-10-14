#!/bin/bash

docker start noc-data
docker run -t -i --name noc-server \
           --volumes-from noc-data \
           noc-build \
           /bin/sh /opt/update-noc-server.sh 
docker rm noc-build
