#!/bin/bash

mkdir data
docker run -d --name noc-server \
           -v $(pwd)/data:/opt/NoC-data:rw \
           --sig-proxy=true \
           noc-installed \
           /opt/NoC-Server-src/Server/dist/build/NoC-Server-dev/NoC-Server-dev -c /opt/NoC.conf
docker run -d -p 80:80 --name nginx-server \
           -v $(pwd)/../HTML2/dist:/usr/share/nginx/html:ro \
           -v $(pwd)/data:/opt/NoC-data:ro \
           --link noc-server:noc-server \
           noc-nginx
docker logs --follow=true --timestamps=true noc-server 
echo "  Shutting down..."
docker stop noc-server
docker rm noc-server
docker stop nginx-server
docker rm nginx-server
