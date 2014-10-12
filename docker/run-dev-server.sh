#!/bin/bash

mkdir data
docker run -t -i -d -p 8000:8000 --name noc-server \
           -v $(pwd)/data:/opt/NoC-data \
           noc-server
docker run -d -p 80:80 --name nginx-server \
           -v $(pwd)/../HTML2/dist:/usr/share/nginx/html:ro \
           -v $(pwd)/data:/opt/NoC-data \
           --link noc-server:noc-server
           noc-nginx
docker attach noc-server 
docker stop noc-server
docker rm noc-server
docker stop nginx-server
docker rm nginx-server
