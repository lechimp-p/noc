#!/bin/bash

cd noc-base 
git clone ../.. src
docker build -t noc-base .
rm -r src* 
cd ..
docker run -t --name noc-build -v $(pwd)/..:/opt/NoC-Server-git noc-base /bin/sh /opt/install-noc-server.sh 
docker commit noc-build noc-installed
docker stop noc-build
docker rm noc-build
cd noc-nginx
docker build -t noc-nginx .
