#!/bin/bash

cd docker-dev
cp -r ../Server src
docker stop noc-server
docker rm noc-server
docker rmi noc-server
docker build -t noc-server .
rm -r src
