#!/bin/bash

docker run -t -i -p 8000:8000 --name noc-server -v $(pwd)/NoC-data:/opt/NoC-data noc-server /bin/sh -c 'cd /opt/NoC-data && /opt/NoC-Server/dist/build/NoC-Server-dev/NoC-Server-dev'
docker stop noc-server
docker rm noc-server
