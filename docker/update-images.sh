#!/bin/bash

docker start noc-data

docker run -d -p 50022:22 --name noc-ssh \
           --volumes-from noc-data \
           noc-ssh
git push -f devsrv master
scp -P 50022 noc-data/NoC.conf root@localhost:/opt/NoC-data/NoC.conf
docker stop noc-ssh
docker rm noc-ssh

docker run -t -i --name noc-build \
           --volumes-from noc-data \
           noc-server \
           /bin/sh /opt/update-noc-server.sh 
docker rm noc-build


