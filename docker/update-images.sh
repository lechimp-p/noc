#!/bin/bash

ssh-add ~/.ssh/noc_dev_rsa

docker start noc-data
docker start noc-ssh

sleep 1s
git push -f devsrv master
scp -P 50022 noc-data/NoC.conf root@localhost:/opt/NoC-data/NoC.conf
docker stop noc-ssh

docker run -t -i --name noc-build \
           --volumes-from noc-data \
           noc-server \
           /bin/sh /opt/update-noc-server.sh 
docker stop noc-build
docker wait noc-build
docker rm noc-build


