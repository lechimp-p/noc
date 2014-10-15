#!/bin/bash

docker build -t noc-data noc-data
docker run -d --name noc-data noc-data
docker stop noc-data

rm noc-ssh/authorized_keys
rm ~/.ssh/noc_dev_rsa
ssh-keygen -f "/home/lechimp/.ssh/known_hosts" -R [localhost]:50022
ssh-keygen -t rsa -f ~/.ssh/noc_dev_rsa -N ""
cat ~/.ssh/noc_dev_rsa.pub >> noc-ssh/authorized_keys
ssh-add ~/.ssh/noc_dev_rsa
docker build -t noc-ssh noc-ssh
rm noc-ssh/authorized_keys

git clone .. noc-server/src
docker build -t noc-server noc-server 
rm -rf noc-server/src 

docker start noc-data
docker run -d -p 50022:22  \
           --volumes-from noc-data \
           --name noc-ssh \
           noc-ssh 
echo ""
echo " Type 'yes' and press Enter!"
echo ""
ssh root@localhost -p 50022 echo ssh connection to noc works
ssh root@localhost -p 50022 /usr/bin/git init --bare /opt/NoC-Server-git

git remote remove devsrv
git remote add devsrv ssh://root@$(boot2docker ip):50022/opt/NoC-Server-git
git push -f devsrv master
ssh -p 50022 root@$(boot2docker ip) git clone /opt/NoC-Server-git /opt/NoC-Server-src/

cd ../HTML2
grunt dist
grunt upload-dist
cd ../docker

docker stop noc-ssh
docker rm noc-ssh
docker stop noc-data

docker run -t -i --name noc-build \
           --volumes-from noc-data \
           noc-server \
           /bin/sh /opt/update-noc-server.sh 
docker rm noc-build

 docker build -t noc-nginx noc-nginx 
