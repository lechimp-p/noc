#!/bin/bash

cd noc-data
docker build -t noc-data .
rm -rf src* 
cd ..
docker run -d -p 50022:22 --name noc-data noc-data /bin/sh /opt/run.sh
docker stop noc-data

cd noc-server
git clone ../.. src
docker build -t noc-server .
rm -rf src* 
cd ..

docker start noc-data
echo "Add your ssh key as key for root@$(boot2docker ip):50022 (password is 'password') and then press ENTER to resume."
read
#ssh-keygen -t rsa -f ~/.ssh/id_rsa 
#ssh -p 50022 root@$(boot2docker ip) mkdir -p .ssh
#cat ~/.ssh/id_rsa.pub | ssh -p 50022 root@$(boot2docker ip) 'cat >> ~/.ssh/authorized_keys'
#ssh-add ~/.ssh/id_rsa.pub
git remote add dev ssh://root@$(boot2docker ip):50022/opt/NoC-Server-git
git push -f dev master
ssh -p 50022 root@$(boot2docker ip) git clone /opt/NoC-Server-git /opt/NoC-Server-src/
docker stop noc-data

docker run -t -i --name noc-build \
           --volumes-from noc-data \
           noc-server \
           /bin/sh /opt/update-noc-server.sh 
docker rm noc-build

cd noc-nginx
docker build -t noc-nginx .
cd ..
