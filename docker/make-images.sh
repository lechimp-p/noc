#!/bin/bash

cd noc-data
docker build -t noc-data .
rm -rf src* 
cd ..
docker run -d -p 50022:22 --name noc-data noc-data
docker stop noc-data

#cd noc-server
#git clone ../.. src
#docker build -t noc-server .
#rm -rf src* 
#cd ..
#

#docker start noc-data
#git remote add dev ssh://lechimp@$(boot2docker ip):50022/opt/NoC-Server-git
#echo 'password' > git push dev master
#docker stop noc-data

#docker run -t -i --name noc-build \
#           --volumes-from noc-data \
#           noc-server \
#           /bin/sh /opt/update-noc-server.sh 
#docker rm noc-build

#cd noc-nginx
#docker build -t noc-nginx
#cd ..

