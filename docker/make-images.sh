#!/bin/bash

if [ -f docker_ip ]
then
    echo "docker_ip exists. Great!"
else
    echo "You need to define a docker_ip script"\
         "that returns the ip of your docker daemon."
    exit
fi

docker stop noc-ssh noc-data noc-build noc-server noc-nginx
docker rm noc-ssh noc-data noc-build noc-server noc-nginx
docker rmi noc-ssh noc-data noc-build noc-server noc-nginx

echo ""
echo ""
echo "***************************************************"
echo ""
echo " Cleaned up existing containers and images of noc."
echo ""
echo "***************************************************"
echo ""
echo ""

docker build -t noc-data noc-data
docker run -d --name noc-data noc-data

echo ""
echo ""
echo "***************************************************"
echo ""
echo " Build image and container for NoC-data"
echo ""
echo "***************************************************"
echo ""
echo ""

rm noc-ssh/authorized_keys
rm ~/.ssh/noc_dev_rsa
ssh-keygen -R $(./docker_ip):50022
ssh-keygen -t rsa -f ~/.ssh/noc_dev_rsa -N ""
cat ~/.ssh/noc_dev_rsa.pub >> noc-ssh/authorized_keys
ssh-add ~/.ssh/noc_dev_rsa
docker build -t noc-ssh noc-ssh

docker run -d -p 50022:22  \
           --volumes-from noc-data \
           --name noc-ssh \
           noc-ssh 

ssh-keygen -f ~/.ssh/known_hosts -R [$(./docker_ip)]:50022
rm noc-ssh/authorized_keys

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Build image and container for ssh server"
echo " and initialized keys"
echo ""
echo "********************************************************"
echo ""
echo ""


echo ""
echo " ATTENTION Press Enter, then type 'yes' and press Enter again!"
echo ""
read
ssh root@$(./docker_ip) -p 50022 echo ssh connection to noc works
ssh root@$(./docker_ip) -p 50022 /usr/bin/git init --bare /opt/NoC-Server-git

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Checked ssh and initialized git repo on data container"
echo ""
echo "********************************************************"
echo ""
echo ""

git clone .. noc-server/src
docker build -t noc-server noc-server
rm -rf noc-server/src 

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Build image noc-server"
echo ""
echo "********************************************************"
echo ""
echo ""

git remote remove devsrv
git remote add devsrv ssh://root@$(./docker_ip):50022/opt/NoC-Server-git
git push -f devsrv master
ssh -p 50022 root@$(./docker_ip) git clone /opt/NoC-Server-git /opt/NoC-Server-src/

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Updated and clone git repo in data container"
echo ""
echo "********************************************************"
echo ""
echo ""

cd ../HTML
grunt dist
grunt upload-dist
cd ../docker

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Uploaded website content to data container"
echo ""
echo "********************************************************"
echo ""
echo ""

docker stop noc-ssh

docker run -t -i --name noc-build \
           --volumes-from noc-data \
           noc-server \
           /bin/sh /opt/update-noc-server.sh 
docker wait noc-build
docker rm noc-build
docker stop noc-data

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Build NoC" 
echo ""
echo "********************************************************"
echo ""
echo ""

docker build -t noc-nginx noc-nginx 

echo ""
echo ""
echo "********************************************************"
echo ""
echo " Build nginx-Server."
echo ""
echo " FINISHED" 
echo ""
echo "********************************************************"
echo ""
echo ""

