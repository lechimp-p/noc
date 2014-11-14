#!/bin/bash

ssh-add ~/.ssh/noc_dev_rsa

docker start noc-data
docker run -d --name noc-server \
           --volumes-from noc-data \
           noc-server 
docker run -d -p 80:80 --name nginx-server \
           --volumes-from noc-data \
           --link noc-server:noc-server \
           noc-nginx
docker start noc-ssh

echo "  "
echo "  "
echo " You can view your page at:"
echo "  "
echo $(boot2docker_ip)
echo "  "
echo "  "
echo "Press Ctrl-C to stop the environment."
echo "If nothing happens here while you visit the page, something is wrong..."
docker logs --follow=true --timestamps=true noc-server 
echo "  Shutting down..."
docker stop noc-ssh
docker stop noc-server
docker rm noc-server
docker stop nginx-server
docker rm nginx-server
docker stop noc-data
