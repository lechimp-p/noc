FROM ubuntu:14.04
MAINTAINER Richard Klees <richard.klees@rwth-aachen.de>
RUN apt-get update && apt-get install -y \
    git \
    openssh-server
RUN mkdir /var/run/sshd 
RUN chsh -s /bin/bash root 
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
VOLUME /opt/NoC-data
VOLUME /opt/NoC-Server-git
VOLUME /opt/NoC-Server-src
VOLUME /opt/NoC-website
RUN echo 'root:password' | chpasswd
RUN sed -ri 's/session\s+required\s+pam_loginuid.so/session optional pam_loginuid.so/' /etc/pam.d/sshd
RUN cd /opt/NoC-Server-git && \
    git init --bare
COPY run.sh /opt/run.sh
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D", "-e"]
