FROM ubuntu:14.04
MAINTAINER Richard Klees <richard.klees@rwth-aachen.de>
RUN apt-get update && apt-get install -y \
    git \
    openssh-server
RUN mkdir /var/run/sshd 
RUN chsh -s /bin/bash root 
RUN sed -ri 's/session\s+required\s+pam_loginuid.so/session optional pam_loginuid.so/' /etc/pam.d/sshd
COPY authorized_keys /root/.ssh/authorized_keys
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D", "-e"]
