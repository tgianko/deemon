# domxss/taint_flow_mysql_example
FROM docker.wdf.sap.corp:50000/ubuntu:16.10 
MAINTAINER Florian Loch <florian.loch@sap.com>

# Set proxy for usage in SAP_CORPORATE
ENV http_proxy=http://147.204.6.136:8080
ENV https_proxy=http://147.204.6.136:8080
ENV no_proxy=sap.corp,no_proxy=sap.corp,localhost,127.0.0.1
ENV DEBIAN_FRONTEND noninteractive

RUN echo "deb http://download.virtualbox.org/virtualbox/debian wily contrib" >> /etc/apt/sources.list

RUN apt-get update
RUN apt-get -y install wget

RUN wget -O - https://debian.neo4j.org/neotechnology.gpg.key | apt-key add -
RUN echo 'deb http://debian.neo4j.org/repo stable/' | tee /etc/apt/sources.list.d/neo4j.list

RUN apt-get update
RUN apt-get install -y --force-yes openjdk-8-jre-headless git python virtualbox linux-headers-generic virtualbox-dkms sqlite3 libsqlite3-dev qemu-utils netcat-openbsd sbcl libssh2-1-dev

RUN wget https://beta.quicklisp.org/quicklisp.lisp

# RUN git clone https://projects.cispa.saarland/giancarlo.pellegrino/vilanoo.git # TODO, check regarding credentials
# Adding, or better mounting? We will mount to /usr/src/app.
# Still we add it right now and remove it again at the end - just to make ln and other commands happy 
ADD . /usr/src/app 

RUN mkdir -p ~/quicklisp/local-projects
RUN cd ~/quicklisp/local-projects && ln -s /usr/src/app/mosgi/src/ mosgi && ln -s /usr/src/app/rawtrace-analysis/src/ analyzer
RUN cd /usr/src/app
RUN git clone https://github.com/simkoc/cl-libssh2.git ~/quicklisp/local-projects/cl-libssh2

RUN apt-get install python-pip -y
RUN pip install git+https://github.com/tgianko/py2neo.git#egg=py2neo

RUN rm -rf /usr/src/app

RUN mkdir ~/.ssh

RUN echo "================================= \nSetting up zsh and some tools for more efficient working!\n ================================="
RUN apt-get install -y zsh screen vim tmux
RUN echo 'shell "/usr/bin/zsh"' > ~/.screenrc 
RUN sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -) || true"

RUN echo "================================= \nDO NOT FORGET TO RUN THE MANUAL STEPS!\n ================================="

WORKDIR /usr/src/app
# CMD 

# Run the following steps by hand:
# 1. Run container
# docker run -it vilanoo:latest /bin/zsh
# 2. Install Quicklisp
# sbcl --load /quicklisp.lisp
# 3. In the REPL enter the following commands (obvious to skip proxy param when not in SAP_CORPORATE)
# (quicklisp-quickstart:install :proxy "http://147.204.6.136:8080")
# (ql:add-to-init-file)
# 4. Commit the changes as new image (exit docker container first)
# docker commit <id> vilanoo:latest

# Run with docker-compose and interactive shell:
# docker-compose run vilanoo /bin/zsh