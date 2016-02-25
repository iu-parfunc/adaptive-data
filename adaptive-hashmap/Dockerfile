FROM fpco/stack-build

# to draw plots
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get -y install gnuplot-nox

# rewrite git ssh urls
RUN git config --global url."https://github.com/".insteadOf "git@github.com:"

WORKDIR /root
RUN git clone --recursive https://github.com/iu-parfunc/adaptive-data.git

WORKDIR /root/adaptive-hashmap
RUN stack build
