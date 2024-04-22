from ubuntu:20.04
ENV DEBIAN_FRONTEND="noninteractive" 
ENV TZ=Europe/Amsterdam
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get -y update && apt-get -y upgrade
RUN apt-get -y install haskell-platform make xz-utils libghc-readline-dev 
RUN mkdir /home/mini-java/
EXPOSE 5002
WORKDIR /home/mini-java/
COPY . .
RUN tar -xvf  ./cabal-install-3.6.0.0-x86_64-linux.tar.xz
RUN mv ./cabal /usr/bin/
RUN cabal update
RUN cabal build mini-java

