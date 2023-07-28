FROM node

RUN npm install -g http-server

WORKDIR /landingpage/

ADD hello.html ./index.html

EXPOSE 666

CMD  "http-server" "-p" "666" 