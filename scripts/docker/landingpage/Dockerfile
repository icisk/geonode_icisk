FROM node

RUN npm install -g http-server

WORKDIR /landingpage/

ADD index.html ./index.html
ADD style.css ./style.css

EXPOSE 666

CMD  "http-server" "-p" "666" 