FROM golang:latest

RUN curl -sL https://deb.nodesource.com/setup_13.x | bash -
RUN apt-get install -y nodejs

WORKDIR /go
COPY . .

# RUN go get -d -v ./...
# RUN go install -v ./...

# RUN go 

# ENTRYPOINT ["./docker_main.sh"]
# CMD ["./docker_main.sh"]