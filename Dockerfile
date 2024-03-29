FROM golang:1.13-alpine

WORKDIR /src

ENV REFRESHED_AT 2019-09-24

COPY main.go .

RUN go build main.go

EXPOSE 80

CMD [ "./main" ]