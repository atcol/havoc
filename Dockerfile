FROM alpine

WORKDIR /

ADD ./havoc /havoc

RUN chmod +x havoc

ENTRYPOINT ["./havoc"]
