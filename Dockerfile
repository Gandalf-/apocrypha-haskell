# docker build -t apocrypha .
# docker run --rm -p 9999:9999 -v /home/leaf/local-work/db/:/mnt apocrypha

FROM ubuntu:focal

RUN apt update && apt install -y netbase && rm -rf /var/lib/apt/lists/*

COPY .stack-work/dist/x86_64-linux/*/build/d/d /bin/d
COPY .stack-work/dist/x86_64-linux/*/build/apocrypha-server/apocrypha-server /bin/server

HEALTHCHECK --interval=5s --timeout=2s CMD d --keys | grep -q .

CMD ["server", "--headless", "--database", "/mnt/db.json"]
