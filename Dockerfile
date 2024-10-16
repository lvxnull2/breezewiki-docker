FROM racket/racket:8.14-full as builder

WORKDIR /app

COPY . .

RUN raco pkg install --batch --auto --no-docs --skip-installed req-lib \
    && raco req -d \
    && raco exe -v -o breezewiki dist.rkt \
    && touch config.ini \
    && raco distribute -v output breezewiki \
    && ln -sf /etc/config.ini ./output/lib/plt/breezewiki/exts/ert/r0/app/config.ini

FROM debian:stable-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && apt-get clean -y \
    && apt-get autopurge -y \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/output /usr/local

EXPOSE 8080

CMD ["breezewiki"]
