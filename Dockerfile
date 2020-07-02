FROM alpine

COPY tmp/scalafmt-linux-musl/scalafmt /bin/scalafmt
RUN chmod +x /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
