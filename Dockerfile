FROM alpine

COPY tmp/scalafmt-docker-build/scalafmt /bin/scalafmt
RUN chmod +x /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
