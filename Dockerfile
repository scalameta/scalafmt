FROM sbtscala/scala-sbt:graalvm-ce-22.3.0-b2-java11_1.9.5_3.3.1 AS builder

WORKDIR /app

COPY . .

RUN microdnf install -y git

RUN bin/build-native-image.sh && \
  mv scalafmt-cli/graalvm-native-image/cli /bin/scalafmt

FROM scratch

COPY --from=builder /bin/scalafmt /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
