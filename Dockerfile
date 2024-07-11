FROM sbtscala/scala-sbt:graalvm-ce-22.3.3-b1-java17_1.10.1_3.4.2 AS builder

WORKDIR /app

COPY . .

RUN microdnf install -y git \
  && bin/build-native-image.sh \
  && mv scalafmt-cli/target/native-image/cli /bin/scalafmt

FROM ubuntu:24.04

COPY --from=builder /bin/scalafmt /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
