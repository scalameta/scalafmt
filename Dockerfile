FROM alpine

ARG VERSION
ARG TARGETARCH

RUN apk add --no-cache --virtual build-deps curl unzip \
  && if [ "${VERSION}" != "v"* ]; then VERSION="v${VERSION}"; fi \
  && echo "Installing scalafmt version ${VERSION} for architecture ${TARGETARCH}" \
  && if [ "${TARGETARCH}" = "amd64" ]; then ARCH=x86_64; \
  elif [ "${TARGETARCH}" = "arm64" ]; then ARCH=aarch64; \
  else \
  echo "Unsupported architecture: ${TARGETARCH}"; \
  exit 1; \
  fi \
  && curl -L "https://github.com/scalameta/scalafmt/releases/download/${VERSION}/scalafmt-${ARCH}-pc-linux.zip" -o /tmp/scalafmt.zip \
  && unzip /tmp/scalafmt.zip -d /bin \
  && rm -rf /tmp/* \
  && apk del build-deps \
  && apk add --no-cache gcompat libstdc++ \
  && chmod +x /bin/scalafmt

ENTRYPOINT ["/bin/scalafmt"]
