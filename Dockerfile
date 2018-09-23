FROM hseeberger/scala-sbt:8u171_2.12.6_1.2.1
WORKDIR /app
ADD . .
RUN sbt docs/run

FROM node:8-alpine
WORKDIR /root
COPY --from=0 /app/website .
RUN yarn install
RUN yarn build

FROM nginx:alpine
EXPOSE 80
COPY --from=1 /root/build/Scalafmt /usr/share/nginx/html/scalafmt
