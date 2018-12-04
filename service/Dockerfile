FROM openjdk:jre-alpine
LABEL maintainer="Leonid Dubinsky dub@podval.org"
ADD dependencies /service/lib
ADD bin /service/bin
ADD project-libs /service/lib
ENTRYPOINT ["/service/bin/service"]
ENV SERVICE_PORT 80
EXPOSE 80
