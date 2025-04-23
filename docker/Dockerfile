FROM openjdk:21-jdk

WORKDIR /app

COPY BE/build/libs/chickenStock-0.0.1-SNAPSHOT.jar backend.jar

ENV TZ=Asia/Seoul
ENV SERVER_PORT=8080

# Actuator가 활성화 되어있나 확인필요
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl -f http://localhost:${SERVER_PORT}/actuator/health || exit 1

ENTRYPOINT ["java", "-jar", "-Xmx1024m", \
"-Dserver.port=${SERVER_PORT}", "backend.jar"]

EXPOSE ${SERVER_PORT}