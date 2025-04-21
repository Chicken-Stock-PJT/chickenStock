# Jenkins LTS 버전을 기본 이미지로 사용
FROM jenkins/jenkins:lts-jdk17

# 컨테이너 내부에서 root 권한으로 명령 실행 (필요한 패키지 설치를 위함)
USER root

# Docker CLI 설치를 위한 패키지 추가
RUN apt-get update && \
    apt-get -y install apt-transport-https \
    ca-certificates \
    curl \
    gnupg2 \
    software-properties-common && \
    curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add - && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -cs) stable" && \
    apt-get update && \
    apt-get -y install docker-ce-cli

# Docker Compose 설치
RUN curl -L "https://github.com/docker/compose/releases/download/v2.23.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose && \
    chmod +x /usr/local/bin/docker-compose

# 기타 유용한 도구 설치
RUN apt-get update && apt-get install -y git wget unzip

# 젠킨스 플러그인 설치를 위한 jenkins-plugin-cli 사용
# 필수적인 CI/CD 파이프라인 관련 플러그인 설치
RUN jenkins-plugin-cli --plugins \
    git \
    workflow-aggregator \
    docker-workflow \
    pipeline-stage-view \
    blueocean \
    credentials-binding \
    timestamper \
    github-branch-source

# 작업 디렉토리 생성 및 권한 설정
RUN mkdir -p /var/jenkins_home/workspace
RUN chown -R jenkins:jenkins /var/jenkins_home

# jenkins 사용자로 다시 전환
USER jenkins

# 젠킨스 실행 포트 노출
EXPOSE 8080 50000
