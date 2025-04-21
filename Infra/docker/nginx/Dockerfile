# Nginx 공식 이미지를 기반으로 합니다
FROM nginx:stable-alpine

# 필요한 패키지 설치 (필요한 경우)
RUN apk update && apk add --no-cache bash

# 기본 Nginx 설정 파일 제거
RUN rm /etc/nginx/conf.d/default.conf

# 사용자 정의 Nginx 설정 파일 복사
COPY nginx.conf /etc/nginx/conf.d/

# 80, 443 포트 노출
EXPOSE 80 443

# Nginx 실행
CMD ["nginx", "-g", "daemon off;"]
