# Python 3.10 슬림 이미지를 기본 이미지로 사용
FROM python:3.9-slim

# 작업 디렉토리 설정
WORKDIR /app

# 환경 변수 설정
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    POETRY_VERSION=2.1.1 \
    POETRY_HOME="/opt/poetry" \
    POETRY_VIRTUALENVS_CREATE=false

# Poetry 및 필요한 시스템 의존성 설치
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    apt-utils \
    && pip install --no-cache-dir "poetry==$POETRY_VERSION" \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Poetry 설정 파일 복사
COPY pyproject.toml poetry.lock* README.md /app/

# 의존성 설치
RUN poetry install --no-interaction --no-ansi --only main

# 프로젝트 파일 복사
COPY . /app/

# 애플리케이션 실행
CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]

# 포트 노출
EXPOSE 8000