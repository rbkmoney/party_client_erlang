#!/bin/bash
cat <<EOF
version: '2.1'
services:
  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      party-management:
        condition: service_healthy

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:1dbb330957077d3fbd93bd93d78d138634b0a2a7
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:00aa3098226e103a1a3626b3edf63864b94c4036
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

  shumway:
    image: dr2.rbkmoney.com/rbkmoney/shumway:058b2459317d1bff0922574e8e8240432c2444cd
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    depends_on:
      - shumway-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  shumway-db:
    image: dr2.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db

  party-management:
    image: dr2.rbkmoney.com/rbkmoney/party-management:935c91235f88f0669d7dc435be686d834a7d397f
    command: /opt/party-management/bin/party-management foreground
    depends_on:
      - machinegun
      - dominant
      - shumway
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20
EOF
