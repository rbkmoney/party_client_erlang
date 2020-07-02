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
      hellgate:
        condition: service_healthy

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:035868ba0ab4dd6ea3a6ac57157be2ca4b8a3361
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

  hellgate:
    image: dr2.rbkmoney.com/rbkmoney/hellgate:3f3c8e18cd59551dd739682539136fc541b60738
    command: /opt/hellgate/bin/hellgate foreground
    depends_on:
      machinegun:
        condition: service_healthy
      dominant:
        condition: service_healthy
      shumway:
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
EOF
