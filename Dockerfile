FROM ubuntu:22.04
RUN mkdir -p /opt/irrcalc \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get install -y ca-certificates && update-ca-certificates \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN
ARG YESOD_TIME_ZONE
ARG YESOD_SUPERUSER_USERNAME=$YESOD_SUPERUSER_USERNAME
ARG YESOD_SUPERUSER_PASSWORD=$YESOD_SUPERUSER_PASSWORD
ARG YESOD_GCLOUD_PROJECT_ID=$YESOD_GCLOUD_PROJECT_ID

WORKDIR       /opt/irrcalc
COPY irrcalc  /opt/irrcalc
COPY static   /opt/irrcalc/static
COPY config   /opt/irrcalc/config
COPY demo     /opt/irrcalc/demo

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_TIME_ZONE=${YESOD_TIME_ZONE}
ENV YESOD_SUPERUSER_USERNAME=${YESOD_SUPERUSER_USERNAME}
ENV YESOD_SUPERUSER_PASSWORD=${YESOD_SUPERUSER_PASSWORD}
ENV YESOD_GCLOUD_PROJECT_ID=${YESOD_GCLOUD_PROJECT_ID}

EXPOSE 8080
CMD ["./irrcalc"]
