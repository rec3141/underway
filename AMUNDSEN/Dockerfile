# The underway dashboard as one image: page server, builder, alerts, cameras,
# ice charts and the cruise report builder, run by `python -m dashboard supervise`.
# Build from this directory, with the game (github.com/rec3141/amundsen-game) as a second context:
#   docker build --build-context game=/data/dev/amundsen-game -t underway:<version> .
# Everything that changes lives under /underway (a host folder) and in the settings
# the /settings page writes to /underway/config; see deploy/container/README.md.
FROM python:3.12-slim-bookworm

ENV DEBIAN_FRONTEND=noninteractive PIP_NO_CACHE_DIR=1 PYTHONUNBUFFERED=1
# packages over HTTPS: the ship's firewall refuses package managers' plain-HTTP requests
RUN sed -i 's|http://deb.debian.org|https://deb.debian.org|g' /etc/apt/sources.list.d/debian.sources
RUN apt-get update && apt-get install -y --no-install-recommends \
      caddy cifs-utils rsync ffmpeg gdal-bin nodejs util-linux procps tzdata ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY pyproject.toml README.md ./
COPY dashboard dashboard
COPY cruisereport cruisereport
RUN pip install '.[chat,gcal,cameras,ice-charts,report]' && rm -rf build
COPY tools tools
COPY deploy deploy
COPY update_underway_py.sh ./

# the game at /game/: its server and the Hearts table's Node service; what it
# writes (runtime/) lives in the data folder
COPY --from=game server.py hearts.py crew_chat.py crew-personas.json hearts-service.cjs /game/
COPY --from=game locales /game/locales
COPY --from=game static /game/static
COPY --from=game vendor /game/vendor
RUN ln -s /underway/game /game/runtime

# the fixed layout inside the container; the supervisor ignores these keys in the settings files
ENV UNDERWAY_HOME=/underway \
    UNDERWAY_CONFIG=/underway/config \
    UNDERWAY_DB_DIR=/underway/db \
    UNDERWAY_CACHE_DIR=/underway/cache \
    UNDERWAY_WEBROOT=/underway/www \
    UNDERWAY_CHAT_DB=/underway/chat/chat.sqlite \
    UNDERWAY_CAMERA_OUTPUT=/underway/camera360 \
    CRUISE_STATE_DIR=/underway/report \
    UNDERWAY_TILES_DIR=/underway/tiles \
    ARCTIC_HISTORY_ROOT=/underway/arctic-history \
    UNDERWAY_MIRROR=/underway/mirror \
    UNDERWAY_LOCAL=1 \
    UNDERWAY_PORT=8042 \
    UNDERWAY_PYTHON=/usr/local/bin/python3 \
    UNDERWAY_LLM=0 \
    AMUNDSEN_GAME_DB=/underway/game/suggestions.sqlite \
    AMUNDSEN_HEARTS_DB=/underway/game/hearts.json \
    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 \
    TZ=America/Toronto

EXPOSE 80
VOLUME ["/underway"]
HEALTHCHECK --interval=1m --timeout=10s --start-period=2m \
  CMD python3 -c "import urllib.request; urllib.request.urlopen('http://127.0.0.1:8042/api/health', timeout=8)"
CMD ["python3", "-m", "dashboard", "supervise"]
