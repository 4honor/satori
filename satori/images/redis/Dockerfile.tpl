FROM USE_MIRRORopenjdk:8-slim
MAINTAINER feisuzhu@163.com

ENV TERM xterm
RUN [ -z "USE_MIRROR" ] || sed -E -i 's/(deb|security).debian.org/mirrors.aliyun.com/g' /etc/apt/sources.list
RUN apt-get update && apt-get install -y python redis-server
EXPOSE 6379
VOLUME /var/lib/redis
ADD redis.conf /etc/redis/redis.conf
CMD exec /usr/bin/redis-server /etc/redis/redis.conf
