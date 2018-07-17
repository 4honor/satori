FROM USE_MIRRORopenjdk:8-slim
MAINTAINER feisuzhu@163.com

ENV TERM xterm
WORKDIR /tmp
RUN [ -z "USE_MIRROR" ] || sed -E -i 's/(deb|security).debian.org/mirrors.aliyun.com/g' /etc/apt/sources.list
RUN apt-get update && apt-get -y install git supervisor
EXPOSE 5555
ADD app /app
ADD .build/riemann-reloader /app/riemann-reloader
CMD ["/usr/bin/supervisord","-n","-c","/app/supervisord.conf"]
