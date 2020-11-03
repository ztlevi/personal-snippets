Put in your Dockerfile something adapted from

```
# Set the locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
```

If you run Debian or Ubuntu, you also need to install `locales` to have `locale-gen` with

```
apt-get -y install locales
```

this is extracted from the very good post on that subject, from

http://jaredmarkell.com/docker-and-locales/
