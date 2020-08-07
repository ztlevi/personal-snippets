# Launch in background
brew services start redis
brew services stop redis

# Launch in foreground
redis-server /usr/local/etc/redis.conf
redis-cli shutdown