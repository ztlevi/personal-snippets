# Mysql shell login

```
mysql -u root -p
```

# Create user

mysql> grant all privileges on dbTest.\* To 'mmall'@'localhost' identified by 'mmallMMALL.123456';

## all database and all ip allowed

mysql> grant all privileges on _._ To 'mmall'@'%' identified by 'mmallMMALL.123456';

# Format result by \G

mysql> select \* from mysql.user \G;

# Step 2 — Starting MySQL

We'll start the daemon with the following command:

```
sudo systemctl start mysqld
```

systemctl doesn't display the outcome of all service management commands, so to be sure we
succeeded, we'll use the following command:

```
sudo systemctl status mysqld
```

If MySQL has successfully started, the output should contain Active: active (running) and the final
line should look something like:

```
Dec 01 19:02:20 centos-512mb-sfo2-02 systemd[1]: Started MySQL Server.
```

> Note: MySQL is automatically enabled to start at boot when it is installed. You can change that
> default behavior with sudo systemctl disable mysqld

Check the temp password

```
sudo grep 'temporary password' /var/log/mysqld.log
```

# Step 3 — Configuring MySQL

MySQL includes a security script to change some of the less secure default options for things like
remote root logins and sample users.

Use this command to run the security script. sudo mysql_secure\
\_installation

# Step 4 — Testing MySQL

We can verify our installation and get information about it by connecting with the mysqladmin tool,
a client that lets you run administrative commands. Use the following command to connect to MySQL as
root (-u root), prompt for a password (-p), and return the version.

```
mysqladmin -u root -p version
```
