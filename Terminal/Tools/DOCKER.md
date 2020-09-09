# DOCKER

https://segmentfault.com/a/1190000020411601

很多人都感觉这文章这么简单、这么基础，可是别忘记了“万丈高楼平地起”，如果少了这个平地址，你这高楼估计也起不了。所以，基础
是学习任何一门技术或者一个技术点的重中之重，也可以说是很关键的决胜点。

所以呢，今天，民工哥给大家总结了这 20 个 Docker Command，愿各位小伙伴在通往“玩转 Docker”路上不再无助!!!!

安装完成 docker 容器服务之后，需要了解如何操作它？在 shell 命令行下直接输入 docker 就可以查看帮助信息，如下。

```
[root@master ~]# docker
Usage:    docker COMMAND
A self-sufficient runtime for containers

Options:
      --config string      Location of client config files (default "/root/.docker")
  -D, --debug              Enable debug mode
      --help               Print usage
  -H, --host list          Daemon socket(s) to connect to (default [])
  -l, --log-level string   Set the logging level ("debug", "info", "warn", "error", "fatal") (default "info")
      --tls                Use TLS; implied by --tlsverify
      --tlscacert string   Trust certs signed only by this CA (default "/root/.docker/ca.pem")
      --tlscert string     Path to TLS certificate file (default "/root/.docker/cert.pem")
      --tlskey string      Path to TLS key file (default "/root/.docker/key.pem")
      --tlsverify          Use TLS and verify the remote
  -v, --version            Print version information and quit

Management Commands:
  container   Manage containers
  image       Manage images
  network     Manage networks
  node        Manage Swarm nodes
  plugin      Manage plugins
  secret      Manage Docker secrets
  service     Manage services
  stack       Manage Docker stacks
  swarm       Manage Swarm
  system      Manage Docker
  volume      Manage volumes

Commands:
  attach      Attach to a running container
  build       Build an image from a Dockerfile
  commit      Create a new image from a container's changes
  cp          Copy files/folders between a container and the local filesystem
  create      Create a new container
  diff        Inspect changes on a container's filesystem
  events      Get real time events from the server
  exec        Run a command in a running container
  export      Export a container's filesystem as a tar archive
  history     Show the history of an image
  images      List images
  import      Import the contents from a tarball to create a filesystem image
  info        Display system-wide information
  inspect     Return low-level information on Docker objects
  kill        Kill one or more running containers
  load        Load an image from a tar archive or STDIN
  login       Log in to a Docker registry
  logout      Log out from a Docker registry
  logs        Fetch the logs of a container
  pause       Pause all processes within one or more containers
  port        List port mappings or a specific mapping for the container
  ps          List containers
  pull        Pull an image or a repository from a registry
  push        Push an image or a repository to a registry
  rename      Rename a container
  restart     Restart one or more containers
  rm          Remove one or more containers
  rmi         Remove one or more images
  run         Run a command in a new container
  save        Save one or more images to a tar archive (streamed to STDOUT by default)
  search      Search the Docker Hub for images
  start       Start one or more stopped containers
  stats       Display a live stream of container(s) resource usage statistics
  stop        Stop one or more running containers
  tag         Create a tag TARGET_IMAGE that refers to SOURCE_IMAGE
  top         Display the running processes of a container
  unpause     Unpause all processes within one or more containers
  update      Update configuration of one or more containers
  version     Show the Docker version information
  wait        Block until one or more containers stop, then print their exit codes
```

# Run docker container as non-root user

```
# run as non-root user
docker run -v /etc/passwd:/etc/passwd -u $(id -u) -w /home ...
```

## Give the user root permission, then you can use sudo with this user

```
# Switching to a non-root user, please refer to https://aka.ms/vscode-docker-python-user-rights
ENV user appuser

RUN useradd -m -d /home/${user} ${user} && \
    chown -R ${user} /home/${user} && \
    adduser ${user} sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R ${user} /workspace

USER ${user}
```

## Change to use the user after all the apps are installed (place this at the end of the dockerfile)

```
RUN useradd appuser && chown -R appuser /app
USER appuser
```

# 1、docker start/stop/restart/kill

启动/停止/重启/杀掉容器

实例操作如下：

```
[root@docker ~]# docker start myweb
[root@docker ~]# docker stop myweb
[root@docker ~]# docker restart myweb
[root@docker ~]# docker kill -s kill myweb
```

参数 -s #向容器发送信号  
这些命令不做过多解释

# 2、docker run

创建并启动一个新的容器

```
常用参数如下：
-d              #后台运行容器，并返回容器ID
-i              #以交互式模式运行容器，常与-t参数同时使用
-t              #给容器重新分配一个伪终端，常与-i参数同时使用
--name          #给容器指定一个名称
-m              #指定容器使用内存的最大值
--net           #指定容器使用的网络类型
--link          #链接到另一个容器
```

实例操作如下：

```
[root@docker ~]# docker run -d --name nginx nginx:latest
#后台启动并运行一个名为nginx的容器，运行前它会自动去docker镜像站点下载最新的镜像文件

[root@docker ~]# docker run -d -P 80:80 nginx:latest
#后台启动并运名为nginx的容器，然后将容器的80端口映射到物理机的80端口

[root@docker ~]# docker run -d -v /docker/data:/docker/data -P 80:80 nginx:latest
#后台启动并运名为nginx的容器，然后将容器的80端口映射到物理机的80端口,并且将物理机的/docker/data目录映射到容器的/docker/data

[root@docker ~]# docker run -it  nginx:latest /bin/bash
#以交互式模式运行容器，然后在容器内执行/bin/bash命令

[root@docker ~]# docker run --entrypoint "bash" -it  nginx:latest
# Override entrypoint
```

# 3、docker rm

删除容器

```
常用参数如下：
-f     #强制删除一个运行中的容器
-l     #删除指定的链接
-v     #删除与容器关联和卷
```

实例操作如下：

```
[root@docker ~]# docker rm -f mydocker
#强制删除容器mydocker

[root@docker ~]# docker rm -f dockerA dockerB
#强制删除容器dockerA，dockerB

[root@docker ~]# docker rm -v mydocker
#删除容器，并删除容器挂载的数据卷
```

# 4、docker create

创建一个新的容器但不启动它

```
[root@docker ~]# docker create --name myserver nginx:latest
09b93464c2f75b7b69f83d56a9cfc23ceb50a48a9db7652ee4c27e3e2cb1961f
#创建一个名为myserver的容器
```

# 5、docker exec

在运行的容器中执行命令

```
常用参数如下：
-d      #在后台运行
-i      #保持STDIN打开
-t      #分配一个伪终端
```

实例操作如下：

```
[root@docker ~]# docker exec -it mydocker /bin/sh /server/scripts/docker.sh
hello world!!!!!!!!!!
#以交互模式执行容器中的/server/scripts/docker.sh脚本
[root@docker ~]# docker exec -it mydocker /bin/sh
root@b1a0703e41e7:/#
#以交互模式给容器分配一个伪终端连接
```

# 6、docker ps

列出容器（正在运行）

```
常用参数如下：
-a      #列出所有容器包括停止的
-f      #根据条件过滤显示内容
-l      #列出最近创建的容器
-n      #列出最近创建的N个容器，N为数字
-q      #只显示容器ID
-s      #显示总文件大小
```

实例操作如下：

```
[root@docker ~]# docker ps
CONTAINER ID  IMAGE            COMMAND                  CREATED       STATUS       PORTS                                            NAMES
bd96d72ed9c7  google/cadvisor  "/usr/bin/cadvisor..."   47 hours ago  Up 47 hours  0.0.0.0:8082->8080/tcp                           cadvisor
665563143eb7  grafana/grafana  "/run.sh"                2 days ago    Up 2 days    0.0.0.0:3000->3000/tcp                           grafana
f2304dad5855  tutum/influxdb   "/run.sh"                2 days ago    Up 2 days    0.0.0.0:8083->8083/tcp, 0.0.0.0:8086->8086/tcp   influxdb
#列出正在运行的容器

[root@docker ~]# docker ps -n 2
CONTAINER ID   IMAGE               COMMAND                  CREATED       STATUS       PORTS                    NAMES
bd96d72ed9c7   google/cadvisor     "/usr/bin/cadvisor..."   47 hours ago  Up 47 hours  0.0.0.0:8082->8080/tcp   cadvisor
665563143eb7   grafana/grafana     "/run.sh"                2 days ago    Up 2 days    0.0.0.0:3000->3000/tcp   grafana
#列出最近创建的2个容器

[root@docker ~]# docker ps -a -q
bd96d72ed9c7
665563143eb7
f2304dad5855
9921d2660307
#显示所有容器的ID
```

# 7、docker inspect

获取容器的元数据

```
常用参数如下：
-f        #指定返回值格式或模板文件
-s        #显示总文件大小
--type    #为指定类型返回JSON
```

实例操作如下：

```
[root@docker ~]# docker inspect bd96d72ed9c7
[
    {
        "Id": "bd96d72ed9c713591ba8db0ed4c0ae2689188255da71033c7bced6bb34aa8542",
        "Created": "2018-05-23T09:22:10.633809699Z",
        "Path": "/usr/bin/cadvisor",
        "Args": [
            "-logtostderr",
            "-storage_driver=influxdb",
            "-storage_driver_db=cadvisor",
            "-storage_driver_host=192.168.3.82:8086"
        ],
        "State": {
            "Status": "running",
            "Running": true,
            "Paused": false,
            "Restarting": false,
            "OOMKilled": false,
            "Dead": false,
            "Pid": 17589,
            "ExitCode": 0,
            "Error": "",
            "StartedAt": "2018-05-23T09:22:10.769771142Z",
            "FinishedAt": "0001-01-01T00:00:00Z"
        },
        "Image": "sha256:75f88e3ec333cbb410297e4f40297ac615e076b4a50aeeae49f287093ff01ab1",
        "ResolvConfPath": "/var/lib/docker/containers/bd96d72ed9c713591ba8db0ed4c0ae2689188255da71033c7bced6bb34aa8542/resolv.conf",
        "HostnamePath": "/var/lib/docker/containers/bd96d72ed9c713591ba8db0ed4c0ae2689188255da71033c7bced6bb34aa8542/hostname",
        "HostsPath": "/var/lib/docker/containers/bd96d72ed9c713591ba8db0ed4c0ae2689188255da71033c7bced6bb34aa8542/hosts",
        "LogPath": "",
        "Name": "/cadvisor",
        "RestartCount": 0,
        "Driver": "overlay2",
        "MountLabel": "",
        "ProcessLabel": "",
        "AppArmorProfile": "",
        "ExecIDs": null,
        "HostConfig": {
            "Binds": [
                "/:/rootfs,ro",
                "/var/run:/var/run",
-------------------------------------------省略部分内容
#获取容器ID为bd96d72ed9c7的元数据信息

[root@docker ~]# docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' cadvisor
172.17.0.3
[root@docker ~]# docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' influxdb
172.17.0.2
#获取容器名为influxdb\cadvisor的IP地址
```

# 8、docker logs

获取容器的日志

```
常用参数如下：
-f        #跟踪日志输出
-t        #显示时间戳
--tail    #只显示最新n条容器日志
--since   #显示某个开始时间的所有日志
```

实例操作如下：

```
[root@docker ~]# docker logs -f cadvisor
I0523 09:22:10.794233       1 storagedriver.go:48] Using backend storage type "influxdb"
I0523 09:22:10.794295       1 storagedriver.go:50] Caching stats in memory for 2m0s
I0523 09:22:10.794551       1 manager.go:151] cAdvisor running in container: "/sys/fs/cgroup/cpuacct,cpu"
I0523 09:22:10.810585       1 fs.go:139] Filesystem UUIDs: map[]
I0523 09:22:10.810599       1 fs.go:140] Filesystem partitions: map[shm:{mountpoint:/dev/shm major:0 minor:47 fsType:tmpfs blockSize:0} overlay:{mountpoint:/ major:0 minor:46 fsType:overlay blockSize:0} tmpfs:{mountpoint:/dev major:0 minor:50 fsType:tmpfs blockSize:0} /dev/mapper/centos-root:{mountpoint:/rootfs,ro major:253 minor:0 fsType:xfs blockSize:0} /dev/sda1:{mountpoint:/rootfs,ro/boot major:8 minor:1 fsType:xfs blockSize:0} /dev/mapper/centos-home:{mountpoint:/rootfs,ro/home major:253 minor:2 fsType:xfs blockSize:0}]
W0523 09:22:10.812419       1 info.go:52] Couldn't collect info from any of the files in "/etc/machine-id,/var/lib/dbus/machine-id"
I0523 09:22:10.812460       1 manager.go:225] Machine: {NumCores:1 CpuFrequency:2799091 MemoryCapacity:8203235328 HugePages:[{PageSize:2048 NumPages:0}] MachineID: SystemUUID:564D5235-FED8-3630-AA2B-D65F0855D036 BootID:fd7b3fb5-e74f-4280-80cf-0a7096239619 Filesystems:[{Device:tmpfs DeviceMajor:0 DeviceMinor:50 Capacity:4101615616 Type:vfs Inodes:1001371 HasInodes:true} {Device:/dev/mapper/centos-root DeviceMajor:253 DeviceMinor:0 Capacity:140633964544 Type:vfs Inodes:68681728 HasInodes:true} {Device:/dev/sda1 DeviceMajor:8 DeviceMinor:1 Capacity:1063256064 Type:vfs Inodes:524288 HasInodes:true} {Device:/dev/mapper/centos-home DeviceMajor:253 DeviceMinor:2 Capacity:21464350720 Type:vfs Inodes:10485760 HasInodes:true} {Device:shm DeviceMajor:0 DeviceMinor:47 Capacity:67108864 Type:vfs Inodes:1001371 HasInodes:true} {Device:overlay DeviceMajor:0 DeviceMinor:46 Capacity:140633964544 Type:vfs Inodes:68681728 HasInodes:true}] DiskMap:map[253:0:{Name:dm-0 Major:253 Minor:0 Size:140660178944 Scheduler:none} 253:1:{Name:dm-1 Major:253 Minor:1 Size:8455716864 Scheduler:none} 253:2:{Name:dm-2 Major:253 Minor:2 Size:21474836480 Scheduler:none} 2:0:{Name:fd0 Major:2 Minor:0 Size:4096 Scheduler:deadline} 8:0:{Name:sda Major:8 Minor:0 Size:171798691840 Scheduler:deadline}] NetworkDevices:[{Name:eth0 MacAddress:02:42:ac:11:00:03 Speed:10000 Mtu:1500}] Topology:[{Id:0 Memory:8589467648 Cores:[{Id:0 Threads:[0] Caches:[{Size:32768 Type:Data Level:1} {Size:32768 Type:Instruction Level:1} {Size:262144 Type:Unified Level:2}]}] Caches:[{Size:26214400 Type:Unified Level:3}]}] CloudProvider:Unknown InstanceType:Unknown InstanceID:None}
#跟踪查看容器cadvisor的日志
```

# 9、docker port

显示指定容器的端口映射

实例操作如下：

```
[root@docker ~]# docker port cadvisor
8080/tcp -> 0.0.0.0:8082
#显示cadvisor容器的端口映射信息
```

# 10、docker commit

用已存在的容器重新创建一个新的镜像

```
常用参数如下：
-a      #提交的镜像作者
-c      #使用Dockerfile指令来创建镜像
-m      #提交时附上说明文字
-p      #在commit时，将容器暂停
```

实例操作如下：

```
[root@docker ~]# docker commit -a "mingongge" -m "add a new images" bd96d72ed9c7  newdocker_images:v1.0.0
sha256:20ee805752cb7cae660fbae89d7c6ea4a9c6372f16a6cb079ecf6c79f87ed8c9
[root@docker ~]# docker images
REPOSITORY          TAG       IMAGE ID       CREATED        SIZE
newdocker_images    v1.0.0    20ee805752cb   7 seconds ago  62.2 MB
#将容器bd96d72ed9c7重新生成一个新的镜像名为newdocker_images
```

# 11、docker cp

用于容器与物理主机之间拷贝文件

实例操作如下：

```
[root@docker ~]# docker cp /data/index.html bd96d72ed9c7:/web/
#将物理主机中的/data/index.html拷贝到容器bd96d72ed9c7:/web/目录下

[root@docker ~]# docker cp /data/index.html bd96d72ed9c7:/web/index.php
#将物理主机中的/data/index.html拷贝到容器bd96d72ed9c7:/web/目录下并改名为index.php

[root@docker ~]# docker cp  bd96d72ed9c7:/web  /data/
#拷贝容器bd96d72ed9c7:/web/目录到物理主机中的/data/目录下
```

# 12、docker login/logout

用于登录与登出容器镜像仓库

```
docker login
#登陆到一个Docker镜像仓库，如果未指定镜像仓库地址，默认为官方仓库 Docker Hub

docker logout
#登出一个Docker镜像仓库，如果未指定镜像仓库地址，默认为官方仓库 Docker Hub

常用参数如下：
-u      #登陆的用户名
-p      #登陆的密码
```

实例操作如下：

```
[root@docker ~]# docker login -u username -p password
Login Succeeded
[root@docker ~]# docker logout
Removing login credentials for https://index.docker.io/v1/
#登录与登出默认的容器镜像仓库
```

# 13、docker pull/push

```
docker pull    #从镜像仓库中拉取或者更新指定镜像
docker push    #将本地的镜像上传到镜像仓库,要先登陆到镜像仓库
```

实例操作如下：

```
[root@docker ~]# docker  pull  nginx
Using default tag: latest
Trying to pull repository docker.io/library/nginx ...
latest: Pulling from docker.io/library/nginx
f2aa67a397c4: Already exists
3c091c23e29d: Pulling fs layer
4a99993b8636: Pulling fs layer
#从镜像仓库中拉取或者更新指定镜像，输出信息如上

[root@docker ~]# docker push newdocker_images:v1.0.0
#上传镜像到镜像仓库上
```

# 14、docker images

显示系统本地容器镜像文件

```
常用参数如下：
-a                #列出所有的镜像（含中间映像层，默认，过滤掉中间映像层）；
--digests         #显示镜像的摘要信息；
-f                #显示满足条件的镜像；
--format          #指定返回值的模板文件；
--no-trunc        #显示完整的镜像信息；
-q                #只显示镜像ID。
```

实例操作如下：

```
[root@docker ~]# docker images
REPOSITORY                                 TAG       IMAGE ID       CREATED         SIZE
newdocker_images                           v1.0.0    20ee805752cb   28 minutes ago  62.2 MB
docker.io/grafana/grafana                  latest    4700307f41f2   9 days ago      238 MB
registry.jumpserver.org/public/guacamole   1.0.0     6300349f2642   2 months ago    1.23 GB
docker.io/google/cadvisor                  latest    75f88e3ec333   5 months ago    62.2 MB
docker.io/tutum/influxdb                   latest    c061e5808198   19 months ago   290 MB
#列出本地所有的镜像

[root@docker ~]# docker images -q
20ee805752cb
4700307f41f2
6300349f2642
75f88e3ec333
c061e5808198
#只显示容器ID

[root@docker ~]# docker images --digests
REPOSITORY                                 TAG      DIGEST                                                                    IMAGE ID        CREATED         SIZE
newdocker_images                           v1.0.0   <none>                                                                    20ee805752cb    32 minutes ago  62.2 MB
docker.io/grafana/grafana                  latest   sha256:364bec4a39ecbec744ea4270aae35f6554eb6f2047b3ee08f7b5f1134857c32c   4700307f41f2    9 days ago      238 MB
registry.jumpserver.org/public/guacamole   1.0.0    sha256:ea862bb2e83b648701655c27900bca14b0ab7ab9d4572e716c25a816dc55307b   6300349f2642    2 months ago    1.23 GB
docker.io/google/cadvisor                  latest   sha256:9e347affc725efd3bfe95aa69362cf833aa810f84e6cb9eed1cb65c35216632a   75f88e3ec333    5 months ago    62.2 MB
docker.io/tutum/influxdb                   latest   sha256:5b7c5e318303ad059f3d1a73d084c12cb39ae4f35f7391b79b0ff2c0ba45304b   c061e5808198    19 months ago   290 MB
[root@docker ~]# docker images --no-trunc
REPOSITORY                                 TAG      IMAGE ID                                                                  CREATED         SIZE
newdocker_images                           v1.0.0   sha256:20ee805752cb7cae660fbae89d7c6ea4a9c6372f16a6cb079ecf6c79f87ed8c9   32 minutes ago  62.2 MB
docker.io/grafana/grafana                  latest   sha256:4700307f41f249630f6d772638ad8d32c7d7e3ec86c324d449d5e21076991bb7   9 days ago      238 MB
registry.jumpserver.org/public/guacamole   1.0.0    sha256:6300349f264218e783cd2bd6f7863d356ac8d5ac05a62584cb4680af7ebec292   2 months ago    1.23 GB
docker.io/google/cadvisor                  latest   sha256:75f88e3ec333cbb410297e4f40297ac615e076b4a50aeeae49f287093ff01ab1   5 months ago    62.2 MB
docker.io/tutum/influxdb                   latest   sha256:c061e580819875fad91910841fd3fc53893524bbb9326a68b2470861633aebb1   19 months ago   290 MB
#可以对比下两个参数显示的不同信息
```

# 15、docker rmi

删除镜像

```
常用参数如下：
-f      #强制删除
```

实例操作如下：

```
[root@docker ~]# docker images
REPOSITORY                                 TAG       IMAGE ID       CREATED         SIZE
newdocker_images                           v1.1.0    858cbd9ba687   6 seconds ago   62.2 MB
newdocker_images                           v1.0.0    20ee805752cb   36 minutes ago  62.2 MB
docker.io/grafana/grafana                  latest    4700307f41f2   9 days ago      238 MB
registry.jumpserver.org/public/guacamole   1.0.0     6300349f2642   2 months ago    1.23 GB
docker.io/google/cadvisor                  latest    75f88e3ec333   5 months ago    62.2 MB
docker.io/tutum/influxdb                   latest    c061e5808198   19 months ago   290 MB
[root@docker ~]# docker rmi 20ee805752cb
Untagged: newdocker_images:v1.0.0
Deleted: sha256:20ee805752cb7cae660fbae89d7c6ea4a9c6372f16a6cb079ecf6c79f87ed8c9
[root@docker ~]# docker images
REPOSITORY                                 TAG       IMAGE ID      CREATED          SIZE
newdocker_images                           v1.1.0    858cbd9ba687  39 seconds ago   62.2 MB
docker.io/grafana/grafana                  latest    4700307f41f2  9 days ago       238 MB
registry.jumpserver.org/public/guacamole   1.0.0     6300349f2642  2 months ago     1.23 GB
docker.io/google/cadvisor                  latest    75f88e3ec333  5 months ago     62.2 MB
docker.io/tutum/influxdb                   latest    c061e5808198  19 months ago    290 MB
#删除一个镜像
```

# 16、docker tag

标记本地镜像

实例操作如下：

```
[root@docker ~]# docker images
REPOSITORY           TAG        IMAGE ID            CREATED             SIZE
newdocker_images     v1.1.0     858cbd9ba687        39 seconds ago      62.2 MB
[root@docker ~]# docker tag newdocker_images:v1.1.0 newdocker_images:v2
[root@docker ~]# docker images
REPOSITORY           TAG        IMAGE ID            CREATED             SIZE
newdocker_images     v1.1.0     858cbd9ba687        4 minutes ago       62.2 MB
newdocker_images     v2         858cbd9ba687        4 minutes ago       62.2 MB
#从结果可以看出两个容器的ID是一样的，只是TAG改变了，类似于linux中文件与文件的硬链接一样，其两者的inode号相同。
```

# 17、docker build :

使用 Dockerfile 创建镜像

```
常用参数如下：
-f                    #指定要使用的Dockerfile路径
--label=[]            #设置镜像使用的元数据；
-m                    #设置内存最大值
--memory-swap         #设置Swap的最大值为内存+swap，"-1"表示不限swap
--no-cache           #创建镜像的过程不使用缓存
--pull               #尝试去更新镜像的新版本
-q                   #安静模式，成功后只输出镜像ID
--rm                 #设置镜像成功后删除中间容器
--ulimit            #Ulimit配置
```

实例操作如下：

```
docker build https://github.com/nginxinc/docker-nginx/

docker build --tag my-lsp-docker-container:1.0 .
docker build --tag my-lsp-docker-container:latest .
```

# 18、docker history

查看指定镜像的创建历史

```
常用参数如下：
-H               #以可读的格式打印镜像大小和日期，默认为true；
--no-trunc       #显示完整的提交记录；
-q               #仅列出提交记录ID。
```

实例操作如下：

```
[root@docker ~]# docker history newdocker_images:v2
IMAGE             CREATED          CREATED BY                                    SIZE      COMMENT
858cbd9ba687    32 minutes ago   -storage_driver=influxdb -storage_driver_d...   0 B       add new images
75f88e3ec333    5 months ago     /bin/sh -c #(nop)  ENTRYPOINT ["/usr/bin/c...   0 B
<missing>       5 months ago     /bin/sh -c #(nop)  EXPOSE 8080/tcp              0 B
<missing>       5 months ago     /bin/sh -c #(nop) ADD file:e138bb5c0c12107...   26.5 MB
<missing>       5 months ago     /bin/sh -c apk --no-cache add ca-certifica...   30.9 MB
<missing>       5 months ago     /bin/sh -c #(nop)  ENV GLIBC_VERSION=2.23-r3    0 B
<missing>       5 months ago     /bin/sh -c #(nop)  MAINTAINER dengnan@goog...   0 B
<missing>       5 months ago     /bin/sh -c #(nop)  CMD ["/bin/sh"]              0 B
<missing>       5 months ago     /bin/sh -c #(nop) ADD file:c05a199f603e2a9...   4.82 MB
```

# 19、docker info

显示 Docker 系统信息，包括镜像和容器数

实例操作如下：

```
[root@docker ~]# docker info
Containers: 4
 Running: 3
 Paused: 0
 Stopped: 1
Images: 5
Server Version: 1.13.1
Storage Driver: overlay2
 Backing Filesystem: xfs
 Supports d_type: true
 Native Overlay Diff: true
Logging Driver: journald
Cgroup Driver: systemd
Plugins:
 Volume: local
 Network: bridge host macvlan null overlay
Swarm: inactive
Runtimes: docker-runc runc
Default Runtime: docker-runc
Init Binary: /usr/libexec/docker/docker-init-current
containerd version:  (expected: aa8187dbd3b7ad67d8e5e3a15115d3eef43a7ed1)
runc version: e9c345b3f906d5dc5e8100b05ce37073a811c74a (expected: 9df8b306d01f59d3a8029be411de015b7304dd8f)
init version: 5b117de7f824f3d3825737cf09581645abbe35d4 (expected: 949e6facb77383876aeff8a6944dde66b3089574)
Security Options:
 seccomp
  WARNING: You're not using the default seccomp profile
  Profile: /etc/docker/seccomp.json
Kernel Version: 3.10.0-693.el7.x86_64
Operating System: CentOS Linux 7 (Core)
OSType: linux
Architecture: x86_64
Number of Docker Hooks: 3
CPUs: 1
Total Memory: 7.64 GiB
Name: docker
ID: K7N6:CHF5:KAZP:QFDB:VYBP:IWMW:7VMV:L4TB:OJD2:SEZI:YRRR:4TJN
Docker Root Dir: /var/lib/docker
Debug Mode (client): false
Debug Mode (server): false
Registry: https://index.docker.io/v1/
WARNING: bridge-nf-call-iptables is disabled
WARNING: bridge-nf-call-ip6tables is disabled
Experimental: false
Insecure Registries:
 127.0.0.0/8
Live Restore Enabled: false
Registries: docker.io (secure)
```

# 20、docker version

显示 Docker 版本信息

实例操作如下：

```
[root@docker ~]# docker version
Client:
 Version:         1.13.1
 API version:     1.26
 Package version: docker-1.13.1-63.git94f4240.el7.centos.x86_64
 Go version:      go1.9.4
 Git commit:      94f4240/1.13.1
 Built:           Fri May 18 15:44:33 2018
 OS/Arch:         linux/amd64

Server:
 Version:         1.13.1
 API version:     1.26 (minimum version 1.12)
 Package version: docker-1.13.1-63.git94f4240.el7.centos.x86_64
 Go version:      go1.9.4
 Git commit:      94f4240/1.13.1
 Built:           Fri May 18 15:44:33 2018
 OS/Arch:         linux/amd64
 Experimental:    false
```
