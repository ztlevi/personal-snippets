https://docs.sevenbridges.com/docs/upload-your-docker-image-1

# Create and upload a Docker image

## Overview

In order to use your own tools on the Seven Bridges Platform, you need to install each tool in an individual
[Docker](https://docs.sevenbridges.com/docs/docker-basics) image, and then upload the image to
[the Seven Bridges Image Registry](https://docs.sevenbridges.com/docs/the-image-registry) or to Docker Hub.

## Prerequisites

Before you can create and upload a Docker image, you need to
[install Docker](https://docs.sevenbridges.com/docs/install-docker). You also need to make sure Docker is running, as
follows:

- **Mac OS 10.10.3 Yosemite or newer**: run Docker for Mac and start a terminal of your choice.
- **Mac OS 10.8 Mountain Lion or newer**: run Docker Quickstart terminal to start Docker Machine.
- **Windows 10**: run Docker for Windows and start a terminal of your choice.
- **Windows 7** or **8**: run Docker Quickstart Terminal to start Docker Machine.
- **Linux**: no action required.

## Steps

To create and upload a Docker image:

1.  Run `docker login images.sbgenomics.com` and enter your Seven Bridges credentials. Don't forget: enter your username
    in lowercase, replacing spaces with hyphens, and enter your
    [authentication token](https://docs.sevenbridges.com/docs/get-your-authentication-token) when prompted for a
    password.

> If you are using the Seven Bridges Platform on AWS EU, please use `eu-images.sbgenomics.com` as the image registry
> instead of `images.sbgenomics.com`.

1.  [Open a Docker base image](https://docs.sevenbridges.com/docs/upload-your-docker-image-1#section-open-a-docker-image).
2.  [Install your tool](https://docs.sevenbridges.com/docs/upload-your-docker-image-1#section-install-your-tool-inside-a-docker-container)
    in the image.
3.  [Commit your image](https://docs.sevenbridges.com/docs/upload-your-docker-image-1#section-commit-your-image).
4.  [Push your image](https://docs.sevenbridges.com/docs/upload-your-docker-image-1#section-push-your-image) to the
    Seven Bridges registry.

## Open a Docker image

If you are installing a tool, you'll need to open a new [base image](https://docs.sevenbridges.com/docs/docker-basics)
to install it on. On the other hand, if you are modifying a tool that you have already uploaded, you can open the image
containing the tool from [the Seven Bridges registry](https://docs.sevenbridges.com/docs/the-image-registry).  
To install a tool, start by opening a base image. This can be any base image from Docker Hub, but starting with a plain
operating system image like Ubuntu is generally recommended. To use this base image, enter:

```
docker run -ti ubuntu
```

To open an image from the Seven Bridges registry, enter the repository that the image is stored in, followed by the
image tag, separated by a colon. The repository has the format `<user_name>/<project_name>`. For example, if the user
`rfranklin` wanted to open the image tagged `1.3` from her project `picard`, she would enter

```
docker-run -ti images.sbgenomics.com/rfranklin/picard:1.3
```

## Install your tool inside a Docker container

Inside a container you can install your chosen tool or modify an existing tool. Do this in the way that you would
normally, using methods appropriate for your tool, e.g. `apt-get`. Files from the local host can be accessed in the
directory `/mountedcwd`. So, if the software you want to install is stored in a remote repository, you can `git pull`
your code and mount it into the container.

When you've finished installing a tool, leave the container by typing `exit`.

```
root@container$ exit
```

## Commit your image

After you exit the container, you can commit the image of it.  
First, list all your local containers, so that you can commit an image (snapshot) of the ubuntu container that you just
created. The `-a` option here lists all containers, include those that are not currently running; you should see a
container that was just created recently. This is the one you want.

```
$ docker ps -a

CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS                      PORTS               NAMES

c52b82b497eb        ubuntu              "/bin/bash"              2 minutes ago       Exited (0) 2 minutes ago                        kickass_liskov
ae194ed75819        debian              "/bin/bash"              7 hours ago         Exited (0) 7 hours ago                          jovial_swanson
34ff1377fbee        hello-world         "/hello"                 26 hours ago        Exited (0) 26 weeks ago                         high_almeida
54240578230c        c0bfb9c8e377        "/bin/sh -c '/usr/gam"   26 hours ago        Exited (0) 26 weeks ago                         serene_pare
517904a42f3d        docker/whalesay     "cowsay hhLinked Appl"   27 hours ago        Exited (0) 27 weeks ago                         romantic_bhaskara
1aad55d740cd        docker/whalesay     "cowsay docker/whales"   27 hours ago        Exited (0) 27 weeks ago                         cocky_bhaskara
7bfb18e0d18a        hello-world         "/hello"                 28 hours ago        Exited (0) 28 weeks ago                         stupefied_williams
```

### Your local containers

If you have followed the Docker Quickstart, on their website, or used Docker in any other way, you might have already
created some containers. You should see those listed too.

Grab the `CONTAINER ID` of the `ubuntu image` that was created 2 minutes ago. It's `c52b82b497eb`.  
Now, we'll commit an image of that container. This also gives you the opportunity to name your image. You must name it
with the format `images.sbgenomics.com/<user_name>/<project_name>[:tag`]. For example, if the user `rfranklin` wanted to
commit her changes to a container in a repository corresponding to the project `picard`, with tag `1.4` she would name
the image `images.sbgenomics.com/rfranklin/picard:1.4`.

Commit the image as follows:

```
$ docker commit c52b82b497eb images.sbgenomics.com/rfranklin/picard:1.4
```

If you want to confirm that the image has been named, you can list all of your local images.

```
$ docker images

REPOSITORY                                TAG                 IMAGE ID            CREATED             VIRTUAL SIZE

images.sbgenomics.com/rfranklin/picard    1.4                 0fe5d1d1aaec        10 minutes ago      125.1 MB
rfranklin/test                            latest              0fe5d1d1aaec        18 minutes ago      125.1 MB
debian                                    latest              7a01cc5f27b1        7 hours ago         125.1 MB
ubuntu                                    latest              6cc0fc2a5ee3        8 months ago        187.9 MB
hello-world                               latest              0a6ba66e537a        8 months ago        960 B
docker/whalesay                           latest              ded5e192a685        8 months ago        247 MB
```

## Push your image

To push your image to the Seven Bridges image registry, run the command
`docker push images.sbgenomics.com/<repository>[:tag]`, where `<repository>[:tag]` refers to the image that you have
already committed. For example:

```
$ docker push images.sbgenomics.com/rfranklin/picard:1.4
The push refers to a repository [images.sbgenomics.com/rfranklin/picard] (len: 1)
container@root: pushed
1.4: digest: sha256:afc9023f29719ffd361cdcbc334fe4ec2c041997ee501a15a86ed7f6e9277008 size: 3990
```

The progress of the image upload will be shown in the terminal. When it has completed, you will see the message
`pushed`.

## Delete a local Docker image

If you want to delete a Docker image, use `docker rmi images.sbgenomics.com/<image_name><:tag>`. For example:

```
$ docker rmi images.sbgenomics.com/rfranklin/picard:1
Deleted 02c8c0913b94a09053dccded886512b77fbd2ecbe18930d1b059bb573f13afd1
```

# Create and upload a Docker image with a Dockerfile

### Overview

Dockerfiles are text files that store the commands you would execute on the command line inside a container to create a
Docker image. When using Dockerfiles, the process of building an image is automated as Docker reads the commands
(instructions) from a Dockerfile and executes them in succession in order to create the final image.

The benefit of Dockerfiles is that they store the whole procedure on how an image is created. They are also significant
as they help facilitate and automate the process of maintaining tools that are wrapped for use on the Seven Bridges
Platform. Specifically, Dockerfiles can contain instructions to install the required dependencies into the container
that is loaded from the base image, add the required tool and tool-related files from its repository to the container,
and install the tool into the container. This means that when changes are made to a tool and it needs to be wrapped for
use on the Platform again, the image containing the tool can be built automatically based on the Dockerfile with no
changes or only minor changes made to the Dockerfile itself.

### Format

A Dockerfile consists of two kind of items: **instructions followed by arguments** and **comments**. The basic
Dockerfile format is shown below:

```
# Comment
INSTRUCTION arguments
```

Instructions are not case-sensitive, but are usually written in uppercase so that they can be differentiated from
arguments more easily. Comments have the hash symbol (#) at the beginning of the line. However, if the same symbol is
located anywhere else in a line, the line will not be treated as a comment.

An example of an instruction is shown below:

```
FROM ubuntu
```

This instruction and argument assign the `ubuntu` image as the base image that you will build upon.

### Usage

This section will present some of the most common instructions used in Dockerfiles and the way in which they are usually
used when wrapping tools for use on the Seven Bridges Platform. For a full list of instructions and all of their
possible formats and uses, please refer to the official
[Dockerfile reference](https://docs.docker.com/engine/reference/builder/).

**FROM** Docker runs instructions in the order in which they are listed in the Dockerfile. The first instruction and the
first non-comment line in a Dockerfile **must be** `FROM` in order to specify the base image from which you will start
building your new image. The instruction is entered in the following format:

```
FROM <image>
```

or

```
FROM <image>:<tag>
```

or

```
FROM <image>@<digest>
```

The `<tag>` part of the argument is used to specify a version of the image. This means that the instruction
`FROM ubuntu:14.04` will automatically load the latest available version of Ubuntu 14.04 as the base image. On the other
hand, `<digest>` is more specific as it is used to refer to an exact image which might not be the latest available
version. For example, if you want to use a specific version of the Ubuntu 14.04 image which is not the latest available
one, the instruction would be, for example:

```
FROM ubuntu@sha256:45b23dee08af5e43a7fea6c4cf9c25ccf269ee113168c19722f87876677c5cb2
```

The `<image>` argument is mandatory when using the `FROM` instruction, while `<tag>` or `<digest>` are optional. If they
are not specified, the assumed tag will be `:latest` and the latest available version of the base image will be used.

If you want to use the `ubuntu` base image, your Dockerfile has to start with the following instruction:

```
FROM ubuntu
```

Learn more about the [FROM instruction](https://docs.docker.com/engine/reference/builder/#/from).

**MAINTAINER** The `MAINTAINER` instruction is not mandatory, but is highly suggested so that
contahttps://docs.sevenbridges.com/docs/upload-your-docker-image-1
