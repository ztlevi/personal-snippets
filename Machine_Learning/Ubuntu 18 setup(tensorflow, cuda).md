# Ubuntu 18 setup (tensorflow, cuda)

## Official Tensorflow GPU installation guide

https://www.tensorflow.org/install/gpu#install_cuda_with_apt

```sh
# Add NVIDIA package repositories
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/cuda-repo-ubuntu1804_10.0.130-1_amd64.deb
sudo dpkg -i cuda-repo-ubuntu1804_10.0.130-1_amd64.deb
sudo apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/7fa2af80.pub
sudo apt-get update
wget http://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1804/x86_64/nvidia-machine-learning-repo-ubuntu1804_1.0.0-1_amd64.deb
sudo apt install ./nvidia-machine-learning-repo-ubuntu1804_1.0.0-1_amd64.deb
sudo apt-get update

# Install NVIDIA driver
# Recommended: Use Software & Updates -> Additional Drivers to install the nvidia-410
sudo apt-get install --no-install-recommends nvidia-driver-410
# Reboot. Check that GPUs are visible using the command: nvidia-smi

# Install development and runtime libraries (~4GB)
sudo apt-get install --no-install-recommends \
    cuda-10-0 \
    libcudnn7=7.4.1.5-1+cuda10.0  \
    libcudnn7-dev=7.4.1.5-1+cuda10.0


# Install TensorRT. Requires that libcudnn7 is installed above.
sudo apt-get update && \
        sudo apt-get install nvinfer-runtime-trt-repo-ubuntu1804-5.0.2-ga-cuda10.0 \
        && sudo apt-get update \
        && sudo apt-get install -y --no-install-recommends libnvinfer-dev=5.0.2-1+cuda10.0

```

## Manually Installation

Install Nvidai driver 410, Cuda 10.0, cudnn 7.4.2 for cuda 10.0

### Install Nvidia 410 driver

Use Software & Updates -> Additional Drivers to install the nvidia-410

### Install Cuda 10.0

https://developer.nvidia.com/cuda-10.0-download-archive?target_os=Linux&target_arch=x86_64&target_distro=Ubuntu&target_version=1804&target_type=runfilelocal

```
sudo sh cuda_10.0.130_410.48_linux.run

# Make sure
# 1. PATH includes /usr/local/cuda-10.0/bin
# 2. LD_LIBRARY_PATH includes /usr/local/cuda-10.0/lib64, or, add /usr/local/cuda-10.0/lib64 to /etc/ld.so.conf and run ldconfig as root
sudo ldconfig
```

### Install Cudnn for cuda 10.0

```
tar -xzvf cudnn-10.0-linux-x64-v7.4.2.24.tgz
sudo cp -r cuda/* /usr/
sudo chmod a+r /usr/include/cudnn.h /usr/lib64/libcudnn*
```

## Install tensorflow

- Tensorflow_all

  Install tensorflow 12 and protobuf 3.6.0, https://github.com/cjweeks/tensorflow-cmake

  Do not run
  `sudo find /usr/local/include/google/tensorflow/tensorflow -type f ! -name "*.h" -delete`

* Tensorflow_cc (Other way, not integrated with DSM)

  https://github.com/FloopCZ/tensorflow_cc
  https://github.com/tensorflow/tensorflow/commit/62ebf62948554f36401132bf93d821c0b8abc510#diff-ade1d3e4b7c35655f854151d899df62b

## Opencv

```
cmake -DWITH_QT=ON -DWITH_OPENGL=ON -DFORCE_VTK=ON -DWITH_TBB=ON -DWITH_GDAL=ON -DWITH_XINE=ON -DBUILD_EXAMPLES=OFF -DWITH_LAPACK=OFF -DWITH_OPENCL=OFF -D WITH_FFMPEG=ON -DBUILD_opencv_cudacodec=OFF ..
```
