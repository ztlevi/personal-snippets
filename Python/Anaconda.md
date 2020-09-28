# Anaconda

## create **Anaconda Virtual environment:**

Once you have Anaconda installed, it makes sense to create a virtual environment for the course. If
you choose not to use a virtual environment, it is up to you to make sure that all dependencies for
the code are installed globally on your machine. To set up a virtual environment, run (in a
terminal)

to create an environment called cs231n.

```
conda create -n cs231n python=3.6
```

> **Note**: the last arguements are packages need to be installed

## Remove conda environment

```
conda env remove -n ENV_NAME
```

## Conda list all environments

```
conda info --envs
```

## Activate

Then, to activate and enter the environment, run

```
source activate cs231n
```

## Deactivate

To exit, you can simply close the window, or run

```
source deactivate cs231n
```
