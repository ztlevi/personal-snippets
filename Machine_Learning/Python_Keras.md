# Keras

## Set gpu usage

```python
config = tf.ConfigProto()
config.gpu_options.per_process_gpu_memory_fraction = 0.6
keras.backend.set_session(tf.Session(config=config))
```
