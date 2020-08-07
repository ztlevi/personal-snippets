# Callback

https://stackoverflow.com/questions/49516612/visualize-cnn-layer-or-pooling-layer-in-tflearn

you can see the output produced by an intermediate layer by simply defining a new model that has the observed layer as
output.

```python
class PlottingCallback(tflearn.callbacks.Callback):
    def __init__(self, model, x,
                 layers_to_observe=(),
                 kernels=10,
                 inputs=1):
        self.model = model
        self.x = x
        self.kernels = kernels
        self.inputs = inputs
        self.observers = [tflearn.DNN(l) for l in layers_to_observe]

    def on_epoch_end(self, training_state):
        outputs = [o.predict(self.x) for o in self.observers]

        for i in range(self.inputs):
            plt.figure(frameon=False)
            plt.subplots_adjust(wspace=0.1, hspace=0.1)
            ix = 1
            for o in outputs:
                for kernel in range(self.kernels):
                    plt.subplot(len(outputs), self.kernels, ix)
                    plt.imshow(o[i, :, :, kernel])
                    plt.axis('off')
                    ix += 1
            plt.savefig('outputs-for-image:%i-at-epoch:%i.png'
                        % (i, training_state.epoch))

model.fit(X_train, y_train,
          ...
          callbacks=[PlottingCallback(model, X_test, (max_0, max_1, max_2))])
```
