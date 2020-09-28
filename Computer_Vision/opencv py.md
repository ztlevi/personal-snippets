# OPENCV Python

## Installation

```
conda install -c conda-forge opencv # opencv 3.4
conda install -c menpo opencv3
python3 -m pip install opencv-python
```

## Operations

- read and resize image

  ```python
  img = cv2.imread(addr, 1)
  img = cv2.resize(img, (224, 224), interpolation=cv2.INTER_CUBIC)
  img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
  ```

- show image

  - Wait util key a key is hit.
    ```python
    cv2.imshow("image", img)
    cv2.waitKey(0)
    cv2.destroyAllWindows()
    ```
  - Here, window shows image for 1000 ms, or 1 second. After that, the window would disappear
    itself. But in some cases, it won't. So you can forcefully destroy it using
    cv2.destroyAllWindows()
    ```python
    cv2.waitKey(1000)
    cv2.destroyAllWindows()
    ```

## Storage add image

```python
def storage_add_images(storage, addrs, dataset_dir, split_name):
    num_images = len(addrs)
    for i in range(num_images):
        if (i + 1) % 100 == 0 and i > 1:
            sys.stdout.write(f'\r{split_name} data {i+1}/{num_images} processed...')
            sys.stdout.flush()

        # read an image and resize to (224,224)
        # cv2 load images as BGR, convert it to RGB
        addr = os.path.join(dataset_dir, addrs[i])
        try:
            img = cv2.imread(addr, 1)
            img = cv2.resize(img, (224, 224), interpolation=cv2.INTER_CUBIC)
            img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)

            storage.append(img[None])
        except Exception as e:
            print(addr)
            print(str(e))
            continue
    print(f"\nFinish converting {split_name} data...")


def run(dataset_dir):
    training_csv_data = get_csv_data(dataset_dir, True)

    img_dtype = tables.UInt8Atom()
    data_shape = (0, 224, 224, 3)
    hdf5_path = os.path.join(dataset_dir, 'affectnet_test.h5')
    hdf5_file = tables.open_file(hdf5_path, mode='w')

    train_addrs = training_csv_data.subDirectory_filePath.values

    train_storage = hdf5_file.create_earray(hdf5_file.root, 'train_img', img_dtype, shape=data_shape)

    # create the label arrays and copy the labels data in them
    hdf5_file.create_array(hdf5_file.root, 'train_labels', training_csv_data.expression.values)

    storage_add_images(train_storage, train_addrs, dataset_dir, "training")
    hdf5_file.close()
```
