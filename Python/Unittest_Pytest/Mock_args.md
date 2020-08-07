# Mock system arguments

The **argparse** module actually reads input variables from special variable, which is called **ARGV** (short from
**ARG**ument **V**ector). This variable is usually accessed by reading **sys.argv** from **sys** module.

This variable is a ordinary list, so you can append your command-line parameters to it like this:

```python
import sys
sys.argv.extend(['-a', SOME_VALUE])
main()
```

However, messing with sys.argv at runtime is not a good way of testing. A much more cleaner way to replace the
**sys.argv** for some limited scope is using **unittest.mock.patch** context manager, like this:

```python
with unittest.mock.patch('sys.argv'. ['__file__', '-a', SOME_VALUE]):
    main()
```

If you do pass `sys.argv` to `parse_args`, then the path or name of the script itself is the first item in sys.argv and
thus becomes the value of option.filename. The hehe then becomes an unknown argument.

## `mock.Mock`

```python
mock_model_path.suffix = ".not_ckpt"
mock_model_path.is_dir = mock.Mock(return_value=False)
with pytest.raises(ValueError):
    train_utils.export_frozen_graph(
        model_path=mock_model_path, model_name="test_model", model_config=model_config, output_path="/tmp")

mock_model_path.parent.glob = mock.Mock(return_value=["foo.ckpt"])
train_utils.export_frozen_graph(
            model_path=mock_model_path,
            model_name=training_params.model_name,
            model_config=model_config,
            output_path=mock_output_path,
            optimize_flag=False)
```

## `mock.patch`

```python
import os
import mock

with mock.patch('os.urandom', return_value='pumpkins') as abc_urandom_function:
    assert abc_urandom(5) == 'abcpumpkins'

@mock.patch("os.listdir", mock.MagicMock(return_value="test1"))
def test1():
    assert "test1" == os.listdir()


@mock.patch("os.listdir")
def test2(mock_listdir):
    mock_listdir.return_value = "test2"
    assert "test2" == os.listdir()


@mock.patch("os.listdir")
class Test():

    def not_decorated_and_not_tested(self):
        assert False

    def test3(self, mock_listdir):
        mock_listdir.return_value = "test3"
        assert "test3" == os.listdir()

@patch.object(SomeClass, 'class_method')
def test(mock_method):
    SomeClass.class_method(3)
    mock_method.assert_called_with(3)

test()

import os
import unittest
from unittest.mock import patch
@patch.dict('os.environ', {'newkey': 'newvalue'})
class TestSample(unittest.TestCase):
    def test_sample(self):
        self.assertEqual(os.environ['newkey'], 'newvalue')
```

### `autospec`

`car.py`

```python
class Car(object):
    def drive(self, speed):
        return "GOGOGOGO - {0}".format(speed)
def create_and_drive():
    new_car = Car()
    return new_car.drive()
```

`test.py`

```python
from unittest.mock import create_autospec
from unittest import TestCase, mock
from car import create_and_drive, Car

def boom(type, size, weight):
    return "BOOOOOM"
mock_boom = create_autospec(boom)

class test_car(TestCase):
    @mock.patch("car.Car.drive", autospec=True)
    def test_drive(self, mock_drive):

        mock_drive.return_value = "BRRRRUUUUMMMM"

        noise = create_and_drive()
        assert(noise == "BRRRRUUUUMMMM")
```

With `autospec=True`, this will raise missing argument `speed`, but it doesn't pop error if don't use autospec.
