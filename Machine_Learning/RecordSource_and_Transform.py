# AXIS_ORDER.HWC {H:0, W:1, C:2}
# TensorFormat(AXIS_ORDER, shape, np.dtype)
# np_array_matches_format: match AXIS_ORDER, shape, and np.ndtype
# np_warp: HWC to CHW

from typing_extensions import Protocol


class RecordSource(Protocol):
    def __iter__(self):
        pass


class RecordChain(Protocol):
    source = None

    def set_input(self, source: RecordSource):
        self.source = source

    def __iter__(self):
        pass


class RecordChainFromTransform(RecordChain):
    def __init__(self, transforms):
        self.transforms = transforms

    def __iter__(self):
        for data in self.source:
            for transform in self.transforms:
                data = transform(data)
            yield data


class ConsumerFactory:
    registered = {}
    # def __init__(self):
    #     self.registered = {}  # string : class

    @classmethod
    def register(cls, new_cls):
        cls.registered[new_cls.__name__] = new_cls
        return cls

    @staticmethod
    def build(nested_dict):
        """Nested dictionaries with transforms.

        Use registered dictionary to initialize the transform objects.
        """


class Transform(Protocol):
    def transform_schema(self, schema):
        pass

    def transform_data(self, data):
        pass


@ConsumerFactory.register
class ATransform(Transform):
    def transform_schema(self, schema):
        pass

    def transform_data(self, data):
        pass


print(ConsumerFactory.registered)
