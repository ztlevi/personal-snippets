from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Sequence


class AXISES(dict):
    def __init__(self, axises: str, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = {c: i for i, c in enumerate(axises)}

    def __repr__(self):
        return str(self.__dict__)


class AXIS_ORDER:
    NHWC = AXISES("NHWC")
    HWC = AXISES("HWC")


class TensorFormat:
    def __init__(self, axis_order: AXIS_ORDER, shape: Sequence[int], dtype: Any):
        pass


def np_array_format_match():
    pass


print(AXIS_ORDER.HWC)
