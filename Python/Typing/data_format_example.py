#!/usr/bin/env python3

from typing import Dict, Generic, List, TypeVar


class DimensionalElement:
    instances: Dict[str, "DimensionalElement"] = {}

    def __init__(self, abbreviation, name):
        self.abbreviation = abbreviation
        self.name = name
        self._add_instance(self)

    @classmethod
    def _add_instance(cls, instance: "DimensionalElement") -> None:
        if instance.abbreviation not in cls.instances:
            cls.instances[instance.abbreviation] = instance


DimensionalElementType = TypeVar("DimensionalElementType", bound=DimensionalElement)


class DimensionalElementOrdering(Generic[DimensionalElementType]):
    def __init__(self, *dimensional_elements: DimensionalElementType) -> None:
        if len(dimensional_elements) != len(set(dimensional_elements)):
            raise ValueError(
                f"Fields must be unique, but found duplicates: {dimensional_elements}."
            )

        self.name = "".join(
            dimensional_element.abbreviation
            for dimensional_element in dimensional_elements
        )
        for i, dimensional_element in enumerate(dimensional_elements):
            setattr(self, dimensional_element.name, i)

        self._dimensional_elements = tuple(dimensional_elements)

    @property
    def fields(self) -> List[str]:
        return [
            dimensional_element.name
            for dimensional_element in self._dimensional_elements
        ]

    @property
    def size(self) -> int:
        return len(self)


class Axis(DimensionalElement):
    pass


class AxisOrder(DimensionalElementOrdering[Axis]):
    pass


BATCH = Axis("N", "BATCH")
HEIGHT = Axis("H", "HEIGHT")
WIDTH = Axis("W", "WIDTH")
CHANNEL = Axis("C", "CHANNEL")

AXIS_ORDER_NCHW = AxisOrder(BATCH, CHANNEL, HEIGHT, WIDTH)
print(AXIS_ORDER_NCHW.fields)
