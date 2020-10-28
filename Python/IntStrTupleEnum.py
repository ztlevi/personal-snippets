#!/usr/bin/env python3
from enum import Enum, EnumMeta


class IntStrTupleEnumMeta(EnumMeta):
    def __call__(cls, value, names=None, *args, **kwargs):
        if names is None:
            for idx, typ in zip([0,1], [int,str]):
                if isinstance(value, typ):
                    for e in cls:
                        if e.value[idx] == value:
                            return e

        return super().__call__(value, names, **kwargs)

    # Override 'in' operator
    def __contains__(cls, key):
        for e in cls:
            if isinstance(key, str):
                if e.value[1] == key:
                    return True
            else:
                if e == key:
                    return True
        return False

class IntStrTupleEnum(Enum, metaclass=IntStrTupleEnumMeta):
    pass

class DummyType(IntStrTupleEnum):
    A = 0, 'A'
    B = 1, 'B'
    C = 2, 'C'

def test_conv_type_enum():
    conv_type = "C"
    assert DummyType(conv_type) == DummyType.C
    assert DummyType(1) == DummyType.B

    with pytest.raises(ValueError):
        DummyType("FF")

    assert "A" in DummyType
    assert "ff" not in DummyType
    assert DummyType(1) in DummyType
