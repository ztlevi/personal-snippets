#!/usr/bin/env python3


class Registry:
    def __init__(self, registry_name):
        self._registry = {}
        self._name = registry_name
        self.default_key_fn = lambda class_or_fn: class_or_fn.__name__

    @property
    def name(self):
        return self._name

    def __contains__(self, key):
        return key in self._registry

    def __setitem__(self, key, value):
        if key is None:
            key = self.default_key_fn(value)
        if key in self:
            raise KeyError(f"key {key} already registered in registry {self._name}")
        if not callable(value):
            raise ValueError("value must be callable")
        self._registry[key] = value

    def __getitem__(self, key):
        if key not in self:
            raise KeyError()
        value = self._registry[key]
        return value

    def register(self, key_or_value=None):
        def decorator(key, value):
            self[key] = value
            return value

        if callable(key_or_value):
            return decorator(key=None, value=key_or_value)
        else:
            return lambda value: decorator(key=key_or_value, value=value)


class Registries:
    def __init__(self):
        raise RuntimeError("Registries is not intended to be instantiated.")

    stage = Registry("stage")


@Registries.stage.register
class DummyStage:
    pass


print(Registries.stage._registry)
