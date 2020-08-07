"""Additional click utiliy function."""
from typing import Callable, List


def click_add_options(options: List[Callable]) -> Callable:
    """Add shared click options for different commands.

    Args:
        options: list of click options. 

    Raises:
        ValueError if options contains invalid click.option.
    """
    def _add_options(func: Callable) -> Callable:
        for option in reversed(options):
            if not callable(option):
                raise ValueError(
                    "The input option list contains invalid click.option!")
            func = option(func)
        return func

    return _add_options
