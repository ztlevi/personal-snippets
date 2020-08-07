#!/usr/bin/env python3

import io
import logging
import os
import pathlib
import subprocess
import sys
import threading
from typing import IO, Any, List, Optional, Sequence, Union

logger = logging.getLogger(__name__)
log_lock = threading.Lock()


def run(
    command: Sequence[Union[str, pathlib.PurePath]],
    check: bool = False,
    quiet: bool = False,
    stderr: Union[int, IO[Any]] = subprocess.STDOUT,
    wait: bool = True,
    buffered: bool = True,
    **kwargs: Any
) -> "subprocess.Popen[str]":
    """Run a command. Print the output live and get it returned as well.

    It's like the best of run, check_output and check_call in one.

    Args:
        command: the command to run.
        check: raise an error if the return code is not 0.
        quiet: don't print to the terminal while running (but still return the output).
        stderr: what to do with stderr. Default: pipe to stdout, so that it's captured and returned as output.
        kwargs: miscellaneous arguments (passed straight to Popen).
    Returns:
        Stopped Popen instance with returncode and stdout set.
    Raises:
        subprocess.CalledProcessError if check is true and the return code is nonzero.
        ValueError if check is true and wait is false

    Stdout truth table:
                    quiet    verbose
      buffered      PIPE     PIPE and echo to stdout
      unbuffered    DEVNULL  STDOUT
    This lets us buffer output when we want (i.e. pipe it and store it), send it to stdout, both, or neither.
    """

    if "stdout" not in kwargs:
        if buffered:
            kwargs["stdout"] = subprocess.PIPE
        elif quiet:
            kwargs["stdout"] = subprocess.DEVNULL
        else:
            kwargs["stdout"] = None

    popen = subprocess.Popen(command, stderr=stderr, universal_newlines=True, **kwargs)
    if buffered:
        output = ""
        assert popen.stdout is not None, "stdout is piped and cannot be None"
        for stdout_line in iter(popen.stdout.readline, ""):
            output += stdout_line
            if not quiet and len(stdout_line) > 1:
                print(
                    stdout_line, end="\r"
                )  # Force a carriage return, readline returns a newline
        # You can't seek stdout back to 0, so we'll make a new file-like object and overwrite the old stdout
        popen.stdout = io.StringIO(output)

    if wait or check:
        if not wait:
            raise ValueError("Requested to check error code and also not wait.")
        returncode = popen.wait()

    if check and returncode != 0:
        if quiet and popen.stdout:
            print(popen.stdout.read())
        raise subprocess.CalledProcessError(returncode, command)

    return popen


def subprocess_run(
    command: Union[str, List[str]],
    dump_stdout: bool = False,
    cwd: Optional[str] = None,
    stderr: Union[int, IO[Any]] = subprocess.PIPE,
    stdout: Union[int, IO[Any]] = subprocess.PIPE,
    encoding: str = "utf-8",
    **kwargs: Any
) -> "subprocess.CompletedProcess[str]":
    """Run a command. Wait for it to complete and return the subprocess.CompletedProcess.

    When to use this function:

    1. You want a blocking subprocess call.
    2. `PIPE` is an OK value for `stderr` and `stdout`.
    3. You want to logger the results of your call.

    This is a simple pass-through to `subprocess.run` that adds a little additional logging functionality.
    Specifically, non-empty `stderr` is logged at the info level and non-empty stdout is logged at
    the debug level.

    Replacing standard subprocess calls:

    - `subprocess.call(..., stderr=PIPE, stdout=PIPE)` -> `subprocess_run(...).returncode`
    - `subprocess.check_call(..., stderr=PIPE, stdout=PIPE)` -> `subprocess_run(...).check_returncode()`
    - `output = subprocess.check_output(..., stderr=PIPE, stdout=PIPE)` ->
      ```
      result = subprocess_run(...)
      result.check_return_code()
      output = result.stdout
      ```

    If `dump_stdout` is set to True, the stdout stream of the process will be dumped to the console as the process
    runs.
    """
    for key in ("stdout", "stderr"):
        if locals()[key] is not subprocess.PIPE:
            raise ValueError("subprocess_run only supports PIPE for {}.".format(key))
    if cwd is None:
        cwd = os.getcwd()

    logger.info("Invoking %r from '%s'", command, cwd)
    process = subprocess.Popen(
        command, cwd=cwd, stdout=stdout, stderr=stderr, encoding=encoding, **kwargs
    )
    stdout_lines = []
    with process as p:
        assert p.stdout is not None, "stdout is piped and cannot be None"
        assert p.stderr is not None, "stderr is piped and cannot be None"
        for line in p.stdout:
            stdout_lines.append(line)
            if dump_stdout:
                sys.stdout.write(line)
        stderr_txt = p.stderr.read()
        stdout_txt = "".join(stdout_lines)

    if dump_stdout:
        sys.stdout.flush()

    process_result = subprocess.CompletedProcess(
        command, process.returncode, stdout_txt, stderr_txt
    )
    with log_lock:
        logger.info(
            "Subprocess completed: [%r] in %r exit code [%d]",
            process_result.args,
            cwd,
            process_result.returncode,
        )
        if process_result.stderr.strip():
            logger.info("Subprocess stderr:\n%r", process_result.stderr)
        else:
            logger.debug("Subprocess did not emit anything to stderr.")

        if process_result.stdout.strip():
            logger.debug("Subprocess stdout:\n%r", process_result.stdout)
        else:
            logger.debug("Subprocess did not emit anything to stdout.")

    return process_result


popen = run("ls", buffered=True, stderr=subprocess.PIPE, quiet=True)
print(type(popen.stdout))
b = popen.stdout.getvalue().splitlines()
print(b)

popen = run(
    "ls", buffered=True, stderr=subprocess.PIPE, check=False, wait=False, quiet=True
)
a = popen.stdout.readlines()
a = [line.rstrip() for line in a]
print(a)

# popen = subprocess.Popen("ls", stdout=subprocess.PIPE, universal_newlines=True)
# b = popen.stdout.getvalue().splitlines()
# print(b)
# a = popen.stdout.readlines()
# print(a)
