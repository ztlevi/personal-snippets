#!/usr/bin/env python3

import requests

def create_requests_retry_session(retries: int = 3,
                                  backoff_factor: float = 0.05,
                                  status_forcelist: Sequence[int] = (500, 502, 504)) -> requests.sessions.Session:
    """Create a requests session with exponential backoff using urllib3 Retry module.

    Args:
        retries: Number of times to attempt a retry.
        backoff_factor: The urllib3 will sleep for {backoff factor} * (2 ^ ({number of total retries} - 1)).
        status_forcelist: A set of integer HTTP status codes that we should force a retry on.

    Returns:
        A requests session that can be used for requests with an exponential backoff.
    """

    retry = requests.packages.urllib3.util.retry.Retry(
        total=retries, read=retries, connect=retries, backoff_factor=backoff_factor, status_forcelist=status_forcelist)

    adapter = requests.adapters.HTTPAdapter(max_retries=retry)
    session = requests.Session()
    session.mount("http://", adapter)
    session.mount("https://", adapter)
    return session
