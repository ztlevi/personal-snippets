#!/usr/bin/env bash

function get_github_latest_release_url() {
  # Function "get_github_latest_release_url": get repo's latest release download url
  # Requirement: jq needs to be installed

  # first argument: USER/REPO
  # second argument (optional): some repo's release has many assets targeting different platform. In this case,
  #   the file extension is needed to identify the asset you want.
  #   This argument is used as a regex. For example, you can either use "deb" or "\.deb".
  # Example:
  # get_github_latest_release_url "wagoodman/dive" "\.deb"

  _callable() { command -v "$1" >/dev/null || return 1; }
  if ! _callable jq; then
    echo "jq is not found. Please install jq first."
    return 1
  fi

  local token1=7fdc9d185a66352918c3
  local token2=ce10c30f07f539baab0b
  oauth_token=$token1$token2
  local url

  local repo=$1
  local suffix=$2

  if [ -z "${repo}" ]; then
    echo "No USER/REPO defined, please use something like get_github_latest_release_url \"jgm/pandoc\"."
    return 1
  fi

  local assets
  assets=$(curl -H "Authorization: token ${oauth_token}" \
    --silent "https://api.github.com/repos/$repo/releases/latest" | jq -r ".assets")

  # Try release API
  if [[ "${assets}" != "null" ]]; then
    if [ -z "${suffix}" ]; then
      url=$(echo "${assets}" | jq -r ".[0].browser_download_url")
    else
      local found=false
      for row in $(echo "${assets}" | jq -r ".[] | @base64"); do
        _jq() {
          echo "${row}" | base64 --decode | jq -r "${1}"
        }
        url=$(_jq ".browser_download_url")
        if echo "${url}" | grep -qe "${suffix}$"; then
          found=true
          break
        fi
      done

      if ! "${found}"; then
        echo "Cannot find ${repo} release's assets with ${suffix} as suffix."
        return 1
      fi
    fi
  # Fallback to tag API
  else
    suffix=${suffix:-"tar.gz"}
    local version
    version=$(curl -H "Authorization: token ${oauth_token}" \
      --silent "https://api.github.com/repos/$repo/tags" | jq -r ".[0].name")
    if [[ -n "${version}" ]]; then
      local url=https://github.com/$repo/archive/$version.${suffix}
    fi
  fi

  if [[ "$url" != "null" ]]; then
    echo "$url"
    return 0
  fi

  echo "Cannot find specifided $repo, please fix the USER/REPO name."
  return 1
}

get_github_latest_release_url
get_github_latest_release_url "daniruiz/flat-remix"
get_github_latest_release_url "jgm/pandoc"
get_github_latest_release_url "baskerville/bspwm" "tar.gz"
get_github_latest_release_url "wagoodman/dive" "deb"
get_github_latest_release_url "rime/librime" "osx\.zip"
get_github_latest_release_url "rime/librime" "osxfff\.zip"
