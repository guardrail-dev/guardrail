#!/bin/bash

set -e

fetch_pr() {
  pr_number="$1"
  cache="$2"
  echo "Fetching labels for current PR (${pr_number})" >&2
  curl \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    -o "$cache" \
    "https://api.github.com/repos/guardrail-dev/guardrail/pulls/${pr_number}"
}

cachedir=target/github
if [ -n "$GITHUB_EVENT_PATH" ]; then
  mkdir -p "$cachedir"
  pr_number="$(jq --raw-output .pull_request.number "$GITHUB_EVENT_PATH")"
  cache="$cachedir/guardrail-ci-${pr_number}-labels-cache.json"

  if [ -f "$cache" ]; then
    echo "Using PR labels from $cache" >&2
  elif [ "$pr_number" != "null" ]; then
    fetch_pr "$pr_number" "$cache"
  else
    # If $pr_number is null, we're either building a branch or master,
    # so either way just skip labels.
    echo '{"labels": []}' > "$cache"
  fi

  msg="$(jq -r .message < "$cache")"
  if [ "$msg" != "null" ]; then  # If the API returned an error message
    echo "ERROR: ${msg}" >&2
    exit 1
  fi
  cat "$cache" | jq -r '.labels | map(.name)[]'
else
  echo 'Skipping finding labels because GITHUB_EVENT_PATH is not defined' >&2
fi
