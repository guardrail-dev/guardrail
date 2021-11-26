#!/bin/bash
cachedir=target/github
if [ -n "$GITHUB_EVENT_PATH" ]; then
  mkdir -p "$cachedir"
  pr_number="$(jq --raw-output .pull_request.number "$GITHUB_EVENT_PATH")"
  cache="$cachedir/guardrail-ci-${pr_number}-labels-cache.json"
  (
    if [ -f "$cache" ]; then
      cat "$cache"
    else
      echo "Fetching labels for current PR"
      curl \
        -H "Accept: application/vnd.github.v3+json" \
        -H "Authorization: Bearer ${GITHUB_TOKEN}" \
        "https://api.github.com/repos/guardrail-dev/guardrail/pulls/${pr_number}" | \
        tee "$cache"
    fi
  ) | jq -r '.labels | map(.name)[]'
else
  echo 'Skipping finding labels because GITHUB_EVENT_PATH is not defined' >&2
fi
