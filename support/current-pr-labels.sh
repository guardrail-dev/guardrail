#!/bin/bash

set -e

module_name="$1"

gh_api() {
  path="$1"; shift
  curl -s \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "$@" \
    "https://api.github.com/$path"
}

fetch_pr() {
  pr_number="$1"
  cache="$2"
  echo "Fetching labels for current PR (${pr_number})" >&2
  gh_api "repos/guardrail-dev/guardrail/pulls/${pr_number}"
}

labels_since_last_release() {
  module_name="$1"; shift || die 'Missing module_name'

  latest_tag="$(git tag | grep "^$module_name-v[0-9]" | tail -n 1)"
  if [ -z "$latest_tag" ]; then
    echo '{"labels":[]}'
  else
    module_released_on="$(git show "${latest_tag}" --format=%cI)"
    gh_api "search/issues?q=repo:guardrail-dev/guardrail+type:pr+is:merged+closed:>${module_released_on}+label:${module_name}" \
      | jq -cM '{labels: (.items | map(.labels) | flatten | map(.name) | unique | map({ name: . })) }'
  fi
}

merge_labels() {
  sofar="$1"
  jq --argjson sofar "$sofar" '{ labels: ($sofar.labels + .labels) | unique }'
}

cachedir=target/github
mkdir -p "$cachedir"

if [ -n "$GITHUB_EVENT_PATH" ]; then
  pr_number="$(jq --raw-output '.pull_request.number // ""' "$GITHUB_EVENT_PATH")"
  if [ -n "$pr_number" ]; then
    cache="$cachedir/guardrail-ci-${pr_number}-${module_name}-labels-cache.json"
  else
    cache="$cachedir/guardrail-ci-HEAD-${module_name}-labels-cache.json"
  fi
else
  rev="$(git rev-parse --short @)"
  cache="$cachedir/guardrail-ci-${rev}-${module_name}-labels-cache.json"
fi

if [ -f "$cache" ]; then
  echo "Using PR labels from $cache" >&2
else
  # If $pr_number is null, we're either building a branch or master,
  # so either way just skip labels.
  sofar='{"labels": []}'

  # Accumulate all labels up until now
  if [ -n "$module_name" ]; then
    sofar="$(labels_since_last_release "$module_name" | merge_labels "$sofar")"
  fi
  # Include this PR's labels too
  if [ -n "$pr_number" ] && [ "$pr_number" != "null" ]; then
    sofar="$(fetch_pr "$pr_number" | merge_labels "$sofar")"
  fi
  echo "$sofar" > "$cache"
fi

msg="$(jq -r .message < "$cache")"
if [ -n "$msg" ] && [ "$msg" != "null" ]; then  # If the API returned an error message
  echo "ERROR: ${msg}" >&2
  exit 1
fi
cat "$cache" | jq -r '.labels | map(.name)[]'
