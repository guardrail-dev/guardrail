pr_number="$(jq --raw-output '.pull_request.number' "$GITHUB_EVENT_PATH")"
user="$(jq --raw-output '.pull_request.user.login' "$GITHUB_EVENT_PATH")"

if [ "$user" = 'scala-steward' ]; then
  curl \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "https://api.github.com/repos/guardrail-dev/guardrail/issues/${pr_number}/labels" \
    --data '["chore"]' | \
    jq -r '.labels | map(.name)[]'
fi
