GITHUB_TOKEN="$1"

die() {
  code="$1"; shift
  msg="$1"; shift
  echo "Error: ${msg}"
  exit "$code"
}

api() {
  url="$1"; shift || die 1 "Missing URL for api call"
  curl -s \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "https://api.github.com/${url#/}" \
    "$@"  # All other args are expected to be to cURL
}

[ -n "$GITHUB_TOKEN" ] || die 2 "Missing GITHUB_TOKEN"

to_label=(
  $(
      api "/repos/guardrail-dev/guardrail/pulls" | \
      jq -r 'map(select(.user.login == "scala-steward" and (.labels | any(.name == "chore") | not)))[].number'
  )
)

echo "PRs to tag: ${to_label}" >&2

api "/repos/guardrail-dev/guardrail/pulls"

for pr_number in "${to_label[@]}"; do
  labels=(
    $(
      api "/repos/guardrail-dev/guardrail/issues/${pr_number}/labels" --data '{"labels": ["chore"]}' | \
        jq -r '.[].name'
    )
  )

  echo "${pr_number} is now tagged with: ${labels[@]}" >&2
done
