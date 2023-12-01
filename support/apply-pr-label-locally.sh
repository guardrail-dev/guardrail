pr_number="$(jq --raw-output .pull_request.number "$GITHUB_EVENT_PATH")"
labels=(
  $(curl \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "https://api.github.com/repos/guardrail-dev/guardrail/pulls/${pr_number}" | \
    jq -r '.labels | map(.name)[]'
  )
)

bump=
major=
minor=
patch=1

for label in "${labels[@]}"; do
  if [ "$label" = major ]; then
    bump=1
    major=1
  elif [ "$label" = minor ]; then
    bump=1
    minor=1
  elif [ "$label" = patch ]; then
    bump=1
    patch=1
  fi
done

echo "Bump: major=${major} minor=${minor} patch=${patch} bump=${bump}"

if [ -n "$bump" ]; then

  modules=( cli core guardrail java-async-http java-dropwizard java-spring-mvc java-support scala-akka-http scala-dropwizard scala-http4s scala-zio-http scala-support )

  for module in "${modules[@]}"; do
    previous="$(git tag --sort=-taggerdate | grep "$(printf '^%s-v' "$module")" | tail -n 1)"
    last="$(echo "$previous" | sed "s/^${module}-v\\([0-9.]*\\)$/\\1/")"
    parts=( ${last//./ } )

    if [ -n "$major" ]; then
      parts=( $((parts[0]+1)) 0 0 )
    elif [ -n "$minor" ]; then
      parts=( ${parts[0]} $((parts[1]+1)) 0 )
    elif [ -n "$patch" ]; then
      parts=( ${parts[0]} ${parts[1]} $((parts[2]+1)) )
    fi

    next="${parts[0]}.${parts[1]}.${parts[2]}"

    echo "$last -> $next"
    git tag "${module}-v${next}"
  done
fi
