#!/usr/bin/env bash

hash gh 2>/dev/null >&2 || die "$0 requires an authenticated gh. Ensure gh is on your path and gh auth login has been successfully completed before trying again"

if [ "$1" = "go" ]; then
  go=1
fi

drafts() {
  gh release list -R github.com/guardrail-dev/guardrail | \
    grep Draft | \
    cut -f 1 -d $'\t' | \
    sed 's/-v[0-9.]*$//'
}

find_draft_release_id() {
  module="$1"
  gh api repos/guardrail-dev/guardrail/releases --jq "$(printf 'map(select((.draft == true) and (.name | startswith("%s"))))[0].id' "$module")"
}

promote_release_draft_id() {
  us="$1"; shift || exit 1
  id="$1"
  echo -n "Promoting $us ($id), draft: "
  draft=$(gh api -X PATCH "repos/guardrail-dev/guardrail/releases/$id" --input <(echo '{"draft":false}') --jq '.draft')
  echo "$draft"
}

trigger() {
  us="$1"
  found=
  blockers=
  for pending in "${todo[@]}"; do
    [ -n "$blockers" ] && break
    if [ "$pending" = "$us" ]; then
      found="$pending"
      continue
    fi
    for needed in "${reverse_deps[@]}"; do
      if [ "$pending" = "$needed" ]; then
        blockers=1
        break
      fi
    done
  done

  if [ -n "$found" ] && [ -z "$blockers" ]; then
    if [ -n "$go" ]; then
      promote_release_draft_id "$us" "$(find_draft_release_id "$us")"
    else
      echo "Would have released $us, $(find_draft_release_id "$us")"
    fi
  fi
}

todo=( start )
while [ "${#todo[@]}" -gt 0 ]; do
  todo=( $(drafts) )

  reverse_deps=()
  trigger core

  reverse_deps=( java-support )
  trigger java-async-http

  reverse_deps=( java-support java-async-http )
  trigger java-dropwizard

  reverse_deps=( java-support )
  trigger java-spring-mvc

  reverse_deps=( core )
  trigger java-support

  reverse_deps=( scala-support )
  trigger scala-akka-http

  reverse_deps=( scala-support )
  trigger scala-dropwizard

  reverse_deps=( scala-support )
  trigger scala-endpoints

  reverse_deps=( scala-support )
  trigger scala-http4s

  reverse_deps=( core )
  trigger scala-support

  reverse_deps=( core java-dropwizard java-spring-mvc java-support java-async-http scala-akka-http scala-dropwizard scala-endpoints scala-http4s scala-support )
  trigger guardrail

  reverse_deps=( guardrail )
  trigger cli
done

echo "Done!"
