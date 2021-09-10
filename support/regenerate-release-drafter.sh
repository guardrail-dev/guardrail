#!/bin/sh

set -e

die() {
  echo "$@" >&2
  exit 1
}

render() {
  module="$1"; shift || die "Missing module for render"

  cat <<!
name-template: '$module-v\$RESOLVED_VERSION'
tag-template: '$module-v\$RESOLVED_VERSION'
tag-prefix: $module-v
include-paths:
!
  for path in "$@"; do
    [ -e "$path" ] || die "$path specified in $module, but does not exist"
    cat <<!
  - "$path"
!
  done
  cat <<!
categories:
  - title: '🚀 Features'
    labels:
      - 'enhancement'
  - title: '🐛 Bug Fixes'
    labels:
      - 'bug'
  - title: '🧰 Maintenance'
    label: 'chore'
change-template: '- \$TITLE @\$AUTHOR (#\$NUMBER)'
change-title-escapes: '\<*_&' # You can add # and @ to disable mentions, and add \` to disable code blocks.
version-resolver:
  major:
    labels:
      - 'major'
  minor:
    labels:
      - 'minor'
  patch:
    labels:
      - 'patch'
  default: patch
template: |
  ## Changes

  \$CHANGES

  ## Contributors

  Thanks to \$CONTRIBUTORS for your contributions to this release!
!
}

write() {
  module="$1"; shift || die "Missing module for write"
  render "$module" "$@" \
    > ".github/release-drafter-$module.yml"
}

write core              modules/core/              build.sbt  project/
write guardrail         modules/codegen/           build.sbt  project/
write java-async-http   modules/java-async-http/   build.sbt  project/
write java-dropwizard   modules/java-dropwizard/   build.sbt  project/
write java-spring-mvc   modules/java-spring-mvc/   build.sbt  project/
write java-support      modules/java-support/      build.sbt  project/
write scala-akka-http   modules/scala-akka-http/   build.sbt  project/
write scala-dropwizard  modules/scala-dropwizard/  build.sbt  project/
write scala-endpoints   modules/scala-endpoints/   build.sbt  project/
write scala-http4s      modules/scala-http4s/      build.sbt  project/
write scala-support     modules/scala-support/     build.sbt  project/
