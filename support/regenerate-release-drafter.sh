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
# NB: Managed by support/regenerate-release-drafter.sh
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

reinitialize_release() {
  cat > .github/workflows/release.yml <<!
name: Release

on:
  release:
    types:
      - released
  workflow_dispatch: {}

jobs:
  publish:
    name: 'Publish release'
    runs-on: ubuntu-20.04
    steps:
      - name: 'Extract project from tag'
        id: set-project-from-tag
        run: |
          module="\$(echo "\$GITHUB_REF" | sed 's~^refs/tags/\\(.*\\)-v[0-9.]\+$~\1~')"
          echo "extract project: \${GITHUB_REF}, \${module}"
          echo "::set-output name=module::\$module"
      - uses: actions/checkout@v2
        with:
          fetch-depth: 0
      - uses: actions/setup-java@v1
        with:
          java-version: 8
      - uses: ruby/setup-ruby@v1.66.1
        with:
          bundler-cache: true
      - name: 'Install microsite deps'
        run: |
          bundle install --jobs 4 --retry 3 --system
      - name: 'Print versions'
        run: |
          java -version
          gpg --version
          ruby --version
          jekyll --version
!
}

write_release() {
  module="$1"; shift || die "Missing module for write_release"
  cat <<!
      # NB: Managed by support/regenerate-release-drafter.sh
      - name: 'Publish artifacts [${module}]'
        if: \${{ steps.set-project-from-tag.outputs.module == '${module}' }}
        run: sbt 'show version' "project ${module}" clean compile versionCheck ci-release
        env:
          PGP_PASSPHRASE: \${{ secrets.PGP_PASSPHRASE }}
          PGP_SECRET: \${{ secrets.PGP_SECRET }}
          SONATYPE_USERNAME: \${{ secrets.SONATYPE_USERNAME }}
          SONATYPE_PASSWORD: \${{ secrets.SONATYPE_PASSWORD }}
          GUARDRAIL_RELEASE_MODULE: \${{ steps.set-project-from-tag.outputs.module }}
!
}

write() {
  module="$1"; shift || die "Missing module for write"
  render "$module" "$@" \
    > ".github/release-drafter-$module.yml"
  write_release "$module" \
    >> .github/workflows/release.yml
}

reinitialize_release

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
