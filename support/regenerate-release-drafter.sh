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
      - name: 'Print versions'
        run: |
          java -version
          gpg --version
!
}

reinitialize_release_drafter() {
  cat > .github/workflows/release-drafter.yml <<!
name: Release Drafter

on:
  push:
    branches:
      - master
  workflow_dispatch: {}

jobs:
!
}

write_release() {
  module="$1"; shift || die "Missing module for write_release"
  project_name="$1"; shift || die "Missing project_name for write_release"
  cat <<!
      # NB: Managed by support/regenerate-release-drafter.sh
      - name: 'Publish artifacts [${module}]'
        if: \${{ steps.set-project-from-tag.outputs.module == '${module}' }}
        run: sbt 'show version' "project ${project_name}" clean compile versionCheck test ci-release
        env:
          PGP_PASSPHRASE: \${{ secrets.PGP_PASSPHRASE }}
          PGP_SECRET: \${{ secrets.PGP_SECRET }}
          SONATYPE_USERNAME: \${{ secrets.SONATYPE_USERNAME }}
          SONATYPE_PASSWORD: \${{ secrets.SONATYPE_PASSWORD }}
          GUARDRAIL_RELEASE_MODULE: \${{ steps.set-project-from-tag.outputs.module }}
!
}

write_release_drafter() {
  module="$1"; shift || die "Missing module for write_release"
  cat <<!
  ${module}:
    name: '[${module}] Draft release'
    runs-on: ubuntu-20.04
    steps:
      - uses: blast-hardcheese/release-drafter@v5.16.42
        with:
          config-name: release-drafter-${module}.yml
        env:
          GITHUB_TOKEN: \${{ secrets.GITHUB_TOKEN }}
!
}

write() {
  module="$1"; shift || die "Missing module for write"
  project_name="$1"; shift || die "Missing project_name for write"
  render "$module" "$@" \
    > ".github/release-drafter-$module.yml"
  write_release "$module" "$project_name" \
    >> .github/workflows/release.yml
  write_release_drafter "$module" \
    >> .github/workflows/release-drafter.yml
}

reinitialize_release
reinitialize_release_drafter

write core              guardrail-core              modules/core/src/main/              project/src/main/scala/Build.scala  project/src/main/scala/modules/core.scala
write java-async-http   guardrail-java-async-http   modules/java-async-http/src/main/   project/src/main/scala/Build.scala  project/src/main/scala/modules/javaAsyncHttp.scala
write java-dropwizard   guardrail-java-dropwizard   modules/java-dropwizard/src/main/   project/src/main/scala/Build.scala  project/src/main/scala/modules/javaDropwizard.scala
write java-spring-mvc   guardrail-java-spring-mvc   modules/java-spring-mvc/src/main/   project/src/main/scala/Build.scala  project/src/main/scala/modules/javaSpringMvc.scala
write java-support      guardrail-java-support      modules/java-support/src/main/      project/src/main/scala/Build.scala  project/src/main/scala/modules/javaSupport.scala
write scala-akka-http   guardrail-scala-akka-http   modules/scala-akka-http/src/main/   project/src/main/scala/Build.scala  project/src/main/scala/modules/scalaAkkaHttp.scala
write scala-dropwizard  guardrail-scala-dropwizard  modules/scala-dropwizard/src/main/  project/src/main/scala/Build.scala  project/src/main/scala/modules/scalaDropwizard.scala
write scala-http4s      guardrail-scala-http4s      modules/scala-http4s/src/main/      project/src/main/scala/Build.scala  project/src/main/scala/modules/scalaHttp4s.scala
write scala-support     guardrail-scala-support     modules/scala-support/src/main/     project/src/main/scala/Build.scala  project/src/main/scala/modules/scalaSupport.scala
write cli               guardrail-cli               modules/cli/src/main/               project/src/main/scala/Build.scala  project/src/main/scala/modules/cli.scala
