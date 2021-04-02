# Releasing

The guardrail release process is entirely governed by GitHub Actions, no direct commits or tags are required by maintainers.

## Release steps

1. Navigate to https://github.com/guardrail-dev/guardrail/releases
   The `release-drafter` GitHub Action automatically accumulates merged PRs into release notes for the subsequent release. *This unpublished release will only be visible to maintainers.*
   
   The PR Labels listed in `.github/release-drafter.yml`, when associated with PRs as they are merged, guide the behaviour or `release-drafter` (categorizing PRs, controling the next version).

2. Once the release is published, Github Actions will start building a release immediately.
   
   Artifacts will be published to sonatype automatically.

3. Plugin repositories (https://github.com/guardrail-dev/sbt-guardrail/, https://github.com/guardrail-dev/guardrail-maven-plugin/, and https://github.com/guardrail-dev/guardrail-gradle-plugin/) have integrations
   that automatically bump library upgrades.
   
   Once a guardrail version bumping PR comes in, merging that PR will trigger a build and release.
