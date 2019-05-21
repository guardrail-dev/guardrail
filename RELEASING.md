1. Create and edit release notes

    bash support/write-release-template.sh

2. Create and push a tag locally

    git tag v0.xx.yy
    git push github v0.xx.yy

3. `sbt githubRelease` to push the release to github

4. Once the tag is pushed, travis-ci will start building a release immediately.
   Artifacts should be published to bintray and sonatype automatically.
