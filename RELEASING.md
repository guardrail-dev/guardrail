1. Create and edit release notes

    bash support/write-release-template.sh

2. Create and push a tag locally

    git tag v0.xx.yy
    git push github v0.xx.yy

3. `sbt githubRelease` to push the release to github

4. `sbt codegen/clean` to ensure only fresh artifacts are published

5. `sbt publishBintray` to publish to bintray

5a. `sbt codegen/bintrayRelease` to release the published artifact

6. `publishSonatype` to publish to Maven Central
6a. `sonatypeClose` to close the staging repo
6b. `sonatypeRelease` to release the repo
