1. Generate list of contributors:

    git log --format='%aN' $(git tag | tail -n1)..@ | sort | uniq -c | sort -nr | sed 's/^ *[[:digit:]]* /- /' | pbcopy

2. Create a release tag: [link](https://github.com/twilio/guardrail/releases)

3. Checkout the tag. This will cause `sbt version` to print out a non-SNAPSHOT version number

4. `sbt codegen/clean` to ensure only fresh artifacts are published

5. `sbt publishBintray` to publish to bintray

5a. `sbt codegen/bintrayRelease` to release the published artifact

6. `publishSonatype` to publish to Maven Central
6a. `sonatypeClose` to close the staging repo
6b. `sonatypeRelease` to release the repo
