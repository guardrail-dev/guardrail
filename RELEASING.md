1. Generate list of contributors:

    git log --format='%aN' $(git tag | tail -n1)..@ | sort | uniq -c | sort -nr | sed 's/^ *[[:digit:]]* /- /' | pbcopy

2. Create a release tag: [link](https://github.com/twilio/guardrail/releases)

3. Checkout the tag. This will cause `sbt version` to print out a non-SNAPSHOT version number

4. `sbt codegen/publish` to stage artifacts

5. `sbt codegen/bintrayRelease` to release binary artifacts
