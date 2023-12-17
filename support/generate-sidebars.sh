#!/usr/bin/env bash

render() {
  local SCALA_TREE JAVA_TREE
  section="$1"
  case "$section" in
    scala)
      SCALA_TREE="$(cat <<!!
  - [akka-http](scala/akka-http/README.md)
  - [http4s](scala/http4s/README.md)
  - [pekko-http](scala/pekko-http/README.md)
!!
)"
      ;;
    scala-akka-http)
      SCALA_TREE="$(cat <<!!
  - [akka-http](scala/akka-http/README.md)
  - [akka-http-jackson](scala/akka-http/jackson.md)
!!
)"
      ;;
    scala-pekko-http)
      SCALA_TREE="$(cat <<!!
  - [pekko-http](scala/pekko-http/README.md)
!!
)"
      ;;
    scala-http4s)
      SCALA_TREE="$(cat <<!!
  - [http](scala/http4s/README.md)
!!
)"
      ;;
    java)
      JAVA_TREE="$(cat <<!!
  - [dropwizard](java/dropwizard/README.md)
  - [spring-mvc](java/spring-mvc/README.md)
!!
)"
      ;;
    java-dropwizard)
      JAVA_TREE="$(cat <<!!
  - [dropwizard](java/dropwizard/README.md)
!!
)"
      ;;
    java-spring-mvc)
      JAVA_TREE="$(cat <<!!
  - [spring-mvc](java/spring-mvc/README.md)
!!
)"
      ;;
  esac
  sed '/^$/d' <<!
- [Home](/)
- [What is guardrail?](about.md)
- [Sample API Specification](sample-spec.md)
- [Scala](scala/README.md)
$SCALA_TREE
- [Java](java/README.md)
$JAVA_TREE
- [Extensions](extensions.md)
!
}

render > docs/_sidebar.md
render scala > docs/scala/_sidebar.md
render scala-akka-http > docs/scala/akka-http/_sidebar.md
render scala-pekko-http > docs/scala/pekko-http/_sidebar.md
render scala-http4s > docs/scala/http4s/_sidebar.md
render java > docs/java/_sidebar.md
render java-dropwizard > docs/java/dropwizard/_sidebar.md
render java-spring-mvc > docs/java/spring-mvc/_sidebar.md
