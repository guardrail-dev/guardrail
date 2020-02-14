# Library compatibility matrix

The following tables are intended to be rough guidelines on which library
versions are generally expected to have a fair degree of support for
different guardrail features.

Only direct dependencies are listed.

## Scala

### akka-http

akka-http has no direct dependencies on circe or cats, which is why the versions are unrelated.

guardrail version | akka-http | akka-stream | circe  | cats  | Notes
----------------- | --------- | ----------- | ------ | ----- | -----
0.34.0 ⚠          | 10.0.2    |             | 0.6.0  | 0.8.1 | Some syntax incompatibilities
 "                |  "        |             | 0.7.0  | 0.9.0 |
 "                |  "        |             | 0.8.0  | 0.9.0 |
 "                |  "        |             | 0.9.0  | 1.0.1 |
 "                |  "        |             | 0.10.0 | 1.4.0 |
 "                |  "        |             | 0.11.0 | 1.5.0 |
 "                | 10.0.15   |             |  "     |  "    |
 "                | 10.1.0    | 2.3.5       |  "     |  "    |
 "                | 10.1.7    | 2.5.19      |  "     |  "    |
0.54.0            |  "        |  "          | 0.12.1 |  "    | For Scala 2.11, use --module circe-java8

### http4s

guardrail version | http4s | circe-core | cats  | cats-effect | Notes
----------------- | ------ | ---------- | ----- | ----------- | -----
0.34.0 ⚠          | 0.18.0 | 0.9.0      | 1.0.1 | 0.10        | Only clients are supported
0.39.0            | 0.18.0 |  "         |  "    |  "          | First server release
0.41.1            | 0.19.0 | 0.10.0     | 1.4.0 | 1.0.0       |
0.51.0            | 0.20.0 |  "         |  "    |  "          |
0.54.0            |   "    | 0.12.1     |  "    |  "          | For Scala 2.11, use --module circe-java8
0.55.4            | 0.21.0 | 0.13.0     | 2.1.0 | 2.1.0       |
