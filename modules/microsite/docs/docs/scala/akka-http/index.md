---
layout: docs
title: "akka-http - scala - guardrail"
---

Table of Contents
=================

1. [What is guardrail](what-is-guardrail)
   1. [Single Point of Truth](what-is-guardrail#single-point-of-truth)
   1. [Unexpected API changes are compiler errors](what-is-guardrail#unexpected-api-changes-are-compiler-errors)
   1. [Fewer binary dependencies](what-is-guardrail#fewer-binary-dependencies)
1. [Installation](installation)
1. [Sample API specification](sample-api-specification)
1. [Generating Domain Objects](dtos)
1. [Generating a Server](generating-a-server)
   1. [Separation of business logic](generating-a-server#separation-of-business-logic)
   1. [API structure slip is impossible](generating-a-server#api-structure-slip-is-impossible)
   1. [Generating test-only (real) server mocks for unit tests](generating-a-server#generating-test-only-real-server-mocks-for-unit-tests)
   1. [A note about scalatest integration](generating-a-server#a-note-about-scalatest-integration)
1. [Generating clients](generating-clients)
   1. [Separation of protocol-concerns from API-level concerns](generating-clients#separation-of-protocol-concerns-from-api-level-concerns)
1. [guardrail Extensions](guardrail-extensions)
