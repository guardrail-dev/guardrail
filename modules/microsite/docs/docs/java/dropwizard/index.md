---
layout: docs
title: "dropwizard - java - guardrail"
---

Table of Contents
=================

1. [What is guardrail](what-is-guardrail)
   1. [Single Point of Truth](what-is-guardrail#single-point-of-truth)
   1. [Unexpected API changes are compiler errors](what-is-guardrail#unexpected-api-changes-are-compiler-errors)
   1. [Fewer binary dependencies](what-is-guardrail#fewer-binary-dependencies)
1. [Installation](installation)
1. [Sample API specification](sample-api-specification)
1. [Generating a Server](generating-a-server)
   1. [Setup](generating-a-server#setup)
   1. [Server Handlers, Resources](generating-a-server#server-handlers-resources)
   1. [Separation of business logic](generating-a-server#separation-of-business-logic)
   1. [API structure slip is impossible](generating-a-server#api-structure-slip-is-impossible)
1. [Generating clients](generating-clients)

Java Dropwizard
---------------

Support for Dropwizard 1.3 has been available since guardrail v0.45.0.

Using guardrail you can generate server definions and http clients.
