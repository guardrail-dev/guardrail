package dev.guardrail

package core {
  object implicits extends IndexedDistributiveImplicits with MappishImplicits
}

package object core extends core.IndexedDistributiveImplicits with core.MappishImplicits
