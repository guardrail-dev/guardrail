package dev.guardrail.core

trait TrackerTestExtensions {
  implicit class TrackerSyntax(o: Tracker.type) {
    def apply[A](value: A): Tracker[A] = new Tracker(value, Vector.empty)
  }
}
