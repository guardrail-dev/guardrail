ghreleaseRepoOrg := "twilio"
ghreleaseRepoName := "guardrail"

ghreleaseAssets := Seq.empty

ghreleaseNotes := { tagName =>
  val ver = tagName.stripPrefix("v")
  IO.read(baseDirectory.value / "notes" / s"${ver}.md")
}
ghreleaseTitle := { tagName =>
  val ver = tagName.stripPrefix("v")
  IO.read(baseDirectory.value / "notes" / s"${ver}.md").linesIterator.take(1).toList.head
}
