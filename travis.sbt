pgpPublicRing := file("./travis/local.pubring.asc")
pgpSecretRing := file("./travis/local.secring.asc")

lazy val pgpPass = Option(System.getenv("PGP_PASS"))

pgpPassphrase := {
    if (pgpPass.isDefined) {
      println("Running under CI and PGP password specified under settings.")
      pgpPass.map(_.toCharArray)
    } else {
      println("Could not find settings for a PGP passphrase.")
      None
    }
}
