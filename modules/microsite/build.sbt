enablePlugins(MicrositesPlugin)
micrositePushSiteWith := GitHub4s
micrositeGithubToken := Option(System.getenv("MICROSITE_PUBLISH_TOKEN"))

micrositeName := "guardrail"
micrositeDescription := "Principled code generation from OpenAPI specifications"
micrositeUrl := "https://guardrail.dev/"
micrositeAuthor := "guardrail.dev"
micrositeOrganizationHomepage := "https://guardrail.dev/"
micrositeTwitter := "@guardrail_code"
micrositeTwitterCreator := "@guardrail_dev"
micrositeGithubOwner := "guardrail-dev"
micrositeGithubRepo := "guardrail"
micrositeHighlightLanguages ++= Seq("yaml", "scala")
micrositeGitterChannel := false
micrositeAnalyticsToken := "UA-154175369-1"
micrositeTheme := "light"
mdocIn := baseDirectory.value / "docs"
//micrositeDataDirectory := baseDirectory.value / "docs" / "data"
//micrositeImgDirectory := baseDirectory.value / "docs" / "images"

micrositeDocumentationLabelDescription := "Documentation"
micrositeDocumentationUrl := "/docs/"
micrositeHomeButtonTarget := "docs"

// micrositeCssDirectory
// micrositeExternalIncludesDirectory
// micrositeExternalLayoutsDirectory
// micrositeJsDirectory
// micrositePluginsDirectory
// micrositeStaticDirectory

// micrositeBaseUrl
// micrositeCDNDirectives
// micrositeConfig
// micrositeConfigYaml
// micrositeEditButton
// micrositeExtraMdFiles
// micrositeExtraMdFilesOutput
// micrositeFavicons
// micrositeFooterText
// micrositeGitHostingService
// micrositeGitHostingUrl
// micrositeGithubLinks
// micrositeGitterChannelUrl
// micrositeHighlightTheme
// micrositeHomepage
// micrositeMakeExtraMdFiles
// micrositeOrganizationHomepage
// micrositePalette
// micrositeShareOnSocial
// micrositeTutExtraMdFiles
// micrositeTwitter
