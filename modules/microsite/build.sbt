enablePlugins(MicrositesPlugin)
micrositePushSiteWith := GitHub4s
micrositeGithubToken := Option(System.getenv("GITHUB_TOKEN"))

micrositeName := "guardrail"
micrositeDescription := "Principled code generation from OpenAPI specifications"
micrositeUrl := "https://guardrail.dev/"
micrositeDocumentationLabelDescription := "Documentation"
micrositeAuthor := "Twilio"
micrositeTwitterCreator := "@guardrail_code"
micrositeGithubOwner := "twilio"
micrositeGithubRepo := "guardrail"
micrositeHighlightLanguages ++= Seq("yaml", "swagger")
micrositeCompilingDocsTool := WithMdoc
mdocIn := baseDirectory.value / "docs"
//micrositeDataDirectory := baseDirectory.value / "docs" / "data"
//micrositeImgDirectory := baseDirectory.value / "docs" / "images"

// micrositeCssDirectory
// micrositeExternalIncludesDirectory
// micrositeExternalLayoutsDirectory
// micrositeJsDirectory
// micrositePluginsDirectory
// micrositeStaticDirectory

// micrositeAnalyticsToken
// micrositeBaseUrl
// micrositeCDNDirectives
// micrositeConfig
// micrositeConfigYaml
// micrositeDocumentationUrl
// micrositeEditButton
// micrositeExtraMdFiles
// micrositeExtraMdFilesOutput
// micrositeFavicons
// micrositeFooterText
// micrositeGitHostingService
// micrositeGitHostingUrl
// micrositeGithubLinks
// micrositeGitterChannel
// micrositeGitterChannelUrl
// micrositeHighlightTheme
// micrositeHomepage
// micrositeMakeExtraMdFiles
// micrositeOrganizationHomepage
// micrositePalette
// micrositeShareOnSocial
// micrositeTutExtraMdFiles
// micrositeTwitter
