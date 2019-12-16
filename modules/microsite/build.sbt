enablePlugins(MicrositesPlugin)
micrositePushSiteWith := GitHub4s
micrositeGithubToken := Option(System.getenv("GITHUB_TOKEN"))

micrositeName := "guardrail"
micrositeDescription := "Principled code generation from OpenAPI specifications"
micrositeUrl := "https://guardrail.dev/"
micrositeDocumentationLabelDescription := "Documentation"
micrositeAuthor := "Twilio"
micrositeOrganizationHomepage := "https://www.twilio.com/"
micrositeTwitter := "@guardrail_code"
micrositeTwitterCreator := "@twilio"
micrositeGithubOwner := "twilio"
micrositeGithubRepo := "guardrail"
micrositeHighlightLanguages ++= Seq("yaml", "scala")
micrositeCompilingDocsTool := WithMdoc
micrositeGitterChannel := false
micrositeAnalyticsToken := "UA-154175369-1"
micrositeTheme := "pattern"
mdocIn := baseDirectory.value / "docs"
//micrositeDataDirectory := baseDirectory.value / "docs" / "data"
//micrositeImgDirectory := baseDirectory.value / "docs" / "images"

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
// micrositeDocumentationUrl
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
