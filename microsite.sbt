enablePlugins(MicrositesPlugin)
micrositePushSiteWith := GitHub4s

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
micrositeDataDirectory := (baseDirectory in Compile).value / "docs" / "data"

// micrositeCssDirectory
// micrositeDataDirectory
// micrositeExternalIncludesDirectory
// micrositeExternalLayoutsDirectory
// micrositeImgDirectory
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
