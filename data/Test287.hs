showPackageDetailedInfo pkginfo =
  renderStyle (style { lineLength = 80, ribbonsPerLine = 1 })
    $   char '*'
    $+$ something
          [ entry "Synopsis" synopsis hideIfNull reflowParagraphs
          , entry "Versions available"
                  sourceVersions
                  (altText null "[ Not available from server ]")
                  (dispTopVersions 9 (preferredVersions pkginfo))
          , entry
            "Versions installed"
            installedVersions
            (altText
              null
              (if hasLib pkginfo then "[ Not installed ]" else "[ Unknown ]")
            )
            (dispTopVersions 4 (preferredVersions pkginfo))
          , entry "Homepage"      homepage     orNotSpecified  text
          , entry "Bug reports"   bugReports   orNotSpecified  text
          , entry "Description"   description  hideIfNull      reflowParagraphs
          , entry "Category"      category     hideIfNull      text
          , entry "License"       license      alwaysShow      disp
          , entry "Author"        author       hideIfNull      reflowLines
          , entry "Maintainer"    maintainer   hideIfNull      reflowLines
          , entry "Source repo"   sourceRepo   orNotSpecified  text
          , entry "Executables"   executables  hideIfNull      (commaSep text)
          , entry "Flags" flags hideIfNull (commaSep dispFlag)
          , entry "Dependencies" dependencies hideIfNull (commaSep dispExtDep)
          , entry "Documentation" haddockHtml  showIfInstalled text
          , entry "Cached"        haveTarball  alwaysShow      dispYesNo
          , if not (hasLib pkginfo)
            then empty
            else text "Modules:"
              $+$ nest 4 (vcat (map disp . sort . modules $ pkginfo))
          ]
