let home = "/Users/mulk" in

{ host = "mulkinator"
, stashes =
    [ { name = "mulk.tar"
      , baseDir = home

      , tiers =
          { hourly  = { keep = 24 }
          , daily   = { keep =  7 }
          , weekly  = { keep =  4 }
          , monthly = { keep = 12 }
          }

      , exclusions =
          [
          , "**/.stack-work"
          , "**/dist-newstyle"

          , "${home}/.boot/cache"
          , "${home}/.cabal/bin"
          , "${home}/.cabal/packages"
          , "${home}/.cache"
          , "${home}/.cargo/bin"
          , "${home}/.cargo/registry"
          , "${home}/.codestream/agent"
          , "${home}/.conan/data"
          , "${home}/.cpanm"
          , "${home}/.cpanplus"
          , "${home}/.ghcup"
          , "${home}/.gradle"
          , "${home}/.hoogle"
          , "${home}/.ivy2"
          , "${home}/.m2"
          , "${home}/.npm"
          , "${home}/.rustup"
          , "${home}/.sbt"
          , "${home}/.stack"

          , "${home}/Library/Caches"

          , "${home}/Library/Containers/com.apple.Safari/Data/Library/Caches"
          , "${home}/Library/Containers/com.atlassian.jira.mac/Data/Library/Caches"
          , "${home}/Library/Containers/com.docker.docker"
          , "${home}/Library/Containers/com.tinyspeck.slackmacgap"

          , "${home}/Library/Metadata/CoreSpotlight"

          , "${home}/Library/Safari/*.db"
          , "${home}/Library/Safari/*.db-*"
          ]
      }
    ]
}
