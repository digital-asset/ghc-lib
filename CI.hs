
-- CI script, invoked by both Travis and Appveyor

import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra

main :: IO ()
main = do
    let cmd x = do
            putStrLn $ "\n\n# Running: " ++ x
            hFlush stdout
            (t, _) <- duration $ system_ x
            putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
            hFlush stdout
    -- debugging
    -- cmd "echo 'GET / HTTP/1.0' | openssl s_client -state -nbio -connect gitlab.haskell.org:443"
    when isWindows $
        cmd "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"
    -- We want to use gitlab but Travis has issues:
    -- "fatal: unable to access 'https://gitlab.haskell.org/ghc/ghc.git/': gnutls_handshake() failed: Handshake failed"
    cmd $ unlines [
      "cat >rootCA.pem <<EOF"
      , "MIIEkjCCA3qgAwIBAgIQCgFBQgAAAVOFc2oLheynCDANBgkqhkiG9w0BAQsFADA/"
      , "MSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT"
      , "DkRTVCBSb290IENBIFgzMB4XDTE2MDMxNzE2NDA0NloXDTIxMDMxNzE2NDA0Nlow"
      , "SjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUxldCdzIEVuY3J5cHQxIzAhBgNVBAMT"
      , "GkxldCdzIEVuY3J5cHQgQXV0aG9yaXR5IFgzMIIBIjANBgkqhkiG9w0BAQEFAAOC"
      , "AQ8AMIIBCgKCAQEAnNMM8FrlLke3cl03g7NoYzDq1zUmGSXhvb418XCSL7e4S0EF"
      , "q6meNQhY7LEqxGiHC6PjdeTm86dicbp5gWAf15Gan/PQeGdxyGkOlZHP/uaZ6WA8"
      , "SMx+yk13EiSdRxta67nsHjcAHJyse6cF6s5K671B5TaYucv9bTyWaN8jKkKQDIZ0"
      , "Z8h/pZq4UmEUEz9l6YKHy9v6Dlb2honzhT+Xhq+w3Brvaw2VFn3EK6BlspkENnWA"
      , "a6xK8xuQSXgvopZPKiAlKQTGdMDQMc2PMTiVFrqoM7hD8bEfwzB/onkxEz0tNvjj"
      , "/PIzark5McWvxI0NHWQWM6r6hCm21AvA2H3DkwIDAQABo4IBfTCCAXkwEgYDVR0T"
      , "AQH/BAgwBgEB/wIBADAOBgNVHQ8BAf8EBAMCAYYwfwYIKwYBBQUHAQEEczBxMDIG"
      , "CCsGAQUFBzABhiZodHRwOi8vaXNyZy50cnVzdGlkLm9jc3AuaWRlbnRydXN0LmNv"
      , "bTA7BggrBgEFBQcwAoYvaHR0cDovL2FwcHMuaWRlbnRydXN0LmNvbS9yb290cy9k"
      , "c3Ryb290Y2F4My5wN2MwHwYDVR0jBBgwFoAUxKexpHsscfrb4UuQdf/EFWCFiRAw"
      , "VAYDVR0gBE0wSzAIBgZngQwBAgEwPwYLKwYBBAGC3xMBAQEwMDAuBggrBgEFBQcC"
      , "ARYiaHR0cDovL2Nwcy5yb290LXgxLmxldHNlbmNyeXB0Lm9yZzA8BgNVHR8ENTAz"
      , "MDGgL6AthitodHRwOi8vY3JsLmlkZW50cnVzdC5jb20vRFNUUk9PVENBWDNDUkwu"
      , "Y3JsMB0GA1UdDgQWBBSoSmpjBH3duubRObemRWXv86jsoTANBgkqhkiG9w0BAQsF"
      , "AAOCAQEA3TPXEfNjWDjdGBX7CVW+dla5cEilaUcne8IkCJLxWh9KEik3JHRRHGJo"
      , "uM2VcGfl96S8TihRzZvoroed6ti6WqEBmtzw3Wodatg+VyOeph4EYpr/1wXKtx8/"
      , "wApIvJSwtmVi4MFU5aMqrSDE6ea73Mj2tcMyo5jMd6jmeWUHK8so/joWUoHOUgwu"
      , "X4Po1QYz+3dszkDqMp4fklxBwXRsW10KXzPMTZ+sOPAveyxindmjkW8lGy+QsRlG"
      , "PfZ+G6Z6h7mjem0Y+iWlkYcV4PIWL1iwBi8saCbGS5jN2p8M+X+Q7UNKEkROb3N6"
      , "KOqkqm57TH2H3eDJAkSnh6/DNFu0Qg=="
      , "-----END CERTIFICATE-----"
      ,"EOF"
      ]
    cmd $ "git config --global http.sslCAInfo `pwd`/rootCA.pem"
    cmd $ "echo 'GET / HTTP/1.0' | gnutls-cli --print-cert gitlab.haskell.org"
    cmd "GIT_CURL_VERBOSE=1 git clone https://gitlab.haskell.org/ghc/ghc.git --recursive" -- --recurse-submodules=libraries/Cabal
    -- not essential, but make the cache work between Hadrian and ghc-lib and build Hadrian quicker
    appendFile "ghc/hadrian/stack.yaml" $ unlines ["ghc-options:","  \"$everything\": -O0 -j"]
    cmd "stack exec -- ghc-lib-gen ghc"

    -- Generate an sdist and extract it to ensure it works
    stackYaml <- readFile' "stack.yaml"
    writeFile "stack.yaml" $ stackYaml ++ unlines ["- ghc"]
    cmd "stack sdist ghc --tar-dir=."
    [tarball] <- filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    cmd $ "tar -xf " ++ tarball
    renameDirectory (dropExtension $ dropExtension tarball) "ghc-lib"

    -- Test the new projects
    writeFile "stack.yaml" $
      stackYaml ++
      unlines [ "- ghc-lib"
              , "- examples/mini-hlint"
              , "- examples/mini-compile"
              ]
    cmd "stack build --no-terminal --interleaved-output"
    cmd "stack exec --no-terminal -- ghc-lib --version"
    cmd "stack exec --no-terminal -- mini-hlint examples/mini-hlint/test/MiniHlintTest.hs"
    cmd "stack exec --no-terminal -- mini-compile examples/mini-compile/test/MiniCompileTest.hs"
