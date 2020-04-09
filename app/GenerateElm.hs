{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.List
import           Data.Text                         hiding ( intercalate
                                                          , map
                                                          )
import           Servant.Elm                              ( DefineElm(DefineElm)
                                                          , UrlPrefix (Static)
                                                          , Proxy(Proxy)
                                                          , defaultOptions
                                                          , defElmImports
                                                          , defElmOptions
                                                          , deriveBoth
                                                          , generateElmModuleWith
                                                          , urlPrefix
                                                          )
import           Api.User                    (UserAPI)
import           Models                      (User)

main :: IO ()
main = generateElmModuleWith
  defElmOptions {urlPrefix = Static "http://localhost:8081"}
  ["Api"]
  defElmImports
  "../src"
  [DefineElm (Proxy :: Proxy User)]
  (Proxy :: Proxy UserAPI)
