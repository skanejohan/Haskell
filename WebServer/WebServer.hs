{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy (pack)
import Data.Monoid (mconcat)
import Network.Wai
import Network.Wai.Middleware.AddHeaders

import Math.ExpressionParser
import Math.ExpressionUtils
import Math.Diff.Differentiation

main = scotty 80 $ do
	middleware $ addHeaders [("Access-Control-Allow-Origin", "*")]
	get "/differentiation" $ do
		file "Differentiation.html"
	get "/Differentiation" $ do
		file "Differentiation.html"
	get "/Differentiation.js" $ do
		file "Differentiation.js"
	get "/Differentiation.css" $ do
		file "Differentiation.css"
	get "/diff" $ do
		var  <- param "v"
		expr <- param "e"
		html $ pack $ differentiate (head var) expr

differentiate v e = case parseExpression e of
    Right result -> mathjax (simplify (diff v result))
    Left err     -> show err
