{-# LANGUAGE TupleSections #-}

module Main where
import PlanarTerms
import System.Environment
import System.FilePath
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  let parseNumArgs xs = case mapM readMaybe xs of
       Just [sz,from,n] -> Just (sz,from,n) :: Maybe (Int,Int,Int)
       _ -> Nothing
  let opts = case args of
               ("--svgdir":s:rest) -> fmap (Just s,) $ parseNumArgs rest
               _ -> fmap (Nothing,) $ parseNumArgs args
  case opts of
    Nothing -> putStrLn "usage: [--svgdir DIR] TERMSIZE FIRST N" >>
               putStrLn "Prints N planar terms of given size beginning (0 indexed) at FIRST." >>
               putStrLn "When given a directory, instead of printing, emits them as svgs."
    Just (msvgdir,(sz,from,n)) ->
       let tms = take n $ drop from $ genTerm sz
       in  case msvgdir of
            Nothing -> mapM_ (putStrLn . showFOL . termToFOL) tms
            Just svgdir -> mapM_ (uncurry emitSvg) . (`zip` (map ((svgdir </>) . show) [from..])) $ tms