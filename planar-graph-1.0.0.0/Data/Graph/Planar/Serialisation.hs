{-# LANGUAGE TypeFamilies #-}

{- |
   Module      : Data.Graph.Planar.Serialisation
   Description : Serialisation for planar graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

 -}
module Data.Graph.Planar.Serialisation
    ( PlanarEncoding(..)
    , encodePlanarFile
    , encodePlanarFileFrom
    , decodePlanarFile
    ) where

import Data.Graph.Planar hiding (isEmpty)
import Data.Graph.Planar.Serialisation.Internal

import Blaze.ByteString.Builder(toLazyByteString)
import Blaze.ByteString.Builder.Char8(fromChar)
import Data.Attoparsec.ByteString.Lazy(parse, eitherResult, many1, (<?>))
import Data.Attoparsec.ByteString.Char8(endOfLine)
import qualified Data.ByteString.Lazy as B
import Data.Monoid(mempty, mappend)
import Control.Applicative((<*))
import Control.Monad(foldM)

-- -----------------------------------------------------------------------------

-- | Encode a list of planar graphs to file using the specified
--   encoding.
encodePlanarFile :: (PlanarEncoding ser) => ser -> FilePath
                    -> [PlanarGraph (NLabel ser) (ELabel ser)]
                    -> IO Int
encodePlanarFile ser fp = encodePlanarFileFrom ser fp . map ((,) Nothing)

-- | Encode a list of planar graphs to file using the specified
--   encoding, with the serialisation traversing from the an
--   optionally specified edge.
encodePlanarFileFrom :: (PlanarEncoding ser) => ser -> FilePath
                        -> [(Maybe Edge,PlanarGraph (NLabel ser) (ELabel ser))]
                        -> IO Int
encodePlanarFileFrom ser fp pgs = do B.writeFile fp $ toLazyByteString header
                                     foldM printCount 0 pgs
  where
    header = putName ser

    maybeNewline | sepByNewline ser = fromChar '\n'
                 | otherwise        = mempty

    printCount c pg = c `seq` (B.appendFile fp (toB pg) >> return (c+1))
    toB pg = toLazyByteString $ putSG ser (toSer pg) `mappend` maybeNewline

    toSer (me,pg) = ((order pg, size pg), serialiseBFS pg me)

-- | Read in a file containing encoded graphs.  The 'PlanarEncoding'
--   argument is only used for its /type/ to determine which parser to
--   use.
decodePlanarFile        :: (PlanarEncoding ser) => ser -> FilePath
                           -> IO [PlanarGraph (NLabel ser) (ELabel ser)]
decodePlanarFile ser fp = do bs <- B.readFile fp
                             case eitherResult $ parse parser bs of
                               Left err  -> error $ "Could not parse file " ++ fp ++ " with the error: " ++ err
                               Right sgs -> return $ map deserialise sgs

    where
      parser = do nm <- fmap (`asTypeOf` ser) getName <?> "Parsing encoding header"
                  many1 $ getSG nm <* maybeNewLine

      maybeNewLine | sepByNewline ser = endOfLine
                   | otherwise        = return ()
