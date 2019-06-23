{-# LANGUAGE TypeFamilies #-}

{- |
   Module      : Data.Graph.Planar.Serialisation.AsciiCode
   Description : Implementation of ASCII CODE.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

 -}
module Data.Graph.Planar.Serialisation.AsciiCode(AsciiCode(..)) where

import Data.Graph.Planar(SerialisedGraph)
import Data.Graph.Planar.Serialisation.Internal

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.Attoparsec.ByteString.Lazy hiding (satisfy)
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString as SBS
import Data.Char(ord)
import Data.List(intersperse)
import Data.Foldable(foldMap)
import Control.Applicative((<*))
import Data.Monoid(Monoid(..))
import Data.Word(Word8,Word)

-- -----------------------------------------------------------------------------

{- |

 ASCII_CODE is a human-readable variant of
 "Data.Graph.Planar.Serialisation.PlanarCode".  The same caveats
 regarding loops apply, but it is only able to represent graphs with
 @<=26@ nodes.

 -}
data AsciiCode = AsciiCode
                 deriving (Eq, Ord, Show, Read)

instance PlanarEncoding AsciiCode where
  type NLabel AsciiCode = ()
  type ELabel AsciiCode = ()

  putSG = const putAsciiCode

  getSG = const getAsciiCode

  putName = mempty

  getName = return AsciiCode

  sepByNewline = const True

putAsciiCode :: ((Int,Int),SerialisedGraph n e) -> Builder
putAsciiCode ((p,_),sg) = fromShow p `mappend` fromChar ' '
                          `mappend`
                          mconcat (intersperse (fromChar ',')
                                   $ map (fromWrite . putNode . nodeEdgesSer) sg)
  where
    putNode es = foldMap (writeAsLetter . toNodeSer) es

writeAsLetter :: Word -> Write
writeAsLetter = writeWord8 . (aValue+) . fromIntegral

getAsciiCode :: Parser (SerialisedGraph () ())
getAsciiCode = do _ <- C.satisfy C.isDigit <* C.skipSpace -- We don't actually use this...
                  ess <- sepBy1 (SBS.unpack `fmap` C.takeWhile1 C.isAlpha_ascii)  (C.char ',')
                  let ess' = map (map $ fromIntegral . subtract aValue) ess
                  return $ processPC ess'

aValue :: Word8
aValue = fromIntegral $ ord 'a'
