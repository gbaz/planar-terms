{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

{- |
   Module      : Data.Graph.Planar.Serialisation.PlanarCode
   Description : Implementation of PLANAR CODE.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

 -}
module Data.Graph.Planar.Serialisation.PlanarCode(PlanarCode(..)) where

import Data.Graph.Planar(SerialisedGraph)
import Data.Graph.Planar.Serialisation.Internal

import Blaze.ByteString.Builder
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString as SBS
import Data.Foldable(foldMap)
import Control.Applicative((<*))
import Data.Monoid(Monoid(..))
import Control.Monad(replicateM)

-- -----------------------------------------------------------------------------

{- |

 PLANAR_CODE is the most common encoding for planar graphs, and is
 supported by various generation and visualisation tools.  It is a
 binary format and not intended to be human-readable.

 The default encoding only supports graphs with @<256@ nodes, and
 takes @2*|E|+|N|+1@ bytes per graph.

 Please note that PLANAR_CODE is /not/ suitable for graphs with
 multiple loops on vertices (multiple edges with distinct endpoints
 however are catered for).  As such, no guarantees are made about what
 happens with multiple loops.

 -}
data PlanarCode = PlanarCode
                  deriving (Eq, Ord, Show, Read)

instance PlanarEncoding PlanarCode where
  type NLabel PlanarCode = ()
  type ELabel PlanarCode = ()

  putSG = const putPlanarCode

  getSG = const getPlanarCode

  putName = const $ fromByteString ">>planar_code<<"

  getName = string ">>planar_code<<" >> return PlanarCode

  sepByNewline = const False

putPlanarCode :: ((Int,Int),SerialisedGraph n e) -> Builder
putPlanarCode ((ord,_),sg) = fromWord8 (fromIntegral ord)
                             `mappend` foldMap (fromWrite . putNode . nodeEdgesSer)
                                               sg
  where
    putNode es = foldMap (writeWord8 . succ . fromIntegral . toNodeSer) es
                 `mappend` writeWord8 0
                 -- Need succ here, because the SerialisedGraph is 0-based, but PC is 1-based.

getPlanarCode :: Parser (SerialisedGraph () ())
getPlanarCode = do num <- fromIntegral `fmap` anyWord8
                   ess <- replicateM num getNode
                   -- Convert to 0-based Word values
                   let ess' = map (map $ fromIntegral . pred) ess
                   return $ processPC ess'
  where
    getNode = SBS.unpack `fmap` takeWhile1 (/= 0) <* anyWord8 -- will be 0
