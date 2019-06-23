{-# LANGUAGE TypeFamilies #-}

{- |
   Module      : Data.Graph.Planar.Serialisation.Internal
   Description : Internal definitions of serialisation classes.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.Graph.Planar.Serialisation.Internal where

import Data.Graph.Planar

import Blaze.ByteString.Builder
import Data.Attoparsec.ByteString.Lazy
import Data.Function(on)
import Data.List(groupBy, sortBy, mapAccumL)
import qualified Data.Map as M
import Data.Word(Word)
import Control.Arrow(first, second)
import Control.Monad(liftM2)

-- -----------------------------------------------------------------------------

-- | A class covering the different ways of encoding and decoding
--   planar graphs from binary data.
class PlanarEncoding ser where
  type NLabel ser
  type ELabel ser

  putSG :: ser -> ((Int,Int),SerialisedGraph (NLabel ser) (ELabel ser)) -> Builder

  getSG :: ser -> Parser (SerialisedGraph (NLabel ser) (ELabel ser))

  -- | Print the required header if appropriate; otherwise return an
  --   empty 'Builder'.  Should end in a newline if appropriate.
  putName :: ser -> Builder

  -- | Attempt to parse a header; if none exists, this should return
  --   an appropriate default (if allowable).  Should also parse
  --   trailing newlines if appropriate.
  getName :: Parser ser

  -- | Is each graph on a new line?
  sepByNewline :: ser -> Bool

-- -----------------------------------------------------------------------------

type SerialisedNode n e = (Word, n, [SerialisedEdge e])

nodeSer :: SerialisedNode n e -> Word
nodeSer (n, _, _) = n

nodeLabelSer :: SerialisedNode n e -> n
nodeLabelSer (_, l, _) = l

nodeEdgesSer :: SerialisedNode n e -> [SerialisedEdge e]
nodeEdgesSer (_, _, es) = es

withEdgesSer :: ([SerialisedEdge e] -> a)
                -> SerialisedNode n e -> (Word, n, a)
withEdgesSer f (n, l, es) = (n, l, f es)

type SerialisedEdge e = (Word, Word, e, Word)

edgeIDSer :: SerialisedEdge e -> Word
edgeIDSer (e, _, _, _) = e

toNodeSer :: SerialisedEdge e -> Word
toNodeSer (_, t, _, _) = t

edgeLabelSer :: SerialisedEdge e -> e
edgeLabelSer (_, _, l, _) = l

inverseEdgeSer :: SerialisedEdge e -> Word
inverseEdgeSer (_, _, _, ei) = ei

-- -----------------------------------------------------------------------------

-- Process a PlanarCode-like input.  The [[Word]] is expected to be 0-based node IDs.
processPC :: [[Word]] -> SerialisedGraph () ()
processPC ess = snd . mapAccumL processNode initSt $ nes'
  where
    -- Give each node an ID.
    nes = zip [0..] ess

    -- Give all the edges temporary IDs to start with.

    -- eIDsTmp :: [((from node, to node), tmpID)]
    eIDsTmp = flip zip [0 :: Word ..] . concatMap (uncurry (map . (,))) $ nes
    -- nes' :: [(from node, [(to node, tmpID)])]
    nes' = groupSortCollectBy (fst . fst) (first snd) eIDsTmp

    -- eGrps :: [((from node, to node), [tmpID])]
    eGrps = map (\ (f,(t,es)) -> ((f,t),es))
            . concatMap (uncurry $ map . (,))
            . map (second $ groupSortCollectBy fst snd)
            $ nes'

    eMp = M.fromList eGrps

    -- tmpInvs :: Map tmpID tmpID
    tmpInvs = M.fromList
              . concatMap getInvs
              $ eGrps

    getInvs ((f,t),ftes) = zip ftes . neighbourList' $ eMp M.! (t,f)
        where
          neighbourList' | f == t    = reverse
                         | otherwise = neighbourList

    processEdge (assgnd, next) (t, eTmp) = case eTmp `M.lookup` assgnd of
                                             Just (e,ei) -> ((assgnd,next), (e, t, (), ei)) -- Can delete eTmp here
                                             Nothing     -> ((assgnd',next'), (e', t, (), ei'))
        where
          e' = next
          ei' = succ e'
          next' = succ ei'
          assgnd' = M.insert (tmpInvs M.! eTmp) (ei', e') assgnd

    processNode st (f,esTmp) = second ((,,) f ()) $ mapAccumL processEdge st esTmp

    initSt = (M.empty, 0)

-- Swap the edge list of a neighbour.
neighbourList        :: [a] -> [a]
neighbourList []     = []
neighbourList (e:es) = e : reverse es

applyUntil :: (Monad m) => (a -> Bool) -> m a -> m [a]
applyUntil p m = do a <- m
                    if p a
                      then return [a]
                      else do as <- applyUntil p m
                              return $ a:as

groupSortBy   :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

groupSortCollectBy     :: (Ord b) => (a -> b) -> (a -> c) -> [a] -> [(b,[c])]
groupSortCollectBy f g = map (liftM2 (,) (f . head) (map g)) . groupSortBy f

swap       :: (a,b) -> (b,a)
swap (a,b) = (b,a)
