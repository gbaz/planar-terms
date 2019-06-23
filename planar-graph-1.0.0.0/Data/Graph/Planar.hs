{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      : Data.Graph.Planar
   Description : Planar Graph data structure.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Planar graphs are graphs that can be embedded onto a surface
   (i.e. they can be drawn on that surface without any edges crossing).
   As such, it is preferable to use a dedicated data structure for them
   that has information about how to achieve this embedding rather than a
   standard graph data structure.

   (Please note however that this implementation has only been tested
   in terms of the embedding being on the unit sphere or disc; whether
   it works or not as-is on any other type of surface is unknown.)

   The implementation here is loosely based upon that found in
   /plantri/ by Gunnar Brinkmann and Brendan McKay:
   <http://cs.anu.edu.au/~bdm/plantri/> (which is similar in concept
   to a doubly-connected edge list).  The main differences are (if my
   understanding of the C code is correct):

   * plantri uses arrays (technically it uses one big array that it
     continually mutates); planar-graph uses Maps (thus making it
     easier to grow/shrink graphs).

   * plantri doesn't explicitly store nodes, just edges.

   * plantri utilises pointers, avoiding extra lookups.

   * Each edge stores in plantri has the face it is on, but only after
     they are explicitly calculated.  In planar-graph, @getFaces@ instead
     returns a Map for the faces.

   * plantri doesn't allow labels.

   In particular, all edges - even undirected ones - are stored as two
   opposing directed half-edges.  As such, care should be taken when
   dealing with edges.  Also, the 'Node', 'Edge' and 'Face'
   identifiers are all abstract, and as such cannot be constructed
   directly.

   All returned 'CList's represent values in a clockwise fashion
   (relative to the 'Node' or 'Face' in question).

   Care should also be taken when dealing with more than one connected
   component, as there is no fixed embedding of multiple graphs on the
   same surface.

 -}
module Data.Graph.Planar
       ( PlanarGraph
         -- * Graph Information
         -- ** Information about the nodes
       , Node
       , order
       , hasNode
       , nodes
       , labNodes
       , outgoingEdges
       , incomingEdges
       , neighbours
       , nodeLabel
         -- ** Information about the edges
         -- $edges
       , Edge
       , size
       , hasEdge
       , halfEdges
       , labHalfEdges
       , halfEdgesBetween
       , labHalfEdgesBetween
       , edges
       , labEdges
       , edgesBetween
       , labEdgesBetween
       , fromNode
       , toNode
       , prevEdge
       , nextEdge
       , inverseEdge
       , edgeLabel
         -- * Graph Manipulation
       , mergeGraphs
       , mergeAllGraphs
         -- ** Graph Construction
       , empty
       , addNode
       , addUNode
       , EdgePos(..)
       , addEdge
       , addEdgeUndirected
       , addUEdge
         -- ** Graph Deconstruction
       , isEmpty
       , deleteNode
       , deleteEdge
       , contractEdge
         -- ** Other
       , unlabel
       , mapNodes
       , adjustNodeLabel
       , setNodeLabel
       , mapEdges
       , adjustEdgeLabel
       , setEdgeLabel
         -- * Graph traversal
       , traverse
       , connectedComponents
       , renumber
         -- ** Controlling traversal
       , Traversal
       , breadthFirst
       , depthFirst
       , antiClockwiseTraversal
       , spanningTraversal
         -- ** Results of traversing
       , GraphTraversal
       , TraversedValues
       , visited
       , traversed
       , anyMissing
       , mergeGraphTraversals
         -- * Graph duals and faces
         -- $duals
         -- ** Faces in the graph
       , Face
       , FaceMap
       , FaceInfo
       , faceNodes
       , edgeCrossings
       , faceEdges
       , adjoiningFaces
       , getFaces
       , getFace
         -- ** Constructing the dual
       , makeDual
       , toDual
         -- * Isomorphism testing
       , canonicalExampleBy
       , onlyCanonicalExamples
         -- * Alternate representations
         -- ** Serialisation
         -- $serialisation
       , SerialisedGraph
       , serialise
       , deserialise
       , serialTraversal
       , serialiseBFS
         -- ** Pretty-Printing
       , prettify
       , prettyPrint
       ) where

import Prelude hiding (traverse)
import qualified Data.CircularList as CL
import Data.CircularList hiding (empty, isEmpty, size)
import qualified Data.Foldable as F
import Data.List(unfoldr,partition,mapAccumL,foldl1',delete)
import Data.Maybe(fromJust,fromMaybe)
import Data.Monoid(Monoid(mempty))
import qualified Data.Map as M
import Data.Map(Map, (!))
import qualified Data.Set as S
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (<|), (|>), (><), viewl, ViewL(..))
import Data.Word(Word)
import Control.Arrow((***), first, second)
import Control.DeepSeq(NFData(..))
import Control.Monad(liftM2,ap)

import Text.Read(Lexeme(Ident), lexP, parens, readPrec)
import Text.ParserCombinators.ReadPrec(ReadPrec, lift, prec)
import Text.ParserCombinators.ReadP(string, char, readS_to_P)

-- -----------------------------------------------------------------------------

{-

 INVARIANT: | e - inverse e | == 1

 That is, edge IDs are +/- 1 their inverse.

 The lower edge identifier is assumed to be the primary one (i.e. the
 actual edge).

-}

-- | The overall planar graph data structure.
data PlanarGraph n e = PG { _nodes :: !(NodeMap n)
                          , _edges :: !(EdgeMap e)
                          }
                       deriving (Eq)

instance Functor (PlanarGraph n) where
  fmap = mapEdges

instance (Show n, Show e) => Show (PlanarGraph n e) where
  showsPrec d pg = showParen (d > 10) $
                   showString "deserialise " . shows (serialise pg)

instance (Read n, Read e) => Read (PlanarGraph n e) where
  readPrec = parens . prec 10
             $ do Ident "deserialise" <- lexP
                  lst <- readPrec
                  return $ deserialise lst

instance (NFData n, NFData e) => NFData (PlanarGraph n e) where
  rnf (PG ns es) = rnf ns `seq` rnf es

withNodes      :: (NodeMap n -> NodeMap n') -> PlanarGraph n e -> PlanarGraph n' e
withNodes f pg = PG { _nodes = f $ _nodes pg
                    , _edges = _edges pg
                    }

{-# INLINE withNodes #-}

withEdges      :: (EdgeMap e -> EdgeMap e') -> PlanarGraph n e -> PlanarGraph n e'
withEdges f pg = PG { _nodes = _nodes pg
                    , _edges = f $ _edges pg
                    }

{-# INLINE withEdges #-}

-- | The number of nodes in the graph (i.e. @length . nodes@).
order :: PlanarGraph n e -> Int
order = M.size . _nodes

-- | The number of edges in the graph (i.e. @length . edges@).
size :: PlanarGraph n e -> Int
size = (`div`2) . M.size . _edges

-- | Remove all labels from this graph.
unlabel :: PlanarGraph n e -> PlanarGraph () ()
unlabel = withNodes rmNs . withEdges rmEs
  where
    rmNs = M.map (\ni -> ni {nodeInfo = ()})
    rmEs = M.map (\ei -> ei {edgeInfo = ()})

-- | Apply a mapping function over the node labels.
mapNodes   :: (n -> n') -> PlanarGraph n e -> PlanarGraph n' e
mapNodes f = withNodes (M.map mf)
  where
    mf ni = NInfo { outgoing = outgoing ni
                  , nodeInfo = f $ nodeInfo ni
                  }

mapNodeIDs :: (Node -> Node) -> PlanarGraph n e -> PlanarGraph n e
mapNodeIDs = mapNodeIDsWith M.mapKeys

mapNodeIDsMonotonic :: (Node -> Node) -> PlanarGraph n e -> PlanarGraph n e
mapNodeIDsMonotonic = mapNodeIDsWith M.mapKeysMonotonic

mapNodeIDsWith          :: ((Node -> Node) -> NodeMap n -> NodeMap n)
                           -> (Node -> Node) -> PlanarGraph n e -> PlanarGraph n e
mapNodeIDsWith mapMap f = withNodes (mapMap f)
                          . withEdges (M.map fEI)
  where
    fEI ei = ei { _fromNode = f $ _fromNode ei
                , _toNode   = f $ _toNode ei
                }

-- | Apply a mapping function over the edge labels.
mapEdges   :: (e -> e') -> PlanarGraph n e -> PlanarGraph n e'
mapEdges f = withEdges (M.map mf)
  where
    mf ei = EInfo { _fromNode = _fromNode ei
                  , _toNode   = _toNode ei
                  , _prevEdge = _prevEdge ei
                  , _nextEdge = _nextEdge ei
                  , inverse   = inverse ei
                  , edgeInfo  = f $ edgeInfo ei
                  }

mapEdgeIDs :: (Edge -> Edge) -> PlanarGraph n e -> PlanarGraph n e
mapEdgeIDs = mapEdgeIDsWith M.mapKeys

mapEdgeIDsMonotonic :: (Edge -> Edge) -> PlanarGraph n e -> PlanarGraph n e
mapEdgeIDsMonotonic = mapEdgeIDsWith M.mapKeysMonotonic

mapEdgeIDsWith          :: ((Edge -> Edge) -> EdgeMap e -> EdgeMap e)
                           -> (Edge -> Edge) -> PlanarGraph n e -> PlanarGraph n e
mapEdgeIDsWith mapMap f = withNodes (M.map fNI)
                          . withEdges (mapMap f . M.map fEI)
  where
    fNI ni = ni { outgoing = fmap f $ outgoing ni }

    fEI ei = ei { _prevEdge = f $ _prevEdge ei
                , _nextEdge = f $ _nextEdge ei
                , inverse   = f $ inverse ei
                }

-- | Is this node still in the graph?
hasNode      :: PlanarGraph n e -> Node -> Bool
hasNode pg n = n `M.member` _nodes pg

-- | All the nodes in the graph (in some arbitrary order).
nodes :: PlanarGraph n e -> [Node]
nodes = M.keys . _nodes

-- | All the nodes and their labels in the graph (in some arbitrary
--   order).
labNodes :: PlanarGraph n e -> [(Node, n)]
labNodes = map (second nodeInfo) . M.assocs . _nodes

-- | Is this edge still in the graph?
hasEdge      :: PlanarGraph n e -> Edge -> Bool
hasEdge pg e = e `M.member` _edges pg

-- | All the half-edges (thus also including inverses) in the graph
--   (in some arbitrary order).
halfEdges :: PlanarGraph n e -> [Edge]
halfEdges = M.keys . _edges

-- | All the half-edges and their labels in the graph (in some
--   arbitrary order).
labHalfEdges :: PlanarGraph n e -> [(Edge, e)]
labHalfEdges = map (second edgeInfo) . M.assocs . _edges

-- | A variant of 'halfEdges' that returns the pair of nodes that form an
--   edge rather than its unique identifier (again including inverse
--   edges).
halfEdgesBetween :: PlanarGraph n e -> [(Node,Node)]
halfEdgesBetween = map (liftM2 (,) _fromNode _toNode) . M.elems . _edges

-- | As with 'halfEdgesBetween', but including the labels.
labHalfEdgesBetween :: PlanarGraph n e -> [((Node,Node),e)]
labHalfEdgesBetween = map (liftM2 (,) (liftM2 (,) _fromNode _toNode) edgeInfo)
                      . M.elems . _edges

-- The following four functions cheat by abusing the fact that the
-- first edge of every pair added is even.

-- | All the primary edges in the graph returned in arbitrary order.
edges :: PlanarGraph n e -> [Edge]
edges = filter (even . edge) . halfEdges

-- | All the primary edges and their labels in the graph (in some
--   arbitrary order).
labEdges :: PlanarGraph n e -> [(Edge, e)]
labEdges = filter (even . edge . fst) . labHalfEdges

-- | A variant of 'edges' that returns the pair of nodes that form the
--   primary edges.
edgesBetween :: PlanarGraph n e -> [(Node,Node)]
edgesBetween = map (liftM2 (,) _fromNode _toNode . snd)
               . filter (even . edge . fst)
               . M.assocs . _edges

-- | As with 'edgesBetween' but including the labels.
labEdgesBetween :: PlanarGraph n e -> [((Node,Node),e)]
labEdgesBetween = map (liftM2 (,) (liftM2 (,) _fromNode _toNode) edgeInfo . snd)
                  . filter (even . edge . fst)
                  . M.assocs . _edges

{-

 The following definitions are defined such that when adding a new
 node/edge then old deleted identifiers can be reused, rather than
 continually marching on upwards.

 This way we can try and delay any possibility of the number of
 identifiers going over maxBound :: Word, and also try and prevent
 users from getting used to what the identifiers mean.

-}

newNodeID :: PlanarGraph n e -> Node
newNodeID = newID _nodes initNode succNode node

{-# INLINE newNodeID #-}

newEdgeID :: PlanarGraph n e -> Edge
newEdgeID = newID _edges initEdge succEdge edge

{-# INLINE newEdgeID #-}

newID :: (Ord k) => (PlanarGraph n e -> Map k a) -> k -> (k -> k)
         -> (k -> Word) -> PlanarGraph n e -> k
newID used initID succID fromID pg
    | M.null ks        = initID
    | compactMap       = succID maxUsed -- Need a completely new ID.
    | minUsed > initID = initID -- Missing from front
    | otherwise        = fst . head
                         . filter (uncurry (<)) -- Remove non-gaps
                         . map (first succID)
                         . (zip`ap`tail)
                         $ ksIDs
    where
      ks = used pg
      ksIDs = M.keys ks
      minUsed = head ksIDs
      maxUsed = fst $ M.findMax ks
      -- See if the set contains [0..max] already.
      -- Assumes we don't go too large...
      compactMap = fromIntegral (fromID maxUsed) == pred (M.size ks)

-- | @mergeGraphs pg1 pg2@ creates a disjoint union between @pg1@ and
--   @pg2@ (i.e. puts them into the same graph but disconnected).
--   This is used when they were created independently and thus
--   probably have clashing @Node@ and @Edge@ values.  For best
--   performance, @pg1@ should be larger than @pg2@.
--
--   Along with the merged graph, two functions are returned: they
--   respectively convert Node and Edge values from @pg2@ to those
--   found in the merged graph.
--
--   Please note that these functions are /partial/ and should only be
--   used for the Node and Edge identifiers from @pg2@.
mergeGraphs :: PlanarGraph n e -> PlanarGraph n e -> (PlanarGraph n e, Node -> Node, Edge -> Edge)
mergeGraphs pg1@(PG ns1 es1) pg2
  = (PG ns es, transN, transE)
  where
    transN = transNodeID pg1 pg2
    transE = transEdgeID pg1 pg2

    PG { _nodes = ns2', _edges = es2' }
      = mapNodeIDsMonotonic transN . mapEdgeIDsMonotonic transE $ pg2

    ns = ns1 `M.union` ns2'
    es = es1 `M.union` es2'


{-

 These three functions used for merging purposes only.

 For efficiency reasons, use a direct "numeric" shift of IDs when
 merging two graphs as the translation function becomes O(1).

-}

transNodeID :: PlanarGraph n e -> PlanarGraph n e -> (Node -> Node)
transNodeID = transID _nodes Node node

{-# INLINE transNodeID #-}

transEdgeID :: PlanarGraph n e -> PlanarGraph n e -> (Edge -> Edge)
transEdgeID = transID _edges Edge edge

{-# INLINE transEdgeID #-}

transID :: (PlanarGraph n e -> Map k a) -> (Word -> k) -> (k -> Word)
           -> PlanarGraph n e -> PlanarGraph n e -> (k -> k)
transID used toID fromID pg1 pg2
    | M.null ks1  = id
    | M.null ks2  = id
    | min2 > max1 = id -- Already distinct pairings
    | otherwise   = \ id2 -> toID $ fromID id2 - min2 + max1 + 1
    where
      ks1 = used pg1
      ks2 = used pg2

      max1 = fromID . fst $ M.findMax ks1
      min2 = fromID . fst $ M.findMin ks2

-- | Merge all the provided planar graphs together into one large
--   graph, and provide translation functions for every graph in the
--   list (the first pair in this list is just @('id','id')@).
--
--   See 'mergeGraphs' for more information.  For best performance,
--   the graphs should be decreasing in size/order.
mergeAllGraphs          :: [PlanarGraph n e]
                           -> (PlanarGraph n e, [(Node -> Node, Edge -> Edge)])
mergeAllGraphs []       = (empty, [])
mergeAllGraphs (pg:pgs) = second ((id,id):) $ mapAccumL mrg pg pgs
  where
    mrg = (shift .) . mergeGraphs
    shift (a,b,c) = (a,(b,c))

-- -----------------------------------------------------------------------------
-- Construction

-- | Constructs an empty planar graph.
empty :: PlanarGraph n e
empty = PG { _nodes = M.empty
           , _edges = M.empty
           }

-- | Add a node with the provided label to the graph, returning the
--   updated graph and the node identifier.
addNode      :: n -> PlanarGraph n e -> (Node, PlanarGraph n e)
addNode n pg = (n', withNodes (M.insert n' ni) pg)
  where
    n' = newNodeID pg

    ni = NInfo { outgoing = CL.empty
               , nodeInfo = n
               }

-- | As with 'addNode', but uses @'mempty'@ as the label.
addUNode :: (Monoid n) => PlanarGraph n e -> (Node, PlanarGraph n e)
addUNode = addNode mempty

-- | Specification of where to place a new edge on a node in clockwise order.
data EdgePos = Anywhere         -- ^ The new edge can be placed anywhere.
             | BeforeEdge !Edge -- ^ The new edge should be placed before the specified edge.
             | AfterEdge  !Edge -- ^ The new edge should be placed after the specified edge.
             deriving (Eq, Ord, Show, Read)

{- | Add an edge between two nodes @f@ and @t@.  In reality, since all
     edges are duplicated (see 'inverseEdge'), two half-edges are
     inserted, and the identifiers of both are returned.

     For functions such as 'edges', the first added half-edge is
     assumed to be the /primary/ one.

     If either node does not currently have any edges, then its
     corresponding 'EdgePos' value is ignored.  An 'EdgePos' of 'Anywhere'
     will place the edge before (i.e. anti-clockwise) of the last edge
     added to that node.

     For example, let @g@ refer to the following graph (where
     @n1@, etc. are both the labels and the variable names):

     >     ====                    ====
     >    ( n1 )                  ( n2 )
     >     ====                    ====
     >
     >
     >
     >
     >
     >                             ====
     >                            ( n3 )
     >                             ====

     We can add an edge between @n1@ and @n2@ (using 'Anywhere' as the
     'EdgePos' since there are currently no edges on either node):

     > ((e1,e2),g') = addEdge n1 Anywhere n2 Anywhere "e1" "e2" g

     This will result in the following graph:

     >                  e2
     >     ====  <---------------  ====
     >    ( n1 )                  ( n2 )
     >     ====  --------------->  ====
     >                  e1
     >
     >
     >
     >
     >                             ====
     >                            ( n3 )
     >                             ====

     If we want to add edges between @n2@ and @n3@, we have three
     options for the location on @n2@:

     * Use @'Anywhere'@: since there is only one other edge, it makes no
       difference in terms of the embedding where the second edge goes.

     * Put the new edge @'BeforeEdge' e2@ (going clockwise around @n2@).

     * Put the new edge @'AfterEdge' e2@ (going clockwise around @n2@).

     Since @n2@ currently only has one edge, all three 'EdgePos' values
     will result in the same graph, so we can arbitrarily pick one:

     > ((e3,e4),g'') = addEdge n2 (BeforeEdge e2) n3 Anywhere "e3" "e4" g'

     However, with more edges care must be taken on which 'EdgePos'
     value is used.  The resulting graph is:

     >                  e2
     >     ====  <---------------  ====
     >    ( n1 )                  ( n2 )
     >     ====  --------------->  ====
     >                  e1         |  ^
     >                             |  |
     >                          e3 |  | e4
     >                             |  |
     >                             v  |
     >                             ====
     >                            ( n3 )
     >                             ====

     The same graph (up to the actual 'Edge' values; so it won't satisfy
     @==@) would have been obtained with:

     > ((e4,e3), g'') = addEdge n3 Anywhere n2 (BeforeEdge e2) "e4" "e3" g'

     (Note, however, that now 'edges' will return @e4@ rather than
     @e3@ as it is considered to be the primary edge.)

 -}
addEdge :: Node -- ^ The node @f@ at which the main edge starts.
           -> EdgePos         -- ^ Positioning information at @f@.
           -> Node            -- ^ The node @t@ at which the main edge ends.
           -> EdgePos         -- ^ Positioning information at @t@ for
                              --   the inverse edge (i.e. refers to
                              --   @'outgoingEdges' t@).
           -> e               -- ^ The label for the main edge @f -> t@.
           -> e               -- ^ The label for the inverse edge @t -> f@.
           -> PlanarGraph n e -- ^ The graph at which to add the edge.
           -> ((Edge, Edge), PlanarGraph n e) -- ^ The main and inverse edge
                                              --   identifiers, and the updated
                                              --   graph.
addEdge f fpos t tpos e1 e2 pg = ((e1',e2'), pg')
  where
    pg' = withEdges updateEdges
          . withNodes (M.insert t tNi . M.insert f fNi)
          $ pg

    e1' = newEdgeID pg
    -- Relying upon the invariant here, so that any gaps will be of even size.
    e2' = succEdge e1'

    updateEdges = fixAround e1' fPrv fNxt . fixAround e2' tPrv tNxt
                  . M.insert e2' ei2 . M.insert e1' ei1

    fixAround e p n = M.adjust (\inf -> inf {_nextEdge = e}) p
                      . M.adjust (\inf -> inf {_prevEdge = e}) n

    (fPrv,fNxt,fNi) = edgePos f fpos e1' pg
    (tPrv,tNxt,tNi) = edgePos t tpos e2' pg

    ei1 = EInfo { _fromNode = f
                , _toNode   = t
                , _prevEdge = fPrv
                , _nextEdge = fNxt
                , inverse   = e2'
                , edgeInfo  = e1
                }

    ei2 = EInfo { _fromNode = t
                , _toNode   = f
                , _prevEdge = tPrv
                , _nextEdge = tNxt
                , inverse   = e1'
                , edgeInfo  = e2
                }

-- Work out where to put the new edge at the specified starting node.
edgePos :: Node -> EdgePos -> Edge -> PlanarGraph n e -> (Edge, Edge, NodeInfo n)
edgePos f pos e pg
  | CL.isEmpty es = (e, e, ni { outgoing = singleton e })
  | otherwise     = (prv, nxt, ni { outgoing = insertR e esR })
  where
    ni = getNodeInfo pg f
    es = outgoing ni
    esR = case pos of
            Anywhere        -> es
            (BeforeEdge e') -> rot e' es
            (AfterEdge  e') -> rotR $ rot e' es
    prv = prevElem esR
    -- No need to use nextElem as we haven't added the new edge yet
    -- Safe to use fromJust here: esR won't be empty or else an error
    -- would have already been thrown.
    nxt = fromJust $ focus esR

    rot e' = fromMaybe (error $ "When adding an edge, " ++ show f
                                ++ " does not have " ++ show e' ++ " as an outgoing edge")
             . rotateTo e'

-- | As with 'addEdge', but the edges are meant to be undirected so
--   use the same label for both.
addEdgeUndirected :: Node -> EdgePos -> Node -> EdgePos -> e -> PlanarGraph n e
                     -> (Edge, PlanarGraph n e)
addEdgeUndirected f fP t tP e = first fst . addEdge f fP t tP e e

-- | As with 'addEdge', but both labels are set to @'mempty'@.
addUEdge :: (Monoid e) => Node -> EdgePos -> Node -> EdgePos -> PlanarGraph n e
            -> ((Edge, Edge), PlanarGraph n e)
addUEdge f fP t tP = addEdge f fP t tP mempty mempty

-- -----------------------------------------------------------------------------
-- Deconstruction

-- | Determines if the graph is empty.
isEmpty :: PlanarGraph n e -> Bool
isEmpty = M.null . _nodes

-- | Delete the node and all adjacent edges from the graph.
deleteNode      :: Node -> PlanarGraph n e -> PlanarGraph n e
deleteNode n pg = withNodes (delens' . M.delete n)
                  . withEdges (deles' . deles)
                  $ pg
  where
    es = toList $ outgoingEdges pg n
    deles em = foldr M.delete em es

    es' = map (inverseEdge pg) es
    eis' = map (getEdgeInfo pg) es'
    deles' em = foldr ($) em $ zipWith delE es' eis'

    delens' nm = foldr ($) nm $ zipWith delEN es' eis'

-- | Delete the edge and its inverse from the graph.
deleteEdge      :: Edge -> PlanarGraph n e -> PlanarGraph n e
deleteEdge e pg = withEdges (delE eInv eiInv . delE e ei)
                  . withNodes (delEN eInv eiInv . delEN e ei)
                  $ pg
  where
    ei = getEdgeInfo pg e
    eInv = inverse ei
    eiInv = getEdgeInfo pg eInv


-- Deletes the provided edge from the outgoing list of the node it
-- comes from in the NodeMap.
delEN      :: Edge -> EdgeInfo e -> NodeMap n -> NodeMap n
delEN e ei = M.adjust (\inf -> inf { outgoing = removeR . fromJust
                                                . rotateTo e $ outgoing inf })
                      (_fromNode ei)

-- Deletes the provided edge from the EdgeMap, and fixes up references
-- in adjacent edges.
delE      :: Edge -> EdgeInfo e -> EdgeMap e -> EdgeMap e
delE e ei = M.adjust (\inf -> inf { _prevEdge = _prevEdge ei }) (_nextEdge ei)
            . M.adjust (\inf -> inf { _nextEdge = _nextEdge ei }) (_prevEdge ei)
            . M.delete e

-- | Merges the two nodes adjoined by this edge, and delete all edges
--   between them.  The provided function is to decide what the label
--   for the resulting node should be (if the edge goes from @f@ to
--   @t@, then the function is @fLabel -> tLabel -> newLabel@).  The
--   'Node' value for the merged node is @'fromNode' pg e@.
--
--   Note that this may result in multiple edges between the new node
--   and another node if it is adjacent to both nodes being merged.
contractEdge :: Edge -> (n -> n -> n) -> PlanarGraph n e -> PlanarGraph n e
contractEdge e newL pg = withNodes adjNs . withEdges adjEs $ pg
  where
    ei = getEdgeInfo pg e
    f = _fromNode ei
    t = _toNode ei

    eInv = inverse ei
    -- eInvI = getEdgeInfo pg eInv

    fi = getNodeInfo pg f
    (fout,fDel) = validEs t e $ outgoing fi
    ti = getNodeInfo pg t
    (tout,tDel) = validEs f eInv $ outgoing ti

    es = fromList es'
    es' = fout ++ tout
    esL = toList $ rotL es
    esR = toList $ rotR es

    -- fromJust should be safe here, as it's using values obtained
    -- internally.
    validEs n' e' = partition ((/=n') . _toNode . getEdgeInfo pg)
                    . toList . fromJust . rotateTo e'

    fi' = NInfo { outgoing = es
                , nodeInfo = newL (nodeInfo fi) (nodeInfo ti)
                }
    adjNs = M.insert f fi' . M.delete t

    adjEs = fixRefs . flip (foldr M.delete) (fDel ++ tDel)
    fixRefs pg' = foldr fixRef pg' $ zip3 esL es' esR
    fixRef (prv,thisE,nxt) = M.adjust (\ei' -> ei' { _fromNode = f
                                                   , _prevEdge = prv
                                                   , _nextEdge = nxt
                                                   }
                                      ) thisE
                             . M.adjust (\ei' -> ei' { _toNode = f })
                                        (inverseEdge pg thisE)


-- -----------------------------------------------------------------------------

type NodeMap n = Map Node (NodeInfo n)

-- | An abstract representation of a node.
newtype Node = Node { node :: Word }
               deriving (Eq, Ord, NFData)

succNode :: Node -> Node
succNode = Node . succ . node

enumNode :: Node -> [Node]
enumNode = map Node . enumFrom . node

-- | This instance of 'Show' does not produce valid Haskell code;
--   however, the 'Node' type is abstract and not designed to be
--   directly accessed.
instance Show Node where
  showsPrec = showsFrom node "Node"

-- | Note that this instance of 'Read' only works when directly
--   applied to a 'String'; it is supplied solely to assist with
--   debugging.
instance Read Node where
  readPrec = readsFrom Node "Node"

initNode :: Node
initNode = Node minBound

data NodeInfo n = NInfo { outgoing :: !(CList Edge)
                        , nodeInfo :: !n
                        }
                  deriving (Eq, Show, Read)

instance (NFData n) => NFData (NodeInfo n) where
  rnf (NInfo out inf) = rnf out `seq` rnf inf

-- Assumes the node is part of the graph
withNode        :: (NodeInfo n -> a) -> PlanarGraph n e -> Node -> a
withNode f pg n = maybe err f . M.lookup n $ _nodes pg
  where
    err = error $ "The node " ++ show n ++ " is not part of the specified graph."

{-# INLINE withNode #-}

getNodeInfo :: PlanarGraph n e -> Node -> NodeInfo n
getNodeInfo = withNode id

{-# INLINE getNodeInfo #-}

-- | Returns all outgoing edges for the specified node, travelling
--   clockwise around the node.  It assumes the node is indeed in the
--   graph.
outgoingEdges :: PlanarGraph n e -> Node -> CList Edge
outgoingEdges = withNode outgoing

-- | Returns all incoming edges for the specified node, travelling
--   clockwise around the node.  It assumes the node is indeed in the
--   graph.
incomingEdges    :: PlanarGraph n e -> Node -> CList Edge
incomingEdges pg = fmap (inverseEdge pg) . outgoingEdges pg

-- | Returns the label for the specified node.
nodeLabel :: PlanarGraph n e -> Node -> n
nodeLabel = withNode nodeInfo

-- | Apply a function to the label of the specified node.
adjustNodeLabel   :: (n -> n) -> Node -> PlanarGraph n e -> PlanarGraph n e
adjustNodeLabel f = withNodes . M.adjust (\ni -> ni { nodeInfo = f $ nodeInfo ni })

-- | Set the label of the specified node.
setNodeLabel :: n -> Node -> PlanarGraph n e -> PlanarGraph n e
setNodeLabel = adjustNodeLabel . const

-- | The 'Node's that are connected to this 'Node' with an edge (in
--   clockwise order).
neighbours    :: PlanarGraph n e -> Node -> CList Node
neighbours pg = withNode (fmap (toNode pg) . outgoing) pg

-- -----------------------------------------------------------------------------

{- $edges

   To be able to embed the required order of edges around a particular
   'Node', we can't rely on just having each node specify which other
   nodes are adjacent to it as with non-planar graph types; instead,
   we need a unique identifier (to be able to distinguish between
   multiple edges between two nodes).  Furthermore, each edge has an
   /inverse edge/ in the opposite direction.  To be more precise,
   these can be referred to as /half-edges/.

   Due to every edge having an inverse, a 'PlanarGraph' implicitly
   /undirected/ even though each edge is directed.  As such, if you
   require a directed planar graph, use appropriate edge labels to
   denote whether an edge is the one you want or just its inverse.

   Note the distinction between functions such as 'edges' and
   'halfEdges': the latter returns every single half-edge (i.e the
   inverse \"edge\" is also included) whereas the former only
   considers the /primary/ edge.  The distinction is made when adding
   edges to the graph: the first edge added in 'addEdge' is considered
   the primary one.

   To be more specific:

   > length . edges == size
   > length . halfEdges == 2 * size

 -}

type EdgeMap e = Map Edge (EdgeInfo e)

-- | An abstract representation of an edge.  Note that an explicit
--   identifier is used for each edge rather than just using the two
--   nodes that the edge connects.  This is required in case more than
--   one edge connects two nodes as we need to be able to distinguish
--   them.
newtype Edge = Edge { edge :: Word }
               deriving (Eq, Ord, NFData)

succEdge :: Edge -> Edge
succEdge = Edge . succ . edge

enumEdge :: Edge -> [Edge]
enumEdge = map Edge . enumFrom . edge

-- | This instance of 'Show' does not produce valid Haskell code;
--   however, the 'Edge' type is abstract and not designed to be
--   directly accessed.
instance Show Edge where
  showsPrec = showsFrom edge "Edge"

-- | Note that this instance of 'Read' only works when directly
--   applied to a 'String'; it is supplied solely to assist with
--   debugging.
instance Read Edge where
  readPrec = readsFrom Edge "Edge"

initEdge :: Edge
initEdge = Edge minBound

data EdgeInfo e = EInfo { -- | The 'Node' which this 'Edge' is coming from.
                          _fromNode :: !Node
                          -- | The 'Node' this 'Edge' is going to.
                        , _toNode   :: !Node

                          -- | The previous 'Edge' going clockwise around the '_fromNode'.
                        , _prevEdge :: !Edge
                          -- | The next 'Edge' going clockwise around the '_fromNode'.
                        , _nextEdge :: !Edge

                          -- | The 'Edge' that is an inverse to this one; i.e.:
                          --
                          --   > _fromNode ei == _toNode $ inverse ei
                          --   > _toNode ei == _fromNode $ inverse ei
                        , inverse   :: !Edge

                          -- | The stored information for this 'Edge'.
                        , edgeInfo  :: !e
                        }
                deriving (Eq, Ord, Show, Read)

instance (NFData e) => NFData (EdgeInfo e) where
  rnf (EInfo f t p n i l) = rnf f `seq` rnf t
                            `seq` rnf p `seq` rnf n
                            `seq` rnf i `seq` rnf l

-- Assumes the edge is part of the graph
withEdge           :: String -> (EdgeInfo e -> a) -> PlanarGraph n e -> Edge -> a
withEdge nm f pg e = maybe err f . M.lookup e $ _edges pg
  where
    err = error $ nm ++ ": the edge " ++ show e
                  ++ " is not part of the specified graph."

{-# INLINE withEdge #-}

getEdgeInfo :: PlanarGraph n e -> Edge -> EdgeInfo e
getEdgeInfo = withEdge "getEdgeInfo" id

{-# INLINE getEdgeInfo #-}

-- | The 'Node' which this 'Edge' is coming from.
fromNode :: PlanarGraph n e -> Edge -> Node
fromNode = withEdge "fromNode" _fromNode

-- | The 'Node' which this 'Edge' is going to.
toNode :: PlanarGraph n e -> Edge -> Node
toNode = withEdge "toNode" _toNode

-- | The previous 'Edge' going clockwise around the 'fromNode'.
prevEdge :: PlanarGraph n e -> Edge -> Edge
prevEdge = withEdge "prevEdge" _prevEdge

-- | The next 'Edge' going clockwise around the 'fromNode'.
nextEdge :: PlanarGraph n e -> Edge -> Edge
nextEdge = withEdge "nextEdge" _nextEdge

-- | The 'Edge' that is an inverse to this one; i.e.:
--
--   > fromNode pg e == toNode pg $ inverseEdge pg e
--   > toNode pg e == fromNode pg $ inverseEdge pg e
inverseEdge :: PlanarGraph n e -> Edge -> Edge
inverseEdge = withEdge "inverseEdge" inverse

-- | Return the label for the specified edge.
edgeLabel :: PlanarGraph n e -> Edge -> e
edgeLabel = withEdge "edgeLabel" edgeInfo

-- | Apply a function to the label of the specified edge.
adjustEdgeLabel   :: (e -> e) -> Edge -> PlanarGraph n e -> PlanarGraph n e
adjustEdgeLabel f = withEdges . M.adjust (\ei -> ei { edgeInfo = f $ edgeInfo ei })

-- | Set the label of the specified edge.
setEdgeLabel :: e -> Edge -> PlanarGraph n e -> PlanarGraph n e
setEdgeLabel = adjustEdgeLabel . const

-- -----------------------------------------------------------------------------

{- $duals

   The /dual/ of a planar graph /G/ is another planar graph /H/ such
   that /H/ has an node for every face in /G/, and an edge between two
   nodes if the corresponding faces in /G/ are adjacent.  For example,
   the graph (drawn as an undirected graph for simplicity):

   >                o---------o---------o
   >                |         |         |
   >                |   f1    |   f2    |
   >                |         |         |
   >                o---------o---------o
   >                 \                 /
   >                  \               /
   >                   \     f3      /
   >                    \           /
   >        outer        \         /
   >         face         \       /
   >                       \     /
   >                        \   /
   >                         \ /
   >                          o

   has a dual graph of:

   >                 ......
   >            .....      .....
   >         ...                ..
   >       ..      ......        ..
   >      .       .      .         .
   >     .       .     =====     ===== .....
   >     .      .   ..( f1  )...( f2  )    ....
   >     .     .   ..  =====     =====         ..
   >     .    .   .       .      .               .
   >     .   .   .          .   .                 .
   >     .  =====           =====                  .
   >     . /     \.........( f3  )...               .
   >      /       \         =====   ....             .
   >      | outer |                     .            .
   >      \  face /                      .           .
   >       \     / .                      .          .
   >        =====   .                     .          .
   >           .      .                  .           .
   >            .       .               .           .
   >              .       .............            .
   >                .                             .
   >                  ..                         .
   >                     .                      .
   >                       .               ....
   >                        ................

   A dual graph is a planar /multigraph/: it will still be a planar
   graph, but may have loops and multiple edges.  However, the dual of a
   dual graph will be the original graph (though no guarantees are made
   that @g == makeDual (makeDual g)@ due to differing 'Node' and 'Edge'
   values).

   Note that the functions here assume that the graph is /connected/;
   in effect multiple connected components will be treated individually
   with no notion of relative embeddings.
-}

-- | Information about the faces in a planar graph.
type FaceMap = Map Face FaceInfo

type EdgeFaceMap = Map Edge Face

-- | An abstract representation of a face.
newtype Face = Face { face :: Word }
               deriving (Eq, Ord, NFData)

enumFace :: Face -> [Face]
enumFace = map Face . enumFrom . face

-- | This instance of 'Show' does not produce valid Haskell code;
--   however, the 'Face' type is abstract and not designed to be
--   directly accessed.
instance Show Face where
  showsPrec = showsFrom face "Face"

-- | Note that this instance of 'Read' only works when directly
--   applied to a 'String'; it is supplied solely to assist with
--   debugging.
instance Read Face where
  readPrec = readsFrom Face "Face"

initFace :: Face
initFace = Face minBound

-- | Information about a particular 'Face'.
data FaceInfo = FInfo { -- | The 'Node's that make up the face.
                        faceNodes     :: !(CList Node)

                        -- | The 'Edge's that make up the face, its
                        --   inverse and the 'Face' on the other side
                        --   of that 'Edge'.
                      , edgeCrossings :: !(CList ((Edge,Edge), Face))
                      }
              deriving (Eq, Show, Read)

instance NFData FaceInfo where
  rnf (FInfo ns ecs) = rnf ns `seq` rnf ecs

-- | The 'Edge's that make up the face.
faceEdges :: FaceInfo -> CList Edge
faceEdges = fmap (fst . fst) . edgeCrossings

-- | The adjoining 'Face's.  Will have repeats if the 'Face's are
--   adjacent over more than one 'Edge'.
adjoiningFaces :: FaceInfo -> CList Face
adjoiningFaces = fmap snd . edgeCrossings

-- | Create the dual of a planar graph.  If actual node and edge
--   labels are required, use 'toDual'.
makeDual :: PlanarGraph n e -> PlanarGraph () ()
makeDual = snd . toDual (const ()) (const . const . const ()) . getFaces

-- | Create the planar graph corresponding to the dual of the face
--   relationships.  The usage of 'FaceMap' rather than 'PlanarGraph'
--   is to allow you to use the 'FaceMap' for constructing the
--   label-creation functions if you so wish.
--
--   The function @eLabel@ for edge labels takes the 'Face' that the
--   edge comes from, the 'Edge' belonging to that 'Face' that it is
--   crossing and then the 'Face' that it is going to.  For example:
--
--   >                  ....              ....>
--   >                      ...> =====....
--   >                          (#####)
--   >                           =====
--   >                            | ^  e2
--   >                            | |
--   >                            | |
--   >              face1         | |      face2
--   >                            | |
--   >                            | |
--   >                            | |
--   >                        e1  v |
--   >                           =====
--   >                          (#####)
--   >                        ...===== <..
--   >                    <...            ....
--   >                                        ...
--
--   Here, the edge in the dual graph going from /face1/ to /face2/
--   will have a label of \"@eLabel face1 e1 face2@\", and the edge
--   going from /face2/ to /face1/ will have a label of \"@eLabel
--   face2 e2 face1@\".
--
--   The returned functions are a mapping from the faces in the
--   'FaceMap' to the nodes in the dual graph, and the edges in the
--   original graph to the edge in the dual that crosses it (e.g. in
--   the above diagram, /e1/ will have a mapping to the edge from
--   /face1/ to /face2/).
toDual           :: (Face -> n) -> (Face -> Edge -> Face -> e)
                    -> FaceMap -> ((Face -> Node,Edge -> Edge), PlanarGraph n e)
toDual nLab eLab fm = ((f2n, e2e), dl)
  where
    -- Need a more rigorous definition of this; currently relies on
    -- behaviour of deserialise :s
    f2n = Node . face
    e2e = id
    dl = deserialise . map serialiseFace . M.assocs $ fm
    serialiseFace (f,fi) = (face f, nLab f, es)
      where
        es = map (mkFace f) . toList $ edgeCrossings fi

    mkFace f ((e,ei),f') = (edge e, face f', eLab f e f', edge ei)

-- | Finds all faces in the planar graph.  A face is defined by
--    traversing along the right-hand-side of edges, e.g.:
--
--   >
--   >           o----------------------------->o
--   >           ^..............................|
--   >           |..............................|
--   >           |..............FACE............|
--   >           |..............................|
--   >           |..............................v
--   >           o<-----------------------------o
--   >
--
--   (with the inverse edges all being on the outside of the edges
--   shown).
getFaces    :: PlanarGraph n e -> FaceMap
getFaces pg = M.fromList fis
  where
    efm = M.fromList
          . concatMap (\(fid, finfs) -> map (flip (,) fid) finfs)
          $ map (second (toList . faceEdges)) fis
    fis = zip (enumFace initFace)
          . unfoldr (getNextFace pg efm) . M.keysSet $ _edges pg

-- | Finds a new face in the provided graph, where the Set contains
--   all edges which have not yet been matched up to a face.
getNextFace :: PlanarGraph n e -> EdgeFaceMap -> Set Edge -> Maybe (FaceInfo, Set Edge)
getNextFace pg efm unmatchedEs
  | S.null unmatchedEs = Nothing
  | otherwise          = Just (f, unmatchedEs')
    where
      e = S.findMin unmatchedEs
      (ns, es) = getFace pg e
      toCrossing e' = let ei' = inverseEdge pg e'
                      in ((e',ei'), efm ! ei')
      unmatchedEs' = unmatchedEs `S.difference` S.fromList es
      f = FInfo { faceNodes     = fromList ns
                , edgeCrossings = fromList $ map toCrossing es
                }

-- | Returns all nodes and edges in the same face as the provided edge
--   (including that edge); assumes the edge is part of the graph.
getFace      :: PlanarGraph n e -> Edge -> ([Node], [Edge])
getFace pg e = unzip $ (fromNode pg e, e) : unfoldr go e
  where
    go e'
      | e == e''  = Nothing
      | otherwise = Just (nxt, e'')
        where
          nxt = nextInFace pg e'
          e'' = snd nxt

-- | Returns the next node and edge in the same face as the provided
--   edge.  Assumes the provided edge is indeed in this graph.
nextInFace      :: PlanarGraph n e -> Edge -> (Node, Edge)
nextInFace pg e = (n', e')
  where
    eI = _edges pg ! e
    eInv = inverse eI
    n' = _toNode eI
    e' = _prevEdge $ _edges pg ! eInv

-- -----------------------------------------------------------------------------
-- Serialisation and pretty-printing

{- $serialisation

   Serialisation support can be found here to aid in converting a
   'PlanarGraph' to alternate formats.  Care should be taken when
   constructing the 'SerialisedGraph', and these functions should not be
   abused just to edit an existing 'PlanarGraph'.
-}

-- | The definition of a more compact, serialised form of a planar
--   graph.  The various fields correspond to:
--
--   > [( node index
--   >  , node label
--   >  , [( edge index
--   >     , node index that this edge points to
--   >     , edge label
--   >     , inverse edge index
--   >    )]
--   > )]
--   >
--
--   The list of edges should be in clockwise order around the node.
--
--   Note that there will be twice as many edges lists as the /size/;
--   that's because each edge is listed twice.
type SerialisedGraph n e = [(Word, n, [(Word, Word, e, Word)])]

-- | Create the serialised form of this graph.
serialise    :: PlanarGraph n e -> SerialisedGraph n e
serialise pg = map serialiseN . M.assocs $ _nodes pg
  where
    serialiseN (n,ni) = (node n, nodeInfo ni, getEs ni)

    es = _edges pg
    getEs = map serialiseE . toList . outgoing
    serialiseE e = (edge e, node $ _toNode ei, edgeInfo ei, edge $ inverse ei)
      where
        ei = es ! e

-- | An alias for 'serialiseBFS' with no specified edge.  Also added
--   are the 'order' and 'size' of the graph.
--
--   This function is mainly intended for use by the
--   "Data.Graph.Planar.Serialisation" module.
serialTraversal    :: PlanarGraph n e -> ((Int,Int),SerialisedGraph n e)
serialTraversal pg = (ordSz, serialiseBFS pg Nothing)
  where
    ordSz = (order pg, size pg)

-- | Perform a breadth-first traversal serialisation of the provided
--   graph.  If an edge is provided, then it is the first edge and its
--   'fromNode' is the first node; if no edge is provided then an
--   arbitrary edge is chosen.
--
--   Up to the choice of starting edge, the returned 'SerialisedGraph'
--   should be unique no matter how the graph was constructed.
--
--   Note that only one connected component is used: this is because
--   if there is more than one component then the serialisation is
--   /not/ unique (due to how to choose the ordering of the
--   components).
serialiseBFS :: PlanarGraph n e -> Maybe Edge -> SerialisedGraph n e
serialiseBFS pg me
  | M.null es = []
  | otherwise = unfoldr travNodes ts
  where
    es = _edges pg

    se = fromMaybe (fst $ M.findMin es) me

    ts = initTS (fromNode pg se) se

    travNodes = fmap (uncurry $ traverseNode pg) . nextNode

data TravState = TSt { visitedNodes :: Map Node Word
                     , toVisit      :: Seq (Node,Edge)
                     , visitedEdges :: Map Edge (Word,Word) -- Include inverse
                     , currentNode  :: !Word
                     , nextNodeRep  :: !Word
                     , nextEdgeRep  :: !Word
                     }
               deriving (Eq, Ord, Show, Read)

initTS     :: Node -> Edge -> TravState
initTS n e = TSt { visitedNodes = M.singleton n 0
                 , toVisit      = Seq.singleton (n,e)
                 , visitedEdges = M.empty
                 , currentNode  = 0
                 , nextNodeRep  = 1
                 , nextEdgeRep  = 0
                 }

-- Edge, then its inverse.
traverseEdge           :: TravState -> (Edge, EdgeInfo e) -> (TravState,(Word,Word,e,Word))
traverseEdge ts (e,ei) = (ts'', eTrav)
  where
    eInv = inverse ei
    n = _toNode ei

    ((eRep,eRepInv),ts') = getEdgeReplacement e eInv ts
    (nRep,ts'') = getNodeReplacement n eInv ts'

    eTrav = (eRep, nRep, edgeInfo ei, eRepInv)

-- Edge then its inverse
getEdgeReplacement :: Edge -> Edge -> TravState -> ((Word,Word), TravState)
getEdgeReplacement e ei ts = case e `M.lookup` es of
                               Just eids -> (eids, ts)   -- TODO: try deleting e here, since it will never need to be looked up again.
                               Nothing   -> ((eRep,eRepInv), ts')
  where
    es = visitedEdges ts

    eRep = nextEdgeRep ts
    eRepInv = succ eRep

    -- Note: we insert /ei/ into the Map, because that's what'll be looked up!
    ts' = ts { visitedEdges = M.insert ei (eRepInv,eRep) es
             , nextEdgeRep  = succ eRepInv
             }

-- Node and outgoing edge from it
getNodeReplacement           :: Node -> Edge -> TravState -> (Word, TravState)
getNodeReplacement n eOut ts = case n `M.lookup` ns of
                                 Just nid -> (nid, ts)
                                 Nothing  -> (nRep, ts')
  where
    ns = visitedNodes ts

    nRep = nextNodeRep ts

    ts' = ts { visitedNodes = M.insert n nRep ns
             , toVisit      = toVisit ts |> (n,eOut)
             , nextNodeRep  = succ nRep
             }

nextNode    :: TravState -> Maybe ((Node,Edge,Word),TravState)
nextNode ts = case viewl $ toVisit ts of
                EmptyL        -> Nothing
                (n,e) :< vis' -> let nid = visitedNodes ts M.! n
                                 in Just ((n,e,nid), ts { toVisit = vis', currentNode = nid })

traverseNode :: PlanarGraph n e -> (Node,Edge,Word) -> TravState -> ((Word,n,[(Word,Word,e,Word)]),TravState)
traverseNode pg (n,e,nid) ts = ((nid,nodeInfo ni,eReps),ts')
  where
    ni = getNodeInfo pg n

    es = map (ap (,) (getEdgeInfo pg))
         . toList . fromJust . rotateTo e $ outgoing ni

    (ts',eReps) = mapAccumL traverseEdge ts es

-- | Creates the graph from its serialised form.  Assumes that the
--   graph is valid.
deserialise :: SerialisedGraph n e -> PlanarGraph n e
deserialise = uncurry PG . (M.fromList *** M.unions)
              . unzip . map deserialiseNode

deserialiseNode          :: (Word, n, [(Word, Word, e, Word)]) -> ((Node, NodeInfo n), EdgeMap e)
deserialiseNode (n,l,es) = (nd, M.fromList $ map mkE es)
  where
    nd = (n', NInfo { outgoing = os, nodeInfo = l })
    n' = Node n
    -- Outgoing edges
    os = fromList $ map (\(e,_,_,_) -> Edge e) es

    mkE (e,t,el,i) = (e', EInfo { _fromNode = n'
                                , _toNode   = Node t
                                , _prevEdge = prevElem os'
                                , _nextEdge = nextElem os'
                                , inverse   = Edge i
                                , edgeInfo  = el
                                })
      where
        os' = fromJust $ rotateTo e' os
        e' = Edge e

-- | Pretty-print the graph.  Note that this loses a lot of
--   information, such as edge inverses, etc.
prettify :: (Show n, Show e) => PlanarGraph n e -> String
prettify = unlines . map (printN . third (map eParts)) . serialise
  where
    printN (n,l,as) = show n ++ ":" ++ show l
                      ++ "->" ++ show as

    eParts (_,t,l,_) = (l,t)

    third f (a,b,c) = (a, b, f c)

-- | Pretty-print the graph to stdout.
prettyPrint :: (Show n, Show e) => PlanarGraph n e -> IO ()
prettyPrint = putStr . prettify

-- -----------------------------------------------------------------------------

{- | Different ways of traversing through a graph.

To assist in visualising how the traversals differ, sample traversals
will be provided for the following graph:

>                                =====
>                               (  1  )
>                                =====
>                                  |
>                                a |
>                                  |
>                                =====
>                               (  2  )
>                                =====
>                                / | \
>                        b      /  |  \      c
>                 /-------------   |   -------------\
>                /                 |                 \
>             =====              d |                =====
>            (  3  )               |               (  5  )
>             =====              =====              =====
>               |               (  4  )             /   \
>               |                =====             /     \
>               |                  |              /       \
>             e |                f |           g /         \ h
>               |                  |            /           \
>               |                  |           |             |
>               |                 /            |             |
>               |                /             |             |
>             =====             /           =====           =====
>            (  6  )-----------/           (  7  )         (  8  )
>             =====                         =====           =====
>

Each traversal shall start at the edge labelled /a/: note that
whenever an edge is traversed, it immediately also traverses its
inverse.

In particular, note where the node labelled /4/ and its two adjacent
edges are found.

 -}
data Traversal = Trv { -- First Seq is existing one, second is new values
                       addValues  :: Seq [Edge] -> [Edge] -> Seq [Edge]
                     , mkEdgeList :: CList Edge -> [Edge]
                     -- | Should an edge be included even if the node
                     --   it points to has already been visited?
                     , allEdges   :: !Bool
                     }

defTraversal :: Traversal
defTraversal = Trv { addValues  = undefined
                   , mkEdgeList = rightElements
                   , allEdges   = True
                   }

{- | A breadth-first traversal on the sample graph would visit the
     nodes and edges in the following order:

         [/nodes/:] 1 2 5 4 3 8 7 6

         [/edges/:] a c d b h g f e

     If 'spanningTraversal' was used, then the edge /e/ wouldn't be
     traversed; if 'antiClockwiseTraversal' was also used, then
     instead /f/ wouldn't be traversed.

 -}
breadthFirst :: Traversal
breadthFirst = defTraversal { addValues  = (|>) }

{- | A depth-first traversal on the sample graph would visit the nodes
     and edges in the following order:

         [/nodes/:] 1 2 5 8 7 4 6 3

         [/edges/:] a c h g d f e b

     If 'spanningTraversal' was used, then the edge /b/ wouldn't be
     traversed; if 'antiClockwiseTraversal' was also used then instead
     /d/ wouldn't be traversed.

 -}
depthFirst :: Traversal
depthFirst = defTraversal { addValues  = flip (<|) }

-- | By default, the traversals do so in a clockwise fashion, just as
--   the outgoing edges are defined for each node.  This lets you
--   specify that an anti-clockwise traversal should be done instead.
--
--   This is not computationally any more expensive than clockwise
--   traversals.
antiClockwiseTraversal     :: Traversal -> Traversal
antiClockwiseTraversal trv = trv { mkEdgeList = leftElements }

-- | Perform a traversal suitable for a spanning tree.  In this case,
--   edges that reach a node that has already been visited won't be
--   traversed.
--
--   This /does/ make getting each connected component more expensive.
spanningTraversal :: Traversal -> Traversal
spanningTraversal trv = trv { allEdges = False }

-- | The values found whilst traversing.  See 'GraphTraversal' for
--   more specific information.
data TraversedValues a = TV { -- | All values encountered.
                              visited    :: !(Set a)
                              -- | The order in which values are
                              --   encountered.
                            , traversed  :: !(Seq a)
                              -- | Did we skip any edges?
                            , anyMissing :: !Bool
                            }

mergeTV :: (Ord a) => TraversedValues a -> TraversedValues a -> TraversedValues a
mergeTV (TV v1 t1 am1) (TV v2 t2 am2) = TV (v1 `S.union` v2)
                                           (t1 >< t2)
                                           (am1 || am2)

mergeGT :: GraphTraversal -> GraphTraversal -> GraphTraversal
mergeGT (n1,e1) (n2,e2) = (mergeTV n1 n2, mergeTV e1 e2)

-- | Merge the results from 'traverse' into one traversal (i.e. you
--   don't care about individual components).
mergeGraphTraversals     :: [GraphTraversal] -> GraphTraversal
mergeGraphTraversals []  = (initTV, initTV)
mergeGraphTraversals gts = foldl1' mergeGT gts

-- | Specify part of a graph found by traversing it.  For nodes,
--   @'visited' == 'S.fromList' . 'F.toList' . 'traversed'@; the same
--   is true for edges /except/ when 'spanningTraversal' is used.  In
--   that case, 'traversed' may contain a sub-set of 'visited' (and if
--   they aren't equal, 'anyMissing' will be 'True'.).
type GraphTraversal = (TraversedValues Node, TraversedValues Edge)

initTV :: TraversedValues a
initTV = TV S.empty Seq.empty False

addValue      :: (Ord a) => a -> TraversedValues a -> TraversedValues a
addValue a tv = tv { visited   = S.insert a $ visited tv
                   , traversed = traversed tv |> a
                   }

visitNotTraverse      :: (Ord a) => a -> TraversedValues a -> TraversedValues a
visitNotTraverse a tv = tv { visited    = S.insert a $ visited tv
                           , anyMissing = True
                           }

hasValue   :: (Ord a) => a -> TraversedValues a -> Bool
hasValue a = S.member a . visited

-- | Perform a re-numbering of the identifiers in this graph using the
--   specified traversal and optionally starting from a specified
--   edge.
--
--   If there is only one connected component in the graph and the
--   same edge is specified each time (relative to the location in the
--   graph), then the re-numbering is /canonical/: that is, it can be
--   used to compare whether two graphs constructed via separate paths
--   (and thus using different identifiers) are indeed the same.
renumber           :: Traversal -> PlanarGraph n e -> Maybe Edge
                      -> (PlanarGraph n e, (Node -> Node, Edge -> Edge))
renumber trv pg me = renumberComponent pg gt
  where
    gt = mergeGraphTraversals $ traverse trv pg me

-- | Use a 'breadthFirst' traversal to find all the connected
--   components.  The node and edge identifiers for each component are
--   re-numbered.
connectedComponents    :: PlanarGraph n e
                          -> [(PlanarGraph n e, (Node -> Node, Edge -> Edge))]
connectedComponents pg = map (onlyComponent pg) tcs
  where
    tcs = traverseAll breadthFirst pg

-- Get the graph corresponding just to this component; also renumbers it.
onlyComponent :: PlanarGraph n e -> GraphTraversal
                 -> (PlanarGraph n e, (Node -> Node, Edge -> Edge))
onlyComponent pg tc@(TV{visited=vn},tve) = renumberComponent pg' tc
  where
    pg' = pg { _nodes = _nodes pg `setIntersection` vn
             , _edges = _edges pg `setIntersection` ve
             }

    ve | anyMissing tve = S.fromList . F.toList $ traversed tve
       | otherwise      = visited tve

setIntersection     :: (Ord k) => Map k a -> Set k -> Map k a
setIntersection m s = m `M.intersection` m'
  where
    m' = M.fromAscList . map (flip (,) ()) $ S.toAscList s

-- This requires that all nodes and edges in the graph are listed in
-- the component; if not, delete the others first!
renumberComponent :: PlanarGraph n e -> GraphTraversal
                    -> (PlanarGraph n e,(Node -> Node, Edge -> Edge))
renumberComponent pg (TV{traversed = ns}, TV{traversed = es}) = (pg', (fn,fe))
  where
    nMap = M.fromList . zip (F.toList ns) $ enumNode initNode
    eMap = M.fromList . zip (F.toList es) $ enumEdge initEdge

    fn n = nMap M.! n
    fe e = eMap M.! e

    pg' = mapEdgeIDs fe . mapNodeIDs fn $ pg

-- | Traverse through a graph, and return each connected component
--   found.  If an edge is specified, start with that edge and then
--   for subsequent components (if there are any) arbitrarily pick
--   edges to start with; if no edge is provided than start at an
--   arbitrary edge.
traverse        :: Traversal -> PlanarGraph n e -> Maybe Edge -> [GraphTraversal]
traverse trv pg = maybe (traverseAll trv pg) (traverseFrom trv pg)

-- No edge specified.
traverseAll :: Traversal -> PlanarGraph n e -> [GraphTraversal]
traverseAll trv pg
  | M.null es = traverseNodes . M.keysSet $ _nodes pg
  | otherwise = traverseFrom trv pg . fst $ M.findMin es
  where
    es = _edges pg

traverseFrom :: Traversal -> PlanarGraph n e -> Edge -> [GraphTraversal]
traverseFrom trv pg se = go se sns ses
  where
    sns = M.keysSet $ _nodes pg
    ses = M.keysSet $ _edges pg

    go e ns es = tnes : if S.null es'
                        then traverseNodes ns'
                        else go e' ns' es'
       where
         tnes@(tns,tes) = traverseNextFrom trv pg e
         ns' = ns `S.difference` visited tns
         es' = es `S.difference` visited tes

         e' = S.findMin es'

-- Only singleton nodes left
traverseNodes :: Set Node -> [GraphTraversal]
traverseNodes = map mkComp . S.elems
  where
    mkComp n = (addValue n initTV, initTV)

traverseNextFrom :: Traversal -> PlanarGraph n e -> Edge -> GraphTraversal
traverseNextFrom (Trv add mkList allEs) pg se = go ses snm sem
  where
    addEdges st n e = st `add`
                      (mkList . fromJust . rotateTo e $ outgoingEdges pg n)

    sn = fromNode pg se
    ses = addEdges Seq.empty sn se
    snm = addValue sn initTV
    sem = initTV -- addValue (inverseEdge pg se) $ addValue se initEdgeTV

    go sq nm em
      | Seq.null sq   = (nm,em)
      | null es       = go sq' nm em
      | hasValue e em = go sq'' nm em
      | hasValue n nm = go sq'' nm $ if allEs then em' else vem
      | otherwise     = go sq''' nm' em'
      where
        (es :< sq') = viewl sq
        (e:es') = es
        sq'' = es' <| sq'

        ei = getEdgeInfo pg e

        n = _toNode ei
        e' = inverse ei

        em' = addValue e' $ addValue e em
        nm' = addValue n nm

        sq''' = addEdges sq'' n e'

        vem = visitNotTraverse e' $ visitNotTraverse e em

-- -----------------------------------------------------------------------------

-- | Determine if this graph is the canonical representative of the
--   isomorphic class (defined as such by having a breadth-first
--   serialisation via 'serialiseBFS' that is @<=@ any other such
--   serialisation).
--
--   The function specifies all possible starting edges for the
--   traversal (it is safe to leave the specified edge being returned
--   by this function).  If there are no known unique aspects of this
--   graph that could be used to minimise \"uniqueness\", then use the
--   'halfEdges' function (note: you probably do /not/ want to use
--   'edges' if the graph is undirected).
--
--   Note that this really only makes sense for graphs of type
--   @PlanarGraph () ()@, unless you are sure that the labels won't
--   affect the comparisons.
canonicalExampleBy         :: (Ord n, Ord e) => (PlanarGraph n e -> [Edge])
                              -> Edge -> PlanarGraph n e -> Bool
canonicalExampleBy fe e pg = all ((serE <=) . toSer) es
  where
    es = delete e $ fe pg

    toSer = serialiseBFS pg . Just

    serE = toSer e

-- | Filter out all those graphs for which 'canonicalExampleBy' isn't True.
--
--   For this function to be correct, no two @(Edge, PlanarGraph n e)@
--   pairs should have the same result from 'serialiseBFS'.  For
--   example, consider the following graph /g/:
--
--   >
--   >                 e1
--   >      ===== <--------- =====
--   >     (     )--------->(     )
--   >      =====          / =====
--   >      | ^           / /| | ^
--   >      | |          / /   | |
--   >      | |         / /    | |
--   >      | |        / /     | |
--   >      | |       / /      | |
--   >      | |      / /       | |
--   >      | |     / /        | |
--   >      | |    / /         | |
--   >      | |   / /          | |
--   >      v | |/ /           v |
--   >      ===== /          =====
--   >     (     )<---------(     )
--   >      ===== ---------> =====
--   >                 e2
--   >
--
--   Then @onlyCanonicalExamples 'halfEdges' [(e1,g), (e2,g)]@ will
--   return both graphs, even though they represent the same graph.
--
--   Note that this really only makes sense for graphs of type
--   @PlanarGraph () ()@, unless you are sure that the labels won't
--   affect the comparisons.
onlyCanonicalExamples    :: (Ord n, Ord e) => (PlanarGraph n e -> [Edge])
                            -> [(Edge, PlanarGraph n e)] -> [(Edge, PlanarGraph n e)]
onlyCanonicalExamples fe = filter (uncurry $ canonicalExampleBy fe)

-- -----------------------------------------------------------------------------
-- Utility functions

showsFrom :: (a -> Word) -> String -> Int -> a -> ShowS
showsFrom f nm _ a = showString nm . showChar '_' . shows (f a)

readsFrom :: (Word -> a) -> String -> ReadPrec a
readsFrom f nm = lift $ do _ <- string nm
                           _ <- char '_'
                           n <- readS_to_P reads
                           return $ f n

-- Get the previous element in the CList; assumes non-empty
prevElem :: CList a -> a
prevElem = fromJust . focus . rotL

nextElem :: CList a -> a
nextElem = fromJust . focus . rotR
