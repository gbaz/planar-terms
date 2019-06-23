{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module PlanarTerms where
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (shift)
import Diagrams.TwoD.GraphViz
import Data.GraphViz.Attributes.Complete
import Control.Arrow(first)
import Graphics.SVGFonts
import Data.Graph.Planar as P
import qualified Data.Map as M
import Data.List(nub)
import Data.Maybe (fromJust)


-- context is generated by zero, extension (add one to the right), usage (move turnstile left), and closure (chop off right side of turnstile).

data Nat = Z | S Nat deriving (Read, Show, Eq)


data C = C [Nat] [Nat] deriving (Read, Show, Eq)

-- variables used in scope, terms built in scope
emptyStack = C [Z] [Z]

partitions (S Z) = [(S Z, Z)]
partitions (S x) = (S Z, x) : map (\(a,b) -> (S a, b)) (partitions x)

intro :: C -> [C]
intro (C (h:hs) ts) = map (\(h1,h2) -> C (h1 : h2 : hs) (Z : ts)) (partitions (S h))
intro (C h ts) = [C (S Z : h) (Z : ts)]

shift :: C -> [C]
shift (C (S h : hs) (t:ts)) = [C (h : hs) (S t : ts)]
shift _ = []

app :: C -> [C]
app (C h (S (S t) : ts)) = [C h (S t : ts)]
app _ = []

close :: C -> [C]
close (C (Z:hs) (S Z:t:ts)) = [C hs (S t : ts)]
close _ = []

data Op = CI | CA | CS | CC deriving (Read, Show, Eq, Ord)

{-

Term generation produces the expected sequence

*> map length (map genTerm [1..])
[1,4,32,336,4096,54912,

-}

genTerm :: Int -> [[Op]]
genTerm x = map reverse . map snd $ go x (emptyStack,[])
  where go n (c,os) = is ++ ss ++ as ++ cs
         where
          is | n == 0 = if c == C [Z] [S Z] then [(c,os)] else []
             | otherwise = go (n - 1) . (,CI:os) =<< intro c
          as = go n . (,CA:os) =<< app c
          ss = go n . (,CS:os) =<< shift c
          cs = go n . (,CC:os) =<< close c

data FOL = Bind String FOL | App FOL FOL | Var String deriving Show

termToFOL :: [Op] -> FOL
termToFOL xs =  snd $ go ['a'..'z'] [] [] xs
  where go ns cxt [t] [] = (([],[],[],[]),t)
        go (n:ns) cxt tms (CI:os) =
                  let ((ns', cxt', tms', os'),body) =  go ns (n:cxt) [] os
                  -- should be cxt == cxt'
                  in go ns' cxt' ((Bind (n:[]) body) : tms) os'
        go ns (v:cxt) tms (CS:os) = go ns cxt ((Var (v:[])):tms) os
        go ns (cxt) tms (CA:os) = case tms of
                                       (t1:t2:ts) -> go ns cxt ((App t1 t2) :ts) os
        go ns cxt (t:[]) (CC:os) = ((ns,cxt,[],os),t)

        go ns cxt tms os = error $ show (ns, cxt, tms, os)

showFOL :: FOL -> String
showFOL (Bind s x) = "λ"++s++"."++showFOL x
showFOL (App x y) = "("++showFOL x ++ ")(" ++ showFOL y ++ ")"
showFOL (Var s) = s

boringLambda :: String -> String
boringLambda = map go
 where go 'λ' = '\\'
       go x = x

folToGraph = uncurry mkGraph . stripSelfLoop . fst . folToGraphData

stripSelfLoop (vs,es) = (vs,filter go es)
 where go (i,o,_) = i /= o

folToGraphData
  :: FOL -> (([String], [(String, [Char], [Char])]), Integer)
folToGraphData = (first . first) ("in":) . go "in" 0
  where
    go parent n (Bind s x) =
       let lbl = s
           ((v1,e1),n1) = go lbl n x
       in ((lbl:v1, (lbl,parent,lbl ++ "->" ++ parent):e1),n1)
    go parent n (App x y) =
       let lbl = show n
           ((v1,e1),n1) = go lbl (n+1) x
           ((v2,e2),n2) = go lbl n1 y
       in ((lbl : (v1 ++ v2), (lbl,parent, lbl ++ "!->" ++ parent) : (e1 ++ e2)),n2)
    go parent n (Var s) =
       let lbl = s
       in (([], (lbl,parent, lbl ++ "!!->" ++ parent) : []),n)

-- todo control edge positioning?
folToPlanarGraph :: FOL -> PlanarGraph String String
folToPlanarGraph tm = let (nd, gr) = P.addNode "in" P.empty
                      in go nd gr 0 (M.singleton "in" nd) tm
    where
      go parent g num env (Bind s x) =
         let (n1, g1) = addNode s g
             gph = go n1 g1 num (M.insert s n1 env) x
         in snd $ addEdge n1 Anywhere parent Anywhere (show (n1,parent)) (show (n1,parent)) gph
      go parent g num env (App x y) =
         let (n1, g1) = addNode (show num) g
             gph  = go n1 g1 (num+1) (M.insert (show num) n1 env) x
             gph2 = go n1 gph (num+1) (M.insert (show num) n1 env) y
         in snd $ addEdge n1 Anywhere parent Anywhere (show (n1,parent)) (show (n1,parent)) gph2
      go parent g num env (Var s) =
         let Just n1 = M.lookup s env
         in snd $ addEdge n1 Anywhere parent Anywhere (show (n1,parent)) (show (n1,parent)) g

planarToGraph :: Eq c => PlanarGraph b c -> ([b], [(b, b, c)])
planarToGraph x = rename . stripSelfLoop . fmap nub $ (vs, es)
  where
    xs = serialise x
    vs = map (\(a,_,_)-> a) xs
    es = concatMap (\(a,_,e1) -> map (\(_,b,e,_) -> ( a, b,e)) e1) xs
    vnames = M.fromList . map (\(a,b,_) -> (a,b)) $ xs
    renameVar  = fromJust . flip M.lookup vnames
    rename (vws,ews) = (map renameVar vws,
                        map (\(a,b,c) -> (renameVar a, renameVar b, c)) ews)

--planarToGraph :: PlanarGraph String String ->



dualPlanar g = snd $ toDual vlabel elabel (getFaces g)
  where elabel _ _ _ = "edge"
        vlabel f = show f

-- e.g. mapM_ (uncurry emitSvg) $ zip (genTerm 3) (map show [0..5])

emitSvg tm name = do
  l2 <- lin2
  let text' d s = (strokeP $ textSVG' (TextOpts l2 INSIDE_H KERN False d d) s)
                   # lw none # fc black

  let gph = folToGraph . termToFOL $ tm
--   print gph
--  putStrLn (showFOL . termToFOL $ tm)
  gr <- layoutGraph Dot (gph)
  let dia :: Diagram B
      dia = drawGraph
                     (\lbl loc -> place (text' 6 lbl <> circle 19) loc)
                     (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)
                     gr
      opts p = with & gaps .~ 22 & arrowShaft .~ (unLoc . head $ pathTrails p)
      labeledDia = vsep 2 [text' 10 (boringLambda . showFOL . termToFOL $ tm), dia]

  renderSVG (name ++ ".svg") (mkWidth 250) labeledDia

{-
sft =  map showFOL . map termToFOL . genTerm

pft = mapM_ putStrLn . sft

gft = map folToGraph . map termToFOL . genTerm


pgft = map folToPlanarGraph . map termToFOL . genTerm

go x y = do
  l2 <- lin2
  let text' d s = (strokeP $ textSVG' (TextOpts l2 INSIDE_H KERN False d d) s)
                   # lw none # fc black

  let gph = folToGraph . termToFOL $ genTerm x !! y
  print gph
  putStrLn (showFOL . termToFOL $ genTerm x !! y)
  gr <- layoutGraph Dot (gph)
  let dia :: Diagram B
      dia = drawGraph
                     (\lbl loc -> place (text' 6 lbl <> circle 19) loc)
                     (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)
                     gr
      opts p = with & gaps .~ 22 & arrowShaft .~ (unLoc . head $ pathTrails p)

  renderSVG "out.svg" (mkWidth 250) dia

  let gph2 = planarToGraph . folToPlanarGraph . termToFOL $ genTerm x !! y
  gr <- layoutGraph Dot (uncurry mkGraph gph2)
  let dia :: Diagram B
      dia = drawGraph
                     (\lbl loc -> place (text' 6 lbl <> circle 19) loc)
                     (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)
                     gr
      opts p = with & gaps .~ 22 & arrowShaft .~ (unLoc . head $ pathTrails p)

  renderSVG "out2.svg" (mkWidth 350) dia



main = simpleGraphDiagram Dot (gft 3 !! 7) >>= defaultMain
-}
{-
main = theGraph >>= defaultMain
  where
    theGraph :: IO (Diagram B)
    theGraph = simpleGraphDiagram Dot hex
-}

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )
