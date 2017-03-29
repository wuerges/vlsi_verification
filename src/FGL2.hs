module FGL2 where

import qualified Data.IntMap as IM
import qualified Data.Sequence as SQ
import qualified Data.Set as S


type Node = Int
data Gr a b  = Gr { ctxMap :: IM.IntMap (Ctx a b)
                  , reMap :: IM.IntMap Node }
  remap :: IM.IntMap Node
data Ctx a b = Ctx { out' :: SQ.Seq (Link a)
                   , node' :: Node
                   , lab' :: b
                   , inn' ::  SQ.Seq (Link a) }
data Match a b = Match (Ctx a b) (Gr a b)
data Link a = Link a Node
data Edge a = Edge Node Node a



delNode :: Node ->  Gr a b -> Gr a b
delNode

matchJoinCtx ctx1 y g =
  case match y g@(Gr cm rm) of
    Nothing -> Just ctx1 g
    Just ctx2 -> Just (joinCtx ctx1 ctx2) (Gr (IM.delete y cm) rm)

match :: Node -> Gr a b -> Maybe (Match a b)
match n g@(Gr cm rm) =
  case (IM.lookup n cm, IM.lookup n rm) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just y) -> match n Nothing y g
    (Just x, Nothing) -> Just $ Match x (Gr (IM.delete n cm) rm)
    (Just x, Just y ) -> matchJoinCtx x y (Gr (IM.delete x cm) rm)

(&) :: Ctx a b -> Gr a b -> Gr a b
ctx & (Gr cm rm) = (Gr (IM.insert (node' ctx) ctx) rm)

