module DAM4G.Compiler.Optimizer.Translate.MatchComp where

import Prelude

import Control.Monad.State (class MonadState)
import DAM4G.Compiler.Name as N
import DAM4G.Compiler.Optimizer.Translate.Monad (TranslState, lookupConstructor, lookupType)
import DAM4G.Compiler.Syntax.AST as AST
import DAM4G.Compiler.Syntax.Source (emptyLoc)
import DAM4G.Compiler.Types (AtomicConstant, ConstructorTag)
import Data.Array (mapWithIndex, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)

type MatchMatrixLine a = { pats :: Array (AST.Pattern a), act :: Int }

type MatchMatrixRec a =
  { matrix :: Array (MatchMatrixLine a)
  , heads :: Array (AST.Expr a)
  }

data DecisionTree a
  = Fail
  | Leaf Int
  | Choice (DecisionTree a) (DecisionTree a)
  | Conditional (AST.Expr a) (Array (AtomicConstant /\ DecisionTree a))
  | JumpThru (AST.Expr a) (Array (ConstructorTag /\ DecisionTree a))

derive instance Functor DecisionTree
derive instance Generic (DecisionTree a) _
instance Show a => Show (DecisionTree a) where
  show it = genericShow it

matchAlways :: forall a. AST.Pattern a -> Boolean
matchAlways = case _ of
  AST.PatWildcard _ -> true
  AST.PatVar _ _ -> true
  AST.PatAlias _ pat _ -> matchAlways pat
  AST.PatTyped _ pat _ -> matchAlways pat
  _ -> false

splitReduce :: forall a. MatchMatrixRec a -> (MatchMatrixRec a /\ MatchMatrixRec a)
splitReduce { heads, matrix } = do
  let
    { init: reducible
    , rest: irreducible
    } = matrix
      # Array.span (_.pats >>> Array.head >>> maybe false matchAlways)
  (/\)
    { heads: Array.drop 1 heads
    , matrix:
        (\line -> line { pats = Array.drop 1 line.pats }) <$> reducible
    }

    { heads, matrix: irreducible }

type Discriminator = Either AtomicConstant ConstructorTag

toplevelDiscriminator :: forall a. AST.Pattern a -> Maybe Discriminator
toplevelDiscriminator = case _ of
  AST.PatAlias _ pat _ -> toplevelDiscriminator pat
  AST.PatTyped _ pat _ -> toplevelDiscriminator pat
  AST.PatConst _ cst -> Just $ Left cst
  AST.PatConstructor _ _ tag _ -> Just $ Right tag
  _ -> Nothing

decomposeLeftmost
  :: forall m
   . MonadState TranslState m
  => MatchMatrixRec AST.Ann
  -> m
       { matchHead :: AST.Expr AST.Ann
       , jumpTbl :: Array (Discriminator /\ MatchMatrixRec AST.Ann)
       , restMatrix :: MatchMatrixRec AST.Ann
       }
decomposeLeftmost matrix = do
  -- let
  --   _ = unsafePerformEffect do
  --     Console.log "======= enter: decompose left most ======"
  --     Console.log "[input matrix]"
  --     Console.logShow $ const NoAnn <$> AST.MatchMatrix matrix
  case Array.uncons matrix.heads of
    Nothing -> unsafeCrashWith "Impossible: cannot decompose 0-width matrix"
    Just { head: matchHead, tail: headsRest } -> do
      let
        -- 最左列のパターンのトップレベルシンボルによって行列を分類
        classify matrices matrix' =
          let
            _ = unsafePerformEffect do
              Console.log "==== classify ===="
              Console.log "[input matrix]"
              Console.logShow matrix'
              Console.log "[matrices]"
              Console.logShow matrices

          in
            matrix' # Array.uncons >>> case _ of
              Nothing ->
                -- let
                --   _ = unsafePerformEffect do
                --     Console.log "====== leave: classify (empty matrix) ======="
                --     Console.log "[jump table]"
                --     Console.logShow (map (const NoAnn) <$> matrices)
                --     Console.log "[rest matrix]"
                --     Console.logShow $ const NoAnn <$> (AST.MatchMatrix (matrix { matrix = [] }))
                -- in

                pure $ (Map.toUnfoldable matrices) /\ { heads: matrix.heads, matrix: [] }
              Just { head: { pats, act }, tail: matRest } -> do
                case Array.uncons pats of
                  Nothing -> unsafeCrashWith "Impossible"
                  Just { head: pat, tail: patRest } -> do
                    case toplevelDiscriminator pat of
                      Nothing ->
                        -- let
                        --   _ = unsafePerformEffect do
                        --     Console.log "====== leave: classify (reached reducible left-most) ======="
                        --     Console.log "[jump table]"
                        --     Console.logShow (map (const NoAnn) <$> matrices)
                        --     Console.log "[rest matrix]"
                        --     Console.logShow $ const NoAnn <$> (AST.MatchMatrix (matrix { matrix = matrix' }))
                        -- in
                        pure $ (/\)
                          (Map.toUnfoldable matrices)
                          (matrix { matrix = matrix' })
                      Just d -> do
                        newHeads /\ newLine <- expandLine matchHead headsRest act pat patRest
                        case Map.lookup d matrices of
                          Just mat -> do
                            let
                              newMatrix =
                                { heads: newHeads
                                , matrix: Array.snoc mat.matrix newLine
                                }
                            -- _ = unsafePerformEffect do
                            --   Console.log "[Toplevel discriminator]"
                            --   Console.logShow d
                            --   Console.log "[new matrix]"
                            --   Console.logShow (const NoAnn <$> newMatrix)
                            --   Console.log "[rest matrix]"
                            --   Console.logShow $ const NoAnn <$> (AST.MatchMatrix (matrix { matrix = matRest }))
                            classify (Map.update (\_ -> Just newMatrix) d matrices) matRest
                          _ -> do

                            let
                              newMatrix =
                                { heads: newHeads
                                , matrix: [ newLine ]
                                }
                            -- _ = unsafePerformEffect do
                            --   Console.log "[Toplevel discriminator]"
                            --   Console.logShow d
                            --   Console.log "[new matrix]"
                            --   Console.logShow (const NoAnn <$> newMatrix)
                            --   Console.log "[rest matrix]"
                            --   Console.logShow $ const NoAnn <$> (AST.MatchMatrix (matrix { matrix = matRest }))
                            classify (Map.insert d newMatrix matrices) matRest
      jumpTbl /\ restMatrix <- classify Map.empty matrix.matrix
      -- let
      --   _ = unsafePerformEffect do
      --     Console.log "========= leave: decompose left most ========"
      --     Console.log "[jumpTbl]"
      --     Console.logShow $ rmap (map (const NoAnn)) <$> jumpTbl
      --     Console.log "[rest matrix]"
      --     Console.logShow $ restMatrix
      pure $ { matchHead, jumpTbl, restMatrix }

  where
  expandLine matchHead headsRest act pat patRest = do
    let
      go = case _ of
        AST.PatTyped _ p _ -> go p
        AST.PatAlias _ p _ -> go p
        AST.PatConst _ _ -> do
          pure $ headsRest /\ { pats: patRest, act }
        AST.PatConstructor _ typname tag subpats -> do
          typeInfo <- lookupType typname
          case typeInfo.constrs !! tag of
            Nothing -> unsafeCrashWith "Impossible: constructor is not a member of the type"
            Just constrname -> do
              constrDesc <- lookupConstructor constrname
              let
                expandedHeads = constrDesc.argTypes #
                  mapWithIndex \i argTyp -> do
                    let
                      ann = AST.AnnExpr
                        emptyLoc
                        (const { src: N.Compiler } <$> argTyp)
                    AST.ExprField ann matchHead i

              pure $ (expandedHeads <> headsRest)
                /\ { pats: subpats <> patRest, act }
        _ -> unsafeCrashWith "Impossible: classify always-matched pattern"
    go pat

data NoAnn = NoAnn

instance Show NoAnn where
  show _ = ""

composeDecisionTree :: forall m. MonadState TranslState m => AST.MatchMatrix AST.Ann -> m (DecisionTree AST.Ann)
composeDecisionTree (AST.MatchMatrix matrix) = divide $ matrix { matrix = mapWithIndex (\i ln -> ln { act = i }) matrix.matrix }
  where
  divide mm = do
    -- let
    --   _ = unsafePerformEffect do
    --     Console.log "======== divide ======="
    --     Console.log "[input matrix]"
    --     logShow $ (\_ -> NoAnn) <$> matrix
    case Array.uncons mm.matrix of
      Nothing -> pure Fail
      Just { head: line }
        | [] <- mm.heads
        , [] <- line.pats -> pure $ Leaf line.act
        | otherwise -> do
            let
              reduced /\ reducible = splitReduce mm
            -- let
            --   _ = unsafePerformEffect do
            --     Console.log "======= splitted & reduced ======"
            --     Console.log "[reduced]"
            --     Console.logShow (const NoAnn <$> reduced)
            --     Console.log "[reducible]"
            --     Console.logShow (const NoAnn <$> reducible)

            { matchHead, jumpTbl, restMatrix } <- decomposeLeftmost reducible
            composedJumpTbl <- composeJumpTable matchHead jumpTbl
            -- let
            --   _ = unsafePerformEffect do
            --     Console.log "======= decomposed ======"
            --     Console.log "[jump table]"
            --     Console.logShow jumpTbl
            --     Console.log "[composed jump table]"
            --     Console.logShow composedJumpTbl
            --     Console.log "[rest matrix]"
            --     Console.logShow (const NoAnn <$> restMatrix)

            leftTree <- mkChoice <$> divide reduced <*> pure composedJumpTbl --composeJumpTable matchHead jumpTbl
            mkChoice leftTree <$> divide restMatrix

  composeJumpTable matchHead tbl =
    case partition [] [] tbl of
      { conditional, switch }
        | [] <- conditional
        , [] <- switch -> pure Fail
        | [] <- switch -> do
            Conditional matchHead <$> traverse (\(cst /\ mat) -> (/\) cst <$> divide mat) conditional
        | [] <- conditional -> do
            JumpThru matchHead <$> traverse (\(constr /\ mat) -> (/\) constr <$> divide mat) switch
        | otherwise -> unsafeCrashWith "Impossible: Mixed toplevel discriminator"
    where
    partition conditional switch = Array.uncons >>> case _ of
      Nothing -> { conditional, switch }
      Just { head, tail } -> case head of
        Right constrTag /\ tbl' -> partition conditional (Array.snoc switch (constrTag /\ tbl')) tail
        Left cst /\ tbl' -> partition (Array.snoc conditional (cst /\ tbl')) switch tail

  mkChoice = case _, _ of
    Fail, b2 -> b2
    b1, Fail -> b1
    b1@(Leaf _), _ -> b1
    b1, b2 -> Choice b1 b2
