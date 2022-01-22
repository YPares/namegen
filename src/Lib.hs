{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.TypeLits
import Data.Serialize
import Data.Proxy
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.Environment (getArgs)
import System.Random.MWC.CondensedTable (CondensedTableU, tableFromWeights, genFromTable)
import qualified Data.Vector.Unboxed as V
import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import System.Exit


-- | A sequence of n characters
newtype NGram (n::Nat) = NGram String
  deriving (Eq, Ord, Show, Serialize, A.ToJSONKey)

-- | Get the n first elems of the string to make a 'NGram', and returns the rest
-- of the string. Fails if the string contains less than n characters
toNGram :: forall n. (KnownNat n) => String -> (NGram n, String)
toNGram s | length s' == n = (NGram s', rest)
          | otherwise = error $ "toNGram: '" ++ s ++ "' contains fewer than "
                                ++ show n ++ " characters"
  where
    n = fromIntegral $ natVal (Proxy :: Proxy n)
    (s',rest) = L.splitAt n s

-- | A ngram of just n times '^'
initNGram :: forall n. (KnownNat n) => NGram n
initNGram = NGram $ replicate (fromIntegral $ natVal (Proxy :: Proxy n)) '^'

-- | Adds a char at the end of the NGram and pops the first
nextNGram :: NGram n -> Char -> NGram n
nextNGram (NGram s) c = NGram (drop 1 $ s ++ [c])

-- | The model to train: the conditional probability distribution
-- P(nextChar|previousNGram). Note: Integers are weights are, they are not
-- normalized probabilities.
newtype Model n = Model (Map.Map (NGram n) (Map.Map Char Integer))
  deriving (Show, Serialize, A.ToJSON)
instance Semigroup (Model n) where
  Model m1 <> Model m2 = Model $ Map.unionWith (Map.unionWith (+)) m1 m2
instance Monoid (Model n) where
  mempty = Model mempty

-- * Training

-- | A string with a length of 20 + n*2 lowercase chars, with n times '^' at the
-- beginning and least n times '$' at the end
newtype NormalizedString (n::Nat) = NormalizedString String
  deriving (Show)

normalizeString :: forall n. (KnownNat n) => String -> NormalizedString n
normalizeString s = NormalizedString $ s' ++ replicate (wantedLength - length s') '$'
  where p = Proxy :: Proxy n
        wantedLength = fromIntegral $ 20 + 2*natVal p
        s' = replicate (fromIntegral $ natVal p) '^' ++ take 20 (map C.toLower s)

unnormalizeString :: String -> String
unnormalizeString s = case s' of "" -> ""
                                 (c:cs) -> C.toUpper c : cs
  where
    s' = reverse $ dropWhile (=='$') $ reverse $ dropWhile (=='^') s

trainModel1 :: (KnownNat ngramSize) => NormalizedString ngramSize -> Model ngramSize
trainModel1 (NormalizedString s0) = mconcat $ go s0
  where
    go [] = error "getTrainingData: String did not contain padding at the end"
    go s@(_:rest) = let (ngram, nextChar:_) = toNGram s
                    in Model (Map.singleton ngram (Map.singleton nextChar 1))
                       : case nextChar of '$' -> []
                                          _   -> go rest

trainModel :: (KnownNat ngramSize) => [String] -> Model ngramSize
trainModel = mconcat . map (trainModel1 . normalizeString)

-- * Sampling

-- | A model ready to be sampled
type CondensedModel n = Map.Map (NGram n) (CondensedTableU Char)

toCondensedModel :: Model n -> CondensedModel n
toCondensedModel (Model mdl) = fmap toTable mdl
  where
    toTable = tableFromWeights . V.fromList . map (\(c,w) -> (c, fromIntegral w)) . Map.toList

sampleModel :: (PrimMonad m, KnownNat n)
            => Gen (PrimState m)
            -> CondensedModel n
            -> m String
sampleModel gen model = unnormalizeString <$> go initNGram
  where
    go ngram = do
      let table = model Map.! ngram
      c <- genFromTable table gen
      (c:) <$> case c of
                 '$' -> return []
                 _ -> go (nextNGram ngram c)

someFunc :: IO ()
someFunc = do
  args@(~[trainingFile, ngramSize]) <- getArgs
  case args of
    [_,_] -> return ()
    _ -> do putStrLn "Usage: exeName <training_file> <n>\n\
                     \Trains a model that learns the conditional probability of the kth character\n\
                     \of a name given the n characters before, and uses it to generate some new names.\n\
                     \For each generated name, say if it already existed in the training set."
            exitWith $ ExitFailure 1
  trainingSet <- lines <$> readFile trainingFile
  case someNatVal (read ngramSize) of
    Just (SomeNat (_ :: Proxy n)) -> do
      let model = toCondensedModel (trainModel trainingSet :: Model n)
      -- T.putStrLn $ T.decodeUtf8 $ Y.encode
      gen <- createSystemRandom
      samples <- replicateM 100 $ sampleModel gen model
      let existing = Set.fromList trainingSet
      forM_ samples $ \sample -> do
        putStrLn $ sample ++ if sample `Set.member` existing
                             then " (already in training set)"
                             else ""
    _ -> error "Could not read ngram size"
  
