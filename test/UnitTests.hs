{-# language
        LambdaCase
      , OverloadedStrings
#-}

module Main (main) where

import Data.Aeson (ToJSON(..))
import Data.Aeson ((.=), Value(..), object, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Foldable (toList)
import Data.List (group, sort)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as BC8

import Grafana

main :: IO ()
main = defaultMain (testGroup "Tests" [unitTests])

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "makeTargets makes distinct refIds" makeTargetsDistinctRefids
  , graphiteQuerySerialize
  , panelSerialize
  ]

makeTargetsDistinctRefids :: Assertion
makeTargetsDistinctRefids =
  let query1 = Metric [Literal "a", Literal "b", Literal "c"]
      query2 = Metric [Literal "d", Literal "e", Literal "f"]
      targets = makeTargets [query1, query2]
  in  distinctOn refId targets @? "Non-distinct refids"

distinctOn :: (Eq b, Ord b, Foldable t) => (a -> b) -> t a -> Bool
distinctOn predicate collection =
  all isSingleton . group . sort . map predicate . toList $ collection
  where
    isSingleton = \case
      [_] -> True
      _ -> False

graphiteQuerySerialize :: TestTree
graphiteQuerySerialize = testGroup "graphite queries are serialized properly"
  [ testCase "alias" serializeAlias
  , testCase "aliasSub" serializeAliasSub
  , testCase "escaping chars" serializeEscapesBadChars
  ]
  
serializeAlias :: Assertion
serializeAlias =
  let query = Alias (Metric [Anything, Anything, Anything]) "Something else"
  in  serializeQuery query @?= "alias(*.*.*,'Something else')"

serializeAliasSub :: Assertion
serializeAliasSub =
  let query = AliasSub (Metric [Anything, Anything, Anything]) "X" "Y"
  in  serializeQuery query @?= "aliasSub(*.*.*,'X','Y')"

serializeEscapesBadChars :: Assertion
serializeEscapesBadChars =
  let query = Metric [Literal "minipops 67 [120.2][source field mix]"]
  in  serializeQuery query @?= "minipops671202sourcefieldmix"

panelSerialize :: TestTree
panelSerialize = testGroup "Panel serialization"
  [ testCase "row serialization" rowSerialize
  , testCase "graph serialization" graphSerialize
  , testCase "table serialization" tableSerialize
  ]

defGridPos :: GridPos
defGridPos = GridPos 1 1 0 0

defGridPosJSON :: Value
defGridPosJSON = 
  object
    [ "h" .= Number 1
    , "w" .= Number 1
    , "x" .= Number 0
    , "y" .= Number 0
    ]

defQueries :: [GraphiteQuery]
defQueries =
  [ Metric [Anything, Anything, Anything]
  , Metric [Anything, Anything, Anything]
  ] 

assertEqJSON :: (Show a, Eq a, ToJSON a) => a -> a -> Assertion
assertEqJSON a b =
  let prettyJSON = lines . BC8.unpack . encodePretty
      diff = ppDiff (getGroupedDiff (prettyJSON a) (prettyJSON b))
  in  (a == b) @? diff 

rowSerialize :: Assertion
rowSerialize =
  assertEqJSON
    (toJSON (rowPanel (Row "name") defGridPos))
    (object 
      [ "gridPos" .= defGridPosJSON
      , "title" .= String "name"
      , "type" .= String "row"
      ])

graphSerialize :: Assertion
graphSerialize =
  assertEqJSON 
    (toJSON (graphPanel (Graph "name" defQueries Connected Nothing) defGridPos))
    (object
      [ "gridPos" .= defGridPosJSON
      , "title" .= String "name"
      , "type" .= String "graph"
      , "nullPointMode" .= Connected
      , "targets" .= 
          [ object [ "refId" .= String "I0", "target" .= String "*.*.*"]
          , object [ "refId" .= String "I1", "target" .= String "*.*.*"]
          ]
      ])

tableSerialize :: Assertion
tableSerialize =
  let
    table = defaultTable
      { tableTitle = "name"
      , tableQueries = defQueries
      , tableColumns = columns ["Avg", "Current"]
      , tableStyles = 
          [ defaultStyles
              { thresholds = StyleThresholds [5, 10]
              , decimals = 2
              , unit = PercentFormat
              , colorMode = ColorCell
              }
          ]
      }
  in
    assertEqJSON
      (toJSON (tablePanel table defGridPos))
      (object
        [ "gridPos" .= defGridPosJSON
        , "title" .= String "name"
        , "type" .= String "table"
        , "styles" .= 
            [ object
                [ "pattern" .= String "/.*/"
                , "alias" .= String ""
                , "thresholds" .= [String "5.0", String "10.0"]
                , "decimals" .= Number 2
                , "type" .= String "number"
                , "colorMode" .= String "cell"
                , "colors" .= Array mempty
                , "unit" .= String "percent"
                ]
            ]
        , "targets" .= 
            [ object ["refId" .= String "I0", "target" .= String "*.*.*"]
            , object ["refId" .= String "I1", "target" .= String "*.*.*"]
            ]
        , "columns" .= 
            [ object ["text" .= String "Avg", "value" .= String "avg"]
            , object ["text" .= String "Current", "value" .= String "current"]
            ]
        , "valueFontSize" .= Number 100
        , "transform" .= String "timeseries_aggregations"
        ])
