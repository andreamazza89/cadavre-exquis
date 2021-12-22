module Utils.List exposing (traverseMaybe)

-- probably inefficient, as I should probably define sequence in terms of traverse using id instead of this


traverseMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
traverseMaybe toB =
    List.map toB >> sequenceMaybe


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe =
    List.foldl folding (Just [])


folding : Maybe a -> Maybe (List a) -> Maybe (List a)
folding item acc =
    Maybe.andThen
        (\i ->
            Maybe.map ((::) i) acc
        )
        item
