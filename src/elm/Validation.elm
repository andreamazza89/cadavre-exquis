module Validation exposing
    ( Validation
    , checkIsTrue
    , for
    , require
    , run
    )


type Validation input output
    = Validation (input -> Maybe output)


for : output -> Validation input output
for output =
    Validation (always (Just output))


require : (input -> Maybe outA) -> Validation input (outA -> outB) -> Validation input outB
require toArgument (Validation toBuilder) =
    Validation
        (\input ->
            Maybe.map2 (<|)
                (toBuilder input)
                (toArgument input)
        )


checkIsTrue : (input -> Bool) -> Validation input output -> Validation input output
checkIsTrue predicate (Validation validation) =
    Validation
        (\input ->
            if predicate input then
                validation input

            else
                Nothing
        )


run : input -> Validation input output -> Maybe output
run input (Validation runValidation) =
    runValidation input
