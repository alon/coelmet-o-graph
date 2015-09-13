module CSV where

import String

parse : String -> List (List String)
parse s =
        List.map (String.split ",") (String.lines s)
