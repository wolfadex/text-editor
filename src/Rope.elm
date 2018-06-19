module Rope exposing (Rope(..), toString, insert, remove, getLength)


type Rope
    = Empty
    | Leaf String Int
    | Node Rope Rope Int


splitLength : Int
splitLength = 10


joinLength : Int
joinLength = 5


rebalanceRatio : Float
rebalanceRatio = 1.2


adjust : Rope -> Rope
adjust rope =
    case rope of
        Empty ->
            Empty
        Leaf string length ->
            if length > splitLength then
                let
                    midPoint = length // 2
                    leftString = String.left midPoint string
                    rightString = String.dropLeft midPoint string
                    leftRope = adjust (Leaf leftString midPoint)
                    rightLength = length - midPoint
                    rightRope = if rightLength > 0 then
                                    adjust (Leaf rightString rightLength)
                                else
                                    Empty
                in
                    Node leftRope rightRope midPoint
            else
                rope
        Node left right leftLength ->
            if leftLength < joinLength then
                let
                    newText = toString left ++ (toString right)
                in
                    Leaf newText (String.length newText)
            else
                rope


toString : Rope -> String
toString rope =
    case rope of
        Empty ->
           ""
        Leaf string _ ->
           string
        Node left right _ ->
           (toString left) ++ (toString right)


insert : String -> Int -> Rope -> Rope
insert text position rope =
    case rope of
        Empty ->
            adjust (Leaf text (String.length text))
        Leaf string _ ->
            let
                newText = String.concat [ String.left position string
                                        , text
                                        , String.dropLeft position string
                                        ]
            in
                adjust <| Leaf newText (String.length newText)
        Node left right leftLength ->
            if position < leftLength then
                Node (insert text position left) right (leftLength + (String.length text))
            else
                Node left (insert text (position - leftLength) right) leftLength


remove : Int -> Int -> Rope -> Rope
remove from to rope =
    case rope of
        Empty ->
            Empty
        Leaf string length ->
            let
                l = String.left from string
                r = String.dropLeft to string
                newText = l ++ r
            in
                Leaf newText (String.length newText)
        Node left right leftLength ->
            -- rope
            let
                leftStart = min from leftLength
                leftEnd = min to leftLength
                rightLength = getLength right
                rightStart = max 0 (min (from - leftLength) rightLength)
                rightEnd = max 0 (min (to - leftLength) rightLength)
                newLeft = if leftStart < leftLength then remove leftStart leftEnd left else left
                newRight = if rightEnd > 0 then remove rightStart rightEnd right else right
            in
                adjust <| Node newLeft newRight (getLength newLeft)


getLength : Rope -> Int
getLength rope =
    case rope of
        Empty ->
            0
        Leaf _ length ->
            length
        Node _ right leftLength ->
            getLengthHelper leftLength right


getLengthHelper : Int -> Rope -> Int
getLengthHelper lengthSoFar rope =
    case rope of
        Empty ->
            lengthSoFar
        Leaf _ length ->
            lengthSoFar + length
        Node _ right leftLength ->
            getLengthHelper (lengthSoFar + leftLength) right


-- rebuild : Rope -> Rope TODO
-- rebuild rope =
--     case rope of
--         Empty ->
--
--         Leaf string int ->
--
--         Node rope rope2 int ->
--


-- rebalance : Rope -> Rope TODO
-- rebalance rope =
--     case rope of
--         Empty ->
--
--         Leaf string int ->
--
--         Node rope rope2 int ->
--


-- getSubstring : Int -> Int -> Rope -> String TODO
-- getSubstring from to rope =
--     case rope of
--         Empty ->
--             ""
--         Leaf string int ->
--
--         Node rope rope2 int ->
--
