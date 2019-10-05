module MyHuffman exposing (encode, get_huffman_table)

import Dict exposing (Dict, empty, insert)


type
    HTreeNode
    -- Huffman Tree
    = HTreeNode
        { name : String
        , prob : Float
        , children :
            Maybe
                -- There is always either two or 0 children
                { lchild : HTreeNode
                , rchild : HTreeNode
                }
        }


htget f x =
    case x of
        HTreeNode y ->
            f y


get_sym2code_dict : List ( String, Float, String ) -> Dict String String
get_sym2code_dict l =
    let
        helper : List ( String, Float, String ) -> Dict String String -> Dict String String
        helper l2 d =
            case l2 of
                ( sym, _, code ) :: ltail ->
                    helper ltail (insert sym code d)

                [] ->
                    d
    in
    helper l empty


encode : String -> List ( String, Float, String ) -> String
encode s table =
    let
        sym2code =
            get_sym2code_dict table

        helper : String -> String -> String
        helper s2 s_encoded =
            case String.uncons s2 of
                Nothing ->
                    s_encoded

                Just ( sym, stail ) ->
                    case Dict.get (String.fromChar sym) sym2code of
                        Nothing ->
                            -- Should not happen anyway
                            ""

                        Just code ->
                            helper stail (s_encoded ++ code)
    in
    helper s ""


get_huffman_table : String -> Maybe (List ( String, Float, String ))
get_huffman_table =
    get_probs >> get_tree >> huffman_partition >> assign_codes


get_probs_helper : String -> Dict Char Float -> Dict Char Float
get_probs_helper s d =
    case String.uncons s of
        Nothing ->
            d

        Just ( ch, stail ) ->
            case Dict.get ch d of
                Just v ->
                    get_probs_helper stail (insert ch (v + 1.0) d)

                Nothing ->
                    get_probs_helper stail (insert ch 1.0 d)


get_probs : String -> List ( Char, Float )
get_probs s =
    List.map (\( ch, f ) -> ( ch, f / toFloat (String.length s) ))
        (Dict.toList
            (get_probs_helper s empty)
        )


get_tree_helper : List ( Char, Float ) -> List HTreeNode -> List HTreeNode
get_tree_helper l ht =
    case l of
        ( ch, f ) :: ltail ->
            get_tree_helper ltail
                (HTreeNode
                    { name = String.fromChar ch
                    , prob = f
                    , children = Nothing
                    }
                    :: ht
                )

        [] ->
            ht


get_tree : List ( Char, Float ) -> List HTreeNode
get_tree l =
    get_tree_helper l []


huffman_partition : List HTreeNode -> Maybe HTreeNode
huffman_partition xs =
    let
        sorted_xs =
            List.sortBy (htget .prob) xs
    in
    case
        sorted_xs
    of
        rchild :: lchild :: ys ->
            huffman_partition
                (HTreeNode
                    { name = htget .name rchild ++ htget .name lchild
                    , prob = htget .prob rchild + htget .prob lchild
                    , children =
                        Just
                            { lchild = lchild
                            , rchild = rchild
                            }
                    }
                    :: ys
                )

        y :: [] ->
            Just y

        [] ->
            Nothing


assign_code_helper : HTreeNode -> String -> List ( String, Float, String )
assign_code_helper (HTreeNode { name, prob, children }) st =
    case children of
        Nothing ->
            [ ( name, prob, st ) ]

        Just { lchild, rchild } ->
            assign_code_helper lchild (st ++ "0") ++ assign_code_helper rchild (st ++ "1")


assign_codes : Maybe HTreeNode -> Maybe (List ( String, Float, String ))
assign_codes tnode =
    case tnode of
        Just atnode ->
            Just (assign_code_helper atnode "")

        Nothing ->
            Nothing
