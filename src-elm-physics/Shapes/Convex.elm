module Shapes.Convex exposing
    ( Convex
    , Face
    , addDirectionIfDistinct
    , expandBoundingSphereRadius
    , foldFaceEdges
    , fromBlock
    , init
    , placeIn
    , raycast
    )

import Array exposing (Array)
import Internal.Transform3d as Transform3d exposing (Transform3d)
import Internal.Vector3 as Vec3 exposing (Vec3)


type alias Face =
    { vertices : List Vec3
    , normal : Vec3
    }


type alias Convex =
    { faces : List Face
    , vertices : List Vec3 -- cached for performance
    , uniqueEdges : List Vec3 -- unique edges
    , uniqueNormals : List Vec3 -- unique face normals
    , position : Vec3
    , volume : Float
    }


placeIn : Transform3d coordinates defines -> Convex -> Convex
placeIn transform3d { faces, vertices, uniqueEdges, uniqueNormals, position, volume } =
    { faces = facesPlaceInHelp transform3d faces []
    , vertices = Transform3d.pointsPlaceIn transform3d vertices
    , uniqueEdges = Transform3d.directionsPlaceIn transform3d uniqueEdges
    , uniqueNormals = Transform3d.directionsPlaceIn transform3d uniqueNormals
    , volume = volume
    , position = Transform3d.pointPlaceIn transform3d position
    }


{-| Places faces into the frame.
Note that this reverses the faces list.
It reversces vertices list of each face,
but it is reversed originally so this is fine!
-}
facesPlaceInHelp : Transform3d coordinates defines -> List Face -> List Face -> List Face
facesPlaceInHelp transform3d faces result =
    case faces of
        { vertices, normal } :: remainingFaces ->
            facesPlaceInHelp
                transform3d
                remainingFaces
                ({ vertices = Transform3d.pointsPlaceIn transform3d vertices
                 , normal = Transform3d.directionPlaceIn transform3d normal
                 }
                    :: result
                )

        [] ->
            result


init : List (List Int) -> Array Vec3 -> Convex
init faceVertexLists vertices =
    let
        faces =
            initFaces faceVertexLists vertices
    in
    { faces = faces
    , vertices = Array.toList vertices
    , uniqueEdges =
        List.foldl
            (\face edges ->
                foldFaceEdges
                    (\v1 v2 ->
                        addDirectionIfDistinct (Vec3.direction v1 v2)
                    )
                    edges
                    face.vertices
            )
            []
            faces
    , uniqueNormals =
        List.foldl
            (\{ normal } ->
                addDirectionIfDistinct normal
            )
            []
            faces
    , position = Vec3.zero
    , volume = 0
    }


initFaces : List (List Int) -> Array Vec3 -> List Face
initFaces faceVertexLists convexVertices =
    List.map
        (\vertexIndices ->
            let
                vertices =
                    List.filterMap
                        (\i -> Array.get i convexVertices)
                        vertexIndices

                normal =
                    case vertices of
                        v1 :: v2 :: v3 :: _ ->
                            computeNormal v1 v2 v3

                        _ ->
                            -- Shouldn’t happen
                            Vec3.zero
            in
            { vertices = List.reverse vertices
            , normal = normal
            }
        )
        faceVertexLists


computeNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
computeNormal v1 v2 v3 =
    Vec3.normalize (Vec3.cross (Vec3.sub v3 v2) (Vec3.sub v1 v2))


fromBlock : Float -> Float -> Float -> Convex
fromBlock x y z =
    let
        v0 =
            { x = -x, y = -y, z = -z }

        v1 =
            { x = x, y = -y, z = -z }

        v2 =
            { x = x, y = y, z = -z }

        v3 =
            { x = -x, y = y, z = -z }

        v4 =
            { x = -x, y = -y, z = z }

        v5 =
            { x = x, y = -y, z = z }

        v6 =
            { x = x, y = y, z = z }

        v7 =
            { x = -x, y = y, z = z }
    in
    { faces =
        -- faces vertices are reversed for local coordinates
        -- then they become correct after transformation
        -- this is needed for performance
        [ { vertices = List.reverse [ v3, v2, v1, v0 ]
          , normal = Vec3.zNegative
          }
        , { vertices = List.reverse [ v4, v5, v6, v7 ]
          , normal = Vec3.zAxis
          }
        , { vertices = List.reverse [ v5, v4, v0, v1 ]
          , normal = Vec3.yNegative
          }
        , { vertices = List.reverse [ v2, v3, v7, v6 ]
          , normal = Vec3.yAxis
          }
        , { vertices = List.reverse [ v0, v4, v7, v3 ]
          , normal = Vec3.xNegative
          }
        , { vertices = List.reverse [ v1, v2, v6, v5 ]
          , normal = Vec3.xAxis
          }
        ]
    , vertices = [ v0, v1, v2, v3, v4, v5, v6, v7 ]
    , uniqueEdges = Vec3.basis
    , uniqueNormals = Vec3.basis
    , volume = x * y * z * 8
    , position = Vec3.zero
    }


{-| Add a candidate direction to a set if it is not a
near duplicate or near opposite of a direction already in the set.
-}
addDirectionIfDistinct : Vec3 -> List Vec3 -> List Vec3
addDirectionIfDistinct currentNormal uniques =
    if
        List.any
            (\direction ->
                Vec3.almostZero
                    (Vec3.cross
                        direction
                        currentNormal
                    )
            )
            uniques
    then
        uniques

    else
        currentNormal :: uniques


expandBoundingSphereRadius : Convex -> Float -> Float
expandBoundingSphereRadius { vertices } boundingSphereRadius =
    vertices
        |> List.foldl
            (\vertex ->
                max (Vec3.lengthSquared vertex)
            )
            (boundingSphereRadius * boundingSphereRadius)
        |> sqrt


raycast : { from : Vec3, direction : Vec3 } -> Convex -> Maybe { distance : Float, point : Vec3, normal : Vec3 }
raycast { direction, from } convex =
    List.foldl
        (\{ normal, vertices } maybeHit ->
            let
                dot =
                    Vec3.dot direction normal

                point =
                    case vertices of
                        first :: _ ->
                            first

                        [] ->
                            Vec3.zero
            in
            if dot < 0 then
                let
                    pointToFrom =
                        Vec3.sub point from

                    scalar =
                        Vec3.dot normal pointToFrom / dot
                in
                if scalar >= 0 then
                    let
                        intersectionPoint =
                            { x = direction.x * scalar + from.x
                            , y = direction.y * scalar + from.y
                            , z = direction.z * scalar + from.z
                            }

                        isInsidePolygon =
                            foldFaceEdges
                                (\p1 p2 result ->
                                    result
                                        && (Vec3.dot
                                                (Vec3.sub intersectionPoint p1)
                                                (Vec3.sub p2 p1)
                                                > 0
                                           )
                                )
                                True
                                vertices
                    in
                    if isInsidePolygon then
                        case maybeHit of
                            Just { distance } ->
                                if scalar - distance < 0 then
                                    Just
                                        { distance = scalar
                                        , point = intersectionPoint
                                        , normal = normal
                                        }

                                else
                                    maybeHit

                            Nothing ->
                                Just
                                    { distance = scalar
                                    , point = intersectionPoint
                                    , normal = normal
                                    }

                    else
                        maybeHit

                else
                    maybeHit

            else
                maybeHit
        )
        Nothing
        convex.faces


{-| Map the function to pairs of consecutive vertices,
starting with the pair (first, second), and so on, until (last, first).
-}
foldFaceEdges : (Vec3 -> Vec3 -> b -> b) -> b -> List Vec3 -> b
foldFaceEdges fn resultSeed vertices =
    case vertices of
        first :: _ :: _ ->
            foldFaceEdgesHelp fn first resultSeed vertices

        _ ->
            -- The list is empty or contains one element.
            resultSeed


foldFaceEdgesHelp : (Vec3 -> Vec3 -> b -> b) -> Vec3 -> b -> List Vec3 -> b
foldFaceEdgesHelp fn seed resultSeed vertices =
    case vertices of
        el1 :: rest1 ->
            case rest1 of
                [] ->
                    fn el1 seed resultSeed

                el2 :: _ ->
                    foldFaceEdgesHelp
                        fn
                        seed
                        (fn el1 el2 resultSeed)
                        rest1

        [] ->
            resultSeed
