module Common.Scene exposing (view)

import Common.Camera
import Common.Math
import Common.Meshes
import Common.Settings
import Common.Shaders
import Direction3d
import Frame3d
import Geometry.Interop.LinearAlgebra.Direction3d
import Geometry.Interop.LinearAlgebra.Frame3d
import Geometry.Interop.LinearAlgebra.Point3d
import Html
import Html.Attributes
import Length
import Math.Matrix4
import Math.Vector3
import Physics.Body
import Physics.Contact
import Physics.Coordinates
import Physics.World
import Point3d
import WebGL


type alias Params a =
    { settings : Common.Settings.Settings
    , world : Physics.World.World a
    , camera : Common.Camera.Camera
    , meshes : a -> Common.Meshes.Meshes
    , maybeRaycastResult : Maybe (Physics.World.RaycastResult a)
    , floorOffset :
        { x : Float
        , y : Float
        , z : Float
        }
    }


view :
    { red : Float, green : Float, blue : Float, alpha : Float }
    -> { red : Float, green : Float, blue : Float }
    -> Params { b | color : { red : Float, green : Float, blue : Float } }
    -> Html.Html msg
view bgColor shadowColor { settings, world, floorOffset, camera, maybeRaycastResult, meshes } =
    let
        lightDirection =
            Math.Vector3.normalize (Math.Vector3.vec3 -1 -1 -1)

        sceneParams =
            { lightDirection = lightDirection
            , camera = camera
            , debugWireframes = settings.debugWireframes
            , debugCenterOfMass = settings.debugCenterOfMass
            , maybeRaycastResult = maybeRaycastResult
            , meshes = meshes
            , shadow =
                Common.Math.makeShadow
                    (Math.Vector3.fromRecord floorOffset)
                    Math.Vector3.k
                    lightDirection
            }
    in
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor bgColor.red bgColor.green bgColor.blue bgColor.alpha
        ]
        [ Html.Attributes.width (round camera.width)
        , Html.Attributes.height (round camera.height)
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        ]
        ([ ( True
           , \entities ->
                List.foldl
                    -- (Physics.Body.data body).id
                    (\body ->
                        addBodyEntities
                            (Physics.Body.data body).color
                            shadowColor
                            sceneParams
                            body
                    )
                    entities
                    (Physics.World.bodies world)
           )
         , ( settings.debugContacts
           , \entities -> List.foldl (addContactIndicator sceneParams) entities (getContactPoints world)
           )
         ]
            |> List.filter Tuple.first
            |> List.map Tuple.second
            |> List.foldl (<|) []
        )


getContactPoints : Physics.World.World a -> List (Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates)
getContactPoints world =
    world
        |> Physics.World.contacts
        |> List.concatMap Physics.Contact.points
        |> List.map .point


type alias SceneParams a =
    { lightDirection : Math.Vector3.Vec3
    , camera : Common.Camera.Camera
    , debugWireframes : Bool
    , debugCenterOfMass : Bool
    , shadow : Math.Matrix4.Mat4
    , maybeRaycastResult : Maybe (Physics.World.RaycastResult a)
    , meshes : a -> Common.Meshes.Meshes
    }


addBodyEntities :
    { red : Float, green : Float, blue : Float }
    -> { red : Float, green : Float, blue : Float }
    -> SceneParams a
    -> Physics.Body.Body a
    -> List WebGL.Entity
    -> List WebGL.Entity
addBodyEntities colorBody colorShadow ({ meshes, lightDirection, shadow, camera, debugWireframes, debugCenterOfMass, maybeRaycastResult } as sceneParams) body entities =
    let
        transform =
            Geometry.Interop.LinearAlgebra.Frame3d.toMat4 (Physics.Body.frame body)

        color =
            Math.Vector3.vec3 colorBody.red colorBody.green colorBody.blue

        showRayCastNormal =
            False

        addNormals : List WebGL.Entity -> List WebGL.Entity
        addNormals acc =
            case maybeRaycastResult of
                Just res ->
                    if showRayCastNormal && Physics.Body.data res.body == Physics.Body.data body then
                        addNormalIndicator sceneParams transform { normal = res.normal, point = res.point } acc

                    else
                        acc

                Nothing ->
                    acc

        addCenterOfMass acc =
            if debugCenterOfMass then
                addContactIndicator sceneParams (Point3d.placeIn (Physics.Body.frame body) (Physics.Body.centerOfMass body)) acc

            else
                acc

        { mesh, wireframe } =
            meshes (Physics.Body.data body)
    in
    entities
        |> addCenterOfMass
        |> addNormals
        |> (if debugWireframes then
                (::)
                    (WebGL.entity
                        Common.Shaders.vertex
                        Common.Shaders.wireframeFragment
                        wireframe
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = color
                        , lightDirection = lightDirection
                        , transform = transform
                        }
                    )

            else
                (::)
                    (WebGL.entity
                        Common.Shaders.vertex
                        Common.Shaders.fragment
                        mesh
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = color
                        , lightDirection = lightDirection
                        , transform = transform
                        }
                    )
           )
        |> (if debugWireframes then
                -- Without shadow
                identity

            else
                -- With shadow
                (::)
                    (WebGL.entity
                        Common.Shaders.vertex
                        Common.Shaders.shadowFragment
                        mesh
                        { camera = camera.cameraTransform
                        , perspective = camera.perspectiveTransform
                        , color = Math.Vector3.vec3 colorShadow.red colorShadow.green colorShadow.blue
                        , lightDirection = lightDirection
                        , transform = Math.Matrix4.mul shadow transform
                        }
                    )
           )


{-| Render a collision point for the purpose of debugging
-}
addContactIndicator :
    SceneParams a
    -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
    -> List WebGL.Entity
    -> List WebGL.Entity
addContactIndicator { lightDirection, camera } point tail =
    WebGL.entity
        Common.Shaders.vertex
        Common.Shaders.fragment
        Common.Meshes.contact
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , color = Math.Vector3.vec3 1 0 0
        , lightDirection = lightDirection
        , transform = Geometry.Interop.LinearAlgebra.Frame3d.toMat4 (Frame3d.atPoint point)
        }
        :: tail


{-| Render a normal for the purpose of debugging
-}
addNormalIndicator :
    SceneParams a
    -> Math.Matrix4.Mat4
    ->
        { point : Point3d.Point3d Length.Meters Physics.Coordinates.BodyCoordinates
        , normal : Direction3d.Direction3d Physics.Coordinates.BodyCoordinates
        }
    -> List WebGL.Entity
    -> List WebGL.Entity
addNormalIndicator { lightDirection, camera } transform { normal, point } tail =
    WebGL.entity
        Common.Shaders.vertex
        Common.Shaders.fragment
        Common.Meshes.normal
        { camera = camera.cameraTransform
        , perspective = camera.perspectiveTransform
        , lightDirection = lightDirection
        , color = Math.Vector3.vec3 1 0 1
        , transform =
            Common.Math.makeRotateKTo (Geometry.Interop.LinearAlgebra.Direction3d.toVec3 normal)
                |> Math.Matrix4.mul
                    (Geometry.Interop.LinearAlgebra.Point3d.toVec3 point
                        |> Math.Matrix4.makeTranslate
                        |> Math.Matrix4.mul transform
                    )
        }
        :: tail
