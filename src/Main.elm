module Main exposing (conf, main)

-- TODO
--
-- * Add sound as per tempt/CarEngines/engine2.js
-- * Add music as per https://www.w3schools.com/graphics/game_sound.asp

import Acceleration
import Angle
import Axis3d
import Block3d
import Browser
import Browser.Events
import Common.Camera
import Common.Events
import Common.Fps
import Common.Meshes
import Common.Scene
import Common.Settings
import Direction3d
import Duration
import Force
import Frame3d
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Length
import Mass
import Physics.Body
import Physics.Coordinates
import Physics.Material as Material
import Physics.World
import Point3d
import Quantity
import Sphere3d
import Starter.ConfMain
import Starter.Flags
import Vector3d


conf : Starter.ConfMain.Conf
conf =
    { urls = []
    , assetsToCache = [ "/index.html" ]
    }


color :
    { box : { blue : Float, green : Float, red : Float }
    , car : { blue : Float, green : Float, red : Float }
    , slope : { blue : Float, green : Float, red : Float }
    , wall : { blue : Float, green : Float, red : Float }
    , wheel : { blue : Float, green : Float, red : Float }
    }
color =
    { wheel = { red = 0.4, green = 0.6, blue = 1 }
    , car = { red = 1, green = 0.7, blue = 0.5 }
    , slope = { red = 0.7, green = 0.8, blue = 1 }
    , wall = { red = 0.8, green = 0.9, blue = 1 }
    , box = { red = 1, green = 0.8, blue = 0.5 }
    }


{-| Give a name to each body, so that we can configure constraints
-}
type alias Data =
    { meshes : Common.Meshes.Meshes
    , id : Id
    , color : { red : Float, green : Float, blue : Float }
    }


type Id
    = Obstacle
    | Car (List Wheel)


type alias CarSettings =
    { downDirection : Direction3d.Direction3d Physics.Coordinates.BodyCoordinates
    , rightDirection : Direction3d.Direction3d Physics.Coordinates.BodyCoordinates
    , forwardDirection : Direction3d.Direction3d Physics.Coordinates.BodyCoordinates
    , suspensionRestLength : Length.Length
    , minSuspensionLength : Length.Length
    , maxSuspensionLength : Length.Length
    , radius : Length.Length
    , suspensionStiffness : Float
    , dampingCompression : Float
    , dampingRelaxation : Float
    , frictionSlip : Float
    , rollInfluence : Float
    , maxSuspensionForce : Force.Force
    , customSlidingRotationalSpeed : Maybe Float
    }


carSettings : CarSettings
carSettings =
    { downDirection = Direction3d.negativeZ
    , rightDirection = Direction3d.y
    , forwardDirection = Direction3d.x
    , suspensionRestLength = Length.meters 0.3
    , minSuspensionLength = Length.meters 0
    , maxSuspensionLength = Length.meters 0.6
    , radius = Length.meters 0.8
    , suspensionStiffness = 30
    , dampingCompression = 4.4
    , dampingRelaxation = 2.3
    , frictionSlip = 5
    , rollInfluence = 0.01
    , maxSuspensionForce = Force.newtons 100000
    , customSlidingRotationalSpeed = Just -30
    }


type alias Wheel =
    { chassisConnectionPoint : Point3d.Point3d Length.Meters Physics.Coordinates.BodyCoordinates
    , steering : Angle.Angle
    , rotation : Angle.Angle
    , deltaRotation : Angle.Angle
    , suspensionImpulse :
        Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , suspensionLength : Length.Length
    , engineForce : Force.Force
    , brake : Force.Force
    , contact :
        Maybe
            { point : Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
            , normal : Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
            , body : Physics.Body.Body Data
            }
    }


defaultWheel : Wheel
defaultWheel =
    { chassisConnectionPoint = Point3d.origin -- set for different wheels
    , steering = Quantity.zero
    , rotation = Quantity.zero
    , deltaRotation = Quantity.zero
    , suspensionImpulse = Quantity.zero
    , suspensionLength = Quantity.zero
    , engineForce = Quantity.zero
    , brake = Quantity.zero
    , contact = Nothing
    }


type alias Model =
    { flags : Flags
    , world : Physics.World.World Data
    , fps : List Float
    , settings : Common.Settings.Settings
    , camera : Common.Camera.Camera
    , speeding : Float
    , steering : Float
    , braking : Bool
    }


type Command
    = Speed Float
    | Steer Float
    | Brake
    | Reset
    | ToggleSettings


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "a" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "d" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "w" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "s" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    "b" ->
                        Json.Decode.succeed (toMsg Brake)

                    "r" ->
                        Json.Decode.succeed (toMsg Reset)

                    "q" ->
                        Json.Decode.succeed (toMsg ToggleSettings)

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )


type Msg
    = ForSettings Common.Settings.SettingsMsg
    | Tick Float
    | Resize Float Float
    | KeyDown Command
    | KeyUp Command


type alias Flags =
    { starter : Starter.Flags.Flags }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , world = initialWorld
      , fps = []
      , settings = Common.Settings.settings
      , speeding = 0
      , steering = 0
      , braking = False
      , camera =
            Common.Camera.camera
                { from = { x = -40, y = 40, z = 30 }
                , to = { x = 0, y = -7, z = 0 }
                }
      }
    , Common.Events.measureSize Resize
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ForSettings settingsMsg ->
            { model
                | settings = Common.Settings.update settingsMsg model.settings
            }

        Tick dt ->
            { model
                | fps = Common.Fps.update dt model.fps
                , world =
                    model.world
                        |> Physics.World.update
                            (\body ->
                                case (Physics.Body.data body).id of
                                    Car wheels ->
                                        simulateCar (Duration.seconds (1 / 60)) model wheels body

                                    _ ->
                                        body
                            )
                        |> Physics.World.simulate (Duration.seconds (1 / 60))
            }

        Resize width height ->
            { model | camera = Common.Camera.resize width height model.camera }

        KeyDown (Steer k) ->
            { model | steering = k }

        KeyDown (Speed k) ->
            { model | speeding = k }

        KeyDown Brake ->
            { model | braking = True }

        KeyDown Reset ->
            model

        KeyDown ToggleSettings ->
            model

        KeyUp (Steer k) ->
            { model
                | steering =
                    if k == model.steering then
                        0

                    else
                        model.steering
            }

        KeyUp (Speed k) ->
            { model
                | speeding =
                    if k == model.speeding then
                        0

                    else
                        model.speeding
            }

        KeyUp Brake ->
            { model | braking = False }

        KeyUp Reset ->
            { model | world = initialWorld }

        KeyUp ToggleSettings ->
            let
                settings =
                    model.settings
            in
            { model | settings = { settings | showSettings = not settings.showSettings } }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Common.Events.onResize Resize
        , Common.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


view : Model -> Html.Html Msg
view { settings, fps, world, camera } =
    Html.div [ Html.Attributes.id "elm" ]
        [ Common.Scene.view { red = 1, green = 0.5, blue = 0.5, alpha = 1 }
            { red = 0.9, green = 0.4, blue = 0.4 }
            { settings = settings
            , world = addWheelsToWorld world
            , camera = camera
            , meshes = .meshes
            , maybeRaycastResult = Nothing
            , floorOffset = floorOffset
            }
        , Common.Settings.view ForSettings settings []
        , if settings.showFpsMeter then
            Common.Fps.view fps (List.length (Physics.World.bodies world))

          else
            Html.text ""
        ]


addWheelsToWorld : Physics.World.World Data -> Physics.World.World Data
addWheelsToWorld world =
    let
        maybeCar =
            world
                |> Physics.World.bodies
                |> List.filterMap
                    (\b ->
                        case (Physics.Body.data b).id of
                            Car wheels ->
                                Just ( wheels, b )

                            _ ->
                                Nothing
                    )
                |> List.head
    in
    case maybeCar of
        Just ( wheels, car ) ->
            List.foldl
                (\wheel ->
                    let
                        frame =
                            Physics.Body.frame car

                        position =
                            wheel.chassisConnectionPoint
                                |> Point3d.placeIn (Physics.Body.frame car)

                        downDirection =
                            carSettings.downDirection
                                |> Direction3d.placeIn (Physics.Body.frame car)

                        rightDirection =
                            carSettings.rightDirection
                                |> Direction3d.placeIn (Physics.Body.frame car)

                        newPosition =
                            position |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                        newFrame =
                            frame
                                |> Frame3d.moveTo newPosition
                                |> Frame3d.rotateAround (Axis3d.through newPosition downDirection) wheel.steering
                                |> Frame3d.rotateAround (Axis3d.through newPosition rightDirection) wheel.rotation
                    in
                    Physics.World.add
                        (Physics.Body.sphere
                            wheelShape
                            { id = Obstacle
                            , meshes = wheelMesh
                            , color = color.wheel
                            }
                            |> Physics.Body.placeIn newFrame
                        )
                )
                world
                wheels

        Nothing ->
            world


addBoxes : ( Float, Float, Float ) -> Physics.World.World Data -> Physics.World.World Data
addBoxes ( x, y, z ) world =
    world
        |> Physics.World.add (box (Point3d.meters (15 + x) (-15 + y) (-0.5 + z)))
        |> Physics.World.add (box (Point3d.meters (15 + x) (-16.5 + y) (-0.5 + z)))
        |> Physics.World.add (box (Point3d.meters (15 + x) (-18 + y) (-0.5 + z)))
        |> Physics.World.add (box (Point3d.meters (15 + x) (-16 + y) (0.5 + z)))
        |> Physics.World.add (box (Point3d.meters (15 + x) (-17.5 + y) (0.5 + z)))
        |> Physics.World.add (box (Point3d.meters (15 + x) (-16.5 + y) (1.5 + z)))


initialWorld : Physics.World.World Data
initialWorld =
    Physics.World.empty
        |> Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> Physics.World.add floor
        |> Physics.World.add slope1
        |> Physics.World.add slope2
        |> Physics.World.add wall1
        |> Physics.World.add wall2
        |> Physics.World.add wall3
        |> addBoxes ( 0, 0, 0 )
        |> addBoxes ( -30, 10, 0 )
        |> addBoxes ( -30, 10, 0 )
        |> Physics.World.add (largeBox (Point3d.meters 0 -38 10))
        |> Physics.World.add (largeBox (Point3d.meters 0 -35 12))
        |> Physics.World.add (largeBox (Point3d.meters 0 -32 14))
        |> Physics.World.add (mediumBox (Point3d.meters 5 10 10))
        |> Physics.World.add (Physics.Body.moveTo (Point3d.meters 0 0 5) base)


simulateCar :
    Duration.Duration
    -> Model
    -> List Wheel
    -> Physics.Body.Body Data
    -> Physics.Body.Body Data
simulateCar dt { world, steering, braking, speeding } wheels car =
    case wheels of
        [ w1, w2, w3, w4 ] ->
            let
                engineForce =
                    Force.newtons (500 * speeding)

                brake =
                    if braking then
                        Force.newtons 1000

                    else
                        Quantity.zero

                wheel1 =
                    { w1 | steering = Angle.degrees (30 * steering), engineForce = engineForce, brake = brake }

                wheel2 =
                    { w2 | steering = Angle.degrees (30 * steering), engineForce = engineForce, brake = brake }

                wheel3 =
                    { w3 | engineForce = engineForce, brake = brake }

                wheel4 =
                    { w4 | engineForce = engineForce, brake = brake }
            in
            updateSuspension dt world (Physics.Body.frame car) car [ wheel1, wheel2, wheel3, wheel4 ] car [] 0

        _ ->
            car


updateSuspension :
    Duration.Duration
    -> Physics.World.World Data
    -> Frame3d.Frame3d Length.Meters Physics.Coordinates.WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
    -> Physics.Body.Body Data
    -> List Wheel
    -> Physics.Body.Body Data
    -> List Wheel
    -> Int
    -> Physics.Body.Body Data
updateSuspension dt world frame originalCar currentWheels updatedCar updatedWheels numWheelsOnGround =
    case currentWheels of
        [] ->
            updateFriction dt world frame updatedCar numWheelsOnGround updatedWheels [] [] False

        wheel :: remainingWheels ->
            let
                ray =
                    Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection
                        |> Axis3d.placeIn frame
            in
            case Physics.World.raycast ray (Physics.World.keepIf (\b -> (Physics.Body.data b).id == Obstacle) world) of
                Just { body, normal, point } ->
                    let
                        bodyFrame =
                            Physics.Body.frame body

                        contactPoint =
                            Point3d.placeIn bodyFrame point

                        contactNormal =
                            Direction3d.placeIn bodyFrame normal

                        distance =
                            Point3d.distanceFrom contactPoint (Axis3d.originPoint ray)

                        maxDistance =
                            Quantity.plus carSettings.suspensionRestLength carSettings.radius
                    in
                    if Quantity.lessThan maxDistance distance then
                        let
                            suspensionLength =
                                distance
                                    |> Quantity.minus carSettings.radius
                                    |> Quantity.clamp
                                        carSettings.minSuspensionLength
                                        carSettings.maxSuspensionLength

                            difference =
                                carSettings.suspensionRestLength
                                    |> Quantity.minus suspensionLength
                                    |> Length.inMeters

                            (Quantity.Quantity projectedVelocity) =
                                Vector3d.dot
                                    (Direction3d.toVector contactNormal)
                                    (Physics.Body.velocityAt contactPoint originalCar)

                            (Quantity.Quantity denominator) =
                                Vector3d.dot
                                    (Direction3d.toVector contactNormal)
                                    (Direction3d.toVector (Axis3d.direction ray))

                            ( suspensionRelativeVelocity, clippedInvContactDotSuspension ) =
                                if denominator >= -0.1 then
                                    ( 0, 1 / 0.1 )

                                else
                                    ( -projectedVelocity / denominator, -1 / denominator )

                            damping =
                                if suspensionRelativeVelocity < 0 then
                                    carSettings.dampingCompression

                                else
                                    carSettings.dampingRelaxation

                            suspensionImpulse =
                                ((carSettings.suspensionStiffness * difference * clippedInvContactDotSuspension)
                                    - (damping * suspensionRelativeVelocity)
                                )
                                    |> (*) (Physics.Body.mass originalCar |> Maybe.map Mass.inKilograms |> Maybe.withDefault 0)
                                    |> Force.newtons
                                    |> Quantity.clamp Quantity.zero carSettings.maxSuspensionForce
                                    |> Quantity.times dt
                        in
                        updateSuspension dt
                            world
                            frame
                            originalCar
                            remainingWheels
                            (Physics.Body.applyImpulse
                                suspensionImpulse
                                contactNormal
                                contactPoint
                                updatedCar
                            )
                            ({ wheel
                                | contact =
                                    Just
                                        { point = contactPoint
                                        , normal = contactNormal
                                        , body = body
                                        }
                                , suspensionLength = suspensionLength
                                , suspensionImpulse = suspensionImpulse
                             }
                                :: updatedWheels
                            )
                            (numWheelsOnGround + 1)

                    else
                        updateSuspension dt
                            world
                            frame
                            originalCar
                            remainingWheels
                            updatedCar
                            ({ wheel
                                | contact = Nothing
                                , suspensionLength = carSettings.suspensionRestLength
                             }
                                :: updatedWheels
                            )
                            numWheelsOnGround

                Nothing ->
                    updateSuspension dt
                        world
                        frame
                        originalCar
                        remainingWheels
                        updatedCar
                        ({ wheel
                            | contact = Nothing
                            , suspensionLength = carSettings.suspensionRestLength
                         }
                            :: updatedWheels
                        )
                        numWheelsOnGround


type alias WheelFriction =
    { forward : Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    , axle : Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    , sideImpulse : Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , forwardImpulse : Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , skidInfo : Float
    , contactPoint : Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
    , contactBody : Physics.Body.Body Data
    }


updateFriction :
    Duration.Duration
    -> Physics.World.World Data
    -> Frame3d.Frame3d Length.Meters Physics.Coordinates.WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
    -> Physics.Body.Body Data
    -> Int
    -> List Wheel
    -> List WheelFriction
    -> List Wheel
    -> Bool
    -> Physics.Body.Body Data
updateFriction dt world frame updatedCar numWheelsOnGround currentWheels wheelFrictions updatedWheels sliding =
    case currentWheels of
        [] ->
            applyImpulses dt world frame updatedCar updatedWheels sliding wheelFrictions

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal, body } ->
                    let
                        worldAxle =
                            carSettings.rightDirection
                                |> Direction3d.rotateAround (Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection) wheel.steering
                                |> Direction3d.placeIn frame

                        (Quantity.Quantity proj) =
                            Vector3d.dot (Direction3d.toVector normal) (Direction3d.toVector worldAxle)

                        axle =
                            Direction3d.toVector worldAxle
                                |> Vector3d.minus (Vector3d.scaleBy proj (Direction3d.toVector normal))
                                |> Vector3d.direction
                                |> Maybe.withDefault normal

                        forward =
                            Vector3d.cross (Direction3d.toVector normal) (Direction3d.toVector axle)
                                |> Vector3d.direction
                                |> Maybe.withDefault normal

                        sideImpulse =
                            resolveSingleBilateral updatedCar body point axle

                        maxImpulse =
                            if wheel.brake == Quantity.zero then
                                -- TODO: think about default rolling friction impulse
                                Quantity.zero

                            else
                                Quantity.times dt wheel.brake

                        forwardImpulse =
                            Quantity.times dt wheel.engineForce
                                |> Quantity.plus (calcRollingFriction updatedCar body point forward maxImpulse numWheelsOnGround)

                        -- Switch between active rolling (throttle), braking and non-active rolling friction (nthrottle/break)
                        maximpSide =
                            Quantity.multiplyBy carSettings.frictionSlip wheel.suspensionImpulse

                        impulseSquared =
                            Quantity.times forwardImpulse forwardImpulse
                                |> Quantity.multiplyBy 0.25
                                |> Quantity.plus (Quantity.times sideImpulse sideImpulse)

                        isSliding =
                            Quantity.lessThan (Quantity.times maximpSide maximpSide) impulseSquared

                        skidInfo =
                            if isSliding then
                                1

                            else
                                Quantity.ratio maximpSide (Quantity.sqrt impulseSquared)
                    in
                    updateFriction
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        ({ forward = forward
                         , axle = axle
                         , sideImpulse = sideImpulse
                         , forwardImpulse = forwardImpulse
                         , skidInfo = skidInfo
                         , contactPoint = point
                         , contactBody = body
                         }
                            :: wheelFrictions
                        )
                        (wheel :: updatedWheels)
                        (sliding || isSliding)

                Nothing ->
                    updateFriction
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        wheelFrictions
                        (wheel :: updatedWheels)
                        sliding


applyImpulses :
    Duration.Duration
    -> Physics.World.World Data
    -> Frame3d.Frame3d Length.Meters Physics.Coordinates.WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
    -> Physics.Body.Body Data
    -> List Wheel
    -> Bool
    -> List WheelFriction
    -> Physics.Body.Body Data
applyImpulses dt world frame car wheels sliding wheelFrictions =
    case wheelFrictions of
        [] ->
            rotateWheels dt frame car wheels []

        friction :: remainingFrictions ->
            let
                centerOfMass =
                    Physics.Body.centerOfMass car
                        |> Point3d.placeIn frame

                up =
                    Direction3d.reverse carSettings.downDirection
                        |> Direction3d.placeIn frame

                verticalDistance =
                    Vector3d.from friction.contactPoint centerOfMass
                        |> Vector3d.componentIn up
                        |> Quantity.multiplyBy (1 - carSettings.rollInfluence)

                closerToCenterOfMass =
                    Point3d.translateIn up verticalDistance friction.contactPoint

                forwardImpulse =
                    if sliding then
                        Quantity.multiplyBy friction.skidInfo friction.forwardImpulse

                    else
                        friction.forwardImpulse

                sideImpulse =
                    if sliding then
                        Quantity.multiplyBy friction.skidInfo friction.sideImpulse

                    else
                        friction.sideImpulse

                newCar =
                    car
                        |> Physics.Body.applyImpulse forwardImpulse friction.forward friction.contactPoint
                        |> Physics.Body.applyImpulse sideImpulse friction.axle closerToCenterOfMass

                -- TODO: apply the reverse of the sideImpulse on the ground object too, for now assume it is static
            in
            applyImpulses
                dt
                world
                frame
                newCar
                wheels
                sliding
                remainingFrictions


rotateWheels :
    Duration.Duration
    -> Frame3d.Frame3d Length.Meters Physics.Coordinates.WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
    -> Physics.Body.Body Data
    -> List Wheel
    -> List Wheel
    -> Physics.Body.Body Data
rotateWheels dt frame car wheels updatedWheels =
    case wheels of
        [] ->
            Physics.Body.withData
                { id = Car (List.reverse updatedWheels)
                , meshes = (Physics.Body.data car).meshes
                , color = color.car
                }
                car

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal } ->
                    let
                        velocity =
                            Physics.Body.velocityAt point car

                        forward =
                            Direction3d.placeIn frame carSettings.forwardDirection

                        proj =
                            Direction3d.componentIn normal forward

                        (Quantity.Quantity proj2) =
                            forward
                                |> Direction3d.toVector
                                |> Vector3d.minus (Vector3d.withLength (Quantity.Quantity proj) normal)
                                |> Vector3d.dot velocity

                        deltaRotation =
                            Quantity.Quantity (proj2 * Duration.inSeconds dt / Length.inMeters carSettings.radius)

                        newWheel =
                            { wheel
                                | deltaRotation = deltaRotation
                                , rotation = Quantity.plus wheel.rotation wheel.deltaRotation
                            }
                    in
                    rotateWheels dt frame car remainingWheels (newWheel :: updatedWheels)

                Nothing ->
                    let
                        deltaRotation =
                            Quantity.multiplyBy 0.99 wheel.deltaRotation

                        newWheel =
                            { wheel
                              -- damping when not in contact
                                | deltaRotation = Quantity.multiplyBy 0.99 wheel.deltaRotation
                                , rotation = Quantity.plus wheel.rotation deltaRotation
                            }
                    in
                    rotateWheels dt frame car remainingWheels (newWheel :: updatedWheels)


resolveSingleBilateral :
    Physics.Body.Body Data
    -> Physics.Body.Body Data
    -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
    -> Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    -> Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
resolveSingleBilateral body1 body2 point direction =
    let
        velocity1 =
            Physics.Body.velocityAt point body1

        velocity2 =
            Physics.Body.velocityAt point body2

        (Quantity.Quantity relativeVelocity) =
            Vector3d.dot (Vector3d.minus velocity2 velocity1) (Direction3d.toVector direction)

        contactDamping =
            0.2

        invMass1 =
            case Physics.Body.mass body1 of
                Just mass ->
                    1 / Mass.inKilograms mass

                Nothing ->
                    0

        invMass2 =
            case Physics.Body.mass body2 of
                Just mass ->
                    1 / Mass.inKilograms mass

                Nothing ->
                    0

        massTerm =
            1 / (invMass1 + invMass2)
    in
    Quantity.Quantity (-contactDamping * relativeVelocity * massTerm)


calcRollingFriction :
    Physics.Body.Body Data
    -> Physics.Body.Body Data
    -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
    -> Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    -> Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    -> Int
    -> Quantity.Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
calcRollingFriction body1 body2 point forward maxImpulse numWheelsOnGround =
    let
        velocity1 =
            Physics.Body.velocityAt point body1

        velocity2 =
            Physics.Body.velocityAt point body2

        (Quantity.Quantity relativeVelocity) =
            Vector3d.dot (Vector3d.minus velocity2 velocity1) (Direction3d.toVector forward)

        denom1 =
            computeImpulseDenominator body1 point forward

        denom2 =
            computeImpulseDenominator body2 point forward
    in
    Quantity.Quantity (-relativeVelocity / (denom1 + denom2) / toFloat numWheelsOnGround)
        |> Quantity.clamp (Quantity.negate maxImpulse) maxImpulse


computeImpulseDenominator :
    Physics.Body.Body Data
    -> Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates
    -> Direction3d.Direction3d Physics.Coordinates.WorldCoordinates
    -> Float
computeImpulseDenominator body point normal =
    let
        position =
            Point3d.placeIn (Physics.Body.frame body) (Physics.Body.centerOfMass body)

        r0 =
            Vector3d.from position point

        c0 =
            Vector3d.cross r0 (Direction3d.toVector normal)

        vec =
            Vector3d.cross (Physics.Body.inertia body c0) r0

        (Quantity.Quantity dot) =
            Vector3d.dot (Direction3d.toVector normal) vec
    in
    case Physics.Body.mass body of
        Just mass ->
            1 / Mass.inKilograms mass + dot

        Nothing ->
            dot


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Physics.Body.Body Data
floor =
    Physics.Body.plane
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles []
        , color = { red = 1, green = 1, blue = 1 }
        }
        |> Physics.Body.moveTo (Point3d.fromMeters floorOffset)


{-| A slope to give a car the initial push.
-}
slope1 : Physics.Body.Body Data
slope1 =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.01
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.slope
        }
        |> Physics.Body.rotateAround Axis3d.x (Angle.radians (pi / 16))
        |> Physics.Body.moveTo (Point3d.meters 3 -6 0.6)


slope2 : Physics.Body.Body Data
slope2 =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 10
                , Length.meters 0.01
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.slope
        }
        |> Physics.Body.rotateAround Axis3d.z (Angle.radians pi)
        |> Physics.Body.rotateAround Axis3d.x (Angle.radians (pi / 10))
        |> Physics.Body.moveTo (Point3d.meters 3 -25 0.6)


wall1 : Physics.Body.Body Data
wall1 =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 35
                , Length.meters 0.2
                , Length.meters 1
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.wall
        }
        |> Physics.Body.rotateAround Axis3d.z (Angle.radians 0.7)
        |> Physics.Body.moveTo (Point3d.meters -13 9 -0.5)


wall2 : Physics.Body.Body Data
wall2 =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 70
                , Length.meters 0.2
                , Length.meters 1
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.wall
        }
        |> Physics.Body.rotateAround Axis3d.z (Angle.radians -0.65)
        |> Physics.Body.moveTo (Point3d.meters 30 -1 -0.5)


wall3 : Physics.Body.Body Data
wall3 =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 70
                , Length.meters 0.2
                , Length.meters 1
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.wall
        }
        |> Physics.Body.rotateAround Axis3d.z (Angle.radians -1.1)
        |> Physics.Body.moveTo (Point3d.meters -11 -35 -0.5)


box : Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates -> Physics.Body.Body Data
box position =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 1
                , Length.meters 1
                , Length.meters 1
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.box
        }
        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 10))
        |> Physics.Body.moveTo position


largeBox : Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates -> Physics.Body.Body Data
largeBox position =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 1
                , Length.meters 10
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.box
        }
        |> Physics.Body.rotateAround Axis3d.z (Angle.radians (pi / 14))
        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 50))
        |> Physics.Body.moveTo position


mediumBox : Point3d.Point3d Length.Meters Physics.Coordinates.WorldCoordinates -> Physics.Body.Body Data
mediumBox position =
    let
        block3d =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 3
                , Length.meters 3
                , Length.meters 3
                )
    in
    Physics.Body.block
        block3d
        { id = Obstacle
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block3d)
        , color = color.box
        }
        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 50))
        |> Physics.Body.moveTo position


wheelShape : Sphere3d.Sphere3d Length.Meters Physics.Coordinates.BodyCoordinates
wheelShape =
    Sphere3d.atOrigin carSettings.radius


wheelMesh : Common.Meshes.Meshes
wheelMesh =
    Common.Meshes.fromTriangles (Common.Meshes.sphere 2 wheelShape)


base : Physics.Body.Body Data
base =
    let
        block =
            Block3d.centeredOn
                Frame3d.atOrigin
                ( Length.meters 4, Length.meters 2, Length.meters 0.2 )

        wheels =
            [ { defaultWheel | chassisConnectionPoint = Point3d.meters 1 1 0.2 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters 1 -1 0.2 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters -1 1 0.2 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters -1 -1 0.2 }
            ]

        -- We don't want the car to stuck when bumping into the obstacles
        slippy =
            Material.custom { friction = -1, bounciness = 0.6 }
    in
    Physics.Body.block
        block
        { id = Car wheels
        , meshes = Common.Meshes.fromTriangles (Common.Meshes.block block)
        , color = color.car
        }
        |> Physics.Body.rotateAround Axis3d.z (Angle.radians (pi / -2))
        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 150))
        |> Physics.Body.withMaterial slippy
