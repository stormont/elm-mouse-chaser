
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window


-- MODEL

type alias MouseX = Int
type alias MouseY = Int
type alias Point = (MouseX, MouseY)  -- Mouse position (x,y).
type alias Model = List Point        -- A list of points.


-- UPDATE

update : Point -> Model -> Model
update p model =
  let
    numPoints = 12              -- Number of points to retain.
    points = p :: model         -- Prepend the latest Point (AKA, "cons").
  in
    List.take numPoints points  -- Update the path (limiting path length).


-- VIEW

lineStyle : LineStyle
lineStyle =
  { defaultLine          -- Extend the "default" definition.
      | width <- 10.0    -- Line width.
      , color <- blue    -- Assign the color.
      , cap   <- Round   -- The shape of the end points.
      , join  <- Smooth  -- The shape of the joints.
  }


drawLineSegments : (Int,Int) -> List Point -> Form
drawLineSegments (w,h) points =
  List.map (\(x,y) -> (toFloat x, toFloat -y)) points  -- Convert the mouse points to
                                                       -- "model" coordinates.
    |> path                                      -- Build a path from the points.
    |> traced lineStyle                          -- Trace the line with defined form.
    |> move (-(toFloat w) / 2, (toFloat h) / 2)  -- Move drawing from middle to upper
                                                 -- left ("screen" coordinates).

view : (Int,Int) -> Model -> Element
view (w,h) model =
  collage w h [ drawLineSegments (w,h) model ]  -- Draw the path.


-- SIGNALS

mousePositions : Signal Model
mousePositions =
  Signal.foldp      -- Fold each signal into an accumulated model...
    update          -- ... through the update function.
    []              -- Start with an empty list.
    Mouse.position  -- Updates given by mouse position changes.


main : Signal Element
main =
  Signal.map2          -- Map two signals together...
    view               -- ... through the view function.
    Window.dimensions  -- Use updates to the window dimensions as the first signal.
    mousePositions     -- Use updates to mouse positions as the second signal.
