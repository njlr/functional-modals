module App

open Fable
open Fable.Import

let canvas =
  Browser.document.getElementById "root"
  :?> Browser.HTMLCanvasElement

type Input =
| KeyPress of int

type State =
  {
    HandleInput : Input -> State;
    Update : float -> State;
    Render : Browser.CanvasRenderingContext2D -> Unit;
  }

let rec acknowledge message belowState =
  {
    HandleInput = (fun e ->
      match e with
      | KeyPress 0 -> belowState
      | _ -> acknowledge message belowState
    )
    Update = (fun _ -> acknowledge message belowState)
    Render = (fun ctx ->
      ctx.save ()

      belowState.Render ctx

      ctx.restore ()

      ctx.fillStyle <- Core.U3.Case1 "rgba(0, 0, 0, 0.5)"
      ctx.fillRect (0.0, 0.0, canvas.width, canvas.height)

      ctx.fillStyle <- Core.U3.Case1 "white"
      ctx.font <- "normal 32px sans-serif"
      ctx.textAlign <- "center"
      ctx.textBaseline <- "middle"

      let (x, y) = canvas.width / 2.0, canvas.height / 2.0

      ctx.fillText (message, x, y)
    )
  }

let rec timerState time =
  {
    HandleInput = (fun e ->
      match e with
      | KeyPress 0 -> timerState time |> acknowledge "Paused"
      | _ -> timerState time
    )
    Update = ((*) 0.001) >> ((+) time) >> timerState
    Render = (fun ctx ->
      ctx.fillStyle <- Core.U3.Case1 "purple"
      ctx.fillRect (0.0, 0.0, canvas.width, canvas.height)

      ctx.fillStyle <- Core.U3.Case1 "white"
      ctx.font <- "normal 32px sans-serif"
      ctx.textAlign <- "center"
      ctx.textBaseline <- "middle"
      ctx.fillText (time |> int |> string, canvas.width / 2.0, canvas.height / 2.0)

      ()
    )
  }

let ctx = canvas.getContext_2d ()

let mutable inputEvents = []

Browser.window.addEventListener_keypress (fun e ->
  inputEvents <- List.Cons (KeyPress <| int e.keyCode, inputEvents)
)

let rec loop state timestamp =
  Browser.window.requestAnimationFrame (fun nextTimestamp ->
    let dt = nextTimestamp - timestamp

    state.Render ctx

    let nextState =
      inputEvents
      |> List.fold (fun s n -> s.HandleInput n) (state.Update dt)

    inputEvents <- []

    loop nextState nextTimestamp
  )
  |> ignore

let initialState =
  timerState 0.0
  |> acknowledge "Press space to pause the timer. "

loop initialState 0.0
