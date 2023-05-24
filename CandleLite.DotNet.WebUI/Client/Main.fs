module CandleLite.DotNet.WebUI.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.JSInterop

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Play
    | [<EndPoint "/about">] About
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/data">] Data

/// The Elmish application's model.
type Model =
    {
        page: Page

        // Play page
        request: FhirRequest
        responses: FhirResponse list
        loading: bool

        counter: int
        books: Book[] option
        error: string option
        username: string
        password: string
        signedInAs: option<string>
        signInFailed: bool
    }
and FhirRequest =
    {
        method: string
        path: string
        body: string
    }
and FhirResponse =
    {
        serverName: string
        status: int
        headers: string list
        body: string
    }

and Book =
    {
        title: string
        author: string
        publishDate: DateTime
        isbn: string
    }

let initModel =
    {
        page = Play
        request = { method = "GET"; path = "Patient"; body = "" }
        responses = []
        loading = false
        counter = 0
        books = None
        error = None
        username = ""
        password = ""
        signedInAs = None
        signInFailed = false
    }

/// Remote service definition.
type BookService =
    {
        sendRequest: FhirRequest -> Async<FhirResponse list>
    }

    interface IRemoteService with
        member this.BasePath = "/books"

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    // Play page
    | SetFhirMethod of string
    | SetFhirPath of string
    | SetFhirRequestBody of string
    | SendRequest
    | ReceiveResponse of FhirResponse list
    // Bolero demo
    | Increment
    | Decrement
    | SetCounter of int
    | GetBooks
    | GotBooks of Book[]
    | SetUsername of string
    | SetPassword of string
    | GetSignedInAs
    | RecvSignedInAs of option<string>
    | SendSignIn
    | RecvSignIn of option<string>
    | SendSignOut
    | RecvSignOut
    // Error
    | Error of exn
    | ClearError

let update  (js: IJSRuntime) remote message model =
    let onSignIn = function
        | Some _ -> Cmd.ofMsg GetBooks
        | None -> Cmd.none
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    // Play page
    | SetFhirMethod method ->
        { model with request = { model.request with method = method } }, Cmd.none
    | SetFhirPath path ->
        { model with request = { model.request with path = path } }, Cmd.none
    | SetFhirRequestBody body ->
        { model with request = { model.request with body = body } }, Cmd.none
    | SendRequest ->
        let request = model.request
        let cmd = Cmd.OfAsync.either remote.sendRequest request ReceiveResponse Error
        { model with loading = true}, cmd
    | ReceiveResponse responses ->
        { model with responses = responses; loading = false }, Cmd.none

    // Bolero demo

    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none

    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."; signedInAs = None }, Cmd.none

    | Error exn ->
        { model with error = Some exn.Message; loading = false }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let aboutPage model dispatch =
    Main.About().Elt()

let playPage model dispatch =
    Main.Play()
        .Send(fun _ -> dispatch SendRequest)
        .FhirMethod(model.request.method, fun str -> dispatch (SetFhirMethod str))
        .FhirPath(model.request.path, fun str -> dispatch (SetFhirPath str))
        .FhirBodyField(cond (model.request.method = "POST") <| function
            | true ->
                Main.FhirBodyFieldControl()
                    .FhirBody(model.request.body, fun str -> dispatch (SetFhirRequestBody str))
                    .Elt()
            | false -> empty())
        .ResultsField(cond (model.responses.Length > 0) <| function
            | true ->
                Main.ResultsFieldTemplate()
                    .Results(forEach model.responses <| fun response ->
                        Main.ResultTemplate()
                            .ResultBody(
                                elt "prism-highlight" {
                                    "lang" => "json"
                                    "code" => response.body
                                }
                            )
                            .Elt())
                    .Elt()
            | false -> empty())
        .Elt()

let counterPage model dispatch =
    Main.Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, fun v -> dispatch (SetCounter v))
        .Elt()

let dataPage model (username: string) dispatch =
    Main.Data()
        .Reload(fun _ -> dispatch GetBooks)
        .Username(username)
        .SignOut(fun _ -> dispatch SendSignOut)
        .Rows(cond model.books <| function
            | None ->
                Main.EmptyData().Elt()
            | Some books ->
                forEach books <| fun book ->
                    tr {
                        td { book.title }
                        td { book.author }
                        td { book.publishDate.ToString("yyyy-MM-dd") }
                        td { book.isbn }
                    })
        .Elt()

let signInPage model dispatch =
    Main.SignIn()
        .Username(model.username, fun s -> dispatch (SetUsername s))
        .Password(model.password, fun s -> dispatch (SetPassword s))
        .SignIn(fun _ -> dispatch SendSignIn)
        .ErrorNotification(
            cond model.signInFailed <| function
            | false -> empty()
            | true ->
                Main.ErrorNotification()
                    .HideClass("is-hidden")
                    .Text("Sign in failed. Use any username and the password \"password\".")
                    .Elt()
        )
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat {
            menuItem model Play "Play"
            menuItem model Counter "Counter"
            menuItem model Data "Download data"
            menuItem model About "About"
        })
        .Body(
            cond model.page <| function
            | Play -> playPage model dispatch
            | About -> aboutPage model dispatch
            | Counter -> counterPage model dispatch
            | Data ->
                cond model.signedInAs <| function
                | Some username -> dataPage model username dispatch
                | None -> signInPage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =

        let bookService = this.Remote<BookService>()
        let update = update this.JSRuntime bookService
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
        |> Program.withConsoleTrace
#endif
