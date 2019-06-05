---
title: "My Second Post"
date: 2019-05-28T14:17:54+02:00
---

buttons package, 


## Package structure

`To make our code maintainable we divide our big functionalities into packages with structure model, operations, helper functions and rarely layout function.`

~~~
-- Forms.elm
type alias FormsModel =
    { fieldErrors : List String
    }

initialFormsModel_ =
    { fieldErrors = []
    }

type FormsOperation =
    MaybeAddError String

type alias Update a msg =
    msg -> ModelWihForms a -> ( ModelWihForms a, Cmd msg )

-- Page model where forms package is included ex 
-- (type alias Login.Model = {
    formsModel_ : FormsModel
})
type alias ModelWihForms a =
    { a | formsModel_ : FormsModel }

updateFormsSubmodel_ : ModelWihForms a -> FormsModel -> ModelWihForms a
updateFormsSubmodel_ model newModel =
    { model | formsModel_ = newModel }

initFormOperations_ : Update a msg -> (FormsOperation -> msg) -> ModelWihForms a -> FormsOperation -> ( ModelWihForms a, Cmd msg )
initFormOperations_ update wrapper model operation =
    let
        formsModel =
            model.formsModel_
    in
        case operation of
            MaybeAddError fieldData ->
                (updateFormsSubmodel_ model { formsModel | fieldErrors = error :: formsModel.fieldErrors }, Cmd.none )
~~~

`Something like this gives us option to use same functionality on multiple pages without much hustle. We just include package in some page.`

~~~
import Forms as Forms
-- Page.Login
type alias Model =
    { formsModel_ : Forms.FormsModel
    }

init : ( Model, Cmd Msg )
init =
    ( { formsModel_ = Forms.initialFormsModel_
    }
    , Cmd.none
    )

type Msg =
    NoOp
    | FormsOperations Forms.FormsOperation

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FormsOperations op ->
            Forms.initFormOperations_ update (\a -> FormsOperations a) model op
~~~

## Package | component

`This kind of packages would go into the same category as exsisting elm libraries, you get some functionality without worrying about undelaying structure. You have exposed msgs and helper functions which do certain things without worrying about how it's done, you just care about end result. Second important thing is that package should have clear cut and well defiend functionality. For example when we talk about forms we almost 
uniformly know what we need in every single project we use it in. Also we shouldn't include layout in packages most of the times beacuse how resuable is it really? In my experience resuability of layout without changing things is rare. If you need something like that we would call that component, simmilar to package, but more specific to the project and less clear cut.(Elaborate on example).`

## Why this? Elaborate why we choose this approach

Usually we have three things in every project:
    1. some common functionality which we use in multiple projects, that would be the thing we call package.(form validation, time...)
    2. parts of single project which we reuse in that project - we would call that components (header, notifications...)
    3. we have pages

## Dependency

`Packages with model and msg generally shouldn't depend on each other beacuse then they become less clear cut and more complex then they should. What package should form validation depend on? Two packages should merge in components or pages. Thats the point when our code gets more complicated. Components shouldn't depend on pages beacuse we want to maintain one way flow of package -> components -> page. Elm thought me that one way flow of app is although sometimes more bolierplated in most cases much easier to debug and maintain, so we are trying to implement that mindset in any part of app we can.
`

`Question is why take such a rigid approach. Well looser approach is usually takes less time to code and you can take shortcuts, but main goal for us is to make that trade off for easier maintaince of our codebase. With that said rigid approach trades easier coding for easier debuging which is consistent with other architectural choices we make.`



## End

`With model-msg packages we get decent chunk of functionality without worrying about model, msg or any aspect of how it is implemented, we just call msgs or functions from that module. We get just enough to help us a lot, but not restrict us in any way if we take the approach we were talking about.`
