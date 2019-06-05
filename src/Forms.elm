module Forms exposing (FormsOperation(..), FormsModel, validateEmpty, validateEmail, initFormOperations_, initialFormsModel_, updateFormsSubmodel_, isFormPassed, isFieldErrorState, fieldNameErrorClassList, getFieldFromForm, validateFormInModel, validateZeroUuid, validateEmailNonEmpty)
{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.
    # Definition
    @docs FormsModel
    
    # Definition
    @docs validateEmpty
    
    # Definition
    @docs validateEmail
    
    # Definition
    @docs initFormOperations_
    
    # Definition
    @docs FormsOperation(..)
    
    # Definition
    @docs initialFormsModel_
    
    # Definition
    @docs updateFormsSubmodel_
    
    # Definition
    @docs isFormPassed
    
    # Definition
    @docs isFieldErrorState
    
    # Definition
    @docs fieldNameErrorClassList
    
    # Definition
    @docs getFieldFromForm
    
    # Definition
    @docs validateFormInModel
    
    # Definition
    @docs validateZeroUuid
    
    
    # Definition
    @docs validateEmailNonEmpty
-}

import List.Extra
import Maybe.Extra
import Update.Extra as UE
import Html
import Html.Attributes
import Uuid
import Regex





-- zeroUuid : Uuid.Uuid
-- zeroUuid =
--     case Uuid.fromString "00000000-0000-4000-8000-000000000000" of
--         Nothing ->
--             Uuid.Uuid "00000000-0000-4000-8000-000000000000"
            

--         Just u ->
--             u

-- FormsModel

{-|
    lala
-}

type alias FormsModel a =
    { fieldErrors : List a
    , serverErrors : List (List a)
    , isFormSubmittedFirstTime : Bool
    , focusedField : Maybe a
    }


-- initialFormsModel_

{-|
    lala
-}

initialFormsModel_ : FormsModel a
initialFormsModel_ =
    { fieldErrors = []
    , serverErrors = []
    , isFormSubmittedFirstTime = False
    , focusedField = Nothing
    }


-- FormsOperation

{-|
    lala
-}

type FormsOperation a
    = MaybeAddError { fieldName : a, fieldValue : String, fieldValidator : String -> Bool }
    | AddErrorWithoutValidation a
    | RemoveErrorWithoutValidation a
    | ValidateForm (List { fieldName : a, fieldValue : String, fieldValidator : String -> Bool })
    | SetIsFormSubmittedFirstTime Bool
    | SetFocusedField (Maybe a)
    | AddServerError (List a)
    | RemoveServerError a
    | ClearServerErrors


-- Update

{-|
    lala
-}

type alias Update a b msg =
    msg -> ModelWihForms a b -> ( ModelWihForms a b, Cmd msg )


-- ModelWihForms

{-|
    lala
-}

type alias ModelWihForms a b =
    { a | formsModel_ : FormsModel b }


-- updateFormsSubmodel_

{-|
    lala
-}

updateFormsSubmodel_ : ModelWihForms a b -> FormsModel b -> ModelWihForms a b
updateFormsSubmodel_ model newModel =
    { model | formsModel_ = newModel }


-- maybeAddError

{-|
    lala
-}

maybeAddError : FormsModel a -> { fieldName : a, fieldValue : String, fieldValidator : String -> Bool } -> FormsModel a
maybeAddError formsModel fieldData =
    let
        isError =
            fieldData.fieldValidator fieldData.fieldValue

        isAlreadyInErrors =
            List.member fieldData.fieldName formsModel.fieldErrors
    in
        if isError then
            if isAlreadyInErrors then
                formsModel
            else
                { formsModel | fieldErrors = fieldData.fieldName :: formsModel.fieldErrors }
        else
            { formsModel | fieldErrors = List.filter (\a -> a /= fieldData.fieldName) formsModel.fieldErrors }


-- initFormOperations_

{-|
    lala
-}

initFormOperations_ : Update a b msg -> (FormsOperation b -> msg) -> ModelWihForms a b -> FormsOperation b -> ( ModelWihForms a b, Cmd msg )
initFormOperations_ update wrapper model operation =
    let
        formsModel =
            model.formsModel_
    in
        case operation of
            MaybeAddError fieldData ->
                (if formsModel.isFormSubmittedFirstTime then
                    ( updateFormsSubmodel_ model (maybeAddError formsModel fieldData), Cmd.none )
                 else
                    ( model, Cmd.none )
                )
                    |> UE.andThen update (wrapper (RemoveServerError fieldData.fieldName))

            AddErrorWithoutValidation fieldName ->
                (if formsModel.isFormSubmittedFirstTime then
                    ( updateFormsSubmodel_ model { formsModel | fieldErrors = fieldName :: formsModel.fieldErrors }, Cmd.none )
                 else
                    ( model, Cmd.none )
                )
                    |> UE.andThen update (wrapper (RemoveServerError fieldName))

            RemoveErrorWithoutValidation fieldName ->
                if formsModel.isFormSubmittedFirstTime then
                    ( updateFormsSubmodel_ model { formsModel | fieldErrors = List.Extra.filterNot (\field -> field == fieldName) formsModel.fieldErrors }, Cmd.none )
                else
                    ( model, Cmd.none )

            ValidateForm data ->
                ( updateFormsSubmodel_ model
                    { formsModel
                        | fieldErrors =
                            List.foldl
                                (\first second ->
                                    if List.member first second then
                                        second
                                    else
                                        first :: second
                                )
                                []
                                ((validateFormFields data) ++ formsModel.fieldErrors)
                    }
                , Cmd.none
                )

            SetIsFormSubmittedFirstTime bool ->
                ( updateFormsSubmodel_ model { formsModel | isFormSubmittedFirstTime = bool }, Cmd.none )

            SetFocusedField maybeField ->
                ( updateFormsSubmodel_ model { formsModel | focusedField = maybeField }, Cmd.none )

            AddServerError errors ->
                ( updateFormsSubmodel_ model { formsModel | serverErrors = errors :: formsModel.serverErrors }, Cmd.none )

            RemoveServerError error ->
                ( updateFormsSubmodel_ model { formsModel | serverErrors = List.filter (\b -> not (List.member error b)) formsModel.serverErrors }, Cmd.none )

            ClearServerErrors ->
                ( updateFormsSubmodel_ model { formsModel | serverErrors = [] }, Cmd.none )


-- validateFormFields

{-|
    lala
-}

validateFormFields : List { fieldName : a, fieldValue : String, fieldValidator : String -> Bool } -> List a
validateFormFields data =
    Maybe.Extra.values
        (List.map
            (\fieldData ->
                if fieldData.fieldValidator fieldData.fieldValue then
                    Just fieldData.fieldName
                else
                    Nothing
            )
            data
        )


-- isFormPassed

{-|
    lala
-}

isFormPassed : FormsModel a -> List { fieldName : a, fieldValue : String, fieldValidator : String -> Bool } -> Bool
isFormPassed model form =
    let
        fieldNames =
            List.map (\a -> a.fieldName) form
    in
        List.all (\b -> b == False) (List.map (\a -> List.member a model.fieldErrors) fieldNames)


-- validEmail

{-|
    lala
-}

validEmail : Regex.Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


-- validateEmail

{-|
    lala
-}

validateEmail : String -> Bool
validateEmail val =
    if (Regex.contains validEmail (String.trim val)) then
        False
    else
        True


-- validateEmailNonEmpty

{-|
    lala
-}


validateEmailNonEmpty : String -> Bool
validateEmailNonEmpty val =
    if String.trim val == "" then
        False
    else if (Regex.contains validEmail (String.trim val)) then
        False
    else
        True


-- validateEmpty

{-|
    lala
-}


validateEmpty : String -> Bool
validateEmpty str =
    String.trim str == ""


-- validateZeroUuid

{-|
    lala
-}

validateZeroUuid : String -> Bool
validateZeroUuid str =
    str == ""


-- isFieldErrorState

{-|
    lala
-}

isFieldErrorState : List a -> List (List a) -> a -> Bool
isFieldErrorState errors serverErrors fieldName =
    List.member fieldName errors || List.member fieldName (List.concat serverErrors)

-- fieldNameErrorClassList

{-|
    lala
-}

fieldNameErrorClassList : List a -> List (List a) -> a -> Html.Attribute msg
fieldNameErrorClassList errors serverErrors fieldName =
    Html.Attributes.classList [ ( "error-state", isFieldErrorState errors serverErrors fieldName ) ]


-- getFieldFromForm

{-|
    lala
-}

getFieldFromForm : List { fieldName : a, fieldValue : String, fieldValidator : String -> Bool } -> a -> Maybe { fieldName : a, fieldValue : String, fieldValidator : String -> Bool }
getFieldFromForm list field =
    List.head (List.filter (\fieldInner -> fieldInner.fieldName == field) list)


-- validateFormInModel

{-|
    lala
-}

validateFormInModel : ModelWihForms a b -> FormsModel b -> List { fieldName : b, fieldValue : String, fieldValidator : String -> Bool } -> ModelWihForms a b
validateFormInModel model formsModel data =
    updateFormsSubmodel_ model
        { formsModel
            | fieldErrors =
                List.foldl
                    (\first second ->
                        if List.member first second then
                            second
                        else
                            first :: second
                    )
                    []
                    ((validateFormFields data) ++ formsModel.fieldErrors)
        }
