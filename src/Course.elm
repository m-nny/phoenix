module Course exposing (Course, CourseInfo, courseListDecoder, emptyCourse)

import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Extra exposing (withDefault)


type alias CourseInfo =
    { abbriviature : String
    , title : String
    , credits : Int
    , school : String
    , department : String
    , description : String
    }


type alias Course =
    { id : Int
    , info : CourseInfo
    }


emptyCourse : Course
emptyCourse =
    Course 0 (CourseInfo "" "" 0 "" "" "")


courseInfoDecoder : Decoder CourseInfo
courseInfoDecoder =
    Decode.map6 CourseInfo
        (field "abbriviature" string)
        (field "title" string)
        (field "credits" int |> withDefault 0)
        (field "school" string)
        (field "department" string)
        (field "description" string |> withDefault "")


courseDecoder : Decoder Course
courseDecoder =
    Decode.map2 Course
        (field "id" int)
        (field "info" courseInfoDecoder)


courseListDecoder : Decoder (List Course)
courseListDecoder =
    list courseDecoder
