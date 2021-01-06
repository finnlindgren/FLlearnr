#' Custom learnr methods
#'
#' @param question_ids character vector
#'
#' @importFrom rlang .data
#' @export
#' @rdname FL_learnr_methods
FL_autodecoder_logic <- function(question_ids = NULL) {
  p <- parent.frame()
  learnrhash:::check_server_context(p)
  assign("question_ids", question_ids, envir = p)
  local(
    {
      my_encoded_txt <- shiny::eventReactive(
        input$hash_generate,
        {
          objs <- learnr:::get_all_state_objects(session)
          objs <- learnr:::submissions_from_state_objects(objs)
          if (strip_output) {
            objs <- purrr::map(objs, function(x) {
              if ((x$type == "exercise_submission") &&
                  !is.null(x$data$output)) {
                x$data$output <- list()
              }
              x
            })
          }
          learnrhash::encode_obj(objs)
        }
      )
      shiny::observeEvent(input$hash_generate, {
        d <- tibble::tibble(hash = my_encoded_txt())
        qu <- try(learnrhash::extract_questions(d, .data$hash), silent = TRUE)
        ex <- try(learnrhash::extract_exercises(d, .data$hash), silent = TRUE)

        labels <- names(shiny::reactiveValuesToList(input))
        idx <- grepl("tutorial-exercise-", labels)
        exercise_ids <-
          sub(
            pattern = "-code-editor",
            replacement = "",
            x = sub(
              pattern = "tutorial-exercise-",
              replacement = "",
              x = labels[idx]
            )
          )

        answered_question_ids <- qu$question_id
        answered_exercise_ids <- ex$exercise_id

        qu_answered <- paste0(
          "Answered questions: ",
          paste0(answered_question_ids, collapse = ", ")
        )
        qu_unanswered <- paste0(
          if (is.null(question_ids)) {
            NULL
          } else {
            paste0(
              "Unanswered questions: ",
              paste0(setdiff(question_ids, answered_question_ids), collapse = ", ")
            )
          }
        )
        ex_answered <- paste0(
          "Answered exercises: ",
          paste0(answered_exercise_ids, collapse = ", ")
        )
        ex_unanswered <- paste0(
          "Unanswered exercises: ",
          paste0(setdiff(exercise_ids, answered_exercise_ids), collapse = ", ")
        )
        the_text <- paste0(c(qu_answered, ex_answered, qu_unanswered, ex_unanswered), collapse = "\n")

        attributes(the_text) <- NULL
        output$autodecode_summary <- shiny::renderText(the_text)
      })
    },
    envir = p
  )
}
#' @export
#' @rdname FL_learnr_methods
FL_autodecoder_ui <- function() {
  learnrhash:::check_not_server_context(parent.frame())
  shiny::tags$div(
    shiny::tags$h4("Summary of answers:"),
    learnrhash:::wrapped_verbatim_text_output("autodecode_summary")
  )
}

#' @param url character
#' @param add_summary logical
#' @param before logical; If `TRUE`, generates the ui to place before the hash
#' generator. Otherwise generates the submission form ui, placed after the
#' generator.
#'
#' @export
#' @rdname FL_learnr_methods
FL_ui <- function(url, add_summary = TRUE, before = TRUE) {
  if (before) {
    shiny::div(
      "If you have completed this assignment and are happy with all of your",
      "answers and solutions, please click the button below to generate your",
      "hash and submit it using the form below:"
    )
  } else {
    shiny::div(
      if (add_summary) {
        FL_autodecoder_ui()
      } else {
        NULL
      },
      shiny::h4("Submission form:"),
      "If the submission form isn't visible on your system, use the following link instead:",
      shiny::tags$br(),
      shiny::tags$a(url, href = url, target = "_blank"),
      shiny::tags$br(),
      learnrhash::iframe_ui(
        paste0(url, "&embed=true"),
        width = "640px", height = "600px",
        frameborder = "0",
        marginwidth = "0",
        marginheight = "0",
        style = "border: none; max-width:100%; max-height:100vh",
        allowfullscreen = "true",
        webkitallowfullscreen = "true",
        mozallowfullscreen = "true",
        msallowfullscreen = "true"
      )
    )
  }
}
#' @export
#' @rdname FL_learnr_methods
FL_encoder_ui <- function(url, add_summary = TRUE) {
  learnrhash::encoder_ui(
    ui_before = FL_ui(url, add_summary = add_summary, TRUE),
    ui_after = FL_ui(url, add_summary = add_summary, FALSE)
  )
}


