.FLlearnr_envir <- new.env(parent = emptyenv())

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
  if (is.null(question_ids)) {
    assign("question_ids", FL_get_question_ids(), envir = p)
  } else {
    assign("question_ids", question_ids, envir = p)
  }
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
      "submission hash and submit it using the form below:"
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


#' @title Get access to the internal environment
#' @keywords internal
FL_env_get <- function() {
  pkg_envir <- parent.env(environment())
  envir <- get0(".FLlearnr_envir", envir = pkg_envir)
  if (!is.environment(envir)) {
    stop("Something went wrong: cannot find internal .FLlearnr_envir environment.")
  }
  envir
}

#' @export
#' @rdname FL_learnr_methods
FL_add_question <- function(question_ids) {
  env <- FL_env_get()
  env$.FLlearnr_question_ids <-
    c(env$.FLlearnr_question_ids, question_ids)
  invisible(env$.FLlearnr_question_ids)
}

#' @export
#' @rdname FL_learnr_methods
FL_get_question_ids <- function() {
  env <- FL_env_get()
  env$.FLlearnr_question_ids
}

# Documentation would clash with base .onLoad documentation
# @title Initialise log storage and global options
# @param libname a character string giving the library directory where the
#   package defining the namespace was found.
# @param pkgname a character string giving the name of the package.
# @aliases namespace_hooks
# @keywords internal
# @rdname namespace_hooks
.onLoad <- function(libname, pkgname) {
  env <- FL_env_get()
  assign(".FLlearnr_question_ids", character(0), envir = env)
}









#' Grade alternatives
#' @param valid_answers A vector of valid answers
#' @export
grade_alternatives <- function(valid_answers) {
  msg <- character(0)
  if (!(is.numeric(.result) &&
        (.result %in% valid_answers))) {
    if (is.character(valid_answers[1])) {
      msg <- c(msg, "Please enter either ",
               paste0('"',
                      valid_answers[-length(valid_answers)],
                      '"',
                      collapse = ", "),
               " or ",
               '"',
               valid_answers[length(valid_answers)],
               '"')
    } else {
      msg <- c(msg, "Please enter either ",
               paste0(valid_answers[-length(valid_answers)], collapse = ", "),
               " or ", valid_answers[length(valid_answers)])
    }
  }
  if (length(msg) == 0) {
    graded(correct = TRUE, message = "Partial check ok! (Answer has valid syntax.)")
  } else {
    graded(correct = FALSE, message = paste0(msg, collapse = ". "))
  }
}
