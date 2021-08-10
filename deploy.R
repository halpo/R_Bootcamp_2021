library(tidyverse)

git <- function (..., echo_cmd = TRUE, echo = TRUE, error_on_status = TRUE) {
    invisible(
        processx::run( "git"
                     , c(...)
                     , echo_cmd = echo_cmd
                     , echo = echo
                     , error_on_status = error_on_status)
    )
}

git_has_remote_branch <- function (remote, branch) {
    git("ls-remote", "--quiet",
        "--exit-code", remote, branch, echo = FALSE, echo_cmd = FALSE,
        error_on_status = FALSE)$status == 0
}
git_current_branch <- function () {
    branch <- git("rev-parse", "--abbrev-ref", "HEAD",
        echo = FALSE, echo_cmd = FALSE)$stdout
    sub("\n$", "", branch)
}
github_worktree_add <- function (dir, remote, branch) {
    cli::cat_rule("Adding worktree", line = 1)
    git("worktree", "add", "--track", "-B",
        branch, dir, paste0(remote, "/", branch))
}
github_worktree_remove <- function (dir) {
    cli::cat_rule("Removing worktree", line = 1)
    git("worktree", "remove", dir)
}

render_site <-
function( input = "."
        , output_format = "all"
        , envir = parent.frame()
        , quiet = FALSE
        , encoding = "UTF-8"
        ) {
    original_input <- input
    input <- input_as_dir(input)
    input_file <- NULL
    if (!dir_exists(original_input)) {
        input_file <- original_input
        if (output_format == "all")
            output_format <- NULL
    }
    generator <- site_generator(input, output_format)
    if (is.null(generator))
        stop("No site generator found.")
    generator$render( input_file = input_file
                    , output_format = output_format
                    , envir = envir
                    , quiet = quiet)
    if (!dir_exists(original_input))
        output <- file_with_ext(basename(original_input), "html")
    else output <- "index.html"
    output <- file.path(input, generator$output_dir, output)
    output <- normalized_relative_to(input, output)
    invisible(output)
}
github_push <-
function( dir = '.'
        , commit_message = "Committing page"
        , remote='origin'
        , branch='gh-pages'
        ) {
    force(commit_message)
    cli::cat_rule("Commiting updated site", line = 1)
    withr::with_dir(dir, {
        git("add", "-A", ".")
        git("commit", "--allow-empty", "-m",
            commit_message)
        cli::cat_rule("Deploying to GitHub Pages", line = 1)
        git("remote", "-v")
        git("push", "--force", remote, paste0("HEAD:",
            branch))
    })
}

needs_render <- function(in.dir = '.', out.dir = '_site'){

    out.file.info <- list.files(out.dir, "*.html", full.names=TRUE)  %>%
        file.info() %>% as_tibble(rownames='out.file') %>%
        mutate(base = out.file %>% basename() %>% xfun::sans_ext())

    in.file.info <- list.files(in.dir, "*.Rmd", full.names=TRUE) %>%
        file.info() %>% as_tibble(rownames='in.file') %>%
        mutate(base = in.file %>% basename() %>% xfun::sans_ext())


    full_join(in.file.info, out.file.info, 'base',
              suffix = c('_in', '_out')) %>%
        filter( mtime_in> mtime_out
              | is.na(mtime_out)
              ) %>% pull(in.file) %>%
        basename
}

update_site_files <- function(){
    render <- needs_render()
    map(render, rmarkdown::render, output_dir = '_site')
}

deploy <-
function( dir = ".", render = NA
        , commit_message = "Deployed"
        , branch = "gh-pages"
        , remote = "origin"
        , github_pages = (branch == "gh-pages")
        , ...)
{
    dest_dir <- fs::dir_create(fs::file_temp())
    on.exit(fs::dir_delete(dest_dir))
    if (!git_has_remote_branch(remote, branch)) {
        old_branch <- git_current_branch()
        git("checkout", "--orphan", branch)
        git("rm", "-rf", "--quiet", ".")
        git("commit", "--allow-empty", "-m",
            sprintf("Initializing %s branch", branch))
        git("push", remote, paste0("HEAD:", branch))
        git("checkout", old_branch)
    }
    git("remote", "set-branches", remote, branch)
    git("fetch", remote, branch)
    github_worktree_add(dest_dir, remote, branch)
    on.exit(github_worktree_remove(dest_dir), add = TRUE)

    # write_lines(paste("output_dir:",shQuote(gsub("/", "\\\\\\\\", fs::path_norm(dest_dir)))), "_site.yml", append=TRUE)
    if(is.na(render)){
        render <- needs_render()
        map(render, rmarkdown::render, output_dir = '_site')
    }
    # if (is.character(render))
    if (isTRUE(render))
        rmarkdown::render_site(dir)
    file.copy(list.files("_site", full.names=TRUE), dest_dir, recursive=TRUE, overwrite=TRUE)

    # git("checkout", "_site.yml")

    # pkg <- as_pkgdown(pkg, override = list(destination = dest_dir))
    # build_site(pkg, devel = FALSE, preview = FALSE, install = FALSE, ...)
    # if (github_pages) {
    #     build_github_pages(pkg)
    # }
    github_push(dest_dir, commit_message, remote, branch)
    invisible()
}
