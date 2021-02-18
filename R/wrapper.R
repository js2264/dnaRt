#' dnart
#'
#' @export

dnart <- function(
    dob = "", 
    given = "", 
    top = 100, 
    folder = 'data', 
    step = 5, 
    width = 100, 
    force = FALSE, 
    genome_seq = NULL, 
    cores = 1, 
    data = NULL
) {
    `%>%` <- tidyr::`%>%`
    # ---- All steps if is.null(data)
    if (is.null(data)) {
        project <- initiateProject(
            dob = dob,
            given = given,
            top = top,
            folder = folder,
            step = step,
            width = width, 
            genome_seq = genome_seq,
            cores = cores
        ) %>% 
            getSequences(force = force) %>%
            getPairwiseAlnScores(force = force) %>% 
            getGraph(force = force) %>% 
            getLayout(force = force) %>% 
            getPlottingData(force = force) 
        return(project)
    } 
    else {
        project <- initiateProject(
            dob = dob,
            given = given,
            top = top,
            folder = folder,
            step = step,
            width = width, 
            genome_seq = genome_seq,
            cores = cores
        )
        project$seqs_path <- NA
        project$mat_path <- NA
        project$graph_path <- NA
        project$layout_path <- NA
        project$plotdf_path <- data
        if (is.data.frame(data)) {
            project$data <- data
        }
        else if (tools::file_ext(data) == 'rds') {
            project$data <- readRDS(data)
        }
        return(project)
    }
}
