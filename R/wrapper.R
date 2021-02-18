#' dnart
#'
#' @export

dnart <- function(
    dob = "20/06/1992", 
    given = "jacques", 
    top = 4, 
    folder = 'data', 
    step = 5, 
    width = 100, 
    force = FALSE, 
    genome_seq = NULL, 
    cores = 1
) {
    `%>%` <- tidyr::`%>%`
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