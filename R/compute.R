#' initiateProject
#'
#' @param dob 
#' @param given 
#' @param folder 
#' @param top 
#' @param step 
#' @param width 
#' @param genome_seq 
#' @param cores 
#'
#' @return project
#'
#' @export

initiateProject <- function(dob, given, folder = 'data', top = 100, step = 10, width = 100, genome_seq = NULL, cores = 5) {
    dob <- as.Date(dob, "%d/%m/%Y")
    y <- as.numeric(substr(dob, 1, 4))
    d <- substr(dob, 6, 10)
    path <- glue::glue("{folder}/{y}-{d}_{given}")
    if (!dir.exists('data')) { dir.create('data') }
    if (!dir.exists(path)) { dir.create(path) }
    l <- list(
        "folder" = folder,
        "project_path" = path, 
        "yob" = y, 
        "dob" = dob, 
        "given" = given, 
        "top" = top, 
        "step" = step, 
        "width" = width, 
        "genome_seq" = genome_seq, 
        "cores" = cores 
    )
    msg_success("Project initiated")
    msg_note(glue::glue("Folder: {folder}"))
    msg_note(glue::glue("D.O.B.: {dob}"))
    msg_note(glue::glue("Given name: {given}"))
    return(l)
}

#' getSequences
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

getSequences <- function(project, force = FALSE) {
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    step <- project[["step"]]
    width <- project[["width"]]
    genome_seq <- project[["genome_seq"]]
    project_path <- project[["project_path"]]
    seqs_path <- glue::glue("{project_path}/seqs.rds")
    #
    if (!file.exists(seqs_path) | force) {
        if (is.null(genome_seq)) {
           msg_note(glue::glue("Importing genome sequence..."))
            # ------- Import genome sequence
            ah <- AnnotationHub::AnnotationHub()
            seq <- AnnotationHub::query(ah, 'Homo_sapiens.GRCh38.dna.primary_assembly.2bit')
            seq <- ah[['AH49723']]
            seq <- rtracklayer::import(seq)
            seq <- seq[GenomicRanges::width(seq) > 50000000]
        }
        else {
            seq = genome_seq
        }
        # ------- Define sequences specific to yob-dob
        msg_note(glue::glue("Extracting sequences corresponding to D.O.B: {dob}..."))
        set.seed(glue::glue(digest::digest2int("{yob}{dob}")))
        chr <- sample(names(seq), 1)
        loc <- sample(1:{width(seq[chr]) - 2000000}, 1)
        gr <- GRanges(seqnames = chr, ranges = IRanges(loc))
        gr <- resize(gr, fix = 'start', width = floor(365.25*top))
        gr <- slidingWindows(gr, step = step, width = width)
        gr <- gr[[1]]
        seqs <- seq[gr]
        msg_note(glue::glue("Saving sequences in {seqs_path}..."))
        saveRDS(seqs, seqs_path)
        msg_success(glue::glue("Sequences imported!"))
    } 
    else {
        msg_success(glue::glue("Sequences found in {seqs_path}"))
    }
    project[["seqs_path"]] <- seqs_path
    return(project)
}

#' getPairwiseAlnScores
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

getPairwiseAlnScores <- function(project, force = FALSE) {
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    cores <- project[["cores"]]
    project_path <- project[["project_path"]]
    mat_path <- glue::glue("{project_path}/mat.rds")
    #
    if (!file.exists(mat_path) | force) {
        seqs <- readRDS(project[["seqs_path"]])
        # ------- Get pairwise aln scores
        substitution_matrix <- Biostrings::nucleotideSubstitutionMatrix(
            match = 1, mismatch = 0, baseOnly = FALSE, 
            type = "DNA"
        )[c('A', 'T', 'C', 'G', 'N'), c('A', 'T', 'C', 'G', 'N')]
        msg_note(glue::glue("Computing alignment matrix..."))
        mat <- parallel::mclapply(mc.cores = cores, 1:floor(length(seqs)/top*top), function(K) {
            aln <- Biostrings::pairwiseAlignment(
                seqs[1:floor(length(seqs)/top*top)], 
                seqs[K], substitutionMatrix = substitution_matrix
            )
            Biostrings::score(aln)
        })
        mat <- do.call(rbind, mat)
        rownames(mat) <- paste0('idx_', 1:nrow(mat))
        colnames(mat) <- paste0('idx_', 1:nrow(mat))
        mat[lower.tri(mat)] <- NA
        msg_note(glue::glue("Saving alignment matrix in {mat_path}..."))
        saveRDS(mat, mat_path)
        msg_success(glue::glue("Alignment done!"))
    }
    else {
        msg_success(glue::glue("Alignment matrix found in {mat_path}"))
    }
    project[["mat_path"]] <- mat_path
    return(project)
}

#' getGraph
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

getGraph <- function(project, force = FALSE) {
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    step <- project[["step"]]
    project_path <- project[["project_path"]]
    graph_path <- glue::glue("{project_path}/graph.rds")
    #
    if (!file.exists(graph_path) | force) {
        mat <- readRDS(project[["mat_path"]])
        # ------- Get nodes / edges from mat
        `%>%` <- tidyr::`%>%`
        msg_note(glue::glue("Computing nodes..."))
        nodes <- data.frame(
            idx = paste0('idx_', 1:nrow(mat)),
            numidx = 1:nrow(mat), 
            year = round(seq(0, floor(nrow(mat)/365.25*step), length.out = nrow(mat)))
        )
        msg_note(glue::glue("Computing edges..."))
        edges <- mat %>% 
            as.data.frame() %>%
            tibble::rownames_to_column('from') %>% 
            tidyr::gather('to', 'weight', -from) %>% 
            dplyr::filter(!is.na(weight)) %>% 
            dplyr::filter(from != to) %>% 
            dplyr::filter(weight > 30)
        # ------- Turn it into a graph
        msg_note(glue::glue("Computing graph..."))
        set.seed(glue::glue(digest::digest2int("{yob}{dob}")))
        graph <- tidygraph::tbl_graph(
            nodes = nodes, 
            edges = edges,
            directed = FALSE
        )
        msg_note(glue::glue("Saving graph data in {graph_path}..."))
        saveRDS(graph, graph_path)
        msg_success(glue::glue("Graph data computed!"))
    }
    else {
        msg_success(glue::glue("Graph data found in {graph_path}"))
    }
    project[["graph_path"]] <- graph_path
    return(project)
}

#' getLayout
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

getLayout <- function(project, force = FALSE) {
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    project_path <- project[["project_path"]]
    layout_path <- glue::glue("{project_path}/layout.rds")
    #
    if (!file.exists(layout_path) | force) {
        msg_note(glue::glue("Computing layout data..."))
        graph <- readRDS(project[["graph_path"]])
        lay <- ggraph::create_layout(graph, 'auto', weights = NA)
        msg_note(glue::glue("Saving layout data in {layout_path}..."))
        saveRDS(lay, layout_path)
        msg_success(glue::glue("Layout data computed!"))
    }
    else {
        msg_success(glue::glue("Layout data found in {layout_path}"))
    }
    project[["layout_path"]] <- layout_path
    return(project)
}

#' getPlottingData
#'
#' @param project 
#' @param force 
#'
#' @return project
#'
#' @export

getPlottingData <- function(project, force = FALSE) {
    `%>%` <- tidyr::`%>%`
    yob <- project[["yob"]]
    dob <- project[["dob"]]
    given <- project[["given"]]
    folder <- project[["folder"]]
    top <- project[["top"]]
    project_path <- project[["project_path"]]
    plotdf_path <- glue::glue("{project_path}/plotdf.rds")
    #
    if (!file.exists(plotdf_path) | force) {
        lay <- readRDS(project[["layout_path"]])
        # ------- Add noise and coloring 
        msg_note(glue::glue("Computing plotting data..."))
        set.seed(digest::digest2int(given))
        plotdf <- lay %>% 
            dplyr::select(idx, x, y, year) %>%
            dplyr::mutate(
                x_ = (x - min(x))/(max(x) - min(x)), 
                y_ = (y - min(y))/(max(y) - min(y)), 
                pertube = ambient::gen_simplex(x, y, frequency = 5) / 25,
                noise = ambient::gen_worley(x, y, value = 'distance', frequency = 5) / 25, 
                x = x_ - noise, 
                y = y_ - noise
            )
        msg_note(glue::glue("Saving plotting data in {plotdf_path}..."))
        saveRDS(plotdf, plotdf_path)
        msg_success(glue::glue("Plotting data computed!"))
    }
    else {
        msg_success(glue::glue("Plotting data found in {plotdf_path}"))
    }
    project[["plotdf_path"]] <- plotdf_path
    plotdf <- readRDS(plotdf_path)
    project[["data"]] <- plotdf
    return(project)
}
