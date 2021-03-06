randomProject <- function(
	folder = 'data', 
	given = "sample", 
    dob = "01/01/1990", 
	seed = NULL, 
	nedges = 'max'
) 
{
	top = 100
	width = 100
	step = 5
	cores = 15
	nrows <- length(seq(1, 365.25*top-width, by = step)) + 1
	#
	if (!is.null(seed)) {
		set.seed(seed)
		given <- sample(dnaRt::names, 1)
		dob <- as.Date("01/01/1990", "%d/%m/%Y") + sample(1:1825, 1)
	}
	dob <- as.Date(dob, "%d/%m/%Y")
	y <- as.numeric(substr(dob, 1, 4))
    d <- substr(dob, 6, 10)
	path <- glue::glue("{folder}/{y}-{d}_{given}")
    if (!dir.exists('data')) { dir.create('data') }
    if (!dir.exists(path)) { dir.create(path) }
	#
	msg_note(glue::glue("Initiating project..."))
	project <- list(
		"folder" = folder, 
		"project_path" = path, 
		"yob" = y, 
		"dob" = dob, 
		"given" = given, 
		"top" = top, 
		"step" = step,
		"width" = width, 
		"genome_seq" = NA, 
		"cores" = cores, 
		"seqs_path" = NA, 
		"mat_path" = NA, 
		"graph_path" = NA,
		"layout_path" = NA, 
		"plotdf_path" = glue::glue("{path}/plotdf.rds")
	)
	#
	SEED <- digest::digest2int(glue::glue(
		"{project$given}{project$yob}{project$dob}"
	))
	#
	msg_note(glue::glue("Simulating graph..."))
	set.seed(SEED) 
	edges <- dnaRt::sample_edges
	if (nedges == 'max') nedges <- nrow(edges)
	edges <- edges %>% 
		dplyr::mutate(
			to = sample(to),
			from = sample(from)
		) %>% 
		dplyr::filter(weight > 35) %>% 
		dplyr::top_n(nedges, wt = weight)
	nodes <- data.frame(
		idx = paste0('idx_', 1:nrows),
		numidx = 1:nrows, 
		year = round(seq(0, floor(nrows/365.25*step), length.out = nrows))
	) %>% 
		dplyr::filter(as.numeric(gsub('idx_', '', idx)) %in% c(edges$to, edges$from))
	graph <- tidygraph::tbl_graph(
		nodes = nodes, 
		edges = edges,
		directed = FALSE
	)
	#
	msg_note(glue::glue("Computing layout of simulated graph..."))
	set.seed(SEED)
	lay <- graphlayouts::layout_with_sparse_stress(graph, pivots = 100) %>% 
		data.frame(., nodes$idx, nodes$year) %>% 
		setNames(c('x', 'y', 'idx', 'year'))
	msg_note(glue::glue("Adding plotting features..."))
	set.seed(SEED)
	plotdf <- lay %>% 
		dplyr::select(idx, x, y, year) %>%
		dplyr::mutate(
			x_ = (x - min(x))/(max(x) - min(x)), 
			y_ = (y - min(y))/(max(y) - min(y)), 
			pertube = ambient::gen_simplex(x, y, frequency = 5) / 5,
			noise = ambient::gen_worley(x, y, value = 'distance', frequency = 5) / 5, 
			x = x_ - noise, 
			y = y_ - noise
		)
	#
	an <- sample(seq(0, 2*pi, length.out = 100), 1)
    plotdf[, c('x', 'y')] <- rotateXY(plotdf[, c('x', 'y')], angle = an)
	#
	msg_success(glue::glue("Successfully simulated graph for {given}, {dob}!"))
    msg_note(glue::glue("Folder: {folder}"))
    msg_note(glue::glue("D.O.B.: {dob}"))
    msg_note(glue::glue("Given name: {given}"))
	project[["data"]] <- plotdf
	saveRDS(plotdf, project$plotdf_path)
	return(project)
}