randomProject <- function(
	folder = 'data', 
	given = "sample", 
    dob = "01/01/1990", 
	seed = NULL
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
	d <- as.Date(dob, "%d/%m/%Y")
	y <- as.numeric(substr(d, 1, 4))
	path <- glue::glue("{folder}/{y}-{d}_{given}")
	dir.create(path)
	#
	msg_note(glue::glue("Initiating project..."))
	project <- list(
		"folder" = folder, 
		"project_path" = path, 
		"yob" = y, 
		"dob" = d, 
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
	msg_note(glue::glue("Simulating graph..."))
	set.seed(glue::glue(digest::digest2int("{project$yob}{project$dob}")))
	nodes <- data.frame(
		idx = paste0('idx_', 1:nrows),
		numidx = 1:nrows, 
		year = round(seq(0, floor(nrows/365.25*step), length.out = nrows))
	)
	x <- data(dnaRt::sample_edges)
	sample_edges$to <- sample(sample_edges$to)
	sample_edges$from <- sample(sample_edges$from)
	sample_edges <- sample_edges %>% 
		dplyr::filter(weight > 35)
	graph <- tidygraph::tbl_graph(
		nodes = nodes, 
		edges = sample_edges,
		directed = FALSE
	)
	#
	msg_note(glue::glue("Computing layout of simulated graph..."))
	lay <- ggraph::create_layout(graph, 'auto', weights = NA)
	# lay <- ggraph::create_layout(graph, 'stress')
	# plot(lay$x, lay$y)
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
	#
	msg_success(glue::glue("Successfully simulated graph for {given}, {dob}!"))
	project[["data"]] <- plotdf
	return(project)
}