#' Plot output of `get_nns_ratio()`
#'
#' A way of visualizing the top nearest neighbors of a pair of ALC embeddings that captures
#' how "discriminant" each feature is of each embedding (group).
#'
#' @param x output of get_nns_ratio
#' @param alpha (numerical) betwee 0 and 1. Significance threshold to identify significant values.
#' These are denoted by a `*` on the plot.
#' @param horizontal (logical) defines the type of plot. if TRUE results are plotted on 1 dimension.
#' If FALSE, results are plotted on 2 dimensions, with the second dimension catpuring the ranking
#' of cosine ratio similarties.
#'
#' @return a `ggplot-class` object.
#'
#' @export
#' @rdname plot_nns_ratio
#' @keywords plot_nns_ratio
#' @examples
#'
#' library(ggplot2)
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigration", window = 6L)
#'
#' # sample 100 instances of the target term, stratifying by party (only for example purposes)
#' set.seed(2022L)
#' immig_toks <- tokens_sample(immig_toks, size = 100, by = docvars(immig_toks, 'party'))
#'
#' # we limit candidates to features in our corpus
#' feats <- featnames(dfm(immig_toks))
#'
#' # compute ratio
#' set.seed(2022L)
#' immig_nns_ratio <- get_nns_ratio(x = immig_toks,
#'                                  N = 10,
#'                                  groups = docvars(immig_toks, 'party'),
#'                                  numerator = "R",
#'                                  candidates = feats,
#'                                  pre_trained = cr_glove_subset,
#'                                  transform = TRUE,
#'                                  transform_matrix = cr_transform,
#'                                  bootstrap = TRUE,
#'                                  # num_bootstraps should be at least 100,
#'                                  # we use 10 here due to CRAN-imposed constraints
#'                                  # on example execution time
#'                                  num_bootstraps = 10,
#'                                  permute = TRUE,
#'                                  num_permutations = 10,
#'                                  verbose = FALSE)
#'
#' plot_nns_ratio(x = immig_nns_ratio, alpha = 0.01, horizontal = TRUE)
plot_nns_ratio <- function(x, alpha = 0.01, horizontal = TRUE){

  # warning
  if(nrow(x) > 20) message("Consider setting plotting fewer values (<= 20), the plot may otherwise look unreadable.")

  # group labels
  group1_label <- attributes(x)$numerator
  group2_label <- setdiff(unique(x$group), c(group1_label, "shared"))

  # order features by value and compute absolute deviation
  x <- x %>%
    dplyr::mutate(absdev = abs(1 - value)) %>%
    dplyr::arrange(-absdev) %>%
    dplyr::mutate(featureID = 1:nrow(.))

  # add significance stars if available
  if("p.value" %in% colnames(x)){ x <- x %>%
    dplyr::mutate(significant = dplyr::if_else(p.value < alpha, 'yes', 'no')) %>%
    dplyr::mutate(feature = dplyr::if_else(significant == 'yes', paste0(feature, "*"), feature))}



  # change factor order
  x$group = factor(x$group, levels = c(group2_label,'shared', group1_label))

  # select type of plot
  if(horizontal){

    # add jitter to values
    x$valueJitter <- jitter(x$value, amount = 0.5)
    x <- x %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(valueJitter = value + seq(0.2, 0.2*nrow(.), 0.2))

    # horizontal (1D) plot
    ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = valueJitter, y = c(0), color = group, shape = group),
                 data = x, size = 3) +
      ggplot2::scale_color_manual(values=c("black", "gray30", "gray60")) +
      ggplot2::geom_text(ggplot2::aes(x = valueJitter, y = c(0), label=feature),
                data = x, hjust = dplyr::if_else(x$group == "shared", -0.25, 1.2),
                vjust = 0.5, size = 4, angle = 90) +
      #annotate(geom = "text", x = c(min(x$valueJitter) + 1, max(x$valueJitter) - 1), y = c(1,-1), label=c(paste0("more ", group2_label), paste0("more ", group1_label)), size = 6) +
      ggplot2::xlab(paste0("cosine similarity ratio ", "(", group1_label, "/", group2_label, ")")) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
            legend.text=ggplot2::element_text(size=14),
            legend.title=ggplot2::element_blank(),
            legend.key=ggplot2::element_blank(),
            legend.position = "top",
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_text(size=16, margin = ggplot2::margin(t = 15, r = 0, b = 15, l = 0)),
            axis.ticks.y = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.spacing.x = ggplot2::unit(0.25, 'cm'),
            plot.margin=ggplot2::unit(c(1,1,0,0),"cm"))}else{

              # vertical (2D) plot
              ggplot2::ggplot() +
                ggplot2::geom_point(ggplot2::aes(x = value, y = featureID, color = group, shape = group),
                           data = x, size = 2) +
                ggplot2::geom_vline(xintercept = 1, colour = 'black', linetype = "dashed", size = 0.5) +
                ggplot2::geom_text(ggplot2::aes(x = value, y = featureID, label=feature), data = x,
                          hjust = dplyr::if_else(x$value>1, -0.2, 1.2), vjust = 0.25, size = 5) +
                #annotate(geom = "text", x = c(0.5,1.5), y = c(nrow(x) + 8,nrow(x) + 8),
                #         label=c(paste0("more ", group2_label), paste0("more ", group1_label)), size = 6) +
                ggplot2::scale_color_manual(values=c("black", "gray30", "gray60")) +
                ggplot2::xlim(min(x$value) - 0.5, max(x$value) + 0.5) +
                #ylim(0,(nrow(x) + )) +
                ggplot2::ylab('') +
                ggplot2::xlab(paste0("cosine similarity ratio ", "(", group1_label, "/", group2_label, ")")) +
                ggplot2::theme(panel.background = ggplot2::element_blank(),
                      plot.title = ggplot2::element_text(size=18, hjust = 0.5, color = 'blue'),
                      axis.text.x = ggplot2::element_text(size=16),
                      axis.text.y = ggplot2::element_text(size=16),
                      axis.title.y = ggplot2::element_text(size=16, margin = ggplot2::margin(t = 0, r = 15, b = 0, l = 15)),
                      axis.title.x = ggplot2::element_text(size=16, margin = ggplot2::margin(t = 15, r = 0, b = 15, l = 0)),
                      legend.text=ggplot2::element_text(size=16),
                      legend.title=ggplot2::element_blank(),
                      legend.key=ggplot2::element_blank(),
                      legend.position = "top",
                      legend.spacing.x = ggplot2::unit(0.25, 'cm'),
                      plot.margin=ggplot2::unit(c(1,1,0,0),"cm"))}

}
