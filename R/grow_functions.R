#'@title Grow Sapling
#'
#'@description Initializes the tree: the very first shoot is the "sapling"
#'@importFrom magrittr %>%
#'
#'@keywords internal
grow_sapling = function() {
  sapling = tibble::tibble(
    x_0 = 0, y_0 = 0,   z_0 = 0, # first shoot starts at origin
    x_1 = 0, y_1 = 0.5, z_1 = 0, # first shoot guide is its midpoint
    x_2 = 0, y_2 = 1,   z_2 = 0, # first shoot grow to y = 1
    seg_deg  = 0,       # segment orientation is vertical
    seg_deg2 = 0,      # segment orientation is vertical
    seg_len  = 1,       # segment length is 1
    id_time  = 1L       # the acorn grows at "time 1"
  )
  return(sapling)
}

#'@title Grow Shoots
#'
#'@description For each existing shoot on the tree, grow an additional shoot that
#'extends it; then prune some of them away
#'
#'@param time Branch depth.
#'@param shoots Number of shoots.
#'@param param Parameters defining the tree.
#'@importFrom magrittr "%>%"
#'
#'@keywords internal
grow_shoots = function(time, shoots, param) {
  seg_deg <- seg_deg2 <- seg_len <- x <- x_0 <- x_2 <- y <- id_time <-
    y_0 <- y_2 <- z <- z_0 <- z_2 <- NULL

  n_shoots = nrow(shoots)
  n_pruned = stats::rbinom(n = 1, size = n_shoots - 1, prob = param$prune)

  ch_seg_len = sample(x = param$scale, size = n_shoots, replace = TRUE)
  ch_seg_deg = sample(x = param$angle, size = n_shoots, replace = TRUE)
  ch_seg_deg2 = sample(x = param$angle_z, size = n_shoots, replace = TRUE)
  shoots = shoots %>%
    dplyr::mutate(
      x_0 = x_2,
      y_0 = y_2,
      z_0 = z_2,
      seg_len = seg_len * ch_seg_len,
      x_1 = x_0 + extend_x(seg_len/2, seg_deg, seg_deg2),
      y_1 = y_0 + extend_y(seg_len/2, seg_deg, seg_deg2),
      z_1 = z_0 + extend_z(seg_len/2, seg_deg, seg_deg2),
      seg_deg = seg_deg + ch_seg_deg,
      seg_deg2 = seg_deg2 + ch_seg_deg2,
      id_time = id_time + 1L,
      x_2 = x_0 + extend_x(seg_len, seg_deg, seg_deg2) ,
      y_2 = y_0 + extend_y(seg_len, seg_deg, seg_deg2),
      z_2 = z_0 + extend_z(seg_len, seg_deg, seg_deg2)
    ) %>%
    dplyr::sample_n(size = n_shoots - n_pruned)

  return(shoots)
}


#'@title Grow Layer
#'
#'@description To grow a "layer" of the shrub, we extend (and possibly prune) each
#'existing shoot multiple times
#'
#'@param shoots Number of shoots.
#'@param time Branch depth.
#'@param param Params.
#'@importFrom magrittr %>%
#'
#'@keywords internal
grow_layer = function(shoots, time, param) {
  new_shoots = purrr::map_dfr(
    .x = 1:param$split,
    .f = grow_shoots,
    shoots = shoots,
    param = param
  )
  return(new_shoots)
}


#'@title Grow Tree
#'
#'@description To grow the whole tree we need to "accumulate" the growth: starting with
# the sapling (a single shoot) we grow the second layer; the set of shoots
# that make the second layer are then used to grow the third later; and so on
#'
#'@param sapling Existing sampling
#'@param param Parameters defining the tree.
#'
#'@keywords internal
grow_tree = function(sapling, param) {

  tree = purrr::accumulate(
    .x = 1:param$time,
    .f = grow_layer,
    .init = sapling,
    param = param
  )
  return(tree)
}

#'@title Grow Layer
#'
#'@description To grow a "layer" of the shrub, we extend (and possibly prune) each
#'existing shoot multiple times
#'
#'@param tree Tree to shape.
#'@importFrom magrittr "%>%"
#'
#'@keywords internal
shape_tree = function(tree, shrink_rate = 15) {
  axis <- coord <- coord_x <- coord_y <- coord_z <- id_path <- id_step <- id_time <-
    seg_deg <- seg_deg2 <- seg_len <- seg_wid <- x <- x_0 <- x_2 <- y <-
    y_0 <- y_2 <- z <- z_0 <- z_2 <- NULL
  tree = tree %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id_path = as.integer(1:dplyr::n())) %>%
    tidyr::pivot_longer(
      cols = x_0:z_2,
      names_to = "id_step",
      values_to = "coord"
    ) %>%
    tidyr::separate(col = id_step, into = c("axis", "id_step")) %>%
    tidyr::pivot_wider(names_from = axis, values_from = coord) %>%
    dplyr::mutate(
      id_step = as.integer(id_step),
      seg_wid = exp(-id_time^2 / shrink_rate)
    ) %>%
    dplyr::rename(coord_x = x, coord_y = y, coord_z = z) %>%
    dplyr::select(
      coord_x, coord_y, coord_z, seg_deg, seg_deg2, seg_len, seg_wid,
      id_time, id_path, id_step
    )
  return(tree)
}


#'@title Grow Bonsai
#'
#'@description Grow the entire tree.
#'
#'@param seed Existing sampling
#'@param time Total number of layers.
#'@param scale Possible values to scale the branch from layer to layer.
#'@param angle Possible
#'@param split Number of split branches per layer.
#'@param prune Probability of pruning a branch.
#'
#'@importFrom magrittr "%>%"
#'
#'@keywords internal
grow_bonsai = function(seed = 1987,
                       time = 6,
                       scale = c(.8, .9),
                       angle = c(-10, 10),
                       angle_z = c(-10, 10),
                       split = 2,
                       prune = 0,
                       shrink_rate = 15) {
  # parameters defining the tree
  param = list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    angle_z = angle_z,
    split = split,  # number of new shoots from each old shoot at each time
    prune = prune   # probability of immediately pruning a new shoot
  )

  # set the seed for the random number generator
  set.seed(param$seed)

  tree = grow_sapling() %>%
    grow_tree(param) %>%
    shape_tree(shrink_rate)

  return(tree)
}
