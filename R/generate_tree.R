#'@title Generate Tree
#'
#'@description Generates a procedural tree with
#'
#'@param x Default `0`. Either the x-coordinate, or if a length-3 vector the x,y, and z coordinates of the base of the tree.
#'@param y Default `NULL`. The y-coordinate of the base of the tree. Ignored if the `x` is a length-3 vector.
#'@param z Default `NULL`. The z-coordinate of the base of the tree. Ignored if the `x` is a length-3 vector.
#'@param seed Default `2`. Random seed for generating the tree.
#'@param midpoint Default `TRUE`. Method of extending branches. If `FALSE`, it grows directly to the
#'next node. Else, it first extends a midpoint given the previous orientation
#'and grows from there to the end point.
#'@param branch_depth Default `6`. Number of branch splits to end tree.
#'@param branch_scale Default `c(0.8,0.9)`.
#'@param branch_angle Default `c(-30, 30)`. Horizontal branching angle from previous branch.
#'@param branch_angle_vert Default `seq(-45,45, by=5)`. Vertical branching angle from previous branch.
#'@param branch_split Default `2`.
#'@param branch_prune_prob Default `0`.
#'@param branch_color Default `#603000`.
#'@param branch_radius_shrink Default `15`. Constant that determines the rate the radius shrinks. Higher
#'values result in less shrinking.
#'@param leaf_color Default `NULL`.
#'@param leaf_depth_start Default `NULL`, automatically set
#'@param leaf_size Default `0.01`.
#'@param leaf_prob Default `1`.
#'@param scale Default `1`. Uniformly scale the tree.
#'
#'@importFrom magrittr %>%
#'
#'@export
#'@import rayrender
#'@examples
#'\donttest{
#'library(rayrender)
#'generate_tree(seed=1) %>%
#'  render_tree()
#'
#'#Change the branch angle choices
#'generate_tree(seed=1,branch_angle_vert = c(-15,15)) %>%
#'  render_tree()
#'
#'#Change the horizontal branch angle choices
#'generate_tree(seed=6,branch_angle = seq(-90,90,by=10)) %>%
#'  render_tree()
#'
#'#Increase the number of layers
#'generate_tree(seed=1,branch_depth = 8) %>%
#'  render_tree()
#'
#'#Have the leaves start appearing at branch 6 to fill in the tree
#'generate_tree(seed=1,branch_depth = 8, leaf_depth_start = 6) %>%
#'  render_tree()
#'
#'#Change the color and seed to get a different structure
#'generate_tree(seed=2,branch_depth = 6, leaf_depth_start = 4, leaf_color="pink") %>%
#'  render_tree()
#'
#'#Shorten the branches at each junction by random values
#'generate_tree(seed=2,branch_depth = 6, leaf_depth_start = 4, leaf_color="pink",
#'              branch_scale = c(0.5,0.6)) %>%
#'  render_tree()
#'
#'
#'#Lengthen the branches at each junction by random values (this results in a wild tree)
#'generate_tree(seed=2,branch_depth = 6, leaf_depth_start = 4, leaf_color="red",
#'              branch_scale = c(1.1,1.2)) %>%
#'  render_tree()
#'
#'#All angles one sign make the tree lean over, and here we double the size of the leaf
#'generate_tree(seed=2,branch_depth = 6,
#'              leaf_color="purple", leaf_size=0.4,
#'              branch_angle_vert = c(15,5)) %>%
#'  render_tree()
#'
#'#Include a random chance to not grow branches
#'generate_tree(seed=4,branch_depth = 8, leaf_depth_start = 6, leaf_color="red",
#'              branch_prune_prob = 0.5) %>%
#'  render_tree()
#'}
generate_tree = function(x = 0, y = NULL, z = NULL,
                         seed = 2000, midpoint = TRUE,
                         branch_depth  = 6,
                         branch_scale  = c(0.8, 0.9),
                         branch_angle  = c(-30, 30),
                         branch_angle_vert  = seq(-45,45, by=5),
                         branch_split  = 2,
                         branch_prune_prob  = 0,
                         branch_color  = "#603000",
                         branch_radius_shrink = 15,
                         leaf_color = "green",
                         leaf_depth_start = NULL,
                         leaf_size  = 0.2,
                         leaf_prob  = 1,
                         scale = 1) {
  if(length(x) == 3) {
    z1 = x[3]
    y1 = x[2]
    x1 = x[1]
  }  else {
    x1 = x[1]
  }
  if(is.null(y) && length(x) != 3) {
    y1 = 0
  } else if (!is.null(y)) {
    y1 = y
  }
  if(is.null(z) && length(x) != 3) {
    z1 = 0
  } else if (!is.null(z)) {
    z1 = z
  }
  if(is.null(branch_angle_vert)) {
    branch_angle_vert = branch_angle
  }
  id_path <- NULL
  treeval = grow_bonsai(seed = seed, time=branch_depth, scale = branch_scale,
                        split=branch_split, prune = branch_prune_prob, angle = branch_angle,
                        angle_z = branch_angle_vert, shrink_rate = branch_radius_shrink)
  max_path = max(treeval$id_path)
  max_time = max(treeval$id_time)
  if(is.null(leaf_depth_start)) {
    leaf_depth_start = max_time
  }
  treeval[,1:3] = treeval[,1:3] * scale
  treeval$seg_wid = treeval$seg_wid * scale
  leaf_size = leaf_size*scale
  treelist = list()
  counter = 1
  for(i in 1:max_path) {
    branch = dplyr::filter(treeval, id_path == i)
    temp_val = 1
    if(midpoint) {
      treelist[[counter]] = segment(start=as.numeric(branch[1,1:3]) + c(x1,y1,z1),
                                    end = as.numeric(branch[2,1:3]) + c(x1,y1,z1),
                                    radius = as.numeric(branch$seg_wid[1])/10,
                                    material = diffuse(color=branch_color, sigma=90))
      counter = counter + 1
      treelist[[counter]] = sphere(x=as.numeric(branch[2,1]) + x1,
                                   y=as.numeric(branch[2,2]) + y1,
                                   z=as.numeric(branch[2,3]) + z1, radius = as.numeric(branch$seg_wid[2])/10,
                                   material = diffuse(color=branch_color, sigma=90))
      counter = counter + 1
      temp_val = 2
    }
    treelist[[counter]] = segment(start=as.numeric(branch[temp_val,1:3])  + c(x1,y1,z1),
                                  end = as.numeric(branch[3,1:3])  + c(x1,y1,z1),
                                  radius = as.numeric(branch$seg_wid[1])/10,
                                  material = diffuse(color=branch_color, sigma=90))
    counter = counter + 1
    treelist[[counter]] = sphere(x=as.numeric(branch[3,1]) + x1,
                                 y=as.numeric(branch[3,2]) + y1,
                                 z=as.numeric(branch[3,3]) + z1, radius = as.numeric(branch$seg_wid[3])/10,
                                 material = diffuse(color=branch_color, sigma=90))
    counter = counter + 1
    if((as.numeric(branch$id_time[1]) >= leaf_depth_start && stats::runif(1) < leaf_prob) && !is.null(leaf_color)) {
      branchend = as.numeric(branch[3,1:3])
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
      treelist[[counter]] = triangle(v1 = branchend + c(x1,y1,z1),
                                     v2 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     v3 = branchend + leaf_size*c(stats::runif(3)-0.5) + c(x1,y1,z1),
                                     material=diffuse(color=leaf_color))
      counter = counter + 1
    }
  }
  tree_scene = do.call(rbind,treelist)
  tree_scene
}
