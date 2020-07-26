#' Render Tree
#'
#' Automatically plots the tree with a camera position and field of view that includes the full model.
#' For more control over the scene, pass the scene to `rayrender::render_scene()` and specify
#' the camera position manually. Note: spheres and cylinders in the scene are used to automatically
#' compute the field of view of the scene--adding additional sphere (e.g. with `rayrender::generate_ground()`)
#' will change this calculation. Use `rayrender::render_scene()` instead if this is a problem.
#'
#' @param scene Scene of tree model, to be passed to `rayrender`.
#' @param ground_radius Default `10`. Radius of the ground.
#' @param ground Default `TRUE`. Whether to add a grassy ground scene to the tree.
#' @param ground_color1 `darkgreen`. Primary ground color.
#' @param ground_color2 `lightgreen`. Secondary ground color.
#' @param lookfrom Default `NULL`. Camera position. Automatically calculated unless specified.
#' @param lookat Default `NULL`. Position camera is directed at. Automatically calculated unless specified.
#' @param fov Default `NULL`, automatically calculated. Camera field of view.
#' @param angle Default `c(0,0,0)`. Degrees to rotate the tree around the X, Y, and Z axes. If this
#' is a single number, it will be taken as the Y axis rotation.
#' @param order_rotation Default `c(1,2,3)`. What order to apply the rotations specified in `angle`.
#' @param lights Default `TRUE`. If `FALSE`, removes all default lights.
#' @param lightintensity Default `80`. Light intensity.
#' @param clamp_value Default `10`. Amount of clamp the light intensity. Finite values help reduce rendering
#' artifacts, set to `Inf` to turn off this feature.
#' @param width Default `600`. Width, in pixels, of the rendered image.
#' @param height Default `600`. Height, in pixels, of the rendered image.
#' @param ... Other arguments to pass to rayrender::render_scene()
#'
#' @return Rayrender scene.
#' @import rayrender
#' @export
#'
#' @examples
#' # Generate a basic scene with the default tree.
#' library(rayrender)
#'\donttest{
#' generate_tree() %>%
#'   render_tree()
#'
#' #Rotate the whole scene
#' generate_tree() %>%
#'   render_tree(angle = c(0,90,0))
#'
#' #Specify a custom camera position/direction/field of view/aperture
#' generate_tree() %>%
#'   render_tree(lookfrom = c(3, 0, 1), lookat = c(0,4,1), fov=30, aperture=1)
#'
#' #Change the ground color
#' generate_tree() %>%
#'   render_tree(ground_color1 = "brown", ground_color2 = "orange")
#'
#'#Turn off lights and add our own
#' generate_tree() %>%
#'   add_object(sphere(x=20,material=light(color="magenta",intensity=400))) %>%
#'   add_object(sphere(x=-20,material=light(color="lightblue",intensity=400))) %>%
#'   render_tree(lights = FALSE, clamp_value = 10)
#'}
render_tree = function(scene,
                       ground_radius = 10, ground = TRUE,
                       ground_color1 = "darkgreen", ground_color2 = "lightgreen",
                       fov = NULL, lookfrom = NULL, lookat = NULL,
                       angle = c(0,0,0), order_rotation = c(1,2,3),
                       lights = TRUE, lightintensity = 60, clamp_value = 10,
                       width = 600, height = 600, ...) {
  if(length(angle) == 1) {
    angle = c(0,angle,0)
  }
  scene_model = scene[is.na(scene$lightintensity) &
                        (scene$shape == "cylinder" |
                         scene$shape == "sphere" ),]
  bbox_x = range(scene_model$x,na.rm=TRUE)
  bbox_y = range(scene_model$y,na.rm=TRUE)
  bbox_z = range(scene_model$z,na.rm=TRUE)
  if(is.null(lookat)) {
    lookat = c(0,mean(bbox_y),0)
  }
  spheresizes = scene[(scene$shape == "sphere" & scene$type != "light"),4]
  if(length(spheresizes) > 0) {
    max_sphere_radii = max(spheresizes,na.rm=TRUE)
  } else {
    max_sphere_radii = 0.5
  }

  widest = max(c(abs(bbox_x),abs(bbox_y),abs(bbox_z)))
  offset_dist = widest + widest/3 + max_sphere_radii
  if(is.null(fov)) {
    fov = atan2(widest+widest/3 + max_sphere_radii, widest*10)/pi*180*2
  }
  if(any(angle != 0)) {
    scene = group_objects(scene, group_angle = angle, group_order_rotation = order_rotation, pivot_point = c(0,0,0))
  }
  if(lights) {
    light = sphere(x=offset_dist*2,y=offset_dist*2,z=offset_dist*2,
                   radius = widest/2,
                   material = light(intensity=lightintensity)) %>%
      add_object(sphere(x=-offset_dist*2,y=offset_dist*2,z=-offset_dist*2,
                        radius = widest/2,
                        material = light(intensity=lightintensity))) %>%
      add_object(sphere(y=offset_dist*4,
                        radius=widest/2,
                        material = light(intensity=lightintensity)))

    scene = scene %>%
      add_object(light)

  }
  if(ground) {
    scene = scene %>%
      add_object(generate_ground(depth=0,spheresize=ground_radius,
                                 material = diffuse(color=ground_color1,
                                                    sigma=90,noisecolor=ground_color2,
                                                    noise=3, noiseintensity = 1)))
  }
  if(is.null(lookfrom)) {
    lookfrom = c(0,2*widest,widest*5)
  }
  render_scene(scene = scene, lookat = lookat, width = width, height = height,
               fov = fov, lookfrom = lookfrom, clamp_value = clamp_value, ...)
}


