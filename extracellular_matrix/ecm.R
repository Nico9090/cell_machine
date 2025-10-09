library(rgl)
rgl.quit()
open3d()
clear3d()

# Outer cube centered at (0.5, 0.5, 0.5), size 1
outer_verts <- make_cube_vertices(center = c(0.5, 0.5, 0.5), size = 1)
cell_2_outer_verts <- make_cube_vertices(center = c(1.5, 0.5, 0.5), size = 1)
cell_3_outer_verts <- make_cube_vertices(center = c(2.5, 0.5, 0.5), size = 1)
cell_4_outer_verts <- make_cube_vertices(center = c(3.5, 0.5, 0.5), size = 1)
draw_wire_cube(outer_verts)
draw_wire_cube(cell_2_outer_verts)
draw_wire_cube(cell_3_outer_verts)
draw_wire_cube(cell_4_outer_verts)

# Inner cube centered at same spot, smaller size
inner_verts <- make_cube_vertices(center = c(0.5, 0.5, 0.5), size = 0.5)
cell_2_inner_verts <- make_cube_vertices(center = c(1.5, 0.5, 0.5), size = 0.5)
cell_3_inner_verts <- make_cube_vertices(center = c(2.5, 0.5, 0.5), size = 0.5)
cell_4_inner_verts <- make_cube_vertices(center = c(3.5, 0.5, 0.5), size = 0.5)

draw_wire_cube(inner_verts)
draw_wire_cube(cell_2_inner_verts)
draw_wire_cube(cell_3_inner_verts)
draw_wire_cube(cell_4_inner_verts)

draw_filled_outer_cube(outer_verts,col = "steelblue",alpha = 0.4)
draw_filled_outer_cube(cell_2_outer_verts,col = "steelblue",alpha = 0.4)
draw_filled_outer_cube(cell_3_outer_verts,col = "steelblue",alpha = 0.4)
draw_filled_outer_cube(cell_4_outer_verts,col = "steelblue",alpha = 0.4)

draw_filled_cube(inner_verts,col = "darkred",alpha = 0.8)
draw_filled_cube(cell_2_inner_verts,col = "darkred",alpha = 0.8)
draw_filled_cube(cell_3_inner_verts,col = "darkred",alpha = 0.8)
draw_filled_cube(cell_4_inner_verts,col = "darkred",alpha = 0.8)
draw_integrins(cell_centers = c(0.5,1.5,2.5,3.5))
draw_basement_membrane(y_location = -1,start = 0,end = 4)
axes3d()
