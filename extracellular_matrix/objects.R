#objects
outer_verts_cell1 <- make_cube_vertices(center = c(0.5, 0.5, 0.5), size = 1)
inner_verts_cell1 <- make_cube_vertices(center = c(0.5, 0.5, 0.5), size = 0.5)

outer_verts_cell2 <- make_cube_vertices(center = c(1.5, 0.5, 0.5), size = 1)
inner_verts_cell2 <- make_cube_vertices(center = c(1.5, 0.5, 0.5), size = 0.5)

outer_verts_cell3 <- make_cube_vertices(center = c(2.5, 0.5, 0.5), size = 1)
inner_verts_cell3 <- make_cube_vertices(center = c(2.5, 0.5, 0.5), size = 0.5)

outer_verts_cell4 <- make_cube_vertices(center = c(3.5, 0.5, 0.5), size = 1)
inner_verts_cell4 <- make_cube_vertices(center = c(3.5, 0.5, 0.5), size = 0.5)

cells <- list(inner_verts_cell4,outer_verts_cell4,inner_verts_cell3,outer_verts_cell3,
              inner_verts_cell2,outer_verts_cell2,inner_verts_cell1,outer_verts_cell1)

outer_membrane <- list(outer_verts_cell4,outer_verts_cell3,outer_verts_cell2,outer_verts_cell1)
inner_membrane <- list(inner_verts_cell4, inner_verts_cell3, inner_verts_cell2, inner_verts_cell1)
