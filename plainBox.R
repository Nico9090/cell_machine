#extracellular matrix in ARPKD
library(rgl)
clear3d()
open3d()
# Define cube corners (a unit cube)
verts <- matrix(c(
  0, 0, 0,  # v1
  1, 0, 0,  # v2
  1, 1, 0,  # v3
  0, 1, 0,  # v4
  0, 0, 1,  # v5
  1, 0, 1,  # v6
  1, 1, 1,  # v7
  0, 1, 1   # v8
), ncol = 3, byrow = TRUE)

# Define edges as pairs of vertex indices
edges <- list(
  c(1,2), c(2,3), c(3,4), c(4,1),  # bottom face
  c(5,6), c(6,7), c(7,8), c(8,5),  # top face
  c(1,5), c(2,6), c(3,7), c(4,8)   # vertical edges
)


for (edge in edges) {
  i <- edge[1]
  j <- edge[2]
  lines3d(
    x = c(verts[i,1], verts[j,1]),
    y = c(verts[i,2], verts[j,2]),
    z = c(verts[i,3], verts[j,3]),
    col = "steelblue", lwd = 2
  )
}
points3d(verts[,1], verts[,2], verts[,3], col = "red", size = 8)

inner_verts <- matrix(c(
  0.25, 0.25, 0,  # v1
  0.75, 0.25, 0,  # v2
  0.75, 0.75, 0,  # v3
  0.25, 0.75, 0,  # v4
  0.25, 0.25, 0.75,  # v5
  0.75, 0.25, 0.75,  # v6
  0.75, 0.75, 0.75,  # v7
  0.25, 0.75, 0.75   # v8
), ncol = 3, byrow = TRUE)

edges <- list(
  c(1,2), c(2,3), c(3,4), c(4,1),  # bottom face
  c(5,6), c(6,7), c(7,8), c(8,5),  # top face
  c(1,5), c(2,6), c(3,7), c(4,8)   # vertical edges
)


for (edge in edges) {
  i <- edge[1]
  j <- edge[2]
  lines3d(
    x = c(inner_verts[i,1], inner_verts[j,1]),
    y = c(inner_verts[i,2], inner_verts[j,2]),
    z = c(inner_verts[i,3], inner_verts[j,3]),
    col = "steelblue", lwd = 2
  )
}
points3d(inner_verts[,1], inner_verts[,2], inner_verts[,3], col = "red", size = 8)
