def draw_cells(coordinates_of_center = [(0,0),(1,1),(0,2),(-1,1)],
               number_of_cells = 3,
               x1_of_center = 1
               y1_of_center = 1
               ):
    center = np.array(coordinates_of_center)
    width_of_center = np.linalg.norm(coordinates_of_center[1] - coordinates_of_center[0])
    shared_width = width_of_center / number_of_cells
    cell_centers = [(0,0),(x1_of_center/number_of_cells,y1_of_center/number_of_cells),
                    (2/number_of_cells,0),
