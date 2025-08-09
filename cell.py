#cell shape type
class Cell():
    def __init__(self,x,y,z):
        super().__init__()
        self.x=x
        self.y=y
        self.z=z
    def calculate_cell_surface_area(self):
        surface_area=self.x*self.y
        return surface_area
    def calculate_cell_volume(self):
        volume=self.x*self.y*self.z
        return volume



