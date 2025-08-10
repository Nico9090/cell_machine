from cell import Cell
from organelle import Organelle
class Grow():
    def __init__(self):
        super().__init__()
    def define_cell(self,*args,**kwargs):
        self.cell=Cell(*args,**kwargs)
        return self.cell
    def add_organelle(self,name,*args,**kwargs):
        organelle=Organelle(name,*args,**kwargs)
        return self.organelle
    pass