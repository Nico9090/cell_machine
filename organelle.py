from dataclasses import dataclass
from typing import Optional
import matplotlib.patches as patches
import matplotlib.pyplot as plt
@dataclass
class Organelle():
    name: str
    shape: str
    size: Optional[float]=None
    radius: Optional[float]=None

    def draw(self, ax, position=(0,0)):
        if self.shape == "circle" and self.radius:
            obj = plt.Circle(position,
                             self.radius,
                             color = "white")
            ax.add_artist(obj)


    
