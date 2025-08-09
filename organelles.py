class AddOrganelle():
    def __init__(self):
        super().__init__()
        self.organelles=[]
    def append_organelle_list(self,organelle):
        self.organelles.append(organelle)
        return self.organelles
    def organelle_details(self):
        return self.organelles