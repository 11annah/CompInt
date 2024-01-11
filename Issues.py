import site
import torchquad
import os

def get_torchquad_path():
  torchquad_path = os.path.dirname(torchquad.__file__)
  print(torchquad_path)
