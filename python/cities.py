x_size = 512
y_size = 512

things = []

class City:
    def __init__(self):
        self.x = int(random()*x_size)
        self.y = int(random()*y_size)
        self.res_a = 5/(random())
        self.res_b = 5/(random())
        things.append(self)
    
    def step(self):
        
        
class Worker:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        things.append(self)
    def step(self):
    
