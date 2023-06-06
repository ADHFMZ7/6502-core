


class BUS:

    def __init__(self):
        self.memory = [0x0] * 64 * 1024
        
        self.data = 0x0
        self.address = 0x0
        
    def read(self, address):
        return self.memory[address]
    
    def write(self, address, data):
        self.memory[address] = data
