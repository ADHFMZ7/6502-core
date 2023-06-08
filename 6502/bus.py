


class BUS:

    def __init__(self):
        self.memory = [0x0] * 64 * 1024
        self.memory[0xFFFC] = 0x00
        self.memory[0xFFFD] = 0x80
        
        self.data = 0x0
        self.address = 0x0
        self.r = 1 
       
    def read(self, address):
        return self.memory[address]
    
    def write(self, address, data):
        self.memory[address] = data
        
    def next_byte(self, byte):
        return self.memory[byte + 1]

