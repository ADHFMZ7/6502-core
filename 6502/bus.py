MEMORY_SIZE = 64 * 1024

class BUS:

    def __init__(self):
        self.memory = [0xea] * MEMORY_SIZE 
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

    def dump_memory(self, start=0, end=MEMORY_SIZE):
        for addr, data in enumerate(self.memory):
            if not addr % 16:
                print(addr, end="\t")
            elif not addr % 8:
                print(" ", end="")
            print("{:02x}".format(data), end=" ")
            if addr % 16 == 15:
                print("\n", end="")

