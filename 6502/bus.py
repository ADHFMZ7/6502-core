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

    def dump_memory(self, start_addr=0):
        # if start_addr % 16:
        #     start = start_adr - start % 16
        start = start_addr if not start_addr % 16 else start_addr - start_addr % 16

        for offset, data in enumerate(self.memory[start:]):
            addr = start + offset
            if not addr % 16:
                print("\033[34m{:04x}\033[0m".format(addr), end="  ")
            elif not addr % 8:
                print(" ", end="")

            if addr == start_addr:
                print("\033[91m{:02x}\033[0m".format(data), end=" ")
            else:
                print("{:02x}".format(data), end=" ")
            if addr % 16 == 15:
                print("\n", end="")
                
    def dump_memory_at_addr(self, start_addr=0):
        # if start_addr % 16:
        #     start = start_adr - start % 16
        
        start = start_addr if not start_addr % 16 else start_addr - start_addr % 16
        start -= 16 * 4
        end = start + 16 * 9 

        for offset, data in enumerate(self.memory[start:end]):
            addr = start + offset
            if not addr % 16:
                print("\033[34m{:04x}\033[0m".format(addr), end="  ")
            elif not addr % 8:
                print(" ", end="")

            if addr == start_addr:
                print("\033[91m{:02x}\033[0m".format(data), end=" ")
            else:
                print("{:02x}".format(data), end=" ")
            if addr % 16 == 15:
                print("\n", end="")
                
    def dump_memory_range(self, start_addr, end_addr):
        for offset, data in enumerate(self.memory[start_addr:end_addr+1]):
            addr = start_addr + offset
            if not addr % 16:
                print("\033[34m{:04x}\033[0m".format(addr), end="  ")
            elif not addr % 8:
                print(" ", end="")

            print("{:02x}".format(data), end=" ")
            if addr % 16 == 15:
                print("\n", end="")


    def load_rom(self, file_name, addr=0x8000):
        with open(file_name, "rb") as file:
            while (byte := file.read(1)):
                self.memory[addr] = ord(byte)
                addr += 1
        return 1
