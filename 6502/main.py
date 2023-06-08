
from cpu import CPU
from bus import BUS







"""






"""

def main():
    bus = BUS()
    cpu = CPU(bus) 
    
    cpu.reset() 
    
    while 1:
        cpu.clock()
        print(f"{bus.address} {bus.data} {bus.rw}")
        print(cpu.pc, cpu.address, cpu.data)
        print(bus)
        input() 
        
         



if __name__ == '__main__':
    main()

