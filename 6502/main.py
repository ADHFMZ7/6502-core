
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
        print(f"{cpu.address} {cpu.data} {bus.r}")
        
         



if __name__ == '__main__':
    main()

